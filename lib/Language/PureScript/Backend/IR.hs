module Language.PureScript.Backend.IR
  ( module Language.PureScript.Backend.IR
  , module Language.PureScript.Backend.IR.Types
  ) where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Writer.Class (MonadWriter (..))
import Data.Char qualified as Char
import Data.Foldable (foldrM)
import Data.List qualified as List
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NE
import Data.Map.Lazy qualified as Map
import Data.Tagged (Tagged (Tagged))
import Data.Text qualified as Text
import Data.Traversable (for)
import Language.PureScript.Backend.IR.Types
import Language.PureScript.CoreFn qualified as Cfn
import Language.PureScript.CoreFn.Laziness (applyLazinessTransform)
import Language.PureScript.Names qualified as PS
import Language.PureScript.PSString (PSString, decodeStringEither)
import Numeric (showHex)
import Relude.Extra (toFst)
import Relude.Unsafe qualified as Unsafe
import Prelude hiding (identity)

data Context = Context
  { contextModule :: Cfn.Module Cfn.Ann
  , contextDataTypes :: DataTypes
  , lastGeneratedNameIndex :: Integer
  , needsRuntimeLazy :: Any
  }

type CfnExp = Cfn.Expr Cfn.Ann
type DataTypes = Map TyName (AlgebraicType, Map CtorName [FieldName])

newtype RepM a = RepM (StateT Context (Either CoreFnError) a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState Context
    , MonadError CoreFnError
    )

instance MonadWriter Any RepM where
  writer (a, nrl) = a <$ modify \ctx -> ctx {needsRuntimeLazy = nrl}
  listen repM = (,) <$> repM <*> gets needsRuntimeLazy
  pass repM = do
    (a, f) <- repM
    a <$ modify' \ctx -> ctx {needsRuntimeLazy = f (needsRuntimeLazy ctx)}

runRepM
  :: Context
  -> RepM a
  -> Either CoreFnError (Tagged "needsRuntimeLazy" Bool, a)
runRepM ctx (RepM m) =
  runStateT m ctx <&> \(a, ctx') ->
    (Tagged . getAny $ needsRuntimeLazy ctx', a)

mkModule
  :: Cfn.Module Cfn.Ann
  -> Either CoreFnError (Tagged "needsRuntimeLazy" Bool, Module)
mkModule cfnModule = do
  let ctx =
        Context
          { contextModule = cfnModule
          , contextDataTypes = collectDataTypes (Cfn.moduleBindings cfnModule)
          , lastGeneratedNameIndex = 0
          , needsRuntimeLazy = Any False
          }
  runRepM ctx do
    moduleBindings <- mkDecls
    moduleImports <- mkImports
    moduleExports <- mkExports
    moduleReExports <- mkReExports
    moduleForeigns <- mkForeign
    pure
      Module
        { moduleName = mkModuleName $ Cfn.moduleName cfnModule
        , modulePath = Cfn.modulePath cfnModule
        , moduleBindings
        , moduleImports
        , moduleExports
        , moduleReExports
        , moduleForeigns
        , dataTypes = contextDataTypes ctx
        }

mkModuleName :: PS.ModuleName -> ModuleName
mkModuleName = ModuleName . PS.runModuleName

mkImports :: RepM [ModuleName]
mkImports = do
  Cfn.Module {moduleName, moduleImports} <- gets contextModule
  pure $ filter (isIncluded moduleName) (mkModuleName . snd <$> moduleImports)
 where
  isIncluded :: PS.ModuleName -> ModuleName -> Bool
  isIncluded currentModule modname = modname /= mkModuleName currentModule

mkExports :: RepM [Name]
mkExports = identToName <<$>> gets (contextModule >>> Cfn.moduleExports)

mkReExports :: RepM (Map ModuleName [Name])
mkReExports =
  Map.fromAscList . fmap (bimap mkModuleName (identToName <$>)) . Map.toAscList
    <$> gets (contextModule >>> Cfn.moduleReExports)

mkForeign :: RepM [Name]
mkForeign = identToName <<$>> gets (contextModule >>> Cfn.moduleForeign)

collectDataTypes :: [Cfn.Bind Cfn.Ann] -> DataTypes
collectDataTypes binds =
  Map.fromList $
    List.groupBy
      ((==) `on` fst)
      [ ( mkTyName tyName
        , (mkCtorName ctorName, mkFieldName <$> fields)
        )
      | bind <- binds
      , Cfn.Constructor _ann tyName ctorName fields <- boundExp bind
      ]
      <&> \ctors ->
        let ty :: TyName
            ty = fst (Unsafe.head ctors) -- groupBy never makes an empty group
            algebraicType = if length ctors == 1 then ProductType else SumType
         in (ty, (algebraicType, Map.fromList (snd <$> ctors)))
 where
  boundExp :: Cfn.Bind a -> [Cfn.Expr a]
  boundExp = \case
    Cfn.Rec bindingGroup -> snd <$> bindingGroup
    Cfn.NonRec _ann _ident expr -> [expr]

mkQualified :: (a -> n) -> PS.Qualified a -> Qualified n
mkQualified f (PS.Qualified by a) =
  case by of
    PS.BySourcePos _sourcePos -> Local (f a)
    PS.ByModuleName mn -> Imported (mkModuleName mn) (f a)

identToName :: PS.Ident -> Name
identToName = Name . PS.runIdent

mkDecls :: RepM [Binding]
mkDecls = do
  psDecls <- gets $ contextModule >>> Cfn.moduleBindings
  traverse mkGrouping psDecls

mkGrouping :: Cfn.Bind Cfn.Ann -> RepM Binding
mkGrouping = \case
  Cfn.NonRec _ann ident cfnExpr ->
    Standalone . (identToName ident,) <$> mkExp cfnExpr
  Cfn.Rec bindingGroup -> do
    modname <- gets $ contextModule >>> Cfn.moduleName
    bindings <- writer $ applyLazinessTransform modname bindingGroup
    case NE.nonEmpty bindings of
      Nothing -> throwError EmptyBindingGroup
      Just bs ->
        RecursiveGroup <$> for bs \((_ann, ident), expr) ->
          (identToName ident,) <$> mkExp expr

mkExp :: CfnExp -> RepM Exp
mkExp cfnExpr =
  case cfnExpr of
    Cfn.Literal _ann literal ->
      mkLiteral literal
    Cfn.Constructor ann tyName ctorName ids ->
      mkConstructor ann tyName ctorName ids
    Cfn.Accessor _ann str expr ->
      mkAccessor str expr
    Cfn.ObjectUpdate _ann expr patches ->
      mkObjectUpdate expr patches
    Cfn.Abs _ann ident expr ->
      mkAbstraction ident expr
    Cfn.App _ann abstr arg ->
      mkApplication abstr arg
    Cfn.Var _ann qualifiedIdent ->
      mkRef qualifiedIdent
    Cfn.Case _ann exprs alternatives ->
      case (exprs,) <$> NE.nonEmpty alternatives of
        Just (es, as) -> mkCase es as
        Nothing -> throwError $ EmptyCase cfnExpr
    Cfn.Let _ann binds exprs -> do
      mkLet binds exprs

mkLiteral :: Cfn.Literal CfnExp -> RepM Exp
mkLiteral = \case
  Cfn.NumericLiteral (Left i) ->
    pure $ integer i
  Cfn.NumericLiteral (Right d) ->
    pure $ float d
  Cfn.StringLiteral s ->
    pure $ string $ psStringToText s
  Cfn.CharLiteral c ->
    pure $ char c
  Cfn.BooleanLiteral b ->
    pure $ boolean b
  Cfn.ArrayLiteral exprs ->
    array <$> traverse mkExp exprs
  Cfn.ObjectLiteral kvs ->
    let props = first (PropName . psStringToText) <$> kvs
     in object <$> traverse (traverse mkExp) props

mkConstructor
  :: Cfn.Ann
  -> PS.ProperName 'PS.TypeName
  -> PS.ProperName 'PS.ConstructorName
  -> [PS.Ident]
  -> RepM Exp
mkConstructor ann properTyName properCtorName fields = do
  Context {contextDataTypes} <- get
  let tyName = mkTyName properTyName
  (algebraicTy, _ctors) <-
    Map.lookup tyName contextDataTypes
      & maybe (throwError (TypeNotDeclared tyName)) pure
  if isNewtype ann
    then pure identity
    else
      pure $
        ctor
          algebraicTy
          tyName
          (mkCtorName properCtorName)
          (mkFieldName <$> fields)

mkTyName :: PS.ProperName 'PS.TypeName -> TyName
mkTyName = TyName . PS.runProperName

mkCtorName :: PS.ProperName 'PS.ConstructorName -> CtorName
mkCtorName = CtorName . PS.runProperName

mkFieldName :: PS.Ident -> FieldName
mkFieldName = FieldName . PS.runIdent

mkAccessor :: PSString -> CfnExp -> RepM Exp
mkAccessor prop cfnExpr =
  mkExp cfnExpr <&> \expr -> objectProp expr (PropName (psStringToText prop))

mkObjectUpdate :: CfnExp -> [(PSString, CfnExp)] -> RepM Exp
mkObjectUpdate cfnExp props = do
  expr <- mkExp cfnExp
  patch <- for props \(prop, cExpr) ->
    (PropName (psStringToText prop),) <$> mkExp cExpr
  maybe (throwError EmptyObjectUpdate) (pure . update expr) (NE.nonEmpty patch)

mkAbstraction :: PS.Ident -> CfnExp -> RepM Exp
mkAbstraction i e = abstraction arg <$> mkExp e
 where
  arg = case PS.runIdent i of
    "$__unused" -> ArgUnused
    n -> ArgNamed (Name n)

mkApplication :: CfnExp -> CfnExp -> RepM Exp
mkApplication e1 e2 =
  if isNewtype (Cfn.extractAnn e1)
    then mkExp e2
    else application <$> mkExp e1 <*> mkExp e2

mkQualifiedIdent :: PS.Qualified PS.Ident -> RepM (Qualified Name)
mkQualifiedIdent (PS.Qualified by ident) =
  gets (Cfn.moduleName . contextModule) <&> \contextModuleName ->
    case by of
      PS.BySourcePos _sourcePos -> Local $ identToName ident
      PS.ByModuleName modName ->
        if modName == contextModuleName
          then Local (identToName ident)
          else Imported (mkModuleName modName) (identToName ident)

mkRef :: PS.Qualified PS.Ident -> RepM Exp
mkRef = refFree <<$>> mkQualifiedIdent

mkLet :: [Cfn.Bind Cfn.Ann] -> CfnExp -> RepM Exp
mkLet binds expr = do
  bindingarations :: NonEmpty Binding <-
    NE.nonEmpty binds & maybe (throwError LetWithoutBinds) (traverse mkGrouping)
  lets bindingarations <$> mkExp expr

psStringToText :: PSString -> Text
psStringToText = foldMap encodeChar . decodeStringEither
 where
  encodeChar :: Either Word16 Char -> Text
  encodeChar (Left c) = "\\x" <> showHex' 6 c
  encodeChar (Right c)
    | c == '\t' = "\\t"
    | c == '\r' = "\\r"
    | c == '\n' = "\\n"
    | c == '"' = "\\\""
    | c == '\'' = "\\\'"
    | c == '\\' = "\\\\"
    | shouldPrint c = Text.singleton c
    | otherwise = "\\x" <> showHex' 6 (Char.ord c)

  -- Note we do not use Data.Char.isPrint here because that includes things
  -- like zero-width spaces and combining punctuation marks, which could be
  -- confusing to print unescaped.
  shouldPrint :: Char -> Bool
  -- The standard space character, U+20 SPACE, is the only space char we should
  -- print without escaping
  shouldPrint ' ' = True
  shouldPrint c =
    Char.generalCategory c
      `elem` [ Char.UppercaseLetter
             , Char.LowercaseLetter
             , Char.TitlecaseLetter
             , Char.OtherLetter
             , Char.DecimalNumber
             , Char.LetterNumber
             , Char.OtherNumber
             , Char.ConnectorPunctuation
             , Char.DashPunctuation
             , Char.OpenPunctuation
             , Char.ClosePunctuation
             , Char.InitialQuote
             , Char.FinalQuote
             , Char.OtherPunctuation
             , Char.MathSymbol
             , Char.CurrencySymbol
             , Char.ModifierSymbol
             , Char.OtherSymbol
             ]
  showHex' :: Enum a => Int -> a -> Text
  showHex' width c =
    let hs = showHex (fromEnum c) ""
     in Text.pack (replicate (width - length hs) '0' <> hs)

--------------------------------------------------------------------------------
-- Case statements are compiled to a decision trees (nested if/else's) ---------
-- The algorithm is based on this document: ------------------------------------
-- https://julesjacobs.com/notes/patternmatching/patternmatching.pdf -----------

mkCase
  :: [CfnExp]
  -> NonEmpty (Cfn.CaseAlternative Cfn.Ann)
  -> RepM Exp
mkCase expressions alternatives = do
  (refExpressions, bindingareRefs) <- expressionsToRefs expressions
  clauses <- traverse (alternativeToClauses refExpressions) alternatives
  bindingareRefs <$> mkCaseClauses (NE.toList clauses)

expressionsToRefs :: [CfnExp] -> RepM ([Exp], Exp -> Exp)
expressionsToRefs cfnExps =
  traverse mkExp cfnExps >>= fmap declareBinds . foldrM inlineOrReference []
 where
  inlineOrReference
    :: Exp
    -> [Either Exp (Name, Exp)]
    -- \^ Either an expression to inline, or a named expression reference.
    -> RepM [Either Exp (Name, Exp)]
  inlineOrReference e acc =
    case unExp e of
      Lit (Integer _) -> inlineExpr
      Lit (Floating _) -> inlineExpr
      Lit (Char _) -> inlineExpr
      Lit (Boolean _) -> inlineExpr
      RefFree (Local _) -> inlineExpr
      RefBound _ -> inlineExpr
      _ -> referenceExpr
   where
    inlineExpr = pure (Left e : acc)
    referenceExpr = (: acc) . Right . (,e) <$> generateName "e"

  -- Declare extracted references
  declareBinds :: [Either Exp (Name, Exp)] -> ([Exp], Exp -> Exp)
  declareBinds expressionsOrRefs =
    ( either id (refFreeLocal . fst) <$> expressionsOrRefs
    , case NE.nonEmpty (mapMaybe rightToMaybe expressionsOrRefs) of
        Nothing -> id
        Just refs -> lets (Standalone <$> refs)
    )

mkCaseClauses :: [CaseClause] -> RepM Exp
mkCaseClauses = mkClauses mempty
 where
  mkClauses :: MatchHistory -> [CaseClause] -> RepM Exp
  mkClauses history = \case
    [] -> pure $ exception "No patterns matched"
    clause : restClauses -> do
      mkClause
        history
        clause
        (`matchChosenByHeuristic` restClauses)
        (`mkClauses` restClauses)

  mkClause
    :: MatchHistory
    -> CaseClause
    -> (CaseClause -> Maybe (Match, CaseClause))
    -> (MatchHistory -> RepM Exp)
    -> RepM Exp
  mkClause history currentClause heuristic nextClause =
    case heuristic currentClause of
      Nothing ->
        -- All matches for this clause have passed
        case NE.nonEmpty (usedClauseBinds currentClause) of
          Nothing -> do
            next <- nextClause history
            pure case clauseResult currentClause of
              Right result -> result
              Left guardedResults ->
                foldr (uncurry ifThenElse) next guardedResults
          Just binds -> do
            n <- generateName "n"
            next <- nextClause history
            pure case clauseResult currentClause of
              Right result -> lets binds result
              Left guardedResults ->
                lets (Standalone (n, next) <| binds) $
                  foldr (uncurry ifThenElse) (refFreeLocal n) guardedResults
      Just (Match {..}, clause) ->
        let expr = foldr applyStep matchExp stepsToFocus
            clause' =
              clause
                { clauseBinds =
                    (Standalone . (,expr) <$> matchBinds) <> clauseBinds clause
                , clauseMatches =
                    nestedMatches <> clauseMatches clause
                }
         in case matchPat of
              PatAny ->
                nextMatch history clause'
              PatArrayLength (fromIntegral -> len) ->
                ifThenElse (integer len `eq` arrayLength expr)
                  <$> nextMatch history clause'
                  <*> nextClause history
              PatInteger i ->
                ifThenElse (integer i `eq` expr)
                  <$> nextMatch history clause'
                  <*> nextClause history
              PatFloating d ->
                ifThenElse (float d `eq` expr)
                  <$> nextMatch history clause'
                  <*> nextClause history
              PatString s ->
                ifThenElse (string s `eq` expr)
                  <$> nextMatch history clause'
                  <*> nextClause history
              PatChar c ->
                ifThenElse (char c `eq` expr)
                  <$> nextMatch history clause'
                  <*> nextClause history
              PatBoolean b ->
                ifThenElse (boolean b `eq` expr)
                  <$> nextMatch history clause'
                  <*> nextClause history
              PatCtor _ty constructor -> do
                case Map.lookup expr history of
                  Just (c, True) ->
                    if c == constructor
                      then -- This constructor matched positively before,
                      -- proceed matching nested constructors.
                        nextMatch history clause'
                      else -- Other constructor matched positively before,
                      -- this one can't match, proceed to next clause.
                        nextClause history
                  Just (c, False)
                    | c == constructor ->
                        -- This constructor matched negatively before,
                        -- proceed to the next clause.
                        nextClause history
                  _ -> do
                    let qctor =
                          constructor
                            & fmap renderCtorName
                            & qualified string \modname c ->
                              string (renderModuleName modname <> "." <> c)
                        history' b = Map.insert expr (constructor, b) history
                    -- Either this constructor is matched for the first time,
                    -- or other constructor didn't pass the match before.
                    ifThenElse (qctor `eq` reflectCtor expr)
                      <$> nextMatch (history' True) clause'
                      <*> nextClause (history' False)
   where
    nextMatch hist clause = mkClause hist clause heuristic nextClause

usedClauseBinds :: CaseClause -> [Binding]
usedClauseBinds CaseClause {clauseBinds} = clauseBinds

matchChosenByHeuristic
  :: CaseClause -> [CaseClause] -> Maybe (Match, CaseClause)
matchChosenByHeuristic thisClause otherClauses =
  case clauseMatches thisClause of
    [] -> Nothing
    [match] -> Just (match, thisClause {clauseMatches = []})
    matches ->
      -- select a match that is present in the maximum number of other clauses
      sortOn
        (Down . fst)
        (toFst (countAffectedClauses otherClauses) <$> matches)
        & uncons
        & fmap \(match, remainingMatches) ->
          (snd match, thisClause {clauseMatches = snd <$> remainingMatches})
 where
  countAffectedClauses :: [CaseClause] -> Match -> Int
  countAffectedClauses clauses Match {matchExp = expr, stepsToFocus = steps} =
    foldr count 0 clauses
   where
    count :: CaseClause -> Int -> Int
    count clause counter =
      maybe counter (\_ -> counter + 1) $
        allClauseMatches clause & find \case
          Match {matchPat = PatAny} -> False
          Match {matchExp, stepsToFocus}
            | matchExp == expr, stepsToFocus == steps -> True
          _ -> False

    allClauseMatches :: CaseClause -> [Match]
    allClauseMatches CaseClause {clauseMatches} = go [] clauseMatches
     where
      go acc = \case
        [] -> acc
        ms -> ms >>= \t -> go (t : acc) (nestedMatches t)

type Guard = Exp

data CaseClause = CaseClause
  { clauseMatches :: [Match]
  , clauseResult :: Either [(Guard, Exp)] Exp
  , clauseBinds :: [Binding]
  }
  deriving stock (Show)

data Pattern
  = PatAny
  | PatCtor (Qualified TyName) (Qualified CtorName)
  | PatInteger Integer
  | PatFloating Double
  | PatString Text
  | PatChar Char
  | PatBoolean Bool
  | PatArrayLength Int
  deriving stock (Eq, Show)

data Step = TakeIndex Natural | TakeProp PropName
  deriving stock (Eq, Show)

applyStep :: Step -> Exp -> Exp
applyStep step expr =
  case step of
    TakeIndex i -> arrayIndex expr i
    TakeProp p -> objectProp expr p

data Match = Match
  { matchExp :: Exp
  , matchPat :: Pattern
  , matchBinds :: [Name]
  , stepsToFocus :: [Step]
  , nestedMatches :: [Match]
  }
  deriving stock (Show)

mkBinder :: Exp -> Cfn.Binder Cfn.Ann -> RepM Match
mkBinder matchExp = go mempty
 where
  go :: [Step] -> Cfn.Binder Cfn.Ann -> RepM Match
  go stepsToFocus = \case
    Cfn.NullBinder _ann ->
      pure $ matchWhole PatAny
    Cfn.VarBinder _ann name ->
      pure
        Match
          { matchExp
          , matchPat = PatAny
          , stepsToFocus
          , matchBinds = [identToName name]
          , nestedMatches = mempty
          }
    Cfn.ConstructorBinder ann tyName ctorName binders ->
      if isNewtype ann
        then case binders of
          [binder] -> go stepsToFocus binder
          _ -> throwError NewtypeCtorBinderHasUnexpectedNumberOfNestedBinders
        else do
          nestedMatches <-
            for (zip [0 ..] binders) \(index, binder) ->
              go (TakeIndex index : stepsToFocus) binder
          pure
            Match
              { matchExp
              , matchPat =
                  PatCtor
                    (mkQualified mkTyName tyName)
                    (mkQualified mkCtorName ctorName)
              , stepsToFocus
              , matchBinds = mempty
              , nestedMatches
              }
    Cfn.LiteralBinder _ann literal ->
      case literal of
        Cfn.NumericLiteral (Left i) ->
          pure $ matchWhole $ PatInteger i
        Cfn.NumericLiteral (Right d) ->
          pure $ matchWhole $ PatFloating d
        Cfn.StringLiteral s ->
          pure $ matchWhole $ PatString (psStringToText s)
        Cfn.CharLiteral c ->
          pure $ matchWhole $ PatChar c
        Cfn.BooleanLiteral b ->
          pure $ matchWhole $ PatBoolean b
        Cfn.ArrayLiteral binders -> do
          nestedMatches <-
            for (zip [0 ..] binders) \(index, binder) ->
              go (TakeIndex index : stepsToFocus) binder
          pure
            Match
              { matchExp
              , stepsToFocus
              , matchPat = PatArrayLength (length binders)
              , matchBinds = mempty
              , nestedMatches
              }
        Cfn.ObjectLiteral kvs -> do
          nestedMatches <-
            for kvs \(PropName . psStringToText -> prop, binder) ->
              go (TakeProp prop : stepsToFocus) binder
          pure
            Match
              { matchExp
              , matchPat = PatAny
              , stepsToFocus
              , matchBinds = mempty
              , nestedMatches
              }
    Cfn.NamedBinder _ann ident binder -> do
      match <- go stepsToFocus binder
      let bind = identToName ident
      pure case matchBinds match of
        [] -> match {matchBinds = [bind]}
        bs -> match {matchBinds = bind : bs}
   where
    matchWhole :: Pattern -> Match
    matchWhole pat =
      Match
        { matchExp
        , stepsToFocus
        , matchPat = pat
        , matchBinds = mempty
        , nestedMatches = mempty
        }

type MatchHistory = Map Exp (Qualified CtorName, Bool)

alternativeToClauses
  :: [Exp] -> Cfn.CaseAlternative Cfn.Ann -> RepM CaseClause
alternativeToClauses
  localRefs
  Cfn.CaseAlternative {caseAlternativeBinders, caseAlternativeResult} = do
    unless (length localRefs == length caseAlternativeBinders) do
      throwError $
        CaseBindersNumberMismatch
          (Tagged $ length localRefs)
          (Tagged $ length caseAlternativeBinders)

    matches <- for (zip localRefs caseAlternativeBinders) do
      uncurry mkBinder

    clauseResult <-
      bitraverse
        (traverse (bitraverse mkExp mkExp))
        mkExp
        caseAlternativeResult

    pure
      CaseClause
        { clauseResult
        , clauseBinds = []
        , clauseMatches = matches
        }

--------------------------------------------------------------------------------
-- Helper functions ------------------------------------------------------------

generateName :: Text -> RepM Name
generateName prefix =
  Name <$> do
    ctx@Context {lastGeneratedNameIndex} <- get
    put $ ctx {lastGeneratedNameIndex = lastGeneratedNameIndex + 1}
    pure $ prefix <> show lastGeneratedNameIndex

isNewtype :: Cfn.Ann -> Bool
isNewtype = \case
  Just Cfn.IsNewtype -> True
  _ -> False

isForeign :: Cfn.Ann -> Bool
isForeign = \case
  Just Cfn.IsForeign -> True
  _ -> False

--------------------------------------------------------------------------------
-- Errors ----------------------------------------------------------------------

data CoreFnError
  = EmptyExportList
  | EmptyObjectUpdate
  | NoDeclarations
  | LetWithoutBinds
  | EmptyCase (Cfn.Expr Cfn.Ann)
  | EmptyBindingGroup
  | TypeNotDeclared TyName
  | NewtypeCtorBinderHasUnexpectedNumberOfNestedBinders
  | CaseBindersNumberMismatch
      (Tagged "expressions" Int)
      (Tagged "binders" Int)
  deriving stock (Show)
