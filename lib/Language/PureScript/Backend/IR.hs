module Language.PureScript.Backend.IR
  ( module Language.PureScript.Backend.IR
  , module Language.PureScript.Backend.IR.Types
  ) where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Writer.Class (MonadWriter (..))
import Data.Foldable (foldrM)
import Data.List qualified as List
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NE
import Data.Map.Lazy qualified as Map
import Data.Tagged (Tagged (Tagged))
import Data.Traversable (for)
import Language.PureScript.Backend.IR.Types
import Language.PureScript.CoreFn qualified as Cfn
import Language.PureScript.CoreFn.Laziness (applyLazinessTransform)
import Language.PureScript.Names (ModuleName (..), runModuleName)
import Language.PureScript.Names qualified as Names
import Language.PureScript.Names qualified as PS
import Language.PureScript.PSString
  ( PSString
  , decodeString
  , decodeStringEscaping
  )
import Relude.Extra (toFst)
import Relude.Unsafe qualified as Unsafe
import Text.Pretty.Simple (pShow)
import Text.Show (Show (..))
import Prelude hiding (identity, show)

data Context = Context
  { contextModule
      ∷ Cfn.Module Cfn.Ann
  , contextDataTypes
      ∷ Map (ModuleName, TyName) (AlgebraicType, Map CtorName [FieldName])
  , lastGeneratedNameIndex
      ∷ Integer
  , needsRuntimeLazy
      ∷ Any
  }

type CfnExp = Cfn.Expr Cfn.Ann

newtype RepM a = RepM (StateT Context (Either CoreFnError) a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState Context
    , MonadError CoreFnError
    )

instance MonadWriter Any RepM where
  writer (a, nrl) = a <$ modify \ctx → ctx {needsRuntimeLazy = nrl}
  listen repM = (,) <$> repM <*> gets needsRuntimeLazy
  pass repM = do
    (a, f) ← repM
    a <$ modify' \ctx → ctx {needsRuntimeLazy = f (needsRuntimeLazy ctx)}

runRepM
  ∷ Context
  → RepM a
  → Either CoreFnError (Tagged "needsRuntimeLazy" Bool, a)
runRepM ctx (RepM m) =
  runStateT m ctx <&> \(a, ctx') →
    (Tagged . getAny $ needsRuntimeLazy ctx', a)

mkModule
  ∷ Cfn.Module Cfn.Ann
  → Map (ModuleName, TyName) (AlgebraicType, Map CtorName [FieldName])
  → Either CoreFnError (Tagged "needsRuntimeLazy" Bool, Module)
mkModule cfnModule contextDataTypes = do
  let ctx =
        Context
          { contextModule = cfnModule
          , contextDataTypes
          , lastGeneratedNameIndex = 0
          , needsRuntimeLazy = Any False
          }
  runRepM ctx do
    moduleBindings ← mkDecls
    moduleImports ← mkImports
    moduleExports ← mkExports
    moduleReExports ← mkReExports
    moduleForeigns ← mkForeign
    pure
      Module
        { moduleName = Cfn.moduleName cfnModule
        , modulePath = Cfn.modulePath cfnModule
        , moduleBindings
        , moduleImports
        , moduleExports
        , moduleReExports
        , moduleForeigns
        }

mkImports ∷ RepM [ModuleName]
mkImports = do
  Cfn.Module {moduleName, moduleImports} ← gets contextModule
  pure $ filter (isIncluded moduleName) (snd <$> moduleImports)
 where
  isIncluded ∷ PS.ModuleName → ModuleName → Bool
  isIncluded currentModule modname = modname /= currentModule

mkExports ∷ RepM [Name]
mkExports = identToName <<$>> gets (contextModule >>> Cfn.moduleExports)

mkReExports ∷ RepM (Map ModuleName [Name])
mkReExports =
  Map.fromAscList . fmap (identToName <<$>>) . Map.toAscList
    <$> gets (contextModule >>> Cfn.moduleReExports)

mkForeign ∷ RepM [Name]
mkForeign = identToName <<$>> gets (contextModule >>> Cfn.moduleForeign)

collectDataDeclarations
  ∷ Map ModuleName (Cfn.Module Cfn.Ann)
  → Map (ModuleName, TyName) (AlgebraicType, Map CtorName [FieldName])
collectDataDeclarations cfnModules = Map.unions do
  Map.toList cfnModules <&> \(modName, cfnModule) →
    Map.fromList
      [ ((modName, ty), (algebraicType, Map.fromList (snd <$> ctors)))
      | ctors ←
          List.groupBy
            ((==) `on` fst)
            [ (mkTyName tyName, (mkCtorName ctorName, mkFieldName <$> fields))
            | bind ← Cfn.moduleBindings cfnModule
            , Cfn.Constructor _ann tyName ctorName fields ← boundExp bind
            ]
      , let ty = fst (Unsafe.head ctors) -- groupBy never makes an empty group
      , let algebraicType = if length ctors == 1 then ProductType else SumType
      ]
 where
  boundExp ∷ Cfn.Bind a → [Cfn.Expr a]
  boundExp = \case
    Cfn.Rec bindingGroup → snd <$> bindingGroup
    Cfn.NonRec _ann _ident expr → [expr]

mkQualified ∷ (a → n) → PS.Qualified a → Qualified n
mkQualified f (PS.Qualified by a) =
  case by of
    PS.BySourcePos _sourcePos → Local (f a)
    PS.ByModuleName mn → Imported mn (f a)

identToName ∷ PS.Ident → Name
identToName = Name . PS.runIdent

mkDecls ∷ RepM [Grouping (Name, Exp)]
mkDecls = do
  psDecls ← gets $ contextModule >>> Cfn.moduleBindings
  traverse mkGrouping psDecls

mkGrouping ∷ Cfn.Bind Cfn.Ann → RepM (Grouping (Name, Exp))
mkGrouping = \case
  Cfn.NonRec _ann ident cfnExpr →
    Standalone . (identToName ident,) <$> makeExp cfnExpr
  Cfn.Rec bindingGroup → do
    modname ← gets $ contextModule >>> Cfn.moduleName
    bindings ← writer $ applyLazinessTransform modname bindingGroup
    case NE.nonEmpty bindings of
      Nothing → throwContextualError EmptyBindingGroup
      Just bs →
        RecursiveGroup <$> for bs \((_ann, ident), expr) →
          (identToName ident,) <$> makeExp expr

makeExp ∷ CfnExp → RepM Exp
makeExp cfnExpr =
  case cfnExpr of
    Cfn.Literal _ann literal →
      mkLiteral literal
    Cfn.Constructor ann tyName ctorName ids →
      mkConstructor ann tyName ctorName ids
    Cfn.Accessor _ann str expr →
      mkAccessor str expr
    Cfn.ObjectUpdate _ann expr patches →
      mkObjectUpdate expr patches
    Cfn.Abs _ann ident expr →
      mkAbstraction ident expr
    Cfn.App _ann abstr arg →
      mkApplication abstr arg
    Cfn.Var _ann qualifiedIdent →
      mkRef qualifiedIdent
    Cfn.Case _ann exprs alternatives →
      case (exprs,) <$> NE.nonEmpty alternatives of
        Just (es, as) → mkCase es as
        Nothing → throwContextualError $ EmptyCase cfnExpr
    Cfn.Let _ann binds exprs → do
      mkLet binds exprs

mkLiteral ∷ Cfn.Literal CfnExp → RepM Exp
mkLiteral = \case
  Cfn.NumericLiteral (Left i) →
    pure $ literalInt i
  Cfn.NumericLiteral (Right d) →
    pure $ literalFloat d
  Cfn.StringLiteral s →
    pure $ literalString $ decodeStringEscaping s
  Cfn.CharLiteral c →
    pure $ literalChar c
  Cfn.BooleanLiteral b →
    pure $ literalBool b
  Cfn.ArrayLiteral exprs →
    literalArray <$> traverse makeExp exprs
  Cfn.ObjectLiteral kvs →
    literalObject <$> traverse (bitraverse mkPropName makeExp) kvs

mkConstructor
  ∷ Cfn.Ann
  → PS.ProperName 'PS.TypeName
  → PS.ProperName 'PS.ConstructorName
  → [PS.Ident]
  → RepM Exp
mkConstructor ann properTyName properCtorName fields = do
  let tyName = mkTyName properTyName
  contextModuleName ← gets (Cfn.moduleName . contextModule)
  algTy ← algebraicTy contextModuleName tyName
  pure
    if isNewtype ann
      then identity
      else
        ctor
          algTy
          contextModuleName
          tyName
          (mkCtorName properCtorName)
          (mkFieldName <$> fields)

mkTyName ∷ PS.ProperName 'PS.TypeName → TyName
mkTyName = TyName . PS.runProperName

mkCtorName ∷ PS.ProperName 'PS.ConstructorName → CtorName
mkCtorName = CtorName . PS.runProperName

mkFieldName ∷ PS.Ident → FieldName
mkFieldName = FieldName . PS.runIdent

mkPropName ∷ PSString → RepM PropName
mkPropName str = case decodeString str of
  Left err → throwContextualError $ UnicodeDecodeError err
  Right decodedString → pure $ PropName decodedString

mkAccessor ∷ PSString → CfnExp → RepM Exp
mkAccessor prop cfnExpr = do
  propName ← mkPropName prop
  makeExp cfnExpr <&> \expr → objectProp expr propName

mkObjectUpdate ∷ CfnExp → [(PSString, CfnExp)] → RepM Exp
mkObjectUpdate cfnExp props = do
  expr ← makeExp cfnExp
  patch ← traverse (bitraverse mkPropName makeExp) props
  case NE.nonEmpty patch of
    Nothing → throwContextualError EmptyObjectUpdate
    Just ps → pure $ objectUpdate expr ps

mkAbstraction ∷ PS.Ident → CfnExp → RepM Exp
mkAbstraction i e = abstraction arg <$> makeExp e
 where
  arg = case PS.runIdent i of
    "$__unused" → ParamUnused
    n → ParamNamed (Name n)

mkApplication ∷ CfnExp → CfnExp → RepM Exp
mkApplication e1 e2 =
  if isNewtype (Cfn.extractAnn e1)
    then makeExp e2
    else application <$> makeExp e1 <*> makeExp e2

mkQualifiedIdent ∷ PS.Qualified PS.Ident → RepM (Qualified Name)
mkQualifiedIdent (PS.Qualified by ident) =
  gets (Cfn.moduleName . contextModule) <&> \contextModuleName →
    case by of
      PS.BySourcePos _sourcePos → Local $ identToName ident
      PS.ByModuleName modName →
        if modName == contextModuleName
          then Local (identToName ident)
          else Imported modName (identToName ident)

mkRef ∷ PS.Qualified PS.Ident → RepM Exp
mkRef = flip ref 0 <<$>> mkQualifiedIdent

mkLet ∷ [Cfn.Bind Cfn.Ann] → CfnExp → RepM Exp
mkLet binds expr = do
  groupings ∷ NonEmpty (Grouping (Name, Exp)) ←
    NE.nonEmpty binds
      & maybe (throwContextualError LetWithoutBinds) (traverse mkGrouping)
  lets groupings <$> makeExp expr

--------------------------------------------------------------------------------
-- Case statements are compiled to a decision trees (nested if/else's) ---------
-- The algorithm is based on this document: ------------------------------------
-- https://julesjacobs.com/notes/patternmatching/patternmatching.pdf -----------

mkCase
  ∷ [CfnExp]
  → NonEmpty (Cfn.CaseAlternative Cfn.Ann)
  → RepM Exp
mkCase expressions alternatives = do
  (refExpressions, refs) ← expressionsToRefs expressions
  clauses ← traverse (alternativeToClauses refExpressions) alternatives
  refs <$> mkCaseClauses (NE.toList clauses)

expressionsToRefs ∷ [CfnExp] → RepM ([Exp], Exp → Exp)
expressionsToRefs cfnExps =
  traverse makeExp cfnExps >>= fmap declareBinds . foldrM inlineOrReference []
 where
  inlineOrReference
    ∷ Exp
    → [Either Exp (Name, Exp)]
    -- Either an expression to inline, or a named expression reference.
    → RepM [Either Exp (Name, Exp)]
  inlineOrReference e acc =
    case e of
      LiteralInt _ → inlineExpr
      LiteralFloat _ → inlineExpr
      LiteralChar _ → inlineExpr
      LiteralBool _ → inlineExpr
      Ref _name _index → inlineExpr
      _ → referenceExpr
   where
    inlineExpr = pure (Left e : acc)
    referenceExpr = (: acc) . Right . (,e) <$> generateName "e"

  -- Declare extracted references
  declareBinds ∷ [Either Exp (Name, Exp)] → ([Exp], Exp → Exp)
  declareBinds expressionsOrRefs =
    ( either id (flip refLocal 0 . fst) <$> expressionsOrRefs
    , case NE.nonEmpty (mapMaybe rightToMaybe expressionsOrRefs) of
        Nothing → id
        Just refs → lets (Standalone <$> refs)
    )

mkCaseClauses ∷ [CaseClause] → RepM Exp
mkCaseClauses = mkClauses mempty
 where
  mkClauses ∷ MatchHistory → [CaseClause] → RepM Exp
  mkClauses history = \case
    [] → pure $ exception "No patterns matched"
    clause : restClauses → do
      mkClause
        history
        clause
        (`matchChosenByHeuristic` restClauses)
        (`mkClauses` restClauses)

  mkClause
    ∷ MatchHistory
    → CaseClause
    → (CaseClause → Maybe (Match, CaseClause))
    → (MatchHistory → RepM Exp)
    → RepM Exp
  mkClause history currentClause heuristic nextClause =
    case heuristic currentClause of
      Nothing →
        -- All matches for this clause have passed
        case NE.nonEmpty (usedClauseBinds currentClause) of
          Nothing → do
            next ← nextClause history
            pure case clauseResult currentClause of
              Right result → result
              Left guardedResults →
                foldr (uncurry ifThenElse) next guardedResults
          Just binds → do
            n ← generateName "n"
            next ← nextClause history
            pure case clauseResult currentClause of
              Right result → lets binds result
              Left guardedResults →
                lets (Standalone (n, next) <| binds) $
                  foldr (uncurry ifThenElse) (refLocal n 0) guardedResults
      Just (Match {..}, clause) →
        let expr = foldr applyStep matchExp stepsToFocus
            clause' =
              clause
                { clauseBinds =
                    (Standalone . (,expr) <$> matchBinds) <> clauseBinds clause
                , clauseMatches =
                    nestedMatches <> clauseMatches clause
                }
         in case matchPat of
              PatAny →
                nextMatch history clause'
              PatArrayLength (fromIntegral → len) →
                ifThenElse (literalInt len `eq` arrayLength expr)
                  <$> nextMatch history clause'
                  <*> nextClause history
              PatInteger i →
                ifThenElse (literalInt i `eq` expr)
                  <$> nextMatch history clause'
                  <*> nextClause history
              PatFloating d →
                ifThenElse (literalFloat d `eq` expr)
                  <$> nextMatch history clause'
                  <*> nextClause history
              PatString s →
                ifThenElse (literalString s `eq` expr)
                  <$> nextMatch history clause'
                  <*> nextClause history
              PatChar c →
                ifThenElse (literalChar c `eq` expr)
                  <$> nextMatch history clause'
                  <*> nextClause history
              PatBoolean b →
                ifThenElse (literalBool b `eq` expr)
                  <$> nextMatch history clause'
                  <*> nextClause history
              PatCtor algTy mn ty ctr → case Map.lookup expr history of
                Just (ctr', True) →
                  if ctr' == ctr
                    then -- This constructor matched positively before,
                    -- proceed matching nested constructors.
                      nextMatch history clause'
                    else -- Other constructor matched positively before,
                    -- this one can't match, proceed to next clause.
                      nextClause history
                Just (ctr', False)
                  | ctr' == ctr →
                      -- This constructor matched negatively before,
                      -- proceed to the next clause.
                      nextClause history
                _ →
                  case algTy of
                    ProductType → nextMatch (history' True) clause'
                    SumType →
                      -- Either this constructor is matched for the first time,
                      -- or other constructor didn't pass the match before.
                      ifThenElse
                        (literalString (ctorId mn ty ctr) `eq` reflectCtor expr)
                        <$> nextMatch (history' True) clause'
                        <*> nextClause (history' False)
                 where
                  history' b = Map.insert expr (ctr, b) history
   where
    nextMatch hist clause = mkClause hist clause heuristic nextClause

usedClauseBinds ∷ CaseClause → [Grouping (Name, Exp)]
usedClauseBinds CaseClause {clauseBinds} = clauseBinds

matchChosenByHeuristic
  ∷ CaseClause → [CaseClause] → Maybe (Match, CaseClause)
matchChosenByHeuristic thisClause otherClauses =
  case clauseMatches thisClause of
    [] → Nothing
    [match] → Just (match, thisClause {clauseMatches = []})
    matches →
      -- select a match that is present in the maximum number of other clauses
      sortOn
        (Down . fst)
        (toFst (countAffectedClauses otherClauses) <$> matches)
        & uncons
        & fmap \(match, remainingMatches) →
          (snd match, thisClause {clauseMatches = snd <$> remainingMatches})
 where
  countAffectedClauses ∷ [CaseClause] → Match → Int
  countAffectedClauses clauses Match {matchExp = expr, stepsToFocus = steps} =
    foldr count 0 clauses
   where
    count ∷ CaseClause → Int → Int
    count clause counter =
      maybe counter (\_ → counter + 1) $
        allClauseMatches clause & find \case
          Match {matchPat = PatAny} → False
          Match {matchExp, stepsToFocus}
            | matchExp == expr, stepsToFocus == steps → True
          _ → False

    allClauseMatches ∷ CaseClause → [Match]
    allClauseMatches CaseClause {clauseMatches} = go [] clauseMatches
     where
      go acc = \case
        [] → acc
        ms → ms >>= \t → go (t : acc) (nestedMatches t)

type Guard = Exp

data CaseClause = CaseClause
  { clauseMatches ∷ [Match]
  , clauseResult ∷ Either [(Guard, Exp)] Exp
  , clauseBinds ∷ [Grouping (Name, Exp)]
  }
  deriving stock (Show)

data Pattern
  = PatAny
  | PatCtor AlgebraicType ModuleName TyName CtorName
  | PatInteger Integer
  | PatFloating Double
  | PatString Text
  | PatChar Char
  | PatBoolean Bool
  | PatArrayLength Int
  deriving stock (Eq, Show)

data Step = TakeIndex Natural | TakeProp PropName
  deriving stock (Eq, Show)

applyStep ∷ Step → Exp → Exp
applyStep step expr =
  case step of
    TakeIndex i → arrayIndex expr i
    TakeProp p → objectProp expr p

data Match = Match
  { matchExp ∷ Exp
  , matchPat ∷ Pattern
  , matchBinds ∷ [Name]
  , stepsToFocus ∷ [Step]
  , nestedMatches ∷ [Match]
  }
  deriving stock (Show)

mkBinder ∷ Exp → Cfn.Binder Cfn.Ann → RepM Match
mkBinder matchExp = go mempty
 where
  go ∷ [Step] → Cfn.Binder Cfn.Ann → RepM Match
  go stepsToFocus = \case
    Cfn.NullBinder _ann →
      pure $ matchWhole PatAny
    Cfn.VarBinder _ann name →
      pure
        Match
          { matchExp
          , matchPat = PatAny
          , stepsToFocus
          , matchBinds = [identToName name]
          , nestedMatches = mempty
          }
    Cfn.ConstructorBinder ann qTypeName qCtorName binders →
      if isNewtype ann
        then case binders of
          [binder] → go stepsToFocus binder
          _ →
            throwContextualError
              NewtypeCtorBinderHasUnexpectedNumberOfNestedBinders
        else do
          nestedMatches ←
            for (zip [0 ..] binders) \(index ∷ Int, binder) →
              let prop = PropName ("value" <> toText (show index))
               in go (TakeProp prop : stepsToFocus) binder

          let qualifiedTypeName = mkQualified mkTyName qTypeName
          Context {contextModule} ← get
          let contextModuleName = Cfn.moduleName contextModule
          (modName, tyName, algTy) ← case qualifiedTypeName of
            Imported modName tyName →
              (modName,tyName,) <$> algebraicTy modName tyName
            Local tyName →
              (contextModuleName,tyName,)
                <$> algebraicTy contextModuleName tyName
          let ctrName = mkCtorName (Names.disqualify qCtorName)
          pure
            Match
              { matchExp
              , matchPat = PatCtor algTy modName tyName ctrName
              , stepsToFocus
              , matchBinds = mempty
              , nestedMatches
              }
    Cfn.LiteralBinder _ann literal →
      case literal of
        Cfn.NumericLiteral (Left i) →
          pure $ matchWhole $ PatInteger i
        Cfn.NumericLiteral (Right d) →
          pure $ matchWhole $ PatFloating d
        Cfn.StringLiteral s →
          pure $ matchWhole $ PatString (decodeStringEscaping s)
        Cfn.CharLiteral c →
          pure $ matchWhole $ PatChar c
        Cfn.BooleanLiteral b →
          pure $ matchWhole $ PatBoolean b
        Cfn.ArrayLiteral binders → do
          nestedMatches ←
            for (zip [0 ..] binders) \(index, binder) →
              go (TakeIndex index : stepsToFocus) binder
          pure
            Match
              { matchExp
              , stepsToFocus
              , matchPat = PatArrayLength (length binders)
              , matchBinds = mempty
              , nestedMatches
              }
        Cfn.ObjectLiteral kvs → do
          nestedMatches ←
            for kvs \(prop, binder) → do
              propName ← mkPropName prop
              go (TakeProp propName : stepsToFocus) binder
          pure
            Match
              { matchExp
              , matchPat = PatAny
              , stepsToFocus
              , matchBinds = mempty
              , nestedMatches
              }
    Cfn.NamedBinder _ann ident binder → do
      match ← go stepsToFocus binder
      let bind = identToName ident
      pure case matchBinds match of
        [] → match {matchBinds = [bind]}
        bs → match {matchBinds = bind : bs}
   where
    matchWhole ∷ Pattern → Match
    matchWhole pat =
      Match
        { matchExp
        , stepsToFocus
        , matchPat = pat
        , matchBinds = mempty
        , nestedMatches = mempty
        }

type MatchHistory = Map Exp (CtorName, Bool)

alternativeToClauses ∷ [Exp] → Cfn.CaseAlternative Cfn.Ann → RepM CaseClause
alternativeToClauses
  localRefs
  Cfn.CaseAlternative {caseAlternativeBinders, caseAlternativeResult} = do
    unless (length localRefs == length caseAlternativeBinders) do
      throwContextualError $
        CaseBindersNumberMismatch
          (Tagged $ length localRefs)
          (Tagged $ length caseAlternativeBinders)

    matches ← for (zip localRefs caseAlternativeBinders) do
      uncurry mkBinder

    clauseResult ←
      bitraverse
        (traverse (bitraverse makeExp makeExp))
        makeExp
        caseAlternativeResult

    pure
      CaseClause
        { clauseResult
        , clauseBinds = []
        , clauseMatches = matches
        }

--------------------------------------------------------------------------------
-- Helper functions ------------------------------------------------------------

generateName ∷ Text → RepM Name
generateName prefix =
  Name <$> do
    ctx@Context {lastGeneratedNameIndex} ← get
    put $ ctx {lastGeneratedNameIndex = lastGeneratedNameIndex + 1}
    pure $ prefix <> toText (show lastGeneratedNameIndex)

isNewtype ∷ Cfn.Ann → Bool
isNewtype = \case
  Just Cfn.IsNewtype → True
  _ → False

isForeign ∷ Cfn.Ann → Bool
isForeign = \case
  Just Cfn.IsForeign → True
  _ → False

algebraicTy ∷ ModuleName → TyName → RepM AlgebraicType
algebraicTy modName tyName = do
  Context {contextDataTypes} ← get
  case Map.lookup (modName, tyName) contextDataTypes of
    Just (algTy, _ctorFields) → pure algTy
    Nothing → throwContextualError $ TypeNotDeclared contextDataTypes tyName

--------------------------------------------------------------------------------
-- Errors ----------------------------------------------------------------------

throwContextualError ∷ CoreFnErrorReason → RepM a
throwContextualError e = do
  currentModule ← gets (contextModule >>> Cfn.moduleName)
  throwError $ CoreFnError currentModule e

data CoreFnError = CoreFnError
  { currentModule ∷ ModuleName
  , reason ∷ CoreFnErrorReason
  }

instance Show CoreFnError where
  show CoreFnError {currentModule, reason} =
    "in module "
      <> toString (runModuleName currentModule)
      <> ": "
      <> show reason

data CoreFnErrorReason
  = EmptyExportList
  | EmptyObjectUpdate
  | NoDeclarations
  | LetWithoutBinds
  | EmptyCase (Cfn.Expr Cfn.Ann)
  | EmptyBindingGroup
  | NewtypeCtorBinderHasUnexpectedNumberOfNestedBinders
  | CaseBindersNumberMismatch (Tagged "expressions" Int) (Tagged "binders" Int)
  | TypeNotDeclared
      (Map (ModuleName, TyName) (AlgebraicType, Map CtorName [FieldName]))
      TyName
  | UnicodeDecodeError UnicodeException

instance Show CoreFnErrorReason where
  show = \case
    EmptyExportList →
      "Empty export list"
    EmptyObjectUpdate →
      "Empty object update"
    NoDeclarations →
      "No declarations"
    LetWithoutBinds →
      "Let without binds"
    EmptyCase _ →
      "Empty case"
    EmptyBindingGroup →
      "Empty binding group"
    NewtypeCtorBinderHasUnexpectedNumberOfNestedBinders →
      "Newtype constructor binder has unexpected number of nested binders"
    CaseBindersNumberMismatch (Tagged exprs) (Tagged binders) →
      "Number of expressions ("
        <> show exprs
        <> ") and binders ("
        <> show binders
        <> ") in case alternative mismatch"
    TypeNotDeclared decls tyName →
      "Type not declared: "
        <> show tyName
        <> ".\n Known types: "
        <> toString (pShow decls)
    UnicodeDecodeError e →
      "Unicode decode error: " <> displayException e
