{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.Backend.IR.Types where

import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)
import Data.Map qualified as Map
import Fmt ((+||), (||+))
import Quiet (Quiet (..))
import Text.Show (show)
import Prelude hiding (show)

data Module = Module
  { moduleName :: ModuleName
  , moduleBindings :: [Grouping (Name, Exp)]
  , moduleImports :: [ModuleName]
  , moduleExports :: [Name]
  , moduleReExports :: Map ModuleName [Name]
  , moduleForeigns :: [Name]
  , modulePath :: FilePath
  , dataTypes :: Map TyName (AlgebraicType, Map CtorName [FieldName])
  }

data Grouping a
  = Standalone a
  | RecursiveGroup (NonEmpty a)
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

listGrouping :: Grouping a -> [a]
listGrouping = \case
  Standalone a -> [a]
  RecursiveGroup as -> toList as

bindingNames :: Grouping (name, exp) -> [name]
bindingNames = fmap fst . listGrouping

bindingExprs :: Grouping (name, Exp) -> [Exp]
bindingExprs = fmap snd . listGrouping

newtype Info = Info {refsFree :: Map (Qualified Name) Natural}

instance Semigroup Info where
  Info a <> Info b = Info (Map.unionWith (+) a b)

instance Monoid Info where
  mempty = Info mempty

data Exp = Exp
  { expInfo :: Info
  , unExp :: ExpF Exp
  }

data ExpF a
  = Lit (Literal a)
  | Prim (PrimOp a)
  | Ctor AlgebraicType TyName CtorName [FieldName]
  | ArrayIndex a Natural
  | ObjectProp a PropName
  | ObjectUpdate a (NonEmpty (PropName, a))
  | Abs Parameter a
  | App a a
  | Ref (Qualified Name) Index
  | Let (NonEmpty (Grouping (Name, a))) a
  | IfThenElse a a a
  | Exception Text
  deriving stock (Eq, Functor)

newtype Index = Index {unIndex :: Natural}
  deriving newtype (Show, Eq, Ord, Num, Enum, Real, Integral)

data Parameter = ParamUnused | ParamNamed Name
  deriving stock (Show, Eq, Ord)

paramName :: Parameter -> Maybe Name
paramName ParamUnused = Nothing
paramName (ParamNamed name) = Just name

data Literal a
  = Integer Integer
  | Floating Double
  | String Text
  | Char Char
  | Boolean Bool
  | Array [a]
  | Object [(PropName, a)]
  deriving stock (Show, Eq, Ord, Functor)

isScalar :: Literal a -> Bool
isScalar = \case
  Integer _ -> True
  Floating _ -> True
  String _ -> True
  Char _ -> True
  Boolean _ -> True
  Array _as -> False
  Object _ps -> False

data PrimOp a
  = ArrayLength a
  | ReflectCtor a
  | DataArgumentByIndex Natural a
  | Eq a a
  deriving stock (Eq, Ord, Functor)

data AlgebraicType = SumType | ProductType
  deriving stock (Generic, Eq, Ord, Show, Enum, Bounded)

--------------------------------------------------------------------------------
-- Names -----------------------------------------------------------------------

newtype Name = Name {nameToText :: Text}
  deriving newtype (Eq, Ord)
  deriving stock (Generic)
  deriving (Show) via (Quiet Name)

newtype ModuleName = ModuleName {renderModuleName :: Text}
  deriving newtype (Eq, Ord)
  deriving stock (Generic)
  deriving (Show) via (Quiet ModuleName)

data QName = QName {qnameModuleName :: ModuleName, qnameName :: Name}
  deriving stock (Eq, Ord, Show)

newtype TyName = TyName {renderTyName :: Text}
  deriving newtype (Eq, Ord)
  deriving stock (Generic)
  deriving (Show) via (Quiet TyName)

newtype CtorName = CtorName {renderCtorName :: Text}
  deriving newtype (Eq, Ord)
  deriving stock (Generic)
  deriving (Show) via (Quiet CtorName)

newtype FieldName = FieldName {renderFieldName :: Text}
  deriving newtype (Eq, Ord)
  deriving stock (Generic)
  deriving (Show) via (Quiet FieldName)

newtype PropName = PropName {renderPropName :: Text}
  deriving newtype (Eq, Ord)
  deriving stock (Generic)
  deriving (Show) via (Quiet PropName)

data Qualified a = Local a | Imported ModuleName a
  deriving stock (Show, Eq, Ord, Functor)

--------------------------------------------------------------------------------
-- Instances -------------------------------------------------------------------

$(deriveEq1 ''PrimOp)
$(deriveEq1 ''Literal)
$(deriveEq1 ''Grouping)
$(deriveEq1 ''ExpF)

$(deriveOrd1 ''PrimOp)
$(deriveOrd1 ''Literal)
$(deriveOrd1 ''Grouping)
$(deriveOrd1 ''ExpF)

$(deriveShow1 ''PrimOp)
$(deriveShow1 ''Literal)
$(deriveShow1 ''Grouping)

instance Show a => Show (PrimOp a) where
  show = \case
    ArrayLength a ->
      "ArrayLength (" +|| a ||+ ")"
    ReflectCtor a ->
      "ReflectCtor (" +|| a ||+ ")"
    DataArgumentByIndex index a ->
      "DataArgumentByIndex (" +|| index ||+ ") (" +|| a ||+ ")"
    Eq a b ->
      "Eq (" +|| a ||+ ") (" +|| b ||+ ")"

deriving stock instance Show Info

instance Show a => Show (ExpF a) where
  show :: ExpF a -> String
  show = \case
    Lit literal ->
      "Lit (" +|| literal ||+ ")"
    Prim primOp ->
      "Prim (" +|| primOp ||+ ")"
    Ctor algebraicType tyName ctorName fieldNames ->
      "Ctor ("
        +|| algebraicType
        ||+ ") ("
        +|| tyName
        ||+ ") ("
        +|| ctorName
        ||+ ") ("
        +|| fieldNames
        ||+ ")"
    ArrayIndex a index ->
      "ArrayIndex (" +|| a ||+ ") (" +|| index ||+ ")"
    ObjectProp a propName ->
      "ObjectProp (" +|| a ||+ ") (" +|| propName ||+ ")"
    ObjectUpdate a patches ->
      "ObjectUpdate (" +|| a ||+ ") (" +|| patches ||+ ")"
    Abs argument a ->
      "Abs (" +|| argument ||+ ") (" +|| a ||+ ")"
    App a b ->
      "App (" +|| a ||+ ") (" +|| b ||+ ")"
    Ref qname index ->
      "Ref (" +|| qname ||+ ") (" +|| index ||+ ")"
    Let bindings a ->
      "Let (" +|| bindings ||+ ") (" +|| a ||+ ")"
    IfThenElse p t e ->
      "IfThenElse (" +|| p ||+ ") (" +|| t ||+ ") (" +|| e ||+ ")"
    Exception msg ->
      "Exception (" +|| msg ||+ ")"

instance Show Exp where
  show :: Exp -> String
  show = unExp >>> show

-- deriving stock instance Show a => Show (ExpF a)

deriving stock instance Show Module

deriving stock instance Eq Info

deriving stock instance Eq Exp

deriving stock instance Eq Module

deriving stock instance Ord Info

deriving stock instance Ord Exp

deriving stock instance Ord a => Ord (ExpF a)

deriving stock instance Ord Module

-- Constructors for expresssions -----------------------------------------------

wrapExpF :: ExpF Exp -> Exp
wrapExpF e = flip Exp e case e of
  Lit (Array as) -> foldMap expInfo as
  Lit (Object ps) -> foldMap (expInfo . snd) ps
  Prim op -> case op of
    ArrayLength a -> expInfo a
    ReflectCtor a -> expInfo a
    DataArgumentByIndex _idx a -> expInfo a
    Eq a b -> expInfo a <> expInfo b
  ArrayIndex a _indx -> expInfo a
  ObjectProp a _prop -> expInfo a
  ObjectUpdate a patches -> expInfo a <> foldMap (expInfo . snd) patches
  App f x -> expInfo f <> expInfo x
  Ref qname _index -> Info {refsFree = Map.singleton qname 1}
  Abs param expr ->
    case param of
      ParamUnused -> oldInfo
      ParamNamed name -> oldInfo {refsFree = Map.delete (Local name) refsFree}
   where
    oldInfo@Info {refsFree} = expInfo expr
  Let binds body ->
    info {refsFree = foldr (Map.delete . Local) refsFree boundNames}
   where
    info@Info {refsFree} = foldMap (expInfo . snd) namedExprs <> expInfo body
    namedExprs = listGrouping =<< toList binds
    boundNames = fst <$> namedExprs
  IfThenElse p th el -> expInfo p <> expInfo th <> expInfo el
  _ -> mempty

arrayIndex :: Exp -> Nat -> Exp
arrayIndex = (wrapExpF .) . ArrayIndex

objectProp :: Exp -> PropName -> Exp
objectProp = (wrapExpF .) . ObjectProp

objectUpdate :: Exp -> NonEmpty (PropName, Exp) -> Exp
objectUpdate = (wrapExpF .) . ObjectUpdate

ctor :: AlgebraicType -> TyName -> CtorName -> [FieldName] -> Exp
ctor = (((wrapExpF .) .) .) . Ctor

abstraction :: Parameter -> Exp -> Exp
abstraction param = wrapExpF . Abs param

identity :: Exp
identity = abstraction (ParamNamed name) (refLocal name 0) where name = Name "x"

lets :: NonEmpty (Grouping (Name, Exp)) -> Exp -> Exp
lets binds body = wrapExpF $ Let binds body

application :: Exp -> Exp -> Exp
application = (wrapExpF .) . App

ref :: Qualified Name -> Index -> Exp
ref qname index =
  case qname of
    Local name -> refLocal name index
    Imported modname name -> refImported modname name index

refLocal :: Name -> Index -> Exp
refLocal name index = wrapExpF (Ref (Local name) index)

refLocal0 :: Name -> Exp
refLocal0 name = refLocal name (Index 0)

refImported :: ModuleName -> Name -> Index -> Exp
refImported modname name index = wrapExpF (Ref (Imported modname name) index)

ifThenElse :: Exp -> Exp -> Exp -> Exp
ifThenElse = ((wrapExpF .) .) . IfThenElse

exception :: Text -> Exp
exception = wrapExpF . Exception

--------------------------------------------------------------------------------
-- Constructors for primitive operations ---------------------------------------

prim :: PrimOp Exp -> Exp
prim = wrapExpF . Prim

eq :: Exp -> Exp -> Exp
eq = (prim .) . Eq

arrayLength :: Exp -> Exp
arrayLength = prim . ArrayLength

reflectCtor :: Exp -> Exp
reflectCtor = prim . ReflectCtor

dataArgumentByIndex :: Natural -> Exp -> Exp
dataArgumentByIndex = (prim .) . DataArgumentByIndex

--------------------------------------------------------------------------------
-- Constructors for literals ---------------------------------------------------

boolean :: Bool -> Exp
boolean = wrapExpF . Lit . Boolean

integer :: Integer -> Exp
integer = wrapExpF . Lit . Integer

float :: Double -> Exp
float = wrapExpF . Lit . Floating

string :: Text -> Exp
string = wrapExpF . Lit . String

char :: Char -> Exp
char = wrapExpF . Lit . Char

array :: [Exp] -> Exp
array = wrapExpF . Lit . Array

object :: [(PropName, Exp)] -> Exp
object = wrapExpF . Lit . Object

--------------------------------------------------------------------------------
-- Traversals ------------------------------------------------------------------

traverseExpM
  :: forall m. Monad m => (Exp -> m ()) -> (forall x. m x -> m x) -> Exp -> m ()
traverseExpM visit around = go
 where
  go :: Exp -> m ()
  go e = around case unExp e of
    Lit (Array as) -> traverse_ go as *> visit e
    Lit (Object props) -> traverse (traverse go) props *> visit e
    Prim op ->
      case op of
        ArrayLength a -> go a *> visit e
        ReflectCtor a -> go a *> visit e
        DataArgumentByIndex _idx a -> go a *> visit e
        Eq a b -> go a *> go b *> visit e
    ArrayIndex a _indx -> go a *> visit e
    ObjectProp a _prop -> go a *> visit e
    ObjectUpdate a patches -> go a *> traverse (traverse go) patches *> visit e
    App a b -> go a *> go b *> visit e
    Abs _arg a -> go a *> visit e
    Let binds body ->
      traverse (traverse (traverse go)) binds *> go body *> visit e
    IfThenElse p th el -> go p *> go th *> go el *> visit e
    _ -> visit e

data RewriteMod = Recurse | Stop
  deriving stock (Show, Eq, Ord)

instance Semigroup RewriteMod where
  Recurse <> Recurse = Recurse
  _ <> _ = Stop

data Rewritten a = NoChange | Rewritten RewriteMod a
  deriving stock (Show, Eq, Ord, Functor)

instance Applicative Rewritten where
  pure :: forall a. a -> Rewritten a
  pure = Rewritten Stop
  NoChange <*> _ = NoChange
  _ <*> NoChange = NoChange
  Rewritten rmf f <*> Rewritten rma a = Rewritten (rmf <> rma) (f a)

instance Monad Rewritten where
  NoChange >>= _ = NoChange
  Rewritten m a >>= f = case f a of
    NoChange -> NoChange
    Rewritten m' a' -> Rewritten (m <> m') a'

instance Alternative Rewritten where
  empty = NoChange
  NoChange <|> a = a
  a <|> _ = a

type RewriteRule = RewriteRuleM Identity
type RewriteRuleM m = Exp -> m (Rewritten Exp)

thenRewrite :: Monad m => RewriteRuleM m -> RewriteRuleM m -> RewriteRuleM m
thenRewrite rewrite1 rewrite2 e =
  rewrite1 e >>= \case
    Rewritten m' e' ->
      rewrite2 e' <&> \case
        NoChange -> Rewritten m' e'
        Rewritten m'' e'' -> Rewritten (m' <> m'') e''
    NoChange -> rewrite2 e

rewriteExpTopDown :: RewriteRule -> Exp -> Exp
rewriteExpTopDown rewrite = runIdentity . rewriteExpTopDownM rewrite

rewriteExpTopDownM :: Monad m => RewriteRuleM m -> Exp -> m Exp
rewriteExpTopDownM rewrite = visit
 where
  visit expression =
    rewrite expression >>= \case
      NoChange -> descendInto expression
      Rewritten Stop expression' -> pure expression'
      Rewritten Recurse expression' -> descendInto expression'

  descendInto e = case unExp e of
    Lit (Array as) -> array <$> traverse visit as
    Lit (Object props) -> object <$> traverse (traverse visit) props
    Prim op ->
      case op of
        ArrayLength a -> arrayLength <$> visit a
        ReflectCtor a -> reflectCtor <$> visit a
        DataArgumentByIndex idx a -> dataArgumentByIndex idx <$> visit a
        Eq a b -> eq <$> visit a <*> visit b
    ArrayIndex a indx -> flip arrayIndex indx <$> visit a
    ObjectProp a prop -> flip objectProp prop <$> visit a
    ObjectUpdate a patches ->
      (Exp (expInfo e) .) . ObjectUpdate
        <$> visit a
        <*> traverse (traverse visit) patches
    App a b -> application <$> visit a <*> visit b
    Abs param expr -> Exp (expInfo e) . Abs param <$> visit expr
    Let binds body ->
      (Exp (expInfo e) .) . Let
        <$> traverse (traverse (traverse visit)) binds
        <*> visit body
    IfThenElse p th el -> ifThenElse <$> visit p <*> visit th <*> visit el
    _ -> pure e

countFreeRefs :: Qualified Name -> Exp -> Natural
countFreeRefs qname e =
  fromMaybe 0 $ Map.lookup qname (refsFree (expInfo e))

subst :: Exp -> Qualified Name -> Exp -> Exp
subst body name = substitute body name 0

-- | Substitute the given variable name and index with an expression
substitute
  :: Exp
  -- ^ The expression to substitute into
  -> Qualified Name
  -- ^ The name of the variable to replace
  -> Index
  -- ^ The index of the variable to replace
  -> Exp
  -- ^ The expression to substitute in place of the given variable
  -> Exp
substitute expression name index replacement =
  case unExp expression of
    Lit (Array as) -> array (as <&> \a -> substitute a name index replacement)
    Lit (Object props) ->
      object (props <&> fmap \p -> substitute p name index replacement)
    Prim op ->
      case op of
        ArrayLength a -> arrayLength (substitute a name index replacement)
        ReflectCtor a -> reflectCtor (substitute a name index replacement)
        DataArgumentByIndex idx a ->
          dataArgumentByIndex idx (substitute a name index replacement)
        Eq a b ->
          eq
            (substitute a name index replacement)
            (substitute b name index replacement)
    ArrayIndex a indx -> arrayIndex (substitute a name index replacement) indx
    ObjectProp a prop -> objectProp (substitute a name index replacement) prop
    ObjectUpdate a patches ->
      objectUpdate
        (substitute a name index replacement)
        (patches <&> fmap \p -> substitute p name index replacement)
    Ref name' index'
      | name == name' && index == index' -> replacement
      | otherwise -> ref name' index'
    Abs argument@(ParamNamed argName) body ->
      abstraction argument (substitute body name index' replacement')
     where
      index' = if name == Local argName then index + 1 else index
      replacement' = shift 1 argName 0 replacement
    Let binds body ->
      lets binds' body'
     where
      binds' =
        binds <&> \grouping ->
          case grouping of
            Standalone (boundName, expr) ->
              Standalone
                ( boundName
                , substitute
                    expr
                    name
                    (if name == Local boundName then index + 1 else index)
                    (shift 1 boundName 0 replacement)
                )
            RecursiveGroup recBinds ->
              let
                boundNames = fst <$> grouping
                index' =
                  if name `elem` fmap Local boundNames then index + 1 else index
                replacement' =
                  foldr (\n r -> shift 1 n 0 r) replacement boundNames
               in
                RecursiveGroup $
                  recBinds <&> \(boundName, expr) ->
                    (boundName, substitute expr name index' replacement')
      body' = substitute body name index' replacement'
       where
        boundNames = toList binds >>= bindingNames
        index' = if name `elem` fmap Local boundNames then index + 1 else index
        replacement' = foldr (\n r -> shift 1 n 0 r) replacement boundNames
    App argument function ->
      application
        (substitute argument name index replacement)
        (substitute function name index replacement)
    IfThenElse p th el ->
      ifThenElse
        (substitute p name index replacement)
        (substitute th name index replacement)
        (substitute el name index replacement)
    _ -> expression

-- | Increase the index of all bound variables matching the given variable name
shift
  :: Int
  -- ^ The amount to shift by
  -> Name
  -- ^ The variable name to match (a.k.a. the namespace)
  -> Index
  -- ^ The minimum bound for which indices to shift
  -> Exp
  -- ^ The expression to shift
  -> Exp
shift offset namespace minIndex expression =
  case unExp expression of
    Ref (Local name) index ->
      refLocal name $
        if name == namespace && minIndex <= index
          then index + fromIntegral offset
          else index
    Abs argument body -> abstraction argument body'
     where
      minIndex'
        | paramName argument == Just namespace = minIndex + 1
        | otherwise = minIndex

      body' = shift offset namespace minIndex' body
    App function argument ->
      application
        (shift offset namespace minIndex argument)
        (shift offset namespace minIndex function)
    _ -> expression
