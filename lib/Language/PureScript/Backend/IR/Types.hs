{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.Backend.IR.Types where

import Control.Lens (Prism', prism')
import Data.Deriving (deriveEq1, deriveOrd1)
import Data.Map qualified as Map
import Data.MonoidMap (MonoidMap)
import Data.MonoidMap qualified as MMap
import Language.PureScript.Names (ModuleName, runModuleName)
import Quiet (Quiet (..))
import Prelude hiding (show)

data Module = Module
  { moduleName ∷ ModuleName
  , moduleBindings ∷ [Grouping (Name, Exp)]
  , moduleImports ∷ [ModuleName]
  , moduleExports ∷ [Name]
  , moduleReExports ∷ Map ModuleName [Name]
  , moduleForeigns ∷ [Name]
  , modulePath ∷ FilePath
  , dataTypes ∷ Map TyName (AlgebraicType, Map CtorName [FieldName])
  }

data Grouping a
  = Standalone a
  | RecursiveGroup (NonEmpty a)
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

listGrouping ∷ Grouping a → [a]
listGrouping = \case
  Standalone a → [a]
  RecursiveGroup as → toList as

bindingNames ∷ Grouping (name, exp) → [name]
bindingNames = fmap fst . listGrouping

bindingExprs ∷ Grouping (name, Exp) → [Exp]
bindingExprs = fmap snd . listGrouping

newtype Info = Info {refsFree ∷ Map (Qualified Name) Natural}

instance Semigroup Info where
  Info a <> Info b = Info (Map.unionWith (+) a b)

instance Monoid Info where
  mempty = Info mempty

type Annotated n f = n (f n)

data RawExp (n ∷ Type → Type)
  = LiteralInt Integer
  | LiteralFloat Double
  | LiteralString Text
  | LiteralChar Char
  | LiteralBool Bool
  | LiteralArray [Annotated n RawExp]
  | LiteralObject [(PropName, Annotated n RawExp)]
  | Ctor AlgebraicType TyName CtorName [FieldName]
  | ReflectCtor (Annotated n RawExp)
  | Eq (Annotated n RawExp) (Annotated n RawExp)
  | DataArgumentByIndex Natural (Annotated n RawExp)
  | ArrayLength (Annotated n RawExp)
  | ArrayIndex (Annotated n RawExp) Natural
  | ObjectProp (Annotated n RawExp) PropName
  | ObjectUpdate (Annotated n RawExp) (NonEmpty (PropName, Annotated n RawExp))
  | Abs (n Parameter) (Annotated n RawExp)
  | App (Annotated n RawExp) (Annotated n RawExp)
  | Ref (Qualified Name) Index
  | Let (NonEmpty (Grouping (n Name, Annotated n RawExp))) (Annotated n RawExp)
  | IfThenElse (Annotated n RawExp) (Annotated n RawExp) (Annotated n RawExp)
  | Exception Text

type Exp = RawExp Identity

deriving stock instance Show Exp
deriving stock instance Eq Exp
deriving stock instance Ord Exp

unAnn ∷ ∀ {a}. Identity a → a
unAnn = runIdentity

newtype Index = Index {unIndex ∷ Natural}
  deriving newtype (Show, Eq, Ord, Num, Enum, Real, Integral)

data Parameter = ParamUnused | ParamNamed Name
  deriving stock (Show, Eq, Ord)

paramName ∷ Parameter → Maybe Name
paramName ParamUnused = Nothing
paramName (ParamNamed name) = Just name

isLiteral ∷ Exp → Bool
isLiteral = (||) <$> isNonRecursiveLiteral <*> isRecursiveLiteral

isNonRecursiveLiteral ∷ Exp → Bool
isNonRecursiveLiteral = \case
  LiteralInt {} → True
  LiteralFloat {} → True
  LiteralString {} → True
  LiteralChar {} → True
  LiteralBool {} → True
  _ → False

isRecursiveLiteral ∷ Exp → Bool
isRecursiveLiteral = \case
  LiteralArray {} → True
  LiteralObject {} → True
  _ → False

data AlgebraicType = SumType | ProductType
  deriving stock (Generic, Eq, Ord, Show, Enum, Bounded)

--------------------------------------------------------------------------------
-- Names -----------------------------------------------------------------------

newtype Name = Name {nameToText ∷ Text}
  deriving newtype (Eq, Ord)
  deriving stock (Generic)
  deriving (Show) via (Quiet Name)

data QName = QName {qnameModuleName ∷ ModuleName, qnameName ∷ Name}
  deriving stock (Eq, Ord, Show)

printQName ∷ QName → Text
printQName QName {..} =
  runModuleName qnameModuleName <> "∷" <> nameToText qnameName

newtype TyName = TyName {renderTyName ∷ Text}
  deriving newtype (Eq, Ord)
  deriving stock (Generic)
  deriving (Show) via (Quiet TyName)

newtype CtorName = CtorName {renderCtorName ∷ Text}
  deriving newtype (Eq, Ord)
  deriving stock (Generic)
  deriving (Show) via (Quiet CtorName)

-- TODO: is it used at all?
newtype FieldName = FieldName {renderFieldName ∷ Text}
  deriving newtype (Eq, Ord)
  deriving stock (Generic)
  deriving (Show) via (Quiet FieldName)

newtype PropName = PropName {renderPropName ∷ Text}
  deriving newtype (Eq, Ord)
  deriving stock (Generic)
  deriving (Show) via (Quiet PropName)

data Qualified a = Local a | Imported ModuleName a
  deriving stock (Show, Eq, Ord, Functor)

qualifiedQName ∷ QName → Qualified Name
qualifiedQName QName {qnameModuleName, qnameName} =
  Imported qnameModuleName qnameName

--------------------------------------------------------------------------------
-- Instances -------------------------------------------------------------------

$(deriveEq1 ''Grouping)
$(deriveOrd1 ''Grouping)

deriving stock instance Show Module

deriving stock instance Eq Module

deriving stock instance Ord Module

-- Constructors for expresssions -----------------------------------------------

arrayIndex ∷ Applicative n ⇒ RawExp n → Natural → RawExp n
arrayIndex e = ArrayIndex (pure e)

objectProp ∷ Applicative n ⇒ RawExp n → PropName → RawExp n
objectProp o = ObjectProp (pure o)

objectUpdate ∷ Exp → NonEmpty (PropName, Exp) → Exp
objectUpdate e ps = ObjectUpdate (pure e) (pure <<$>> ps)

ctor ∷ AlgebraicType → TyName → CtorName → [FieldName] → Exp
ctor = Ctor

abstraction ∷ Applicative n ⇒ Parameter → RawExp n → RawExp n
abstraction p e = Abs (pure p) (pure e)

identity ∷ Exp
identity = abstraction (ParamNamed name) (refLocal name 0) where name = Name "x"

lets ∷ Applicative n ⇒ NonEmpty (Grouping (Name, RawExp n)) → RawExp n → RawExp n
lets binds body = Let (bimap pure pure <<$>> binds) (pure body)

application ∷ Applicative n ⇒ RawExp n → RawExp n → RawExp n
application a b = App (pure a) (pure b)

ref ∷ Qualified Name → Index → RawExp n
ref qname index =
  case qname of
    Local name → refLocal name index
    Imported modname name → refImported modname name index

refLocal ∷ Name → Index → RawExp n
refLocal name = Ref (Local name)

refLocal0 ∷ Name → RawExp n
refLocal0 name = refLocal name (Index 0)

refImported ∷ ModuleName → Name → Index → RawExp n
refImported modname name = Ref (Imported modname name)

ifThenElse ∷ Applicative n ⇒ RawExp n → RawExp n → RawExp n → RawExp n
ifThenElse i t e = IfThenElse (pure i) (pure t) (pure e)

exception ∷ Text → RawExp n
exception = Exception

--------------------------------------------------------------------------------
-- Constructors for primitive operations ---------------------------------------

eq ∷ Applicative n ⇒ RawExp n → RawExp n → RawExp n
eq a b = Eq (pure a) (pure b)

arrayLength ∷ Applicative n ⇒ RawExp n → RawExp n
arrayLength = ArrayLength . pure

reflectCtor ∷ Applicative n ⇒ RawExp n → RawExp n
reflectCtor = ReflectCtor . pure

dataArgumentByIndex ∷ Applicative n ⇒ Natural → RawExp n → RawExp n
dataArgumentByIndex n e = DataArgumentByIndex n (pure e)

--------------------------------------------------------------------------------
-- Constructors for literals ---------------------------------------------------

literalBool ∷ Bool → RawExp n
literalBool = LiteralBool

asLiteralBool ∷ Prism' (RawExp n) Bool
asLiteralBool = prism' literalBool \case
  LiteralBool b → Just b
  _ → Nothing

literalInt ∷ Integer → RawExp n
literalInt = LiteralInt

asLiteralInt ∷ Prism' (RawExp n) Integer
asLiteralInt = prism' literalInt \case
  LiteralInt i → Just i
  _ → Nothing

literalFloat ∷ Double → RawExp n
literalFloat = LiteralFloat

asLiteralFloat ∷ Prism' (RawExp n) Double
asLiteralFloat = prism' literalFloat \case
  LiteralFloat f → Just f
  _ → Nothing

literalString ∷ Text → RawExp n
literalString = LiteralString

asLiteralString ∷ Prism' (RawExp n) Text
asLiteralString = prism' literalString \case
  LiteralString s → Just s
  _ → Nothing

literalChar ∷ Char → RawExp n
literalChar = LiteralChar

asLiteralChar ∷ Prism' (RawExp n) Char
asLiteralChar = prism' literalChar \case
  LiteralChar c → Just c
  _ → Nothing

literalArray ∷ Applicative n ⇒ [RawExp n] → RawExp n
literalArray = LiteralArray . fmap pure

literalObject ∷ Applicative n ⇒ [(PropName, RawExp n)] → RawExp n
literalObject ps = LiteralObject (pure <<$>> ps)

--------------------------------------------------------------------------------
-- Traversals ------------------------------------------------------------------

traverseExpBottomUp
  ∷ ∀ n m
   . (Monad m, Traversable n)
  ⇒ (RawExp n → m (RawExp n))
  → RawExp n
  → m (RawExp n)
traverseExpBottomUp visit = go
 where
  go ∷ RawExp n → m (RawExp n)
  go e =
    visit =<< case e of
      LiteralArray as →
        LiteralArray <$> traverse (traverse go) as
      LiteralObject props →
        LiteralObject <$> traverse (traverse (traverse go)) props
      ReflectCtor a →
        ReflectCtor <$> traverse go a
      DataArgumentByIndex idx a →
        DataArgumentByIndex idx <$> traverse go a
      Eq a b →
        Eq <$> traverse go a <*> traverse go b
      ArrayLength a →
        ArrayLength <$> traverse go a
      ArrayIndex a idx →
        flip ArrayIndex idx <$> traverse go a
      ObjectProp a prp →
        flip ObjectProp prp <$> traverse go a
      ObjectUpdate a ps →
        ObjectUpdate <$> traverse go a <*> traverse (traverse (traverse go)) ps
      App a b →
        App <$> traverse go a <*> traverse go b
      Abs arg a →
        Abs arg <$> traverse go a
      Let bs body →
        Let
          <$> traverse (traverse (traverse (traverse go))) bs
          <*> traverse go body
      IfThenElse p th el →
        IfThenElse <$> traverse go p <*> traverse go th <*> traverse go el
      _ → pure e

data RewriteMod = Recurse | Stop
  deriving stock (Show, Eq, Ord)

instance Semigroup RewriteMod where
  Recurse <> Recurse = Recurse
  _ <> _ = Stop

data Rewritten a = NoChange | Rewritten RewriteMod a
  deriving stock (Show, Eq, Ord, Functor)

instance Applicative Rewritten where
  pure ∷ ∀ a. a → Rewritten a
  pure = Rewritten Stop
  NoChange <*> _ = NoChange
  _ <*> NoChange = NoChange
  Rewritten rmf f <*> Rewritten rma a = Rewritten (rmf <> rma) (f a)

instance Monad Rewritten where
  NoChange >>= _ = NoChange
  Rewritten m a >>= f = case f a of
    NoChange → NoChange
    Rewritten m' a' → Rewritten (m <> m') a'

instance Alternative Rewritten where
  empty = NoChange
  NoChange <|> a = a
  a <|> _ = a

type RewriteRule = RewriteRuleM Identity Identity
type RewriteRuleM m n = RawExp n → m (Rewritten (RawExp n))

thenRewrite ∷ Monad m ⇒ RewriteRuleM m n → RewriteRuleM m n → RewriteRuleM m n
thenRewrite rewrite1 rewrite2 e =
  rewrite1 e >>= \case
    Rewritten m' e' → do
      rewrite2 e' <&> \case
        NoChange → Rewritten m' e'
        Rewritten m'' e'' → Rewritten (m' <> m'') e''
    NoChange → rewrite2 e

rewriteExpTopDown
  ∷ Traversable n ⇒ RewriteRuleM Identity n → RawExp n → RawExp n
rewriteExpTopDown rewrite = runIdentity . rewriteExpTopDownM rewrite

rewriteExpTopDownM
  ∷ (Monad m, Traversable n) ⇒ RewriteRuleM m n → RawExp n → m (RawExp n)
rewriteExpTopDownM rewrite = visit
 where
  visit expression =
    rewrite expression >>= \case
      NoChange → descendInto expression
      Rewritten Stop expression' → pure expression'
      Rewritten Recurse expression' → descendInto expression'

  descendInto e = case e of
    LiteralArray as →
      LiteralArray <$> traverse (traverse visit) as
    LiteralObject props →
      LiteralObject <$> traverse (traverse (traverse visit)) props
    ReflectCtor a →
      ReflectCtor <$> traverse visit a
    DataArgumentByIndex idx a →
      DataArgumentByIndex idx <$> traverse visit a
    Eq a b →
      Eq <$> traverse visit a <*> traverse visit b
    ArrayLength a →
      ArrayLength <$> traverse visit a
    ArrayIndex a indx →
      flip ArrayIndex indx <$> traverse visit a
    ObjectProp a prop →
      flip ObjectProp prop <$> traverse visit a
    ObjectUpdate a patches →
      ObjectUpdate
        <$> traverse visit a
        <*> traverse (traverse (traverse visit)) patches
    App a b →
      App <$> traverse visit a <*> traverse visit b
    Abs param expr →
      Abs param <$> traverse visit expr
    Let binds body →
      Let
        <$> traverse (traverse (traverse (traverse visit))) binds
        <*> traverse visit body
    IfThenElse p th el →
      IfThenElse
        <$> traverse visit p
        <*> traverse visit th
        <*> traverse visit el
    _ → pure e

countFreeRefs ∷ Exp → Map (Qualified Name) Natural
countFreeRefs = fmap getSum . MMap.toMap . countFreeRefs' mempty
 where
  countFreeRefs'
    ∷ Map (Qualified Name) Index
    → Exp
    → MonoidMap (Qualified Name) (Sum Natural)
  countFreeRefs' minIndexes = \case
    Ref qname index →
      if Map.findWithDefault 0 qname minIndexes <= index
        then MMap.singleton qname (Sum 1)
        else mempty
    Abs (unAnn → param) (unAnn → body) →
      case param of
        ParamNamed name → countFreeRefs' minIndexes' body
         where
          minIndexes' = Map.insertWith (+) (Local name) 1 minIndexes
        ParamUnused → countFreeRefs' minIndexes body
    Let binds (unAnn → body) → fold (countsInBody : countsInBinds)
     where
      countsInBody = countFreeRefs' minIndexes' body
       where
        minIndexes' =
          foldr (\name → Map.insertWith (+) name 1) minIndexes $
            toList binds >>= fmap (Local . runIdentity) . bindingNames
      countsInBinds =
        toList binds >>= \case
          Standalone (unAnn → boundName, expr) →
            [countFreeRefs' minIndexes' (unAnn expr)]
           where
            minIndexes' = Map.insertWith (+) (Local boundName) 1 minIndexes
          RecursiveGroup recBinds →
            countFreeRefs' minIndexes' . unAnn . snd <$> toList recBinds
           where
            minIndexes' =
              foldr (\name → Map.insertWith (+) name 1) minIndexes $
                Local . runIdentity . fst <$> recBinds
    App (unAnn → argument) (unAnn → function) →
      go argument <> go function
    LiteralArray as →
      foldMap (go . unAnn) as
    LiteralObject props →
      foldMap (go . unAnn . snd) props
    ReflectCtor (unAnn → a) →
      go a
    DataArgumentByIndex _idx (unAnn → a) →
      go a
    Eq (unAnn → a) (unAnn → b) →
      go a <> go b
    ArrayLength (unAnn → a) →
      go a
    ArrayIndex (unAnn → a) _indx →
      go a
    ObjectProp (unAnn → a) _prop →
      go a
    ObjectUpdate (unAnn → a) patches →
      go a <> foldMap (go . unAnn . snd) patches
    IfThenElse (unAnn → p) (unAnn → th) (unAnn → el) →
      go p <> go th <> go el
    -- Terminals:
    LiteralInt {} → mempty
    LiteralBool {} → mempty
    LiteralFloat {} → mempty
    LiteralString {} → mempty
    LiteralChar {} → mempty
    Ctor {} → mempty
    Exception {} → mempty
   where
    go = countFreeRefs' minIndexes

countFreeRef ∷ Qualified Name → Exp → Natural
countFreeRef name = Map.findWithDefault 0 name . countFreeRefs

-- | Substitute the given variable name and index with an expression
substitute
  ∷ Qualified Name
  -- ^ The name of the variable to replace
  → Index
  -- ^ The index of the variable to replace
  → Exp
  -- ^ The expression to substitute in place of the given variable
  → Exp
  -- ^ The expression to substitute into
  → Exp
substitute name idx replacement = substitute' idx
 where
  substitute' index subExpression = case subExpression of
    Ref name' index'
      | name == name' && index == index' →
          {-
          trace
            ( "Substituting "
                <> show name
                <> "\n\tfor "
                <> show replacement
                <> "\n\tin "
                <> show expression
            )
          -}
          replacement
      | otherwise → ref name' index'
    Abs param body →
      Abs param case unAnn param of
        ParamUnused → go <$> body
        ParamNamed pName → substitute name index' replacement' <$> body
         where
          index' = if name == Local pName then index + 1 else index
          replacement' = shift 1 pName 0 replacement
    Let binds body → Let binds' body'
     where
      binds' =
        binds <&> \grouping →
          case grouping of
            Standalone (boundName, expr) →
              Standalone
                ( boundName
                , substitute name index' replacement' <$> expr
                )
             where
              index'
                | name == Local (unAnn boundName) = index + 1
                | otherwise = index
              replacement' = shift 1 (unAnn boundName) 0 replacement
            RecursiveGroup recBinds →
              RecursiveGroup $
                (substitute name index' replacement' <$>) <<$>> recBinds
             where
              index'
                | name `elem` fmap Local boundNames = index + 1
                | otherwise = index
              replacement' =
                foldr (\n r → shift 1 n 0 r) replacement boundNames
              boundNames = runIdentity . fst <$> grouping
      body' = substitute name index' replacement' <$> body
       where
        boundNames = toList binds >>= fmap runIdentity . bindingNames
        index' = if name `elem` fmap Local boundNames then index + 1 else index
        replacement' = foldr (\n r → shift 1 n 0 r) replacement boundNames
    App argument function → App (go <$> argument) (go <$> function)
    LiteralArray as → LiteralArray (go <<$>> as)
    LiteralObject props → LiteralObject (fmap go <<$>> props)
    ReflectCtor a → ReflectCtor (go <$> a)
    DataArgumentByIndex i a → DataArgumentByIndex i (go <$> a)
    Eq a b → Eq (go <$> a) (go <$> b)
    ArrayLength a → ArrayLength (go <$> a)
    ArrayIndex a indx → ArrayIndex (go <$> a) indx
    ObjectProp a prop → ObjectProp (go <$> a) prop
    ObjectUpdate a patches → ObjectUpdate (go <$> a) (fmap go <<$>> patches)
    IfThenElse p th el → IfThenElse (go <$> p) (go <$> th) (go <$> el)
    -- Terminals:
    LiteralInt {} → subExpression
    LiteralBool {} → subExpression
    LiteralFloat {} → subExpression
    LiteralString {} → subExpression
    LiteralChar {} → subExpression
    Ctor {} → subExpression
    Exception {} → subExpression
   where
    go ∷ Exp → Exp = substitute' index

-- | Increase the index of all bound variables matching the given variable name
shift
  ∷ Int
  -- ^ The amount to shift by
  → Name
  -- ^ The variable name to match (a.k.a. the namespace)
  → Index
  -- ^ The minimum bound for which indices to shift
  → Exp
  -- ^ The expression to shift
  → Exp
shift offset namespace minIndex expression =
  case expression of
    Ref (Local name) index →
      refLocal name $
        if name == namespace && minIndex <= index
          then index + fromIntegral offset
          else index
    Abs argument body → Abs argument body'
     where
      body' = shift offset namespace minIndex' <$> body
      minIndex'
        | paramName (unAnn argument) == Just namespace = minIndex + 1
        | otherwise = minIndex
    Let binds body →
      Let binds' body'
     where
      binds' =
        binds <&> \grouping →
          case grouping of
            Standalone (boundName, expr) → Standalone (boundName, expr')
             where
              expr' = shift offset namespace minIndex' <$> expr
              minIndex'
                | namespace == unAnn boundName = minIndex + 1
                | otherwise = minIndex
            RecursiveGroup recBinds →
              RecursiveGroup $
                (shift offset namespace minIndex' <$>) <<$>> recBinds
             where
              minIndex'
                | namespace `elem` fmap (unAnn . fst) grouping = minIndex + 1
                | otherwise = minIndex
      body' = shift offset namespace minIndex' <$> body
       where
        boundNames' = toList binds >>= fmap unAnn . bindingNames
        minIndex'
          | namespace `elem` boundNames' = minIndex + 1
          | otherwise = minIndex
    App argument function → App (go <$> argument) (go <$> function)
    LiteralArray as → LiteralArray (go <<$>> as)
    LiteralObject props → LiteralObject (fmap go <<$>> props)
    ReflectCtor a → ReflectCtor (go <$> a)
    DataArgumentByIndex idx a → DataArgumentByIndex idx (go <$> a)
    Eq a b → Eq (go <$> a) (go <$> b)
    ArrayLength a → ArrayLength (go <$> a)
    ArrayIndex a indx → ArrayIndex (go <$> a) indx
    ObjectProp a prop → ObjectProp (go <$> a) prop
    ObjectUpdate a patches → ObjectUpdate (go <$> a) (fmap go <<$>> patches)
    IfThenElse p th el → IfThenElse (go <$> p) (go <$> th) (go <$> el)
    _ → expression
 where
  go = shift offset namespace minIndex
