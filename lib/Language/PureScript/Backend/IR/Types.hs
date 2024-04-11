{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.Backend.IR.Types where

import Control.Lens (Prism', prism')
import Data.Deriving (deriveEq1, deriveOrd1)
import Data.Map qualified as Map
import Data.MonoidMap (MonoidMap)
import Data.MonoidMap qualified as MMap
import Language.PureScript.Backend.IR.Inliner qualified as Inliner
import Language.PureScript.Names (ModuleName, runModuleName)
import Quiet (Quiet (..))
import Prelude hiding (show)

type Ann = Maybe Inliner.Annotation

noAnn ∷ Ann
noAnn = Nothing

type Exp = RawExp Ann

data Module = Module
  { moduleName ∷ ModuleName
  , moduleBindings ∷ [Binding]
  , moduleImports ∷ [ModuleName]
  , moduleExports ∷ [Name]
  , moduleReExports ∷ Map ModuleName [Name]
  , moduleForeigns ∷ [Name]
  , modulePath ∷ FilePath
  }

data Grouping a = Standalone a | RecursiveGroup (NonEmpty a)
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

listGrouping ∷ Grouping a → [a]
listGrouping = \case
  Standalone a → [a]
  RecursiveGroup as → toList as

type Binding = Grouping (Ann, Name, Exp)

bindingNames ∷ Grouping (ann, name, exp) → [name]
bindingNames = (\(_ann, name, _exp) → name) <<$>> listGrouping

bindingExprs ∷ Grouping (name, RawExp ann) → [RawExp ann]
bindingExprs = fmap snd . listGrouping

newtype Info = Info {refsFree ∷ Map (Qualified Name) Natural}

instance Semigroup Info where
  Info a <> Info b = Info (Map.unionWith (+) a b)

instance Monoid Info where
  mempty = Info mempty

data RawExp ann
  = LiteralInt ann Integer
  | LiteralFloat ann Double
  | LiteralString ann Text
  | LiteralChar ann Char
  | LiteralBool ann Bool
  | LiteralArray ann [RawExp ann]
  | LiteralObject ann [(PropName, RawExp ann)]
  | Ctor ann AlgebraicType ModuleName TyName CtorName [FieldName]
  | ReflectCtor ann (RawExp ann)
  | Eq ann (RawExp ann) (RawExp ann)
  | DataArgumentByIndex ann Natural (RawExp ann)
  | ArrayLength ann (RawExp ann)
  | ArrayIndex ann (RawExp ann) Natural
  | ObjectProp ann (RawExp ann) PropName
  | ObjectUpdate ann (RawExp ann) (NonEmpty (PropName, RawExp ann))
  | Abs ann (Parameter ann) (RawExp ann)
  | App ann (RawExp ann) (RawExp ann)
  | Ref ann (Qualified Name) Index
  | Let ann (NonEmpty (Grouping (ann, Name, RawExp ann))) (RawExp ann)
  | IfThenElse ann (RawExp ann) (RawExp ann) (RawExp ann)
  | Exception ann Text
  | ForeignImport ann ModuleName FilePath [Name]

deriving stock instance Show ann ⇒ Show (RawExp ann)
deriving stock instance Eq ann ⇒ Eq (RawExp ann)
deriving stock instance Ord ann ⇒ Ord (RawExp ann)

getAnn ∷ RawExp ann → ann
getAnn = \case
  LiteralInt ann _ → ann
  LiteralFloat ann _ → ann
  LiteralString ann _ → ann
  LiteralChar ann _ → ann
  LiteralBool ann _ → ann
  LiteralArray ann _ → ann
  LiteralObject ann _ → ann
  Ctor ann _ _ _ _ _ → ann
  ReflectCtor ann _ → ann
  Eq ann _ _ → ann
  DataArgumentByIndex ann _ _ → ann
  ArrayLength ann _ → ann
  ArrayIndex ann _ _ → ann
  ObjectProp ann _ _ → ann
  ObjectUpdate ann _ _ → ann
  Abs ann _ _ → ann
  App ann _ _ → ann
  Ref ann _ _ → ann
  Let ann _ _ → ann
  IfThenElse ann _ _ _ → ann
  Exception ann _ → ann
  ForeignImport ann _ _ _ → ann

newtype Index = Index {unIndex ∷ Natural}
  deriving newtype (Show, Eq, Ord, Num, Enum, Real, Integral)

data Parameter ann = ParamUnused ann | ParamNamed ann Name
  deriving stock (Show, Eq, Ord)

paramName ∷ Parameter ann → Maybe Name
paramName (ParamUnused _ann) = Nothing
paramName (ParamNamed _ann name) = Just name

isLiteral ∷ RawExp ann → Bool
isLiteral = (||) <$> isNonRecursiveLiteral <*> isRecursiveLiteral

isNonRecursiveLiteral ∷ RawExp ann → Bool
isNonRecursiveLiteral = \case
  LiteralInt {} → True
  LiteralFloat {} → True
  LiteralString {} → True
  LiteralChar {} → True
  LiteralBool {} → True
  _ → False

isRecursiveLiteral ∷ RawExp ann → Bool
isRecursiveLiteral = \case
  LiteralArray {} → True
  LiteralObject {} → True
  _ → False

data AlgebraicType = SumType | ProductType
  deriving stock (Generic, Eq, Ord, Show, Enum, Bounded)

ctorId ∷ ModuleName → TyName → CtorName → Text
ctorId modName tyName ctorName =
  runModuleName modName
    <> "∷"
    <> renderTyName tyName
    <> "."
    <> renderCtorName ctorName

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

instance Ord Module where
  compare a b = compare (moduleName a) (moduleName b)

-- Constructors for expresssions -----------------------------------------------

arrayIndex ∷ Exp → Natural → Exp
arrayIndex = ArrayIndex noAnn

objectProp ∷ Exp → PropName → Exp
objectProp = ObjectProp noAnn

objectUpdate ∷ Exp → NonEmpty (PropName, Exp) → Exp
objectUpdate = ObjectUpdate noAnn

ctor ∷ AlgebraicType → ModuleName → TyName → CtorName → [FieldName] → Exp
ctor = Ctor noAnn

abstraction ∷ Parameter Ann → Exp → Exp
abstraction = Abs noAnn

identity ∷ Exp
identity =
  let name = Name "x"
   in abstraction (paramNamed name) (refLocal name 0)

lets ∷ NonEmpty Binding → Exp → Exp
lets = Let noAnn

application ∷ Exp → Exp → Exp
application = App noAnn

paramNamed ∷ Name → Parameter Ann
paramNamed = ParamNamed noAnn

paramUnused ∷ Parameter Ann
paramUnused = ParamUnused noAnn

ref ∷ Qualified Name → Index → Exp
ref qname index =
  case qname of
    Local name → refLocal name index
    Imported modname name → refImported modname name index

refLocal ∷ Name → Index → Exp
refLocal = Ref noAnn . Local

refLocal0 ∷ Name → Exp
refLocal0 name = refLocal name (Index 0)

refImported ∷ ModuleName → Name → Index → Exp
refImported modname name = Ref noAnn (Imported modname name)

ifThenElse ∷ Exp → Exp → Exp → Exp
ifThenElse = IfThenElse noAnn

exception ∷ Text → Exp
exception = Exception noAnn

--------------------------------------------------------------------------------
-- Constructors for primitive operations ---------------------------------------

eq ∷ Exp → Exp → Exp
eq = Eq noAnn

arrayLength ∷ Exp → Exp
arrayLength = ArrayLength noAnn

reflectCtor ∷ Exp → Exp
reflectCtor = ReflectCtor noAnn

dataArgumentByIndex ∷ Natural → Exp → Exp
dataArgumentByIndex = DataArgumentByIndex noAnn

--------------------------------------------------------------------------------
-- Constructors for literals ---------------------------------------------------

literalBool ∷ Bool → Exp
literalBool = LiteralBool noAnn

literalInt ∷ Integer → Exp
literalInt = LiteralInt noAnn

asLiteralInt ∷ Prism' Exp Integer
asLiteralInt = prism' literalInt \case
  LiteralInt _ann i → Just i
  _ → Nothing

literalFloat ∷ Double → Exp
literalFloat = LiteralFloat noAnn

asLiteralFloat ∷ Prism' Exp Double
asLiteralFloat = prism' literalFloat \case
  LiteralFloat _ann f → Just f
  _ → Nothing

literalString ∷ Text → Exp
literalString = LiteralString noAnn

asLiteralString ∷ Prism' Exp Text
asLiteralString = prism' literalString \case
  LiteralString _ann s → Just s
  _ → Nothing

literalChar ∷ Char → Exp
literalChar = LiteralChar noAnn

asLiteralChar ∷ Prism' Exp Char
asLiteralChar = prism' literalChar \case
  LiteralChar _ann c → Just c
  _ → Nothing

literalArray ∷ [Exp] → Exp
literalArray = LiteralArray noAnn

literalObject ∷ [(PropName, Exp)] → Exp
literalObject = LiteralObject noAnn

--------------------------------------------------------------------------------
-- Traversals ------------------------------------------------------------------

annotateExpM
  ∷ ∀ ann ann' m
   . Monad m
  ⇒ (∀ x. m x → m x)
  → (RawExp ann → m ann')
  → (Parameter ann → m (Parameter ann'))
  → (ann → Name → m ann')
  → (RawExp ann → m (RawExp ann'))
annotateExpM around annotateExp annotateParam annotateName =
  around . \expr → do
    ann ← annotateExp expr
    case expr of
      LiteralInt _ann i →
        pure $ LiteralInt ann i
      LiteralFloat _ann f →
        pure $ LiteralFloat ann f
      LiteralString _ann s →
        pure $ LiteralString ann s
      LiteralChar _ann c →
        pure $ LiteralChar ann c
      LiteralBool _ann b →
        pure $ LiteralBool ann b
      LiteralArray _ann elems → do
        elems' ← traverse mkAnn elems
        pure $ LiteralArray ann elems'
      LiteralObject _ann props → do
        props' ← traverse (traverse mkAnn) props
        pure $ LiteralObject ann props'
      ReflectCtor _ann a → do
        a' ← mkAnn a
        pure $ ReflectCtor ann a'
      Eq _ann a b → do
        a' ← mkAnn a
        b' ← mkAnn b
        pure $ Eq ann a' b'
      DataArgumentByIndex _ann index a → do
        a' ← mkAnn a
        pure $ DataArgumentByIndex ann index a'
      ArrayLength _ann a → do
        a' ← mkAnn a
        pure $ ArrayLength ann a'
      ArrayIndex _ann a index → do
        a' ← mkAnn a
        pure $ ArrayIndex ann a' index
      ObjectProp _ann a prop → do
        a' ← mkAnn a
        pure $ ObjectProp ann a' prop
      ObjectUpdate _ann a props → do
        a' ← mkAnn a
        props' ← traverse (traverse mkAnn) props
        pure $ ObjectUpdate ann a' props'
      Abs _ann param body → do
        body' ← mkAnn body
        param' ← annotateParam param
        pure $ Abs ann param' body'
      App _ann a b → do
        a' ← mkAnn a
        b' ← mkAnn b
        pure $ App ann a' b'
      Ref _ann qname index → pure $ Ref ann qname index
      Let _ann binds body → do
        binds' ←
          forM binds $
            traverse \(a, n, e) → do
              n' ← annotateName a n
              e' ← mkAnn e
              pure (n', n, e')
        body' ← mkAnn body
        pure $ Let ann binds' body'
      IfThenElse _ann i t e → do
        i' ← mkAnn i
        t' ← mkAnn t
        e' ← mkAnn e
        pure $ IfThenElse ann i' t' e'
      Ctor _ann mn aty ty ctr fs → pure $ Ctor ann mn aty ty ctr fs
      Exception _ann m → pure $ Exception ann m
      ForeignImport _ann m p ns → pure $ ForeignImport ann m p ns
 where
  mkAnn ∷ RawExp ann → m (RawExp ann')
  mkAnn = annotateExpM around annotateExp annotateParam annotateName

traverseExpBottomUp
  ∷ ∀ ann m
   . Monad m
  ⇒ (RawExp ann → m (RawExp ann))
  → (RawExp ann → m (RawExp ann))
traverseExpBottomUp visit = go
 where
  go ∷ RawExp ann → m (RawExp ann)
  go e =
    visit =<< case e of
      LiteralArray ann as →
        LiteralArray ann <$> traverse go as
      LiteralObject ann props →
        LiteralObject ann <$> traverse (traverse go) props
      ReflectCtor ann a →
        ReflectCtor ann <$> go a
      DataArgumentByIndex ann idx a →
        DataArgumentByIndex ann idx <$> go a
      Eq ann a b →
        Eq ann <$> go a <*> go b
      ArrayLength ann a →
        ArrayLength ann <$> go a
      ArrayIndex ann a idx → do
        a' ← go a
        pure $ ArrayIndex ann a' idx
      ObjectProp ann a prp → do
        a' ← go a
        pure $ ObjectProp ann a' prp
      ObjectUpdate ann a ps →
        ObjectUpdate ann
          <$> go a
          <*> traverse (traverse go) ps
      App ann a b →
        App ann <$> go a <*> go b
      Abs ann arg a →
        Abs ann arg <$> go a
      Let ann bs body →
        Let ann
          <$> traverse (traverse (\(a, n, expr) → (a,n,) <$> go expr)) bs
          <*> go body
      IfThenElse ann p th el →
        IfThenElse ann <$> go p <*> go th <*> go el
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

type RewriteRule ann = RewriteRuleM Identity ann
type RewriteRuleM m ann = RawExp ann → m (Rewritten (RawExp ann))

thenRewrite
  ∷ Monad m
  ⇒ RewriteRuleM m ann
  → RewriteRuleM m ann
  → RewriteRuleM m ann
thenRewrite rewrite1 rewrite2 e =
  rewrite1 e >>= \case
    Rewritten m' e' → do
      rewrite2 e' <&> \case
        NoChange → Rewritten m' e'
        Rewritten m'' e'' → Rewritten (m' <> m'') e''
    NoChange → rewrite2 e

rewriteExpTopDown ∷ RewriteRuleM Identity ann → RawExp ann → RawExp ann
rewriteExpTopDown rewrite = runIdentity . rewriteExpTopDownM rewrite

rewriteExpTopDownM ∷ Monad m ⇒ RewriteRuleM m ann → RawExp ann → m (RawExp ann)
rewriteExpTopDownM rewrite = visit
 where
  visit expression =
    rewrite expression >>= \case
      NoChange → descendInto expression
      Rewritten Stop expression' → pure expression'
      Rewritten Recurse expression' → descendInto expression'

  descendInto e = case e of
    LiteralArray ann as →
      LiteralArray ann <$> traverse visit as
    LiteralObject ann props →
      LiteralObject ann <$> traverse (traverse visit) props
    ReflectCtor ann a →
      ReflectCtor ann <$> visit a
    DataArgumentByIndex ann idx a →
      DataArgumentByIndex ann idx <$> visit a
    Eq ann a b →
      Eq ann <$> visit a <*> visit b
    ArrayLength ann a →
      ArrayLength ann <$> visit a
    ArrayIndex ann a indx →
      visit a <&> \expr → ArrayIndex ann expr indx
    ObjectProp ann a prop →
      visit a <&> \expr → ObjectProp ann expr prop
    ObjectUpdate ann a patches →
      ObjectUpdate ann <$> visit a <*> traverse (traverse visit) patches
    App ann a b →
      App ann <$> visit a <*> visit b
    Abs ann param expr →
      Abs ann param <$> visit expr
    Let ann binds body →
      Let ann
        <$> forM binds (traverse \(a, n, expr) → (a,n,) <$> visit expr)
        <*> visit body
    IfThenElse ann p th el →
      IfThenElse ann <$> visit p <*> visit th <*> visit el
    _ → pure e

countFreeRefs ∷ RawExp ann → Map (Qualified Name) Natural
countFreeRefs = fmap getSum . MMap.toMap . countFreeRefs' mempty
 where
  countFreeRefs'
    ∷ Map (Qualified Name) Index
    → RawExp ann
    → MonoidMap (Qualified Name) (Sum Natural)
  countFreeRefs' minIndexes = \case
    Ref _ann qname index →
      if Map.findWithDefault 0 qname minIndexes <= index
        then MMap.singleton qname (Sum 1)
        else mempty
    Abs _ann param body →
      case param of
        ParamNamed _paramAnn name → countFreeRefs' minIndexes' body
         where
          minIndexes' = Map.insertWith (+) (Local name) 1 minIndexes
        ParamUnused _paramAnn → countFreeRefs' minIndexes body
    Let _ann binds body → fold (countsInBody : countsInBinds)
     where
      countsInBody = countFreeRefs' minIndexes' body
       where
        minIndexes' =
          foldr (\name → Map.insertWith (+) name 1) minIndexes $
            toList binds >>= fmap Local . bindingNames
      countsInBinds =
        toList binds >>= \case
          Standalone (_nameAnn, boundName, expr) →
            [countFreeRefs' minIndexes' expr]
           where
            minIndexes' = Map.insertWith (+) (Local boundName) 1 minIndexes
          RecursiveGroup recBinds →
            toList recBinds <&> \(_nameAnn, _boundName, expr) →
              countFreeRefs' minIndexes' expr
           where
            minIndexes' =
              foldr
                (\(_nameAnn, qName, _expr) → Map.insertWith (+) (Local qName) 1)
                minIndexes
                recBinds
    App _ann argument function →
      go argument <> go function
    LiteralArray _ann as →
      foldMap go as
    LiteralObject _ann props →
      foldMap (go . snd) props
    ReflectCtor _ann a →
      go a
    DataArgumentByIndex _ann _idx a →
      go a
    Eq _ann a b →
      go a <> go b
    ArrayLength _ann a →
      go a
    ArrayIndex _ann a _indx →
      go a
    ObjectProp _ann a _prop →
      go a
    ObjectUpdate _ann a patches →
      go a <> foldMap (go . snd) patches
    IfThenElse _ann p th el →
      go p <> go th <> go el
    -- Terminals:
    LiteralInt {} → mempty
    LiteralBool {} → mempty
    LiteralFloat {} → mempty
    LiteralString {} → mempty
    LiteralChar {} → mempty
    Ctor {} → mempty
    Exception {} → mempty
    ForeignImport {} → mempty
   where
    go = countFreeRefs' minIndexes

countFreeRef ∷ Qualified Name → RawExp ann → Natural
countFreeRef name = Map.findWithDefault 0 name . countFreeRefs

-- | Substitute the given variable name and index with an expression
substitute
  ∷ ∀ ann
   . Qualified Name
  -- ^ The name of the variable to replace
  → Index
  -- ^ The index of the variable to replace
  → RawExp ann
  -- ^ The expression to substitute in place of the given variable
  → RawExp ann
  -- ^ The expression to substitute into
  → RawExp ann
substitute name idx replacement = substitute' idx
 where
  substitute' ∷ Index → RawExp ann → RawExp ann
  substitute' index subExpression =
    case subExpression of
      Ref ann name' index'
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
        | otherwise → Ref ann name' index'
      Abs ann param body →
        Abs ann param case param of
          ParamUnused _paramAnn → go body
          ParamNamed _paramAnn pName → substitute name index' replacement' body
           where
            index' = if name == Local pName then index + 1 else index
            replacement' = shift 1 pName 0 replacement
      Let ann binds body → Let ann binds' body'
       where
        binds' =
          binds <&> \grouping →
            case grouping of
              Standalone (nameAnn, boundName, expr) →
                Standalone
                  ( nameAnn
                  , boundName
                  , substitute name index' replacement' expr
                  )
               where
                index'
                  | name == Local boundName = index + 1
                  | otherwise = index
                replacement' = shift 1 boundName 0 replacement
              RecursiveGroup recBinds →
                RecursiveGroup $
                  substitute name index' replacement' <<$>> recBinds
               where
                index'
                  | name `elem` fmap Local boundNames = index + 1
                  | otherwise = index
                replacement' =
                  foldr (\n r → shift 1 n 0 r) replacement boundNames
                boundNames = bindingNames grouping
        body' = substitute name index' replacement' body
         where
          boundNames = toList binds >>= bindingNames
          index' =
            index
              & if name `elem` (Local <$> boundNames) then (+ 1) else id
          replacement' = foldr (\n r → shift 1 n 0 r) replacement boundNames
      App ann argument function →
        App ann (go argument) (go function)
      LiteralArray ann as →
        LiteralArray ann (go <$> as)
      LiteralObject ann props →
        LiteralObject ann (go <<$>> props)
      ReflectCtor ann a →
        ReflectCtor ann (go a)
      DataArgumentByIndex ann i a →
        DataArgumentByIndex ann i (go a)
      Eq ann a b →
        Eq ann (go a) (go b)
      ArrayLength ann a →
        ArrayLength ann (go a)
      ArrayIndex ann a indx →
        ArrayIndex ann (go a) indx
      ObjectProp ann a prop →
        ObjectProp ann (go a) prop
      ObjectUpdate ann a patches →
        ObjectUpdate ann (go a) (go <<$>> patches)
      IfThenElse ann p th el →
        IfThenElse ann (go p) (go th) (go el)
      -- Terminals:
      LiteralInt {} → subExpression
      LiteralBool {} → subExpression
      LiteralFloat {} → subExpression
      LiteralString {} → subExpression
      LiteralChar {} → subExpression
      Ctor {} → subExpression
      Exception {} → subExpression
      ForeignImport {} → subExpression
   where
    go = substitute' index

-- | Increase the index of all bound variables matching the given variable name
shift
  ∷ Int
  -- ^ The amount to shift by
  → Name
  -- ^ The variable name to match (a.k.a. the namespace)
  → Index
  -- ^ The minimum bound for which indices to shift
  → RawExp ann
  -- ^ The expression to shift
  → RawExp ann
shift offset namespace minIndex expression =
  case expression of
    Ref ann (Local name) index →
      Ref ann (Local name) $
        index
          + if name == namespace && minIndex <= index
            then fromIntegral offset
            else 0
    Abs ann argument body →
      Abs ann argument (shift offset namespace minIndex' body)
     where
      minIndex'
        | paramName argument == Just namespace = minIndex + 1
        | otherwise = minIndex
    Let ann binds body →
      Let ann binds' body'
     where
      binds' =
        binds <&> \grouping →
          case grouping of
            Standalone (annotation, boundName, expr) →
              Standalone
                ( annotation
                , boundName
                , shift offset namespace minIndex' expr
                )
             where
              minIndex'
                | namespace == boundName = minIndex + 1
                | otherwise = minIndex
            RecursiveGroup recBinds →
              RecursiveGroup $
                recBinds <&> \(nameAnn, boundName, expr) →
                  (nameAnn, boundName, shift offset namespace minIndex' expr)
             where
              minIndex'
                | namespace `elem` bindingNames grouping = minIndex + 1
                | otherwise = minIndex
      body' = shift offset namespace minIndex' body
       where
        boundNames' = toList binds >>= bindingNames
        minIndex'
          | namespace `elem` boundNames' = minIndex + 1
          | otherwise = minIndex
    App ann argument function →
      App ann (go argument) (go function)
    LiteralArray ann as →
      LiteralArray ann (go <$> as)
    LiteralObject ann props →
      LiteralObject ann (go <<$>> props)
    ReflectCtor ann a →
      ReflectCtor ann (go a)
    DataArgumentByIndex ann idx a →
      DataArgumentByIndex ann idx (go a)
    Eq ann a b →
      Eq ann (go a) (go b)
    ArrayLength ann a →
      ArrayLength ann (go a)
    ArrayIndex ann a indx →
      ArrayIndex ann (go a) indx
    ObjectProp ann a prop →
      ObjectProp ann (go a) prop
    ObjectUpdate ann a patches →
      ObjectUpdate ann (go a) (go <<$>> patches)
    IfThenElse ann p th el →
      IfThenElse ann (go p) (go th) (go el)
    _ → expression
 where
  go = shift offset namespace minIndex
