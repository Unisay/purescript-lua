{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.Backend.IR.Types where

import Control.Monad.Trans.Accum (AccumT, add, execAccumT)
import Data.Deriving (deriveEq1, deriveOrd1, deriveShow1)
import Data.List (elemIndex)
import Data.Map qualified as Map
import Data.Tagged (Tagged (..), untag)
import Data.Text qualified as Text
import Quiet (Quiet (..))
import Text.Show (show)
import Prelude hiding (show)
import Prelude qualified as Show

data Module = Module
  { moduleName :: ModuleName
  , moduleBindings :: [Binding]
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

type Binding = Grouping (Name, Exp)

bindingNames :: Grouping (Name, exp) -> [Name]
bindingNames = fmap fst . listGrouping

bindingExprs :: Grouping (name, Exp) -> [Exp]
bindingExprs = fmap snd . listGrouping

newtype Info = Info {refsFree :: Map (Qualified Name) Natural}

instance Semigroup Info where
  Info a <> Info b = Info (Map.unionWith (+) a b)

instance Monoid Info where
  mempty = Info mempty

data Exp = Exp {unExp :: ExpF Exp, expInfo :: Info}

data ExpF a
  = Lit (Literal a)
  | Prim (PrimOp a)
  | Ctor AlgebraicType TyName CtorName [FieldName]
  | ArrayIndex a Natural
  | ObjectProp a PropName
  | ObjectUpdate a (NonEmpty (PropName, a))
  | Abs (AbsBinding a)
  | App a a
  | RefFree (Qualified Name)
  | RefBound Index
  | Let (LetBinding a)
  | IfThenElse a a a
  | Exception Text
  deriving stock (Eq, Functor)

newtype Level = Level {unLevel :: Natural}
  deriving newtype (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype Offset = Offset {unOffset :: Natural}
  deriving newtype (Show, Eq, Ord, Num, Enum, Real, Integral)

data Index = Index {level :: Level, offset :: Offset}
  deriving stock (Show, Eq, Ord)

data Argument = ArgUnused | ArgAnonymous | ArgNamed Name
  deriving stock (Show, Eq, Ord)

argumentName :: Argument -> Maybe Name
argumentName = \case
  ArgUnused -> Nothing
  ArgAnonymous -> Nothing
  ArgNamed name -> Just name

data AbsBinding a = AbsBinding Argument (LocallyNameless a)
  deriving stock (Show, Eq, Ord, Functor)

data LetBinding a
  = LetBinding
      (NonEmpty (Grouping (Name, LocallyNameless a)))
      (LocallyNameless a)
  deriving stock (Show, Eq, Ord, Functor)

newtype LocallyNameless a = LocallyNameless {unLocallyNameless :: a}
  deriving newtype (Show, Eq, Ord)
  deriving stock (Functor, Foldable, Traversable)

data Literal a
  = Integer Integer
  | Floating Double
  | String Text
  | Char Char
  | Boolean Bool
  | Array [a]
  | Object [(PropName, a)]
  deriving stock (Show, Eq, Ord, Functor)

data PrimOp a
  = ArrayLength a
  | ReflectCtor a
  | DataArgumentByIndex Natural a
  | Eq a a
  deriving stock (Show, Eq, Ord, Functor)

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

qualified :: (a -> b) -> (ModuleName -> a -> b) -> Qualified a -> b
qualified l i = \case
  Local a -> l a
  Imported m a -> i m a

qualifiedByModule :: Qualified a -> Maybe ModuleName
qualifiedByModule = qualified (const Nothing) (const . Just)

--------------------------------------------------------------------------------
-- Instances -------------------------------------------------------------------

$( let ts =
        [ ''PrimOp
        , ''Literal
        , ''ExpF
        , ''Grouping
        , ''AbsBinding
        , ''LetBinding
        , ''LocallyNameless
        ]
    in concat . concat
        <$> sequence
          [ traverse deriveEq1 ts
          , traverse deriveOrd1 ts
          , traverse deriveShow1 ts
          ]
 )

deriving stock instance Show Info

-- deriving stock instance Show Exp

instance Show Exp where
  show :: Exp -> String
  show Exp {..} = show unExp

deriving stock instance Show a => Show (ExpF a)
deriving stock instance Show Module
deriving stock instance Eq Info
deriving stock instance Eq Exp
deriving stock instance Eq Module
deriving stock instance Ord Info
deriving stock instance Ord Exp
deriving stock instance Ord a => Ord (ExpF a)
deriving stock instance Ord Module

--------------------------------------------------------------------------------
-- Unbound / Locally Nameless --------------------------------------------------
-- https://www.seas.upenn.edu/~sweirich/papers/icfp11.pdf ----------------------

class Monad m => FreshNames m where
  fresh :: m Name
  refresh :: Name -> m Name

instance FreshNames Identity where
  fresh = pure (Name "n")
  refresh = pure

instance Monad m => FreshNames (StateT [Tagged "fresh" Name] m) where
  fresh =
    get >>= \case
      [] -> error "No more fresh names"
      n : ns -> put ns $> untag n

  refresh n =
    get >>= \case
      [] -> error "No more fresh names"
      n' : ns
        | nameToText n `Text.isPrefixOf` nameToText (untag n') ->
            put ns $> untag n'
      ns ->
        error $
          "Can't refresh a name "
            <> Show.show n
            <> ", have these names: "
            <> Show.show ns

instance Monad m => FreshNames (StateT Natural m) where
  fresh = do
    i <- get
    put (i + 1)
    pure (Name ("n" <> "_" <> Show.show i))
  refresh (Name n) = do
    i <- get
    put (i + 1)
    pure (Name (n <> Show.show i))

class BindingPattern p where
  atOffset :: p -> Offset -> Maybe Name
  findOffset :: p -> Name -> Maybe Offset

instance BindingPattern Name where
  atOffset name i = if i == 0 then Just name else Nothing
  findOffset name name' = if name == name' then Just 0 else Nothing

instance BindingPattern [Name] where
  atOffset names i = names !!? fromIntegral i
  findOffset names name = fromIntegral <$> elemIndex name names

bindAbs :: Argument -> Exp -> AbsBinding Exp
bindAbs arg e = AbsBinding arg case arg of
  ArgNamed n -> close n e
  ArgUnused -> LocallyNameless e
  ArgAnonymous -> LocallyNameless e

unbindAbs :: FreshNames m => AbsBinding Exp -> m (Maybe Name, Exp)
unbindAbs (AbsBinding argument namelessExp) =
  case argument of
    ArgUnused ->
      pure (Nothing, unLocallyNameless namelessExp)
    ArgAnonymous -> do
      freshName <- fresh
      pure (Just freshName, open freshName namelessExp)
    ArgNamed nam -> do
      freshName <- refresh nam
      pure (Just freshName, open freshName namelessExp)

bindLet :: NonEmpty Binding -> Exp -> LetBinding Exp
bindLet bindings inExp =
  LetBinding ((fmap . fmap . fmap) (close names) bindings) (close names inExp)
 where
  names = toList bindings >>= bindingNames

unbindLet :: FreshNames m => LetBinding Exp -> m (NonEmpty Binding, Exp)
unbindLet (LetBinding bindings inExp) = do
  freshNames <- forM names refresh
  let openedBindings = (`evalState` (Tagged @"fresh" <$> freshNames)) $
        forM bindings \case
          Standalone (name, expr) ->
            Standalone . (,open freshNames expr) <$> refresh name
          RecursiveGroup bs ->
            RecursiveGroup <$> forM bs \(name, expr) ->
              refresh name <&> (,open freshNames expr)
  pure (openedBindings, open freshNames inExp)
 where
  names = toList bindings >>= bindingNames

close :: BindingPattern pat => pat -> Exp -> LocallyNameless Exp
close pat =
  LocallyNameless <<< updateRefs
    ( \level name ->
        pat `findOffset` name <&> \offset ->
          wrapExpF $ RefBound (Index {level, offset})
    )
    \_currentLevel _index -> Nothing

open :: BindingPattern pat => pat -> LocallyNameless Exp -> Exp
open pat =
  unLocallyNameless >>> updateRefs
    (\_currentlevel _name -> Nothing)
    \currentLevel index -> do
      guard $ currentLevel == level index
      wrapExpF . RefFree . Local <$> pat `atOffset` offset index

updateRefs
  :: (Level -> Name -> Maybe Exp)
  -- ^ How to update free references. Nothing = leave unchanged
  -> (Level -> Index -> Maybe Exp)
  -- ^ How to update bound references. Nothing = leave unchanged
  -> Exp
  -- ^ Expression to update
  -> Exp
updateRefs withFree withBound e =
  evalState (everywhereTopDownExpM rewrite e) (Level 0)
 where
  rewrite :: MonadState Level m => Exp -> m Exp
  rewrite expr = case unExp expr of
    RefFree qname ->
      case qname of
        Imported _modname _name -> pure expr
        Local name -> do
          level <- get
          maybe (pure expr) pure (withFree level name)
    RefBound index' -> do
      level <- get
      maybe (pure expr) pure (withBound level index')
    Abs {} -> modify (+ 1) $> expr
    Let {} -> modify (+ 1) $> expr
    _other -> pure expr

countBoundRefs :: LocallyNameless Exp -> Offset -> Natural
countBoundRefs (LocallyNameless namelessExp) boundOffset =
  getSum . (`evalState` Level 0) . (`execAccumT` Sum 0) $
    everywhereTopDownExpM visitor namelessExp
 where
  visitor :: Exp -> AccumT (Sum Natural) (State Level) Exp
  visitor expr = case unExp expr of
    Abs {} -> modify (+ 1) $> expr
    Let {} -> modify (+ 1) $> expr
    RefBound Index {level, offset} -> do
      atLevel <- get
      expr <$ when (level == atLevel && offset == boundOffset) do
        add (Sum 1)
    _other -> pure expr

subst
  :: LocallyNameless Exp
  -> Offset
  -> LocallyNameless Exp
  -> LocallyNameless Exp
subst (LocallyNameless nameless) replacedOffset (LocallyNameless replacement) =
  LocallyNameless (evalState (topDownExpM rewrite nameless) (Level 0))
 where
  rewrite :: MonadState Level m => Exp -> m (TopDownStep, Exp)
  rewrite expr = case unExp expr of
    RefBound Index {level, offset} ->
      get <&> \currentLevel ->
        if currentLevel == level && offset == replacedOffset
          then (Stop, adjustFor currentLevel replacement)
          else (Continue, expr)
    Abs {} -> modify (+ 1) $> (Continue, expr)
    Let {} -> modify (+ 1) $> (Continue, expr)
    _other -> pure (Continue, expr)

  adjustFor :: Level -> Exp -> Exp
  adjustFor = \case
    Level 0 -> Prelude.identity
    adjustment -> updateRefs noUpdate \_atLevel index ->
      Just . wrapExpF $ RefBound index {level = adjustment + level index}

  noUpdate :: Level -> Name -> Maybe Exp
  noUpdate _currentLevel _name = Nothing

-- Constructors for expresssions -----------------------------------------------

wrapExpF :: ExpF Exp -> Exp
wrapExpF e = Exp e case e of
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
  RefFree ref -> Info {refsFree = Map.singleton ref 1}
  RefBound _index -> mempty
  Abs (AbsBinding _arg (LocallyNameless expr)) -> expInfo expr
  Let (LetBinding binds (LocallyNameless body)) ->
    foldMap (expInfo . unLocallyNameless . snd) (listGrouping =<< toList binds)
      <> expInfo body
  IfThenElse p th el -> expInfo p <> expInfo th <> expInfo el
  _ -> mempty

arrayIndex :: Exp -> Nat -> Exp
arrayIndex = (wrapExpF .) . ArrayIndex

objectProp :: Exp -> PropName -> Exp
objectProp = (wrapExpF .) . ObjectProp

update :: Exp -> NonEmpty (PropName, Exp) -> Exp
update = (wrapExpF .) . ObjectUpdate

ctor :: AlgebraicType -> TyName -> CtorName -> [FieldName] -> Exp
ctor = (((wrapExpF .) .) .) . Ctor

abstraction :: Argument -> Exp -> Exp
abstraction arg = wrapExpF . Abs . bindAbs arg

abstraction' :: Argument -> LocallyNameless Exp -> Exp
abstraction' arg = wrapExpF . Abs . AbsBinding arg

identity :: Exp
identity =
  wrapExpF . Abs . AbsBinding ArgAnonymous . LocallyNameless $
    refBound (Index (Level 0) (Offset 0))

lets :: NonEmpty Binding -> Exp -> Exp
lets = ((wrapExpF . Let) .) . bindLet

lets'
  :: NonEmpty (Grouping (Name, LocallyNameless Exp))
  -> LocallyNameless Exp
  -> Exp
lets' bindings body = wrapExpF $ Let $ LetBinding bindings body

application :: Exp -> Exp -> Exp
application = (wrapExpF .) . App

refFree :: Qualified Name -> Exp
refFree = wrapExpF . RefFree

refFreeLocal :: Name -> Exp
refFreeLocal = wrapExpF . RefFree . Local

refFreeImported :: ModuleName -> Name -> Exp
refFreeImported modname = wrapExpF . RefFree . Imported modname

refBound :: Index -> Exp
refBound = wrapExpF . RefBound

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

everywhereExp :: (Exp -> Exp) -> Exp -> Exp
everywhereExp f = runIdentity . everywhereExpM (pure . f)

everywhereExpM :: forall m. Monad m => (Exp -> m Exp) -> Exp -> m Exp
everywhereExpM visit = go
 where
  go :: Exp -> m Exp
  go e = case unExp e of
    Lit (Array as) -> visit . array =<< traverse go as
    Lit (Object props) -> visit . object =<< traverse (traverse go) props
    Prim op ->
      case op of
        ArrayLength a -> visit . arrayLength =<< go a
        ReflectCtor a -> visit . reflectCtor =<< go a
        DataArgumentByIndex idx a -> visit . dataArgumentByIndex idx =<< go a
        Eq a b -> visit . wrapExpF . Prim =<< Eq <$> go a <*> go b
    ArrayIndex a indx -> visit . flip arrayIndex indx =<< go a
    ObjectProp a prop -> visit . flip objectProp prop =<< go a
    ObjectUpdate a patches ->
      visit . wrapExpF
        =<< ObjectUpdate
          <$> go a
          <*> traverse (traverse go) patches
    App a b ->
      visit . wrapExpF =<< App <$> go a <*> go b
    Abs (AbsBinding arg (LocallyNameless expr)) ->
      visit . wrapExpF . Abs . AbsBinding arg . LocallyNameless =<< go expr
    Let (LetBinding binds (LocallyNameless body)) ->
      visit . wrapExpF . Let
        =<< LetBinding
          <$> traverse (traverse (traverse (traverse go))) binds
          <*> fmap LocallyNameless (go body)
    IfThenElse p th el -> visit =<< ifThenElse <$> go p <*> go th <*> go el
    _ -> visit e

-- everywhereTopDownExp :: (Exp -> Maybe Exp) -> Exp -> Exp
-- everywhereTopDownExp f = runIdentity . everywhereTopDownExpM (pure . f)

everywhereTopDownExpM :: Monad m => (Exp -> m Exp) -> Exp -> m Exp
everywhereTopDownExpM rewrite = topDownExpM ((Continue,) <<$>> rewrite)

data TopDownStep = Continue | Stop

topDownExpM :: Monad m => (Exp -> m (TopDownStep, Exp)) -> Exp -> m Exp
topDownExpM f = visit
 where
  visit x =
    f x >>= \case
      (Continue, a) -> step a
      (Stop, a) -> pure a

  step e = case unExp e of
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
      (wrapExpF .) . ObjectUpdate
        <$> visit a
        <*> traverse (traverse visit) patches
    App a b -> application <$> visit a <*> visit b
    Abs (AbsBinding arg (LocallyNameless expr)) ->
      wrapExpF . Abs . AbsBinding arg . LocallyNameless <$> visit expr
    Let (LetBinding binds (LocallyNameless body)) ->
      ((wrapExpF . Let) .) . LetBinding
        <$> traverse (traverse (traverse (traverse visit))) binds
        <*> fmap LocallyNameless (visit body)
    IfThenElse p th el -> ifThenElse <$> visit p <*> visit th <*> visit el
    _ -> pure e
