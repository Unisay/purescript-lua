module Language.PureScript.Backend.Lua
  ( fromUberModule
  , fromIR
  , fromName
  , fromQName
  , qualifyName
  , Error (..)
  ) where

import Control.Arrow (left)
import Control.Monad (ap)
import Control.Monad.Oops (CouldBe, Variant)
import Control.Monad.Oops qualified as Oops
import Control.Monad.Trans.Accum (AccumT, add, runAccumT)
import Data.DList qualified as DList
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Tagged (Tagged (..), untag)
import Data.Text qualified as Text
import Data.Traversable (for)
import Language.PureScript.Backend.IR qualified as IR
import Language.PureScript.Backend.IR.Linker (UberModule (..))
import Language.PureScript.Backend.IR.Linker qualified as Linker
import Language.PureScript.Backend.IR.Query (usesPrimModule, usesRuntimeLazy)
import Language.PureScript.Backend.Lua.Fixture qualified as Fixture
import Language.PureScript.Backend.Lua.Key qualified as Key
import Language.PureScript.Backend.Lua.Linker.Foreign qualified as Foreign
import Language.PureScript.Backend.Lua.Name qualified as Lua
import Language.PureScript.Backend.Lua.Name qualified as Name
import Language.PureScript.Backend.Lua.Types (ParamF (..))
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Language.PureScript.Backend.Types (AppOrModule (..))
import Language.PureScript.Names (ModuleName, runModuleName)
import Language.PureScript.Names qualified as PS
import Path (Abs, Dir, Path)
import Prelude hiding (exp, local)

type LuaM e a =
  AccumT UsesObjectUpdate (StateT Natural (ExceptT (Variant e) IO)) a

data UsesObjectUpdate = NoObjectUpdate | UsesObjectUpdate
  deriving stock (Eq, Ord, Show)

instance Semigroup UsesObjectUpdate where
  _ <> UsesObjectUpdate = UsesObjectUpdate
  UsesObjectUpdate <> _ = UsesObjectUpdate
  NoObjectUpdate <> NoObjectUpdate = NoObjectUpdate

instance Monoid UsesObjectUpdate where
  mempty = NoObjectUpdate

data Error
  = UnexpectedRefBound ModuleName IR.Exp
  | LinkerErrorForeign Foreign.Error
  | AppEntryPointNotFound ModuleName PS.Ident
  deriving stock (Show)

fromUberModule
  ∷ ∀ e
   . e `CouldBe` Error
  ⇒ Tagged "foreign" (Path Abs Dir)
  → Tagged "needsRuntimeLazy" Bool
  → AppOrModule
  → Linker.UberModule
  → ExceptT (Variant e) IO Lua.Chunk
fromUberModule foreigns needsRuntimeLazy appOrModule uber = (`evalStateT` 0) do
  (chunk, usesObjectUpdate) ← (`runAccumT` NoObjectUpdate) do
    foreignBindings ←
      forM (Linker.uberModuleForeigns uber) \(IR.QName modname name, irExp) → do
        exp ← asExpression <$> fromIR foreigns Set.empty modname irExp
        pure (Lua.local1 (fromQName modname name) exp)
    bindings ←
      Linker.uberModuleBindings uber & foldMapM \case
        IR.Standalone (IR.QName modname name, irExp) → do
          exp ← fromIR foreigns Set.empty modname irExp
          pure $
            DList.singleton
              (Lua.local1 (fromQName modname name) (asExpression exp))
        IR.RecursiveGroup recGroup → do
          recBinds ← forM (toList recGroup) \(IR.QName modname name, irExp) →
            (fromQName modname name,) . asExpression
              <$> fromIR foreigns Set.empty modname irExp
          let declarations = Lua.local0 . fst <$> DList.fromList recBinds
              assignments = DList.fromList do
                recBinds <&> \(name, exp) →
                  Lua.assign (Lua.VarName name) exp
          pure $ declarations <> assignments

    returnExp ←
      case appOrModule of
        AsModule modname →
          Lua.table <$> forM (uberModuleExports uber) \(fromName → name, expr) →
            Lua.tableRowNV name . asExpression
              <$> fromIR foreigns mempty modname expr
        AsApplication modname ident → do
          case List.lookup name (uberModuleExports uber) of
            Just expr → do
              entry ← fromIR foreigns mempty modname expr
              pure $ Lua.functionCall (asExpression entry) []
            _ → Oops.throw $ AppEntryPointNotFound modname ident
         where
          name = IR.identToName ident

    pure $
      DList.fromList foreignBindings
        <> DList.snoc bindings (Lua.Return (Lua.ann returnExp))

  pure . mconcat $
    [ [Fixture.prim | usesPrimModule uber]
    , [Fixture.runtimeLazy | untag needsRuntimeLazy && usesRuntimeLazy uber]
    , [Fixture.objectUpdate | UsesObjectUpdate ← [usesObjectUpdate]]
    , DList.toList chunk
    ]

asExpression ∷ Either Lua.Chunk Lua.Exp → Lua.Exp
asExpression = \case
  Left chunk → Lua.chunkToExpression chunk
  Right expr → expr

fromQName ∷ ModuleName → IR.Name → Lua.Name
fromQName modname name = qualifyName modname (fromName name)

fromName ∷ HasCallStack ⇒ IR.Name → Lua.Name
fromName = Name.makeSafe . IR.nameToText

fromNameWithIndex ∷ HasCallStack ⇒ IR.Name → IR.Index → Lua.Name
fromNameWithIndex name (IR.unIndex → index) =
  if index == 0
    then fromName name
    else Name.makeSafe $ IR.nameToText name <> show index

fromModuleName ∷ ModuleName → Lua.Name
fromModuleName = Name.makeSafe . runModuleName

fromPropName ∷ IR.PropName → Lua.Name
fromPropName (IR.PropName name) = Name.makeSafe name

fromIR
  ∷ ∀ e
   . e `CouldBe` Error
  ⇒ Tagged "foreign" (Path Abs Dir)
  → Set Lua.Name
  → ModuleName
  → IR.Exp
  → LuaM e (Either Lua.Chunk Lua.Exp)
fromIR foreigns topLevelNames modname ir = case ir of
  IR.LiteralInt _ann i →
    pure . Right $ Lua.Integer i
  IR.LiteralFloat _ann d →
    pure . Right $ Lua.Float d
  IR.LiteralString _ann s →
    pure . Right $ Lua.String s
  IR.LiteralChar _ann c →
    pure . Right $ Lua.String $ Text.singleton c
  IR.LiteralBool _ann b →
    pure . Right $ Lua.Boolean b
  IR.LiteralArray _ann exprs →
    Right . Lua.table <$> forM (zip [1 ..] exprs) \(i, e) →
      Lua.tableRowKV (Lua.Integer i) <$> goExp e
  IR.LiteralObject _ann kvs →
    Right . Lua.table <$> for kvs \(prop, exp) →
      Lua.tableRowNV (fromPropName prop) <$> goExp exp
  IR.ReflectCtor _ann e →
    Right . (`Lua.varIndex` keyCtor) <$> goExp e
  IR.DataArgumentByIndex _ann i e →
    Right . (`Lua.varField` Lua.unsafeName ("value" <> show i)) <$> goExp e
  IR.Eq _ann l r →
    Right <$> liftA2 Lua.equalTo (goExp l) (goExp r)
  IR.Ctor _ann _algebraicTy ctorModName ctorTyName ctorName fieldNames →
    pure . Right $ foldr wrap value args
   where
    wrap name expr = Lua.functionDef [ParamNamed name] [Lua.return expr]
    value = Lua.table $ ctorRow : attributes
    ctorId = IR.ctorId ctorModName ctorTyName ctorName
    ctorRow = Lua.tableRowKV keyCtor (Lua.String ctorId)
    args = Name.unsafeName . IR.renderFieldName <$> fieldNames
    attributes = args <&> ap Lua.tableRowNV Lua.varName
  IR.ArrayLength _ann e →
    Right . Lua.hash <$> goExp e
  IR.ArrayIndex _ann expr index →
    Right . flip Lua.varIndex (Lua.Integer (fromIntegral index)) <$> goExp expr
  IR.ObjectProp _ann expr propName →
    Right . flip Lua.varField (fromPropName propName) <$> goExp expr
  IR.ObjectUpdate _ann expr propValues → do
    add UsesObjectUpdate
    obj ← goExp expr
    vals ←
      Lua.table <$> for (toList propValues) \(propName, e) →
        Lua.tableRowNV (fromPropName propName) <$> goExp e
    pure . Right $
      Lua.functionCall (Lua.varName Fixture.objectUpdateName) [obj, vals]
  IR.Abs _ann param expr → do
    e ← goExp expr
    luaParam ←
      Lua.ParamNamed
        <$> case param of
          IR.ParamUnused _ann → uniqueName "unused"
          IR.ParamNamed _ann name → pure (fromName name)
    pure . Right $ Lua.functionDef [luaParam] [Lua.return e]
  IR.App _ann expr param → do
    e ← goExp expr
    a ← goExp param
    pure . Right $ Lua.functionCall e [a]
  IR.Ref _ann qualifiedName index →
    pure . Right $ case qualifiedName of
      IR.Local name
        | topLevelName ← fromQName modname name
        , Set.member topLevelName topLevelNames →
            Lua.varName topLevelName
      IR.Local name →
        Lua.varName (fromNameWithIndex name index)
      IR.Imported modname' name →
        Lua.varName (fromQName modname' name)
  IR.Let _ann bindings bodyExp → do
    body ← go bodyExp
    recs ←
      bindings & foldMapM \case
        IR.Standalone (_ann, name, expr) →
          DList.singleton . Lua.local1 (fromName name) <$> goExp expr
        IR.RecursiveGroup grp → do
          let binds =
                toList grp <&> \(_ann, irName, _) → do
                  let name =
                        if Set.member (fromQName modname irName) topLevelNames
                          then fromQName modname irName
                          else fromName irName
                  Lua.Local name Nothing
          assignments ← forM (toList grp) \(_ann, irName, expr) → do
            let name =
                  if Set.member (fromQName modname irName) topLevelNames
                    then fromQName modname irName
                    else fromName irName
            Lua.assign (Lua.VarName name) <$> goExp expr
          pure $ DList.fromList binds <> DList.fromList assignments
    pure . Left . DList.toList $
      recs <> either DList.fromList (DList.singleton . Lua.return) body
  IR.IfThenElse _ann cond th el → do
    thenExp ← go th
    elseExp ← go el
    condExp ← goExp cond
    let
      thenBranch = either id (pure . Lua.return) thenExp
      elseBranch = either id (pure . Lua.return) elseExp
    pure $ Left [Lua.ifThenElse condExp thenBranch elseBranch]
  IR.Exception _ann msg →
    pure . Right $ Lua.error msg
  IR.ForeignImport _ann _moduleName path annotatedNames → do
    let foreignNames = fromName <<$>> annotatedNames
    Foreign.Source {header, exports} ←
      Oops.hoistEither =<< liftIO do
        left LinkerErrorForeign
          <$> Foreign.parseForeignSource (untag foreigns) path
    let foreignExports ∷ Lua.Exp =
          Lua.table
            [ Lua.tableRowNV name (Lua.ForeignSourceExp src)
            | (key, src) ← toList exports
            , -- Export tables can contain Lua-reserved words as keys
            -- for example: `{ ["for"] = 42 }`
            let name = Key.toSafeName key
            , name `elem` fmap snd foreignNames
            ]
    pure case header of
      Nothing → Right foreignExports
      Just fh → Left $ Lua.ForeignSourceStat fh : [Lua.return foreignExports]
 where
  go ∷ IR.Exp → LuaM e (Either Lua.Chunk Lua.Exp)
  go = fromIR foreigns topLevelNames modname

  goExp ∷ IR.Exp → LuaM e Lua.Exp
  goExp = asExpression <<$>> go

keyCtor ∷ Lua.Exp
keyCtor = Lua.String "$ctor"

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------

uniqueName ∷ MonadState Natural m ⇒ Text → m Lua.Name
uniqueName prefix = do
  index ← get
  modify' (+ 1)
  pure $ Lua.unsafeName (prefix <> show index)

qualifyName ∷ ModuleName → Lua.Name → Lua.Name
qualifyName modname = Name.join2 (fromModuleName modname)
