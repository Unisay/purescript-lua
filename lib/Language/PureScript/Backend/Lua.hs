module Language.PureScript.Backend.Lua
  ( fromUberModule
  , fromExp
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
    bindings ←
      Linker.uberModuleBindings uber & foldMapM \case
        IR.Standalone (IR.QName modname name, irExp) → do
          exp ← fromExp foreigns Set.empty modname irExp
          pure $ DList.singleton (Lua.local1 (fromQName modname name) exp)
        IR.RecursiveGroup recGroup → do
          recBinds ← forM (toList recGroup) \(IR.QName modname name, irExp) →
            (fromQName modname name,) <$> fromExp foreigns Set.empty modname irExp
          let declarations = Lua.local0 . fst <$> DList.fromList recBinds
              assignments = DList.fromList do
                recBinds <&> \(name, exp) → Lua.assign (Lua.VarName name) exp
          pure $ declarations <> assignments

    returnExp ←
      case appOrModule of
        AsModule modname →
          Lua.table <$> do
            forM (uberModuleExports uber) \(fromName → name, expr) →
              Lua.tableRowNV name <$> fromExp foreigns mempty modname expr
        AsApplication modname ident → do
          case List.lookup name (uberModuleExports uber) of
            Just expr → do
              entry ← fromExp foreigns mempty modname expr
              pure $ Lua.functionCall entry []
            _ → Oops.throw $ AppEntryPointNotFound modname ident
         where
          name = IR.identToName ident

    pure $ DList.snoc bindings (Lua.Return (Lua.ann returnExp))

  pure . mconcat $
    [ [Fixture.prim | usesPrimModule uber]
    , [Fixture.runtimeLazy | untag needsRuntimeLazy && usesRuntimeLazy uber]
    , [Fixture.objectUpdate | UsesObjectUpdate ← [usesObjectUpdate]]
    , DList.toList chunk
    ]

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

fromExp
  ∷ ∀ e
   . e `CouldBe` Error
  ⇒ Tagged "foreign" (Path Abs Dir)
  → Set Lua.Name
  → ModuleName
  → IR.Exp
  → LuaM e Lua.Exp
fromExp foreigns topLevelNames modname ir = case ir of
  IR.LiteralInt i →
    pure $ Lua.Integer i
  IR.LiteralFloat d →
    pure $ Lua.Float d
  IR.LiteralString s →
    pure $ Lua.String s
  IR.LiteralChar c →
    pure $ Lua.String $ Text.singleton c
  IR.LiteralBool b →
    pure $ Lua.Boolean b
  IR.LiteralArray exprs →
    Lua.table <$> forM (zip [1 ..] exprs) \(i, e) →
      Lua.tableRowKV (Lua.Integer i) <$> go (IR.unAnn e)
  IR.LiteralObject kvs →
    Lua.table <$> for kvs \(prop, exp) →
      Lua.tableRowNV (fromPropName prop) <$> go (IR.unAnn exp)
  IR.ReflectCtor e →
    (`Lua.varIndex` keyCtor) <$> go (IR.unAnn e)
  IR.DataArgumentByIndex i e →
    (`Lua.varField` Lua.unsafeName ("value" <> show i)) <$> go (IR.unAnn e)
  IR.Eq l r →
    Lua.equalTo <$> go (IR.unAnn l) <*> go (IR.unAnn r)
  IR.Ctor _algebraicTy ctorModName ctorTyName ctorName fieldNames →
    pure $ foldr wrap value args
   where
    wrap name expr = Lua.functionDef [ParamNamed name] [Lua.return expr]
    value = Lua.table $ ctorRow : attributes
    ctorId = IR.ctorId ctorModName ctorTyName ctorName
    ctorRow = Lua.tableRowKV keyCtor (Lua.String ctorId)
    args = Name.unsafeName . IR.renderFieldName <$> fieldNames
    attributes = args <&> ap Lua.tableRowNV Lua.varName
  IR.ArrayLength e →
    Lua.hash <$> go (IR.unAnn e)
  IR.ArrayIndex expr index →
    flip Lua.varIndex (Lua.Integer (fromIntegral index)) <$> go (IR.unAnn expr)
  IR.ObjectProp expr propName →
    flip Lua.varField (fromPropName propName) <$> go (IR.unAnn expr)
  IR.ObjectUpdate expr propValues → do
    add UsesObjectUpdate
    obj ← go (IR.unAnn expr)
    vals ←
      Lua.table <$> for (toList propValues) \(propName, IR.unAnn → e) →
        Lua.tableRowNV (fromPropName propName) <$> go e
    pure $ Lua.functionCall (Lua.varName Fixture.objectUpdateName) [obj, vals]
  IR.Abs param expr → do
    e ← go $ IR.unAnn expr
    luaParam ←
      Lua.ParamNamed
        <$> case IR.unAnn param of
          IR.ParamUnused → uniqueName "unused"
          IR.ParamNamed name → pure (fromName name)
    pure $ Lua.functionDef [luaParam] [Lua.return e]
  IR.App expr param → do
    e ← go $ IR.unAnn expr
    a ← go $ IR.unAnn param
    pure $ Lua.functionCall e [a]
  IR.Ref qualifiedName index →
    pure case qualifiedName of
      IR.Local name
        | topLevelName ← fromQName modname name
        , Set.member topLevelName topLevelNames →
            Lua.varName topLevelName
      IR.Local name →
        Lua.varName (fromNameWithIndex name index)
      IR.Imported modname' name →
        Lua.varName (fromQName modname' name)
  IR.Let bindings bodyExp → do
    body ← go $ IR.unAnn bodyExp
    recs ←
      bindings & foldMapM \case
        IR.Standalone (IR.unAnn → name, IR.unAnn → expr) → do
          e ← go expr
          pure $ DList.singleton (Lua.local1 (fromName name) e)
        IR.RecursiveGroup grp → do
          let binds =
                toList grp <&> \(IR.unAnn → irName, _) → do
                  let name =
                        if Set.member (fromQName modname irName) topLevelNames
                          then fromQName modname irName
                          else fromName irName
                  Lua.Local name Nothing
          assignments ← forM (toList grp) \(IR.unAnn → irName, expr) → do
            let name =
                  if Set.member (fromQName modname irName) topLevelNames
                    then fromQName modname irName
                    else fromName irName
            Lua.assign (Lua.VarName name) <$> go (IR.unAnn expr)
          pure $ DList.fromList binds <> DList.fromList assignments
    pure . Lua.scope . DList.toList $ DList.snoc recs (Lua.return body)
  IR.IfThenElse (IR.unAnn → cond) (IR.unAnn → th) (IR.unAnn → el) →
    fromIfThenElse <$> go cond <*> go th <*> go el
  IR.Exception msg →
    pure $ Lua.error msg
  IR.ForeignImport _moduleName path (map fromName → names) → do
    Foreign.Source {header, exports} ←
      Oops.hoistEither =<< liftIO do
        left LinkerErrorForeign
          <$> Foreign.parseForeignSource (untag foreigns) path
    let foreignHeader = Lua.ForeignSourceStat <$> header
    let foreignExports ∷ [Lua.TableRow] =
          [ Lua.tableRowNV name (Lua.ForeignSourceExp src)
          | (name, src) ← toList exports
          , name `elem` names
          ]
    pure . Lua.thunks $
      maybe id (:) foreignHeader [Lua.return (Lua.table foreignExports)]
 where
  go ∷ IR.Exp → LuaM e Lua.Exp
  go = fromExp foreigns topLevelNames modname

keyCtor ∷ Lua.Exp
keyCtor = Lua.String "$ctor"

fromIfThenElse ∷ Lua.Exp → Lua.Exp → Lua.Exp → Lua.Exp
fromIfThenElse cond thenExp elseExp = Lua.functionCall fun []
 where
  thenBranch = [Lua.return thenExp]
  elseBranch = [Lua.return elseExp]
  fun = Lua.functionDef [] [Lua.ifThenElse cond thenBranch elseBranch]

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------

uniqueName ∷ MonadState Natural m ⇒ Text → m Lua.Name
uniqueName prefix = do
  index ← get
  modify' (+ 1)
  pure $ Lua.unsafeName (prefix <> show index)

qualifyName ∷ ModuleName → Lua.Name → Lua.Name
qualifyName modname = Name.join2 (fromModuleName modname)
