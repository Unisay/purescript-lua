{-# LANGUAGE QuasiQuotes #-}

module Language.PureScript.Backend.Lua
  ( fromUberModule
  , fromExp
  , fromName
  , fromQName
  , qualifyName
  , Error (..)
  ) where

import Control.Monad (ap)
import Control.Monad.Oops (CouldBe, Variant)
import Control.Monad.Oops qualified as Oops
import Data.DList (DList)
import Data.DList qualified as DList
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
import Path (Abs, Dir, Path, toFilePath)
import Prelude hiding (exp, local)

type LuaM a = StateT Natural (Either Error) a

data Error
  = UnexpectedRefBound ModuleName IR.Exp
  | LinkerErrorForeign Foreign.Error
  deriving stock (Show)

fromUberModule
  ∷ ∀ e
   . e `CouldBe` Error
  ⇒ Tagged "foreign" (Path Abs Dir)
  → Tagged "needsRuntimeLazy" Bool
  → AppOrModule
  → Linker.UberModule
  → ExceptT (Variant e) IO Lua.Chunk
fromUberModule foreigns needsRuntimeLazy appOrModule uber = do
  foreignBindings ←
    Linker.uberModuleForeigns uber & foldMapM \(moduleName, path, names) → do
      moduleForeign ←
        if null names
          then pure []
          else do
            moduleForeign ←
              Oops.hoistEither =<< liftIO do
                Foreign.resolveForModule path (untag foreigns)
                  <&> first LinkerErrorForeign
            pure . Lua.ForeignSourceCode . Text.strip . decodeUtf8
              <$> readFileBS (toFilePath moduleForeign)

      let qfname = qualifyName moduleName [Lua.name|foreign|]
          foreignTable = Lua.local1 qfname (Lua.thunks moduleForeign)
          foreignFields =
            toList names <&> \name →
              Lua.local1
                (fromQName moduleName name)
                (Lua.varField (Lua.varName qfname) (fromName name))
      pure $ foreignTable : foreignFields

  bindings ←
    Linker.uberModuleBindings uber & foldMapM \case
      IR.Standalone (IR.QName modname name, irExp) → runLuaM do
        exp ← fromExp Set.empty modname irExp
        pure $ DList.singleton (Lua.local1 (fromQName modname name) exp)
      IR.RecursiveGroup binds →
        recBindStatements
          <$> forM binds \(IR.QName modname name, irExp) → runLuaM do
            (fromQName modname name,) <$> fromExp Set.empty modname irExp

  returnExp ←
    case appOrModule of
      AsModule modname →
        Lua.table <$> do
          forM (uberModuleExports uber) \(fromName → name, expr) →
            Lua.tableRowNV name <$> runLuaM (fromExp mempty modname expr)
      AsApplication modname (IR.identToName → name) →
        pure $ Lua.functionCall (Lua.varName (fromQName modname name)) []

  pure . mconcat $
    [ if usesPrimModule uber then [Fixture.prim] else empty
    , if untag needsRuntimeLazy && usesRuntimeLazy uber
        then pure Fixture.runtimeLazy
        else empty
    , foreignBindings
    , DList.toList bindings
    , [Lua.Return (Lua.ann returnExp)]
    ]
 where
  runLuaM ∷ LuaM a → ExceptT (Variant e) IO a
  runLuaM = Oops.hoistEither . (`evalStateT` 0)

fromQName ∷ ModuleName → IR.Name → Lua.Name
fromQName modname name = qualifyName modname (fromName name)

fromName ∷ HasCallStack ⇒ IR.Name → Lua.Name
fromName = Name.makeSafe . IR.nameToText

fromModuleName ∷ ModuleName → Lua.Name
fromModuleName = Name.makeSafe . runModuleName

fromPropName ∷ IR.PropName → Lua.Name
fromPropName (IR.PropName name) = Name.makeSafe name

fromExp ∷ Set Lua.Name → ModuleName → IR.Exp → LuaM Lua.Exp
fromExp topLevelNames modname ir = case ir of
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
    flip Lua.varIndex keyCtor <$> go (IR.unAnn e)
  IR.DataArgumentByIndex i e →
    flip Lua.varIndex (Lua.Integer (fromIntegral i)) <$> go (IR.unAnn e)
  IR.Eq l r →
    Lua.equalTo <$> go (IR.unAnn l) <*> go (IR.unAnn r)
  IR.Ctor _algebraicTy _tyName ctorName fieldNames →
    pure $ Lua.functionDef (ParamNamed <$> args) [Lua.return value]
   where
    value = Lua.table $ ctorRow : attributes
    ctorValue =
      Name.toText (fromModuleName modname)
        <> "."
        <> IR.renderCtorName ctorName
    ctorRow = Lua.tableRowKV keyCtor (Lua.String ctorValue)
    args = Name.unsafeName . IR.renderFieldName <$> fieldNames
    attributes = args <&> ap Lua.tableRowNV Lua.varName
  IR.ArrayLength e →
    Lua.hash <$> go (IR.unAnn e)
  IR.ArrayIndex expr index →
    flip Lua.varIndex (Lua.Integer (fromIntegral index)) <$> go (IR.unAnn expr)
  IR.ObjectProp expr propName →
    flip Lua.varField (fromPropName propName) <$> go (IR.unAnn expr)
  IR.ObjectUpdate _expr _patches →
    Prelude.error "fromObjectUpdate is not implemented"
  IR.Abs param expr → do
    e ← go $ IR.unAnn expr
    luaParam ←
      Lua.ParamNamed
        <$> case IR.unAnn param of
          IR.ParamUnused → do
            index ← get
            modify' (+ 1)
            pure $ Lua.unsafeName ("unused" <> show index)
          IR.ParamNamed name → pure (fromName name)
    pure $ Lua.functionDef [luaParam] [Lua.return e]
  IR.App expr param → do
    e ← go $ IR.unAnn expr
    a ← go $ IR.unAnn param
    pure $ Lua.functionCall e [a]
  IR.Ref qualifiedName _index →
    pure case qualifiedName of
      IR.Local name
        | topLevelName ← fromQName modname name
        , Set.member topLevelName topLevelNames →
            Lua.varName topLevelName
      IR.Local name →
        Lua.varName (fromName name)
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
 where
  go ∷ IR.Exp → LuaM Lua.Exp
  go = fromExp topLevelNames modname

keyCtor ∷ Lua.Exp
keyCtor = Lua.String "$ctor"

recBindStatements ∷ NonEmpty (Lua.Name, Lua.Exp) → DList Lua.Statement
recBindStatements (toList → binds) = fmap Lua.local0 names <> assigns
 where
  names ∷ DList Lua.Name =
    DList.fromList (fst <$> binds)
  assigns ∷ DList Lua.Statement =
    DList.fromList (binds <&> \(name, exp) → Lua.assign (Lua.VarName name) exp)

fromIfThenElse ∷ Lua.Exp → Lua.Exp → Lua.Exp → Lua.Exp
fromIfThenElse cond thenExp elseExp = Lua.functionCall fun []
 where
  thenBranch = [Lua.return thenExp]
  elseBranch = [Lua.return elseExp]
  fun = Lua.functionDef [] [Lua.ifThenElse cond thenBranch elseBranch]

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------

qualifyName ∷ ModuleName → Lua.Name → Lua.Name
qualifyName modname = Name.join2 (fromModuleName modname)
