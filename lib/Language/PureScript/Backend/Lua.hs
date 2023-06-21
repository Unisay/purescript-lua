{-# LANGUAGE QuasiQuotes #-}

module Language.PureScript.Backend.Lua
  ( fromUberModule
  , fromIrModules
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
import Data.Tagged (Tagged (..))
import Data.Text qualified as Text
import Data.Traversable (for)
import Language.PureScript.Backend.IR.Linker (topoSorted)
import Language.PureScript.Backend.IR.Linker qualified as Linker
import Language.PureScript.Backend.IR.Types (bindingNames)
import Language.PureScript.Backend.IR.Types qualified as IR
import Language.PureScript.Backend.Lua.Linker.Foreign qualified as Foreign
import Language.PureScript.Backend.Lua.Name qualified as Lua
import Language.PureScript.Backend.Lua.Name qualified as Name
import Language.PureScript.Backend.Lua.Types (ParamF (..))
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Path (Abs, Dir, Path, toFilePath)
import Prelude hiding (exp, local)

type LuaM a = StateT Natural (Either Error) a

data Error
  = UnexpectedRefBound IR.ModuleName IR.Exp
  | LinkerErrorForeign Foreign.Error
  deriving stock (Show)

fromUberModule
  :: e `CouldBe` Error
  => Tagged "foreign" (Path Abs Dir)
  -> Linker.UberModule
  -> ExceptT (Variant e) IO Lua.Chunk
fromUberModule (Tagged foreigns) uberModule = do
  foreignBindings <-
    Linker.uberModuleForeigns uberModule
      & foldMapM \(moduleName, modulePath, names) -> do
        moduleForeign <-
          if null names
            then pure []
            else do
              moduleForeign <-
                liftIO (Foreign.resolveForModule modulePath foreigns)
                  >>= Oops.hoistEither . first LinkerErrorForeign
              pure . Lua.ForeignSourceCode . Text.strip . decodeUtf8
                <$> readFileBS (toFilePath moduleForeign)

        let fname = [Lua.name|foreign|]
            qfname = qualifyName moduleName fname
            foreignTable = Lua.local1 qfname (Lua.thunks moduleForeign)
            foreignFields =
              names
                <&> \name ->
                  Lua.local1
                    (fromQName moduleName name)
                    (Lua.varField (Lua.varName qfname) (fromName name))
        pure $ foreignTable : toList foreignFields
  bindings <-
    Linker.uberModuleBindings uberModule
      & foldMapM \case
        IR.Standalone (IR.QName modname name, irExp) -> do
          exp <- fromExp Set.empty modname irExp
          pure $ DList.singleton (Lua.local1 (fromQName modname name) exp)
        IR.RecursiveGroup binds ->
          recBindStatements
            <$> forM binds \(IR.QName modname name, irExp) ->
              (fromQName modname name,) <$> fromExp Set.empty modname irExp
      & (`evalStateT` 0)
      & Oops.hoistEither
  pure $ foreignBindings <> DList.toList bindings

fromIrModules
  :: e `CouldBe` Error
  => Tagged "foreign" (Path Abs Dir)
  -> [IR.Module]
  -> ExceptT (Variant e) IO Lua.Chunk
fromIrModules (Tagged foreigns) modules =
  DList.toList <$> do
    DList.fromList (topoSorted modules)
      & foldMapM \m@IR.Module {..} -> do
        foreignCode <-
          if null moduleForeigns
            then pure mempty
            else do
              moduleForeign <-
                liftIO (Foreign.resolveForModule modulePath foreigns)
                  >>= Oops.hoistEither . first LinkerErrorForeign
              pure . Lua.ForeignSourceCode . Text.strip . decodeUtf8
                <$> readFileBS (toFilePath moduleForeign)

        let fname = [Lua.name|foreign|]
            qfname = qualifyName moduleName fname

        moduleChunk <-
          fromIrModule m
            & (`evalStateT` 0)
            & Oops.hoistEither

        pure $
          DList.fromList
            [ Lua.local1 qfname (Lua.thunks foreignCode)
            | not (null foreignCode)
            ]
            <> do
              DList.fromList moduleForeigns <&> \name ->
                Lua.local1
                  (fromQName moduleName name)
                  (Lua.varField (Lua.varName qfname) (fromName name))
            <> moduleChunk

fromIrModule :: IR.Module -> LuaM (DList Lua.Statement)
fromIrModule irModule = do
  let modname = IR.moduleName irModule
      groupings = IR.moduleBindings irModule
      topBindingNames = Set.fromList do
        groupings >>= (fromQName modname <$>) . bindingNames
      foreignNames = Set.fromList do
        fromQName modname <$> IR.moduleForeigns irModule
      topNames = topBindingNames <> foreignNames
  groupings & foldMapM \case
    IR.Standalone (name, irExp) -> do
      exp <- fromExp topNames modname irExp
      pure $ DList.singleton (Lua.local1 (fromQName modname name) exp)
    IR.RecursiveGroup binds ->
      recBindStatements <$> forM binds \(fromQName modname -> name, irExp) ->
        (name,) <$> fromExp topNames modname irExp

fromQName :: IR.ModuleName -> IR.Name -> Lua.Name
fromQName modname name = qualifyName modname (fromName name)

fromName :: HasCallStack => IR.Name -> Lua.Name
fromName = Name.makeSafe . IR.nameToText

fromModuleName :: IR.ModuleName -> Lua.Name
fromModuleName = Name.makeSafe . IR.renderModuleName

fromPropName :: IR.PropName -> Lua.Name
fromPropName (IR.PropName name) = Name.makeSafe name

fromExp :: Set Lua.Name -> IR.ModuleName -> IR.Exp -> LuaM Lua.Exp
fromExp topLevelNames modname ir = case IR.unExp ir of
  IR.Lit literal ->
    case literal of
      IR.Integer i ->
        pure $ Lua.Integer i
      IR.Floating d ->
        pure $ Lua.Float d
      IR.String s ->
        pure $ Lua.String s
      IR.Char c ->
        pure $ Lua.String $ Text.singleton c
      IR.Boolean b ->
        pure $ Lua.Boolean b
      IR.Array exprs ->
        Lua.table <$> forM (zip [1 ..] exprs) \(i, e) ->
          Lua.tableRowKV (Lua.Integer i) <$> go e
      IR.Object kvs ->
        Lua.table <$> for kvs \(prop, exp) ->
          Lua.tableRowNV (fromPropName prop) <$> go exp
  IR.Prim op ->
    case op of
      IR.ArrayLength e ->
        Lua.hash <$> go e
      IR.ReflectCtor e ->
        flip Lua.varIndex keyCtor <$> go e
      IR.DataArgumentByIndex i e ->
        flip Lua.varIndex (Lua.Integer (fromIntegral i)) <$> go e
      IR.Eq l r ->
        Lua.equalTo <$> go l <*> go r
  IR.Ctor _algebraicTy _tyName ctorName fieldNames ->
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
  IR.ArrayIndex expr index ->
    flip Lua.varIndex (Lua.Integer (fromIntegral index)) <$> go expr
  IR.ObjectProp expr propName ->
    flip Lua.varField (fromPropName propName) <$> go expr
  IR.ObjectUpdate _expr _patches ->
    Prelude.error "fromObjectUpdate is not implemented"
  IR.Abs param expr -> do
    e <- go expr
    luaParam <-
      Lua.ParamNamed
        <$> case param of
          IR.ParamUnused -> gets (Lua.unsafeName . ("unused" <>) . show)
          IR.ParamNamed name -> pure (fromName name)
    pure $ Lua.functionDef [luaParam] [Lua.return e]
  IR.App expr param -> do
    e <- go expr
    a <- go param
    pure $ Lua.functionCall e [a]
  IR.Ref qualifiedName _index ->
    pure case qualifiedName of
      IR.Local name
        | topLevelName <- fromQName modname name
        , Set.member topLevelName topLevelNames ->
            Lua.varName topLevelName
      IR.Local name ->
        Lua.varName (fromName name)
      IR.Imported modname' name ->
        Lua.varName (fromQName modname' name)
  IR.Let bindings bodyExp -> do
    body <- go bodyExp
    recs <-
      bindings & foldMapM \case
        IR.Standalone (name, expr) -> do
          e <- go expr
          pure $ DList.singleton (Lua.local1 (fromName name) e)
        IR.RecursiveGroup grp -> do
          let binds =
                toList grp <&> \(irName, _) -> do
                  let name =
                        if Set.member (fromQName modname irName) topLevelNames
                          then fromQName modname irName
                          else fromName irName
                  Lua.Local name Nothing
          assignments <- forM (toList grp) \(irName, expr) -> do
            let name =
                  if Set.member (fromQName modname irName) topLevelNames
                    then fromQName modname irName
                    else fromName irName
            Lua.assign (Lua.VarName name) <$> go expr
          pure $ DList.fromList binds <> DList.fromList assignments
    pure . Lua.scope . DList.toList $ DList.snoc recs (Lua.return body)
  IR.IfThenElse cond th el ->
    fromIfThenElse <$> go cond <*> go th <*> go el
  IR.Exception msg ->
    pure $ Lua.error msg
 where
  go :: IR.Exp -> LuaM Lua.Exp
  go = fromExp topLevelNames modname

keyCtor :: Lua.Exp
keyCtor = Lua.String "$ctor"

recBindStatements :: NonEmpty (Lua.Name, Lua.Exp) -> DList Lua.Statement
recBindStatements (toList -> binds) = fmap Lua.local0 names <> assigns
 where
  names :: DList Lua.Name =
    DList.fromList (fst <$> binds)
  assigns :: DList Lua.Statement =
    DList.fromList (binds <&> \(name, exp) -> Lua.assign (Lua.VarName name) exp)

fromIfThenElse :: Lua.Exp -> Lua.Exp -> Lua.Exp -> Lua.Exp
fromIfThenElse cond thenExp elseExp = Lua.functionCall fun []
 where
  thenBranch = [Lua.return thenExp]
  elseBranch = [Lua.return elseExp]
  fun = Lua.functionDef [] [Lua.ifThenElse cond thenBranch elseBranch]

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------

qualifyName :: IR.ModuleName -> Lua.Name -> Lua.Name
qualifyName modname = Name.join2 (fromModuleName modname)
