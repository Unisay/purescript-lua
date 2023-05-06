{-# LANGUAGE QuasiQuotes #-}

module Language.PureScript.Backend.Lua
  ( fromIrModules
  , fromName
  , qualifyName
  , Error (..)
  ) where

import Control.Monad (ap)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Oops (CouldBe, Variant)
import Control.Monad.Oops qualified as Oops
import Data.DList (DList)
import Data.DList qualified as DList
import Data.Graph (graphFromEdges', reverseTopSort)
import Data.Set qualified as Set
import Data.Tagged (Tagged (..))
import Data.Text qualified as Text
import Data.Traversable (for)
import Language.PureScript.Backend.IR.Types (bindingNames)
import Language.PureScript.Backend.IR.Types qualified as IR
import Language.PureScript.Backend.Lua.Linker.Foreign qualified as Foreign
import Language.PureScript.Backend.Lua.Name qualified as Lua
import Language.PureScript.Backend.Lua.Name qualified as Name
import Language.PureScript.Backend.Lua.Optimizer (optimizeChunk)
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Path (Abs, Dir, Path, toFilePath)
import Prelude hiding (exp, local)

type LuaM a = StateT Natural (Either Error) a

data Error
  = UnexpectedRefBound IR.ModuleName IR.Exp
  | LinkerErrorForeign Foreign.Error
  deriving stock (Show)

fromIrModules
  :: e `CouldBe` Error
  => Tagged "foreign" (Path Abs Dir)
  -> [IR.Module]
  -> ExceptT (Variant e) IO Lua.Chunk
fromIrModules (Tagged foreigns) modules =
  optimizeChunk . DList.toList <$> do
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

        moduleChunk <- fromIrModule m & Oops.hoistEither

        pure $
          DList.fromList
            [ Lua.local1 qfname (Lua.thunks foreignCode)
            | not (null foreignCode)
            ]
            <> do
              DList.fromList moduleForeigns <&> \name ->
                Lua.local1
                  (fromName moduleName name)
                  (Lua.varField (Lua.varName qfname) (fromNameLocal name))
            <> moduleChunk

fromIrModule :: IR.Module -> Either Error (DList Lua.Statement)
fromIrModule irModule = (`evalStateT` 0) do
  let modname = IR.moduleName irModule
      groupings = IR.moduleBindings irModule
      topBindingNames = Set.fromList do
        groupings >>= (fromName modname <$>) . bindingNames
      foreignNames = Set.fromList do
        fromName modname <$> IR.moduleForeigns irModule
      topNames = topBindingNames <> foreignNames
  groupings & foldMapM \case
    IR.Standalone (name, irExp) -> do
      exp <- fromExp topNames modname irExp
      pure $ DList.singleton (Lua.local1 (fromName modname name) exp)
    IR.RecursiveGroup binds ->
      recBindStatements <$> forM binds \(fromName modname -> name, irExp) ->
        (name,) <$> fromExp topNames modname irExp

fromName :: IR.ModuleName -> IR.Name -> Lua.Name
fromName modname name = qualifyName modname (fromNameLocal name)

fromNameLocal :: HasCallStack => IR.Name -> Lua.Name
fromNameLocal = Name.makeSafe . IR.nameToText

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
    pure $ Lua.functionDef args [Lua.return value]
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
  IR.Abs binding -> do
    (name, expr) <- IR.unbindAbs binding
    go expr <&> \e ->
      Lua.functionDef
        (maybe [] (pure . fromNameLocal) name)
        [Lua.return e]
  IR.App expr arg -> do
    e <- go expr
    a <- go arg
    pure $ Lua.functionCall e [a]
  IR.RefFree qualifiedName ->
    pure case qualifiedName of
      IR.Local name
        | topLevelName <- fromName modname name
        , Set.member topLevelName topLevelNames ->
            Lua.varName topLevelName
      IR.Local name ->
        Lua.varName (fromNameLocal name)
      IR.Imported modname' name ->
        Lua.varName (fromName modname' name)
  IR.RefBound _index ->
    throwError $ UnexpectedRefBound modname ir
  IR.Let binding -> do
    (bindings, bodyExp) <- IR.unbindLet binding
    body <- go bodyExp
    recs <-
      bindings & foldMapM \case
        IR.Standalone (name, expr) -> do
          e <- go expr
          pure $ DList.singleton (Lua.local1 (fromNameLocal name) e)
        IR.RecursiveGroup grp -> do
          let binds =
                toList grp <&> \(irName, _) -> do
                  let name =
                        if Set.member (fromName modname irName) topLevelNames
                          then fromName modname irName
                          else fromNameLocal irName
                  Lua.Local name Nothing
          assignments <- forM (toList grp) \(irName, expr) -> do
            let name =
                  if Set.member (fromName modname irName) topLevelNames
                    then fromName modname irName
                    else fromNameLocal irName
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

topoSorted :: [IR.Module] -> [IR.Module]
topoSorted modules =
  reverseTopSort graph <&> (nodeFromVertex >>> \(m, _, _) -> m)
 where
  (graph, nodeFromVertex) =
    graphFromEdges' $
      modules <&> \m@(IR.Module {..}) -> (m, moduleName, moduleImports)

qualifyName :: IR.ModuleName -> Lua.Name -> Lua.Name
qualifyName modname = Name.join2 (fromModuleName modname)
