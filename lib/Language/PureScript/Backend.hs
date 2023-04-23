module Language.PureScript.Backend where

import Control.Monad.Oops (CouldBeAnyOf, Variant)
import Control.Monad.Oops qualified as Oops
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Tagged (Tagged (..), untag)
import Language.PureScript.Backend.IR qualified as IR
import Language.PureScript.Backend.IR.DCE qualified as DCE
import Language.PureScript.Backend.IR.Optimizer (optimizeAll)
import Language.PureScript.Backend.IR.Query (usesPrimModule, usesRuntimeLazy)
import Language.PureScript.Backend.Lua qualified as Lua
import Language.PureScript.Backend.Lua.DeadCodeEliminator (DceMode (PreserveReturned), eliminateDeadCode)
import Language.PureScript.Backend.Lua.Fixture qualified as Fixture
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Language.PureScript.CoreFn.Reader qualified as CoreFn
import Language.PureScript.Names qualified as PS
import Path (Abs, Dir, Path, SomeBase)
import Prelude hiding (show)

data AppOrModule
  = AsApplication AppEntryPoint
  | AsModule ModuleEntryPoint
  deriving stock (Show)

data AppEntryPoint = AppEntryPoint PS.ModuleName PS.Ident
  deriving stock (Show)

newtype ModuleEntryPoint = ModuleEntryPoint PS.ModuleName
  deriving stock (Show)

entryPointModule :: AppOrModule -> PS.ModuleName
entryPointModule = \case
  AsApplication (AppEntryPoint modul _ident) -> modul
  AsModule (ModuleEntryPoint modul) -> modul

compileModules
  :: e
    `CouldBeAnyOf` '[ CoreFn.ModuleNotFound
                    , CoreFn.ModuleDecodingErr
                    , IR.CoreFnError
                    , Lua.Error
                    ]
  => Tagged "output" (SomeBase Dir)
  -> Tagged "foreign" (Path Abs Dir)
  -> AppOrModule
  -> ExceptT (Variant e) IO Lua.Chunk
compileModules outputDir foreignDir appOrModule = do
  let entryModuleName = entryPointModule appOrModule
  cfnModules <- CoreFn.readModuleRecursively outputDir entryModuleName
  irResults <- forM (Map.toList cfnModules) \(_psModuleName, cfnModule) ->
    Oops.hoistEither $ IR.mkModule cfnModule
  let (needsRuntimeLazys, irModules) = unzip irResults
  optimizedModules <-
    evalStateT (optimizeAll irDceStrategy irModules) (0 :: Natural)
  let
    addPrim =
      if any usesPrimModule optimizedModules
        then (Fixture.prim :)
        else identity
    addRuntimeLazy =
      if or (fmap untag needsRuntimeLazys)
        && any usesRuntimeLazy optimizedModules
        then (Fixture.runtimeLazy :)
        else identity
  chunk <- Lua.fromIrModules foreignDir optimizedModules
  pure . eliminateDeadCode PreserveReturned $
    addRuntimeLazy (addPrim chunk)
      <> [ Lua.Return $ Lua.ann case appOrModule of
            AsModule (ModuleEntryPoint modname) ->
              let entryModule =
                    optimizedModules & List.find \IR.Module {moduleName} ->
                      moduleName == IR.mkModuleName modname
               in Lua.table case entryModule of
                    Nothing -> []
                    Just IR.Module {moduleExports, moduleName} ->
                      moduleExports <&> \(Lua.fromName moduleName -> name) ->
                        Lua.tableRowNV name (Lua.varName name)
            AsApplication (AppEntryPoint modul ident) ->
              Lua.functionCall
                ( Lua.varName $
                    Lua.fromName (IR.mkModuleName modul) (IR.identToName ident)
                )
                []
         ]
 where
  irDceStrategy =
    case appOrModule of
      AsApplication (AppEntryPoint modul entryIdent) ->
        DCE.EntryPoints $
          NE.singleton
            ( IR.mkModuleName modul
            , NE.singleton (IR.identToName entryIdent)
            )
      AsModule (ModuleEntryPoint modul) ->
        DCE.EntryPointsSomeModules (NE.singleton (IR.mkModuleName modul))
