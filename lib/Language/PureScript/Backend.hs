module Language.PureScript.Backend where

import Control.Monad.Oops (CouldBeAnyOf, Variant)
import Control.Monad.Oops qualified as Oops
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Tagged (Tagged (..), untag)
import Language.PureScript.Backend.IR qualified as IR
import Language.PureScript.Backend.IR.DCE qualified as DCE
import Language.PureScript.Backend.IR.Optimizer (optimizeAll)
import Language.PureScript.Backend.IR.Query (usesPrimModule, usesRuntimeLazy)
import Language.PureScript.Backend.Lua qualified as Lua
import Language.PureScript.Backend.Lua.Fixture qualified as Fixture
import Language.PureScript.Backend.Lua.Linker qualified as Linker
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
                    , Linker.Error
                    , Lua.Error
                    ]
  => Tagged "output" (SomeBase Dir)
  -> Tagged "foreign" (Path Abs Dir)
  -> AppOrModule
  -> ExceptT (Variant e) IO Lua.Chunk
compileModules outputDir foreignDir appOrModule = do
  let entryModule = entryPointModule appOrModule
  cfnModules <- CoreFn.readModuleRecursively outputDir entryModule
  irResults <- forM (Map.toList cfnModules) \(_psModuleName, cfnModule) ->
    Oops.hoistEither $ IR.mkModule cfnModule
  let (needsRuntimeLazys, irModules) = unzip irResults
      optimizedModules = optimizeAll dceStrategy irModules
  luaModules <- forM optimizedModules $ Oops.hoistEither . Lua.fromIrModule
  let addPrimModule =
        if any usesPrimModule optimizedModules
          then (Fixture.primModule :)
          else identity
  chunk <- Linker.linkModules foreignDir (addPrimModule luaModules)
  let addRuntimeLazy =
        if or (fmap untag needsRuntimeLazys)
          && any usesRuntimeLazy optimizedModules
          then (Fixture.runtimeLazy :)
          else identity
  pure $
    addRuntimeLazy chunk
      <> [ Lua.Return case appOrModule of
            AsModule (ModuleEntryPoint modul) ->
              Lua.varName . Lua.unModuleName . Lua.fromModuleName $
                IR.mkModuleName modul
            AsApplication (AppEntryPoint modul ident) ->
              Lua.functionCall
                ( Linker.linkedVar
                    (Lua.fromModuleName $ IR.mkModuleName modul)
                    (Lua.fromName (IR.identToName ident))
                )
                []
         ]
 where
  dceStrategy =
    case appOrModule of
      AsApplication (AppEntryPoint modul entryIdent) ->
        DCE.EntryPoints $
          NE.singleton
            ( IR.mkModuleName modul
            , NE.singleton (IR.identToName entryIdent)
            )
      AsModule (ModuleEntryPoint modul) ->
        DCE.EntryPointsSomeModules (NE.singleton (IR.mkModuleName modul))
