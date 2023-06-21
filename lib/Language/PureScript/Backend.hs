module Language.PureScript.Backend where

import Control.Monad.Oops (CouldBeAnyOf, Variant)
import Control.Monad.Oops qualified as Oops
import Data.Map qualified as Map
import Data.Tagged (Tagged (..), untag)
import Language.PureScript.Backend.IR qualified as IR
import Language.PureScript.Backend.IR.DCE qualified as DCE
import Language.PureScript.Backend.IR.Linker qualified as Linker
import Language.PureScript.Backend.IR.Optimizer (optimizeUberModule)
import Language.PureScript.Backend.IR.Query
  ( usesPrimModuleUber
  , usesRuntimeLazyUber
  )
import Language.PureScript.Backend.Lua qualified as Lua
import Language.PureScript.Backend.Lua.DeadCodeEliminator
  ( DceMode (PreserveReturned)
  , eliminateDeadCode
  )
import Language.PureScript.Backend.Lua.Fixture qualified as Fixture
import Language.PureScript.Backend.Lua.Optimizer (optimizeChunk)
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
  let
    (needsRuntimeLazys, irModules) = unzip irResults
    uberModule@Linker.UberModule {..} =
      Linker.makeUberModule (linkerMode appOrModule) irModules
    optimizedUberModule = optimizeUberModule dceEntryPoint uberModule
    addPrim =
      if usesPrimModuleUber optimizedUberModule
        then (Fixture.prim :)
        else identity
    addRuntimeLazy =
      if or (untag <$> needsRuntimeLazys)
        && usesRuntimeLazyUber optimizedUberModule
        then (Fixture.runtimeLazy :)
        else identity
    addReturn =
      flip
        mappend
        [ Lua.Return $ Lua.ann case appOrModule of
            AsModule (ModuleEntryPoint _modname) ->
              Lua.table $
                uberModuleExports <&> \(modname, name) ->
                  Lua.tableRowNV
                    (Lua.fromName name)
                    (Lua.varName (Lua.fromQName modname name))
            AsApplication (AppEntryPoint modul ident) ->
              Lua.functionCall
                ( Lua.varName $
                    Lua.fromQName (IR.mkModuleName modul) (IR.identToName ident)
                )
                []
        ]

  modulesChunk <- Lua.fromUberModule foreignDir optimizedUberModule
  pure $
    modulesChunk
      & addPrim
      & addRuntimeLazy
      & addReturn
      & optimizeChunk
      & eliminateDeadCode PreserveReturned
 where
  dceEntryPoint =
    case appOrModule of
      AsApplication (AppEntryPoint modul entryIdent) ->
        DCE.EntryPoint (IR.mkModuleName modul) [IR.identToName entryIdent]
      AsModule (ModuleEntryPoint modul) ->
        DCE.EntryPoint (IR.mkModuleName modul) []

linkerMode :: AppOrModule -> Linker.LinkMode
linkerMode = \case
  AsApplication (AppEntryPoint psModuleName psIdent) ->
    Linker.LinkAsApplication
      (IR.mkModuleName psModuleName)
      (IR.identToName psIdent)
  AsModule (ModuleEntryPoint psModuleName) ->
    Linker.LinkAsModule (IR.mkModuleName psModuleName)
