module Language.PureScript.Backend where

import Control.Monad.Oops (CouldBeAnyOf, Variant)
import Control.Monad.Oops qualified as Oops
import Data.Map qualified as Map
import Data.Tagged (Tagged (..), untag)
import Language.PureScript.Backend.IR qualified as IR
import Language.PureScript.Backend.IR.Linker qualified as Linker
import Language.PureScript.Backend.IR.Optimizer (optimizedUberModule)
import Language.PureScript.Backend.Lua qualified as Lua
import Language.PureScript.Backend.Lua.Optimizer (optimizeChunk)
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Language.PureScript.Backend.Types (AppOrModule (..), entryPointModule)
import Language.PureScript.CoreFn.Reader qualified as CoreFn
import Path (Abs, Dir, Path, SomeBase)
import Prelude hiding (show)

data CompilationResult = CompilationResult
  { ir ∷ Linker.UberModule
  , lua ∷ Lua.Chunk
  }

compileModules
  ∷ e
    `CouldBeAnyOf` '[ CoreFn.ModuleNotFound
                    , CoreFn.ModuleDecodingErr
                    , IR.CoreFnError
                    , Lua.Error
                    ]
  ⇒ Tagged "output" (SomeBase Dir)
  → Tagged "foreign" (Path Abs Dir)
  → AppOrModule
  → ExceptT (Variant e) IO CompilationResult
compileModules outputDir foreignDir appOrModule = do
  let entryModuleName = entryPointModule appOrModule
  cfnModules ← CoreFn.readModuleRecursively outputDir entryModuleName
  let dataDecls = IR.collectDataDeclarations cfnModules
  irResults ← forM (Map.toList cfnModules) \(_psModuleName, cfnModule) →
    Oops.hoistEither $ IR.mkModule cfnModule dataDecls
  let (needsRuntimeLazys, irModules) = unzip irResults
  let uberModule =
        Linker.makeUberModule (linkerMode appOrModule) irModules
          & optimizedUberModule
  let needsRuntimeLazy = Tagged (any untag needsRuntimeLazys)
  chunk ← Lua.fromUberModule foreignDir needsRuntimeLazy appOrModule uberModule
  pure CompilationResult {lua = optimizeChunk chunk, ir = uberModule}

linkerMode ∷ AppOrModule → Linker.LinkMode
linkerMode = \case
  AsModule psModuleName → Linker.LinkAsModule psModuleName
  AsApplication psModuleName psIdent →
    Linker.LinkAsApplication psModuleName (IR.identToName psIdent)
