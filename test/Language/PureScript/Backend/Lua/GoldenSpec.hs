{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.Backend.Lua.GoldenSpec where

import Control.Monad.Catch (MonadMask)
import Control.Monad.Oops qualified as Oops
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.String qualified as String
import Data.Tagged (Tagged (..))
import Data.Text qualified as Text
import Language.PureScript.Backend.IR qualified as IR
import Language.PureScript.Backend.IR.DCE qualified as DCE
import Language.PureScript.Backend.IR.Optimizer (optimizeAll)
import Language.PureScript.Backend.IR.Query qualified as Query
import Language.PureScript.Backend.Lua qualified as Lua
import Language.PureScript.Backend.Lua.DeadCodeEliminator (DceMode (PreserveTopLevel), eliminateDeadCode)
import Language.PureScript.Backend.Lua.Fixture qualified as Fixture
import Language.PureScript.Backend.Lua.Optimizer (optimizeChunk)
import Language.PureScript.Backend.Lua.Printer qualified as Printer
import Language.PureScript.CoreFn.Reader qualified as CoreFn
import Language.PureScript.Names qualified as PS
import Path
  ( Abs
  , Dir
  , File
  , Path
  , Rel
  , SomeBase (..)
  , dirname
  , filename
  , mkRelDir
  , parent
  , reldir
  , toFilePath
  , (</>)
  )
import Path.IO
  ( AnyPath (makeRelativeToCurrentDir)
  , ensureDir
  , makeAbsolute
  , walkDirAccum
  , withCurrentDir
  )
import Path.Posix (mkRelFile)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import System.FilePath qualified as FilePath
import System.Process.Typed
  ( ExitCode (..)
  , readProcessInterleaved
  , runProcess
  , setWorkingDir
  , shell
  )
import Test.Hspec
  ( Spec
  , beforeAll_
  , describe
  , it
  , runIO
  , shouldBe
  , shouldNotBe
  )
import Test.Hspec.Extra (annotatingWith)
import Test.Hspec.Golden (defaultGolden)
import Text.Pretty.Simple (OutputOptions (..), defaultOutputOptionsNoColor, pShowOpt)

spec :: Spec
spec = do
  describe "Goldens: *.purs -> *.lua" do
    let compilePs = do
          putText "Comipling PureScript sources"
          exitCode <-
            runProcess . setWorkingDir "test/ps" . shell $
              String.unwords ["spago", "build", "-u", "'-g corefn'"]
          exitCode `shouldBe` ExitSuccess
        psOutputPath = $(mkRelDir "test/ps/output/")
    describe "compiles corefn files to lua" $ beforeAll_ compilePs do
      runIO $ ensureDir psOutputPath
      corefns <- runIO $ collectGoldenCorefns psOutputPath
      it "Finds some corefn files" $ corefns `shouldNotBe` mempty
      for_ corefns \corefn -> do
        let moduleName =
              parent corefn
                & dirname
                & FilePath.dropTrailingPathSeparator . toFilePath
                & PS.ModuleName . toText

            modulePath = parent corefn
        -- IR golden
        let irGolden = modulePath </> $(mkRelFile "golden.ir")
        let irActual = modulePath </> $(mkRelFile "actual.ir")
        irTestName <- runIO do
          toFilePath <$> makeRelativeToCurrentDir irGolden
        it irTestName do
          defaultGolden irGolden (Just irActual) do
            irOutput <- compileCorefn (Tagged (Rel psOutputPath)) moduleName
            pure . toStrict $
              pShowOpt
                defaultOutputOptionsNoColor
                  { outputOptionsIndentAmount = 2
                  , outputOptionsPageWidth = 256
                  , outputOptionsCompact = True
                  }
                irOutput
        -- lua golden
        let luaGolden = modulePath </> $(mkRelFile "golden.lua")
        let luaActual = modulePath </> $(mkRelFile "actual.lua")
        luaTestName <- runIO do
          toFilePath <$> makeRelativeToCurrentDir luaGolden
        it luaTestName do
          defaultGolden luaGolden (Just luaActual) do
            modules <- compileCorefn (Tagged (Rel psOutputPath)) moduleName
            compileIr modules

    describe "golden files should typecheck" do
      luas <- runIO do collectLuas psOutputPath
      for_ luas \lua -> do
        luaFileName <- runIO do makeRelativeToCurrentDir lua
        it (toFilePath luaFileName) do
          let process =
                fromString . List.unwords $
                  [ "luacheck"
                  , "--quiet"
                  , "--std lua53"
                  , "--no-color"
                  , "--no-unused" -- TODO: harden eventually
                  , "--no-max-line-length"
                  , "--formatter plain"
                  , toFilePath lua
                  ]
          (exitCode, out) <- readProcessInterleaved process
          let niceOut =
                decodeUtf8 out
                  & lines
                  & fmap Text.stripStart
                  & filter (not . Text.null)
                  & unlines
                  & toString
          exitCode `shouldBe` ExitSuccess `annotatingWith` niceOut

collectGoldenCorefns :: MonadIO m => Path Rel Dir -> m [Path Abs File]
collectGoldenCorefns = walkDirAccum
  Nothing -- Descend into every directory
  \dir _subdirs files ->
    pure
      [ file
      | file <- files
      , toFilePath (filename file) == "corefn.json"
      , "Golden." `isPrefixOf` toFilePath (dirname dir)
      ]

collectLuas :: MonadIO m => Path Rel Dir -> m [Path Abs File]
collectLuas = walkDirAccum
  Nothing -- Descend into every directory
  \_dir _subdirs files ->
    pure [file | file <- files, toFilePath (filename file) == "golden.lua"]

compileCorefn
  :: forall m
   . (MonadIO m, MonadFail m)
  => Tagged "output" (SomeBase Dir)
  -> PS.ModuleName
  -> m [IR.Module]
compileCorefn outputDir moduleName = do
  cfnModules <-
    CoreFn.readModuleRecursively outputDir moduleName
      & handleModuleNotFoundError
      & handleModuleDecodingError
      & Oops.runOops
      & liftIO

  let irModuleName = IR.mkModuleName moduleName
  modules <-
    forM (toList cfnModules) $
      either (fail . show) (pure . snd) . IR.mkModule
  pure do
    let entry = DCE.EntryPointsSomeModules (NE.singleton irModuleName)
    evalState (optimizeAll entry modules) (0 :: Natural)

compileIr :: (MonadIO m, MonadMask m) => [IR.Module] -> m Text
compileIr irModules = withCurrentDir [reldir|test/ps|] do
  let addPrim =
        if any Query.usesPrimModule irModules
          then (Fixture.prim :)
          else identity
  let addRuntimeLazy =
        if any Query.usesRuntimeLazy irModules
          then (Fixture.runtimeLazy :)
          else identity

  foreignPath <- Tagged <$> makeAbsolute [reldir|foreign|]
  luaChunk <-
    Lua.fromIrModules foreignPath irModules
      & handleLuaError
      & Oops.runOops
      & liftIO

  let doc =
        luaChunk
          & addPrim
          & addRuntimeLazy
          & optimizeChunk
          & eliminateDeadCode PreserveTopLevel
          & Printer.printLuaChunk
  let addTrailingLf = (<> "\n")
  pure $ addTrailingLf $ renderStrict $ layoutPretty defaultLayoutOptions doc

--------------------------------------------------------------------------------
-- Error handlers --------------------------------------------------------------

handleModuleNotFoundError
  :: ExceptT (Oops.Variant (CoreFn.ModuleNotFound ': e)) IO a
  -> ExceptT (Oops.Variant e) IO a
handleModuleNotFoundError = Oops.catch \(CoreFn.ModuleNotFound p) ->
  die . toString . unlines $
    [ "Can't find CoreFn module file: " <> toText (toFilePath p)
    , "Please make sure you did run purs with the `-g corefn` arg."
    ]

handleModuleDecodingError
  :: ExceptT (Oops.Variant (CoreFn.ModuleDecodingErr ': e)) IO a
  -> ExceptT (Oops.Variant e) IO a
handleModuleDecodingError = Oops.catch \(CoreFn.ModuleDecodingErr p e) ->
  die . toString . unlines $
    ["Can't parse CoreFn module file: " <> toText (toFilePath p), toText e]

handleLuaError
  :: ExceptT (Oops.Variant (Lua.Error ': e)) IO a
  -> ExceptT (Oops.Variant e) IO a
handleLuaError = Oops.catch \(e :: Lua.Error) -> die $ show e
