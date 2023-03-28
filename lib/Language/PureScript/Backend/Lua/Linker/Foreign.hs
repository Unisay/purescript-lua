module Language.PureScript.Backend.Lua.Linker.Foreign where

import Data.String qualified as String
import Path (Abs, Dir, File, Path, toFilePath, (</>))
import Path qualified
import Path.IO qualified as Path
import Text.Show (Show (..))
import Prelude hiding (show)

data Error = ForeignFileNotFound
  { modulePath :: FilePath
  , searched :: NonEmpty (Path Abs File)
  }

instance Show Error where
  show ForeignFileNotFound {modulePath, searched} =
    "Foreign file for the module "
      <> show modulePath
      <> " not found in the following locations:\n"
      <> String.unlines (toList searched <&> ("-  " <>) . toFilePath)

resolveForModule
  :: FilePath
  -> Path Abs Dir
  -> IO (Either Error (Path Abs File))
resolveForModule modulePath foreignBaseDir = do
  cwd <- Path.getCurrentDir
  absModulePath <-
    Path.parseAbsFile modulePath
      & maybe (Path.resolveFile cwd modulePath) pure
  -- Its not always true that module path is relative to the cwd
  let relModulePath = Path.makeRelative @_ @Maybe cwd absModulePath
  foreignFile <- Path.filename <$> Path.replaceExtension ".lua" absModulePath
  let searchLocations =
        Path.parent absModulePath
          :| maybeToList (relModulePath <&> (foreignBaseDir </>) . Path.parent)
  filesToSearch <- forM searchLocations $ \location ->
    Path.resolveFile location (toFilePath foreignFile)
  found <- forM filesToSearch \file ->
    bool Nothing (Just file) <$> Path.doesFileExist file
  pure $ ForeignFileNotFound modulePath filesToSearch `maybeToRight` asum found
