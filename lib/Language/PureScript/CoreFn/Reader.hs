{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.CoreFn.Reader where

import Control.Monad.Oops (CouldBe, CouldBeAnyOf, Variant, throw)
import Control.Monad.Oops qualified as Oops
import Data.Aeson qualified as Json
import Data.Map.Lazy qualified as Map
import Data.Tagged (Tagged (unTagged))
import Data.Text qualified as Text
import Language.PureScript.CoreFn qualified as Cfn
import Language.PureScript.CoreFn.FromJSON
  ( ModuleWithVersion
  , moduleWithoutVersion
  )
import Language.PureScript.Names qualified as PS
import Path
  ( Abs
  , Dir
  , File
  , Path
  , SomeBase (..)
  , mkRelFile
  , parseRelDir
  , toFilePath
  , (</>)
  )
import Path.IO (doesFileExist, makeAbsolute)

readModuleRecursively
  :: forall e
   . e `CouldBeAnyOf` '[ModuleNotFound, ModuleDecodingErr]
  => Tagged "output" (SomeBase Dir)
  -> PS.ModuleName
  -> ExceptT (Oops.Variant e) IO (Map PS.ModuleName (Cfn.Module Cfn.Ann))
readModuleRecursively output = recurse mempty . pure
 where
  recurse
    :: Map PS.ModuleName (Cfn.Module Cfn.Ann)
    -> [PS.ModuleName]
    -> ExceptT (Oops.Variant e) IO (Map PS.ModuleName (Cfn.Module Cfn.Ann))
  recurse loaded = \case
    [] -> pure loaded
    modName : otherNames
      | "Prim" `Text.isPrefixOf` PS.runModuleName modName ->
          recurse loaded otherNames
    modName : otherNames
      | Map.member modName loaded ->
          recurse loaded otherNames
    modName : otherNames ->
      readModule output modName >>= \m ->
        recurse
          (Map.insert modName m loaded)
          (otherNames <> (fmap snd . Cfn.moduleImports) m)

readModule
  :: e `CouldBeAnyOf` '[ModuleNotFound, ModuleDecodingErr]
  => Tagged "output" (SomeBase Dir)
  -> PS.ModuleName
  -> ExceptT (Variant e) IO (Cfn.Module Cfn.Ann)
readModule output modName = do
  path <- modulePath output modName
  lift (Json.eitherDecodeFileStrict @ModuleWithVersion (toFilePath path))
    >>= either (throw . ModuleDecodingErr path) (pure . moduleWithoutVersion)

modulePath
  :: e `CouldBe` ModuleNotFound
  => Tagged "output" (SomeBase Dir)
  -> PS.ModuleName
  -> ExceptT (Variant e) IO (Path Abs File)
modulePath psOutPath modName = do
  psOutput <-
    case unTagged psOutPath of
      Abs a -> pure a
      Rel r -> makeAbsolute r
  prd <- parseRelDir (toString (PS.runModuleName modName))
  let path = psOutput </> prd </> $(mkRelFile "corefn.json")
  unlessM (doesFileExist path) $ throw $ ModuleNotFound path
  pure path

--------------------------------------------------------------------------------
-- Errors ----------------------------------------------------------------------

newtype ModuleNotFound = ModuleNotFound (Path Abs File)
data ModuleDecodingErr = ModuleDecodingErr (Path Abs File) String
