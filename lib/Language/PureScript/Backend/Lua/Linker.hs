{-# LANGUAGE QuasiQuotes #-}

module Language.PureScript.Backend.Lua.Linker where

import Control.Monad.Oops
  ( CouldBe
  , Variant
  )

import Control.Monad.Oops qualified as Oops
import Data.Graph (graphFromEdges', reverseTopSort)
import Data.Tagged (Tagged (..))
import Data.Traversable (for)
import Language.PureScript.Backend.Lua.Linker.Foreign qualified as Foreign
import Language.PureScript.Backend.Lua.Name qualified as Lua
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Path (Abs, Dir, Path, toFilePath)
import Prelude hiding (show)

newtype Error = LinkerErrorForeign Foreign.Error
  deriving newtype (Show)

linkModules
  :: e `CouldBe` Error
  => Tagged "foreign" (Path Abs Dir)
  -> [Lua.Module]
  -> ExceptT (Variant e) IO Lua.Chunk
linkModules (Tagged foreigns) luaModules =
  for (topoSorted luaModules) \Lua.Module {..} -> do
    foreignCode <-
      case moduleForeigns of
        [] -> pure []
        _ -> do
          moduleForeign <-
            liftIO (Foreign.resolveForModule modulePath foreigns)
              >>= Oops.hoistEither . first LinkerErrorForeign
          pure . Lua.ForeignSourceCode . decodeUtf8
            <$> readFileBS (toFilePath moduleForeign)

    let fname = [Lua.name|foreign|]

    pure . Lua.local1 (Lua.unModuleName moduleName) . Lua.thunks . mconcat $
      [ [ Lua.local1 fname (Lua.thunks foreignCode)
        | not (null foreignCode)
        ]
      , moduleForeigns <&> \name ->
          Lua.local1 name (Lua.varField (Lua.varName fname) name)
      , moduleChunk
      , [Lua.Return $ Lua.table $ Lua.pun <$> moduleExports]
      ]

topoSorted :: [Lua.Module] -> [Lua.Module]
topoSorted modules =
  reverseTopSort graph <&> (nodeFromVertex >>> \(m, _, _) -> m)
 where
  (graph, nodeFromVertex) =
    graphFromEdges' $
      modules <&> \m@(Lua.Module {..}) -> (m, moduleName, moduleImports)
