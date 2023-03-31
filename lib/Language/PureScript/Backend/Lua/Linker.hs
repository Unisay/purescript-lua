{-# LANGUAGE QuasiQuotes #-}

module Language.PureScript.Backend.Lua.Linker where

import Control.Monad.Oops (CouldBe, Variant)
import Control.Monad.Oops qualified as Oops
import Data.Graph (graphFromEdges', reverseTopSort)
import Data.Set qualified as Set
import Data.Tagged (Tagged (..))
import Data.Traversable (for)
import Language.PureScript.Backend.Lua.Linker.Foreign qualified as Foreign
import Language.PureScript.Backend.Lua.Name qualified as Lua
import Language.PureScript.Backend.Lua.Traversal (everywhereStat)
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Path (Abs, Dir, Path, toFilePath)

newtype Error = LinkerErrorForeign Foreign.Error
  deriving newtype (Show)

linkModules
  :: e `CouldBe` Error
  => Tagged "foreign" (Path Abs Dir)
  -> [Lua.Module]
  -> ExceptT (Variant e) IO Lua.Chunk
linkModules (Tagged foreigns) luaModules =
  join <$> for (topoSorted luaModules) \m@Lua.Module {..} -> do
    foreignCode <-
      case moduleForeigns of
        [] -> pure []
        _ -> do
          moduleForeign <-
            liftIO (Foreign.resolveForModule modulePath foreigns)
              >>= Oops.hoistEither . first LinkerErrorForeign
          pure . Lua.ForeignSourceCode . decodeUtf8
            <$> readFileBS (toFilePath moduleForeign)

    let fname = qualifyName moduleName [Lua.name|foreign|]

    pure . mconcat $
      [ [Lua.local1 fname (Lua.thunks foreignCode) | not (null foreignCode)]
      , moduleForeigns <&> \name ->
          Lua.local1
            (qualifyName moduleName name)
            (Lua.varField (Lua.varName fname) name)
      , qualifyLocalNames m moduleChunk
      ]

qualifyLocalNames :: Lua.Module -> Lua.Chunk -> Lua.Chunk
qualifyLocalNames Lua.Module {moduleForeigns, moduleName} statements =
  everywhereStat updateStat updateExpr <$> statements
 where
  topLocalNames :: Set Lua.Name =
    Set.fromList moduleForeigns <> Set.fromList do
      toList statements & foldMap \case
        Lua.Local names _es -> toList names
        Lua.Assign vars _es ->
          [n | Lua.VarName (Lua.LocalName n) <- toList vars]
        _ -> []

  qualifyIfTopName name =
    if Set.member name topLocalNames
      then qualifyName moduleName name
      else name

  qualifyVar = \case
    Lua.VarName qname -> Lua.VarName case qname of
      Lua.ImportedName modname name ->
        Lua.LocalName (qualifyName modname name)
      Lua.LocalName name -> Lua.LocalName (qualifyIfTopName name)
    Lua.VarIndex e1 e2 -> Lua.VarIndex e1 e2
    Lua.VarField expr name -> Lua.VarField expr name

  updateStat :: Lua.Statement -> Lua.Statement
  updateStat = \case
    Lua.Assign vars exprs -> Lua.Assign (qualifyVar <$> vars) exprs
    Lua.Local names exprs -> Lua.Local (qualifyIfTopName <$> names) exprs
    stat -> stat

  updateExpr :: Lua.Exp -> Lua.Exp
  updateExpr = \case
    Lua.Var var -> Lua.Var (qualifyVar var)
    Lua.Function args body -> Lua.Function (qualifyIfTopName <$> args) body
    expr -> expr

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------

qualifyName :: Lua.ModuleName -> Lua.Name -> Lua.Name
qualifyName modname = Lua.join2 (Lua.unModuleName modname)

linkedVar :: Lua.ModuleName -> Lua.Name -> Lua.Exp
linkedVar = (Lua.varName .) . qualifyName

topoSorted :: [Lua.Module] -> [Lua.Module]
topoSorted modules =
  reverseTopSort graph <&> (nodeFromVertex >>> \(m, _, _) -> m)
 where
  (graph, nodeFromVertex) =
    graphFromEdges' $
      modules <&> \m@(Lua.Module {..}) -> (m, moduleName, moduleImports)
