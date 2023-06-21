{-# LANGUAGE LambdaCase #-}

module Language.PureScript.Backend.IR.Linker where

import Data.Graph (graphFromEdges', reverseTopSort)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Language.PureScript.Backend.IR.Types

--------------------------------------------------------------------------------
-- Data ------------------------------------------------------------------------

data LinkMode
  = LinkAsApplication ModuleName Name
  | LinkAsModule ModuleName
  deriving stock (Show)

data UberModule = UberModule
  { uberModuleBindings :: [Grouping (QName, Exp)]
  , uberModuleForeigns :: [(ModuleName, FilePath, NonEmpty Name)]
  , uberModuleExports :: [(ModuleName, Name)]
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Functions -------------------------------------------------------------------

makeUberModule :: LinkMode -> [Module] -> UberModule
makeUberModule linkMode modules =
  UberModule
    { uberModuleBindings
    , uberModuleForeigns
    , uberModuleExports
    }
 where
  sortedModules = topoSorted modules
  uberModuleBindings = concatMap qualifiedModuleBindings sortedModules
  uberModuleForeigns = concatMap qualifiedModuleForeigns sortedModules
  uberModuleExports =
    case linkMode of
      LinkAsApplication moduleName name ->
        [(moduleName, name)]
      LinkAsModule modname ->
        [ (moduleName, exportedName)
        | Module {moduleName, moduleExports} <- modules
        , moduleName == modname
        , exportedName <- moduleExports
        ]

qualifiedModuleBindings :: Module -> [Grouping (QName, Exp)]
qualifiedModuleBindings Module {moduleName, moduleBindings, moduleForeigns} =
  moduleBindings <&> \case
    Standalone binding -> Standalone $ qualifyBinding binding
    RecursiveGroup bindings -> RecursiveGroup $ qualifyBinding <$> bindings
 where
  qualifyBinding :: (Name, Exp) -> (QName, Exp)
  qualifyBinding = bimap (QName moduleName) (qualifyTopRefs moduleName topRefs)

  topRefs :: Map Name Index
  topRefs =
    Map.fromList $
      (,0) <$> ((moduleBindings >>= bindingNames) <> moduleForeigns)

qualifiedModuleForeigns :: Module -> [(ModuleName, FilePath, NonEmpty Name)]
qualifiedModuleForeigns Module {moduleName, modulePath, moduleForeigns} =
  case NE.nonEmpty moduleForeigns of
    Nothing -> []
    Just foreignNames -> [(moduleName, modulePath, foreignNames)]

qualifyTopRefs :: ModuleName -> Map Name Index -> Exp -> Exp
qualifyTopRefs moduleName = go
 where
  go topNames expression =
    case unExp expression of
      Ref (Local refName) refIndex
        | isTopLevel refName refIndex ->
            ref (Imported moduleName refName) refIndex
      Abs argument body ->
        abstraction argument $ go topNames' body
       where
        topNames' =
          case argument of
            ParamNamed argName -> Map.adjust (+ 1) argName topNames
            _ -> topNames
      Let groupings body ->
        lets (qualifyGroupings groupings) (qualifyBody body)
       where
        qualifyGroupings = fmap \case
          Standalone (name, expr) ->
            Standalone (name, go (Map.adjust (+ 1) name topNames) expr)
          RecursiveGroup recBinds ->
            RecursiveGroup $ go topNames' <<$>> recBinds
           where
            topNames' = foldr (Map.adjust (+ 1)) topNames (fst <$> recBinds)
        qualifyBody = go topNames'
         where
          topNames' = foldr (Map.adjust (+ 1)) topNames boundNames
          boundNames = toList groupings >>= bindingNames
      App function argument -> application (go' function) (go' argument)
      Lit (Array as) -> array (go' <$> as)
      Lit (Object props) -> object (second go' <$> props)
      Prim op ->
        case op of
          ArrayLength a -> arrayLength (go' a)
          ReflectCtor a -> reflectCtor (go' a)
          DataArgumentByIndex idx a -> dataArgumentByIndex idx (go' a)
          Eq a b -> eq (go' a) (go' b)
      ArrayIndex a idx -> arrayIndex (go' a) idx
      ObjectProp a prp -> objectProp (go' a) prp
      ObjectUpdate a patches -> objectUpdate (go' a) (second go' <$> patches)
      IfThenElse p th el -> ifThenElse (go' p) (go' th) (go' el)
      _ -> expression
   where
    isTopLevel name i = Map.lookup name topNames == Just i
    go' = go topNames

--------------------------------------------------------------------------------
-- Utils -----------------------------------------------------------------------

topoSorted :: [Module] -> [Module]
topoSorted modules =
  reverseTopSort graph <&> (nodeFromVertex >>> \(m, _, _) -> m)
 where
  (graph, nodeFromVertex) =
    graphFromEdges' $
      modules <&> \m@(Module {..}) -> (m, moduleName, moduleImports)
