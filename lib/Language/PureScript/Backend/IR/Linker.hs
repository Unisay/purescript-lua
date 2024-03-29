module Language.PureScript.Backend.IR.Linker where

import Data.Graph (graphFromEdges', reverseTopSort)
import Data.Map qualified as Map
import Language.PureScript.Backend.IR.Types
  ( Exp
  , Grouping (..)
  , Index
  , Module (..)
  , Name (..)
  , Parameter (ParamNamed)
  , PropName (..)
  , QName (QName)
  , Qualified (Imported, Local)
  , RawExp (..)
  , groupingNames
  , objectProp
  , ref
  , refImported
  , unAnn
  )
import Language.PureScript.Names (ModuleName)

--------------------------------------------------------------------------------
-- Data ------------------------------------------------------------------------

data LinkMode
  = LinkAsApplication ModuleName Name
  | LinkAsModule ModuleName
  deriving stock (Show)

data UberModule = UberModule
  { uberModuleBindings ∷ [Grouping (QName, Exp)]
  , uberModuleExports ∷ [(Name, Exp)]
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Functions -------------------------------------------------------------------

makeUberModule ∷ LinkMode → [Module] → UberModule
makeUberModule linkMode modules =
  UberModule {uberModuleBindings, uberModuleExports}
 where
  sortedModules = topoSorted modules

  uberModuleBindings =
    concatMap foreignBindings sortedModules
      <> concatMap qualifiedModuleBindings sortedModules

  uberModuleExports ∷ [(Name, Exp)] =
    case linkMode of
      LinkAsApplication moduleName name →
        [(name, refImported moduleName name 0)]
      LinkAsModule modname →
        [ (exportedName, refImported moduleName exportedName 0)
        | Module {moduleName, moduleExports} ← modules
        , moduleName == modname
        , exportedName ← moduleExports
        ]

foreignBindings ∷ Module → [Grouping (QName, Exp)]
foreignBindings Module {moduleName, modulePath, moduleForeigns} =
  foreignModuleBinding <> foreignNamesBindings
 where
  foreignName = Name "foreign"
  foreignModuleRef = refImported moduleName foreignName 0

  foreignModuleBinding ∷ [Grouping (QName, Exp)]
  foreignModuleBinding =
    [ Standalone
      ( QName moduleName foreignName
      , ForeignImport moduleName modulePath moduleForeigns
      )
    | not (null moduleForeigns)
    ]

  foreignNamesBindings ∷ [Grouping (QName, Exp)] =
    moduleForeigns <&> \name →
      Standalone
        ( QName moduleName name
        , objectProp foreignModuleRef (PropName (nameToText name))
        )

qualifiedModuleBindings ∷ Module → [Grouping (QName, Exp)]
qualifiedModuleBindings Module {moduleName, moduleBindings, moduleForeigns} =
  moduleBindings <&> \case
    Standalone binding → Standalone $ qualifyBinding binding
    RecursiveGroup bindings → RecursiveGroup $ qualifyBinding <$> bindings
 where
  qualifyBinding ∷ (Name, Exp) → (QName, Exp)
  qualifyBinding = bimap (QName moduleName) (qualifyTopRefs moduleName topRefs)
   where
    topRefs ∷ Map Name Index = Map.fromList do
      (,0) <$> ((moduleBindings >>= groupingNames) <> moduleForeigns)

qualifyTopRefs ∷ ModuleName → Map Name Index → Exp → Exp
qualifyTopRefs moduleName = go
 where
  go ∷ Map Name Index → Exp → Exp
  go topNames expression =
    case expression of
      Ref (Local refName) refIndex
        | isTopLevel refName refIndex →
            ref (Imported moduleName refName) refIndex
      Abs argument body →
        Abs argument (go topNames' <$> body)
       where
        topNames' =
          case unAnn argument of
            ParamNamed argName → Map.adjust (+ 1) argName topNames
            _ → topNames
      Let groupings body →
        Let (qualifyGroupings groupings) (qualifyBody <$> body)
       where
        qualifyGroupings = fmap \case
          Standalone (name, expr) →
            Standalone
              ( name
              , go (Map.adjust (+ 1) (unAnn name) topNames) <$> expr
              )
          RecursiveGroup recBinds →
            RecursiveGroup ((go topNames' <$>) <<$>> recBinds)
           where
            topNames' = foldr (Map.adjust (+ 1) . unAnn . fst) topNames recBinds
        qualifyBody = go topNames'
         where
          topNames' = foldr (Map.adjust (+ 1) . unAnn) topNames boundNames
          boundNames = toList groupings >>= groupingNames
      App argument function → App (go' <$> argument) (go' <$> function)
      LiteralArray as → LiteralArray (go' <<$>> as)
      LiteralObject props → LiteralObject (fmap go' <<$>> props)
      ReflectCtor a → ReflectCtor (go' <$> a)
      DataArgumentByIndex idx a → DataArgumentByIndex idx (go' <$> a)
      Eq a b → Eq (go' <$> a) (go' <$> b)
      ArrayLength a → ArrayLength (go' <$> a)
      ArrayIndex a indx → ArrayIndex (go' <$> a) indx
      ObjectProp a prop → ObjectProp (go' <$> a) prop
      ObjectUpdate a patches → ObjectUpdate (go' <$> a) (fmap go' <<$>> patches)
      IfThenElse p th el → IfThenElse (go' <$> p) (go' <$> th) (go' <$> el)
      _ → expression
   where
    isTopLevel name i = Map.lookup name topNames == Just i
    go' = go topNames

--------------------------------------------------------------------------------
-- Utils -----------------------------------------------------------------------

topoSorted ∷ [Module] → [Module]
topoSorted modules =
  reverseTopSort graph <&> (nodeFromVertex >>> \(m, _, _) → m)
 where
  (graph, nodeFromVertex) =
    graphFromEdges' $
      modules <&> \m@(Module {..}) → (m, moduleName, moduleImports)
