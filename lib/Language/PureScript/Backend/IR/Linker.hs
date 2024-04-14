{-# LANGUAGE PartialTypeSignatures #-}

module Language.PureScript.Backend.IR.Linker where

import Data.Graph (graphFromEdges', reverseTopSort)
import Data.Map qualified as Map
import Language.PureScript.Backend.IR.Names
  ( ModuleName
  , Name (..)
  , PropName (PropName)
  , QName (QName)
  , Qualified (Imported, Local)
  )
import Language.PureScript.Backend.IR.Types
  ( Ann
  , Binding
  , Exp
  , Grouping (..)
  , Index
  , Module (..)
  , Parameter (ParamNamed, ParamUnused)
  , RawExp (..)
  , bindingNames
  , noAnn
  , objectProp
  , refImported
  )

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
      , ForeignImport noAnn moduleName modulePath moduleForeigns
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
  qualifyBinding ∷ (Ann, Name, Exp) → (QName, Exp)
  qualifyBinding (_ann, name, expr) =
    (QName moduleName name, qualifyTopRefs moduleName topRefs expr)
   where
    topRefs ∷ Map Name Index = Map.fromList do
      (,0) <$> ((moduleBindings >>= bindingNames) <> moduleForeigns)

qualifyTopRefs ∷ ModuleName → Map Name Index → Exp → Exp
qualifyTopRefs moduleName = go
 where
  go ∷ Map Name Index → Exp → Exp
  go topNames expression =
    case expression of
      Ref ann (Local refName) refIndex
        | isTopLevel refName refIndex →
            Ref ann (Imported moduleName refName) refIndex
      Abs ann parameter body →
        Abs ann parameter (go topNames' body)
       where
        topNames' ∷ Map Name Index =
          case parameter of
            ParamNamed _ann argName → Map.adjust (+ 1) argName topNames
            ParamUnused _ann → topNames
      Let ann groupings body →
        Let ann (qualifyGroupings groupings) (qualifyBody body)
       where
        qualifyGroupings ∷ NonEmpty Binding → NonEmpty Binding
        qualifyGroupings = fmap \case
          Standalone (a, name, expr) →
            Standalone (a, name, go (Map.adjust (+ 1) name topNames) expr)
          RecursiveGroup recBinds →
            RecursiveGroup do
              (a, name, expr) ← recBinds
              pure (a, name, go indexedNames expr)
           where
            boundNames = toList recBinds <&> \(_, n, _) → n
            indexedNames = foldr (Map.adjust (+ 1)) topNames boundNames
        qualifyBody =
          let boundNames = toList groupings >>= bindingNames
           in go (foldr (Map.adjust (+ 1)) topNames boundNames)
      App ann argument function →
        App ann (go' argument) (go' function)
      LiteralArray ann as →
        LiteralArray ann (go' <$> as)
      LiteralObject ann props →
        LiteralObject ann (go' <<$>> props)
      ReflectCtor ann a →
        ReflectCtor ann (go' a)
      DataArgumentByIndex ann idx a →
        DataArgumentByIndex ann idx (go' a)
      Eq ann a b →
        Eq ann (go' a) (go' b)
      ArrayLength ann a →
        ArrayLength ann (go' a)
      ArrayIndex ann a indx →
        ArrayIndex ann (go' a) indx
      ObjectProp ann a prop →
        ObjectProp ann (go' a) prop
      ObjectUpdate ann a patches →
        ObjectUpdate ann (go' a) (go' <<$>> patches)
      IfThenElse ann p th el →
        IfThenElse ann (go' p) (go' th) (go' el)
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
      modules <&> \m@Module {..} →
        (m, moduleName, moduleImports)
