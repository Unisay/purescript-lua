module Language.PureScript.Backend.IR.Query where

import Control.Monad.Trans.Accum (Accum, add, execAccum)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Language.PureScript.Backend.IR.Linker (UberModule (..))
import Language.PureScript.Backend.IR.Types
  ( Exp
  , Name (..)
  , Qualified (..)
  , countFreeRef
  , countFreeRefs
  , groupingNames
  , listGrouping
  , traverseExpBottomUp
  )
import Language.PureScript.Backend.IR.Types qualified as IR
import Language.PureScript.Names (runModuleName)

usesRuntimeLazy ∷ UberModule → Bool
usesRuntimeLazy UberModule {uberModuleBindings, uberModuleExports} =
  getAny $
    foldMap
      (foldMap (\(_qname, e) → Any (findRuntimeLazyInExpr e)) . listGrouping)
      uberModuleBindings
      <> foldMap (Any . findRuntimeLazyInExpr . snd) uberModuleExports

findRuntimeLazyInExpr ∷ Exp → Bool
findRuntimeLazyInExpr expr =
  countFreeRef (Local (Name "$__runtime_lazy")) expr > 0

usesPrimModule ∷ UberModule → Bool
usesPrimModule UberModule {uberModuleBindings, uberModuleExports} =
  getAny $
    foldMap
      (foldMap (\(_qname, e) → Any (findPrimModuleInExpr e)) . listGrouping)
      uberModuleBindings
      <> foldMap (Any . findPrimModuleInExpr . snd) uberModuleExports

findPrimModuleInExpr ∷ Exp → Bool
findPrimModuleInExpr expr =
  Map.keys (countFreeRefs expr) & any \case
    Local _name → False
    Imported moduleName _name → runModuleName moduleName == "Prim"

collectBoundNames ∷ Exp → Set Name
collectBoundNames =
  (`execAccum` Set.empty) . traverseExpBottomUp @_ @(Accum (Set Name)) \e →
    case e of
      IR.Abs (IR.unAnn → IR.ParamNamed name) _body →
        e <$ add (Set.singleton name)
      IR.Let groupings _body →
        e <$ add do
          Set.fromList
            [ IR.unAnn iname
            | grouping ← toList groupings
            , iname ← groupingNames grouping
            ]
      _ → pure e
