module Language.PureScript.Backend.IR.Query where

import Control.Monad.Trans.Accum (Accum, add, execAccum)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Language.PureScript.Backend.IR.Linker (UberModule (..))
import Language.PureScript.Backend.IR.Names
  ( Name (Name)
  , Qualified (Imported, Local)
  , runModuleName
  )
import Language.PureScript.Backend.IR.Types
  ( Exp
  , bindingNames
  , countFreeRef
  , countFreeRefs
  , listGrouping
  , traverseExpBottomUp
  )
import Language.PureScript.Backend.IR.Types qualified as IR
import Language.PureScript.Names (runtimeLazyName)

usesRuntimeLazy ∷ UberModule → Bool
usesRuntimeLazy UberModule {uberModuleBindings, uberModuleExports} =
  getAny $
    foldMap
      (foldMap (\(_qname, e) → Any (findRuntimeLazyInExpr e)) . listGrouping)
      uberModuleBindings
      <> foldMap (Any . findRuntimeLazyInExpr . snd) uberModuleExports

findRuntimeLazyInExpr ∷ Exp → Bool
findRuntimeLazyInExpr expr =
  countFreeRef (Local (Name runtimeLazyName)) expr > 0

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
      IR.Abs _ann (IR.ParamNamed _paramAnn name) _body →
        e <$ add (Set.singleton name)
      IR.Let _ann groupings _body →
        e <$ add do
          Set.fromList
            [iname | grouping ← toList groupings, iname ← bindingNames grouping]
      _ → pure e
