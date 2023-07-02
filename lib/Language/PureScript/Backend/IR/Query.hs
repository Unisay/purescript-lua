module Language.PureScript.Backend.IR.Query where

import Data.Map qualified as Map
import Language.PureScript.Backend.IR.Linker (UberModule (..))
import Language.PureScript.Backend.IR.Types
  ( Exp
  , Name (..)
  , Qualified (..)
  , countFreeRef
  , countFreeRefs
  , listGrouping
  )
import Language.PureScript.Names (runModuleName)

usesRuntimeLazy :: UberModule -> Bool
usesRuntimeLazy UberModule {uberModuleBindings, uberModuleExports} =
  getAny $
    foldMap
      (foldMap (\(_qname, e) -> Any (findRuntimeLazyInExpr e)) . listGrouping)
      uberModuleBindings
      <> foldMap (Any . findRuntimeLazyInExpr . snd) uberModuleExports

findRuntimeLazyInExpr :: Exp -> Bool
findRuntimeLazyInExpr expr =
  countFreeRef (Local (Name "$__runtime_lazy")) expr > 0

usesPrimModule :: UberModule -> Bool
usesPrimModule UberModule {uberModuleBindings, uberModuleExports} =
  getAny $
    foldMap
      (foldMap (\(_qname, e) -> Any (findPrimModuleInExpr e)) . listGrouping)
      uberModuleBindings
      <> foldMap (Any . findPrimModuleInExpr . snd) uberModuleExports

findPrimModuleInExpr :: Exp -> Bool
findPrimModuleInExpr expr =
  Map.keys (countFreeRefs expr) & any \case
    Local _name -> False
    Imported moduleName _name -> runModuleName moduleName == "Prim"
