module Language.PureScript.Backend.IR.Query where

import Data.Map qualified as Map
import Language.PureScript.Backend.IR.Linker (UberModule (..))
import Language.PureScript.Backend.IR.Types
  ( Exp
  , Info (..)
  , Module (..)
  , ModuleName (..)
  , Name (..)
  , Qualified (..)
  , bindingExprs
  , expInfo
  , listGrouping
  )

usesRuntimeLazy :: Module -> Bool
usesRuntimeLazy Module {moduleBindings = bs} =
  getAny $ foldMap (foldMap (Any . findRuntimeLazyInExpr) . bindingExprs) bs

usesRuntimeLazyUber :: UberModule -> Bool
usesRuntimeLazyUber UberModule {uberModuleBindings = bs} =
  getAny $
    foldMap
      (foldMap (\(_qname, e) -> Any (findRuntimeLazyInExpr e)) . listGrouping)
      bs

findRuntimeLazyInExpr :: Exp -> Bool
findRuntimeLazyInExpr expr =
  Local (Name "$__runtime_lazy") `Map.member` refsFree (expInfo expr)

usesPrimModule :: Module -> Bool
usesPrimModule Module {moduleBindings = bs} =
  getAny $ foldMap (foldMap (Any . findPrimModuleInExpr) . bindingExprs) bs

usesPrimModuleUber :: UberModule -> Bool
usesPrimModuleUber UberModule {uberModuleBindings = bs} =
  getAny $
    foldMap
      (foldMap (\(_qname, e) -> Any (findPrimModuleInExpr e)) . listGrouping)
      bs

findPrimModuleInExpr :: Exp -> Bool
findPrimModuleInExpr expr =
  Map.keys (refsFree (expInfo expr)) & any \case
    Local _name -> False
    Imported (ModuleName moduleName) _name -> moduleName == "Prim"
