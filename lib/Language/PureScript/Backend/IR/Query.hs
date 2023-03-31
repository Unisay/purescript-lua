module Language.PureScript.Backend.IR.Query where

import Data.Set qualified as Set
import Language.PureScript.Backend.IR.Types
  ( Exp (..)
  , Info (..)
  , Module (..)
  , ModuleName (..)
  , Name (..)
  , Qualified (Local)
  , bindingExprs
  , qualifiedByModule
  )

usesRuntimeLazy :: Module -> Bool
usesRuntimeLazy Module {moduleBindings = bs} =
  getAny $ foldMap (foldMap (Any . findRuntimeLazyInExpr) . bindingExprs) bs
 where
  findRuntimeLazyInExpr Exp {expInfo = Info {refsFree}} =
    Local (Name "$__runtime_lazy") `Set.member` refsFree

usesPrimModule :: Module -> Bool
usesPrimModule Module {moduleBindings = bs} =
  getAny $ foldMap (foldMap (Any . findPrimModuleInExpr) . bindingExprs) bs
 where
  findPrimModuleInExpr Exp {expInfo = Info {refsFree}} =
    any (qualifiedByModule >>> (== Just (ModuleName "Prim"))) refsFree
