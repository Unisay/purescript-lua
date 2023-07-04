module Language.PureScript.CoreFn.Module where

import Language.PureScript.CoreFn.Expr (Bind)
import Language.PureScript.Names (Ident, ModuleName)

{- |
The CoreFn module representation
-}
data Module a = Module
  { moduleName ∷ ModuleName
  , modulePath ∷ FilePath
  , moduleImports ∷ [(a, ModuleName)]
  , moduleExports ∷ [Ident]
  , moduleReExports ∷ Map ModuleName [Ident]
  , moduleForeign ∷ [Ident]
  , moduleBindings ∷ [Bind a]
  }
  deriving stock (Functor, Show)
