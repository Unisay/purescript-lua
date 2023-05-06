module Language.PureScript.Backend.IR.Optimizer where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Language.PureScript.Backend.IR.DCE qualified as DCE
import Language.PureScript.Backend.IR.Types
  ( Exp (..)
  , ExpF (..)
  , FreshNames
  , Grouping (..)
  , Info (..)
  , LetBinding (..)
  , Literal (..)
  , LocallyNameless (..)
  , Module (moduleBindings, moduleImports)
  , ModuleName (..)
  , Name
  , bindingNames
  , countBoundRefs
  , everywhereTopDownExpM
  , findOffset
  , listGrouping
  , qualified
  , subst
  , wrapExpF
  )

optimizeAll :: FreshNames m => DCE.Strategy -> [Module] -> m [Module]
optimizeAll dceStrategy modules =
  DCE.eliminateDeadCode dceStrategy <$> traverse optimizeModule modules

optimizeModule :: FreshNames m => Module -> m Module
optimizeModule m = do
  moduleBindings <- optimizeDecls (moduleBindings m)
  pure m {moduleImports = moduleImports', moduleBindings}
 where
  moduleImports' = moduleImports m

optimizeImports :: [ModuleName] -> [Grouping (name, Exp)] -> [ModuleName]
optimizeImports imports bindings =
  toList $ Set.intersection (Set.fromList imports) referencedModules
 where
  referencedModules = foldMap collectImportedModules (groupingsExprs bindings)

groupingsExprs :: [Grouping (name, exp)] -> [exp]
groupingsExprs groupings = [e | g <- groupings, (_name, e) <- listGrouping g]

collectImportedModules :: Exp -> Set ModuleName
collectImportedModules Exp {expInfo = Info {refsFree}} =
  Map.keys refsFree
    & foldMap (qualified mempty \modname _ -> Set.singleton modname)

optimizeDecls
  :: FreshNames m => [Grouping (name, Exp)] -> m [Grouping (name, Exp)]
optimizeDecls = (traverse . traverse . traverse) optimizeExpression

type RewriteRule m = Exp -> m Exp

optimizeExpression :: FreshNames m => Exp -> m Exp
optimizeExpression =
  everywhereTopDownExpM $
    inlineBindings
      <=< removeUnreachableElseBranch
      <=< removeUnreachableThenBranch

removeUnreachableThenBranch :: Applicative m => RewriteRule m
removeUnreachableThenBranch e = pure case unExp e of
  IfThenElse (Exp (Lit (Boolean False)) _info) _unreachable elseBranch ->
    elseBranch
  _ -> e

removeUnreachableElseBranch :: Applicative m => RewriteRule m
removeUnreachableElseBranch e = pure case unExp e of
  IfThenElse (Exp (Lit (Boolean True)) _info) thenBranch _unreachable ->
    thenBranch
  _ -> e

-- Inlining is a tricky business:
-- https://www.microsoft.com/en-us/research/wp-content/uploads/2002/07/inline.pdf

inlineBindings :: FreshNames m => RewriteRule m
inlineBindings e =
  case unExp e of
    Let (LetBinding groupings body) -> do
      let names = toList groupings >>= bindingNames
      pure . wrapExpF . Let . LetBinding groupings $
        foldr (inlineBinding names) body groupings
    _ -> pure e

inlineBinding
  :: [Name]
  -> Grouping (Name, LocallyNameless Exp)
  -> LocallyNameless Exp
  -> LocallyNameless Exp
inlineBinding names grouping body =
  case grouping of
    Standalone (name, replacement@(LocallyNameless inlinee))
      | Just offset <- names `findOffset` name
      , isRef inlinee
          || isNonRecursiveLiteral inlinee
          || countBoundRefs body offset == 1 ->
          subst body offset replacement
    _ -> body
 where
  isRef :: Exp -> Bool
  isRef =
    unExp >>> \case
      RefFree {} -> True
      RefBound {} -> True
      _ -> False

  isNonRecursiveLiteral :: Exp -> Bool
  isNonRecursiveLiteral =
    unExp >>> \case
      Lit Integer {} -> True
      Lit Floating {} -> True
      Lit String {} -> True
      Lit Char {} -> True
      Lit Boolean {} -> True
      _ -> False
