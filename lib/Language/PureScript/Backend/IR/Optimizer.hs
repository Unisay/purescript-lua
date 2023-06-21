module Language.PureScript.Backend.IR.Optimizer where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Language.PureScript.Backend.IR.DCE qualified as DCE
import Language.PureScript.Backend.IR.Linker (UberModule (..))
import Language.PureScript.Backend.IR.Types
  ( Exp (..)
  , ExpF (..)
  , Grouping (..)
  , Info (..)
  , Literal (..)
  , ModuleName (..)
  , Name
  , PrimOp (..)
  , Qualified (..)
  , RewriteMod (..)
  , RewriteRule
  , Rewritten (..)
  , boolean
  , countFreeRefs
  , expInfo
  , isScalar
  , lets
  , listGrouping
  , rewriteExpTopDown
  , subst
  , thenRewrite
  , unExp
  )

optimizeUberModule :: DCE.EntryPoint -> UberModule -> UberModule
optimizeUberModule dceStrategy =
  idempotently $ DCE.eliminateDeadCode dceStrategy . optimizeModule

idempotently :: Eq a => (a -> a) -> a -> a
idempotently = fix $ \i f a ->
  let a' = f a
   in if a' == a
        then a
        else i f a'

optimizeModule :: UberModule -> UberModule
optimizeModule m =
  m {uberModuleBindings = optimizeDecls (uberModuleBindings m)}

optimizeImports :: [ModuleName] -> [Grouping (name, Exp)] -> [ModuleName]
optimizeImports imports bindings =
  toList $ Set.intersection (Set.fromList imports) referencedModules
 where
  referencedModules = foldMap collectImportedModules (groupingsExprs bindings)

groupingsExprs :: [Grouping (name, exp)] -> [exp]
groupingsExprs groupings = [e | g <- groupings, (_name, e) <- listGrouping g]

collectImportedModules :: Exp -> Set ModuleName
collectImportedModules expr =
  Map.keys (refsFree (expInfo expr)) & foldMap \case
    Local _ -> Set.empty
    Imported modname _ -> Set.singleton modname

optimizeDecls :: [Grouping (name, Exp)] -> [Grouping (name, Exp)]
optimizeDecls = (fmap . fmap . fmap) optimizeExpression

optimizeExpression :: Exp -> Exp
optimizeExpression =
  rewriteExpTopDown $
    constantFolding
      `thenRewrite` removeUnreachableThenBranch
      `thenRewrite` removeUnreachableElseBranch
      `thenRewrite` inlineBindings

constantFolding :: RewriteRule
constantFolding e =
  pure case unExp e of
    Prim (Eq (unExp -> Lit a) (unExp -> Lit b))
      | isScalar a ->
          Rewritten Stop $ boolean $ a == b
    _ -> NoChange

removeUnreachableThenBranch :: RewriteRule
removeUnreachableThenBranch e =
  pure case unExp e of
    IfThenElse (unExp -> Lit (Boolean False)) _unreachable elseBranch ->
      Rewritten Recurse elseBranch
    _ -> NoChange

removeUnreachableElseBranch :: RewriteRule
removeUnreachableElseBranch e = pure case unExp e of
  IfThenElse (unExp -> Lit (Boolean True)) thenBranch _unreachable ->
    Rewritten Recurse thenBranch
  _ -> NoChange

-- Inlining is a tricky business:
-- https://www.microsoft.com/en-us/research/wp-content/uploads/2002/07/inline.pdf

inlineBindings :: RewriteRule
inlineBindings = \case
  Exp {unExp = Let groupings body} -> do
    pure . Rewritten Recurse . lets groupings $ foldr inlineBinding body groupings
  _ -> pure NoChange

inlineBinding :: Grouping (Name, Exp) -> Exp -> Exp
inlineBinding grouping body =
  case grouping of
    RecursiveGroup _grp -> body -- TODO: inline recursive bindings
    Standalone (name, inlinee) ->
      if isRef inlinee
        || isNonRecursiveLiteral inlinee
        || countFreeRefs (Local name) body == 1
        then subst body (Local name) inlinee
        else body

isRef :: Exp -> Bool
isRef =
  unExp >>> \case
    Ref {} -> True
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
