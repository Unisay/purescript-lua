module Language.PureScript.Backend.IR.Optimizer where

import Data.Set qualified as Set
import Language.PureScript.Backend.IR.DCE qualified as DCE
import Language.PureScript.Backend.IR.Types
  ( Exp (..)
  , ExpF (..)
  , Grouping (..)
  , Info (..)
  , Literal (Boolean)
  , Module (moduleBindings, moduleImports)
  , ModuleName (..)
  , Qualified (Imported)
  , everywhereTopDownExp
  , listGrouping
  )

optimizeAll :: DCE.Strategy -> [Module] -> [Module]
optimizeAll dceStrategy =
  DCE.eliminateDeadCode dceStrategy . fmap optimizeModule

optimizeModule :: Module -> Module
optimizeModule m =
  m
    { moduleImports = moduleImports'
    , moduleBindings = moduleBindings'
    }
 where
  moduleBindings' = optimizeDecls (moduleBindings m)
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
  Set.fromList $
    refsFree >>= \case
      Imported modname _ -> [modname]
      _ -> []

optimizeDecls :: [Grouping (name, Exp)] -> [Grouping (name, Exp)]
optimizeDecls = (fmap . fmap . fmap) optimizeExpression

type RewriteRule = Exp -> Exp

optimizeExpression :: Exp -> Exp
optimizeExpression =
  everywhereTopDownExp $
    removeUnreachableElseBranch . removeUnreachableThenBranch

removeUnreachableThenBranch :: RewriteRule
removeUnreachableThenBranch e = case unExp e of
  IfThenElse (Exp (Lit (Boolean False)) _info) _unreachable elseBranch ->
    elseBranch
  _ -> e

removeUnreachableElseBranch :: RewriteRule
removeUnreachableElseBranch e = case unExp e of
  IfThenElse (Exp (Lit (Boolean True)) _info) thenBranch _unreachable ->
    thenBranch
  _ -> e

{-

{- -- Inlining is a tricky business:
-- https://www.microsoft.com/en-us/research/wp-content/uploads/2002/07/inline.pdf
inlineBindings :: RewriteRule
inlineBindings e =
  case e of
    Let binds expr ->
      case allNonRecBinds (toList binds) of
        [] -> Fix e
        (name, inlinee) : bs ->
          if isRef inlinee
            then lets binds (safeReplace (ref name) inlinee expr)
            else
              let expr' = markOccurrences name expr
               in if countMarkedRefs expr' name == 1
                    then safeReplaceMarked (ref name) inlinee expr'
                    else Fix e
    e -> Fix e
 where
  allNonRecBinds :: [Bind Exp] -> [(Name, Exp)]
  allNonRecBinds = (=<<) \case
    BindNonRec n e -> [(n, e)]
    BindRec _bs -> []

  markOccurrences :: Name -> Exp -> (Bool, Exp)
  markOccurrences name expr = undefined

  countMarkedRefs :: (Bool, Exp) -> Name -> Natural
  countMarkedRefs (marked, e) name =
    e & foldFix \case
      Ref (Local name') | name' == name && marked -> 1
      Lit literal ->
        case literal of
          Array as -> sum as
          Object ps -> sum (snd <$> ps)
          _ -> 0
      Prim op ->
        case op of
          ArrayLength a -> a
          ReflectCtor a -> a
          DataArgumentByIndex _idx a -> a
          Eq a1 a2 -> a1 + a2
      ArrayIndex a _idx -> a
      ObjectProp a _propName -> a
      ObjectUpdate a ps -> a + sum (snd <$> ps)
      Abs _name _a -> 0
      App a1 a2 -> a1 + a2
      Let binds a -> a + sum (fmap snd . bindPairs =<< binds)
      IfThenElse c t e -> c + t + e
      _ -> 0

  safeReplace :: Exp -> Exp -> Exp -> Exp
  safeReplace target replacement expr = undefined

  safeReplaceMarked :: Exp -> Exp -> (Bool, Exp) -> Exp
  safeReplaceMarked _target _replacement (False, expr) = expr
  safeReplaceMarked target replacement (True, expr) = undefined

  isRef = \case
    Fix (Ref _) -> True
    _ -> False
 -}
eliminateUnusedBindings :: ExpF Exp -> Exp
eliminateUnusedBindings = \case
  App (Fix (Abs name expr)) _ | not (expr `refersTo` name) -> expr
  e -> Fix e
 where
  refersTo :: Exp -> Name -> Bool
  refersTo expr name =
    expr & foldFix \case
      Ref (Local name') | name' == name -> True
      Lit literal ->
        case literal of
          Array as -> or as
          Object ps -> or (snd <$> ps)
          _ -> False
      Prim op ->
        case op of
          ArrayLength a -> a
          ReflectCtor a -> a
          DataArgumentByIndex _idx a -> a
          Eq a1 a2 -> a1 || a2
      ArrayIndex a _idx -> a
      ObjectProp a _propName -> a
      ObjectUpdate a ps -> a || or (snd <$> ps)
      Abs name' a -> name' /= name && a
      App a1 a2 -> a1 || a2
      Let binds a -> a || or (fmap snd . bindPairs =<< binds)
      IfThenElse c t e -> c || t || e
      _ -> False

 -}
