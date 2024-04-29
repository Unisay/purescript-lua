module Language.PureScript.Backend.Lua.Optimizer where

import Control.Monad.Trans.Accum (Accum, add, execAccum)
import Data.List qualified as List
import Data.Map qualified as Map
import Language.PureScript.Backend.Lua.Name qualified as Lua
import Language.PureScript.Backend.Lua.Traversal
  ( everywhereExp
  , everywhereInChunkM
  , everywhereStat
  , everywhereStatM
  )
import Language.PureScript.Backend.Lua.Types
  ( Chunk
  , Exp
  , ExpF (..)
  , Statement
  , StatementF (..)
  , TableRowF (..)
  , VarF (..)
  , functionDef
  , return
  )

import Language.PureScript.Backend.Lua.Types qualified as Lua
import Prelude hiding (return)

optimizeChunk ∷ Chunk → Chunk
optimizeChunk = fmap optimizeStatement

substituteVarForValue ∷ Lua.Name → Exp → Chunk → Chunk
substituteVarForValue name inlinee =
  runIdentity . everywhereInChunkM (pure . subst) pure
 where
  subst = \case
    Lua.Var _ (Lua.VarName _ varName) | varName == name → inlinee
    expr → expr

countRefs ∷ Statement → Map Lua.Name (Sum Natural)
countRefs = everywhereStatM pure countRefsInExpression >>> (`execAccum` mempty)
 where
  countRefsInExpression ∷ Exp → Accum (Map Lua.Name (Sum Natural)) Exp
  countRefsInExpression = \case
    expr@(Lua.Var _ (Lua.VarName _ name)) →
      add (Map.singleton name (Sum 1)) $> expr
    expr → pure expr

optimizeStatement ∷ Statement → Statement
optimizeStatement = everywhereStat identity optimizeExpression

optimizeExpression ∷ Exp → Exp
optimizeExpression = foldr (>>>) identity rewriteRulesInOrder

rewriteRulesInOrder ∷ [RewriteRule]
rewriteRulesInOrder =
  [ pushDeclarationsDownTheInnerScope
  , removeScopeWhenInsideEmptyFunction
  , reduceTableDefinitionAccessor
  ]

type RewriteRule = Exp → Exp

rewriteExpWithRule ∷ RewriteRule → Exp → Exp
rewriteExpWithRule rule = everywhereExp rule identity

{-
  Local
    name
    ( Just

            (Function args [ Return ( Function innerArgs innerBody)])

      ) →
      let args' = fmap unAnn (args <> innerArgs)
          val = functionDef args' (fmap unAnn innerBody)
       in DList.snoc acc $ Lua.local1 name val
  Assign
    name

        (Function args [ Return ( Function innerArgs innerBody)])

      | length args + length innerArgs <= minApplications name →
          let args' = fmap unAnn (args <> innerArgs)
              val = functionDef args' (fmap unAnn innerBody)
           in DList.snoc acc (Lua.assign (unAnn name) val)
 -}

--------------------------------------------------------------------------------
-- Rewrite rules for expressions -----------------------------------------------

pushDeclarationsDownTheInnerScope ∷ RewriteRule
pushDeclarationsDownTheInnerScope = \case
  Function _ outerArgs outerBody
    | Just lastStatement ← viaNonEmpty last outerBody
    , Return _ (Function _ innerArgs innerBody) ← lastStatement
    , declarations ← List.init outerBody
    , not (null declarations)
    , all isDeclaration declarations →
        functionDef
          outerArgs
          [return $ functionDef innerArgs (declarations <> innerBody)]
  e → e
 where
  isDeclaration ∷ Statement → Bool
  isDeclaration = \case
    Local {} → True
    Assign {} → True
    _ → False

removeScopeWhenInsideEmptyFunction ∷ RewriteRule
removeScopeWhenInsideEmptyFunction = \case
  Function
    _
    outerArgs
    [Return _ (FunctionCall _ (Function _ [] body) [])] →
      functionDef outerArgs body
  e → e

-- | Rewrites '{ foo = 1, bar = 2 }.foo' to '1'
reduceTableDefinitionAccessor ∷ RewriteRule
reduceTableDefinitionAccessor = \case
  Var _ (VarField _ (TableCtor _ rows) accessedField) →
    fromMaybe Lua.nil $
      listToMaybe
        [ fieldValue
        | TableRowNV _ tableField fieldValue ← rows
        , tableField == accessedField
        ]
  e → e
