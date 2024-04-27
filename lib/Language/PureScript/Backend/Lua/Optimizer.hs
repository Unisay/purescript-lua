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
  , unAnn
  , pattern Ann
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
    Lua.Var (unAnn → Lua.VarName varName) | varName == name → inlinee
    expr → expr

countRefs ∷ Statement → Map Lua.Name (Sum Natural)
countRefs = everywhereStatM pure countRefsInExpression >>> (`execAccum` mempty)
 where
  countRefsInExpression ∷ Exp → Accum (Map Lua.Name (Sum Natural)) Exp
  countRefsInExpression = \case
    expr@(Lua.Var (unAnn → Lua.VarName name)) →
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
        ( Ann
            (Function args [Ann (Return (Ann (Function innerArgs innerBody)))])
          )
      ) →
      let args' = fmap unAnn (args <> innerArgs)
          val = functionDef args' (fmap unAnn innerBody)
       in DList.snoc acc $ Lua.local1 name val
  Assign
    name
    ( Ann
        (Function args [Ann (Return (Ann (Function innerArgs innerBody)))])
      )
      | length args + length innerArgs <= minApplications name →
          let args' = fmap unAnn (args <> innerArgs)
              val = functionDef args' (fmap unAnn innerBody)
           in DList.snoc acc (Lua.assign (unAnn name) val)
 -}

--------------------------------------------------------------------------------
-- Rewrite rules for expressions -----------------------------------------------

pushDeclarationsDownTheInnerScope ∷ RewriteRule
pushDeclarationsDownTheInnerScope = \case
  Function outerArgs outerBody
    | Just lastStatement ← viaNonEmpty last outerBody
    , Ann (Return (Ann (Function innerArgs innerBody))) ← lastStatement
    , declarations ← unAnn <$> List.init outerBody
    , not (null declarations)
    , all isDeclaration declarations →
        functionDef
          (fmap unAnn outerArgs)
          [ return $
              functionDef
                (fmap unAnn innerArgs)
                (declarations <> fmap unAnn innerBody)
          ]
  e → e
 where
  isDeclaration ∷ Statement → Bool
  isDeclaration = \case
    Local _ _ → True
    _ → False

removeScopeWhenInsideEmptyFunction ∷ RewriteRule
removeScopeWhenInsideEmptyFunction = \case
  Function
    outerArgs
    [Ann (Return (Ann (FunctionCall (Ann (Function [] body)) [])))] →
      Function outerArgs body
  e → e

-- | Rewrites '{ foo = 1, bar = 2 }.foo' to '1'
reduceTableDefinitionAccessor ∷ RewriteRule
reduceTableDefinitionAccessor = \case
  Var (Ann (VarField (Ann (TableCtor rows)) accessedField)) →
    fromMaybe Nil $
      listToMaybe
        [ fieldValue
        | (_ann, TableRowNV tableField (Ann fieldValue)) ← rows
        , tableField == accessedField
        ]
  e → e
