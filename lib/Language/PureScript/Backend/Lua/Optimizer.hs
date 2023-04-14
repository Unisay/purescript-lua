module Language.PureScript.Backend.Lua.Optimizer where

import Data.List qualified as List
import Language.PureScript.Backend.Lua.Traversal
  ( everywhereExp
  , everywhereStat
  )
import Language.PureScript.Backend.Lua.Types
  ( Chunk
  , Exp
  , ExpF (..)
  , Statement
  , StatementF (Local, Return)
  , functionDef
  , return
  , unAnn
  , pattern Ann
  )
import Prelude hiding (return)

optimizeChunk :: Chunk -> Chunk
optimizeChunk = fmap optimizeStatement

optimizeStatement :: Statement -> Statement
optimizeStatement = everywhereStat identity optimizeExpression

optimizeExpression :: Exp -> Exp
optimizeExpression =
  foldr (>>>) identity rewriteRulesInOrder

rewriteRulesInOrder :: [RewriteRule]
rewriteRulesInOrder =
  [ pushDeclarationsDownTheInnerScope
  , removeScopeWhenInsideEmptyFunction
  -- , collapseNestedFunctions
  ]

type RewriteRule = Exp -> Exp

rewriteExpWithRule :: RewriteRule -> Exp -> Exp
rewriteExpWithRule rule = everywhereExp rule identity

--------------------------------------------------------------------------------
-- Rewrite rules for expressions -----------------------------------------------

pushDeclarationsDownTheInnerScope :: RewriteRule
pushDeclarationsDownTheInnerScope = \case
  Function outerArgs outerBody
    | lastStatement <- List.last outerBody
    , Ann (Return (Ann (Function innerArgs innerBody))) <- lastStatement
    , declarations <- unAnn <$> List.init outerBody
    , not (null declarations)
    , all isDeclaration declarations ->
        functionDef
          outerArgs
          [ return $
              functionDef innerArgs (declarations <> fmap unAnn innerBody)
          ]
  e -> e
 where
  isDeclaration :: Statement -> Bool
  isDeclaration = \case
    Local _ _ -> True
    _ -> False

{- collapseNestedFunctions :: RewriteRule
collapseNestedFunctions = \case
  Function outerArgs [Return (Function innerArgs innerBody)] ->
    Function (outerArgs <> innerArgs) innerBody
  e -> e
 -}
removeScopeWhenInsideEmptyFunction :: RewriteRule
removeScopeWhenInsideEmptyFunction = \case
  Function
    outerArgs
    [Ann (Return (Ann (FunctionCall (Ann (Function [] body)) [])))] ->
      Function outerArgs body
  e -> e
