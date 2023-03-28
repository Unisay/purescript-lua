{-# LANGUAGE QuasiQuotes #-}

module Language.PureScript.Backend.Lua.OptimizerSpec where

import Language.PureScript.Backend.Lua.Name (name)
import Language.PureScript.Backend.Lua.Optimizer
  ( pushDeclarationsDownTheInnerScope
  , removeScopeWhenInsideEmptyFunction
  , rewriteExpWithRule
  )
import Language.PureScript.Backend.Lua.Types
import Shower (shower)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (assertEqual)

spec :: Spec
spec = describe "Lua AST Optimizer" do
  describe "optimizes expressions" do
    {-     it "collapses nested functions" do
          let original =
                Function
                  [[name|a|], [name|b|]]
                  [ Return
                      ( Function
                          [[name|c|], [name|d|]]
                          [Return (varName [name|d|])]
                      )
                  ]
              expected =
                Function
                  [[name|a|], [name|b|], [name|c|], [name|d|]]
                  [Return (varName [name|d|])]
          assertEqual (shower original) expected $
            optimizeExpression original
          assertEqual
            (shower original)
            (optimizeExpression original)
            (rewriteExpWithRule collapseNestedFunctions original) -}

    it "removes scope when inside an empty Function" do
      let original =
            Function
              [[name|a|]]
              [ Return
                  ( Function
                      [[name|b|]]
                      [Return (scope [Return (varName [name|c|])])]
                  )
              ]
          expected =
            Function
              [[name|a|]]
              [ Return
                  ( Function
                      [[name|b|]]
                      [Return (varName [name|c|])]
                  )
              ]
      assertEqual (shower original) expected $
        rewriteExpWithRule removeScopeWhenInsideEmptyFunction original

    it "pushes declarations down into an inner scope" do
      let original =
            Function
              [[name|a|], [name|b|]]
              [ local1 [name|i|] (Integer 42)
              , local1 [name|j|] (Integer 43)
              , Return
                  ( Function
                      [[name|d|]]
                      [Return (varName [name|c|])]
                  )
              ]
          expected =
            Function
              [[name|a|], [name|b|]]
              [ Return
                  ( Function
                      [[name|d|]]
                      [ local1 [name|i|] (Integer 42)
                      , local1 [name|j|] (Integer 43)
                      , Return (varName [name|c|])
                      ]
                  )
              ]
      assertEqual (shower original) expected $
        rewriteExpWithRule pushDeclarationsDownTheInnerScope original
