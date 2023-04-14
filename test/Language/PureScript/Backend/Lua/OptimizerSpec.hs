{-# LANGUAGE QuasiQuotes #-}

module Language.PureScript.Backend.Lua.OptimizerSpec where

import Language.PureScript.Backend.Lua.Name (name)
import Language.PureScript.Backend.Lua.Optimizer
  ( pushDeclarationsDownTheInnerScope
  , removeScopeWhenInsideEmptyFunction
  , rewriteExpWithRule
  )
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Shower (shower)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (assertEqual)

spec :: Spec
spec = describe "Lua AST Optimizer" do
  describe "optimizes expressions" do
    it "removes scope when inside an empty function" do
      let original :: Lua.Exp =
            Lua.functionDef
              [[name|a|]]
              [ Lua.return
                  ( Lua.functionDef
                      [[name|b|]]
                      [Lua.return (Lua.scope [Lua.return (Lua.varName [name|c|])])]
                  )
              ]
          expected :: Lua.Exp =
            Lua.functionDef
              [[name|a|]]
              [ Lua.return
                  ( Lua.functionDef
                      [[name|b|]]
                      [Lua.return (Lua.varName [name|c|])]
                  )
              ]
      assertEqual (shower original) expected $
        rewriteExpWithRule removeScopeWhenInsideEmptyFunction original

    it "pushes declarations down into an inner scope" do
      let original :: Lua.Exp =
            Lua.functionDef
              [[name|a|], [name|b|]]
              [ Lua.local1 [name|i|] (Lua.Integer 42)
              , Lua.local1 [name|j|] (Lua.Integer 43)
              , Lua.return
                  ( Lua.functionDef
                      [[name|d|]]
                      [Lua.return (Lua.varName [name|c|])]
                  )
              ]
          expected :: Lua.Exp =
            Lua.functionDef
              [[name|a|], [name|b|]]
              [ Lua.return
                  ( Lua.functionDef
                      [[name|d|]]
                      [ Lua.local1 [name|i|] (Lua.Integer 42)
                      , Lua.local1 [name|j|] (Lua.Integer 43)
                      , Lua.return (Lua.varName [name|c|])
                      ]
                  )
              ]
      assertEqual (shower @Lua.Exp original) expected $
        rewriteExpWithRule pushDeclarationsDownTheInnerScope original
