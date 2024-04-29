{-# LANGUAGE QuasiQuotes #-}

module Language.PureScript.Backend.Lua.Optimizer.Spec where

import Language.PureScript.Backend.Lua.Name (name)
import Language.PureScript.Backend.Lua.Optimizer
  ( pushDeclarationsDownTheInnerScope
  , removeScopeWhenInsideEmptyFunction
  , rewriteExpWithRule
  )
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (assertEqual)
import Text.Pretty.Simple (pShow)

spec ∷ Spec
spec = describe "Lua AST Optimizer" do
  describe "optimizes expressions" do
    it "removes scope when inside an empty function" do
      let original ∷ Lua.Exp =
            Lua.functionDef
              [Lua.paramNamed [name|a|]]
              [ Lua.return
                  ( Lua.functionDef
                      [Lua.paramNamed [name|b|]]
                      [Lua.return (Lua.scope [Lua.return (Lua.varName [name|c|])])]
                  )
              ]
          expected ∷ Lua.Exp =
            Lua.functionDef
              [Lua.paramNamed [name|a|]]
              [ Lua.return
                  ( Lua.functionDef
                      [Lua.paramNamed [name|b|]]
                      [Lua.return (Lua.varName [name|c|])]
                  )
              ]
      assertEqual (toString $ pShow original) expected $
        rewriteExpWithRule removeScopeWhenInsideEmptyFunction original

    it "pushes declarations down into an inner scope" do
      let original ∷ Lua.Exp =
            Lua.functionDef
              [Lua.paramNamed [name|a|], Lua.paramNamed [name|b|]]
              [ Lua.local1 [name|i|] (Lua.integer 42)
              , Lua.local1 [name|j|] (Lua.integer 43)
              , Lua.return
                  ( Lua.functionDef
                      [Lua.paramNamed [name|d|]]
                      [Lua.return (Lua.varName [name|c|])]
                  )
              ]
          expected ∷ Lua.Exp =
            Lua.functionDef
              [Lua.paramNamed [name|a|], Lua.paramNamed [name|b|]]
              [ Lua.return
                  ( Lua.functionDef
                      [Lua.paramNamed [name|d|]]
                      [ Lua.local1 [name|i|] (Lua.integer 42)
                      , Lua.local1 [name|j|] (Lua.integer 43)
                      , Lua.return (Lua.varName [name|c|])
                      ]
                  )
              ]
      assertEqual (toString $ pShow @Lua.Exp original) expected $
        rewriteExpWithRule pushDeclarationsDownTheInnerScope original
