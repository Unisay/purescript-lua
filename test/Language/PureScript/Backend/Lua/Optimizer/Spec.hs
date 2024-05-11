{-# LANGUAGE QuasiQuotes #-}

module Language.PureScript.Backend.Lua.Optimizer.Spec where

import Hedgehog (annotate, forAll, (===))
import Language.PureScript.Backend.Lua.Gen qualified as Gen
import Language.PureScript.Backend.Lua.Name (name)
import Language.PureScript.Backend.Lua.Optimizer
  ( AppliedHow (..)
  , RewriteRule
  , appliedHow
  , collapseFunCalls
  , pushDeclarationsDownTheInnerScope
  , removeScopeWhenInsideEmptyFunction
  , rewriteCurried
  )
import Language.PureScript.Backend.Lua.Printer (printLuaChunk)
import Language.PureScript.Backend.Lua.Traversal (everywhereExp)
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty
  ( assertEqualPretty
  , assertEqualShowing
  , shouldBe
  )
import Test.Hspec.Hedgehog (hedgehog)
import Test.Hspec.Hedgehog.Extended (test)
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
                      [ Lua.return
                          ( Lua.scope
                              [Lua.return $ Lua.var (Lua.varName [name|c|])]
                          )
                      ]
                  )
              ]
          expected ∷ Lua.Exp =
            Lua.functionDef
              [Lua.paramNamed [name|a|]]
              [ Lua.return
                  ( Lua.functionDef
                      [Lua.paramNamed [name|b|]]
                      [Lua.return $ Lua.var (Lua.varName [name|c|])]
                  )
              ]
      assertEqualPretty (toString $ pShow original) expected $
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
                      [Lua.return $ Lua.var (Lua.varName [name|c|])]
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
                      , Lua.return $ Lua.var (Lua.varName [name|c|])
                      ]
                  )
              ]
      assertEqualPretty (toString $ pShow @Lua.Exp original) expected $
        rewriteExpWithRule pushDeclarationsDownTheInnerScope original

  describe "Determines how a variable is applied" do
    it "Unknown always loses" $ hedgehog do
      how ← forAll Gen.appliedHow
      Unknown <> how === how

    it "AppliedAtLeastTwice always loses" $ hedgehog do
      how ← forAll $ Gen.knownAppliedHow
      AppliedAtLeastTwice <> how === how

    it "NotApplied always wins" $ hedgehog do
      how ← forAll $ Gen.knownAppliedHow
      NotApplied <> how === NotApplied

    it "is not applied" do
      let var ∷ Lua.Var = Lua.varName [name|v|]
      let terms = [Lua.S . Lua.return $ Lua.var var]
      appliedHow var terms `shouldBe` NotApplied

    it "is applied once" do
      let var ∷ Lua.Var = Lua.varName [name|v|]
      let terms =
            [ Lua.S . Lua.return $
                Lua.functionCall
                  (Lua.var var)
                  [Lua.functionCall (Lua.var var) [Lua.string "x"]]
            , Lua.S . Lua.return $
                Lua.functionCall
                  ( Lua.functionCall
                      (Lua.var var)
                      [Lua.string "y"]
                  )
                  [Lua.functionCall (Lua.var var) [Lua.string "z"]]
            ]
      appliedHow var terms `shouldBe` AppliedOnce

    it "is applied at least twice" do
      let var ∷ Lua.Var = Lua.varName [name|v|]
      let terms =
            [ Lua.S . Lua.return $
                Lua.functionCall (Lua.functionCall (Lua.var var) []) []
            ]
      appliedHow var terms `shouldBe` AppliedAtLeastTwice

    it "is applied twice but with different vars" do
      let var1 ∷ Lua.Var = Lua.varName [name|v1|]
      let var2 ∷ Lua.Var = Lua.varName [name|v2|]
      let terms =
            [ Lua.S . Lua.return $
                Lua.functionCall (Lua.functionCall (Lua.var var1) []) []
            ]
      appliedHow var2 terms `shouldBe` Unknown

  describe "Uncurries functions" do
    let var ∷ Lua.Var = Lua.varName [name|v|]
    let printedChunk s =
          toString . unlines $
            [ renderStrict . layoutPretty defaultLayoutOptions $
                printLuaChunk (fromList s)
            ]
    let assertRewriteCurried
          ∷ HasCallStack
          ⇒ Lua.Var
          → [Lua.Statement]
          → Maybe [Lua.Statement]
          → IO ()
        assertRewriteCurried variable stats expected =
          assertEqualShowing showRewriteCurried "" expected actual
         where
          actual = rewriteCurried variable stats
          -- actual = rewriteTillFixpoint (rewriteCurried variable) stats
          showRewriteCurried = \case
            Nothing → "Nothing"
            Just s → printedChunk s

    it "No uncurrying if function is never referred to by name" do
      assertRewriteCurried var [] Nothing

    it "No uncurrying if function is never applied" do
      let stats = [Lua.return $ Lua.var var]
      assertRewriteCurried var stats Nothing

    it "No uncurrying if a function is applied once" do
      let call1 = Lua.functionCall (Lua.var var) [Lua.integer 1]
      let call2 = Lua.functionCall call1 [Lua.integer 2]
      let stats = [Lua.assignVar [name|r|] call1, Lua.return call2]
      assertRewriteCurried var stats Nothing

    it "No uncurrying if a function is applied to one argument at least once" do
      let varEx = Lua.var var
          call1 = Lua.functionCall varEx []
          call2 = Lua.functionCall call1 [Lua.integer 1]
          call3 = Lua.functionCall call2 [Lua.integer 2]
          stats =
            [ Lua.assignVar [name|tmp1|] call2
            , Lua.assignVar [name|tmp2|] call1
            , Lua.return call3
            ]
      assertRewriteCurried var stats Nothing

    it "No uncurrying if a variable is reassigned" do
      let var0 = Lua.varName [name|v0|]
          var1 = Lua.varName [name|v1|]
          call1 = Lua.functionCall (Lua.var var1) [Lua.integer 1]
          stats =
            [ Lua.assign var1 (Lua.var var0)
            , Lua.return $ Lua.functionCall call1 [Lua.integer 2]
            ]
      assertRewriteCurried var stats Nothing

    it "Uncurried: up to min number of applications (2)" do
      let varEx = Lua.var var
          call1 = Lua.functionCall varEx []
          call2 = Lua.functionCall call1 [Lua.nil, Lua.integer 1]
          call3 = Lua.functionCall call2 [Lua.integer 2]
          call2' =
            Lua.functionCall varEx [Lua.nil, Lua.nil, Lua.integer 1]
          call3' =
            Lua.functionCall
              ( Lua.functionCall
                  varEx
                  [Lua.nil, Lua.nil, Lua.integer 1]
              )
              [Lua.integer 2]
          stats =
            [ Lua.assignVar [name|tmp1|] call2
            , Lua.assignVar [name|tmp2|] call3
            , Lua.return call3
            ]
          stats' =
            [ Lua.assignVar [name|tmp1|] call2'
            , Lua.assignVar [name|tmp2|] call3'
            , Lua.return call3'
            ]
      assertRewriteCurried var stats $ Just stats'

    test "Uncurried: rewrite 3 times" do
      let varEx = Lua.var var
          call3 =
            Lua.functionCall
              ( Lua.functionCall
                  (Lua.functionCall varEx [])
                  [Lua.nil, Lua.integer 1]
              )
              [Lua.integer 2]

      let rewrittenOnce = rewriteCurried var [Lua.return call3]

      annotate $ toString $ pShow rewrittenOnce

      let rewrittenTwice = rewriteCurried var =<< rewrittenOnce

      annotate $ toString $ pShow rewrittenTwice

      let rewrittenThrice = rewriteCurried var =<< rewrittenTwice

      annotate $ toString $ pShow rewrittenThrice

      rewrittenOnce
        === Just
          [ Lua.return $
              Lua.functionCall
                (Lua.functionCall varEx [Lua.nil, Lua.nil, Lua.integer 1])
                [Lua.integer 2]
          ]

      rewrittenTwice
        === Just
          [ Lua.return $
              Lua.functionCall
                varEx
                [ Lua.nil
                , Lua.nil
                , Lua.integer 1
                , Lua.integer 2
                ]
          ]

      rewrittenThrice === Nothing

    {-

    (fun ((fun 1) 2)) ((fun 3) 4)

    ==>

    fun (fun (1, 2), fun (3, 4))

    -}
    test "rewrite 2 + 2" do
      let fun = Lua.var var
          terms =
            [ Lua.return $
                Lua.functionCall
                  ( Lua.functionCall
                      fun
                      [ Lua.functionCall
                          (Lua.functionCall fun [Lua.string "1"])
                          [Lua.string "2"]
                      ]
                  )
                  [ Lua.functionCall
                      (Lua.functionCall fun [Lua.string "3"])
                      [Lua.string "4"]
                  ]
            ]

          actual = rewriteTillFixpoint (rewriteCurried var) terms

      annotate $ toString $ pShow actual

      actual
        === Just
          [ Lua.return $
              Lua.functionCall
                fun
                [ Lua.functionCall fun [Lua.string "1", Lua.string "2"]
                , Lua.functionCall fun [Lua.string "3", Lua.string "4"]
                ]
          ]

    let subterms =
          Lua.functionCall -- 5
            ( Lua.functionCall -- 4
                ( Lua.functionCall -- 3
                    ( Lua.functionCall -- 2
                        ( Lua.functionCall -- 1
                            (Lua.varNameExp [name|v|])
                            [] -- 1
                        )
                        [Lua.string "a"] -- 2
                    )
                    [Lua.string "b", Lua.integer 1] -- 3
                )
                [Lua.integer 2] -- 4
            )
            [Lua.integer 3] -- 5
            --
    test "collapseFunCalls 0" do
      collapseFunCalls 0 subterms === Lua.E subterms

    test "collapseFunCalls 1" do
      collapseFunCalls 1 subterms === Lua.E subterms

    test "collapseFunCalls 2" do
      collapseFunCalls 2 subterms
        === Lua.E
          ( Lua.functionCall -- 5
              ( Lua.functionCall -- 4
                  ( Lua.functionCall -- 3
                      ( Lua.functionCall -- (1 + 2)
                          (Lua.varNameExp [name|v|])
                          [Lua.nil, Lua.string "a"]
                      )
                      [Lua.string "b", Lua.integer 1] -- 3
                  )
                  [Lua.integer 2] -- 4
              )
              [Lua.integer 3] -- 5
          )

    test "collapseFunCalls 4" do
      collapseFunCalls 4 subterms
        === Lua.E
          ( Lua.functionCall -- 5
              ( Lua.functionCall
                  (Lua.varNameExp [name|v|])
                  [ Lua.nil -- 1
                  , Lua.string "a" -- 2
                  , Lua.string "b" -- 3
                  , Lua.integer 1 -- 3
                  , Lua.integer 2 -- 4
                  ]
              )
              [Lua.integer 3] -- 5
          )

rewriteExpWithRule ∷ RewriteRule → Lua.Exp → Lua.Exp
rewriteExpWithRule rule = everywhereExp rule identity

rewriteTillFixpoint ∷ (t → Maybe t) → t → Maybe t
rewriteTillFixpoint f ss =
  let r = f ss
   in case r of
        Nothing → Just ss
        Just ss' → rewriteTillFixpoint f ss'
