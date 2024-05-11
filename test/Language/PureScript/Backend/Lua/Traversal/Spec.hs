{-# LANGUAGE QuasiQuotes #-}

module Language.PureScript.Backend.Lua.Traversal.Spec where

import Control.Lens.Plated qualified as Plated
import Control.Monad.Trans.Accum (Accum, add, execAccum)
import Data.Set qualified as Set
import Hedgehog (annotate, forAll, (===))
import Language.PureScript.Backend.Lua.Gen qualified as Gen
import Language.PureScript.Backend.Lua.Name qualified as Lua
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)
import Test.Hspec.Hedgehog.Extended (test)
import Text.Pretty.Simple (pShow)

spec ∷ Spec
spec = do
  describe "Plated-based traversals" do
    test "Not rewriting a single term is identity" do
      name ← forAll Gen.name
      let term = Lua.S (Lua.assignVar name (Lua.boolean True))
      annotate $ toString $ pShow term
      Plated.rewrite (const Nothing) term === term

    it "Not rewriting a single term visits every term once" $ hedgehog do
      term ← forAll Gen.term
      annotate $ toString $ pShow term
      let visit ∷ Lua.Term → Accum [Lua.Term] (Maybe (Lua.Term))
          visit t = do
            add [t]
            pure Nothing
      Set.fromList (execAccum (Plated.rewriteM visit term) [])
        === Set.fromList (Plated.universe term)

    it "Rewrites all named variables to fields" do
      term ← forAll Gen.term
      name ← forAll Gen.name
      annotate $ toString $ pShow term
      let term' =
            term & Plated.rewrite \case
              Lua.V (Lua.VarName ann n)
                | n /= name →
                    Just $ Lua.V (Lua.VarField ann (Lua.varNameExp name) n)
              _ → Nothing
      [n | Lua.V (Lua.VarName _ann n) ← Plated.universe term', n /= name] === []

    test "Rewrites terms bottom-up" do
      let term =
            Lua.E
              ( Lua.functionCall
                  ( Lua.functionCall
                      ( Lua.functionCall
                          (Lua.varNameExp [Lua.name|innermost|])
                          [Lua.string "inner"]
                      )
                      [Lua.string "outer"]
                  )
                  [Lua.string "outermost"]
              )
      annotate $ toString $ pShow term
      execAccum (Plated.transformM (\t → add [t] $> t) term) []
        === [ Lua.V (Lua.varName [Lua.name|innermost|])
            , Lua.E (Lua.varNameExp [Lua.name|innermost|])
            , Lua.E (Lua.string "inner")
            , Lua.E
                ( Lua.functionCall
                    (Lua.varNameExp [Lua.name|innermost|])
                    [Lua.string "inner"]
                )
            , Lua.E (Lua.string "outer")
            , Lua.E
                ( Lua.functionCall
                    ( Lua.functionCall
                        (Lua.varNameExp [Lua.name|innermost|])
                        [Lua.string "inner"]
                    )
                    [Lua.string "outer"]
                )
            , Lua.E (Lua.string "outermost")
            , Lua.E
                ( Lua.functionCall
                    ( Lua.functionCall
                        ( Lua.functionCall
                            (Lua.varNameExp [Lua.name|innermost|])
                            [Lua.string "inner"]
                        )
                        [Lua.string "outer"]
                    )
                    [Lua.string "outermost"]
                )
            ]
