module Language.PureScript.Backend.IR.OptimizerSpec where

import Hedgehog (annotateShow, forAll, (===))
import Language.PureScript.Backend.IR.Gen qualified as Gen
import Language.PureScript.Backend.IR.Optimizer (optimizeExpression)
import Language.PureScript.Backend.IR.Types
  ( Exp (..)
  , ExpF (..)
  , Grouping (Standalone)
  , Literal (..)
  , Name
  , application
  , boolean
  , ifThenElse
  , lets
  , refFreeLocal
  )
import Test.Hspec (Spec, describe)
import Test.Hspec.Hedgehog.Extended (test)

spec :: Spec
spec = describe "IR Optimizer" do
  describe "optimizes expressions" do
    test "removes redundant else branch" do
      thenBranch <- forAll Gen.exp
      elseBranch <- forAll Gen.exp
      let ifThenElseStatement = ifThenElse (boolean True) thenBranch elseBranch
      annotateShow ifThenElseStatement
      thenBranch === optimize ifThenElseStatement

    test "removes redundant then branch" do
      thenBranch <- forAll Gen.exp
      elseBranch <- forAll Gen.exp
      let ifThenElseStatement = ifThenElse (boolean False) thenBranch elseBranch
      annotateShow ifThenElseStatement
      elseBranch === optimize ifThenElseStatement

  describe "inlines expressions" do
    test "inlines literals" do
      name <- forAll Gen.name
      inlinee <- forAll Gen.scalarExp
      let original = let1 name inlinee (refFreeLocal name)
          expected = let1 name inlinee inlinee
      optimize original === expected

    test "inlines references" do
      name <- forAll Gen.name
      inlinee <- forAll Gen.refFreeLocal
      let original = let1 name inlinee (refFreeLocal name)
          expected = let1 name inlinee inlinee
      optimize original === expected

    test "inlines expressions referenced once" do
      name <- forAll Gen.name
      inlinee <-
        forAll $ mfilter (\e -> not (isRefFree e || isLiteral e)) Gen.exp
      let body = refFreeLocal name
          original = let1 name inlinee body
          expected = let1 name inlinee inlinee
      annotateShow body
      optimize original === expected

    test "doesn't inline expressions referenced more than once" do
      name <- forAll Gen.name
      inlinee <-
        forAll $ mfilter (\e -> not (isRefFree e || isLiteral e)) Gen.exp
      let body = application (refFreeLocal name) (refFreeLocal name)
          original = let1 name inlinee body
      annotateShow original
      optimize original === original

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------

optimize :: Exp -> Exp
optimize = flip evalState (0 :: Natural) . optimizeExpression

let1 :: Name -> Exp -> Exp -> Exp
let1 n e = lets (Standalone (n, e) :| [])

isRefFree :: Exp -> Bool
isRefFree =
  unExp >>> \case
    RefFree {} -> True
    _ -> False

isLiteral :: Exp -> Bool
isLiteral =
  unExp >>> \case
    Lit {} -> True
    _ -> False

isScalar :: Exp -> Bool
isScalar =
  unExp >>> \case
    Lit l ->
      case l of
        Integer {} -> True
        Floating {} -> True
        String {} -> True
        Char {} -> True
        Boolean {} -> True
        _ -> False
    _ -> False
