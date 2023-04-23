module Language.PureScript.Backend.IR.OptimizerSpec where

import Language.PureScript.Backend.IR.Optimizer (optimizeExpression)
import Language.PureScript.Backend.IR.Types (Exp, boolean, ifThenElse, integer)
import Shower (shower)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (assertEqual)

spec :: Spec
spec = describe "IR Optimizer" do
  describe "optimizes expressions" do
    it "removes redundant else branch" do
      let original = ifThenElse (boolean True) (integer 1) (integer 2)
          expected = integer 1
      assertEqual (shower original) expected $ optimize original
    it "removes redundant then branch" do
      let original = ifThenElse (boolean False) (integer 1) (integer 2)
          expected = integer 2
      assertEqual (shower original) expected $ optimize original

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------

optimize :: Exp -> Exp
optimize = flip evalState (0 :: Natural) . optimizeExpression
