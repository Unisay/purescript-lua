module Language.PureScript.Backend.IR.OptimizerSpec where

import Data.Map qualified as Map
import Hedgehog (annotateShow, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Language.PureScript.Backend.IR.Gen qualified as Gen
import Language.PureScript.Backend.IR.Linker (LinkMode (..))
import Language.PureScript.Backend.IR.Linker qualified as Linker
import Language.PureScript.Backend.IR.Optimizer
  ( optimizedExpression
  , optimizedUberModule
  )
import Language.PureScript.Backend.IR.Types
  ( Exp
  , Grouping (Standalone)
  , Module (..)
  , Name (..)
  , RawExp (..)
  , application
  , eq
  , ifThenElse
  , isLiteral
  , lets
  , literalBool
  , literalInt
  , refLocal0
  )
import Language.PureScript.Names (moduleNameFromString)
import Test.Hspec (Spec, describe)
import Test.Hspec.Hedgehog.Extended (test)

spec ∷ Spec
spec = describe "IR Optimizer" do
  describe "optimizes expressions" do
    test "removes redundant else branch" do
      thenBranch ← forAll Gen.exp
      elseBranch ← forAll Gen.exp
      let ifThenElseStatement = ifThenElse (literalBool True) thenBranch elseBranch
      annotateShow ifThenElseStatement
      thenBranch === optimizedExpression ifThenElseStatement

    test "removes redundant then branch" do
      thenBranch ← forAll Gen.exp
      elseBranch ← forAll Gen.exp
      let ifThenElseStatement = ifThenElse (literalBool False) thenBranch elseBranch
      annotateShow ifThenElseStatement
      elseBranch === optimizedExpression ifThenElseStatement

  describe "inlines expressions" do
    test "inlines literals" do
      name ← forAll Gen.name
      inlinee ← forAll Gen.scalarExp
      let original = let1 name inlinee (refLocal0 name)
          expected = let1 name inlinee inlinee
      optimizedExpression original === expected

    test "inlines references" do
      name ← forAll Gen.name
      inlinee ← forAll Gen.refLocal
      let original = let1 name inlinee (refLocal0 name)
          expected = let1 name inlinee inlinee
      optimizedExpression original === expected

    test "inlines expressions referenced once" do
      name ← forAll Gen.name
      inlinee ←
        forAll $ mfilter (\e → not (isRef e || isLiteral e)) Gen.exp
      let body = refLocal0 name
          original = let1 name inlinee body
          expected = let1 name inlinee inlinee
      annotateShow body
      optimizedExpression original === expected

    test "doesn't inline expressions referenced more than once" do
      name ← forAll Gen.name
      inlinee ← forAll $ Gen.choice [Gen.exception, Gen.ctor]
      let original =
            let1 name inlinee $
              application (refLocal0 name) (refLocal0 name)
      annotateShow original
      optimizedExpression original === original

  describe "inliner unlocks more optimizations" do
    test "constant folding after inlining" do
      name ← forAll Gen.name
      let uberName = moduleNameFromString "Main"
          linkMode = LinkAsModule uberName
          mkUber = Linker.makeUberModule linkMode . pure . wrapInModule
      let original =
            mkUber $
              let1 name (literalInt 42) $
                ifThenElse
                  (eq (refLocal0 name) (literalInt 42))
                  (literalInt 1)
                  (literalInt 2)
          expected =
            Linker.UberModule
              { uberModuleBindings = []
              , uberModuleExports = [(Name "main", literalInt 1)]
              }
      annotateShow original
      annotateShow expected
      optimizedUberModule original === expected

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------

wrapInModule ∷ Exp → Module
wrapInModule e =
  Module
    { moduleName = moduleNameFromString "Main"
    , moduleBindings = [Standalone (Name "main", e)]
    , moduleImports = []
    , moduleExports = [Name "main"]
    , moduleReExports = Map.empty
    , moduleForeigns = []
    , modulePath = "Main.purs"
    , dataTypes = Map.empty
    }

let1 ∷ Name → Exp → Exp → Exp
let1 n e = lets (Standalone (n, e) :| [])

isRef ∷ Exp → Bool
isRef = \case
  Ref {} → True
  _ → False
