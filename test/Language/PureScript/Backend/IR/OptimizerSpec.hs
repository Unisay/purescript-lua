module Language.PureScript.Backend.IR.OptimizerSpec where

import Data.Map qualified as Map
import Hedgehog (annotateShow, forAll, (===))
import Language.PureScript.Backend.IR.DCE qualified as DCE
import Language.PureScript.Backend.IR.Gen qualified as Gen
import Language.PureScript.Backend.IR.Linker (LinkMode (..))
import Language.PureScript.Backend.IR.Linker qualified as Linker
import Language.PureScript.Backend.IR.Optimizer
  ( optimizeExpression
  , optimizeUberModule
  )
import Language.PureScript.Backend.IR.Types
  ( Exp
  , ExpF (..)
  , Grouping (Standalone)
  , Literal (..)
  , Module (..)
  , ModuleName (..)
  , Name (..)
  , application
  , boolean
  , eq
  , ifThenElse
  , integer
  , lets
  , refLocal0
  , unExp
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
      thenBranch === optimizeExpression ifThenElseStatement

    test "removes redundant then branch" do
      thenBranch <- forAll Gen.exp
      elseBranch <- forAll Gen.exp
      let ifThenElseStatement = ifThenElse (boolean False) thenBranch elseBranch
      annotateShow ifThenElseStatement
      elseBranch === optimizeExpression ifThenElseStatement

  describe "inlines expressions" do
    test "inlines literals" do
      name <- forAll Gen.name
      inlinee <- forAll Gen.scalarExp
      let original = let1 name inlinee (refLocal0 name)
          expected = let1 name inlinee inlinee
      optimizeExpression original === expected

    test "inlines references" do
      name <- forAll Gen.name
      inlinee <- forAll Gen.refLocal
      let original = let1 name inlinee (refLocal0 name)
          expected = let1 name inlinee inlinee
      optimizeExpression original === expected

    test "inlines expressions referenced once" do
      name <- forAll Gen.name
      inlinee <-
        forAll $ mfilter (\e -> not (isRef e || isLiteral e)) Gen.exp
      let body = refLocal0 name
          original = let1 name inlinee body
          expected = let1 name inlinee inlinee
      annotateShow body
      optimizeExpression original === expected

    test "doesn't inline expressions referenced more than once" do
      name <- forAll Gen.name
      inlinee <-
        forAll $ mfilter (\e -> not (isRef e || isLiteral e)) Gen.exp
      let original =
            let1 name inlinee $
              application (refLocal0 name) (refLocal0 name)
      annotateShow original
      optimizeExpression original === original

  describe "inliner unlocks more optimizations" do
    test "constant folding after inlining" do
      name <- forAll Gen.name
      let uberName = ModuleName "Main"
          linkMode = LinkAsModule uberName
          dceEntry = DCE.EntryPoint uberName []
          mkUber = Linker.makeUberModule linkMode . pure . wrapInModule
      let original =
            mkUber $
              let1 name (integer 42) $
                ifThenElse
                  (eq (refLocal0 name) (integer 42))
                  (integer 1)
                  (integer 2)
          expected = mkUber $ integer 1
      annotateShow original
      annotateShow expected
      optimizeUberModule dceEntry original === expected

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------

wrapInModule :: Exp -> Module
wrapInModule e =
  Module
    { moduleName = ModuleName "Main"
    , moduleBindings = [Standalone (Name "main", e)]
    , moduleImports = []
    , moduleExports = [Name "main"]
    , moduleReExports = Map.empty
    , moduleForeigns = []
    , modulePath = "Main.purs"
    , dataTypes = Map.empty
    }

let1 :: Name -> Exp -> Exp -> Exp
let1 n e = lets (Standalone (n, e) :| [])

isRef :: Exp -> Bool
isRef =
  unExp >>> \case
    Ref {} -> True
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
