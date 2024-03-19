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
  , renameShadowedNamesInExpr
  )
import Language.PureScript.Backend.IR.Types
  ( Exp
  , Grouping (Standalone)
  , Module (..)
  , Name (..)
  , Parameter (..)
  , RawExp (..)
  , abstraction
  , application
  , eq
  , ifThenElse
  , isLiteral
  , lets
  , literalBool
  , literalInt
  , refLocal
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
      inlinee ← forAll $ fmap optimizedExpression do
        mfilter (\e → not (isRef e || isLiteral e)) Gen.exp
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

  describe "renames shadowed names" do
    test "nested λ-abstractions" do
      name ← forAll Gen.name
      let
        name1 = Name $ nameToText name <> "1"
        name2 = Name $ nameToText name <> "2"
        name3 = Name $ nameToText name <> "3"

      let original =
            abstraction
              (ParamNamed name)
              ( abstraction
                  (ParamNamed name)
                  ( application
                      (refLocal name 0)
                      ( abstraction
                          (ParamNamed name)
                          ( abstraction
                              (ParamNamed name1)
                              ( application
                                  (refLocal name 0)
                                  (refLocal name 2)
                              )
                          )
                      )
                  )
              )

          renamed =
            abstraction
              (ParamNamed name)
              ( abstraction
                  (ParamNamed name2)
                  ( application
                      (refLocal name2 0)
                      ( abstraction
                          (ParamNamed name3)
                          ( abstraction
                              (ParamNamed name1)
                              ( application
                                  (refLocal name3 0)
                                  (refLocal name 0)
                              )
                          )
                      )
                  )
              )
      renameShadowedNamesInExpr mempty original === renamed

    test "nested let-bindings" do
      nameA ← forAll Gen.name
      nameB ← forAll $ mfilter (/= nameA) Gen.name
      valueA ← forAll Gen.literalNonRecursiveExp
      valueB ← forAll Gen.literalNonRecursiveExp
      let original =
            lets
              (Standalone (nameA, valueA) :| [Standalone (nameB, valueB)])
              ( lets
                  ( Standalone (nameA, refLocal nameA 0)
                      :| [Standalone (nameB, refLocal nameB 0)]
                  )
                  ( application
                      (application (refLocal nameA 0) (refLocal nameA 1))
                      (application (refLocal nameB 0) (refLocal nameB 1))
                  )
              )

          nameA1 = Name $ nameToText nameA <> "1"
          nameB1 = Name $ nameToText nameB <> "1"

          renamed =
            lets
              ( Standalone (nameA, valueA)
                  :| [Standalone (nameB, valueB)]
              )
              ( lets
                  ( Standalone (nameA1, refLocal nameA 0)
                      :| [Standalone (nameB1, refLocal nameB 0)]
                  )
                  ( application
                      (application (refLocal nameA1 0) (refLocal nameA 0))
                      (application (refLocal nameB1 0) (refLocal nameB 0))
                  )
              )
      renameShadowedNamesInExpr mempty original === renamed

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
    }

let1 ∷ Name → Exp → Exp → Exp
let1 n e = lets (Standalone (n, e) :| [])

isRef ∷ Exp → Bool
isRef = \case
  Ref {} → True
  _ → False
