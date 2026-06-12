module Language.PureScript.Backend.IR.Linker.Spec where

import Data.Map qualified as Map
import Hedgehog ((===))
import Language.PureScript.Backend.IR.Linker (qualifyTopRefs)
import Language.PureScript.Backend.IR.Names
  ( ModuleName (..)
  , Name (..)
  )
import Language.PureScript.Backend.IR.Types
  ( Grouping (..)
  , lets
  , literalInt
  , noAnn
  , refImported
  , refLocal
  )
import Test.Hspec (Spec, describe)
import Test.Hspec.Hedgehog.Extended (test)

spec ∷ Spec
spec = describe "IR Linker" do
  -- See Note [Sequential scoping of Let bindings]
  describe "qualifyTopRefs" do
    let modname = ModuleName "Main"
        x = Name "x"
        y = Name "y"
        topX = Map.fromList [(x, 0)]
        qualify = qualifyTopRefs modname topX

    test "ref bound by an earlier sibling is not qualified" do
      let e =
            lets
              ( Standalone (noAnn, x, literalInt 1)
                  :| [Standalone (noAnn, y, refLocal x 0)]
              )
              (literalInt 0)
      qualify e === e

    test "ref to a top-level name in own RHS is qualified" do
      let original =
            lets (Standalone (noAnn, x, refLocal x 0) :| []) (literalInt 0)
          expected =
            lets
              (Standalone (noAnn, x, refImported modname x 0) :| [])
              (literalInt 0)
      qualify original === expected

    test "ref in the body pointing past the binder is qualified" do
      let original =
            lets (Standalone (noAnn, x, literalInt 1) :| []) (refLocal x 1)
          expected =
            lets
              (Standalone (noAnn, x, literalInt 1) :| [])
              (refImported modname x 1)
      qualify original === expected

    test "ref in the body bound by the let is not qualified" do
      let e = lets (Standalone (noAnn, x, literalInt 1) :| []) (refLocal x 0)
      qualify e === e
