module Language.PureScript.Backend.IR.Types.Spec where

import Data.Map qualified as Map
import Hedgehog ((===))
import Language.PureScript.Backend.IR.Names
  ( ModuleName (..)
  , Name (..)
  , Qualified (Imported, Local)
  )
import Language.PureScript.Backend.IR.Types
  ( Exp
  , Grouping (..)
  , abstraction
  , application
  , countFreeRef
  , countFreeRefs
  , lets
  , literalInt
  , noAnn
  , paramNamed
  , paramUnused
  , refImported
  , refLocal
  , shift
  , substitute
  )
import Test.Hspec (Spec, describe)
import Test.Hspec.Hedgehog.Extended (test)

spec ∷ Spec
spec = describe "Types" do
  test "countFreeRefs" do
    countFreeRefs expr
      === Map.fromList
        [ (Imported (ModuleName "Data.Array") (Name "add"), 1)
        , (Imported (ModuleName "Data.Array") (Name "eq1"), 1)
        , (Imported (ModuleName "Data.Array") (Name "findLastIndex"), 1)
        , (Imported (ModuleName "Data.Array") (Name "fromJust"), 1)
        , (Imported (ModuleName "Data.Array") (Name "insertAt"), 1)
        , (Imported (ModuleName "Data.Maybe") (Name "maybe"), 1)
        , (Imported (ModuleName "Data.Ordering") (Name "GT"), 1)
        , (Imported (ModuleName "Partial.Unsafe") (Name "unsafePartial"), 1)
        ]

  -- Convention: Let bindings have sequential (let*) scoping — in a
  -- standalone binding's RHS the earlier siblings of the same Let are
  -- in scope, while the binding's own name refers to an outer binder.
  describe "Let sequential (let*) scoping" do
    let x = Name "x"
        y = Name "y"

    test "shift: ref bound by an earlier sibling is not shifted" do
      let e =
            lets
              ( Standalone (noAnn, x, literalInt 1)
                  :| [Standalone (noAnn, y, refLocal x 0)]
              )
              (literalInt 0)
      shift 1 x 0 e === e

    test "shift: ref to an outer name in own RHS is shifted" do
      let original =
            lets (Standalone (noAnn, x, refLocal x 0) :| []) (literalInt 0)
          shifted =
            lets (Standalone (noAnn, x, refLocal x 1) :| []) (literalInt 0)
      shift 1 x 0 original === shifted

    test "shift: ref in the body bound by the let is not shifted" do
      let e =
            lets (Standalone (noAnn, x, literalInt 1) :| []) (refLocal x 0)
      shift 1 x 0 e === e

    test "countFreeRefs: ref bound by an earlier sibling is not free" do
      let e =
            lets
              ( Standalone (noAnn, x, literalInt 1)
                  :| [Standalone (noAnn, y, refLocal x 0)]
              )
              (literalInt 0)
      countFreeRef (Local x) e === 0

    test "countFreeRefs: ref to an outer name in own RHS is free" do
      let e =
            lets (Standalone (noAnn, x, refLocal x 0) :| []) (literalInt 0)
      countFreeRef (Local x) e === 1

    test "substitute: ref bound by an earlier sibling is not substituted" do
      let e =
            lets
              ( Standalone (noAnn, x, literalInt 1)
                  :| [Standalone (noAnn, y, refLocal x 0)]
              )
              (literalInt 0)
      substitute (Local x) 0 (literalInt 42) e === e

    test "substitute: ref to an outer name in own RHS is substituted" do
      let original =
            lets (Standalone (noAnn, x, refLocal x 0) :| []) (literalInt 0)
          expected =
            lets (Standalone (noAnn, x, literalInt 42) :| []) (literalInt 0)
      substitute (Local x) 0 (literalInt 42) original === expected

expr ∷ Exp
expr =
  abstraction
    (paramNamed (Name "cmp"))
    ( abstraction
        (paramNamed (Name "x"))
        ( abstraction
            (paramNamed (Name "ys"))
            ( lets
                ( Standalone
                    ( noAnn
                    , Name "i"
                    , application
                        ( application
                            ( application
                                (refImported (ModuleName "Data.Maybe") (Name "maybe") 0)
                                (literalInt 0)
                            )
                            ( abstraction
                                (paramNamed (Name "v"))
                                ( application
                                    ( application
                                        (refImported (ModuleName "Data.Array") (Name "add") 0)
                                        (refLocal (Name "v") 0)
                                    )
                                    (literalInt 1)
                                )
                            )
                        )
                        ( application
                            ( application
                                (refImported (ModuleName "Data.Array") (Name "findLastIndex") 0)
                                ( abstraction
                                    (paramNamed (Name "y"))
                                    ( application
                                        ( application
                                            (refImported (ModuleName "Data.Array") (Name "eq1") 0)
                                            ( application
                                                ( application
                                                    (refLocal (Name "cmp") 0)
                                                    (refLocal (Name "x") 0)
                                                )
                                                (refLocal (Name "y") 0)
                                            )
                                        )
                                        (refImported (ModuleName "Data.Ordering") (Name "GT") 0)
                                    )
                                )
                            )
                            (refLocal (Name "ys") 0)
                        )
                    )
                    :| []
                )
                ( application
                    (refImported (ModuleName "Partial.Unsafe") (Name "unsafePartial") 0)
                    ( abstraction
                        paramUnused
                        ( application
                            (refImported (ModuleName "Data.Array") (Name "fromJust") 0)
                            ( application
                                ( application
                                    ( application
                                        (refImported (ModuleName "Data.Array") (Name "insertAt") 0)
                                        (refLocal (Name "i") 0)
                                    )
                                    (refLocal (Name "x") 0)
                                )
                                (refLocal (Name "ys") 0)
                            )
                        )
                    )
                )
            )
        )
    )
