module Language.PureScript.Backend.IR.Types.Spec where

import Data.Map qualified as Map
import Hedgehog ((===))
import Language.PureScript.Backend.IR.Names
  ( ModuleName (..)
  , Name (..)
  , Qualified (Imported)
  )
import Language.PureScript.Backend.IR.Types
  ( Exp
  , Grouping (..)
  , abstraction
  , application
  , countFreeRefs
  , lets
  , literalInt
  , noAnn
  , paramNamed
  , paramUnused
  , refImported
  , refLocal
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
