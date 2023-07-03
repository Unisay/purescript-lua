module Language.PureScript.Backend.IR.TypesSpec where

import Data.Map qualified as Map
import Hedgehog ((===))
import Language.PureScript.Backend.IR.Types
  ( Exp
  , Grouping (..)
  , Name (..)
  , Parameter (ParamNamed, ParamUnused)
  , Qualified (Imported, Local)
  , RawExp (..)
  , abstraction
  , application
  , countFreeRefs
  , lets
  )
import Language.PureScript.Names (ModuleName (..))
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
    (ParamNamed (Name "cmp"))
    ( abstraction
        (ParamNamed (Name "x"))
        ( abstraction
            (ParamNamed (Name "ys"))
            ( lets
                ( Standalone
                    ( Name "i"
                    , application
                        ( application
                            ( application
                                (Ref (Imported (ModuleName "Data.Maybe") (Name "maybe")) 0)
                                (LiteralInt 0)
                            )
                            ( abstraction
                                (ParamNamed (Name "v"))
                                ( application
                                    ( application
                                        (Ref (Imported (ModuleName "Data.Array") (Name "add")) 0)
                                        (Ref (Local (Name "v")) 0)
                                    )
                                    (LiteralInt 1)
                                )
                            )
                        )
                        ( application
                            ( application
                                (Ref (Imported (ModuleName "Data.Array") (Name "findLastIndex")) 0)
                                ( abstraction
                                    (ParamNamed (Name "y"))
                                    ( application
                                        ( application
                                            (Ref (Imported (ModuleName "Data.Array") (Name "eq1")) 0)
                                            ( application
                                                ( application
                                                    (Ref (Local (Name "cmp")) 0)
                                                    (Ref (Local (Name "x")) 0)
                                                )
                                                (Ref (Local (Name "y")) 0)
                                            )
                                        )
                                        (Ref (Imported (ModuleName "Data.Ordering") (Name "GT")) 0)
                                    )
                                )
                            )
                            (Ref (Local (Name "ys")) 0)
                        )
                    )
                    :| []
                )
                ( application
                    (Ref (Imported (ModuleName "Partial.Unsafe") (Name "unsafePartial")) 0)
                    ( abstraction
                        ParamUnused
                        ( application
                            (Ref (Imported (ModuleName "Data.Array") (Name "fromJust")) 0)
                            ( application
                                ( application
                                    ( application
                                        (Ref (Imported (ModuleName "Data.Array") (Name "insertAt")) 0)
                                        (Ref (Local (Name "i")) 0)
                                    )
                                    (Ref (Local (Name "x")) 0)
                                )
                                (Ref (Local (Name "ys")) 0)
                            )
                        )
                    )
                )
            )
        )
    )
