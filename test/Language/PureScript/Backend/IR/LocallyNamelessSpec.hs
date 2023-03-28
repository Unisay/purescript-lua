module Language.PureScript.Backend.IR.LocallyNamelessSpec where

import Control.Monad.Trans.Accum (Accum, add, execAccum)
import Data.List.NonEmpty qualified as NE
import Hedgehog (annotate, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Range qualified as Range
import Language.PureScript.Backend.IR.Gen qualified as Gen
import Language.PureScript.Backend.IR.Types
import Shower (shower)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog.Extended (hedgehog, test)

spec :: Spec
spec = describe "LocallyNameless encoding" do
  it "AbsBinding roundtrip" $ hedgehog do
    name <- forAll Gen.name
    body <- forAll Gen.exp
    (Just name, body) === runIdentity (unbindAbs (bindAbs (ArgNamed name) body))

  it "LetBinding roundtrip" $ hedgehog do
    body <- forAll Gen.exp
    bindings <- forAll $ Gen.nonEmpty (Range.linear 1 5) Gen.binding
    (bindings, body) === runIdentity (unbindLet (bindLet bindings body))

  test "AbsBinding sample" do
    a <- forAll Gen.name
    b <- forAll $ Gen.filter (/= a) Gen.name
    c <- forAll $ Gen.filter (/= a) $ Gen.filter (/= b) Gen.name
    let bound =
          abstraction
            (ArgNamed a)
            ( abstraction
                (ArgNamed b)
                ( abstraction
                    (ArgNamed c)
                    ( refFreeLocal a
                        `application` refFreeLocal b
                        `application` refFreeLocal c
                    )
                )
            )
    collectBoundRefs bound
      === [ Index {level = 2, offset = 0}
          , Index {level = 1, offset = 0}
          , Index {level = 0, offset = 0}
          ]

  test "LetBinding sample" do
    {-
      let a = b{0,1}
          b = let c = 3
                  d = a{1,0}
               in c{0,0} d{0,1}
          in let e = b{1,1} in a{1,0} e{0,0}
     -}
    [a, b, c, d, e] <- forAll $ toList <$> Gen.set (Range.singleton 5) Gen.name
    ce <- forAll Gen.literalNonRecursiveExp
    let bound =
          lets
            ( pure . RecursiveGroup $
                NE.fromList
                  [ (a, refFreeLocal b)
                  ,
                    ( b
                    , lets
                        ( NE.fromList
                            [ Standalone (c, ce)
                            , Standalone (d, refFreeLocal a)
                            ]
                        )
                        (refFreeLocal c `application` refFreeLocal d)
                    )
                  ]
            )
            ( lets
                (pure (Standalone (e, refFreeLocal b)))
                (refFreeLocal a `application` refFreeLocal e)
            )
    annotate $ shower bound
    collectBoundRefs bound
      === [ Index {level = 0, offset = 1} -- b
          , Index {level = 1, offset = 0} -- a
          , Index {level = 0, offset = 0} -- c
          , Index {level = 0, offset = 1} -- d
          , Index {level = 1, offset = 1} -- b
          , Index {level = 1, offset = 0} -- a
          , Index {level = 0, offset = 0} -- e
          ]

collectBoundRefs :: Exp -> [Index]
collectBoundRefs e = execAccum (everywhereTopDownExpM visit e) []
 where
  visit :: Exp -> Accum [Index] Exp
  visit expr@Exp {unExp} =
    case unExp of
      RefBound index -> add [index] $> expr
      _ -> pure expr
