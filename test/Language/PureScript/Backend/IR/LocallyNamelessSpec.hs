module Language.PureScript.Backend.IR.LocallyNamelessSpec where

import Control.Monad.Trans.Accum (Accum, add, execAccum)
import Data.List.NonEmpty qualified as NE
import Hedgehog (annotate, annotateShow, forAll, (===))
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

  it "UpdateRefs identity" $ hedgehog do
    expr <- forAll Gen.exp
    expr
      === updateRefs
        (const $ Just . refFreeLocal)
        (const $ Just . refBound)
        expr

  test "subst" do
    v <- forAll Gen.literalNonRecursiveExp
    let a = Name "a"
        b = Name "b"
        c = Name "c"
        d = Name "d"
    let original =
          abstraction (ArgNamed b) $
            lets
              (Standalone (a, refFreeLocal b) :| [Standalone (d, v)])
              (abstraction (ArgNamed c) (refFreeLocal a))
    annotateShow original
    let expected =
          abstraction (ArgNamed b) $
            lets
              (Standalone (a, refFreeLocal b) :| [Standalone (d, v)])
              (abstraction (ArgNamed c) (refFreeLocal b))
    let substituted =
          case unExp original of
            Abs
              ( AbsBinding
                  arg
                  ( LocallyNameless
                      ( unExp ->
                          Let (LetBinding binds@(Standalone (_, r) :| _) body)
                        )
                    )
                ) ->
                abstraction arg $
                  lets
                    (fmap unLocallyNameless <<$>> binds)
                    (unLocallyNameless (subst body (Offset 0) r))
            _ -> error "subst: impossible"

    substituted === expected

  test "subst2" do
    let inlinee = LocallyNameless $ refBound (Index {level = 0, offset = 0})
    let body :: Exp =
          lets'
            ( Standalone
                ( Name "v"
                , LocallyNameless $ refBound (Index {level = 0, offset = 0})
                )
                :| []
            )
            (LocallyNameless (integer 0))

    unLocallyNameless (subst inlinee (Offset 0) (LocallyNameless body))
      === lets'
        ( Standalone
            ( Name "v"
            , LocallyNameless $ refBound (Index {level = 0, offset = 0})
            )
            :| []
        )
        (LocallyNameless (integer 0))

  test "countBoundRefs" do
    v <- forAll Gen.literalNonRecursiveExp
    let a = Name "a"
        b = Name "b"
        c = Name "c"
        d = Name "d"
    let
      ex =
        abstraction
          (ArgNamed b)
          ( lets
              (Standalone (a, refFreeLocal b) :| [Standalone (d, v)])
              ( abstraction
                  (ArgNamed c)
                  (application (refFreeLocal b) (refFreeLocal a))
              )
          )
    case ex of
      Exp {unExp = Abs (AbsBinding _ namelessBody)} -> do
        annotateShow namelessBody
        countBoundRefs namelessBody (Offset 0) === 2
      _ -> fail "Precondition failed: ex is not an abstraction"

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------

collectBoundRefs :: Exp -> [Index]
collectBoundRefs e = execAccum (everywhereTopDownExpM visit e) []
 where
  visit :: Exp -> Accum [Index] Exp
  visit expr@Exp {unExp} =
    case unExp of
      RefBound index -> add [index] $> expr
      _ -> pure expr
