module Test.Hspec.Hedgehog.Extended
  ( module H
  , test
  , xtest
  ) where

import Hedgehog (PropertyT)
import Test.Hspec (SpecWith, it, xit)
import Test.Hspec.Hedgehog (hedgehog, modifyMaxShrinks, modifyMaxSuccess)
import Test.Hspec.Hedgehog qualified as H

test ∷ String → PropertyT IO () → SpecWith ()
test title =
  modifyMaxShrinks (const 0)
    . modifyMaxSuccess (const 1)
    . it title
    . hedgehog

xtest ∷ String → PropertyT IO () → SpecWith ()
xtest title = xit title . hedgehog
