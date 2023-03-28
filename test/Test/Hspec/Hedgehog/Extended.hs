module Test.Hspec.Hedgehog.Extended (module H, test) where

import Hedgehog (PropertyT)
import Test.Hspec (SpecWith, it)
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)
import Test.Hspec.Hedgehog qualified as H

test :: String -> PropertyT IO () -> SpecWith ()
test title = modifyMaxSuccess (const 1) . it title . hedgehog
