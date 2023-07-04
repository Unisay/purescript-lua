module Test.Hspec.Extra where

import Control.Exception (catch, throwIO)
import Test.HUnit.Lang
  ( FailureReason (Reason)
  , HUnitFailure (HUnitFailure)
  , formatFailureReason
  )
import Test.Hspec (Expectation)

{- |
 Decorate an @'Expectation'@ with a message. The @'String'@ is prepended to
 failure messages.

 @
 myValue `shouldBe` myExpectation `annotatingWith` "Oh GAWD no!"
 @
-}
annotatingWith ∷ Expectation → String → Expectation
annotatingWith action message =
  action `catch` \(HUnitFailure l r) →
    throwIO . HUnitFailure l . Reason $
      message <> "\n\n" <> formatFailureReason r

infix 0 `annotatingWith`
