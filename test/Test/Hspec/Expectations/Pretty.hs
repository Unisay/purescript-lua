module Test.Hspec.Expectations.Pretty where

import Control.Exception (throwIO)
import Data.CallStack (SrcLoc, callStack)
import Test.HUnit.Lang
  ( Assertion
  , FailureReason (ExpectedButGot)
  , HUnitFailure (HUnitFailure)
  )
import Text.Pretty.Simple (pShow)

shouldBe ∷ (HasCallStack, Eq a, Show a) ⇒ a → a → Assertion
shouldBe expected actual = assertEqual "" actual expected

{- | Asserts that the specified actual value is equal to the expected value.
 The output message will contain the prefix, the expected value, and the
 actual value.

 If the prefix is the empty string (i.e., @\"\"@), then the prefix is omitted
 and only the expected and actual values are output.
-}
assertEqual
  ∷ (HasCallStack, Eq a, Show a)
  ⇒ String
  -- ^ The message prefix
  → a
  -- ^ The expected value
  → a
  -- ^ The actual value
  → Assertion
assertEqual preface expected actual =
  unless (actual == expected) do
    prefaceMsg
      `deepseq` expectedMsg
      `deepseq` actualMsg
      `deepseq` throwIO
        ( HUnitFailure location $
            ExpectedButGot prefaceMsg expectedMsg actualMsg
        )
 where
  prefaceMsg
    | null preface = Nothing
    | otherwise = Just preface
  expectedMsg = toString $ pShow expected
  actualMsg = toString $ pShow actual

location ∷ HasCallStack ⇒ Maybe SrcLoc
location = case reverse Data.CallStack.callStack of
  (_, loc) : _ → Just loc
  [] → Nothing
