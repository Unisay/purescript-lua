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
shouldBe expected actual = assertEqualPretty "" actual expected

assertEqualPretty ∷ (HasCallStack, Eq a, Show a) ⇒ String → a → a → Assertion
assertEqualPretty = assertEqualShowing (toString . pShow)

{- | Asserts that the specified actual value is equal to the expected value.
 The output message will contain the prefix, the expected value, and the
 actual value.

 If the prefix is the empty string (i.e., @\"\"@), then the prefix is omitted
 and only the expected and actual values are output.
-}
assertEqualShowing
  ∷ (HasCallStack, Eq a)
  ⇒ (a → String)
  -- ^ A function to convert the expected value to a string
  → String
  -- ^ The message prefix
  → a
  -- ^ The expected value
  → a
  -- ^ The actual value
  → Assertion
assertEqualShowing shower preface expected actual =
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
  expectedMsg = shower expected
  actualMsg = shower actual

location ∷ HasCallStack ⇒ Maybe SrcLoc
location = case reverse Data.CallStack.callStack of
  (_, loc) : _ → Just loc
  [] → Nothing
