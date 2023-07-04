{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Hspec.Golden
  ( Golden (..)
  , defaultGolden
  )
where

import Path (Abs, File, Path, parent, toFilePath)
import Path.IO (createDirIfMissing, doesFileExist)
import Test.Hspec.Core.Spec
  ( Example (..)
  , FailureReason (..)
  , Result (..)
  , ResultStatus (..)
  )

{- | Golden tests parameters

 @
 import           Data.Text (Text)
 import qualified Data.Text.IO as T

 goldenText :: Path Abs File -> Text -> Golden Text
 goldenText name actualOutput =
   Golden {
     output = actualOutput,
     encodePretty = prettyText,
     writeToFile = T.writeFile,
     readFromFile = T.readFile,
     goldenFile = ".specific-golden-dir" </> name </> "golden",
     actualFile = Just (".specific-golden-dir" </> name </> "actual"),
     failFirstTime = False
   }

 describe "myTextFunc" $
   it "generates the right output with the right params" $
     goldenText "myTextFunc" (myTextFunc params)
 @
-}
data Golden str = Golden
  { produceOutput ∷ IO str
  -- ^ Output
  , encodePretty ∷ str → String
  -- ^ Makes the comparison pretty when the test fails
  , writeToFile ∷ Path Abs File → str → IO ()
  -- ^ How to write into the golden file the file
  , readFromFile ∷ Path Abs File → IO str
  -- ^ How to read the file,
  , goldenFile ∷ Path Abs File
  -- ^ Where to read/write the golden file for this test.
  , actualFile ∷ Maybe (Path Abs File)
  -- ^ Where to save the actual file for this test.
  -- If it is @Nothing@ then no file is written.
  , failFirstTime ∷ Bool
  -- ^ Whether to record a failure the first time this test is run
  }

instance Eq str ⇒ Example (Golden str) where
  type Arg (Golden str) = ()
  evaluateExample e = evaluateExample (\() → e)

instance Eq str ⇒ Example (arg → Golden str) where
  type Arg (arg → Golden str) = arg
  evaluateExample golden _ action _ = do
    ref ← newIORef (Result "" Success)
    action $ \arg → do
      r ← runGolden (golden arg)
      writeIORef ref (fromGoldenResult r)
    readIORef ref

-- | Transform a GoldenResult into a Result from Hspec
fromGoldenResult ∷ GoldenResult → Result
fromGoldenResult = \case
  SameOutput →
    Result "Golden and Actual output hasn't changed" Success
  FirstExecutionSucceed →
    Result "First time execution. Golden file created." Success
  FirstExecutionFail →
    Result
      "First time execution. Golden file created."
      (Failure Nothing (Reason "failFirstTime is set to True"))
  MissmatchOutput expected actual →
    Result
      "Files golden and actual not match"
      (Failure Nothing (ExpectedButGot Nothing expected actual))

defaultGolden
  ∷ Path Abs File
  → Maybe (Path Abs File)
  → IO Text
  → Golden Text
defaultGolden goldenFile actualFile produceOutput =
  Golden
    { produceOutput
    , encodePretty = show
    , writeToFile = \f → writeFileBS (toFilePath f) . encodeUtf8
    , readFromFile = fmap decodeUtf8 . readFileBS . toFilePath
    , goldenFile
    , actualFile
    , failFirstTime = False
    }

-- | Possible results from a golden test execution
data GoldenResult
  = MissmatchOutput String String
  | SameOutput
  | FirstExecutionSucceed
  | FirstExecutionFail

-- | Runs a Golden test.
runGolden ∷ Eq str ⇒ Golden str → IO GoldenResult
runGolden Golden {..} = do
  let goldenTestDir = parent goldenFile
  createDirIfMissing True goldenTestDir
  goldenFileExist ← doesFileExist goldenFile
  output ← produceOutput

  case actualFile of
    Nothing → pass
    Just actual → do
      let actualDir = parent actual
      createDirIfMissing True actualDir
      writeToFile actual output

  if not goldenFileExist
    then do
      writeToFile goldenFile output
      pure $
        if failFirstTime
          then FirstExecutionFail
          else FirstExecutionSucceed
    else do
      contentGolden ← readFromFile goldenFile
      pure
        if contentGolden == output
          then SameOutput
          else
            MissmatchOutput
              (encodePretty contentGolden)
              (encodePretty output)
