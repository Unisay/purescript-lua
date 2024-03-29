{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fewer imports" #-}

module Language.PureScript.Backend.Lua.Linker.Foreign.Spec where

import Data.List.NonEmpty qualified as NE
import Data.String.Interpolate (__i)
import Language.PureScript.Backend.Lua.Linker.Foreign
import Language.PureScript.Backend.Lua.Name (Name, unsafeName)
import Path (relfile, toFilePath, (</>))
import Path.IO (withSystemTempDir)
import Test.HUnit (Assertion)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.Megaparsec qualified as M

spec ∷ Spec
spec = do
  describe "Foreign parser" do
    it "exports" do
      shouldParse moduleParser (toText rawExports) parsedExports

    it "value" do
      shouldParse valueParser "(((x)(y)))" "((x)(y))"

  describe "Foreign module parser" do
    it "parses file" do
      foreignSource ← withSystemTempDir "foreigns" \foreigns → do
        let path = toFilePath $ foreigns </> [relfile|Foo.lua|]
        writeFile
          path
          [__i|
            local boo = "boo"
            local zoo = "boo" .. "zoo"
            #{rawExports}
          |]
        parseForeignSource foreigns path
      case foreignSource of
        Left err → fail $ show err
        Right source →
          source
            `shouldBe` Source
              { header = Just (toText rawHeader)
              , exports = parsedExports
              }

shouldParse ∷ (Eq a, Show a) ⇒ Parser a → Text → a → Assertion
shouldParse p s e =
  case M.parse p "Test" s of
    Left eb → fail $ M.errorBundlePretty eb
    Right a → a `shouldBe` e

rawHeader ∷ String
rawHeader =
  [__i|
    local boo = "boo"
    local zoo = "boo" .. "zoo"
  |]

rawExports ∷ String
rawExports =
  [__i|
    return {
      foo = (42),
      bar = ("ok"),
      baz = (function(unused) return zoo end)
    }
  |]

parsedExports ∷ NE.NonEmpty (Name, Text)
parsedExports =
  (unsafeName "foo", "42")
    :| [ (unsafeName "bar", "\"ok\"")
       , (unsafeName "baz", "function(unused) return zoo end")
       ]
