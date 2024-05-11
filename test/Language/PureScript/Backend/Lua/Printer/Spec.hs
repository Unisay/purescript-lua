{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Language.PureScript.Backend.Lua.Printer.Spec where

import Data.Text qualified as Text
import Language.PureScript.Backend.Lua.Name qualified as Lua
import Language.PureScript.Backend.Lua.Printer qualified as Printer
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Hspec (Spec, describe, it, shouldBe)

spec ∷ Spec
spec = do
  it "Name" do
    (rendered . Printer.printName) [Lua.name|foo|] `shouldBe` "foo"

  it "VarName" do
    renderedExpression (Lua.varNameExp [Lua.name|foo|]) `shouldBe` "foo"

  describe "VarField" do
    it "var.field" do
      let e = Lua.varNameExp [Lua.name|expr|]
          f = [Lua.name|foo|]
      renderedExpression (Lua.varFieldExp e f) `shouldBe` "expr.foo"

    it "({field = 1}).field" do
      let e = Lua.table [Lua.tableRowNV f (Lua.integer 1)]
          f = [Lua.name|foo|]
      renderedExpression (Lua.varFieldExp e f) `shouldBe` "({ foo = 1 }).foo"

    it "Assignment" do
      let s = Lua.assignVar [Lua.name|foo|] (Lua.boolean True)
      renderedStatement s `shouldBe` "foo = true"

  describe "Local declaration" do
    it "without a value" do
      let s = Lua.local [Lua.name|foo|] Nothing
      renderedStatement s `shouldBe` "local foo"

    it "with value" do
      let s = Lua.local [Lua.name|foo|] (Just (Lua.boolean True))
      renderedStatement s `shouldBe` "local foo = true"

  describe "If Then Else" do
    it "if / then" do
      let p = Lua.boolean True
      let t = pure $ Lua.return $ Lua.integer 1
      let s = Lua.ifThenElse p t []
      renderedStatement s `shouldBe` "if true then return 1 end"
    it "if / then / else" do
      let p = Lua.boolean True
      let t = pure $ Lua.return $ Lua.integer 1
      let e = pure $ Lua.return $ Lua.integer 0
      let s = Lua.ifThenElse p t e
      renderedStatement s `shouldBe` "if true then return 1 else return 0 end"

  describe "Return" do
    it "statement" do
      let s = Lua.return $ Lua.boolean True
      renderedStatement s `shouldBe` "return true"

  describe "Table" do
    it "empty" do
      renderedExpression (Lua.table []) `shouldBe` "{}"

    it "small table constructor in one line" do
      let e =
            Lua.table
              [ Lua.tableRowKV (Lua.integer 42) (Lua.boolean True)
              , Lua.tableRowNV [Lua.name|foo|] (Lua.string "ok")
              ]
      renderedExpression e `shouldBe` "{ [42] = true, foo = \"ok\" }"

    it "large table constructor on muliple lines" do
      let e =
            Lua.table
              [ Lua.tableRowKV (Lua.integer 42) (Lua.boolean True)
              , Lua.tableRowNV [Lua.name|foo|] (Lua.string "bar")
              , Lua.tableRowNV [Lua.name|loooooooooooong1|] (Lua.string "value")
              , Lua.tableRowNV [Lua.name|loooooooooooong2|] (Lua.string "value")
              ]
      renderedExpression e
        `shouldBe` multiline
          [ "{"
          , "  [42] = true,"
          , "  foo = \"bar\","
          , "  loooooooooooong1 = \"value\","
          , "  loooooooooooong2 = \"value\""
          , "}"
          ]

  describe "function" do
    it "one-liner" do
      let params = Lua.paramNamed <$> [[Lua.name|a|], [Lua.name|b|]]
      let result = Lua.integer 1
      let stats = [Lua.assignVar [Lua.name|x|] Lua.nil]
      let expr = Lua.functionDef params (stats <> [Lua.return result])
      renderedExpression expr `shouldBe` "function(a, b) x = nil return 1 end"

    it "multi-liner" do
      let params = Lua.paramNamed <$> [[Lua.name|aaa|], [Lua.name|bbb|]]
      let result =
            Lua.varNameExp
              [Lua.name|aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa|]
      let stats = [Lua.assignVar [Lua.name|x|] Lua.nil]
      let expr = Lua.functionDef params (stats <> [Lua.return result])
      renderedExpression expr
        `shouldBe` multiline
          [ "function(aaa, bbb)"
          , "  x = nil"
          , "  return aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          , "end"
          ]
    it "function application" do
      let expr =
            Lua.functionCall
              ( Lua.functionDef
                  [Lua.paramNamed [Lua.name|a|], Lua.paramNamed [Lua.name|b|]]
                  [Lua.return (Lua.varNameExp [Lua.name|a|])]
              )
              [Lua.integer 1, Lua.integer 2]
      renderedExpression expr
        `shouldBe` "(function(a, b) return a end)(1, 2)"

  describe "expression" do
    describe "unary" do
      it "hash" do
        renderedExpression (Lua.hash (Lua.varNameExp [Lua.name|foo|]))
          `shouldBe` "#(foo)"

      it "negate" do
        renderedExpression (Lua.negate (Lua.varNameExp [Lua.name|foo|]))
          `shouldBe` "-(foo)"

      it "logicalNot" do
        renderedExpression (Lua.logicalNot (Lua.varNameExp [Lua.name|foo|]))
          `shouldBe` "not(foo)"

      it "bitwiseNot" do
        renderedExpression (Lua.bitwiseNot (Lua.varNameExp [Lua.name|foo|]))
          `shouldBe` "~(foo)"

    describe "binary" do
      it "Op with lower precedence is braced" do
        renderedExpression
          ((Lua.integer 2 `Lua.add` Lua.integer 3) `Lua.mul` Lua.integer 4)
          `shouldBe` "(2 + 3) * 4"

      it "Op with higher precedence is not braced" do
        renderedExpression
          (Lua.integer 2 `Lua.add` (Lua.integer 3 `Lua.mul` Lua.integer 4))
          `shouldBe` "2 + 3 * 4"

--------------------------------------------------------------------------------
-- Utility funtions ------------------------------------------------------------

multiline ∷ [Text] → Text
multiline = Text.concat . intersperse "\n"

renderedStatement ∷ Lua.Statement → Text
renderedStatement = rendered . Printer.printStatement

renderedExpression ∷ Lua.Exp → Text
renderedExpression = rendered . Printer.printedExp

rendered ∷ Doc ann → Text
rendered = renderStrict . layoutPretty defaultLayoutOptions
