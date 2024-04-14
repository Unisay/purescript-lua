{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Language.PureScript.Backend.Lua.Printer.Spec where

import Data.Text qualified as Text
import Language.PureScript.Backend.Lua.Name qualified as Lua
import Language.PureScript.Backend.Lua.Printer qualified as Printer
import Language.PureScript.Backend.Lua.Types (ParamF (..))
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Hspec (Spec, describe, it, shouldBe)

spec ∷ Spec
spec = do
  it "Name" do
    (rendered . Printer.printName) [Lua.name|foo|] `shouldBe` "foo"

  it "VarName" do
    renderedExpression (Lua.varName [Lua.name|foo|]) `shouldBe` "foo"

  describe "VarField" do
    it "var.field" do
      let e = Lua.varName [Lua.name|expr|]
          f = [Lua.name|foo|]
      renderedExpression (Lua.varField e f) `shouldBe` "expr.foo"

    it "({field = 1}).field" do
      let e = Lua.table [Lua.tableRowNV f (Lua.Integer 1)]
          f = [Lua.name|foo|]
      renderedExpression (Lua.varField e f) `shouldBe` "({ foo = 1 }).foo"

    it "Assignment" do
      let s = Lua.assign (Lua.VarName [Lua.name|foo|]) (Lua.Boolean True)
      renderedStatement s `shouldBe` "foo = true"

  describe "Local declaration" do
    it "without a value" do
      let s = Lua.Local [Lua.name|foo|] Nothing
      renderedStatement s `shouldBe` "local foo"

    it "with value" do
      let s = Lua.local [Lua.name|foo|] (Just (Lua.Boolean True))
      renderedStatement s `shouldBe` "local foo = true"

  describe "If Then Else" do
    it "if / then" do
      let p = Lua.Boolean True
      let t = pure $ Lua.return $ Lua.Integer 1
      let s = Lua.ifThenElse p t []
      renderedStatement s `shouldBe` "if true then return 1 end"
    it "if / then / else" do
      let p = Lua.Boolean True
      let t = pure $ Lua.return $ Lua.Integer 1
      let e = pure $ Lua.return $ Lua.Integer 0
      let s = Lua.ifThenElse p t e
      renderedStatement s `shouldBe` "if true then return 1 else return 0 end"

  describe "Return" do
    it "statement" do
      let s = Lua.return $ Lua.Boolean True
      renderedStatement s `shouldBe` "return true"

  describe "Table" do
    it "empty" do
      renderedExpression (Lua.table []) `shouldBe` "{}"

    it "small table constructor in one line" do
      let e =
            Lua.table
              [ Lua.tableRowKV (Lua.Integer 42) (Lua.Boolean True)
              , Lua.tableRowNV [Lua.name|foo|] (Lua.String "ok")
              ]
      renderedExpression e `shouldBe` "{ [42] = true, foo = \"ok\" }"

    it "large table constructor on muliple lines" do
      let e =
            Lua.table
              [ Lua.tableRowKV (Lua.Integer 42) (Lua.Boolean True)
              , Lua.tableRowNV [Lua.name|foo|] (Lua.String "bar")
              , Lua.tableRowNV [Lua.name|loooooooooooong1|] (Lua.String "value")
              , Lua.tableRowNV [Lua.name|loooooooooooong2|] (Lua.String "value")
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
      let params = ParamNamed <$> [[Lua.name|a|], [Lua.name|b|]]
      let result = Lua.Integer 1
      let stats = [Lua.assign (Lua.VarName [Lua.name|x|]) Lua.Nil]
      let expr = Lua.functionDef params (stats <> [Lua.return result])
      renderedExpression expr `shouldBe` "function(a, b) x = nil return 1 end"

    it "multi-liner" do
      let params = ParamNamed <$> [[Lua.name|aaa|], [Lua.name|bbb|]]
      let result =
            Lua.varName
              [Lua.name|aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa|]
      let stats = [Lua.assign (Lua.VarName [Lua.name|x|]) Lua.Nil]
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
                  [ParamNamed [Lua.name|a|], ParamNamed [Lua.name|b|]]
                  [Lua.return (Lua.varName [Lua.name|a|])]
              )
              [Lua.Integer 1, Lua.Integer 2]
      renderedExpression expr
        `shouldBe` "(function(a, b) return a end)(1, 2)"

  describe "expression" do
    describe "unary" do
      it "hash" do
        renderedExpression (Lua.hash (Lua.varName [Lua.name|foo|]))
          `shouldBe` "#(foo)"

      it "negate" do
        renderedExpression (Lua.negate (Lua.varName [Lua.name|foo|]))
          `shouldBe` "-(foo)"

      it "logicalNot" do
        renderedExpression (Lua.logicalNot (Lua.varName [Lua.name|foo|]))
          `shouldBe` "not(foo)"

      it "bitwiseNot" do
        renderedExpression (Lua.bitwiseNot (Lua.varName [Lua.name|foo|]))
          `shouldBe` "~(foo)"

    describe "binary" do
      it "Op with lower precedence is braced" do
        renderedExpression
          ((Lua.Integer 2 `Lua.add` Lua.Integer 3) `Lua.mul` Lua.Integer 4)
          `shouldBe` "(2 + 3) * 4"

      it "Op with higher precedence is not braced" do
        renderedExpression
          (Lua.Integer 2 `Lua.add` (Lua.Integer 3 `Lua.mul` Lua.Integer 4))
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
