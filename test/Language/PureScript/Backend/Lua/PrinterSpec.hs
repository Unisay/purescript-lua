{-# LANGUAGE QuasiQuotes #-}

module Language.PureScript.Backend.Lua.PrinterSpec where

import Data.Text qualified as Text
import Language.PureScript.Backend.Lua.Name qualified as Lua
import Language.PureScript.Backend.Lua.Printer qualified as Printer
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Var" do
    let printQname = rendered . Printer.printQualifiedName
    it "local" do
      printQname (Lua.LocalName [Lua.name|foo|]) `shouldBe` "foo"

    it "imported" do
      let modname = Lua.ModuleName [Lua.name|Acme|]
      printQname (Lua.ImportedName modname [Lua.name|foo|])
        `shouldBe` "Acme.foo"

  describe "Assignment" do
    it "singular" do
      let s =
            Lua.assign1
              (Lua.VarName (Lua.LocalName [Lua.name|foo|]))
              (Lua.Boolean True)
      renderedStatement s `shouldBe` "foo = true"

    it "plural" do
      let k =
            Lua.VarName (Lua.LocalName [Lua.name|foo|])
              :| [ Lua.VarField
                    (Lua.varName [Lua.name|a|])
                    [Lua.name|b|]
                 , Lua.VarIndex
                    (Lua.varName [Lua.name|c|])
                    (Lua.varName [Lua.name|d|])
                 ]
          v = Lua.Boolean True :| [Lua.Integer 1]
          s = Lua.Assign k v
      renderedStatement s `shouldBe` "foo, a.b, c[d] = true, 1"

  describe "Local declaration" do
    it "without a value" do
      let s = Lua.Local namelist explist
          namelist :: NonEmpty Lua.Name = [Lua.name|foo|] :| [[Lua.name|bar|]]
          explist :: [Lua.Exp] = []
      renderedStatement s `shouldBe` "local foo, bar"

    it "with value" do
      let s = Lua.Local namelist explist
          namelist :: NonEmpty Lua.Name =
            [Lua.name|foo|] :| [[Lua.name|bar|], [Lua.name|baz|]]
          explist :: [Lua.Exp] = [Lua.Integer 42, Lua.Boolean True]
      renderedStatement s
        `shouldBe` "local foo, bar, baz = 42, true"

  describe "If Then Else" do
    it "if / then" do
      let p = Lua.Boolean True
      let t = pure $ Lua.Return $ Lua.Integer 1
      let s = Lua.IfThenElse p t [] Nothing
      renderedStatement s `shouldBe` "if true then return 1 end"
    it "if / then / else" do
      let p = Lua.Boolean True
      let t = pure $ Lua.Return $ Lua.Integer 1
      let e = pure $ Lua.Return $ Lua.Integer 0
      let s = Lua.IfThenElse p t [] (Just e)
      renderedStatement s `shouldBe` "if true then return 1 else return 0 end"
    it "if / then / elseif / else" do
      let p = Lua.Boolean True
      let t = pure $ Lua.Return $ Lua.varName [Lua.name|tttttt|]
      let i =
            [
              ( Lua.Boolean False
              , pure $ Lua.Return $ Lua.varName [Lua.name|iiiiii|]
              )
            ]
      let e = pure $ Lua.Return $ Lua.varName [Lua.name|eeeeee|]
      let s = Lua.IfThenElse p t i (Just e)
      renderedStatement s
        `shouldBe` multiline
          [ "if true then"
          , "  return tttttt"
          , "elseif false then"
          , "  return iiiiii"
          , "else"
          , "  return eeeeee"
          , "end"
          ]

  describe "Return" do
    it "statement" do
      let s = Lua.Return $ Lua.Boolean True
      renderedStatement s `shouldBe` "return true"

  describe "Table" do
    it "empty" do
      renderedExpression (Lua.table []) `shouldBe` "{}"

    it "small table constructor in one line" do
      let e =
            Lua.table
              [ Lua.TableRowKV (Lua.Integer 42) (Lua.Boolean True)
              , Lua.TableRowNV [Lua.name|foo|] (Lua.String "ok")
              , Lua.TableRowV (Lua.Float 42.0)
              ]
      renderedExpression e `shouldBe` "{ [42] = true, foo = \"ok\", 42.0 }"

    it "large table constructor on muliple lines" do
      let e =
            Lua.table
              [ Lua.TableRowKV (Lua.Integer 42) (Lua.Boolean True)
              , Lua.TableRowNV [Lua.name|foo|] (Lua.String "bar")
              , Lua.TableRowV (Lua.Float 4242424242.0)
              , Lua.TableRowNV [Lua.name|loooooooooooong1|] (Lua.String "value")
              , Lua.TableRowNV [Lua.name|loooooooooooong2|] (Lua.String "value")
              ]
      renderedExpression e
        `shouldBe` multiline
          [ "{"
          , "  [42] = true,"
          , "  foo = \"bar\","
          , "  4.242424242e9,"
          , "  loooooooooooong1 = \"value\","
          , "  loooooooooooong2 = \"value\""
          , "}"
          ]

  describe "function" do
    it "one-liner" do
      let params = [[Lua.name|a|], [Lua.name|b|]]
      let result = Lua.Integer 1
      let stats =
            [ Lua.assign1 (Lua.VarName (Lua.LocalName [Lua.name|x|])) Lua.Nil
            ]
      let expr = Lua.Function params (stats <> [Lua.Return result])
      renderedExpression expr `shouldBe` "function(a, b) x = nil return 1 end"

    it "multi-liner" do
      let params = [[Lua.name|aaa|], [Lua.name|bbb|]]
      let result =
            Lua.varName
              [Lua.name|aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa|]
      let stats =
            [ Lua.assign1
                (Lua.VarName (Lua.LocalName [Lua.name|x|]))
                Lua.Nil
            ]
      let expr = Lua.Function params (stats <> [Lua.Return result])
      renderedExpression expr
        `shouldBe` multiline
          [ "function(aaa, bbb)"
          , "  x = nil"
          , "  return aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          , "end"
          ]

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

multiline :: [Text] -> Text
multiline = Text.concat . intersperse "\n"

renderedStatement :: Lua.Statement -> Text
renderedStatement = rendered . Printer.printStatement

renderedExpression :: Lua.Exp -> Text
renderedExpression = rendered . Printer.printedExp

rendered :: Doc ann -> Text
rendered = renderStrict . layoutPretty defaultLayoutOptions
