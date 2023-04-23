{-# LANGUAGE QuasiQuotes #-}

module Language.PureScript.Backend.Lua.DeadCodeEliminatorSpec where

import Hedgehog (annotateShow, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.PureScript.Backend.Lua.DeadCodeEliminator
  ( DceMode (PreserveReturned)
  , eliminateDeadCode
  )
import Language.PureScript.Backend.Lua.Fixture qualified as Fixture
import Language.PureScript.Backend.Lua.Gen qualified as Gen
import Language.PureScript.Backend.Lua.Name qualified as Lua
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Test.Hspec (Spec, describe)
import Test.Hspec.Hedgehog.Extended (test)

spec :: Spec
spec = describe "Lua Dead Code Elimination" do
  test "Eliminates unused local binding" do
    [usedLocal@(Lua.Local name _val), unusedLocal1, unusedLocal2] <-
      forAll . fmap toList $ Gen.set (Range.singleton 3) Gen.local
    let fnCall :: Lua.Exp = Lua.functionCall (Lua.varName name) []
    let chunk = [unusedLocal1, usedLocal, unusedLocal2, Lua.return fnCall]
    annotateShow chunk
    eliminateDeadCode PreserveReturned chunk === [usedLocal, Lua.return fnCall]

  test "Eliminates unused local binding inside a function" do
    [usedLocal@(Lua.Local name _val), unusedLocal1, unusedLocal2] <-
      forAll . fmap toList $ Gen.set (Range.singleton 3) Gen.local
    let fnCall :: Lua.Exp = Lua.functionCall (Lua.varName name) []
    let body = [unusedLocal1, usedLocal, unusedLocal2, Lua.return fnCall]
        body' = [usedLocal, Lua.return fnCall]
    let chunk = [Lua.return $ Lua.functionDef [[Lua.name|unusedArg|]] body]
        chunk' = [Lua.return $ Lua.functionDef [[Lua.name|unusedArg|]] body']
    annotateShow chunk
    eliminateDeadCode PreserveReturned chunk === chunk'

  test "Doesn't eliminate local binding used transitively" do
    name0 <- forAll Gen.name
    localDef@(Lua.Local name1 _val) <- forAll Gen.local
    let retCall = Lua.return (Lua.functionCall (Lua.varName name0) [])
        chunk =
          [ localDef
          , Lua.local name0 (Just (Lua.varName name1))
          , retCall
          ]
    annotateShow chunk
    eliminateDeadCode PreserveReturned chunk === chunk

  test "Eliminates unused assign statement" do
    localDef@(Lua.Local name _val) <- forAll Gen.local
    name_ <- forAll $ mfilter (/= name) Gen.name
    value_ <- forAll Gen.expression
    let retCall = Lua.return (Lua.functionCall (Lua.varName name) [])
    let chunk =
          [ localDef
          , Lua.local name_ Nothing
          , Lua.assign (Lua.VarName name_) value_
          , retCall
          ]
    annotateShow chunk
    eliminateDeadCode PreserveReturned chunk === [localDef, retCall]

  test "Doesn't eliminate used assign statement" do
    name <- forAll Gen.name
    value_ <- forAll Gen.expression
    let retCall = Lua.return (Lua.functionCall (Lua.varName name) [])
    let chunk =
          [ Lua.Local name Nothing
          , Lua.assign (Lua.VarName name) value_
          , retCall
          ]
    annotateShow chunk
    eliminateDeadCode PreserveReturned chunk === chunk

  test "Doesn't eliminate anything from runtimeLazy" do
    let name = [Lua.name|_S___runtime_lazy|]
    let chunk =
          [ Fixture.runtimeLazy
          , Lua.return (Lua.functionCall (Lua.varName name) [])
          ]
    eliminateDeadCode PreserveReturned chunk === chunk
