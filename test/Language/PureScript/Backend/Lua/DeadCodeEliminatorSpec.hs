{-# LANGUAGE QuasiQuotes #-}

module Language.PureScript.Backend.Lua.DeadCodeEliminatorSpec where

import Control.Monad.Accum (add)
import Control.Monad.Trans.Accum (Accum, execAccum)
import Hedgehog (annotateShow, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.PureScript.Backend.Lua.DeadCodeEliminator
  ( DceMode (PreserveReturned)
  , MonadScopes (..)
  )
import Language.PureScript.Backend.Lua.DeadCodeEliminator qualified as DCE
import Language.PureScript.Backend.Lua.Fixture qualified as Fixture
import Language.PureScript.Backend.Lua.Gen qualified as Gen
import Language.PureScript.Backend.Lua.Name qualified as Lua
import Language.PureScript.Backend.Lua.Types (ParamF (..))
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Test.Hspec (Spec, describe)
import Test.Hspec.Hedgehog.Extended (test)

spec :: Spec
spec = describe "Lua Dead Code Elimination" do
  test "Doesn't eliminate unused function parameter" do
    name1 <- forAll Gen.name
    name2 <- forAll $ mfilter (/= name1) Gen.name
    expr1 <- forAll Gen.nonRecursiveExpression
    expr2 <- forAll $ mfilter (/= expr1) Gen.nonRecursiveExpression

    let chunk =
          [ Lua.local name1 . Just $
              Lua.functionDef [ParamNamed name2] [Lua.return expr1]
          , Lua.return $ Lua.functionCall (Lua.varName name1) [expr2]
          ]
    let chunk' =
          [ Lua.local name1 . Just $
              Lua.functionDef [ParamUnused] [Lua.return expr1]
          , Lua.return $ Lua.functionCall (Lua.varName name1) [expr2]
          ]
    DCE.eliminateDeadCode PreserveReturned chunk === chunk'

  test "Eliminates unused local binding" do
    [usedLocal@(Lua.Local name _val), unusedLocal1, unusedLocal2] <-
      forAll . fmap toList $ Gen.set (Range.singleton 3) Gen.local
    let fnCall :: Lua.Exp = Lua.functionCall (Lua.varName name) []
    let chunk = [unusedLocal1, usedLocal, unusedLocal2, Lua.return fnCall]
    annotateShow chunk
    DCE.eliminateDeadCode PreserveReturned chunk
      === [usedLocal, Lua.return fnCall]

  test "Eliminates unused local binding inside a function" do
    [usedLocal@(Lua.Local name _val), unusedLocal1, unusedLocal2] <-
      forAll . fmap toList $ Gen.set (Range.singleton 3) Gen.local
    let fnCall :: Lua.Exp = Lua.functionCall (Lua.varName name) []
    let body = [unusedLocal1, usedLocal, unusedLocal2, Lua.return fnCall]
        body' = [usedLocal, Lua.return fnCall]
    let chunk =
          [Lua.return $ Lua.functionDef [ParamNamed [Lua.name|unusedArg|]] body]
        chunk' = [Lua.return $ Lua.functionDef [ParamUnused] body']
    annotateShow chunk
    DCE.eliminateDeadCode PreserveReturned chunk === chunk'

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
    DCE.eliminateDeadCode PreserveReturned chunk === chunk

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
    DCE.eliminateDeadCode PreserveReturned chunk === [localDef, retCall]

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
    DCE.eliminateDeadCode PreserveReturned chunk === chunk

  test "Doesn't eliminate anything from runtimeLazy" do
    let name = [Lua.name|_S___runtime_lazy|]
    let chunk =
          [ Fixture.runtimeLazy
          , Lua.return (Lua.functionCall (Lua.varName name) [])
          ]
    DCE.eliminateDeadCode PreserveReturned chunk === chunk

  test "scopes" do
    let name = [Lua.name|_S___runtime_lazy|]
    let chunk =
          [ Lua.local1 name $
              Lua.Function
                []
                [ Lua.ann $
                    Lua.ifThenElse
                      (Lua.Integer 100 `Lua.equalTo` Lua.Integer 0)
                      [Lua.Return ((), Lua.Integer 1)]
                      [Lua.Return ((), Lua.Integer 2)]
                ]
          , Lua.return (Lua.functionCall (Lua.varName name) [])
          ]
    DCE.eliminateDeadCode PreserveReturned chunk === chunk

  test "Adds/removes scopes correctly" do
    let n1 = [Lua.name|a|]
        chunk :: [Lua.Statement]
        chunk =
          [ Lua.local1 n1 $
              Lua.Function
                []
                [ Lua.ann $
                    Lua.ifThenElse
                      (Lua.Integer 100 `Lua.equalTo` Lua.Integer 0)
                      [Lua.Return ((), Lua.Integer 1)]
                      [Lua.Return ((), Lua.Integer 2)]
                ]
          , Lua.return (Lua.functionCall (Lua.varName n1) [])
          ]

    scopeAssignmentTraces chunk
      === [ AddName n1 9 (fromList [(n1, 9)] :| [])
          , AddScope (mempty :| [fromList [(n1, 9)]])
          , AddScope (mempty :| [mempty, fromList [(n1, 9)]])
          , AddScope (mempty :| [mempty, mempty, fromList [(n1, 9)]])
          , DropScope [mempty, mempty, fromList [(n1, 9)]]
          , DropScope [mempty, fromList [(n1, 9)]]
          , DropScope [fromList [(n1, 9)]]
          ]

scopeAssignmentTraces :: [Lua.Statement] -> [Trace]
scopeAssignmentTraces =
  traverse DCE.assignKeys
    >>> (`evalState` 0)
    >>> DCE.assignScopes
    >>> unTraceScopes
    >>> (`execStateT` [])
    >>> (`execAccum` [])

newtype TraceScopes a = TraceScopes
  { unTraceScopes :: StateT [DCE.Scope] (Accum [Trace]) a
  }
  deriving newtype (Functor, Applicative, Monad)

data Trace
  = AddName Lua.Name DCE.Key (NonEmpty DCE.Scope)
  | AddScope (NonEmpty DCE.Scope)
  | DropScope [DCE.Scope]
  | DropScopeError
  deriving stock (Eq, Show)

instance MonadScopes TraceScopes where
  addName name key = TraceScopes do
    scopes <- addName name key
    add [AddName name key scopes]
    pure scopes

  addScope = TraceScopes do
    scopes <- addScope
    add [AddScope scopes]
    pure scopes

  dropScope = TraceScopes do
    getScopes >>= \case
      [] -> add [DropScopeError]
      _ : remainingScopes -> do
        put remainingScopes
        add [DropScope remainingScopes]

  getScopes = TraceScopes getScopes
