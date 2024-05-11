{-# LANGUAGE QuasiQuotes #-}

module Language.PureScript.Backend.Lua.DCE.Spec where

import Control.Monad.Accum (add)
import Control.Monad.Trans.Accum (Accum, execAccum)
import Hedgehog (annotateShow, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.PureScript.Backend.Lua.DCE
  ( DceAnn (..)
  , DceMode (PreserveReturned)
  , MonadScopes (..)
  )
import Language.PureScript.Backend.Lua.DCE qualified as DCE
import Language.PureScript.Backend.Lua.Fixture qualified as Fixture
import Language.PureScript.Backend.Lua.Gen qualified as Gen
import Language.PureScript.Backend.Lua.Name qualified as Lua
import Language.PureScript.Backend.Lua.Types qualified as Lua
import Test.Hspec (Spec, describe)
import Test.Hspec.Hedgehog.Extended (test)

spec ∷ Spec
spec = describe "Lua Dead Code Elimination" do
  test "Doesn't eliminate unused function parameter" do
    name1 ← forAll Gen.name
    name2 ← forAll $ mfilter (/= name1) Gen.name
    expr1 ← forAll Gen.nonRecursiveExpression
    expr2 ← forAll $ mfilter (/= expr1) Gen.nonRecursiveExpression

    let chunk =
          [ Lua.local name1 . Just $
              Lua.functionDef [Lua.paramNamed name2] [Lua.return expr1]
          , Lua.return $ Lua.functionCall (Lua.varNameExp name1) [expr2]
          ]
    let chunk' =
          [ Lua.local name1 . Just $
              Lua.functionDef [Lua.paramUnused] [Lua.return expr1]
          , Lua.return $ Lua.functionCall (Lua.varNameExp name1) [expr2]
          ]
    DCE.eliminateDeadCode PreserveReturned chunk === chunk'

  test "Eliminates unused local binding" do
    [usedLocal@(Lua.Local _ann name _val), unusedLocal1, unusedLocal2] ←
      forAll . fmap toList $ Gen.set (Range.singleton 3) Gen.local
    let fnCall ∷ Lua.Exp = Lua.functionCall (Lua.varNameExp name) []
    let chunk = [unusedLocal1, usedLocal, unusedLocal2, Lua.return fnCall]
    annotateShow chunk
    DCE.eliminateDeadCode PreserveReturned chunk
      === [usedLocal, Lua.return fnCall]

  test "Eliminates unused local binding inside a function" do
    [usedLocal@(Lua.Local _ann name _val), unusedLocal1, unusedLocal2] ←
      forAll . fmap toList $ Gen.set (Range.singleton 3) Gen.local
    let fnCall ∷ Lua.Exp = Lua.functionCall (Lua.varNameExp name) []
    let body = [unusedLocal1, usedLocal, unusedLocal2, Lua.return fnCall]
        body' = [usedLocal, Lua.return fnCall]
    let chunk =
          [Lua.return $ Lua.functionDef [Lua.paramNamed [Lua.name|unusedArg|]] body]
        chunk' = [Lua.return $ Lua.functionDef [Lua.paramUnused] body']
    annotateShow chunk
    DCE.eliminateDeadCode PreserveReturned chunk === chunk'

  test "Doesn't eliminate local binding used transitively" do
    name0 ← forAll Gen.name
    localDef@(Lua.Local _ann name1 _val) ← forAll Gen.local
    let retCall = Lua.return (Lua.functionCall (Lua.varNameExp name0) [])
        chunk =
          [ localDef
          , Lua.local name0 (Just (Lua.varNameExp name1))
          , retCall
          ]
    annotateShow chunk
    DCE.eliminateDeadCode PreserveReturned chunk === chunk

  test "Eliminates unused assign statement" do
    localDef@(Lua.Local _ann name _val) ← forAll Gen.local
    name_ ← forAll $ mfilter (/= name) Gen.name
    value_ ← forAll Gen.expression
    let retCall = Lua.return (Lua.functionCall (Lua.varNameExp name) [])
    let chunk =
          [ localDef
          , Lua.local name_ Nothing
          , Lua.assignVar name_ value_
          , retCall
          ]
    annotateShow chunk
    DCE.eliminateDeadCode PreserveReturned chunk === [localDef, retCall]

  test "Doesn't eliminate used assign statement" do
    name ← forAll Gen.name
    value_ ← forAll Gen.expression
    let retCall = Lua.return (Lua.functionCall (Lua.varNameExp name) [])
    let chunk =
          [ Lua.local name Nothing
          , Lua.assignVar name value_
          , retCall
          ]
    annotateShow chunk
    DCE.eliminateDeadCode PreserveReturned chunk === chunk

  test "Doesn't eliminate anything from runtimeLazy" do
    let name = Fixture.runtimeLazyName
    let chunk =
          [ Fixture.runtimeLazy
          , Lua.return (Lua.functionCall (Lua.varNameExp name) [])
          ]
    DCE.eliminateDeadCode PreserveReturned chunk === chunk

  test "findAssignments" do
    let name = [Lua.name|a|]
    let chunk =
          [ Lua.Local
              (DceAnn Lua.newAnn 1 [])
              name
              (Just (Lua.Integer (DceAnn Lua.newAnn 11 []) 11))
          , Lua.Assign
              (DceAnn Lua.newAnn 2 [])
              (Lua.VarName (DceAnn Lua.newAnn 20 []) name)
              (Lua.Integer (DceAnn Lua.newAnn 21 []) 2)
          , Lua.Return (DceAnn Lua.newAnn 3 []) $
              Lua.FunctionCall
                (DceAnn Lua.newAnn 3 [])
                ( Lua.Var
                    (DceAnn Lua.newAnn 31 [])
                    (Lua.VarName (DceAnn Lua.newAnn 32 []) name)
                )
                []
          ]
    DCE.findAssignments name chunk === pure 2

  test "scopes" do
    name ← forAll Gen.name
    let chunk =
          [ Lua.local1 name $
              Lua.functionDef
                []
                [ Lua.ifThenElse
                    (Lua.integer 100 `Lua.equalTo` Lua.integer 0)
                    [Lua.return (Lua.integer 1)]
                    [Lua.return (Lua.integer 2)]
                ]
          , Lua.return (Lua.functionCall (Lua.varNameExp name) [])
          ]
    annotateShow $ scopeAssignmentTraces chunk
    DCE.eliminateDeadCode PreserveReturned chunk === chunk

  test "Adds/removes scopes correctly" do
    let n1 = [Lua.name|a|]
        chunk ∷ [Lua.Statement] =
          [ Lua.local1 n1 $
              Lua.functionDef
                []
                [ Lua.ifThenElse
                    (Lua.integer 100 `Lua.equalTo` Lua.integer 0)
                    [Lua.return (Lua.integer 1)]
                    [Lua.return (Lua.integer 2)]
                ]
          , Lua.return (Lua.functionCall (Lua.varNameExp n1) [])
          ]

    scopeAssignmentTraces chunk
      === [ AddName n1 0 (fromList [(n1, 0)] :| [])
          , AddScope (mempty :| [fromList [(n1, 0)]])
          , AddScope (mempty :| [mempty, fromList [(n1, 0)]])
          , AddScope (mempty :| [mempty, mempty, fromList [(n1, 0)]])
          , DropScope [mempty, mempty, fromList [(n1, 0)]]
          , DropScope [mempty, fromList [(n1, 0)]]
          , DropScope [fromList [(n1, 0)]]
          ]

scopeAssignmentTraces ∷ [Lua.Statement] → [Trace]
scopeAssignmentTraces =
  traverse DCE.assignKeys
    >>> (`evalState` 0)
    >>> DCE.assignScopes
    >>> unTraceScopes
    >>> (`execStateT` [])
    >>> (`execAccum` [])

newtype TraceScopes a = TraceScopes
  { unTraceScopes ∷ StateT [DCE.Scope] (Accum [Trace]) a
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
    scopes ← addName name key
    add [AddName name key scopes]
    pure scopes

  addScope = TraceScopes do
    scopes ← addScope
    add [AddScope scopes]
    pure scopes

  dropScope = TraceScopes do
    getScopes >>= \case
      [] → add [DropScopeError]
      _ : remainingScopes → do
        put remainingScopes
        add [DropScope remainingScopes]

  getScopes = TraceScopes getScopes
