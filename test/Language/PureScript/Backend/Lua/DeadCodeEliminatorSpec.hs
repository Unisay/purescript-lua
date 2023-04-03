module Language.PureScript.Backend.Lua.DeadCodeEliminatorSpec where

import Data.List.NonEmpty qualified as NE
import Hedgehog (annotateShow, forAll, (===))
import Language.PureScript.Backend.Lua.DeadCodeEliminator
  ( DceEntry (..)
  , eliminateDeadCode
  )
import Language.PureScript.Backend.Lua.Gen qualified as Gen
import Language.PureScript.Backend.Lua.Types (StatementF (..))
import Test.Hspec (Spec, describe)
import Test.Hspec.Hedgehog.Extended (test)

spec :: Spec
spec = describe "Lua Dead Code Elimination" do
  test "Eliminates everything not reachable from a local definition" do
    localDef@(Local names _vals) <- forAll Gen.local
    randomChunk <- forAll Gen.chunk
    let chunk = localDef : randomChunk
    let entries = NE.singleton (DceEntryLocalDef (head names))
    annotateShow entries
    eliminateDeadCode entries chunk === [localDef]
