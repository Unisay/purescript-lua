module Golden.ArrayOfUnits.Test where

import Prelude (Unit, discard, unit)

import Effect (Effect)
import Effect.Console (logShow)
import Data.Traversable (traverse_)
import Data.Foldable (length)

main :: Effect Unit
main = do
  let arr :: Array Unit
      arr = [unit, unit, unit]
  traverse_ logShow arr
  let len :: Int
      len = length arr
  logShow len
