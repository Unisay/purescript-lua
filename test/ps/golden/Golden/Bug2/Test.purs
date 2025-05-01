module Golden.Bug2.Test (main) where

import Prelude (Unit, pure, map)

import Data.Maybe (Maybe(Just, Nothing), maybe)
import Effect (Effect)
import Effect.Exception (throw)

main :: Effect Unit
main =
  maybe
    (throw "Some error")
    pure
    (maybe Nothing Just (map (\x -> x) Nothing))
