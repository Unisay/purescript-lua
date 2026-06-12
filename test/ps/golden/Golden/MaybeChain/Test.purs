module Golden.MaybeChain.Test where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Console (logShow)

-- Repro for issue #30: nested maybe/map chains used to make the
-- (since removed) currying optimizer crash with
-- "Impossible subexpressions: IfThenElse".
main :: Effect Unit
main = do
  logShow $ maybe 0 identity (maybe Nothing Just (map (\x -> x) Nothing))
  logShow $ maybe 0 identity (maybe Nothing Just (map (\x -> x) (Just 42)))
