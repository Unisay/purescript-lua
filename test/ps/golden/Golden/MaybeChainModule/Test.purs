module Golden.MaybeChainModule.Test where

import Prelude

import Data.Maybe (Maybe(..), maybe)

-- Same expressions as Golden.MaybeChain.Test, but without `main` and
-- without an eval golden, so the golden suite compiles this module in
-- LinkAsModule mode: the mode in which issue #30 reported the crash.
chainedNothing :: Int
chainedNothing = maybe 0 identity (maybe Nothing Just (map (\x -> x) Nothing))

chainedJust :: Int
chainedJust = maybe 0 identity (maybe Nothing Just (map (\x -> x) (Just 42)))
