module Golden.Issue37.Test(baz) where

import Prelude 
import Effect (Effect)

baz :: Effect Unit
baz = bar (pure unit)

bar :: forall f. Monad f => f Unit -> f Unit
bar f = do
  f
  _ <- pure [ foo f ]
  pure unit

foo :: forall f. Monad f => f Unit -> f Unit
foo fn1 = do
  _ <- fn1
  fn1
  fn1
  fn1
