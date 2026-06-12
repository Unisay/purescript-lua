module Golden.BugListGenericEq.Test where

import Prelude
import Data.Eq.Generic as GEq
import Data.Generic.Rep as G
import Effect (Effect)
import Effect.Console (logShow)

data List a = Nil | Cons { head :: a, tail :: List a }

cons :: forall a. a -> List a -> List a
cons head tail = Cons { head, tail }

derive instance genericList :: G.Generic (List a) _

instance eqList :: Eq a => Eq (List a) where
  eq x y = GEq.genericEq x y

main :: Effect Unit
main = do
  logShow $ (Nil :: List Int) == Nil
  logShow $ cons 1 (cons 2 Nil) == cons 1 (cons 2 Nil)
  logShow $ cons 1 Nil == cons 2 Nil
