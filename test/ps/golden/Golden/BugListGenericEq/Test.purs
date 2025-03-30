module Golden.BugListGenericEq.Test where

import Prelude
import Data.Generic.Rep as G
import Data.Eq.Generic as GEq

data List a = Nil | Cons { head :: a, tail :: List a }

cons :: forall a. a -> List a -> List a
cons head tail = Cons { head, tail }

derive instance genericList :: G.Generic (List a) _

instance eqList :: Eq a => Eq (List a) where
  eq x y = GEq.genericEq x y
