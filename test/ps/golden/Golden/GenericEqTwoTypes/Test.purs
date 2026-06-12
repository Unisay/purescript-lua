module Golden.GenericEqTwoTypes.Test where

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

data Tree a = Leaf | Node { left :: Tree a, value :: a, right :: Tree a }

node :: forall a. Tree a -> a -> Tree a -> Tree a
node left value right = Node { left, value, right }

derive instance genericTree :: G.Generic (Tree a) _

instance eqTree :: Eq a => Eq (Tree a) where
  eq x y = GEq.genericEq x y

main :: Effect Unit
main = do
  logShow $ cons 1 (cons 2 Nil) == cons 1 (cons 2 Nil)
  logShow $ cons 1 Nil == cons 2 Nil
  logShow $ node Leaf 1 (node Leaf 2 Leaf) == node Leaf 1 (node Leaf 2 Leaf)
  logShow $ node Leaf 1 Leaf == node Leaf 2 Leaf
