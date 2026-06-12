module Golden.DerivedFunctor.Test where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)

data Either a b = Left a | Right b

derive instance functorEither :: Functor (Either a)

data Tree a = Leaf | Node (Tree a) a (Tree a)

derive instance functorTree :: Functor Tree

fromRight :: forall a. Int -> Either a Int -> Int
fromRight fallback = case _ of
  Left _ -> fallback
  Right n -> n

sumTree :: Tree Int -> Int
sumTree = case _ of
  Leaf -> 0
  Node l x r -> sumTree l + x + sumTree r

main :: Effect Unit
main = do
  logShow $ fromRight 0 $ map (_ + 1) (Right 41 :: Either String Int)
  logShow $ fromRight 7 $ map (_ + 1) (Left "no" :: Either String Int)
  logShow $ sumTree $ map (_ * 2)
    $ Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)
