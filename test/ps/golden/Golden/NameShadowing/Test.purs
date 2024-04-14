module Golden.NameShadowing.Test (b, c) where

a :: Int -> Int
a x = f x  1

b :: Int -> Int -> Int
b x x1 = f (f x x1) (a 42)

c ∷ Int -> Int -> Int
c = \x -> (\y -> \x -> f x y) x

f :: Int -> Int -> Int
f 1 _ = 1
f _ 1 = 2
f _ _ = 3
