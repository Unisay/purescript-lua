module Golden.Beta.Test (g) where

f :: Int -> Int
f 42 = 42
f _ = 1

g :: Int -> Int
g x = f x
