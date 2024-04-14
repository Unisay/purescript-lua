module Golden.Values.Test where

i :: Int
i = 1

b :: Boolean
b = true

c :: Char
c = 'c'

a :: Array Int
a = [ 1, 2, 3 ]

o :: { i :: Int, b :: Boolean, c :: Char }
o = { i, b, c }

f :: Int -> Boolean
f _ = true

