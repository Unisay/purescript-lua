module Golden.Newtype.Test where

newtype NT = NT { foo :: Int }

f :: NT -> Int
f (NT n) = n.foo

g :: { foo :: Int } -> NT
g = NT
