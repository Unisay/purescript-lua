module Golden.TestUnbinding where

a = 1
b = 2
f _ _ = 3
c = f a (f b a)
