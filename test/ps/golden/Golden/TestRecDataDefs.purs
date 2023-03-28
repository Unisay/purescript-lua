module Golden.TestRecDataDefs where

data A = A | AB B
data B = B | BA A

a :: A
a = A

b :: B
b = B

ab :: A
ab = AB b

ba :: B
ba = BA ab
