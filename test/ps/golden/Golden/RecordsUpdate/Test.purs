module Golden.RecordsUpdate.Test where

type R = { x :: Int, y :: Boolean, z :: Z }
type Z = { z :: String , p :: Char }

r :: R
r = { x: 1, y: true, z: { z: "foo", p: 'a' } }

test1 :: R
test1 = r { x = 2 }

test2 :: R -> R
test2 = _ { y = false }

test3 :: R -> R
test3 = _ { z { p = 'b' } }

type Poly r = { x :: Int, y :: Char | r }

test4 :: forall r. Poly r -> Poly r
test4 = _ { x = 1 }
