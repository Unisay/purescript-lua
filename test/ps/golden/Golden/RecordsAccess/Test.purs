module Golden.RecordsAccess.Test where

type R = { x :: Int, y :: Boolean }

r :: R
r = { x: 1, y: true }

test1 :: Int
test1 = r.x

test2 :: R -> Int
test2 = _.x

test3 :: R -> Int
test3 { x } = x

test4 :: R -> Int
test4 v = case v of
  { x } -> x
