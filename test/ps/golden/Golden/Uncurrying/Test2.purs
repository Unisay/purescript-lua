module Golden.Uncurrying.Test2 (test) where

uncurried :: Int -> Int -> Boolean
uncurried 1 1 = true
uncurried _ _ = false

test :: { a :: Int, b :: Int } -> Int
test = case _ of
  { a } | uncurried a 0 -> a
  { b } | uncurried b 0 -> b
  _ -> 0
