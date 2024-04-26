module Golden.Fibonacci.Test where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: Effect Unit
main = logShow $ fib 32 -- 2178309
