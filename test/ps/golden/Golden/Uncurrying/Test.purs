module Golden.Uncurrying.Test where

f :: Int -> Boolean -> Char -> Int
f i _b _c = i

call2 :: Char -> Int
call2 = f 1 true

call3 :: Int
call3 = f 2 false 'a'

