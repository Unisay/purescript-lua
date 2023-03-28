module Golden.TestCurrying where

apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

f :: Int -> Boolean -> Char -> Number -> String
f i b c d = "ok"
