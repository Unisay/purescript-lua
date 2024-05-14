module Golden.Uncurrying.Test (call2, call3, call4, call5) where

uncurryFirst2Args :: Int -> Boolean -> Char -> Int
uncurryFirst2Args i _b _c = i

call2 :: Char -> Int
call2 = uncurryFirst2Args 1 true

call3 :: Int
call3 = uncurryFirst2Args 2 false 'a'

uncurryFirst4Args :: Int -> Int -> Int -> Int -> Int -> Int
uncurryFirst4Args i _j _k _l _m = i

call4 :: Int -> Int
call4 = uncurryFirst4Args 1 2 3 (synonym 4 5 6)

uncurryFirst3Args :: Int -> Int -> Int -> Int
uncurryFirst3Args _i _j k = k

synonym :: Int -> Int -> Int -> Int
synonym = uncurryFirst3Args

call5 :: Int -> Int
call5 i = synonym 1 i 3
