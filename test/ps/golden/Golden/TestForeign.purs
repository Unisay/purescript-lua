module Golden.TestForeign (bar) where

data FBool

foreign import foo :: Int
foreign import bar :: Int -> Int
foreign import baz :: FBool
