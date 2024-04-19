module Golden.Foreign.Test (foo, baz) where

import Golden.Foreign.Lib

foreign import foo :: Int
foreign import boo :: Int

baz :: Array Int
baz = [ boo, alive ]
