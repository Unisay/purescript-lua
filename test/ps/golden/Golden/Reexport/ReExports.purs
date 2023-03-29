module Golden.Reexport.ReExports
  ( module Reexported
  , binding2
  ) where

import Golden.Reexport.Exports (binding1) as Reexported

binding2 :: Int
binding2 = 2
