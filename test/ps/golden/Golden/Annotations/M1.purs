-- @inline inlineMe always
module Golden.Annotations.M1 where

inlineMe :: Int -> Int
inlineMe 1 = 2
inlineMe x = x
