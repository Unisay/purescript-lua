-- @inline inlineMe always
-- @inline inlineMeLambda always
module Golden.Annotations.M1 where

inlineMe :: Int -> Int
inlineMe 1 = 2
inlineMe x = x

foreign import dontInlineClosure :: Int -> Int
foreign import inlineMeLambda :: Int -> Int
