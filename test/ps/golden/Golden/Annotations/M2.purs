module Golden.Annotations.M2 where

import Golden.Annotations.M1 (dontInlineClosure, inlineMe, inlineMeLambda)

inlineIntoMe :: Int -> Int
inlineIntoMe i = inlineMe (inlineMe (inlineMe i))

inlineIntoMe2 :: Int
inlineIntoMe2 = dontInlineClosure(inlineMeLambda (inlineIntoMe3 17))
  where
    inlineIntoMe3 :: Int -> Int
    inlineIntoMe3 = inlineMeLambda
