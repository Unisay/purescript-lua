module Golden.Annotations.M2 where

import Golden.Annotations.M1 (inlineMe)

inlineIntoMe :: Int -> Int
inlineIntoMe i = inlineMe (inlineMe (inlineMe i))
