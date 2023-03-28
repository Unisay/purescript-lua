module Golden.TestDataDeclarations1 where

data Void
data Unit = U
data TProduct = P3 Int Boolean String
data TProductWithFields = PF { ii :: Int, bb :: Boolean, ss :: String }
data TSum = S0 | S1 Char | S2 Int Boolean
data Rec = Nop | More Rec
data TySameName = CtorSameName
