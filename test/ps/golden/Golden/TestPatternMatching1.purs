module Golden.TestPatternMatching1 where

data N = Zero | Succ N
data E = Num N | Not E

pat :: E -> Int
pat e = case e of
  Not (Num (Succ _)) -> 1
  Not (Num Zero) -> 2
  Not (Not (Num (Succ _))) -> 3
  Num (Succ _) -> 4
  Num _ -> 5
  _ -> 6

data Tuple = T Int Int

fst :: Tuple -> Int
fst (T x _) = x

snd :: Tuple -> Int
snd (T _ y) = y
