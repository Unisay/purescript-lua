module Golden.TestPatternMatching2 where

import Golden.TestPatternMatching1 as P

data N = Zero | Succ N | Add N N | Mul N N

pat :: N -> Int
pat e = case e of
  (Add (Add _ _) Zero) -> 1
  (Add (Mul _ _) Zero) -> 2
  (Add _ (Mul _ _)) -> 3
  (Add _ (Add _ _)) -> 4
  (Add _ Zero) -> 5
  _ -> 6

bat :: P.N -> Int
bat n = case n of
  P.Zero -> 1
  P.Succ b -> bat b
