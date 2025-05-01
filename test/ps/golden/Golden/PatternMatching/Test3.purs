module Golden.PatternMatching.Test3 where

import Prelude

test1 :: { a :: Int, b :: Int } -> Int
test1 = case _ of
  { a } | a > 0 -> a
  { b } | b > 0 -> b
  _ -> 0

test2 :: { a :: Int, b :: Int } -> Int
test2 = case _ of
  { a } | a > 0 -> a
  { a: _, b } | b > 0 -> b
  _ -> 0

test3 :: { a :: Int, b :: Int } -> Int
test3 = case _ of
  { a, b: _ } | a > 0 -> a
  { b } | b > 0 -> b
  _ -> 0

test4 :: { a :: Int, b :: Int } -> Int
test4 = case _ of
  { a, b: _ } | a > 0 -> a
  { a: _, b } | b > 0 -> b
  _ -> 0

test5 :: { a :: Int, b :: Int } -> Int
test5 = case _ of
  { a, b }
    | a > 0 -> a
    | b > 0 -> b
  _ -> 0
