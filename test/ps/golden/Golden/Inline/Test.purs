module Golden.Inline.Test where

main :: Int
main =
  let x :: forall a. a -> Int
      x _ = 1
  in let  y :: forall b. b -> Int
          y _ = 2
      in x y
