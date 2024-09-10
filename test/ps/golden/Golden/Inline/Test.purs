module Golden.Inline.Test
  ( main
  , Mu
  , runMu
  , iMu
  ) where

main :: Int
main =
  let x :: forall a. a -> Int
      x _ = 1
  in let  y :: forall b. b -> Int
          y _ = 2
      in x y

newtype Mu a = MkMu (Mu a -> a)

runMu :: forall a. Mu a -> a
runMu mu@(MkMu f) = f mu

iMu :: Mu Int
iMu = MkMu runMu
