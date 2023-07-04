module Hedgehog.Gen.Extended
  ( module Gen
  , recursiveFrequency
  ) where

import Hedgehog (MonadGen)
import Hedgehog.Gen as Gen

recursiveFrequency ∷ MonadGen m ⇒ [(Int, m a)] → [(Int, m a)] → m a
recursiveFrequency nonrecur recur =
  Gen.sized $ \n →
    if n <= 1
      then Gen.frequency nonrecur
      else Gen.frequency $ nonrecur <> fmap (fmap Gen.small) recur
