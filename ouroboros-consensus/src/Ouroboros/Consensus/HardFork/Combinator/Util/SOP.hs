{-# LANGUAGE GADTs #-}

module Ouroboros.Consensus.HardFork.Combinator.Util.SOP (
    matchNS
  ) where

import           Data.Functor.Product
import           Data.SOP

matchNS :: NS f xs -> NS g xs -> Maybe (NS (Product f g) xs)
matchNS = go
  where
    go :: NS f xs -> NS g xs -> Maybe (NS (Product f g) xs)
    go (Z fx) (Z gx) = Just $ Z (Pair fx gx)
    go (S l)  (S r)  = S <$> go l r
    go _      _      = Nothing
