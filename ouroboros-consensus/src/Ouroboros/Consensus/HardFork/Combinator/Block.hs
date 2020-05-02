{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Consensus.HardFork.Combinator.Block (
    HardForkBlock(..)
    -- * Type family instances
  , BlockConfig(..)
  , Header(..)
  ) where

import           Data.FingerTree.Strict (Measured (..))

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra

{-------------------------------------------------------------------------------
  Hard fork block
-------------------------------------------------------------------------------}

newtype HardForkBlock xs = HardForkBlock {
      oneEraBlock :: OneEraBlock xs
    }

newtype instance BlockConfig (HardForkBlock xs) = HardForkConfig {
      perEraBlockConfig :: PerEraBlockConfig xs
    }

instance CanHardFork xs => GetHeader (HardForkBlock xs) where
  newtype Header (HardForkBlock xs) = HardForkHeader {
        oneEraHeader :: OneEraHeader xs
      }

  getHeader = HardForkHeader . oneEraBlockHeader . oneEraBlock

{-------------------------------------------------------------------------------
  HasHeader
-------------------------------------------------------------------------------}

type instance HeaderHash (HardForkBlock xs) = OneEraHash xs

instance CanHardFork xs => StandardHash (HardForkBlock xs)

instance CanHardFork xs => Measured BlockMeasure (HardForkBlock xs) where
  measure = blockMeasure

instance CanHardFork xs => HasHeader (HardForkBlock xs) where
  blockHash      =            blockHash     . getHeader
  blockPrevHash  = castHash . blockPrevHash . getHeader
  blockSlot      =            blockSlot     . getHeader
  blockNo        =            blockNo       . getHeader
  blockInvariant = const True

instance CanHardFork xs => HasHeader (Header (HardForkBlock xs)) where
  blockHash      =            blockHash     . oneEraHeader
  blockPrevHash  = castHash . blockPrevHash . oneEraHeader
  blockSlot      =            blockSlot     . oneEraHeader
  blockNo        =            blockNo       . oneEraHeader
  blockInvariant = const True
