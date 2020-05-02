{-# LANGUAGE DeriveGeneric #-}

module Ouroboros.Consensus.HardFork.Combinator.Ledger.Config (
    HardForkLedgerConfig(..)
  ) where

import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra

data HardForkLedgerConfig xs = HardForkLedgerConfig {
      hardForkLedgerConfigPerEra :: PerEraTopLevelConfig xs
    }
  deriving (Generic)

instance CanHardFork xs => NoUnexpectedThunks (HardForkLedgerConfig xs)
