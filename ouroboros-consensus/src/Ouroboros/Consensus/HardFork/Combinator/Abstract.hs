{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.HardFork.Combinator.Abstract (
    SingleEraBlock(..)
  , proxySingle
  , CanHardFork(..)
  ) where

import           Data.Proxy
import           Data.SOP
import           Data.Typeable

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.History (EraParams)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol

import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.Info
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.Translation

-- | Blocks from which we can assemble a hard fork
class LedgerSupportsProtocol blk => SingleEraBlock blk where
  singleEraParams     :: BlockConfig  blk -> EraParams
  singleEraTransition :: LedgerConfig blk -> LedgerState blk -> Maybe EpochNo
  singleEraInfo       :: proxy blk -> SingleEraInfo blk

proxySingle :: Proxy SingleEraBlock
proxySingle = Proxy

class (All SingleEraBlock xs, Typeable xs) => CanHardFork xs where
  hardForkEraTranslations :: EraTranslation xs

instance SingleEraBlock blk => CanHardFork '[blk] where
  hardForkEraTranslations = trivialEraTranslation
