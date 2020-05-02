{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Ouroboros.Consensus.HardFork.Combinator.Ledger.Current (
    CurrentLedgerState(..)
  ) where

import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract

import           Ouroboros.Consensus.HardFork.Combinator.Abstract

{-------------------------------------------------------------------------------
  Current ledger state
-------------------------------------------------------------------------------}

data CurrentLedgerState blk = CurrentLedgerState {
      currentLedgerStart :: !(History.Bound)
    , currentLedgerState :: !(LedgerState blk)
    }
  deriving stock (Generic)

{-------------------------------------------------------------------------------
  IsLedger instance
-------------------------------------------------------------------------------}

instance SingleEraBlock blk => IsLedger (CurrentLedgerState blk) where
  type LedgerCfg (CurrentLedgerState blk) = LedgerConfig blk
  type LedgerErr (CurrentLedgerState blk) = LedgerError  blk

  applyChainTick cfg slot CurrentLedgerState{..} = Ticked slot $
      CurrentLedgerState {
          currentLedgerStart = currentLedgerStart
        , currentLedgerState = ticked
        }
    where
      Ticked _slot ticked = applyChainTick cfg slot currentLedgerState

instance SingleEraBlock blk => ApplyBlock (CurrentLedgerState blk) blk where
  applyLedgerBlock cfg blk (Ticked slot CurrentLedgerState{..}) =
      CurrentLedgerState currentLedgerStart <$>
        applyLedgerBlock cfg blk (Ticked slot currentLedgerState)

  reapplyLedgerBlock cfg blk (Ticked slot CurrentLedgerState{..}) =
      CurrentLedgerState currentLedgerStart $
        reapplyLedgerBlock cfg blk (Ticked slot currentLedgerState)

  ledgerTipPoint = ledgerTipPoint . currentLedgerState

{-------------------------------------------------------------------------------
  Derived type class instances
-------------------------------------------------------------------------------}

deriving stock    instance SingleEraBlock blk => Show               (CurrentLedgerState blk)
deriving stock    instance SingleEraBlock blk => Eq                 (CurrentLedgerState blk)
deriving anyclass instance SingleEraBlock blk => NoUnexpectedThunks (CurrentLedgerState blk)
