{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Ledger () where

import           Data.SOP

import           Ouroboros.Network.Block (castPoint)

import           Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util.Counting

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Config
import qualified Ouroboros.Consensus.HardFork.Combinator.Ledger.State as LedgerState
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra

{-------------------------------------------------------------------------------
  IsLedger
-------------------------------------------------------------------------------}

instance CanHardFork xs => IsLedger (LedgerState (HardForkBlock xs)) where
  type LedgerCfg (LedgerState (HardForkBlock xs)) = HardForkLedgerConfig xs
  type LedgerErr (LedgerState (HardForkBlock xs)) = OneEraLedgerError    xs

  applyChainTick = undefined

{-------------------------------------------------------------------------------
  ApplyBlock
-------------------------------------------------------------------------------}

instance CanHardFork xs
      => ApplyBlock (LedgerState (HardForkBlock xs)) (HardForkBlock xs) where

  applyLedgerBlock   = undefined
  reapplyLedgerBlock = undefined

  ledgerTipPoint = castPoint
                 . oneEraLedgerStateTip
                 . LedgerState.oneEraLedgerState

{-------------------------------------------------------------------------------
  UpdateLedger
-------------------------------------------------------------------------------}

instance CanHardFork xs => UpdateLedger (HardForkBlock xs)

{-------------------------------------------------------------------------------
  HasHardForkHistory
-------------------------------------------------------------------------------}

instance CanHardFork xs => HasHardForkHistory (HardForkBlock xs) where
  type HardForkIndices (HardForkBlock xs) = xs

  hardForkShape       = History.Shape . exactlyFromNP
                      . hcmap proxySingle (K . singleEraParams)
                      . getPerEraBlockConfig
                      . perEraBlockConfig
  hardForkTransitions = LedgerState.transitions
