{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Ouroboros.Consensus.HardFork.Combinator.SingleEra.Combined (
    PerEraTopLevelConfig(..)
  , PerEraConsensusConfig(..)
  , PerEraBlockConfig(..)
  , OneEraBlock(..)
  , OneEraHeader(..)
  , OneEraHash(..)
  , OneEraValidationErr(..)
  , OneEraLedgerView(..)
  , OneEraLedgerState(..)
  , OneEraLedgerError(..)
  , OneEraValidateView(..)
  , OneEraSelectView(..)
  , OneEraIsLeader(..)
  , OneEraInfo(..)
    -- * Utility
  , oneEraBlockHeader
  , oneEraLedgerStateTip
  , oneEraInfo
  ) where

import           Data.FingerTree.Strict (Measured (..))
import           Data.SOP hiding (shift)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.DerivingVia
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.Info
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.Wrappers

{-------------------------------------------------------------------------------
  Combine information for all eras

  TODO: Here and elsewhere we should use a strict variant of sop-core.
-------------------------------------------------------------------------------}

newtype PerEraTopLevelConfig  xs = PerEraTopLevelConfig  { getPerEraTopLevelConfig  :: NP TopLevelConfig           xs }
newtype PerEraConsensusConfig xs = PerEraConsensusConfig { getPerEraConsensusConfig :: NP SingleEraConsensusConfig xs }
newtype PerEraBlockConfig     xs = PerEraBlockConfig     { getPerEraBlockConfig     :: NP BlockConfig              xs }

newtype OneEraBlock           xs = OneEraBlock           { getOneEraBlock           :: NS I                        xs }
newtype OneEraHeader          xs = OneEraHeader          { getOneEraHeader          :: NS Header                   xs }
newtype OneEraHash            xs = OneEraHash            { getOneEraHash            :: NS SingleEraHash            xs }
newtype OneEraValidationErr   xs = OneEraValidationErr   { getOneEraValidationErr   :: NS SingleEraValidationErr   xs }
newtype OneEraLedgerView      xs = OneEraLedgerView      { getOneEraLedgerView      :: NS SingleEraLedgerView      xs }
newtype OneEraLedgerState     xs = OneEraLedgerState     { getOneEraLedgerState     :: NS LedgerState              xs }
newtype OneEraLedgerError     xs = OneEraLedgerError     { getOneEraLedgerError     :: NS SingleEraLedgerError     xs }
newtype OneEraValidateView    xs = OneEraValidateView    { getOneEraValidateView    :: NS SingleEraValidateView    xs }
newtype OneEraSelectView      xs = OneEraSelectView      { getOneEraSelectView      :: NS SingleEraSelectView      xs }
newtype OneEraIsLeader        xs = OneEraIsLeader        { getOneEraIsLeader        :: NS SingleEraIsLeader        xs }
newtype OneEraInfo            xs = OneEraInfo            { getOneEraInfo            :: NS SingleEraInfo            xs }

{-------------------------------------------------------------------------------
  Utility
-------------------------------------------------------------------------------}

oneEraBlockHeader :: CanHardFork xs => OneEraBlock xs -> OneEraHeader xs
oneEraBlockHeader =
      OneEraHeader
    . hcmap proxySingle (getHeader . unI)
    . getOneEraBlock

oneEraLedgerStateTip :: CanHardFork xs
                     => OneEraLedgerState xs -> Point (OneEraBlock xs)
oneEraLedgerStateTip =
      distribPoint
    . hcmap proxySingle ledgerTipPoint
    . getOneEraLedgerState

oneEraInfo :: CanHardFork xs => NS f xs -> OneEraInfo xs
oneEraInfo era = OneEraInfo $ hcmap proxySingle singleEraInfo era

{-------------------------------------------------------------------------------
  HasHeader instance for OneEraHeader
-------------------------------------------------------------------------------}

type instance HeaderHash (OneEraHeader xs) = OneEraHash xs

instance CanHardFork xs => StandardHash (OneEraHeader xs)

instance CanHardFork xs => Measured BlockMeasure (OneEraHeader xs) where
  measure = blockMeasure

instance CanHardFork xs => HasHeader (OneEraHeader xs) where
  blockHash     = OneEraHash
                . hcmap proxySingle (SingleEraHash . blockHash)
                . getOneEraHeader
  blockPrevHash = distribChainHash
                . hcmap proxySingle (Comp . blockPrevHash)
                . getOneEraHeader

  blockSlot = hcollapse . hcmap proxySingle (K . blockSlot) . getOneEraHeader
  blockNo   = hcollapse . hcmap proxySingle (K . blockNo)   . getOneEraHeader

  blockInvariant = const True

{-------------------------------------------------------------------------------
  HasHeader instance for OneEraBlock
-------------------------------------------------------------------------------}

type instance HeaderHash (OneEraBlock xs) = OneEraHash xs

instance CanHardFork xs => StandardHash (OneEraBlock xs)

instance CanHardFork xs => Measured BlockMeasure (OneEraBlock xs) where
  measure = blockMeasure

instance CanHardFork xs => HasHeader (OneEraBlock xs) where
  blockHash      =            blockHash     . oneEraBlockHeader
  blockPrevHash  = castHash . blockPrevHash . oneEraBlockHeader
  blockSlot      =            blockSlot     . oneEraBlockHeader
  blockNo        =            blockNo       . oneEraBlockHeader
  blockInvariant = const True

{-------------------------------------------------------------------------------
  Internal: distributive properties
-------------------------------------------------------------------------------}

distribChainHash :: NS (ChainHash :.: Header) xs -> ChainHash (OneEraHeader xs)
distribChainHash = go
  where
    go :: NS (ChainHash :.: Header) xs -> ChainHash (OneEraHeader xs)
    go (Z (Comp GenesisHash))   = GenesisHash
    go (Z (Comp (BlockHash h))) = BlockHash (OneEraHash (Z $ SingleEraHash h))
    go (S h)                    = shiftChainHash $ go h

distribPoint :: NS Point xs -> Point (OneEraBlock xs)
distribPoint = go
  where
    go :: NS Point xs -> Point (OneEraBlock xs)
    go (Z GenesisPoint)     = GenesisPoint
    go (Z (BlockPoint s h)) = BlockPoint s (OneEraHash $ Z $ SingleEraHash h)
    go (S p)                = shiftPoint (go p)

shiftChainHash :: ChainHash (OneEraHeader xs) -> ChainHash (OneEraHeader (x ': xs))
shiftChainHash GenesisHash   = GenesisHash
shiftChainHash (BlockHash h) = BlockHash (shiftHash h)

shiftPoint :: Point (OneEraBlock xs) -> Point (OneEraBlock (x ': xs))
shiftPoint GenesisPoint     = GenesisPoint
shiftPoint (BlockPoint s h) = BlockPoint s (shiftHash h)

shiftHash :: OneEraHash xs -> OneEraHash (x ': xs)
shiftHash (OneEraHash h) = OneEraHash (S h)

{-------------------------------------------------------------------------------
  NoUnexpectedThunks instances
-------------------------------------------------------------------------------}

deriving via LiftNamedNP "PerEraConsensusConfig" SingleEraConsensusConfig xs
         instance CanHardFork xs => NoUnexpectedThunks (PerEraConsensusConfig xs)

deriving via LiftNamedNP "PerEraTopLevelConfig" TopLevelConfig xs
         instance CanHardFork xs => NoUnexpectedThunks (PerEraTopLevelConfig xs)

deriving via LiftNamedNS "OneEraHash" SingleEraHash xs
         instance CanHardFork xs => NoUnexpectedThunks (OneEraHash xs)

deriving via LiftNamedNS "OneEraLedgerError" SingleEraLedgerError xs
         instance CanHardFork xs => NoUnexpectedThunks (OneEraLedgerError xs)

deriving via LiftNamedNS "OneEraValidationErr" SingleEraValidationErr xs
         instance CanHardFork xs => NoUnexpectedThunks (OneEraValidationErr xs)

deriving via LiftNamedNS "OneEraInfo" SingleEraInfo xs
         instance CanHardFork xs => NoUnexpectedThunks (OneEraInfo xs)

{-------------------------------------------------------------------------------
  Other instances
-------------------------------------------------------------------------------}

deriving via LiftNS SingleEraHash          xs instance CanHardFork xs => Eq   (OneEraHash xs)
deriving via LiftNS SingleEraHash          xs instance CanHardFork xs => Ord  (OneEraHash xs)
deriving via LiftNS SingleEraHash          xs instance CanHardFork xs => Show (OneEraHash xs)

deriving via LiftNS SingleEraLedgerError   xs instance CanHardFork xs => Eq   (OneEraLedgerError xs)
deriving via LiftNS SingleEraLedgerError   xs instance CanHardFork xs => Show (OneEraLedgerError xs)

deriving via LiftNS SingleEraLedgerView    xs instance CanHardFork xs => Eq   (OneEraLedgerView xs)
deriving via LiftNS SingleEraLedgerView    xs instance CanHardFork xs => Show (OneEraLedgerView xs)

deriving via LiftNS SingleEraValidationErr xs instance CanHardFork xs => Eq   (OneEraValidationErr xs)
deriving via LiftNS SingleEraValidationErr xs instance CanHardFork xs => Show (OneEraValidationErr xs)

deriving via LiftNS SingleEraInfo          xs instance CanHardFork xs => Eq   (OneEraInfo xs)
deriving via LiftNS SingleEraInfo          xs instance CanHardFork xs => Show (OneEraInfo xs)
