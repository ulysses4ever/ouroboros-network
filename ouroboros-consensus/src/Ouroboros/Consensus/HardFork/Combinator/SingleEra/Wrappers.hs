{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- | Newtype wrappers around type families
module Ouroboros.Consensus.HardFork.Combinator.SingleEra.Wrappers (
    SingleEraHash(..)
  , SingleEraLedgerError(..)
  , SingleEraConsensusConfig(..)
  , SingleEraIsLeader(..)
  , SingleEraLedgerView(..)
  , SingleEraValidationErr(..)
  , SingleEraValidateView(..)
  , SingleEraSelectView(..)
  ) where

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
  Newtype wrappers around type families

  We need these because type families cannot be partially applied.
-------------------------------------------------------------------------------}

newtype SingleEraHash        blk = SingleEraHash        (HeaderHash   blk)
newtype SingleEraLedgerError blk = SingleEraLedgerError (LedgerError  blk)

newtype SingleEraConsensusConfig blk = SingleEraConsensusConfig (ConsensusConfig (BlockProtocol blk))
newtype SingleEraIsLeader        blk = SingleEraIsLeader        (IsLeader        (BlockProtocol blk))
newtype SingleEraLedgerView      blk = SingleEraLedgerView      (LedgerView      (BlockProtocol blk))
newtype SingleEraValidationErr   blk = SingleEraValidationErr   (ValidationErr   (BlockProtocol blk))
newtype SingleEraValidateView    blk = SingleEraValidateView    (ValidateView    (BlockProtocol blk))
newtype SingleEraSelectView      blk = SingleEraSelectView      (SelectView      (BlockProtocol blk))

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

deriving newtype instance StandardHash blk => Eq                 (SingleEraHash blk)
deriving newtype instance StandardHash blk => Ord                (SingleEraHash blk)
deriving newtype instance StandardHash blk => Show               (SingleEraHash blk)
deriving newtype instance StandardHash blk => NoUnexpectedThunks (SingleEraHash blk)

deriving newtype instance UpdateLedger blk => Eq                 (SingleEraLedgerError blk)
deriving newtype instance UpdateLedger blk => Show               (SingleEraLedgerError blk)
deriving newtype instance UpdateLedger blk => NoUnexpectedThunks (SingleEraLedgerError blk)

deriving newtype instance BlockSupportsProtocol blk => NoUnexpectedThunks (SingleEraConsensusConfig blk)

deriving newtype instance BlockSupportsProtocol blk => Eq   (SingleEraLedgerView blk)
deriving newtype instance BlockSupportsProtocol blk => Show (SingleEraLedgerView blk)

deriving newtype instance BlockSupportsProtocol blk => Eq                 (SingleEraValidationErr blk)
deriving newtype instance BlockSupportsProtocol blk => Show               (SingleEraValidationErr blk)
deriving newtype instance BlockSupportsProtocol blk => NoUnexpectedThunks (SingleEraValidationErr blk)
