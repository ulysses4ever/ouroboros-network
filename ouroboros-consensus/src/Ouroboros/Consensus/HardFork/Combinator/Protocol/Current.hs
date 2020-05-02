{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.HardFork.Combinator.Protocol.Current (CurrentConsensusState(..))
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.Protocol.Current as Current
module Ouroboros.Consensus.HardFork.Combinator.Protocol.Current (
    CurrentConsensusState(..)
  , lift
  ) where

import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Consensus.HardFork.Combinator.Abstract

data CurrentConsensusState blk = CurrentConsensusState {
      currentConsensusState :: ConsensusState (BlockProtocol blk)
    }
  deriving (Generic)

lift :: Functor f
     => (ConsensusState (BlockProtocol blk) -> f (ConsensusState (BlockProtocol blk)))
     -> CurrentConsensusState blk -> f (CurrentConsensusState blk)
lift f (CurrentConsensusState cur) = CurrentConsensusState <$> f cur

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

deriving stock    instance SingleEraBlock blk => Show               (CurrentConsensusState blk)
deriving stock    instance SingleEraBlock blk => Eq                 (CurrentConsensusState blk)
deriving anyclass instance SingleEraBlock blk => NoUnexpectedThunks (CurrentConsensusState blk)
