{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Ouroboros.Consensus.HardFork.Combinator.SingleEra.Translation (
    TranslateEraLedgerState(..)
  , TranslateEraConsensusState(..)
  , EraTranslation(..)
  , trivialEraTranslation
  ) where

import           Cardano.Prelude (NoUnexpectedThunks, OnlyCheckIsWHNF (..))

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (InPairs (..))

newtype TranslateEraLedgerState blk blk' = TranslateEraLedgerState {
      translateLedgerStateWith :: LedgerConfig blk
                               -> LedgerConfig blk'
                               -> LedgerState blk
                               -> LedgerState blk'
    }

newtype TranslateEraConsensusState blk blk' = TranslateEraConsensusState {
      translateConsensusStateWith :: ConsensusConfig (BlockProtocol blk)
                                  -> ConsensusConfig (BlockProtocol blk')
                                  -> ConsensusState (BlockProtocol blk)
                                  -> ConsensusState (BlockProtocol blk')
    }

data EraTranslation xs = EraTranslation {
      translateLedgerState    :: InPairs TranslateEraLedgerState xs
    , translateConsensusState :: InPairs TranslateEraConsensusState xs
    }
  deriving NoUnexpectedThunks
       via OnlyCheckIsWHNF "EraTranslation" (EraTranslation xs)

trivialEraTranslation :: EraTranslation '[blk]
trivialEraTranslation = EraTranslation {
      translateLedgerState    = PNil
    , translateConsensusState = PNil
    }
