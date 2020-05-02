{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Intended for qualified import
--
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.Ledger.State as LedgerState
module Ouroboros.Consensus.HardFork.Combinator.Ledger.State (
    -- * State
    LedgerState(..)
  , oneEraLedgerState
    -- * Apply transition
  , maybeTransition
    -- * Queries
  , transitions
  ) where

import           Control.Monad (guard)
import           Data.SOP

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util.Counting

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Config
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Current
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Past
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.DerivingVia
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (Extend (..), Telescope)
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

{-------------------------------------------------------------------------------
  Ledger state
-------------------------------------------------------------------------------}

newtype instance LedgerState (HardForkBlock xs) = HardForkLedgerState {
      getHardForkLedgerState :: Telescope PastLedgerState CurrentLedgerState xs
    }

oneEraLedgerState :: CanHardFork xs
                  => LedgerState (HardForkBlock xs) -> OneEraLedgerState xs
oneEraLedgerState = OneEraLedgerState
                  . hmap currentLedgerState
                  . Telescope.toNS
                  . getHardForkLedgerState

{-------------------------------------------------------------------------------
  Apply transition
-------------------------------------------------------------------------------}

maybeTransition :: forall xs. CanHardFork xs
                => HardForkLedgerConfig xs
                -> SlotNo
                -> LedgerState (HardForkBlock xs)
                -> LedgerState (HardForkBlock xs)
maybeTransition HardForkLedgerConfig{..} slot (HardForkLedgerState st) =
    HardForkLedgerState $
      Telescope.extend
        (InPairs.requiring (getPerEraTopLevelConfig hardForkLedgerConfigPerEra) $
          InPairs.lift
            proxySingle
            (Requiring . maybeNextLedger)
            (translateLedgerState hardForkEraTranslations))
        st
  where
    maybeNextLedger :: SingleEraBlock blk
                    => TranslateEraLedgerState blk blk'
                    -> TopLevelConfig blk
                    -> TopLevelConfig blk'
                    -> Extend PastLedgerState CurrentLedgerState blk blk'
    maybeNextLedger f cfg cfg' = Extend $ \cur -> do
        e    <- singleEraTransition (configLedger cfg) (currentLedgerState cur)
        past <- inNextEra (configBlock cfg) cur e
        return (past, CurrentLedgerState {
            currentLedgerStart = pastLedgerEnd past
          , currentLedgerState = translateLedgerStateWith f
                                   (configLedger cfg)
                                   (configLedger cfg')
                                   (currentLedgerState cur)
          })

    inNextEra :: SingleEraBlock blk
              => BlockConfig blk
              -> CurrentLedgerState blk  -- Current era
              -> EpochNo                 -- Start of the next
              -> Maybe (PastLedgerState blk)
    inNextEra cfg cur curEnd = do
        -- Upper bound is exclusive
        guard $ slot >= History.boundSlot curEndBound
        return $ PastLedgerState (currentLedgerStart cur) curEndBound
      where
        curEndBound :: History.Bound
        curEndBound = History.mkUpperBound
                        (singleEraParams cfg)
                        (currentLedgerStart cur)
                        curEnd

{-------------------------------------------------------------------------------
  Find all transitions

  TODO: If we make 'hardForkSummary' the primitive function in
  'HasHardForkHistory', ideally this should not be necessary anymore: the
  summary is trivially derivable from the ledger state. This would then
  also obsolete the need for caching.
-------------------------------------------------------------------------------}

transitions :: CanHardFork xs
            => HardForkLedgerConfig xs
            -> LedgerState (HardForkBlock xs) -> History.Transitions xs
transitions HardForkLedgerConfig{..} (HardForkLedgerState st) =
    Telescope.nonEmpty st $
      History.Transitions $
        shiftTransitions (getPerEraTopLevelConfig cfg) $
          allTransitions (getPerEraTopLevelConfig cfg) st
  where
    cfg = hardForkLedgerConfigPerEra

-- | Find transition points in all eras
--
-- This associates each transition with the era it transitions /from/.
-- See also 'shiftTransitions'.
allTransitions :: CanHardFork                                  xs
               => NP TopLevelConfig                            xs
               -> Telescope PastLedgerState CurrentLedgerState xs
               -> AtMost                                       xs EpochNo
allTransitions cfgs st =
    Telescope.toAtMost $
      Telescope.bihap
        (hpure (fn past))
        (hcmap proxySingle (fn . cur . configLedger) cfgs)
        st
  where
    past :: PastLedgerState blk -> K EpochNo blk
    past = K . pastLedgerTransition

    cur :: SingleEraBlock blk
        => LedgerConfig blk -> CurrentLedgerState blk -> K (Maybe EpochNo) blk
    cur cfg = K . singleEraTransition cfg . currentLedgerState

-- | Associate transitions with the era they transition /to/
--
-- 'allTransitions' associates transitions with the era in which they occur,
-- but the hard fork history infrastructure expects them to be associated with
-- the era that they transition /to/. 'shiftTransitions' implements this
-- shift of perspective, and also verifies that the final era cannot have
-- a transition.
shiftTransitions :: NP TopLevelConfig (x ': xs)
                 -> AtMost (x ': xs) EpochNo -> AtMost xs EpochNo
shiftTransitions = go
  where
    go :: NP TopLevelConfig (x ': xs)
       -> AtMost (x ': xs) EpochNo -> AtMost xs EpochNo
    go _                  AtMostNil                = AtMostNil
    go (_ :* cs@(_ :* _)) (AtMostCons t ts)        = AtMostCons t (go cs ts)
    go (_ :* Nil)         (AtMostCons _ AtMostNil) = error invalidTransition

    invalidTransition :: String
    invalidTransition = "Unexpected transition in final era"

{-------------------------------------------------------------------------------
  Required LedgerState instances
-------------------------------------------------------------------------------}

deriving via LiftTelescope PastLedgerState CurrentLedgerState xs
         instance CanHardFork xs => Eq (LedgerState (HardForkBlock xs))

deriving via LiftTelescope PastLedgerState CurrentLedgerState xs
         instance CanHardFork xs => Show (LedgerState (HardForkBlock xs))

deriving via LiftNamedTelescope "HardForkLedgerState" PastLedgerState CurrentLedgerState xs
         instance CanHardFork xs => NoUnexpectedThunks (LedgerState (HardForkBlock xs))
