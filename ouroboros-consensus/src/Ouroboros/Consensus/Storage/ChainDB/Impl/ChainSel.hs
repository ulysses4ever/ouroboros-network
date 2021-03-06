{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Operations involving chain selection: the initial chain selection and
-- adding a block.
module Ouroboros.Consensus.Storage.ChainDB.Impl.ChainSel
  ( initialChainSelection
  , addBlockAsync
  , addBlockSync
  , chainSelectionForBlock
    -- * Exported for testing purposes
  , olderThanK
  ) where

import           Control.Exception (assert)
import           Control.Monad (unless)
import           Control.Monad.Except
import           Control.Monad.Trans.State.Strict
import           Control.Tracer (Tracer, contramap, traceWith)
import           Data.Foldable (foldl')
import           Data.Function (on)
import           Data.List (partition, sortBy)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust, mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Stack (HasCallStack)

import           Ouroboros.Network.AnchoredFragment (Anchor,
                     AnchoredFragment (..))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Fragment.InFuture (CheckInFuture (..))
import qualified Ouroboros.Consensus.Fragment.InFuture as InFuture
import           Ouroboros.Consensus.Fragment.Validated (ValidatedFragment)
import qualified Ouroboros.Consensus.Fragment.Validated as VF
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.AnchoredFragment
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (WithFingerprint (..))

import           Ouroboros.Consensus.Fragment.Diff (ChainDiff (..))
import qualified Ouroboros.Consensus.Fragment.Diff as Diff
import           Ouroboros.Consensus.Fragment.ValidatedDiff
                     (ValidatedChainDiff (..))
import qualified Ouroboros.Consensus.Fragment.ValidatedDiff as ValidatedDiff
import           Ouroboros.Consensus.Storage.ChainDB.API (AddBlockPromise (..),
                     InvalidBlockReason (..))
import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
                     (BlockCache)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache as BlockCache
import           Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB (ImmDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB as ImmDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB (LgrDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB as LgrDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Query as Query
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Types
import           Ouroboros.Consensus.Storage.ChainDB.Impl.VolDB (VolDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.VolDB as VolDB

-- | Perform the initial chain selection based on the tip of the ImmutableDB
-- and the contents of the VolatileDB.
--
-- Returns the chosen validated chain and corresponding ledger.
--
-- See "## Initialization" in ChainDB.md.
initialChainSelection
  :: forall m blk. (IOLike m, LedgerSupportsProtocol blk)
  => ImmDB m blk
  -> VolDB m blk
  -> LgrDB m blk
  -> Tracer m (TraceEvent blk)
  -> TopLevelConfig blk
  -> StrictTVar m (WithFingerprint (InvalidBlocks blk))
  -> StrictTVar m (FutureBlocks blk)
  -> CheckInFuture m blk
  -> m (ChainAndLedger blk)
initialChainSelection immDB volDB lgrDB tracer cfg varInvalid varFutureBlocks
                      futureCheck = do
    -- We follow the steps from section "## Initialization" in ChainDB.md

    i :: Anchor blk <- ImmDB.getAnchorForTip immDB
    (succsOf, ledger) <- atomically $ do
      invalid <- forgetFingerprint <$> readTVar varInvalid
      (,) <$> (ignoreInvalidSuc volDB invalid <$> VolDB.filterByPredecessor volDB)
          <*> LgrDB.getCurrent lgrDB

    chains <- constructChains i succsOf

    -- We use the empty fragment anchored at @i@ as the current chain (and
    -- ledger) and the default in case there is no better candidate.
    let curChain          = Empty (AF.castAnchor i)
        curChainAndLedger = VF.new curChain ledger

    case NE.nonEmpty (filter (preferAnchoredCandidate cfg curChain) chains) of
      -- If there are no candidates, no chain selection is needed
      Nothing      -> return curChainAndLedger
      Just chains' -> maybe curChainAndLedger toChainAndLedger <$>
        chainSelection' curChainAndLedger chains'
  where
    -- | Turn the 'ValidatedChainDiff' into a 'ChainAndLedger'.
    --
    -- The rollback of the 'ChainDiff' must be empty, as the suffix starts
    -- from the tip of the ImmutableDB, and we can't roll back past that tip.
    -- This is guaranteed by the fact that all constructed candidates start
    -- from this tip.
    toChainAndLedger
      :: ValidatedChainDiff blk (LgrDB.LedgerDB blk)
      -> ChainAndLedger blk
    toChainAndLedger (ValidatedChainDiff chainDiff ledger) =
      case chainDiff of
        ChainDiff rollback suffix
          | rollback == 0
          -> VF.new suffix ledger
          | otherwise
          -> error "constructed an initial chain with rollback"

    -- | Use the VolatileDB to construct all chains starting from the tip of
    -- the ImmutableDB.
    constructChains :: Anchor blk -- ^ Tip of the ImmutableDB, @i@
                    -> (WithOrigin (HeaderHash blk) -> Set (HeaderHash blk))
                    -> m [AnchoredFragment (Header blk)]
    constructChains i succsOf = flip evalStateT Map.empty $
        mapM constructChain suffixesAfterI
      where
        suffixesAfterI :: [NonEmpty (HeaderHash blk)]
        suffixesAfterI = VolDB.candidates succsOf (AF.anchorToPoint i)

        constructChain :: NonEmpty (HeaderHash blk)
                       -> StateT (Map (HeaderHash blk) (Header blk))
                                 m
                                 (AnchoredFragment (Header blk))
        constructChain hashes =
          AF.fromOldestFirst (AF.castAnchor i) <$>
          mapM (getKnownHeaderThroughCache volDB) (NE.toList hashes)

    -- | Perform chain selection (including validation) on the given
    -- candidates.
    --
    -- PRECONDITION: all candidates are anchored at @i@.
    --
    -- PRECONDITION: all candidates must be preferred over the current chain.
    chainSelection' :: HasCallStack
                    => ChainAndLedger blk
                       -- ^ The current chain and ledger, corresponding to
                       -- @i@.
                    -> NonEmpty (AnchoredFragment (Header blk))
                       -- ^ Candidates anchored at @i@
                    -> m (Maybe (ValidatedChainDiff blk (LgrDB.LedgerDB blk)))
    chainSelection' curChainAndLedger candidates =
        assert (all ((LgrDB.currentPoint ledger ==) .
                     castPoint . AF.anchorPoint)
                    candidates) $
        assert (all (preferAnchoredCandidate cfg curChain) candidates) $
        chainSelection chainSelEnv (Diff.extend <$> candidates)
      where
        curChain = VF.validatedFragment curChainAndLedger
        ledger   = VF.validatedLedger   curChainAndLedger
        chainSelEnv = ChainSelEnv
          { lgrDB
          , cfg
          , varInvalid
          , varFutureBlocks
          , futureCheck
          , blockCache = BlockCache.empty
          , curChainAndLedger
          , trace = traceWith
              (contramap (TraceInitChainSelEvent . InitChainSelValidation) tracer)
          }

-- | Add a block to the ChainDB, /asynchronously/.
--
-- This adds a 'BlockToAdd' corresponding to the given block to the
-- 'cdbBlocksToAdd' queue. The entries in that queue are processed using
-- 'addBlockSync', see that function for more information.
--
-- When the queue is full, this function will still block.
--
-- An important advantage of this asynchronous approach over a synchronous
-- approach is that it doesn't have the following disadvantage: when a thread
-- adding a block to the ChainDB is killed, which can happen when
-- disconnecting from the corresponding node, we might have written the block
-- to disk, but not updated the corresponding in-memory state (e.g., that of
-- the VolatileDB), leaving both out of sync.
--
-- With this asynchronous approach, threads adding blocks asynchronously can
-- be killed without worries, the background thread processing the blocks
-- synchronously won't be killed. Only when the whole ChainDB shuts down will
-- that background thread get killed. But since there will be no more
-- in-memory state, it can't get out of sync with the file system state. On
-- the next startup, a correct in-memory state will be reconstructed from the
-- file system state.
addBlockAsync
  :: forall m blk. (IOLike m, HasHeader blk)
  => ChainDbEnv m blk
  -> blk
  -> m (AddBlockPromise m blk)
addBlockAsync CDB { cdbTracer, cdbBlocksToAdd } =
    addBlockToAdd (contramap TraceAddBlockEvent cdbTracer) cdbBlocksToAdd

-- | Add a block to the ChainDB, /synchronously/.
--
-- This is the only operation that actually changes the ChainDB. It will store
-- the block on disk and trigger chain selection, possibly switching to a
-- fork.
--
-- When the slot of the block is > the current slot, a chain selection will be
-- scheduled in the slot of the block.
addBlockSync
  :: forall m blk.
     ( IOLike m
     , HasHeader blk
     , LedgerSupportsProtocol blk
     , HasCallStack
     )
  => ChainDbEnv m blk
  -> BlockToAdd m blk
  -> m ()
addBlockSync cdb@CDB {..} BlockToAdd { blockToAdd = b, .. } = do
    (isMember, invalid, curChain) <- atomically $ (,,)
      <$> VolDB.getIsMember               cdbVolDB
      <*> (forgetFingerprint <$> readTVar cdbInvalid)
      <*> Query.getCurrentChain           cdb

    let immBlockNo = AF.anchorBlockNo curChain
        curTip     = castPoint (AF.headPoint curChain)

    -- We follow the steps from section "## Adding a block" in ChainDB.md

    -- Note: we call 'chainSelectionForFutureBlocks' in all branches instead
    -- of once, before branching, because we want to do it /after/ writing the
    -- block to the VolatileDB and delivering the 'varBlockWrittenToDisk'
    -- promise, as this is the promise the BlockFetch client waits for.
    -- Otherwise, the BlockFetch client would have to wait for
    -- 'chainSelectionForFutureBlocks'.

    -- ### Ignore
    if
      | olderThanK hdr (cdbIsEBB hdr) immBlockNo -> do
        trace $ IgnoreBlockOlderThanK (blockRealPoint b)
        deliverPromises False curTip
        chainSelectionForFutureBlocks cdb BlockCache.empty

      | isMember (blockHash b) -> do
        trace $ IgnoreBlockAlreadyInVolDB (blockRealPoint b)
        deliverPromises True curTip
        chainSelectionForFutureBlocks cdb BlockCache.empty

      | Just (InvalidBlockInfo reason _) <- Map.lookup (blockHash b) invalid -> do
        trace $ IgnoreInvalidBlock (blockRealPoint b) reason
        deliverPromises False curTip
        chainSelectionForFutureBlocks cdb BlockCache.empty

      -- The remaining cases
      | otherwise -> do
        VolDB.putBlock cdbVolDB b
        trace $ AddedBlockToVolDB (blockRealPoint b) (blockNo b) (cdbIsEBB hdr)
        atomically $ putTMVar varBlockWrittenToDisk True

        let blockCache = BlockCache.singleton b
        -- Do chain selection for future blocks before chain selection for the
        -- new block. When some future blocks are now older than the current
        -- block, we will do chain selection in a more chronological order.
        chainSelectionForFutureBlocks cdb blockCache
        newTip <- chainSelectionForBlock cdb blockCache hdr
        atomically $ putTMVar varBlockProcessed newTip
  where
    trace :: TraceAddBlockEvent blk -> m ()
    trace = traceWith (contramap TraceAddBlockEvent cdbTracer)

    hdr :: Header blk
    hdr = getHeader b

    -- | Use the given 'Bool' and 'Point' to fill in the 'TMVar's
    -- corresponding to the block's 'AddBlockPromise'.
    deliverPromises :: Bool -> Point blk -> m ()
    deliverPromises writtenToDisk tip = atomically $ do
      putTMVar varBlockWrittenToDisk      writtenToDisk
      putTMVar varBlockProcessed          tip

-- | Return 'True' when the given header should be ignored when adding it
-- because it is too old, i.e., we wouldn't be able to switch to a chain
-- containing the corresponding block because its block number is more than
-- @k@ blocks or exactly @k@ blocks back.
--
-- Special case: the header corresponds to an EBB which has the same block
-- number as the block @k@ blocks back (the most recent \"immutable\" block).
-- As EBBs share their block number with the block before them, the EBB is not
-- too old in that case and can be adopted as part of our chain.
--
-- This special case can occur, for example, when the VolatileDB is empty
-- (because of corruption). The \"immutable\" block is then also the tip of
-- the chain. If we then try to add the EBB after it, it will have the same
-- block number, so we must allow it.
olderThanK
  :: HasHeader (Header blk)
  => Header blk
     -- ^ Header of the block to add
  -> IsEBB
     -- ^ Whether the block is an EBB or not
  -> WithOrigin BlockNo
     -- ^ The block number of the most recent \"immutable\" block, i.e., the
     -- block @k@ blocks back.
  -> Bool
olderThanK hdr isEBB immBlockNo
    | At bNo == immBlockNo
    , isEBB == IsEBB
    = False
    | otherwise
    = At bNo <= immBlockNo
  where
    bNo = blockNo hdr

chainSelectionForFutureBlocks
  :: (IOLike m, LedgerSupportsProtocol blk, HasCallStack)
  => ChainDbEnv m blk -> BlockCache blk -> m ()
chainSelectionForFutureBlocks cdb@CDB{..} blockCache = do
    -- Get 'cdbFutureBlocks' and empty the map in the TVar. It will be
    -- repopulated with the blocks that are still from the future (but not the
    -- ones no longer from the future) during chain selection for those
    -- blocks.
    futureBlockHeaders <- atomically $ do
      futureBlocks <- readTVar cdbFutureBlocks
      writeTVar cdbFutureBlocks Map.empty
      return $ Map.elems futureBlocks
    forM_ futureBlockHeaders $ \hdr -> do
      trace $ ChainSelectionForFutureBlock (headerRealPoint hdr)
      chainSelectionForBlock cdb blockCache hdr
  where
    trace = traceWith (contramap TraceAddBlockEvent cdbTracer)

-- | Trigger chain selection for the given block.
--
-- PRECONDITION: the block is in the VolatileDB.
--
-- PRECONDITION: the slot of the block <= the current (wall) slot
--
-- The new tip of the current chain is returned.
--
-- = Constructing candidate fragments
--
-- The VolatileDB keeps a \"successors\" map in memory, telling us the hashes
-- of the known successors of any block, but it does not keep /headers/ in
-- memory, which are needed to construct candidate fargments. We try to reuse
-- the headers from the current chain fragment where possible, but it will not
-- contain all needed headers. This means that we will need to read some
-- blocks from disk and extract their headers. Under normal circumstances this
-- does not matter too much; although this will be done every time we add a
-- block, the expected number of headers to read from disk is very small:
--
-- * None if we stay on the current chain and this is just the next block
-- * A handful if we stay on the current chain and the block we just received
--   was a missing block and we already received some of its successors
-- * A handful if we switch to a short fork
--
-- This is expensive only
--
-- * on startup: in this case we need to read at least @k@ blocks from the
--   VolatileDB, and possibly more if there are some other chains in the
--   VolatileDB starting from the tip of the ImmutableDB
-- * when we switch to a distant fork
--
-- This cost is currently deemed acceptable.
chainSelectionForBlock
  :: forall m blk.
     ( IOLike m
     , HasHeader blk
     , LedgerSupportsProtocol blk
     , HasCallStack
     )
  => ChainDbEnv m blk
  -> BlockCache blk
  -> Header blk
  -> m (Point blk)
chainSelectionForBlock cdb@CDB{..} blockCache hdr = do
    (invalid, succsOf, predecessor, curChain, tipPoint, ledgerDB)
      <- atomically $ (,,,,,)
          <$> (forgetFingerprint <$> readTVar cdbInvalid)
          <*> VolDB.filterByPredecessor cdbVolDB
          <*> VolDB.getPredecessor      cdbVolDB
          <*> Query.getCurrentChain     cdb
          <*> Query.getTipPoint         cdb
          <*> LgrDB.getCurrent          cdbLgrDB
    let curChainAndLedger :: ChainAndLedger blk
        curChainAndLedger =
          -- The current chain we're working with here is not longer than @k@
          -- blocks (see 'getCurrentChain' and 'cdbChain'), which is easier to
          -- reason about when doing chain selection, etc.
          assert (fromIntegral (AF.length curChain) <= k) $
          VF.new curChain ledgerDB

        immBlockNo :: WithOrigin BlockNo
        immBlockNo = AF.anchorBlockNo curChain

        i :: Point blk
        i = castPoint $ AF.anchorPoint curChain

        -- Let these two functions ignore invalid blocks
        predecessor' = ignoreInvalid    cdb invalid predecessor
        succsOf'     = ignoreInvalidSuc cdb invalid succsOf

    -- The preconditions
    assert (isJust $ predecessor (headerHash hdr)) $ return ()

    if
      -- The chain might have grown since we added the block such that the
      -- block is older than @k@.
      | olderThanK hdr (cdbIsEBB hdr) immBlockNo -> do
        trace $ IgnoreBlockOlderThanK p
        return tipPoint

      -- We might have validated the block in the meantime
      | Just (InvalidBlockInfo reason _) <- Map.lookup (headerHash hdr) invalid -> do
        trace $ IgnoreInvalidBlock p reason
        return tipPoint

      -- The block @b@ fits onto the end of our current chain
      | pointHash tipPoint == castHash (blockPrevHash hdr) -> do
        -- ### Add to current chain
        trace (TryAddToCurrentChain p)
        addToCurrentChain succsOf' curChainAndLedger

      | Just hashes <- VolDB.isReachable predecessor' i p -> do
        -- ### Switch to a fork
        trace (TrySwitchToAFork p hashes)
        switchToAFork succsOf' curChainAndLedger hashes

      | otherwise -> do
        -- ### Store but don't change the current chain
        trace (StoreButDontChange p)
        return tipPoint

    -- Note that we may have extended the chain, but have not trimmed it to
    -- @k@ blocks/headers. That is the job of the background thread, which
    -- will first copy the blocks/headers to trim (from the end of the
    -- fragment) from the VolatileDB to the ImmutableDB.
  where
    SecurityParam k = configSecurityParam cdbTopLevelConfig

    p :: RealPoint blk
    p = headerRealPoint hdr

    trace :: TraceAddBlockEvent blk -> m ()
    trace = traceWith (contramap TraceAddBlockEvent cdbTracer)

    mkChainSelEnv :: ChainAndLedger blk -> ChainSelEnv m blk
    mkChainSelEnv curChainAndLedger = ChainSelEnv
      { lgrDB             = cdbLgrDB
      , cfg               = cdbTopLevelConfig
      , varInvalid        = cdbInvalid
      , varFutureBlocks   = cdbFutureBlocks
      , futureCheck       = cdbCheckInFuture
      , blockCache        = blockCache
      , curChainAndLedger = curChainAndLedger
      , trace             =
          traceWith (contramap (TraceAddBlockEvent . AddBlockValidation) cdbTracer)
      }

    -- | PRECONDITION: the header @hdr@ (and block @b@) fit onto the end of
    -- the current chain.
    addToCurrentChain :: HasCallStack
                      => (WithOrigin (HeaderHash blk) -> Set (HeaderHash blk))
                      -> ChainAndLedger blk
                         -- ^ The current chain and ledger
                      -> m (Point blk)
    addToCurrentChain succsOf curChainAndLedger
                    = assert (AF.validExtension curChain hdr) $ do
        let suffixesAfterB = VolDB.candidates succsOf (realPointToPoint p)

        -- Fragments that are anchored at @curHead@, i.e. suffixes of the
        -- current chain.
        candidates <- case NE.nonEmpty suffixesAfterB of
          -- If there are no suffixes after @b@, just use the suffix just
          -- containing @b@ as the sole candidate.
          Nothing              ->
            return $ (AF.fromOldestFirst curHead [hdr]) NE.:| []
          Just suffixesAfterB' ->
            -- We can start with an empty cache, because we're only looking
            -- up the headers /after/ b, so they won't be on the current
            -- chain.
            flip evalStateT Map.empty $ forM suffixesAfterB' $ \hashes -> do
              hdrs <- mapM (getKnownHeaderThroughCache cdbVolDB) $
                        NE.toList hashes
              return $ AF.fromOldestFirst curHead (hdr : hdrs)

        let chainDiffs = NE.nonEmpty
              $ NE.filter ( preferAnchoredCandidate cdbTopLevelConfig curChain
                          . Diff.getSuffix
                          )
              $ fmap Diff.extend candidates
        -- All candidates are longer than the current chain, so they will be
        -- preferred over it, /unless/ the block we just added is an EBB,
        -- which has the same 'BlockNo' as the block before it, so when
        -- using the 'BlockNo' as the proxy for the length (note that some
        -- protocols might do it differently), the candidate with the EBB
        -- appended will not be preferred over the current chain.
        --
        -- The consequence of this is that when adding an EBB, it will not
        -- be selected by chain selection and thus not appended to the chain
        -- until the block after it is added, which will again result in a
        -- candidate preferred over the current chain. In this case, the
        -- candidate will be a two-block (the EBB and the new block)
        -- extension of the current chain.
        case chainDiffs of
          Nothing          -> return curTip
          Just chainDiffs' ->
            chainSelection chainSelEnv chainDiffs' >>= \case
              Nothing                 ->
                return curTip
              Just validatedChainDiff ->
                switchTo validatedChainDiff (AddedToCurrentChain p)
      where
        chainSelEnv = mkChainSelEnv curChainAndLedger
        curChain    = VF.validatedFragment curChainAndLedger
        curTip      = castPoint $ AF.headPoint curChain
        curHead     = AF.headAnchor curChain

    -- | We have found a path of hashes to the new block through the
    -- VolatileDB. We try to extend this path by looking for forks that start
    -- with the given block, then we do chain selection and /possibly/ try to
    -- switch to a new fork.
    switchToAFork :: HasCallStack
                  => (WithOrigin (HeaderHash blk) -> Set (HeaderHash blk))
                  -> ChainAndLedger blk
                     -- ^ The current chain (anchored at @i@) and ledger
                  -> NonEmpty (HeaderHash blk)
                     -- ^ An uninterrupted path of hashes @(i,b]@.
                  -> m (Point blk)
    switchToAFork succsOf curChainAndLedger hashes = do
        let suffixesAfterB = VolDB.candidates succsOf (realPointToPoint p)
            initCache      = Map.insert (headerHash hdr) hdr (cacheHeaders curChain)
        -- Fragments that are anchored at @i@.
        candidates <- flip evalStateT initCache $
          case NE.nonEmpty suffixesAfterB of
            -- If there are no suffixes after @b@, just use the fragment that
            -- ends in @b@ as the sole candidate.
            Nothing              -> (NE.:| []) <$> constructFork i hashes []
            Just suffixesAfterB' -> mapM (constructFork i hashes . NE.toList)
                                         suffixesAfterB'
        let chainDiffs =
                -- The suffixes all fork off from the current chain within @k@
                -- blocks, so it satisfies the precondition of
                -- 'preferCandidate'.
                filter
                  (preferAnchoredCandidate cdbTopLevelConfig curChain .
                   Diff.getSuffix)
              . mapMaybe (Diff.diff curChain)
              $ NE.toList candidates

        case NE.nonEmpty chainDiffs of
          -- No candidates preferred over the current chain
          Nothing          -> return curTip
          Just chainDiffs' ->
            chainSelection chainSelEnv chainDiffs' >>= \case
              Nothing                 ->
                return curTip
              Just validatedChainDiff ->
                switchTo validatedChainDiff (SwitchedToAFork p)
      where
        chainSelEnv = mkChainSelEnv curChainAndLedger
        curChain    = VF.validatedFragment curChainAndLedger
        curTip      = castPoint $ AF.headPoint curChain
        i           = AF.castAnchor $ anchor curChain

    -- | Try to apply the given 'ChainDiff' on the current chain fragment. The
    -- 'LgrDB.LedgerDB' is updated in the same transaction.
    --
    -- Note that we /cannot/ have switched to a different current chain in the
    -- meantime, since this function will only be called by a single
    -- background thread.
    --
    -- It /is/ possible that the background thread copying headers older than
    -- @k@ from the VolatileDB to the ImmutableDB has removed some headers
    -- from the beginning of the current chain fragment, but does not affect
    -- us, as we cannot roll back more than @k@ headers anyway.
    switchTo
      :: HasCallStack
      => ValidatedChainDiff blk (LgrDB.LedgerDB blk)
         -- ^ Chain and ledger to switch to
      -> (    AnchoredFragment (Header blk)
           -> AnchoredFragment (Header blk)
           -> TraceAddBlockEvent blk
         )
         -- ^ Given the previous chain and the new chain, return the event
         -- to trace when we switched to the new chain.
      -> m (Point blk)
    switchTo (ValidatedChainDiff chainDiff newLedger) mkTraceEvent = do
        (curChain, newChain) <- atomically $ do
          curChain <- readTVar cdbChain
          case Diff.apply curChain chainDiff of
            -- Impossible, as described in the docstring
            Nothing       ->
              error "chainDiff doesn't fit onto current chain"
            Just newChain -> do
              writeTVar cdbChain newChain
              LgrDB.setCurrent cdbLgrDB newLedger

              -- Update the readers
              --
              -- 'Reader.switchFork' needs to know the intersection point
              -- (@ipoint@) between the old and the current chain.
              let ipoint = Diff.getAnchorPoint chainDiff
              readerHandles <- Map.elems <$> readTVar cdbReaders
              forM_ readerHandles $ \readerHandle ->
                rhSwitchFork readerHandle ipoint newChain

              return (curChain, newChain)

        trace $ mkTraceEvent curChain newChain
        traceWith cdbTraceLedger newLedger

        return $ castPoint $ AF.headPoint newChain

    -- | Build a cache from the headers in the fragment.
    cacheHeaders :: AnchoredFragment (Header blk)
                 -> Map (HeaderHash blk) (Header blk)
    cacheHeaders =
      foldl' (\m h -> Map.insert (blockHash h) h m) Map.empty .
      AF.toNewestFirst

    -- | We have a new block @b@ that doesn't fit onto the current chain, but
    -- there is an unbroken path from the tip of the ImmutableDB (@i@ = the
    -- anchor point of the current chain) to @b@. We also have a suffix @s@ of
    -- hashes that starts after @b@.
    --
    -- We will try to construct a fragment @f@ for the fork such that:
    -- * @f@ is anchored at @i@
    -- * @f@ starts with the headers corresponding to the hashes of @(i,b]@
    -- * The next header in @f@ is the header for @b@
    -- * Finally, @f@ ends with the headers corresponding to the hashes
    --   @(b,?]@ of the suffix @s@.
    -- * Any headers from the future are dropped.
    --
    -- Note that we need to read the headers corresponding to the hashes
    -- @(i,b]@ and @(b,?]@ from disk. It is likely that many of these headers
    -- are actually on the current chain, so when possible, we reuse these
    -- headers instead of reading them from disk.
    constructFork
      :: Anchor blk                 -- ^ Tip of ImmutableDB @i@
      -> NonEmpty (HeaderHash blk)  -- ^ Hashes of @(i,b]@
      -> [HeaderHash blk]           -- ^ Suffix @s@, hashes of @(b,?]@
      -> StateT (Map (HeaderHash blk) (Header blk))
                m
                (AnchoredFragment (Header blk))
         -- ^ Fork, anchored at @i@, contains (the header of) @b@ and ends
         -- with the suffix @s@.
    constructFork i hashes suffixHashes
      = fmap (AF.fromOldestFirst (AF.castAnchor i))
      $ mapM (getKnownHeaderThroughCache cdbVolDB)
      $ NE.toList hashes <> suffixHashes


-- | Check whether the header for the hash is in the cache, if not, get
-- the corresponding header from the VolatileDB and store it in the cache.
--
-- PRECONDITION: the header (block) must exist in the VolatileDB.
getKnownHeaderThroughCache
  :: (MonadCatch m, HasHeader blk)
  => VolDB m blk
  -> HeaderHash blk
  -> StateT (Map (HeaderHash blk) (Header blk)) m (Header blk)
getKnownHeaderThroughCache volDB hash = gets (Map.lookup hash) >>= \case
  Just hdr -> return hdr
  Nothing  -> do
    hdr <- lift $ VolDB.getKnownHeader volDB hash
    modify (Map.insert hash hdr)
    return hdr

-- | Environment used by 'chainSelection' and related functions.
data ChainSelEnv m blk = ChainSelEnv
    { lgrDB             :: LgrDB m blk
    , trace             :: TraceValidationEvent blk -> m ()
    , cfg               :: TopLevelConfig blk
    , varInvalid        :: StrictTVar m (WithFingerprint (InvalidBlocks blk))
    , varFutureBlocks   :: StrictTVar m (FutureBlocks blk)
    , futureCheck       :: CheckInFuture m blk
    , blockCache        :: BlockCache blk
    , curChainAndLedger :: ChainAndLedger blk
    }

-- | Perform chain selection with the given candidates. If a validated
-- candidate was chosen to replace the current chain, return it along with the
-- corresponding ledger.
--
-- PRECONDITION: all candidates must be preferred over the current chain.
--
-- PRECONDITION: the candidate chain diffs must fit on the (given) current
-- chain.
chainSelection
  :: forall m blk.
     ( IOLike m
     , LedgerSupportsProtocol blk
     , HasCallStack
     )
  => ChainSelEnv m blk
  -> NonEmpty (ChainDiff blk)
  -> m (Maybe (ValidatedChainDiff blk (LgrDB.LedgerDB blk)))
     -- ^ The (valid) chain diff and corresponding LedgerDB that was selected,
     -- or 'Nothing' if there is no valid chain diff preferred over the
     -- current chain.
chainSelection chainSelEnv chainDiffs =
    assert (all (preferAnchoredCandidate cfg curChain . Diff.getSuffix)
                chainDiffs) $
    assert (all (isJust . Diff.apply curChain)
                chainDiffs) $
    go (sortCandidates (NE.toList chainDiffs))
  where
    ChainSelEnv { cfg, curChainAndLedger, varInvalid, varFutureBlocks } =
      chainSelEnv

    curChain = VF.validatedFragment curChainAndLedger

    sortCandidates :: [ChainDiff blk] -> [ChainDiff blk]
    sortCandidates =
      sortBy (flip (compareAnchoredCandidates cfg) `on` Diff.getSuffix)

    -- 1. Take the first candidate from the list of sorted candidates
    -- 2. Validate it
    --    - If it is invalid -> discard it and go to 1 with the rest of the
    --      list.
    --    - If it is valid and has the same tip -> return it
    --    - If it is valid, but is a prefix of the original ->
    --        add it to the list, sort it and go to 1. See the comment
    --        [Ouroboros] below.
    go :: [ChainDiff blk] -> m (Maybe (ValidatedChainDiff blk (LgrDB.LedgerDB blk)))
    go []            = return Nothing
    go (candidate:candidates0) =
      validateCandidate chainSelEnv candidate >>= \case
        InsufficientSuffix -> do
          candidates1 <- truncateRejectedBlocks candidates0
          go (sortCandidates candidates1)
        FullyValid validatedCandidate@(ValidatedChainDiff candidate' _) ->
          -- The entire candidate is valid
          assert (Diff.getTip candidate == Diff.getTip candidate') $
          return $ Just validatedCandidate
        ValidPrefix candidate' -> do
          -- Prefix of the candidate because it contained rejected blocks
          -- (invalid blocks and/or blocks from the future). Note that the
          -- spec says go back to candidate selection, see [^whyGoBack],
          -- because there might still be some candidates that contain the
          -- same rejected block. To simplify the control flow, we do it
          -- differently: instead of recomputing the candidates taking
          -- rejected blocks into account, we just truncate the remaining
          -- candidates that contain rejected blocks.
          candidates1 <- truncateRejectedBlocks candidates0
          -- Only include the prefix if it is still preferred over the current
          -- chain. When the candidate is now empty because of the truncation,
          -- it will be dropped here, as it will not be preferred over the
          -- current chain.
          let candidates2
                | preferAnchoredCandidate cfg curChain (Diff.getSuffix candidate')
                = candidate':candidates1
                | otherwise
                = candidates1
          go (sortCandidates candidates2)

    -- | Truncate the given (remaining) candidates that contain rejected
    -- blocks. Discard them if they are truncated so much that they are no
    -- longer preferred over the current chain.
    --
    -- A block is rejected if:
    --
    -- * It is invalid (present in 'varInvalid', i.e., 'cdbInvalid').
    -- * It is from the future (present in 'varFutureBlocks', i.e.,
    --   'cdbFutureBlocks').
    truncateRejectedBlocks :: [ChainDiff blk] -> m [ChainDiff blk]
    truncateRejectedBlocks cands = do
      (invalid, futureBlocks) <-
        atomically $ (,) <$> readTVar varInvalid <*> readTVar varFutureBlocks
      let isRejected hdr =
               Map.member (headerHash hdr) (forgetFingerprint invalid)
            || Map.member (headerHash hdr) futureBlocks
      return $ filter (preferAnchoredCandidate cfg curChain . Diff.getSuffix)
             $ mapMaybe (Diff.takeWhileOldest (not . isRejected)) cands

    -- [Ouroboros]
    --
    -- Ouroboros says that when we are given an invalid chain by a peer, we
    -- should reject that peer's chain. However, since we're throwing all
    -- blocks together in the ChainDB, we can't tell which block or which
    -- chain came from which peer, so we can't simply reject a peer's chain.
    --
    -- It might be that a good peer gave us a valid chain, but another peer
    -- gave us an invalid block that fits onto the chain of the good peer. In
    -- that case, we do still want to adopt the chain of the good peer, which
    -- is a prefix of the chain that we constructed using all the blocks we
    -- found in the VolatileDB, including the invalid block.
    --
    -- This is the reason why we still take valid prefixes of a invalid chains
    -- into account during chain selection: they might correspond to the good
    -- peer's valid chain.

-- | Result of 'validateCandidate'.
data ValidationResult blk =
      -- | The entire candidate fragment was valid. No blocks were from the
      -- future.
      FullyValid (ValidatedChainDiff blk (LgrDB.LedgerDB blk))

      -- | The candidate fragment contained invalid blocks and/or blocks from
      -- the future that had to be truncated from the fragment.
    | ValidPrefix (ChainDiff blk)

      -- | After truncating the invalid blocks or blocks from the future from
      -- the 'ChainDiff', it no longer contains enough blocks in its suffix to
      -- compensate for the number of blocks it wants to roll back.
    | InsufficientSuffix

-- | Validate a candidate by applying its blocks to the ledger, and return a
-- 'ValidatedChainDiff' for it, i.e., a chain diff along with a ledger
-- corresponding to its tip (the most recent block).
--
-- PRECONDITION: the candidate (chain diff) must fit onto the given current
-- chain.
--
-- If all blocks in the fragment are valid, then the chain diff in the
-- returned 'ValidatedChainDiff' is the same as the given candidate chain
-- diff.
--
-- If a block in the fragment is invalid, then the fragment in the returned
-- 'ValidatedChainDiff' is a prefix of the given candidate chain diff (upto
-- the last valid block).
--
-- Returns 'Nothing' if after truncating the invalid blocks from the
-- candidate, it no longer has enough new blocks to compensate for the
-- required roll back.
ledgerValidateCandidate
  :: forall m blk.
     ( IOLike m
     , LedgerSupportsProtocol blk
     , HasCallStack
     )
  => ChainSelEnv m blk
  -> ChainDiff blk
  -> m (Maybe (ValidatedChainDiff blk (LgrDB.LedgerDB blk)))
ledgerValidateCandidate chainSelEnv chainDiff@(ChainDiff rollback suffix) =
    LgrDB.validate lgrDB curLedger blockCache rollback newBlocks >>= \case
      LgrDB.ValidateExceededRollBack {} ->
        -- Impossible: we asked the LgrDB to roll back past the immutable tip,
        -- which is impossible, since the candidates we construct must connect
        -- to the immutable tip.
        error "found candidate requiring rolling back past the immutable tip"

      LgrDB.ValidateLedgerError (LgrDB.AnnLedgerError ledger' pt e) -> do
        let lastValid = LgrDB.currentPoint ledger'
        trace (InvalidBlock e pt)
        addInvalidBlock e pt
        case Diff.truncate lastValid chainDiff of
          -- After truncating the invalid blocks on the fragment, it no longer
          -- contains enough blocks to compensate for the rollback.
          Nothing -> return Nothing

          Just chainDiff' -> do
            trace (ValidCandidate (Diff.getSuffix chainDiff'))
            return $ Just $ ValidatedDiff.new chainDiff' ledger'

      LgrDB.ValidateSuccessful ledger' -> do
        trace (ValidCandidate suffix)
        return $ Just $ ValidatedDiff.new chainDiff ledger'
  where
    ChainSelEnv { lgrDB, trace, curChainAndLedger, blockCache, varInvalid } =
      chainSelEnv

    curLedger :: LgrDB.LedgerDB blk
    curLedger = VF.validatedLedger curChainAndLedger

    newBlocks :: [Header blk]
    newBlocks = AF.toOldestFirst suffix

    -- | Record the invalid block in 'cdbInvalid' and change its fingerprint.
    addInvalidBlock :: ExtValidationError blk -> RealPoint blk -> m ()
    addInvalidBlock e (RealPoint slot hash) = atomically $
      modifyTVar varInvalid $ \(WithFingerprint invalid fp) ->
        WithFingerprint
          (Map.insert hash (InvalidBlockInfo (ValidationError e) slot) invalid)
          (succ fp)

-- | Truncate any future headers from the candidate 'ValidatedChainDiff'.
--
-- Future headers that don't exceed the clock skew
-- ('inFutureExceedsClockSkew') are added to 'cdbFutureBlocks'.
--
-- Future headers that exceed the clock skew are added to 'cdbInvalid' with
-- 'InFutureExceedsClockSkew' as the reason.
--
-- When truncation happened, 'Left' is returned, otherwise 'Right'. If too
-- many blocks have been truncated such that the invariant of 'ChainDiff' no
-- longer holds, 'Nothing' is returned.
futureCheckCandidate
  :: forall m blk. (IOLike m, LedgerSupportsProtocol blk)
  => ChainSelEnv m blk
  -> ValidatedChainDiff blk (LgrDB.LedgerDB blk)
  -> m (Maybe (Either (ChainDiff blk)
                      (ValidatedChainDiff blk (LgrDB.LedgerDB blk))))
futureCheckCandidate chainSelEnv validatedChainDiff =
    checkInFuture futureCheck validatedSuffix >>= \case

      (suffix', []) ->
        -- If no headers are in the future, then the fragment must be untouched
        assert (AF.headPoint suffix == AF.headPoint suffix') $
        return $ Just $ Right validatedChainDiff

      (suffix', inFuture) -> do
        let (exceedClockSkew, inNearFuture) =
              partition InFuture.inFutureExceedsClockSkew inFuture
        -- Record future blocks
        unless (null inNearFuture) $ do
          let futureHeaders = InFuture.inFutureHeader <$> inNearFuture
              futureBlocks  = Map.fromList
                [ (headerHash hdr, hdr) | hdr <- futureHeaders ]
          atomically $ modifyTVar varFutureBlocks $ flip Map.union futureBlocks
          -- Trace the original @suffix@, as it contains the headers from the
          -- future
          trace $ CandidateContainsFutureBlocks suffix futureHeaders

        -- Record any blocks exceeding the clock skew as invalid
        unless (null exceedClockSkew) $ do
          let invalidHeaders = InFuture.inFutureHeader <$> exceedClockSkew
              invalidBlocks  = Map.fromList
                [ (headerHash hdr, info)
                | hdr <- invalidHeaders
                , let reason = InFutureExceedsClockSkew (headerRealPoint hdr)
                      info   = InvalidBlockInfo reason (blockSlot hdr)
                ]
          atomically $ modifyTVar varInvalid $ \(WithFingerprint invalid fp) ->
            WithFingerprint (Map.union invalid invalidBlocks) (succ fp)
          trace $
            CandidateContainsFutureBlocksExceedingClockSkew
              -- Trace the original @suffix@, as it contains the headers
              -- from the future
              suffix
              invalidHeaders

        -- Truncate the original 'ChainDiff' to match the truncated
        -- 'AnchoredFragment'.
        return $
          Left <$> Diff.truncate (castPoint (AF.headPoint suffix')) chainDiff
  where
    ChainSelEnv { trace, varInvalid, varFutureBlocks, futureCheck } =
      chainSelEnv

    ValidatedChainDiff chainDiff@(ChainDiff _ suffix) _ = validatedChainDiff

    validatedSuffix :: ValidatedFragment blk (LedgerState blk)
    validatedSuffix =
      ledgerState . LgrDB.ledgerDbCurrent <$>
      ValidatedDiff.toValidatedFragment validatedChainDiff

-- | Validate a candidate chain using 'ledgerValidate' and 'futureCheck'.
validateCandidate
  :: ( IOLike m
     , LedgerSupportsProtocol blk
     , HasCallStack
     )
  => ChainSelEnv m blk
  -> ChainDiff blk
  -> m (ValidationResult blk)
validateCandidate chainSelEnv chainDiff =
    ledgerValidateCandidate chainSelEnv chainDiff >>= \case
      Nothing                 -> return InsufficientSuffix
      Just validatedChainDiff ->
        futureCheckCandidate chainSelEnv validatedChainDiff >>= \case
          Nothing
              -> return InsufficientSuffix
          Just (Left chainDiff')
              -> return $ ValidPrefix chainDiff'
          Just (Right validatedChainDiff')
              | AF.length (Diff.getSuffix chainDiff) ==
                AF.length (Diff.getSuffix chainDiff')
                -- No truncation
              -> return $ FullyValid validatedChainDiff'
              | otherwise
                -- In case of invalid blocks but no blocks from the future, we
                -- throw away the ledger corresponding to the truncated
                -- fragment and will have to validate it again, even when it's
                -- the sole candidate.
              -> return $ ValidPrefix chainDiff'
            where
              chainDiff' = ValidatedDiff.getChainDiff validatedChainDiff'

{-------------------------------------------------------------------------------
  'ChainAndLedger'
-------------------------------------------------------------------------------}

-- | Instantiate 'ValidatedFragment' in the way that chain selection requires.
type ChainAndLedger blk = ValidatedFragment blk (LgrDB.LedgerDB blk)

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Wrap a @getter@ function so that it returns 'Nothing' for invalid blocks.
ignoreInvalid
  :: HasHeader blk
  => proxy blk
  -> InvalidBlocks blk
  -> (HeaderHash blk -> Maybe a)
  -> (HeaderHash blk -> Maybe a)
ignoreInvalid _ invalid getter hash
    | Map.member hash invalid = Nothing
    | otherwise               = getter hash

-- | Wrap a @successors@ function so that invalid blocks are not returned as
-- successors.
ignoreInvalidSuc
  :: HasHeader blk
  => proxy blk
  -> InvalidBlocks blk
  -> (WithOrigin (HeaderHash blk) -> Set (HeaderHash blk))
  -> (WithOrigin (HeaderHash blk) -> Set (HeaderHash blk))
ignoreInvalidSuc _ invalid succsOf =
    Set.filter (`Map.notMember` invalid) . succsOf
