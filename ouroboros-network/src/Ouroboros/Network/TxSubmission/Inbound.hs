{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TxSubmission.Inbound (
    txSubmissionInbound,
    TxSubmissionMempoolWriter(..),
    TraceTxSubmissionInbound(..),
    TxSubmissionProtocolError(..),
  ) where

import           Data.Foldable (foldl')
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import           Data.Word (Word16)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..), forceElemsToWHNF,
                     unsafeNoUnexpectedThunks)

import           Control.Exception (assert)
import           Control.Monad (unless)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadSTM.Strict (checkInvariant)
import           Control.Monad.Class.MonadThrow
import           Control.Tracer (Tracer)

import           Network.TypedProtocol.Pipelined (N, Nat (..))

import           Ouroboros.Network.Protocol.TxSubmission.Server
import           Ouroboros.Network.TxSubmission.Mempool.Reader
                     (MempoolSnapshot (..), TxSubmissionMempoolReader (..))



-- | The consensus layer functionality that the inbound side of the tx
-- submission logic requires.
--
-- This is provided to the tx submission logic by the consensus layer.
--
data TxSubmissionMempoolWriter txid tx idx m =
     TxSubmissionMempoolWriter {

       -- | Compute the transaction id from a transaction.
       --
       -- This is used in the protocol handler to verify a full transaction
       -- matches a previously given transaction id.
       --
       txId          :: tx -> txid,

       -- | Supply a batch of transactions to the mempool. They are either
       -- accepted or rejected individually, but in the order supplied.
       --
       -- The 'txid's of all transactions that were added successfully are
       -- returned.
       mempoolAddTxs :: [tx] -> m [txid]
    }

data TraceTxSubmissionInbound txid tx = TraceTxSubmissionInbound --TODO
  deriving Show

data TxSubmissionProtocolError =
       ProtocolErrorTxNotRequested
     | ProtocolErrorTxIdsNotRequested
  deriving Show

instance Exception TxSubmissionProtocolError where
  displayException ProtocolErrorTxNotRequested =
      "The peer replied with a transaction we did not ask for."
  displayException ProtocolErrorTxIdsNotRequested =
      "The peer replied with more txids than we asked for."


-- | Information maintained internally in the 'txSubmissionInbound' server
-- implementation.
--
data ServerState txid tx = ServerState {
       -- | The number of transaction identifiers that we have requested but
       -- which have not yet been replied to. We need to track this it keep
       -- our requests within the limit on the number of unacknowledged txids.
       --
       requestedTxIdsInFlight :: !Word16,

       -- | Those transactions (by their identifier) that the client has told
       -- us about, and which we have not yet acknowledged. This is kept in
       -- the order in which the client gave them to us. This is the same order
       -- in which we submit them to the mempool (or for this example, the final
       -- result order). It is also the order we acknowledge in.
       unacknowledgedTxIds    :: !(StrictSeq txid),

       -- | Those transactions (by their identifier) that we can request. These
       -- are a subset of the 'unacknowledgedTxIds' that we have not yet
       -- requested. This is not ordered to illustrate the fact that we can
       -- request txs out of order. We also remember the size.
       availableTxids         :: !(Map txid TxSizeInBytes),

       -- | Transactions we have successfully downloaded but have not yet added
       -- to the mempool or acknowledged. This needed because we can request
       -- transactions out of order but must use the original order when adding
       -- to the mempool or acknowledging transactions.
       --
       -- However, it's worth noting that, in a few situations, some of the
       -- transaction IDs in this 'Map' may be mapped to 'Nothing':
       --
       -- * Transaction IDs mapped to 'Nothing' can represent transaction IDs
       --   that were requested, but not received. This can occur because the
       --   client will not necessarily send all of the transactions that we
       --   asked for, but we still need to acknowledge those transactions.
       --
       --   For example, if we request a transaction that no longer exists in
       --   the client's mempool, the client will just exclude it from the
       --   response. However, we still need to acknowledge it (i.e. remove it
       --   from the 'unacknowledgedTxIds') in order to note that we're no
       --   longer awaiting receipt of that transaction.
       --
       -- * Transaction IDs mapped to 'Nothing' can represent transactions
       --   that were not requested from the client because they're already
       --   in the mempool.
       --
       --   For example, if we request some transaction IDs and notice that
       --   some subset of them have are already in the mempool, we wouldn't
       --   want to bother asking for those specific transactions. Therefore,
       --   we would just insert those transaction IDs mapped to 'Nothing' to
       --   the 'bufferedTxs' such that those transactions are acknowledged,
       --   but never actually requested.
       --
       bufferedTxs            :: !(Map txid (Maybe tx)),

       -- | The number of transactions we can acknowledge on our next request
       -- for more transactions. The number here have already been removed from
       -- 'unacknowledgedTxIds'.
       --
       numTxsToAcknowledge    :: !Word16
     }
  deriving (Show, Generic)

instance ( NoUnexpectedThunks txid
         , NoUnexpectedThunks tx
         ) => NoUnexpectedThunks (ServerState txid tx)

initialServerState :: ServerState txid tx
initialServerState = ServerState 0 Seq.empty Map.empty Map.empty 0


txSubmissionInbound
  :: forall txid tx idx m.
     ( Ord txid
     , Ord idx
     , NoUnexpectedThunks txid
     , NoUnexpectedThunks tx
     , MonadSTM m
     , MonadThrow m
     )
  => Tracer m (TraceTxSubmissionInbound txid tx)
  -> Word16         -- ^ Maximum number of unacknowledged txids allowed
  -> TxSubmissionMempoolReader txid tx idx m
  -> TxSubmissionMempoolWriter txid tx idx m
  -> TxSubmissionServerPipelined txid tx m ()
txSubmissionInbound _tracer maxUnacked mpReader mpWriter =
    TxSubmissionServerPipelined $
      continueWithStateM (serverIdle Zero) initialServerState
  where
    -- TODO #1656: replace these fixed limits by policies based on
    -- TxSizeInBytes and delta-Q and the bandwidth/delay product.
    -- These numbers are for demo purposes only, the throughput will be low.
    maxTxIdsToRequest = 3 :: Word16
    maxTxToRequest    = 2 :: Word16

    TxSubmissionMempoolReader{mempoolGetSnapshot} = mpReader

    TxSubmissionMempoolWriter
      { txId
      , mempoolAddTxs
      } = mpWriter

    serverIdle :: forall (n :: N).
                  Nat n
               -> StatefulM (ServerState txid tx) n txid tx m
    serverIdle n = StatefulM $ \st -> case n of
        Zero -> if canRequestMoreTxs st
          then
            -- There are no replies in flight, but we do know some more txs we
            -- can ask for, so lets ask for them and more txids.
            pure $ continueWithState (serverReqTxs Zero) st

          else do
            -- There's no replies in flight, and we have no more txs we can
            -- ask for so the only remaining thing to do is to ask for more
            -- txids. Since this is the only thing to do now, we make this a
            -- blocking call.
            let numTxIdsToRequest = maxTxIdsToRequest `min` maxUnacked
            assert (requestedTxIdsInFlight st == 0
                  && Seq.null (unacknowledgedTxIds st)
                  && Map.null (availableTxids st)
                  && Map.null (bufferedTxs st)) $
              pure $
              SendMsgRequestTxIdsBlocking
                (numTxsToAcknowledge st)
                numTxIdsToRequest
                ()        -- Our result if the client terminates the protocol
                ( collectAndContinueWithState (handleReply Zero) st {
                    numTxsToAcknowledge    = 0,
                    requestedTxIdsInFlight = numTxIdsToRequest
                  }
                . CollectTxIds numTxIdsToRequest
                . NonEmpty.toList)

        Succ n' -> if canRequestMoreTxs st
          then
            -- We have replies in flight and we should eagerly collect them if
            -- available, but there are transactions to request too so we
            -- should not block waiting for replies.
            --
            -- Having requested more transactions, we opportunistically ask
            -- for more txids in a non-blocking way. This is how we pipeline
            -- asking for both txs and txids.
            --
            -- It's important not to pipeline more requests for txids when we
            -- have no txs to ask for, since (with no other guard) this will
            -- put us into a busy-polling loop.
            --
            pure $ CollectPipelined
              (Just (continueWithState (serverReqTxs (Succ n')) st))
              (collectAndContinueWithState (handleReply n') st)

          else
            -- In this case there is nothing else to do so we block until we
            -- collect a reply.
            pure $ CollectPipelined
              Nothing
              (collectAndContinueWithState (handleReply n') st)
      where
        canRequestMoreTxs :: ServerState k tx -> Bool
        canRequestMoreTxs st =
            not (Map.null (availableTxids st))

    handleReply :: forall (n :: N).
                   Nat n
                -> StatefulCollect (ServerState txid tx) n txid tx m
    handleReply n = StatefulCollect $ \st collect -> case collect of
      CollectTxIds reqNo txids -> do
        -- Check they didn't send more than we asked for. We don't need to
        -- check for a minimum: the blocking case checks for non-zero
        -- elsewhere, and for the non-blocking case it is quite normal for
        -- them to send us none.
        let txidsSeq = Seq.fromList (map fst txids)
            txidsMap = Map.fromList txids

        unless (Seq.length txidsSeq <= fromIntegral reqNo) $
          throwM ProtocolErrorTxIdsNotRequested

        -- Upon receiving a batch of new txids we extend our available set,
        -- and extended the unacknowledged sequence.
        --
        -- We also pre-emptively acknowledge those txids that are already in
        -- the mempool. This prevents us from requesting their corresponding
        -- transactions again in the future.
        let st' = st {
          requestedTxIdsInFlight = requestedTxIdsInFlight st - reqNo,
          unacknowledgedTxIds    = unacknowledgedTxIds st <> txidsSeq,
          availableTxids         = availableTxids st <> txidsMap
        }
        mpSnapshot <- atomically mempoolGetSnapshot
        continueWithStateM
          (serverIdle n)
          (acknowledgeTxIdsInMempool st' mpSnapshot)

      CollectTxs txids txs -> do
        -- To start with we have to verify that the txs they have sent us do
        -- correspond to the txs we asked for. This is slightly complicated by
        -- the fact that in general we get a subset of the txs that we asked
        -- for. We should never get a tx we did not ask for. We take a strict
        -- approach to this and check it.
        --
        let txsMap :: Map txid tx
            txsMap = Map.fromList [ (txId tx, tx) | tx <- txs ]

            txidsReceived  = Map.keysSet txsMap
            txidsRequested = Set.fromList txids

        unless (txidsReceived `Set.isSubsetOf` txidsRequested) $
          throwM ProtocolErrorTxNotRequested

            -- We can match up all the txids we requested, with those we
            -- received.
        let txIdsRequestedWithTxsReceived :: Map txid (Maybe tx)
            txIdsRequestedWithTxsReceived =
                Map.map Just txsMap
             <> Map.fromSet (const Nothing) txidsRequested

            -- We still have to acknowledge the txids we were given. This
            -- combined with the fact that we request txs out of order means
            -- our bufferedTxs has to track all the txids we asked for, even
            -- though not all have replies.
            bufferedTxs' = bufferedTxs st <> txIdsRequestedWithTxsReceived

            -- We have to update the unacknowledgedTxIds here eagerly and not
            -- delay it to serverReqTxs, otherwise we could end up blocking in
            -- serverIdle on more pipelined results rather than being able to
            -- move on.

            -- Check if having received more txs we can now confirm any (in
            -- strict order in the unacknowledgedTxIds sequence).
            (acknowledgedTxIds, unacknowledgedTxIds') =
              Seq.spanl (`Map.member` bufferedTxs') (unacknowledgedTxIds st)

            -- If so we can submit the acknowledged txs to our local mempool
            txsReady = foldr (\txid r -> maybe r (:r) (bufferedTxs' Map.! txid))
                             [] acknowledgedTxIds

            -- And remove acknowledged txs from our buffer
            bufferedTxs'' = foldl' (flip Map.delete)
                                   bufferedTxs' acknowledgedTxIds

        _ <- mempoolAddTxs txsReady

        continueWithStateM (serverIdle n) st {
          bufferedTxs         = bufferedTxs'',
          unacknowledgedTxIds = unacknowledgedTxIds',
          numTxsToAcknowledge = numTxsToAcknowledge st
                              + fromIntegral (Seq.length acknowledgedTxIds)
        }

    -- Pre-emptively acknowledge those of the available transaction IDs that
    -- are already in the mempool and return the updated 'ServerState'.
    --
    -- This enables us to effectively filter out transactions that we don't
    -- need to bother requesting from the client since they're already in the
    -- mempool.
    --
    acknowledgeTxIdsInMempool :: ServerState txid tx
                              -> MempoolSnapshot txid tx idx
                              -> ServerState txid tx
    acknowledgeTxIdsInMempool st MempoolSnapshot{mempoolHasTx} =
        -- Return the next 'ServerState'
        st {
          availableTxids      = availableTxids',
          bufferedTxs         = bufferedTxs'',
          unacknowledgedTxIds = unacknowledgedTxIds',
          numTxsToAcknowledge = numTxsToAcknowledge st
                              + fromIntegral (Seq.length acknowledgedTxIds)
        }
      where
        -- Divide the available txs in two: those that are already in the
        -- mempool and those that are not. We'll request some txs from the
        -- latter.
        (ignoredTxids, availableTxids') =
          Map.partitionWithKey
            (\txid _ -> mempoolHasTx txid)
            (availableTxids st)

        -- The txs that we intentionally don't request, because they are
        -- already in the mempool, need to be acknowledged.
        --
        -- So we extend bufferedTxs with those txs (so of course they have
        -- no corresponding reply).
        bufferedTxs' = forceElemsToWHNF $
                       bufferedTxs st
                    <> Map.map (const Nothing) ignoredTxids

        -- Check if having decided not to request more txs we can now
        -- confirm any txids (in strict order in the unacknowledgedTxIds
        -- sequence). This is used in the 'numTxsToAcknowledge' below
        -- which will then be used next time we SendMsgRequestTxIds.
        --
        (acknowledgedTxIds, unacknowledgedTxIds') =
          Seq.spanl (`Map.member` bufferedTxs') (unacknowledgedTxIds st)

        -- If so we can remove acknowledged txs from our buffer
        --
        bufferedTxs'' = foldl' (flip Map.delete)
                                bufferedTxs' acknowledgedTxIds

    serverReqTxs :: forall (n :: N).
                    Nat n
                 -> Stateful (ServerState txid tx) n txid tx m
    serverReqTxs n = Stateful $ \st -> do
        -- TODO: This implementation is deliberately naive, we pick in an
        -- arbitrary order and up to a fixed limit. This is to illustrate
        -- that we can request txs out of order. In the final version we will
        -- try to pick in-order and only pick out of order when we have to.
        -- We will also uses the size of txs in bytes as our limit for
        -- upper and lower watermarks for pipelining. We'll also use the
        -- amount in flight and delta-Q to estimate when we're in danger of
        -- becomming idle, and need to request stalled txs.
        --
        let (txsToRequest, availableTxids') =
              Map.splitAt (fromIntegral maxTxToRequest) (availableTxids st)

        SendMsgRequestTxsPipelined
          (Map.keys txsToRequest)
          (continueWithStateM (serverReqTxIds (Succ n)) st {
             availableTxids = availableTxids'
           })

    serverReqTxIds :: forall (n :: N).
                      Nat n
                   -> StatefulM (ServerState txid tx) n txid tx m
    serverReqTxIds n = StatefulM $ \st -> do
          -- This definition is justified by the fact that the
          -- 'numTxsToAcknowledge' are not included in the
          -- 'unacknowledgedTxIds'.
      let numTxIdsToRequest =
                  (maxUnacked
                    - fromIntegral (Seq.length (unacknowledgedTxIds st))
                    - requestedTxIdsInFlight st)
            `min` maxTxIdsToRequest

      if numTxIdsToRequest > 0
        then pure $ SendMsgRequestTxIdsPipelined
          (numTxsToAcknowledge st)
          numTxIdsToRequest
          (continueWithStateM (serverIdle (Succ n)) st {
                requestedTxIdsInFlight = requestedTxIdsInFlight st
                                       + numTxIdsToRequest,
                numTxsToAcknowledge    = 0
              })
        else continueWithStateM (serverIdle n) st

newtype Stateful s n txid tx m = Stateful (s -> ServerStIdle n txid tx m ())

newtype StatefulM s n txid tx m
  = StatefulM (s -> m (ServerStIdle n txid tx m ()))

newtype StatefulCollect s n txid tx m
  = StatefulCollect (s -> Collect txid tx -> m (ServerStIdle n txid tx m ()))

-- | After checking that there are no unexpected thunks in the provided state,
-- pass it to the provided function.
--
-- See 'checkInvariant' and 'unsafeNoUnexpectedThunks'.
continueWithState :: NoUnexpectedThunks s
                  => Stateful s n txid tx m
                  -> s
                  -> ServerStIdle n txid tx m ()
continueWithState (Stateful f) !st =
    checkInvariant (unsafeNoUnexpectedThunks st) (f st)

-- | A variant of 'continueWithState' to be more easily utilized with
-- 'serverIdle' and 'serverReqTxIds'.
continueWithStateM :: NoUnexpectedThunks s
                   => StatefulM s n txid tx m
                   -> s
                   -> m (ServerStIdle n txid tx m ())
continueWithStateM (StatefulM f) !st =
    checkInvariant (unsafeNoUnexpectedThunks st) (f st)

-- | A variant of 'continueWithState' to be more easily utilized with
-- 'handleReply'.
collectAndContinueWithState :: NoUnexpectedThunks s
                            => StatefulCollect s n txid tx m
                            -> s
                            -> Collect txid tx
                            -> m (ServerStIdle n txid tx m ())
collectAndContinueWithState (StatefulCollect f) !st c =
    checkInvariant (unsafeNoUnexpectedThunks st) (f st c)
