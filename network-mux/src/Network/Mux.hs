{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Network.Mux (
      muxStart
    , muxBearerSetState
    , MuxSDU (..)
    , MiniProtocolLimits (..)
    , ProtocolEnum (..)
    , MiniProtocolId (..)
    , MiniProtocolMode (..)
    , MuxBearerState (..)
    , MuxError (..)
    , MuxErrorType (..)
    , RemoteClockModel (..)
    ) where

import           Control.Monad
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Data.Array
import qualified Data.ByteString.Lazy as BL
import           GHC.Stack
import           Text.Printf

import           Network.Mux.Channel
import           Network.Mux.Interface
import           Network.Mux.Egress
import           Network.Mux.Ingress
import           Network.Mux.Types


-- | muxStart starts a mux bearer for the specified protocols corresponding to
-- one of the provided Versions.
--
-- __Isometric flow control: analysis of head-of-line blocking of the ingress side of the multiplexer__
--
-- For each mini-protocol (enumeratated by @ptcl@), mux will create two
-- channels. One for initiator and one for the responder.  Each channel will use
-- a single 'Wanton'.  When it is filled, it is put in a common queue
-- 'tsrQueue'.  This means that the queue is bound by @2 * |ptcl|@.  Every side
-- of a mini-protocol is served by a single 'Wanton': when an applicaiton sends
-- data, the channel will try to put it into the 'Wanton' (which might block).
-- 'Wanton's are taken from the 'tsrQueue' queue by one of mux threads.  This
-- elimnates head of line blocking: each mini-protocol thread can block on
-- puting more bytes into its 'Wanton', but it cannot block the other
-- mini-protocols or the thread that is reading the 'tsrQueue' queue.  This is
-- ensured since the 'muxChannel' will put only a non-empty 'Wanton' to the
-- 'tsrQueue' queue, and on such wantons the queue is never blocked.  This means
-- that  the only way the queue can block is when its empty, which means that
-- none of the mini-protocols wanted to send.  The egress part will read
-- a 'Wanton', take a fixed amount of bytes encode them in as an 'MuxSDU'; if
-- there are leftovers it will put them back in the 'Wanton' and place it at the
-- end of the queue (reading and writting to it will happen in a single STM
-- transaction which assures that the order of requests from a mini-protocol is
-- preserved.
--
-- Properties:
--
-- * at any given time the 'tsrQueue' contains at most one
--   'TranslocationServiceRequest' from a given mini-protocol of the given
--   'MiniProtocolMode', thus the queue contains at most @2 * |ptcl|@
--   translocation requests.
-- * at any given time each @TranslocationServiceRequest@ contains a non-empty
-- 'Wanton'
--
-- TODO: replace MonadSay with iohk-monitoring-framework.
--
muxStart :: forall m appType ptcl a b.  ( MonadAsync m , MonadSay m , MonadSTM
         m , MonadThrow m , MonadThrow (STM m) , MonadMask m
       , Ord ptcl
       , Enum ptcl
       , Bounded ptcl
       , Show ptcl
       , MiniProtocolLimits ptcl
       )
    => MuxApplication appType ptcl m BL.ByteString a b
    -> MuxBearer ptcl m
    -> m ()
muxStart app bearer = do
    tbl <- setupTbl
    tq <- atomically $ newTBQueue 100
    cnt <- newTVarM 0

    let pmss = PerMuxSS tbl tq bearer
        jobs = [ demux pmss
               , mux cnt pmss
               , muxControl pmss ModeResponder
               , muxControl pmss ModeInitiator
               ]
    mjobs <- sequence [ mpsJob cnt pmss ptcl
                      | ptcl <- [minBound..maxBound] ]

    mask $ \unmask -> do
      as <- traverse (async . unmask) (jobs ++ concat mjobs)
      muxBearerSetState bearer Mature
      unmask (void $ waitAnyCancel as)
      muxBearerSetState bearer Dead

  where
    -- Construct the array of TBQueues, one for each protocol id, and each mode
    setupTbl :: m (MiniProtocolDispatch ptcl m)
    setupTbl = MiniProtocolDispatch
            -- cover full range of type (MiniProtocolId ptcl, MiniProtocolMode)
             . array (minBound, maxBound)
           <$> sequence [ do q <- atomically (newTVar BL.empty)
                             return ((ptcl, mode), q)
                        | ptcl <- [minBound..maxBound]
                        , mode <- [ModeInitiator, ModeResponder] ]


    mpsJob
      :: TVar m Int
      -> PerMuxSharedState ptcl m
      -> ptcl
      -> m [m ()]
    mpsJob cnt pmss mpdId = do

        initiatorChannel <- muxChannel pmss
                              (AppProtocolId mpdId)
                              ModeInitiator
                              cnt

        responderChannel <- muxChannel pmss
                              (AppProtocolId mpdId)
                              ModeResponder
                              cnt

        return $ case app of
          MuxInitiatorApplication initiator -> [ initiator mpdId initiatorChannel >> mpsJobExit cnt ]
          MuxResponderApplication responder -> [ responder mpdId responderChannel >> mpsJobExit cnt ]
          MuxInitiatorAndResponderApplication initiator responder
                                            -> [ initiator mpdId initiatorChannel >> mpsJobExit cnt
                                               , responder mpdId responderChannel >> mpsJobExit cnt
                                               ]

    -- cnt represent the number of SDUs that are queued but not yet sent.  Job
    -- threads will be prevented from exiting until all SDUs have been
    -- transmitted unless an exception/error is encounter. In that case all
    -- jobs will be cancelled directly.
    mpsJobExit :: TVar m Int -> m ()
    mpsJobExit cnt = do
        muxBearerSetState bearer Dying
        atomically $ do
            c <- readTVar cnt
            unless (c == 0) retry

muxControl :: (HasCallStack, MonadSTM m, MonadSay m, MonadThrow m, Ord ptcl, Enum ptcl
              , MiniProtocolLimits ptcl)
           => PerMuxSharedState ptcl m
           -> MiniProtocolMode
           -> m ()
muxControl pmss md = do
    _ <- atomically $ do
        buf <- readTVar (ingressQueue (dispatchTable pmss) Muxcontrol md)
        when (buf == BL.empty)
            retry
    throwM $ MuxError MuxControlProtocolError "MuxControl message on mature MuxBearer" callStack

-- | muxChannel creates a duplex channel for a specific 'MiniProtocolId' and
-- 'MiniProtocolMode'.
--
muxChannel
    :: forall m ptcl.
       ( MonadSTM m
       , MonadSay m
       , MonadThrow m
       , Ord ptcl
       , Enum ptcl
       , Show ptcl
       , MiniProtocolLimits ptcl
       , HasCallStack
       )
    => PerMuxSharedState ptcl m
    -> MiniProtocolId ptcl
    -> MiniProtocolMode
    -> TVar m Int
    -> m (Channel m BL.ByteString)
muxChannel pmss mid md cnt = do
    w <- newEmptyTMVarM
    return $ Channel { send = send (Wanton w)
                     , recv}
  where
    send :: Wanton m
         -> BL.ByteString
         -> m ()
    send want@(Wanton w) encoding = do
        -- We send CBOR encoded messages by encoding them into by ByteString
        -- forwarding them to the 'mux' thread, see 'Desired servicing semantics'.
        -- This check is dependant on the good will of the sender and a receiver can't
        -- assume that it will never receive messages larger than maximumMessageSize.
        --say $ printf "send mid %s mode %s" (show mid) (show md)
        when (BL.length encoding > maximumMessageSize mid) $
            throwM $ MuxError MuxTooLargeMessage
                (printf "Attempting to send a message of size %d on %s %s" (BL.length encoding)
                        (show mid) (show $ md))
                callStack
        atomically $ modifyTVar' cnt (+ 1)
        atomically $ putTMVar w encoding
        atomically $ writeTBQueue (tsrQueue pmss) (TLSRDemand mid md want)

    recv :: m (Maybe BL.ByteString)
    recv = do
        -- We receive CBOR encoded messages as ByteStrings (possibly partial) from the
        -- matching ingress queueu. This is the same queue the 'demux' thread writes to.
        blob <- atomically $ do
            let q = ingressQueue (dispatchTable pmss) mid md
            blob <- readTVar q
            if blob == BL.empty
                then retry
                else writeTVar q BL.empty >> return blob
        -- say $ printf "recv mid %s mode %s blob len %d" (show mid) (show md) (BL.length blob)
        return $ Just blob

muxBearerSetState :: (MonadSTM m, Ord ptcl, Enum ptcl, Bounded ptcl)
                  => MuxBearer ptcl m
                  -> MuxBearerState
                  -> m ()
muxBearerSetState bearer newState = atomically $ writeTVar (state bearer) newState