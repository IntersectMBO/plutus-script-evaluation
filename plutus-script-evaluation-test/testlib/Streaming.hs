module Streaming (
  subscribeToChainSyncEvents,
  ChainSyncEvent (..),
  ChainSyncEventException (..),
  RollbackException (..),
) where

import Cardano.Api (SocketPath)
import Cardano.Api.ChainSync.ClientPipelined (
  Nat (Succ, Zero),
  PipelineDecision (Collect),
 )
import Cardano.Api.Shelley qualified as C
import Cardano.Slotting.Slot (WithOrigin (At, Origin))
import Control.Concurrent.Async (
  ExceptionInLinkedThread (ExceptionInLinkedThread),
  link,
  withAsync,
 )
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (
  Exception,
  SomeException (SomeException),
  handle,
  throw,
 )
import Control.Monad (forever)
import GHC.Generics (Generic)
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined qualified as CSP
import Ouroboros.Network.Protocol.ChainSync.PipelineDecision (pipelineDecisionMax)

{- | `subscribeToChainSyncEvents` uses the chain-sync mini-protocol to
connect to a locally running node and fetch blocks from the given
starting point.
-}
subscribeToChainSyncEvents
  :: SocketPath
  -- ^ Path to the node socket
  -> C.NetworkId
  -> [C.ChainPoint]
  -- ^ The point on the chain to start streaming from
  -> (ChainSyncEvent -> IO ())
  -- ^ Callback receiving the blocks
  -> IO ()
subscribeToChainSyncEvents socketPath networkId points callback = do
  nextChainSyncEvent <- newEmptyMVar
  let
    connection =
      C.connectToLocalNode
        C.LocalNodeConnectInfo
          { C.localConsensusModeParams =
              -- This a parameter needed only for the Byron era.
              -- Since the Byron era is over and the parameter has never
              -- changed it is ok to hardcode this.
              C.CardanoModeParams (C.EpochSlots 21600)
          , C.localNodeNetworkId = networkId
          , C.localNodeSocketPath = socketPath
          }
        C.LocalNodeClientProtocols
          { C.localChainSyncClient =
              C.LocalChainSyncClientPipelined . CSP.ChainSyncClientPipelined $
                pure $
                  CSP.SendMsgFindIntersect
                    points
                    CSP.ClientPipelinedStIntersect
                      { CSP.recvMsgIntersectFound = \_point _tip ->
                          pure $ requestMore Origin Origin Zero
                      , CSP.recvMsgIntersectNotFound =
                          throw NoIntersectionFound
                      }
          , C.localStateQueryClient = Nothing
          , C.localTxMonitoringClient = Nothing
          , C.localTxSubmissionClient = Nothing
          }
    requestMore
      :: WithOrigin C.BlockNo
      -> WithOrigin C.BlockNo
      -> Nat n
      -> CSP.ClientPipelinedStIdle n C.BlockInMode C.ChainPoint C.ChainTip IO ()
    requestMore clientTip serverTip rqsInFlight =
      case pipelineDecisionMax 100 rqsInFlight clientTip serverTip of
        -- handle a response
        Collect -> case rqsInFlight of
          Succ inFlight ->
            CSP.CollectResponse
              Nothing
              CSP.ClientStNext
                { CSP.recvMsgRollForward = \bim ct -> do
                    putMVar nextChainSyncEvent (RollForward bim ct)
                    pure $
                      requestMore
                        (At $ bimBlockNo bim)
                        (fromChainTip ct)
                        inFlight
                , CSP.recvMsgRollBackward = \cp ct -> do
                    putMVar nextChainSyncEvent (RollBackward cp ct)
                    pure $ requestMore Origin (fromChainTip ct) inFlight
                }
        -- fire more requests
        _ ->
          CSP.SendMsgRequestNextPipelined
            (pure ())
            (requestMore clientTip serverTip (Succ rqsInFlight))
  -- Let's rethrow exceptions from the client thread unwrapped, so that the
  -- callback does not have to know anything about async
  handle (\(ExceptionInLinkedThread _async (SomeException e)) -> throw e) $
    withAsync connection \a -> do
      -- Make sure all exceptions in the client thread
      -- are passed to the callback thread
      link a
      -- Run the callback forever
      forever (takeMVar nextChainSyncEvent >>= callback)
 where
  bimBlockNo :: C.BlockInMode -> C.BlockNo
  bimBlockNo (C.BlockInMode _era (C.Block (C.BlockHeader _ _ blockNo) _)) =
    blockNo

  fromChainTip :: C.ChainTip -> WithOrigin C.BlockNo
  fromChainTip ct = case ct of
    C.ChainTipAtGenesis -> Origin
    C.ChainTip _ _ bno -> At bno

data ChainSyncEvent
  = RollForward C.BlockInMode C.ChainTip
  | RollBackward C.ChainPoint C.ChainTip
  deriving stock (Show, Generic)

data ChainSyncEventException = NoIntersectionFound
  deriving stock (Show)
  deriving anyclass (Exception)

data RollbackException = RollbackLocationNotFound C.ChainPoint C.ChainTip
  deriving stock (Show)
  deriving anyclass (Exception)
