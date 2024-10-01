module Streaming (
  subscribeToChainSyncEvents,
  ChainSyncEvent (..),
  ChainSyncEventException (..),
  RollbackException (..),
) where

import Cardano.Api (SocketPath)
import Cardano.Api.ChainSync.Client (
  ClientStIdle (SendMsgFindIntersect, SendMsgRequestNext),
  ClientStIntersect (
    ClientStIntersect,
    recvMsgIntersectFound,
    recvMsgIntersectNotFound
  ),
  ClientStNext (
    ClientStNext,
    recvMsgRollBackward,
    recvMsgRollForward
  ),
 )
import Cardano.Api.Shelley qualified as C
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (WithOrigin (At, Origin), withOrigin)
import Control.Concurrent.Async (ExceptionInLinkedThread (..), link, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (Exception, SomeException (..), handle, throw)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import GHC.Word (Word64)
import System.Exit (exitSuccess)

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
              C.LocalChainSyncClient . C.ChainSyncClient . pure $
                SendMsgFindIntersect points onIntersect
          , C.localStateQueryClient = Nothing
          , C.localTxMonitoringClient = Nothing
          , C.localTxSubmissionClient = Nothing
          }

    onIntersect =
      ClientStIntersect
        { recvMsgIntersectFound = \chainPoint tip ->
            C.ChainSyncClient do
              putMVar nextChainSyncEvent (RollBackward chainPoint tip)
              sendRequestNext
        , recvMsgIntersectNotFound = throw NoIntersectionFound
        }

    sendRequestNext =
      pure $ SendMsgRequestNext (pure ()) do
        ClientStNext
          { recvMsgRollForward = \blockInMode tip ->
              C.ChainSyncClient do
                let blockNo = bimBlockNo blockInMode
                let volatileTip = fromChainTip tip
                let tipBlockNo = withOrigin 0 unBlockNo volatileTip
                let immutableTip = BlockNo (tipBlockNo - securityParam)

                putStrLn $
                  "Roll forward to "
                    ++ show (bimSlotNo blockInMode)
                    ++ ", Current "
                    ++ show blockNo
                    ++ ", Tip "
                    ++ show volatileTip
                putMVar nextChainSyncEvent (RollForward blockInMode tip)

                when (blockNo > immutableTip) $ liftIO do
                  putStrLn $
                    "Reached immutable tip ("
                      ++ show immutableTip
                      ++ "), exiting. "
                  exitSuccess

                sendRequestNext
          , recvMsgRollBackward = \chainPoint tip ->
              C.ChainSyncClient do
                putMVar nextChainSyncEvent (RollBackward chainPoint tip)
                sendRequestNext
          }

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

  bimSlotNo :: C.BlockInMode -> C.SlotNo
  bimSlotNo (C.BlockInMode _era (C.Block (C.BlockHeader slot _ _) _)) = slot

  fromChainTip :: C.ChainTip -> WithOrigin C.BlockNo
  fromChainTip tip = case tip of
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

{- | The security parameter is a non-updatable one:
After how many blocks is the blockchain considered to be final,
and thus can no longer be rolled back
(i.e. what is the maximum allowable length of any chain fork).
-}
securityParam :: Word64
securityParam = 2160
