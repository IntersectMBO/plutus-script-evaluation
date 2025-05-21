module Streaming (
  subscribeToChainSyncEvents,
  ChainSyncEvent (..),
  ChainSyncEventException (..),
  RollbackException (..),
) where

import Cardano.Api (SocketPath)
import Cardano.Api.ChainSync.Client (
  ClientStIdle (SendMsgDone, SendMsgFindIntersect, SendMsgRequestNext),
  ClientStIntersect (
    ClientStIntersect,
    recvMsgIntersectFound,
    recvMsgIntersectNotFound
  ),
  ClientStNext (ClientStNext, recvMsgRollBackward, recvMsgRollForward),
 )
import Cardano.Api.Shelley qualified as C
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (WithOrigin (At, Origin), withOrigin)
import Control.Exception (Exception, throw)
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import GHC.Word (Word64)

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
subscribeToChainSyncEvents socketPath networkId points callback =
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
 where
  onIntersect =
    ClientStIntersect
      { recvMsgIntersectFound = \chainPoint tip ->
          C.ChainSyncClient do
            callback (RollBackward chainPoint tip)
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
              callback (RollForward blockInMode tip)

              if blockNo < immutableTip
                then sendRequestNext
                else liftIO do
                  SendMsgDone ()
                    <$ putStrLn
                      ( "Reached immutable tip ("
                          ++ show immutableTip
                          ++ "), exiting. "
                      )
        , recvMsgRollBackward = \chainPoint tip ->
            C.ChainSyncClient do
              callback (RollBackward chainPoint tip)
              sendRequestNext
        }

  bimBlockNo :: C.BlockInMode -> C.BlockNo
  bimBlockNo
    (C.BlockInMode _era (C.getBlockHeader -> C.BlockHeader _ _ blockNo)) =
      blockNo

  bimSlotNo :: C.BlockInMode -> C.SlotNo
  bimSlotNo (C.BlockInMode _era (C.getBlockHeader -> C.BlockHeader slot _ _)) =
    slot

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

{- |
After how many blocks is the blockchain considered to be final, and thus can no
longer be rolled back (i.e. what is the maximum allowable length of any chain
fork).

The ouroboros-consensus security parameter is a non-updatable one and has value
of 2160. For the purposes of plutus script events evaluation this is too
conservative: in fact, the biggest rollback recorded on the mainnet is 3 blocks.

10 blocks seems to be a reasonable value for our purposes:
- it is small enough to have fresh data for the evaluation.
- its is big enough to minimize the risk of a rollback.
-}
securityParam :: Word64
securityParam = 10
