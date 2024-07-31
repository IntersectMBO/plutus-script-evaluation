module LedgerStates where

import Cardano.Api.Shelley qualified as C
import Control.Exception (throw)
import Data.Function ((&))
import Data.IORef (newIORef, readIORef, writeIORef)
import Render qualified
import Streaming (ChainSyncEvent (RollBackward, RollForward))

data IndexerState = IndexerState
  { env :: C.Env
  , lastLedgerState :: C.LedgerState
  }

makeLedgerStateEventsIndexer
  :: IndexerState
  -> C.ChainPoint
  -> ((C.ChainPoint, C.LedgerState, [C.LedgerEvent]) -> IO ())
  -> IO (ChainSyncEvent -> IO ())
makeLedgerStateEventsIndexer initialIndexerState startedFrom callback = do
  ref <- newIORef initialIndexerState
  pure \case
    RollForward block@(C.BlockInMode _era (C.Block header _)) _chainTip -> do
      let (C.BlockHeader slot hash _blockNo) = header
      let point = C.ChainPoint slot hash
      putStrLn $ "Roll forward to " <> Render.chainPointSlot point
      indexerState@IndexerState{..} <- readIORef ref
      (newLedgerState, ledgerEvents) <-
        C.applyBlock env lastLedgerState C.FullValidation block
          & either throw pure
      writeIORef ref indexerState{lastLedgerState = newLedgerState}
      callback (point, newLedgerState, ledgerEvents)
    RollBackward point _chainTip
      | point == startedFrom ->
          putStrLn $ "Initial Rollback to: " <> Render.chainPointSlot point
    RollBackward point _chainTip ->
      case point of
        C.ChainPointAtGenesis ->
          fail "Unexpected rollback to genesis"
        C.ChainPoint _slotNo _tip -> do
          fail $ "Unexpected rollback: " <> Render.chainPointSlot point
