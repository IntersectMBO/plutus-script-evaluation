module LedgerStates where

import Cardano.Api.Shelley qualified as C
import Control.Exception (throw)
import Data.Function ((&))
import Data.IORef (newIORef, readIORef, writeIORef)
import Streaming (ChainSyncEvent (RollBackward, RollForward))

data LedgerStateHistory = LedgerStateHistory
  { lastLedgerState :: C.LedgerState
  , checkpointsDir :: FilePath
  , env :: C.Env
  }

makeLedgerStateEventsIndexer
  :: C.Env
  -> C.LedgerState
  -> ((C.ChainPoint, C.LedgerState, [C.LedgerEvent]) -> IO ())
  -> IO (ChainSyncEvent -> IO ())
makeLedgerStateEventsIndexer env initialLedgerState callback = do
  ref <- newIORef (env, initialLedgerState)
  pure \case
    RollForward block@(C.BlockInMode _era (C.Block header _)) _chainTip -> do
      let (C.BlockHeader slot hash _blockNo) = header
      let chainPoint = C.ChainPoint slot hash
      putStrLn $
        "Roll forward to "
          <> maybe "Genesis" show (C.chainPointToSlotNo chainPoint)
      (env', ledgerState) <- readIORef ref
      (newLedgerState, ledgerEvents) <-
        C.applyBlock env' ledgerState C.FullValidation block
          & either throw pure

      writeIORef ref (env', newLedgerState)
      callback (chainPoint, newLedgerState, ledgerEvents)
    RollBackward chainPoint chainTip -> do
      case chainPoint of
        C.ChainPointAtGenesis ->
          fail "Rollback to genesis"
        C.ChainPoint _slotNo _tip -> do
          putStrLn "!!!"
          putStrLn $
            "Rollback: "
              <> show chainPoint
              <> ", tip: "
              <> show chainTip
          putStrLn "!!!"
