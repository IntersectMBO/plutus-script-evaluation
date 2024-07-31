module Plutus.Script.Evaluation.Dump (dumpScriptEvents) where

import Cardano.Api (docToString)
import Cardano.Api.Shelley qualified as C
import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString.Base16 qualified as B16
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (__i)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import FileStorage qualified
import LedgerEvents (makeEventIndexer)
import LedgerStates (makeLedgerStateEventsIndexer)
import Path.IO (ensureDir, makeAbsolute)
import Plutus.Script.Evaluation.Options qualified as O
import Plutus.Script.Evaluation.Types (Checkpoint (Checkpoint))
import Streaming (subscribeToChainSyncEvents)

{- | Stream blocks from a local node, and periodically dump ledger events
and checkpoint ledger state.
-}
dumpScriptEvents :: O.Options -> IO ()
dumpScriptEvents O.Options{..} = do
  checkpointsDir <- makeAbsolute optsCheckpointDir
  eventsDir <- makeAbsolute optsDumpDir
  (env, ledgerStateAtGenesis) <-
    runExceptT (C.initialLedgerState optsConfigPath)
      >>= either (fail . docToString . C.prettyError) pure

  Checkpoint chainPoint ledgerState <- do
    checkpoints <- FileStorage.ledgerStates checkpointsDir
    case Map.lookupMax checkpoints of
      Nothing -> do
        putStrLn "No checkpoint found, starting from genesis"
        pure $ Checkpoint C.ChainPointAtGenesis ledgerStateAtGenesis
      Just (_lastSlotNo, lastCheckpoint) -> do
        putStrLn $
          "Reading the last checkpoint file: " <> show lastCheckpoint
        FileStorage.readLedgerState lastCheckpoint

  let slotNo = C.chainPointToSlotNo chainPoint
      headerHash = C.chainPointToHeaderHash chainPoint
  putStrLn
    [__i|
      Starting from checkpoint:
        Slot: #{maybe "Genesis" show slotNo}
        Hash: #{maybe "Genesis" renderBlockHash headerHash}
    |]

  ensureDir checkpointsDir
  subscribeToChainSyncEvents optsSocketPath optsNetworkId [chainPoint]
    =<< makeLedgerStateEventsIndexer env ledgerState
    =<< makeEventIndexer checkpointsDir eventsDir optsEventsPerFile

renderBlockHash :: C.Hash C.BlockHeader -> Text.Text
renderBlockHash = Text.decodeLatin1 . B16.encode . C.serialiseToRawBytes
