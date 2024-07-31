module Plutus.Script.Evaluation.Dump (dumpScriptEvents) where

import Cardano.Api (docToString)
import Cardano.Api.Shelley qualified as C
import Control.Monad.Trans.Except (runExceptT)
import Data.Map.Strict qualified as Map
import FileStorage qualified
import LedgerEvents (makeEventIndexer)
import LedgerStates (IndexerState (IndexerState), makeLedgerStateEventsIndexer)
import Path.IO (ensureDir, makeAbsolute)
import Plutus.Script.Evaluation.Options qualified as O
import Plutus.Script.Evaluation.Types (Checkpoint (Checkpoint))
import Render qualified
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
        putStrLn $ "Reading the last checkpoint file: " <> show lastCheckpoint
        FileStorage.readLedgerState lastCheckpoint

  putStrLn $ Render.startChainPoint chainPoint

  ensureDir checkpointsDir
  subscribeToChainSyncEvents optsSocketPath optsNetworkId [chainPoint]
    =<< makeLedgerStateEventsIndexer (IndexerState env ledgerState) chainPoint
    =<< makeEventIndexer checkpointsDir eventsDir optsEventsPerFile
