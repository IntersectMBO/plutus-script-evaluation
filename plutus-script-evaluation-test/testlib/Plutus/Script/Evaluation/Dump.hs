module Plutus.Script.Evaluation.Dump (dumpScriptEvents) where

import LedgerEvents (makeEventIndexer)
import LedgerStates (
  IndexerState (IndexerState),
  lastCheckpoint,
  makeLedgerStateEventsIndexer,
 )
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
  (env, Checkpoint chainPoint ledgerState) <-
    lastCheckpoint optsConfigPath checkpointsDir

  putStrLn $ Render.startChainPoint chainPoint

  ensureDir checkpointsDir
  subscribeToChainSyncEvents optsSocketPath optsNetworkId [chainPoint]
    =<< makeLedgerStateEventsIndexer (IndexerState env ledgerState) chainPoint
    =<< makeEventIndexer checkpointsDir eventsDir optsEventsPerFile
