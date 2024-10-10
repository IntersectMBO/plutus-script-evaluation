module Dump (dumpScriptEvents) where

import LedgerEvents.FileWriter qualified as FileWriter
import LedgerStates (
  IndexerState (IndexerState),
  lastCheckpoint,
  makeLedgerStateEventsIndexer,
 )
import Options qualified as O
import Path.IO (ensureDir, makeAbsolute)
import Render qualified
import Streaming (subscribeToChainSyncEvents)
import Types (Checkpoint (Checkpoint))

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
    =<< FileWriter.makeEventIndexer checkpointsDir eventsDir optsEventsPerFile
