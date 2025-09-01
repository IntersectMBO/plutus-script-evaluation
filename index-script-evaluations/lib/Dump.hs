module Dump (
  dumpScriptEvents,
  Options (..),
) where

import Cardano.Api (
  FileDirection (In),
  NetworkId,
  NodeConfigFile,
  SocketPath,
 )
import Data.Word (Word64)
import LedgerEvents.FileWriter qualified as FileWriter
import LedgerStates (
  IndexerState (IndexerState),
  lastCheckpoint,
  makeLedgerStateEventsIndexer,
 )
import Path (Dir, SomeBase)
import Path.IO (ensureDir, makeAbsolute)
import Render qualified
import Streaming (subscribeToChainSyncEvents)
import Types (Checkpoint (Checkpoint))

data Options = Options
  { optsConfigPath :: NodeConfigFile In
  , optsSocketPath :: SocketPath
  , optsNetworkId :: NetworkId
  , optsEventsPerFile :: Word64
  , optsDumpDir :: SomeBase Dir
  , optsCheckpointDir :: SomeBase Dir
  }
  deriving (Show)

{- | Stream blocks from a local node, and periodically dump ledger events
and checkpoint ledger state.
-}
dumpScriptEvents :: Options -> IO ()
dumpScriptEvents Options{..} = do
  checkpointsDir <- makeAbsolute optsCheckpointDir
  eventsDir <- makeAbsolute optsDumpDir
  (env, Checkpoint chainPoint ledgerState) <-
    lastCheckpoint optsConfigPath checkpointsDir

  putStrLn $ Render.startChainPoint chainPoint

  ensureDir checkpointsDir
  subscribeToChainSyncEvents optsSocketPath optsNetworkId [chainPoint]
    =<< makeLedgerStateEventsIndexer (IndexerState env ledgerState) chainPoint
    =<< FileWriter.makeEventIndexer checkpointsDir eventsDir optsEventsPerFile
