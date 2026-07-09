module Load (
  loadScriptEvents,
  Options (..),
) where

import Cardano.Api (
  FileDirection (In),
  NetworkId,
  NodeConfigFile,
  SocketPath,
  chainPointToSlotNo,
 )
import Data.ByteString (ByteString)
import Data.String.Interpolate (i)
import Database qualified as Db
import Database.PostgreSQL.Simple (Connection)
import LedgerEvents.DbLoader (makeEventIndexer)
import LedgerStates (
  IndexerState (..),
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
  , optsCheckpointDir :: SomeBase Dir
  , optsDatabaseConnStr :: ByteString
  }
  deriving (Show)

{- | Stream blocks from a local node inserting script evaluation ledger events
into the database.
-}
loadScriptEvents :: Connection -> Options -> IO ()
loadScriptEvents conn Options{..} = do
  checkpointsDir <- makeAbsolute optsCheckpointDir
  (env, Checkpoint chainPoint ledgerState) <-
    lastCheckpoint optsConfigPath checkpointsDir

  putStrLn $ Render.startChainPoint chainPoint

  -- Clear events that will be re-inserted on replay. From a real checkpoint at
  -- slot S the node replays blocks strictly after S, so block S is retained and
  -- we delete only slot > S. From genesis (no checkpoint) the whole chain is
  -- replayed, so every existing row is deleted.
  case chainPointToSlotNo chainPoint of
    Nothing -> do
      numDeleted <- Db.deleteAllEvents conn
      putStrLn [i|Deleted #{numDeleted} events (all; replaying from genesis).|]
    Just slot -> do
      numDeleted <- Db.deleteAfterSlot conn slot
      putStrLn [i|Deleted #{numDeleted} events (slot > #{slot}).|]

  ensureDir checkpointsDir
  subscribeToChainSyncEvents optsSocketPath optsNetworkId [chainPoint]
    =<< makeLedgerStateEventsIndexer (IndexerState env ledgerState) chainPoint
    =<< makeEventIndexer checkpointsDir conn
