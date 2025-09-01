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
import Data.Maybe (fromMaybe)
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

  let slot = fromMaybe 0 (chainPointToSlotNo chainPoint)
  numDeleted <- Db.deleteFromSlotOnwards conn slot
  putStrLn [i|Deleted #{numDeleted} events (slot >= #{slot}).|]

  ensureDir checkpointsDir
  subscribeToChainSyncEvents optsSocketPath optsNetworkId [chainPoint]
    =<< makeLedgerStateEventsIndexer (IndexerState env ledgerState) chainPoint
    =<< makeEventIndexer checkpointsDir conn
