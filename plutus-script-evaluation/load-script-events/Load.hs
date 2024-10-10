module Load (loadScriptEvents) where

import Cardano.Api.Shelley (chainPointToSlotNo)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Database qualified as Db
import Database.PostgreSQL.Simple (connectPostgreSQL)
import LedgerEvents.DbLoader (makeEventIndexer)
import LedgerStates (
  IndexerState (..),
  lastCheckpoint,
  makeLedgerStateEventsIndexer,
 )
import Options qualified as O
import Path.IO (ensureDir, makeAbsolute)
import Render qualified
import Streaming (subscribeToChainSyncEvents)
import Types (Checkpoint (Checkpoint))

{- | Stream blocks from a local node inserting script evaluation ledger events
into the database.
-}
loadScriptEvents :: O.Options -> IO ()
loadScriptEvents O.Options{..} = do
  checkpointsDir <- makeAbsolute optsCheckpointDir
  (env, Checkpoint chainPoint ledgerState) <-
    lastCheckpoint optsConfigPath checkpointsDir

  putStrLn $ Render.startChainPoint chainPoint

  conn <- connectPostgreSQL optsDatabaseConnStr
  let slot = fromMaybe 0 (chainPointToSlotNo chainPoint)
  numDeleted <- Db.deleteFromSlotOnwards conn slot
  putStrLn [i|Deleted #{numDeleted} events (slot >= #{slot}).|]

  ensureDir checkpointsDir
  subscribeToChainSyncEvents optsSocketPath optsNetworkId [chainPoint]
    =<< makeLedgerStateEventsIndexer (IndexerState env ledgerState) chainPoint
    =<< makeEventIndexer checkpointsDir conn
