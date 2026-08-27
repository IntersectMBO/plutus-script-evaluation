module LedgerStates where

import Cardano.Api (BlockNo, FileDirection (In), NodeConfigFile, docToString)
import Cardano.Api qualified as C
import Cardano.Api.LedgerState qualified as C
import Control.Exception (throwIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Function ((&))
import Data.IORef (newIORef, readIORef, writeIORef)
import FileStorage (Order (Asc))
import FileStorage qualified
import Path (Abs, Dir, Path)
import Render qualified
import Streaming (ChainSyncEvent (RollBackward, RollForward))
import System.FS.API (MountPoint (MountPoint), SomeHasFS (SomeHasFS))
import System.FS.IO (ioHasFS)
import Types (Checkpoint (Checkpoint))

data IndexerState = IndexerState
  { env :: C.Env
  , lastLedgerState :: C.LedgerState
  }

makeLedgerStateEventsIndexer
  :: IndexerState
  -> C.ChainPoint
  -> ((BlockNo, Checkpoint, [C.LedgerEvent]) -> IO ())
  -> IO (ChainSyncEvent -> IO ())
makeLedgerStateEventsIndexer initialIndexerState startedFrom callback = do
  ref <- newIORef initialIndexerState
  pure \case
    RollForward block@(C.BlockInMode _era (C.getBlockHeader -> header)) _chainTip -> do
      let (C.BlockHeader slot hash blockNo) = header
      let point = C.ChainPoint slot hash
      indexerState@IndexerState{..} <- readIORef ref
      (newLedgerState, ledgerEvents) <-
        C.applyBlock env lastLedgerState C.FullValidation block
          & either throwIO pure
      writeIORef ref indexerState{lastLedgerState = newLedgerState}
      callback (blockNo, Checkpoint point newLedgerState, ledgerEvents)
    RollBackward point _chainTip
      | point == startedFrom ->
          putStrLn $ "Initial Rollback to: " <> Render.chainPointSlot point
    RollBackward point _chainTip ->
      case point of
        C.ChainPointAtGenesis ->
          fail "Unexpected rollback to genesis"
        C.ChainPoint _slotNo _tip -> do
          fail $ "Unexpected rollback: " <> Render.chainPointSlot point

{- | cardano-api 11 reads the node config through an abstract file system.
Mounting at the root means the @--config@ path is used as given, so it has to
be absolute.
-}
nodeConfigFs :: SomeHasFS IO
nodeConfigFs = SomeHasFS (ioHasFS (MountPoint "/"))

lastCheckpoint :: NodeConfigFile 'In -> Path Abs Dir -> IO (C.Env, Checkpoint)
lastCheckpoint optsConfigPath checkpointsDir = do
  (env, ledgerStateAtGenesis) <-
    runExceptT (C.initialLedgerState nodeConfigFs optsConfigPath)
      >>= either (fail . docToString . C.prettyError) pure
  checkpoints <- FileStorage.listFilesSorted Asc checkpointsDir
  (env,) <$> case checkpoints of
    [] -> do
      putStrLn "No checkpoint found, starting from genesis"
      pure $ Checkpoint C.ChainPointAtGenesis ledgerStateAtGenesis
    someCheckpoints -> do
      let (_lastSlotNo, point) = last someCheckpoints
      putStrLn $ "Reading the last checkpoint file: " <> show point
      FileStorage.readLedgerState point
