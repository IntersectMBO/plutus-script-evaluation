module LedgerStates where

import Cardano.Api (BlockNo, File (File), FileDirection (In), NodeConfigFile, docToString, unFile)
import Cardano.Api qualified as C
import Cardano.Api.LedgerState qualified as LS
import Control.Exception (throwIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Function ((&))
import Data.IORef (newIORef, readIORef, writeIORef)
import FileStorage (Order (Asc))
import FileStorage qualified
import Path (Abs, Dir, Path, toFilePath)
import Path.IO (resolveFile')
import Render qualified
import Streaming (ChainSyncEvent (RollBackward, RollForward))
import System.FS.API (MountPoint (MountPoint), SomeHasFS (SomeHasFS))
import System.FS.IO (ioHasFS)
import Types (Checkpoint (Checkpoint))

data IndexerState = IndexerState
  { env :: LS.Env
  , lastLedgerState :: LS.LedgerState
  }

makeLedgerStateEventsIndexer
  :: IndexerState
  -> C.ChainPoint
  -> ((BlockNo, Checkpoint, [LS.LedgerEvent]) -> IO ())
  -> IO (ChainSyncEvent -> IO ())
makeLedgerStateEventsIndexer initialIndexerState startedFrom callback = do
  ref <- newIORef initialIndexerState
  pure \case
    RollForward block@(C.BlockInMode _era (C.getBlockHeader -> header)) _chainTip -> do
      let (C.BlockHeader slot hash blockNo) = header
      let point = C.ChainPoint slot hash
      indexerState@IndexerState{..} <- readIORef ref
      (newLedgerState, ledgerEvents) <-
        LS.applyBlock env lastLedgerState LS.FullValidation block
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
Mounting at the root means paths are passed to it unchanged, so every path
handed to `LS.initialLedgerState` has to be absolute.
-}
nodeConfigFs :: SomeHasFS IO
nodeConfigFs = SomeHasFS (ioHasFS (MountPoint "/"))

lastCheckpoint :: NodeConfigFile 'In -> Path Abs Dir -> IO (LS.Env, Checkpoint)
lastCheckpoint optsConfigPath checkpointsDir = do
  -- `--config` is parsed from a raw string, so it can be relative. Resolve it
  -- against the working directory; `nodeConfigFs` would otherwise read it as a
  -- path from the filesystem root.
  configPath <- File . toFilePath <$> resolveFile' (unFile optsConfigPath)
  (env, ledgerStateAtGenesis) <-
    runExceptT (LS.initialLedgerState nodeConfigFs configPath)
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
