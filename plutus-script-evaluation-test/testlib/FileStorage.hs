module FileStorage where

import Cardano.Api.Shelley (
  ChainPoint,
  SlotNo (SlotNo),
  chainPointToHeaderHash,
  chainPointToSlotNo,
  unSlotNo,
 )
import Codec.Serialise qualified as CBOR
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  addExtension,
  filename,
  parseRelFile,
  toFilePath,
  (</>),
 )
import Path.IO (listDir)
import Plutus.Script.Evaluation.Types (Checkpoint (Checkpoint))
import PlutusLedgerApi.Test.EvaluationEvent (ScriptEvaluationEvents)
import Render qualified
import Text.Printf (printf)
import Text.Read (readMaybe)

saveLedgerState :: Path Abs Dir -> Checkpoint -> IO ()
saveLedgerState checkpointDir checkpoint@(Checkpoint point _state) = do
  let slot = maybe 0 unSlotNo (chainPointToSlotNo point)
      hash = maybe "genesis" Render.blockHash (chainPointToHeaderHash point)
  file :: Path Rel File <-
    addExtension ".state" =<< do
      parseRelFile (printf "%016d-%s" slot hash)
        & maybe (fail "Can't create ledger state file path") pure
  let filePath = toFilePath (checkpointDir </> file)
  putStrLn $ "Writing ledger state to " <> filePath
  CBOR.writeFileSerialise filePath checkpoint

readLedgerState :: Path Abs File -> IO Checkpoint
readLedgerState file = do
  CBOR.readFileDeserialise (toFilePath file)

saveEvents :: Path Abs Dir -> ChainPoint -> ScriptEvaluationEvents -> IO ()
saveEvents eventsDir point events = do
  let slot = maybe 0 unSlotNo (chainPointToSlotNo point)
      hash = maybe "genesis" Render.blockHash (chainPointToHeaderHash point)
  file :: Path Rel File <-
    addExtension ".event" =<< do
      parseRelFile (printf "%016d-%s" slot hash)
        & maybe (fail "Can't create events file path") pure
  let filePath = toFilePath (eventsDir </> file)
  putStrLn $ "Writing events to " <> filePath
  CBOR.writeFileSerialise filePath events

ledgerStates :: Path Abs Dir -> IO (Map SlotNo (Path Abs File))
ledgerStates checkpoints = do
  (_dirs, files) <- listDir checkpoints
  pure $
    Map.fromList
      [ (slot, file)
      | file <- files
      , slot <- maybeToList (parseSlotNo file)
      ]
 where
  parseSlotNo :: Path Abs File -> Maybe SlotNo
  parseSlotNo fp = do
    let name = toFilePath (filename fp)
    slot <- readMaybe (takeWhile (/= '-') (dropWhile (== '0') name))
    pure (SlotNo slot)
