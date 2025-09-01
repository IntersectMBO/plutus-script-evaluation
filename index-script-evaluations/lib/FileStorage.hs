module FileStorage where

import Cardano.Api (
  ChainPoint,
  SlotNo (SlotNo),
  chainPointToHeaderHash,
  chainPointToSlotNo,
  unSlotNo,
 )
import Codec.Serialise qualified as CBOR
import Data.Foldable (for_)
import Data.Function ((&))
import Data.List (sortBy)
import Data.Maybe (maybeToList)
import Data.Ord (Down (..), comparing)
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
import Path.IO (listDir, removeFile)
import PlutusLedgerApi.Test.EvaluationEvent (ScriptEvaluationEvents)
import Render qualified
import Text.Printf (printf)
import Text.Read (readMaybe)
import Types (Checkpoint (Checkpoint))

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

cleanupLedgerStates :: Path Abs Dir -> IO ()
cleanupLedgerStates checkpointDir = do
  files :: [(SlotNo, Path Abs File)] <- listFilesSorted Asc checkpointDir
  case drop 3 (reverse files) of
    [] -> putStrLn "No old ledger states to remove"
    filesToRemove -> for_ filesToRemove \(_slot, file) -> do
      putStrLn $ "Removing old ledger state " <> toFilePath file
      removeFile file

readLedgerState :: Path Abs File -> IO Checkpoint
readLedgerState file = CBOR.readFileDeserialise (toFilePath file)

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

data Order = Asc | Desc

listFilesSorted :: Order -> Path Abs Dir -> IO [(SlotNo, Path Abs File)]
listFilesSorted order eventsDir = do
  (_dirs, files) <- listDir eventsDir
  let direction = case order of
        Asc -> comparing fst
        Desc -> comparing (Down . fst)
      parsed = [(slot, f) | f <- files, slot <- maybeToList (parseSlotNo f)]
  pure $ sortBy direction parsed

readEventsFile :: Path Abs File -> IO ScriptEvaluationEvents
readEventsFile path = do
  let filePath = toFilePath path
  putStrLn $ "Reading events from " <> filePath
  CBOR.readFileDeserialise filePath

parseSlotNo :: Path Abs File -> Maybe SlotNo
parseSlotNo fp = do
  let name = toFilePath (filename fp)
  slot <- readMaybe (takeWhile (/= '-') (dropWhile (== '0') name))
  pure (SlotNo slot)
