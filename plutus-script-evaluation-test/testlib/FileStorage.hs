module FileStorage where

import Cardano.Api.Shelley (
  BlockHeader,
  ChainPoint,
  Hash,
  LedgerState,
  SerialiseAsRawBytes (serialiseToRawBytes),
  SlotNo (SlotNo),
  chainPointToHeaderHash,
  chainPointToSlotNo,
  unSlotNo,
 )
import Codec.Serialise qualified as CBOR
import Data.ByteString.Base16 qualified as B16
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.String.Interpolate (__i)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
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
import Text.Printf (printf)
import Text.Read (readMaybe)

ledgerStateBySlot :: Path Abs Dir -> SlotNo -> IO LedgerState
ledgerStateBySlot checkpoints slot = do
  file :: Path Abs File <- do
    states <- ledgerStates checkpoints
    Map.lookup slot states
      & maybe (fail $ "No ledger state found for slot " <> show slot) pure
  putStrLn $ "Reading ledger state from " <> toFilePath file
  Checkpoint chainPoint ledgerState <-
    CBOR.readFileDeserialise (toFilePath file)
  putStrLn
    [__i| Starting from checkpoint in #{toFilePath file}
      slot: #{maybe "Genesis" show (chainPointToSlotNo chainPoint)}
      hash: #{maybe "Genesis" hashToStr (chainPointToHeaderHash chainPoint)}
    |]
  pure ledgerState

saveLedgerState :: Path Abs Dir -> Checkpoint -> IO ()
saveLedgerState checkpointDir checkpoint@(Checkpoint chainPoint _state) = do
  let slot = maybe 0 unSlotNo (chainPointToSlotNo chainPoint)
      hash = chainPointToHeaderHash chainPoint
  file :: Path Rel File <-
    addExtension ".state" =<< do
      parseRelFile (printf "%016d-%s" slot (maybe "Genesis" hashToStr hash))
        & maybe (fail "Can't create ledger state file path") pure
  let filePath = toFilePath (checkpointDir </> file)
  putStrLn $ "Writing ledger state to " <> filePath
  CBOR.writeFileSerialise filePath checkpoint

readLedgerState :: Path Abs File -> IO Checkpoint
readLedgerState file = do
  putStrLn $ "Reading ledger state from " <> toFilePath file
  CBOR.readFileDeserialise (toFilePath file)

saveEvents :: Path Abs Dir -> ChainPoint -> ScriptEvaluationEvents -> IO ()
saveEvents eventsDir chainPoint events = do
  let slot = maybe 0 unSlotNo (chainPointToSlotNo chainPoint)
      hash = chainPointToHeaderHash chainPoint
  file :: Path Rel File <-
    addExtension ".event" =<< do
      parseRelFile (printf "%016d-%s" slot (maybe "Genesis" hashToStr hash))
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

hashToStr :: Hash BlockHeader -> String
hashToStr = Text.unpack . Text.decodeLatin1 . B16.encode . serialiseToRawBytes
