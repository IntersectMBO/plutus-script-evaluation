module LedgerEvents.DbLoader where

import Cardano.Api (
  BlockNo (..),
  LedgerEvent (..),
  SlotNo (..),
  chainPointToSlotNo,
  unBlockNo,
 )
import Cardano.Ledger.Binary (encCBOR, getVersion64)
import Cardano.Ledger.Binary qualified as Binary
import Cardano.Ledger.Plutus (
  ExUnits (..),
  LegacyPlutusArgs (..),
  PlutusArgs,
  SLanguage (..),
  getCostModelParams,
  isLanguage,
  plutusBinary,
  plutusFromRunnable,
  unPlutusBinary,
  unPlutusV1Args,
  unPlutusV2Args,
  unPlutusV3Args,
 )
import Cardano.Ledger.Plutus.Evaluate (PlutusWithContext (..))
import Codec.Serialise (serialise)
import Control.Monad (unless, when)
import Data.Bits (shiftL, shiftR, xor)
import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Short (fromShort)
import Data.Digest.Murmur64 (Hash64, hash64, hash64Add)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.List (nub)
import Data.Maybe (fromMaybe, maybeToList)
import Data.String.Interpolate (i)
import Data.Word (Word64)
import Database (SerialisedScriptRecord' (ssLedgerLanguage))
import Database qualified as DB
import Database.PostgreSQL.Simple qualified as PostgreSQL
import FileStorage qualified
import Path (Abs, Dir, Path)
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU, ExMemory)
import PlutusLedgerApi.Common (
  Data,
  MajorProtocolVersion (MajorProtocolVersion),
  PlutusLedgerLanguage (..),
  toData,
 )
import PlutusLedgerApi.V3 (
  ScriptContext (scriptContextScriptInfo),
  ScriptInfo (..),
  scriptContextRedeemer,
 )
import Types (Checkpoint (..))

makeEventIndexer
  :: Path Abs Dir
  -> PostgreSQL.Connection
  -> IO ((BlockNo, Checkpoint, [LedgerEvent]) -> IO ())
makeEventIndexer checkpointDir conn = do
  pure \(blockNo, checkpoint@Checkpoint{cChainPoint}, ledgerEvents) -> do
    let slotNo = fromMaybe (SlotNo 0) (chainPointToSlotNo cChainPoint)
    when (unBlockNo blockNo `mod` 10_000 == 0) do
      putStrLn "Writing ledger state ... "
      FileStorage.saveLedgerState checkpointDir checkpoint
      putStrLn "Done."
      putStrLn "Cleaning up old ledger states..."
      FileStorage.cleanupLedgerStates checkpointDir
      putStrLn "Done."

    let eventRecords = indexLedgerEvents slotNo blockNo ledgerEvents
        scriptEvaluationRecords = nub $ eventRecords <&> event
        costsRecords = nub $ eventRecords >>= maybeToList . costs
        scriptRecords = nub $ eventRecords <&> script

    -- First insert the cost model parameter values
    -- such that script evaluation events can refer them with a FK.
    numCosts <- DB.insertCostModelValues conn costsRecords
    unless (numCosts == 0) do
      putStrLn [i|Inserted #{numCosts} cost model parameter values.|]

    numScripts <- DB.insertSerialisedScripts conn scriptRecords
    unless (numScripts == 0) do
      putStrLn [i|Inserted #{numScripts} serialised scripts.|]

    numEvents <- DB.insertScriptEvaluationEvents conn scriptEvaluationRecords
    unless (numEvents == 0) do
      putStrLn [i|Inserted #{numEvents} script evaluation events.|]

data EventRecords = MkEventRecords
  { event :: DB.EvaluationEventRecord
  , costs :: Maybe DB.CostModelValuesRecord
  , script :: DB.SerialisedScriptRecord
  }

indexLedgerEvents :: SlotNo -> BlockNo -> [LedgerEvent] -> [EventRecords]
indexLedgerEvents eeSlotNo eeBlockNo = foldr indexLedgerEvent []
 where
  indexLedgerEvent :: LedgerEvent -> [EventRecords] -> [EventRecords]
  indexLedgerEvent ledgerEvent events =
    case ledgerEvent of
      SuccessfulPlutusScript plutusEventsWithCtx ->
        foldr (indexPlutusEvent True) events plutusEventsWithCtx
      FailedPlutusScript plutusEventsWithCtx ->
        foldr (indexPlutusEvent False) events plutusEventsWithCtx
      _ -> events

  indexPlutusEvent
    :: Bool
    -> PlutusWithContext
    -> [EventRecords]
    -> [EventRecords]
  indexPlutusEvent
    eeEvaluatedSuccessfully
    PlutusWithContext
      { pwcArgs = args :: PlutusArgs l
      , pwcCostModel
      , pwcScript
      , pwcScriptHash
      , pwcProtocolVersion
      , pwcExUnits
      }
    events = MkEventRecords{event, costs, script} : events
     where
      event :: DB.EvaluationEventRecord =
        DB.MkEvaluationEventRecord'
          { eePk = Nothing
          , eeSlotNo
          , eeBlockNo
          , eeMajorProtocolVersion
          , eeEvaluatedSuccessfully
          , eeExecBudgetCpu
          , eeExecBudgetMem
          , eeScriptHash
          , eeDatum
          , eeRedeemer
          , eeScriptContext
          , eeCostModelParams
          }

      costs :: Maybe DB.CostModelValuesRecord =
        eeCostModelParams <&> \cmPk ->
          DB.MkCostModelValues
            { cmPk
            , cmParamValues
            }

      script :: DB.SerialisedScriptRecord =
        DB.MkSerialisedScriptRecord
          { ssHash = eeScriptHash
          , ssLedgerLanguage
          , ssSerialised
          }

      eeExecBudgetMem :: ExMemory = fromIntegral (exUnitsMem pwcExUnits)

      eeExecBudgetCpu :: ExCPU = fromIntegral (exUnitsSteps pwcExUnits)

      eeCostModelParams :: Maybe Hash64 = hashParamValues cmParamValues

      cmParamValues :: [Int64] = getCostModelParams pwcCostModel

      eeMajorProtocolVersion :: MajorProtocolVersion =
        -- In Ledger the major protocol version is stored as Word64
        -- This seems to be an overkill as there are only 9 major protocol
        -- versions so far, 'Int' is enough to store them.
        MajorProtocolVersion (fromIntegral (getVersion64 pwcProtocolVersion))

      ssLedgerLanguage :: PlutusLedgerLanguage =
        case isLanguage @l of
          SPlutusV1 -> PlutusV1
          SPlutusV2 -> PlutusV2
          SPlutusV3 -> PlutusV3

      eeScriptContext :: ByteString =
        toStrict $ serialise @Data
          case isLanguage @l of
            SPlutusV1 -> case unPlutusV1Args args of
              LegacyPlutusArgs2 _reedemer context -> toData context
              LegacyPlutusArgs3 _datum _reedemer context -> toData context
            SPlutusV2 -> case unPlutusV2Args args of
              LegacyPlutusArgs2 _reedemer context -> toData context
              LegacyPlutusArgs3 _datum _reedemer context -> toData context
            SPlutusV3 -> toData (unPlutusV3Args args)

      eeDatum :: Maybe ByteString =
        toStrict . serialise @Data <$> case isLanguage @l of
          SPlutusV1 -> case unPlutusV1Args args of
            LegacyPlutusArgs2 _reedemer _context -> Nothing
            LegacyPlutusArgs3 datum _reedemer _context -> Just datum
          SPlutusV2 -> case unPlutusV2Args args of
            LegacyPlutusArgs2 _reedemer _context -> Nothing
            LegacyPlutusArgs3 datum _reedemer _context -> Just datum
          SPlutusV3 -> case scriptContextScriptInfo (unPlutusV3Args args) of
            SpendingScript _txOutRef optionalDatum -> toData <$> optionalDatum
            _ -> Nothing

      eeRedeemer :: Maybe ByteString =
        toStrict . serialise @Data <$> case isLanguage @l of
          SPlutusV1 -> case unPlutusV1Args args of
            LegacyPlutusArgs2 redeemer _context -> Just redeemer
            LegacyPlutusArgs3 _datum redeemer _context -> Just redeemer
          SPlutusV2 -> case unPlutusV2Args args of
            LegacyPlutusArgs2 redeemer _context -> Just redeemer
            LegacyPlutusArgs3 _datum redeemer _context -> Just redeemer
          SPlutusV3 ->
            Just (toData (scriptContextRedeemer (unPlutusV3Args args)))

      ssSerialised :: ByteString =
        fromShort . unPlutusBinary . plutusBinary $
          either id plutusFromRunnable pwcScript

      eeScriptHash :: ByteString =
        pwcScriptHash
          & encCBOR
          & Binary.toBuilder pwcProtocolVersion
          & toLazyByteString
          & toStrict

hashParamValues :: [Int64] -> Maybe Hash64
hashParamValues = \case
  [] -> Nothing
  [x] -> Just (hash64 (int64ToWord64 x))
  (x : xs) -> hash64Add (int64ToWord64 x) <$> hashParamValues xs
 where
  int64ToWord64 :: Int64 -> Word64
  int64ToWord64 n = fromIntegral $ shiftL n 1 `xor` shiftR n 63
