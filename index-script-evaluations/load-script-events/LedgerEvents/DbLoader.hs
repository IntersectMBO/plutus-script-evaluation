module LedgerEvents.DbLoader where

import Cardano.Api (
  BlockNo (..),
  SlotNo (..),
  chainPointToSlotNo,
  unBlockNo,
 )
import Cardano.Api.LedgerState (LedgerEvent (..))
import Cardano.Ledger.Binary (encCBOR, getVersion64)
import Cardano.Ledger.Binary qualified as Binary
import Cardano.Ledger.Plutus (
  ExUnits (..),
  Language,
  LegacyPlutusArgs (..),
  PlutusArgs,
  PlutusScriptContext,
  SLanguage (..),
  getCostModelParams,
  isLanguage,
  plutusBinary,
  plutusFromRunnable,
  plutusLanguage,
  plutusRunnableScriptHash,
  unPlutusBinary,
  unPlutusV1Args,
  unPlutusV2Args,
  unPlutusV3Args,
  unPlutusV4Args,
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
  toData,
 )
import PlutusLedgerApi.V3 (
  ScriptContext (scriptContextScriptInfo),
  ScriptInfo (..),
  ToData,
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
        eventRecords = indexLedgerEvents slotNo blockNo ledgerEvents
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

    -- Persist the checkpoint only after this block's events have been inserted.
    -- Each insert above runs in autocommit mode, so once they return the block's
    -- events are durably stored. Saving the checkpoint last guarantees that a
    -- crash can never leave the checkpoint pointing at a block whose events did
    -- not reach the database: on resume the node replays blocks strictly after
    -- the checkpoint, so a checkpoint block with missing events would never be
    -- re-applied and those events would be lost silently.
    when (unBlockNo blockNo `mod` 10_000 == 0) do
      putStrLn "Writing ledger state ... "
      FileStorage.saveLedgerState checkpointDir checkpoint
      putStrLn "Done."
      putStrLn "Cleaning up old ledger states..."
      FileStorage.cleanupLedgerStates checkpointDir
      putStrLn "Done."

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

      ssLedgerLanguage :: Language = plutusLanguage (isLanguage @l)

      eeScriptContext :: ByteString =
        toStrict (serialise @Data contextData)

      eeDatum :: Maybe ByteString =
        toStrict . serialise @Data <$> datumData

      eeRedeemer :: Maybe ByteString =
        toStrict . serialise @Data <$> redeemerData

      -- The ledger (cardano-ledger-core 1.21, capped below plutus-ledger-api
      -- 1.68) reuses the V3 script context for V4 scripts, so language-4 rows
      -- store V3-encoded contexts. Rows written under this pin remain
      -- V3-encoded after the ledger switches to the real V4 context, so that
      -- switch requires a re-index or a context-shape marker.
      (contextData, datumData, redeemerData) =
        case isLanguage @l of
          SPlutusV1 -> fromLegacyArgs (unPlutusV1Args args)
          SPlutusV2 -> fromLegacyArgs (unPlutusV2Args args)
          SPlutusV3 -> fromV3StyleContext (unPlutusV3Args args)
          SPlutusV4 -> fromV3StyleContext (unPlutusV4Args args)

      fromLegacyArgs
        :: (ToData (PlutusScriptContext l'))
        => LegacyPlutusArgs l'
        -> (Data, Maybe Data, Maybe Data)
      fromLegacyArgs = \case
        LegacyPlutusArgs2 redeemer context ->
          (toData context, Nothing, Just redeemer)
        LegacyPlutusArgs3 datum redeemer context ->
          (toData context, Just datum, Just redeemer)

      fromV3StyleContext :: ScriptContext -> (Data, Maybe Data, Maybe Data)
      fromV3StyleContext context =
        ( toData context
        , case scriptContextScriptInfo context of
            SpendingScript _txOutRef optionalDatum -> toData <$> optionalDatum
            _ -> Nothing
        , Just (toData (scriptContextRedeemer context))
        )

      ssSerialised :: ByteString =
        fromShort . unPlutusBinary . plutusBinary $
          plutusFromRunnable pwcScript

      eeScriptHash :: ByteString =
        plutusRunnableScriptHash pwcScript
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
