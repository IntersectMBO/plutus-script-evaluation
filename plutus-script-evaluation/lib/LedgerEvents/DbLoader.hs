module LedgerEvents.DbLoader where

import Cardano.Api.Ledger (StandardCrypto)
import Cardano.Api.Shelley (
  BlockNo (..),
  LedgerEvent (..),
  SlotNo (..),
  chainPointToSlotNo,
  unBlockNo,
 )
import Cardano.Ledger.BaseTypes (getVersion)
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
import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Short (fromShort)
import Data.Int (Int16, Int64)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Database (CostModelValues, EvaluationEvent)
import Database qualified as DB
import Database qualified as Db
import Database.PostgreSQL.Simple qualified as PostgreSQL
import FileStorage qualified
import Path (Abs, Dir, Path)
import PlutusLedgerApi.Common (Data, PlutusLedgerLanguage (..), toData)
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

    let (evaluationEvents, costs) =
          normaliseEventsWithCosts $
            indexLedgerEvents slotNo blockNo ledgerEvents

    -- First insert the cost model parameter values
    -- such that script evaluation events can refer them with a FK.
    numCosts <- Db.insertCostModelValues conn costs
    unless (numCosts == 0) do
      putStrLn [i|Inserted #{numCosts} cost model parameter values.|]

    numEvents <- Db.insertScriptEvaluationEvents conn evaluationEvents
    unless (numEvents == 0) do
      putStrLn [i|Inserted #{numEvents} script evaluation events.|]

data EventWithCosts = MkEventWithCosts
  { event :: EvaluationEvent
  , costs :: CostModelValues
  }

normaliseEventsWithCosts
  :: [EventWithCosts] -> ([EvaluationEvent], [CostModelValues])
normaliseEventsWithCosts eventsWithCosts =
  ( [event | MkEventWithCosts{event} <- eventsWithCosts]
  , [costs | MkEventWithCosts{costs} <- eventsWithCosts]
  )

indexLedgerEvents :: SlotNo -> BlockNo -> [LedgerEvent] -> [EventWithCosts]
indexLedgerEvents eeSlotNo eeBlockNo =
  foldr indexLedgerEvent []
 where
  indexLedgerEvent :: LedgerEvent -> [EventWithCosts] -> [EventWithCosts]
  indexLedgerEvent ledgerEvent events =
    case ledgerEvent of
      SuccessfulPlutusScript plutusEventsWithCtx ->
        foldr (indexPlutusEvent True) events plutusEventsWithCtx
      FailedPlutusScript plutusEventsWithCtx ->
        foldr (indexPlutusEvent False) events plutusEventsWithCtx
      _ -> events

  indexPlutusEvent
    :: Bool
    -> PlutusWithContext StandardCrypto
    -> [EventWithCosts]
    -> [EventWithCosts]
  indexPlutusEvent
    eeEvaluatedSuccessfully
    PlutusWithContext
      { pwcArgs = args :: PlutusArgs l
      , pwcCostModel
      , pwcScript
      , pwcProtocolVersion
      , pwcExUnits
      }
    events = MkEventWithCosts{event, costs} : events
     where
      event :: EvaluationEvent =
        Db.MkEvaluationEvent
          { eeSlotNo
          , eeBlockNo
          , eeEvaluatedSuccessfully
          , eeExecBudgetCpu
          , eeExecBudgetMem
          , eeSerialisedScript
          , eeDatum
          , eeRedeemer
          , eeScriptContext
          , eeLedgerLanguage
          , eeMajorProtocolVersion
          }

      costs :: CostModelValues =
        DB.MkCostModelValues
          { cmLedgerLanguage = eeLedgerLanguage
          , cmMajorProtocolVersion = eeMajorProtocolVersion
          , cmParamValues = getCostModelParams pwcCostModel
          }

      ExUnits
        (fromIntegral -> eeExecBudgetCpu :: Int64)
        (fromIntegral -> eeExecBudgetMem :: Int64) = pwcExUnits

      eeMajorProtocolVersion :: Int16 =
        getVersion pwcProtocolVersion

      eeLedgerLanguage :: PlutusLedgerLanguage =
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

      eeSerialisedScript :: ByteString =
        fromShort . unPlutusBinary . plutusBinary $
          either id plutusFromRunnable pwcScript
