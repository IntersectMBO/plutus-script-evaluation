module LedgerEvents.FileWriter where

import Cardano.Api.Ledger (StrictMaybe (..), strictMaybeToMaybe)
import Cardano.Api.Shelley (
  BlockNo,
  LedgerEvent (FailedPlutusScript, SuccessfulPlutusScript),
 )
import Cardano.Ledger.Alonzo.Scripts (
  PlutusBinary (unPlutusBinary),
  getCostModelParams,
 )
import Cardano.Ledger.BaseTypes (getVersion)
import Cardano.Ledger.Plutus (
  LegacyPlutusArgs (LegacyPlutusArgs2, LegacyPlutusArgs3),
  Plutus (plutusBinary),
  PlutusArgs (unPlutusV1Args, unPlutusV2Args),
  PlutusLanguage (isLanguage),
  PlutusScriptContext,
  PlutusWithContext (PlutusWithContext, pwcArgs, pwcCostModel),
  SLanguage (SPlutusV1, SPlutusV2, SPlutusV3),
  plutusFromRunnable,
  pwcExUnits,
  pwcProtocolVersion,
  pwcScript,
  transExUnits,
  unPlutusV3Args,
 )
import Control.Applicative (Alternative ((<|>)))
import Control.DeepSeq (force)
import Data.DList (DList)
import Data.DList qualified as DList
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List.NonEmpty qualified as NE
import Data.String.Interpolate (__i)
import Data.Word (Word64)
import FileStorage qualified
import Path (Abs, Dir, Path)
import PlutusLedgerApi.Common (
  Data,
  MajorProtocolVersion (MajorProtocolVersion),
  PlutusLedgerLanguage (PlutusV1, PlutusV2, PlutusV3),
  ToData,
  toData,
 )
import PlutusLedgerApi.Test.EvaluationEvent (
  ScriptEvaluationData (ScriptEvaluationData),
  ScriptEvaluationEvent (PlutusEvent),
  ScriptEvaluationEvents (
    ScriptEvaluationEvents,
    eventsCostParamsV1,
    eventsCostParamsV2,
    eventsEvents
  ),
  ScriptEvaluationResult (
    ScriptEvaluationFailure,
    ScriptEvaluationSuccess
  ),
 )
import Types (Checkpoint (Checkpoint))

data EventHandlerState = EventHandlerState
  { costParamsV1 :: !(StrictMaybe [Int64])
  , costParamsV2 :: !(StrictMaybe [Int64])
  , scriptEvents :: DList ScriptEvaluationEvent
  , numScriptEvents :: Int
  }

makeEventIndexer
  :: Path Abs Dir
  -> Path Abs Dir
  -> Word64 -- eventsPerFile
  -> IO ((BlockNo, Checkpoint, [LedgerEvent]) -> IO ())
makeEventIndexer checkpointsDir eventsDir (fromIntegral -> eventsPerFile) = do
  ref <-
    newIORef
      EventHandlerState
        { costParamsV1 = SNothing
        , costParamsV2 = SNothing
        , scriptEvents = DList.empty
        , numScriptEvents = 0
        }
  pure \(_, checkpoint@(Checkpoint chainPoint _ledgerState), ledgerEvents) -> do
    let MkPlutusEvents{..} = indexLedgerEvents ledgerEvents
    state@EventHandlerState{..} <- readIORef ref
    let numNewEvents = length peScriptEvaluationEvents
    putStrLn
      [__i|
      Total script events: #{numScriptEvents + numNewEvents} (+#{numNewEvents})
      |]
    if numScriptEvents + numNewEvents >= eventsPerFile
      then do
        let (eventsToWrite, eventsToCarry) =
              splitAt eventsPerFile $
                DList.toList (scriptEvents <> peScriptEvaluationEvents)
        putStrLn "Writing ledger state ... "
        FileStorage.saveLedgerState checkpointsDir checkpoint
        putStrLn "Done."
        putStrLn "Cleaning up old ledger states..."
        FileStorage.cleanupLedgerStates checkpointsDir
        putStrLn "Done."
        putStrLn $
          "Writing " <> show (length eventsToWrite) <> " script events..."
        FileStorage.saveEvents
          eventsDir
          chainPoint
          ScriptEvaluationEvents
            { eventsCostParamsV1 = strictMaybeToMaybe costParamsV1
            , eventsCostParamsV2 = strictMaybeToMaybe costParamsV2
            , eventsEvents = NE.fromList eventsToWrite
            }
        putStrLn "Done."
        writeIORef
          ref
          state
            { costParamsV1 = costParamsV1 <|> peCostParamsV1
            , costParamsV2 = costParamsV2 <|> peCostParamsV2
            , scriptEvents = DList.fromList eventsToCarry
            , numScriptEvents = numScriptEvents + numNewEvents - eventsPerFile
            }
      else
        writeIORef
          ref
          state
            { costParamsV1 = costParamsV1 <|> peCostParamsV1
            , costParamsV2 = costParamsV2 <|> peCostParamsV2
            , scriptEvents = scriptEvents <> peScriptEvaluationEvents
            , numScriptEvents = numScriptEvents + numNewEvents
            }

data PlutusEvents = MkPlutusEvents
  { peCostParamsV1 :: !(StrictMaybe [Int64])
  , peCostParamsV2 :: !(StrictMaybe [Int64])
  , peCostParamsV3 :: !(StrictMaybe [Int64])
  , peScriptEvaluationEvents :: DList ScriptEvaluationEvent
  }

emptyPlutusEvents :: PlutusEvents
emptyPlutusEvents =
  MkPlutusEvents
    { peCostParamsV1 = mempty
    , peCostParamsV2 = mempty
    , peCostParamsV3 = mempty
    , peScriptEvaluationEvents = mempty
    }

indexLedgerEvents :: [LedgerEvent] -> PlutusEvents
indexLedgerEvents = foldr indexLedgerEvent emptyPlutusEvents
 where
  indexLedgerEvent :: LedgerEvent -> PlutusEvents -> PlutusEvents
  indexLedgerEvent ledgerEvent !plutusEvents =
    case ledgerEvent of
      SuccessfulPlutusScript plutusEventsWithCtx ->
        foldr
          (indexPlutusEvent ScriptEvaluationSuccess)
          plutusEvents
          plutusEventsWithCtx
      FailedPlutusScript plutusEventsWithCtx ->
        foldr
          (indexPlutusEvent ScriptEvaluationFailure)
          plutusEvents
          plutusEventsWithCtx
      _ -> plutusEvents

  indexPlutusEvent
    :: ScriptEvaluationResult
    -> PlutusWithContext
    -> PlutusEvents
    -> PlutusEvents
  indexPlutusEvent
    evaluationResult
    PlutusWithContext
      { pwcArgs = args :: PlutusArgs l
      , pwcCostModel
      , pwcScript
      , pwcProtocolVersion
      , pwcExUnits
      }
    events@MkPlutusEvents{..} =
      case plutusLedgerLanguage of
        PlutusV1 ->
          events
            { peScriptEvaluationEvents = peScriptEvaluationEvents'
            , peCostParamsV1 = force (peCostParamsV1 <|> peCostModelParams)
            }
        PlutusV2 ->
          events
            { peScriptEvaluationEvents = peScriptEvaluationEvents'
            , peCostParamsV2 = force (peCostParamsV2 <|> peCostModelParams)
            }
        PlutusV3 ->
          events
            { peScriptEvaluationEvents = peScriptEvaluationEvents'
            , peCostParamsV3 = force (peCostParamsV3 <|> peCostModelParams)
            }
     where
      plutusLedgerLanguage :: PlutusLedgerLanguage =
        case isLanguage @l of
          SPlutusV1 -> PlutusV1
          SPlutusV2 -> PlutusV2
          SPlutusV3 -> PlutusV3

      peCostModelParams :: StrictMaybe [Int64] =
        SJust (getCostModelParams pwcCostModel)

      peScriptEvaluationEvents' :: DList ScriptEvaluationEvent =
        DList.cons
          (PlutusEvent plutusLedgerLanguage evaluationData evaluationResult)
          peScriptEvaluationEvents

      evaluationData :: ScriptEvaluationData =
        ScriptEvaluationData
          (MajorProtocolVersion (getVersion pwcProtocolVersion))
          (transExUnits pwcExUnits)
          ( either
              (unPlutusBinary . plutusBinary)
              (unPlutusBinary . plutusBinary . plutusFromRunnable)
              pwcScript
          )
          ( case isLanguage @l of
              SPlutusV1 -> legacyPlutusArgsToData (unPlutusV1Args args)
              SPlutusV2 -> legacyPlutusArgsToData (unPlutusV2Args args)
              SPlutusV3 -> [toData (unPlutusV3Args args)]
          )

      legacyPlutusArgsToData
        :: (ToData (PlutusScriptContext l))
        => LegacyPlutusArgs l
        -> [Data]
      legacyPlutusArgsToData = \case
        LegacyPlutusArgs2 redeemer scriptContext ->
          [redeemer, toData scriptContext]
        LegacyPlutusArgs3 datum redeemer scriptContext ->
          [datum, redeemer, toData scriptContext]
