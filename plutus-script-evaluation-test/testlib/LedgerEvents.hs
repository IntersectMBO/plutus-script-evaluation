module LedgerEvents where

import Cardano.Api.Shelley (
  ChainPoint,
  LedgerEvent (FailedPlutusScript, SuccessfulPlutusScript),
  LedgerState,
 )
import Cardano.Ledger.Alonzo.Scripts (
  PlutusBinary (unPlutusBinary),
  getCostModelParams,
 )
import Cardano.Ledger.BaseTypes (getVersion)
import Cardano.Ledger.Crypto (StandardCrypto)
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
 )
import Control.Applicative (Alternative ((<|>)))
import Data.DList (DList)
import Data.DList qualified as DList
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty qualified as NE
import Data.String.Interpolate (__i)
import Data.Word (Word64)
import FileStorage qualified
import Path (Abs, Dir, Path)
import Plutus.Script.Evaluation.Types (Checkpoint (Checkpoint))
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

data EventHandlerState = EventHandlerState
  { costParamsV1 :: Maybe [Integer]
  , costParamsV2 :: Maybe [Integer]
  , scriptEvents :: DList ScriptEvaluationEvent
  , numScriptEvents :: Int
  }

makeEventIndexer
  :: Path Abs Dir
  -> Path Abs Dir
  -> Word64 -- eventsPerFile
  -> IO ((ChainPoint, LedgerState, [LedgerEvent]) -> IO ())
makeEventIndexer checkpointsDir eventsDir (fromIntegral -> eventsPerFile) = do
  ref <-
    newIORef
      EventHandlerState
        { costParamsV1 = Nothing
        , costParamsV2 = Nothing
        , scriptEvents = DList.empty
        , numScriptEvents = 0
        }
  pure \(chainPoint, ledgerState, ledgerEvents) -> do
    let (newScriptEvents, newV1CostParams, newV2CostParams) =
          indexLedgerEvents ledgerEvents
    state@EventHandlerState{..} <- readIORef ref
    let numNewEvents = length newScriptEvents
    putStrLn
      [__i|
      Total script events: #{numScriptEvents + numNewEvents} (+#{numNewEvents})
      |]
    if numScriptEvents + numNewEvents >= eventsPerFile
      then do
        let (eventsToWrite, eventsToCarry) =
              splitAt eventsPerFile $
                DList.toList (scriptEvents <> DList.fromList newScriptEvents)
        putStrLn "Writing ledger state checkpoint... "
        FileStorage.saveLedgerState
          checkpointsDir
          (Checkpoint chainPoint ledgerState)
        putStrLn "Done."
        putStrLn $
          "Writing " <> show (length eventsToWrite) <> " script events..."
        FileStorage.saveEvents
          eventsDir
          chainPoint
          ScriptEvaluationEvents
            { eventsCostParamsV1 = (fromIntegral <$>) <$> costParamsV1
            , eventsCostParamsV2 = (fromIntegral <$>) <$> costParamsV2
            , eventsEvents = NE.fromList eventsToWrite
            }
        putStrLn "Done."
        writeIORef
          ref
          state
            { costParamsV1 = costParamsV1 <|> newV1CostParams
            , costParamsV2 = costParamsV2 <|> newV2CostParams
            , scriptEvents = DList.fromList eventsToCarry
            , numScriptEvents = numScriptEvents + numNewEvents - eventsPerFile
            }
      else
        writeIORef
          ref
          state
            { costParamsV1 = costParamsV1 <|> newV1CostParams
            , costParamsV2 = costParamsV2 <|> newV2CostParams
            , scriptEvents = scriptEvents <> DList.fromList newScriptEvents
            , numScriptEvents = numScriptEvents + numNewEvents
            }

indexLedgerEvents
  :: [LedgerEvent]
  -> ([ScriptEvaluationEvent], Maybe [Integer], Maybe [Integer])
indexLedgerEvents = foldr alg ([], Nothing, Nothing)
 where
  alg
    :: LedgerEvent
    -> ([ScriptEvaluationEvent], Maybe [Integer], Maybe [Integer])
    -> ([ScriptEvaluationEvent], Maybe [Integer], Maybe [Integer])
  alg ledgerEvent acc = case ledgerEvent of
    SuccessfulPlutusScript ds -> foldr (alg' ScriptEvaluationSuccess) acc ds
    FailedPlutusScript ds -> foldr (alg' ScriptEvaluationFailure) acc ds
    _ -> acc

  alg'
    :: ScriptEvaluationResult
    -> PlutusWithContext StandardCrypto
    -> ([ScriptEvaluationEvent], Maybe [Integer], Maybe [Integer])
    -> ([ScriptEvaluationEvent], Maybe [Integer], Maybe [Integer])
  alg'
    evaluationResult
    PlutusWithContext{pwcArgs = args :: PlutusArgs l, ..}
    (scriptEvents, v1, v2) =
      let plutusLedgerLanguage :: PlutusLedgerLanguage =
            case isLanguage @l of
              SPlutusV1 -> PlutusV1
              SPlutusV2 -> PlutusV2
              SPlutusV3 -> PlutusV3
          evaluationData =
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
                  SPlutusV3 -> []
              )
       in ( PlutusEvent plutusLedgerLanguage evaluationData evaluationResult : scriptEvents
          , v1 <|> Just [fromIntegral param | param <- getCostModelParams pwcCostModel]
          , v2
          )
     where
      legacyPlutusArgsToData
        :: (ToData (PlutusScriptContext l))
        => LegacyPlutusArgs l
        -> [Data]
      legacyPlutusArgsToData = \case
        LegacyPlutusArgs2 redeemer scriptContext ->
          [redeemer, toData scriptContext]
        LegacyPlutusArgs3 datum redeemer scriptContext ->
          [datum, redeemer, toData scriptContext]
