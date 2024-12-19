{-# LANGUAGE PartialTypeSignatures #-}

module Evaluate where

import Codec.Serialise (deserialise)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Data.ByteString qualified as BSL
import Data.ByteString.Short qualified as BSS
import Database qualified as Db
import Database.PostgreSQL.Simple qualified as Postgres
import Database.Schema (ScriptEvaluationRecord' (..))
import PlutusLedgerApi.Common (
  Data,
  EvaluationContext (..),
  ExBudget (..),
  MajorProtocolVersion,
  PlutusLedgerLanguage (..),
  ScriptForEvaluation,
  VerboseMode (Quiet),
  deserialiseScript,
  evaluateScriptRestricting,
 )
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V2 qualified as V2
import PlutusLedgerApi.V3 qualified as V3

data ScriptEvaluationInput = MkScriptEvaluationInput
  { seiPlutusLedgerLanguage :: PlutusLedgerLanguage
  , seiMajorProtocolVersion :: MajorProtocolVersion
  , seiEvaluationContext :: EvaluationContext
  , seiData :: [Data]
  , seiScript :: ScriptForEvaluation
  , seiExBudget :: ExBudget
  }

evaluateScripts
  :: Postgres.Connection
  -- ^ Database connection
  -> a
  -- ^ Initial accumulator
  -> (ScriptEvaluationInput -> a -> IO a)
  -- ^ Accumulation function
  -> IO a
evaluateScripts conn initialAccum accumulate =
  Db.withScriptEvaluationEvents conn initialAccum \accum record -> do
    scriptInput <- inputFromRecord record
    accumulate scriptInput accum

inputFromRecord
  :: (MonadFail m) => Db.ScriptEvaluationRecord -> m ScriptEvaluationInput
inputFromRecord MkScriptEvaluationRecord'{..} = do
  let mkEvalCtx f =
        runExceptT (runWriterT f) >>= \case
          Left e -> fail $ "Failed to create evaluation context: " <> show e
          Right (ctx, _warnings) -> pure ctx
  seiEvaluationContext <-
    mkEvalCtx case seLedgerLanguage of
      PlutusV1 -> V1.mkEvaluationContext seCostModelParams
      PlutusV2 -> V2.mkEvaluationContext seCostModelParams
      PlutusV3 -> V3.mkEvaluationContext seCostModelParams
  seiScript <-
    case deserialiseScript
      seLedgerLanguage
      seMajorProtocolVersion
      (BSS.toShort seScript) of
      Left err -> fail $ "Failed to deserialise script: " <> show err
      Right script -> pure script
  pure
    MkScriptEvaluationInput
      { seiPlutusLedgerLanguage = seLedgerLanguage
      , seiMajorProtocolVersion = seMajorProtocolVersion
      , seiEvaluationContext
      , seiScript
      , seiData =
          (deserialise . BSL.fromStrict <$>)
            . maybe id (:) seDatum
            $ maybe id (:) seRedeemer [seScriptContext]
      , seiExBudget = ExBudget seExecBudgetCpu seExecBudgetMem
      }

onScriptEvaluationInput :: ScriptEvaluationInput -> ExBudget -> IO ExBudget
onScriptEvaluationInput MkScriptEvaluationInput{..} budget = do
  let
    (_logOutput, evaluationResult) =
      evaluateScriptRestricting
        seiPlutusLedgerLanguage
        seiMajorProtocolVersion
        Quiet
        seiEvaluationContext
        (ExBudget maxBound maxBound)
        seiScript
        seiData

  putStrLn ""
  case evaluationResult of
    Left err -> do
      putStrLn $ "Script evaluation was not successful: " <> show err
    Right (ExBudget cpu mem) -> do
      putStrLn $
        "Script evaluation was successful.\nConsumed: "
          <> show cpu
          <> ", "
          <> show mem
      putStrLn $
        let ExBudget cpu' mem' = seiExBudget
         in "Expected: " <> show cpu' <> ", " <> show mem'

  pure budget
