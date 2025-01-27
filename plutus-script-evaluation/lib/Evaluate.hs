module Evaluate where

import Codec.Serialise (deserialise)
import Control.Monad (when)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Data.ByteString qualified as BSL
import Data.ByteString.Short qualified as BSS
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Time.Clock (getCurrentTime)
import Database qualified as Db
import Database.PostgreSQL.Simple qualified as Postgres
import Database.Schema (ScriptEvaluationRecord' (..))
import Ouroboros.Consensus.Block (BlockNo)
import PlutusLedgerApi.Common (
  Data,
  EvaluationContext (..),
  ExBudget (..),
  MajorProtocolVersion,
  PlutusLedgerLanguage (..),
  ScriptForEvaluation,
  ScriptNamedDeBruijn (..),
  VerboseMode (Quiet),
  deserialiseScript,
  deserialisedScript,
  evaluateScriptRestricting,
 )
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V2 qualified as V2
import PlutusLedgerApi.V3 qualified as V3
import Text.PrettyBy qualified as Pretty

data ScriptEvaluationInput = MkScriptEvaluationInput
  { seiPlutusLedgerLanguage :: PlutusLedgerLanguage
  , seiMajorProtocolVersion :: MajorProtocolVersion
  , seiEvaluationContext :: EvaluationContext
  , seiData :: [Data]
  , seiScript :: ScriptForEvaluation
  , seiExBudget :: ExBudget
  , seiEvaluationSuccess :: Bool
  , seiBlock :: BlockNo
  }

renderScriptEvaluationInput :: ScriptEvaluationInput -> String
renderScriptEvaluationInput MkScriptEvaluationInput{..} =
  "\n\nseiPlutusLedgerLanguage = "
    ++ show seiPlutusLedgerLanguage
    ++ "\n\nseiMajorProtocolVersion = "
    ++ show seiMajorProtocolVersion
    ++ "\n\nseiEvaluationContext = <evaluation context>"
    ++ "\n\nseiExBudget = "
    ++ show seiExBudget
    ++ "\n\nseiEvaluationSuccess = "
    ++ show seiEvaluationSuccess
    ++ "\n\nseiBlock = "
    ++ show seiBlock
    ++ "\n\nseiData = "
    ++ Pretty.display seiData
    ++ "\n\nseiScript = "
    ++ ( let ScriptNamedDeBruijn uplc = deserialisedScript seiScript
          in Pretty.display uplc
       )

evaluateScripts
  :: (MonadFail m, MonadUnliftIO m)
  => Postgres.Connection
  -- ^ Database connection
  -> a
  -- ^ Initial accumulator
  -> (ScriptEvaluationInput -> a -> m a)
  -- ^ Accumulation function
  -> m a
evaluateScripts conn initialAccum accumulate =
  Db.withScriptEvaluationEvents conn initialAccum \accum record -> do
    scriptInput <- inputFromRecord record
    accumulate scriptInput accum

inputFromRecord
  :: (MonadFail m)
  => Db.ScriptEvaluationRecord
  -> m ScriptEvaluationInput
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
      , seiEvaluationSuccess = seEvaluatedSuccessfully
      , seiBlock = seBlockNo
      }

onScriptEvaluationInput :: ScriptEvaluationInput -> () -> IO ()
onScriptEvaluationInput input@MkScriptEvaluationInput{..} _accum = do
  let
    (_logOutput, evaluationResult) =
      evaluateScriptRestricting
        seiPlutusLedgerLanguage
        seiMajorProtocolVersion
        Quiet
        seiEvaluationContext
        (ExBudget maxBound maxBound) -- will check the budget separately
        seiScript
        seiData

  let budgetExceeded (ExBudget cpu mem) =
        let ExBudget cpuPaidFor memPaidFor = seiExBudget
         in cpu > cpuPaidFor || mem > memPaidFor

  let evaluationSuccess =
        either (const False) (not . budgetExceeded) evaluationResult

  print seiBlock

  when (evaluationSuccess /= seiEvaluationSuccess) do
    let msg =
          "Script evaluation result ("
            ++ show evaluationSuccess
            ++ ") does not match the recorded result ("
            ++ show seiEvaluationSuccess
            ++ ")"

    nonce <- getCurrentTime
    let logFile = show seiBlock ++ "_" ++ show nonce ++ ".log"

    putStrLn msg
    putStrLn $ "Writing log to " ++ logFile

    TIO.writeFile logFile $
      Text.pack msg
        <> "\n\nEvaluation result:\n"
        <> Text.pack (show evaluationResult)
        <> "\n\nScript evaluation inputs:\n"
        <> Text.pack (renderScriptEvaluationInput input)

  case evaluationResult of
    Left err ->
      putStrLn $ "Script evaluation was not successful: " <> show err
    Right spentExBudget -> do
      if budgetExceeded spentExBudget
        then do
          putStrLn "Budget exceeded!"
          putStrLn $ "Paid for: " <> show seiExBudget
          putStrLn $ "Consumed: " <> show spentExBudget
        else
          if seiExBudget == spentExBudget
            then do
              putStrLn $ "Budget matches exactly: " <> show spentExBudget
            else do
              putStrLn "Budget is sufficient:"
              putStrLn $ "Paid for: " <> show seiExBudget
              putStrLn $ "Consumed: " <> show spentExBudget
