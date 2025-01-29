module Evaluate where

import Cardano.Slotting.Block (unBlockNo)
import Codec.Serialise (deserialise)
import Control.Concurrent (getNumCapabilities)
import Control.Monad (when)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Data.ByteString qualified as BSL
import Data.ByteString.Short qualified as BSS
import Data.Either (isRight)
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
import UnliftIO (IORef, atomicModifyIORef', liftIO, newIORef, readIORef)
import UnliftIO.Concurrent (forkFinally, threadDelay)

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

accumulateScripts
  :: (MonadFail m, MonadUnliftIO m)
  => Postgres.Connection
  -- ^ Database connection
  -> BlockNo
  -- ^ Block number to start from
  -> a
  -- ^ Initial accumulator
  -> (ScriptEvaluationInput -> a -> m a)
  -- ^ Accumulation function
  -> m a
accumulateScripts conn startBlock initialAccum accumulate =
  Db.withScriptEvaluationEvents conn startBlock initialAccum \accum record -> do
    scriptInput <- inputFromRecord record
    accumulate scriptInput accum

evaluateScripts
  :: forall m
   . (MonadFail m, MonadUnliftIO m)
  => Postgres.Connection
  -- ^ Database connection
  -> BlockNo
  -- ^ Block number to start from
  -> (ScriptEvaluationInput -> m ())
  -- ^ Callback
  -> m ()
evaluateScripts conn startBlock callback = do
  maxThreads <- liftIO getNumCapabilities
  threadCounter <- newIORef 0
  Db.withScriptEvaluationEvents conn startBlock () \_unit record -> do
    waitForAFreeThread maxThreads threadCounter
    atomicModifyIORef' threadCounter \n -> (n + 1, ())
    _threadId <- forkFinally (callback =<< inputFromRecord record) \_ ->
      atomicModifyIORef' threadCounter \n -> (n - 1, ())
    pure ()
 where
  waitForAFreeThread :: Int -> IORef Int -> m ()
  waitForAFreeThread maxThreads counter = do
    threadCount <- readIORef counter
    when (threadCount >= maxThreads) do
      threadDelay 1_000 -- wait for 1ms
      waitForAFreeThread maxThreads counter

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

onScriptEvaluationInput :: ScriptEvaluationInput -> IO ()
onScriptEvaluationInput input@MkScriptEvaluationInput{..} = do
  let
    (_logOutput, evaluationResult) =
      evaluateScriptRestricting
        seiPlutusLedgerLanguage
        seiMajorProtocolVersion
        Quiet
        seiEvaluationContext
        seiExBudget
        seiScript
        seiData

  print seiBlock

  let evaluationSuccess = isRight evaluationResult

  when (evaluationSuccess /= seiEvaluationSuccess) do
    let msg =
          "Script evaluation result ("
            ++ show evaluationSuccess
            ++ ") does not match the recorded result ("
            ++ show seiEvaluationSuccess
            ++ ")"
            ++ "\n\nEvaluation result:\n"
            ++ show evaluationResult
            ++ "\n\nScript evaluation inputs:\n"
            ++ renderScriptEvaluationInput input
            ++ "\n\n"
    putStr msg

    nonce <- getCurrentTime
    let logFile = show (unBlockNo seiBlock) ++ "_" ++ show nonce ++ ".log"
    putStrLn $ "Writing log to " ++ logFile
    TIO.writeFile logFile (Text.pack msg)

  case evaluationResult of
    Right _spentExBudget -> pure ()
    Left err -> putStrLn $ "Script evaluation was not successful: " <> show err
