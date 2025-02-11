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
import Data.Digest.Murmur64 (Hash64)
import Data.Either (isRight)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Word (Word32)
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
import System.Exit (ExitCode (..), exitWith)
import Text.PrettyBy qualified as Pretty
import UnliftIO (IORef, MonadIO, atomicModifyIORef', liftIO, newIORef, readIORef, writeIORef)
import UnliftIO.Concurrent (forkFinally, threadDelay)

data ScriptEvaluationInput = MkScriptEvaluationInput
  { seiPlutusLedgerLanguage :: !PlutusLedgerLanguage
  , seiMajorProtocolVersion :: !MajorProtocolVersion
  , seiEvaluationContext :: !EvaluationContext
  , seiData :: [Data]
  , seiScript :: !ScriptForEvaluation
  , seiExBudget :: !ExBudget
  , seiEvaluationSuccess :: !Bool
  , seiBlock :: !BlockNo
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
accumulateScripts conn startBlock initialAccum accumulate = do
  evaluationContexts <- newIORef Map.empty
  Db.withScriptEvaluationEvents conn startBlock initialAccum \accum record -> do
    scriptInput <- inputFromRecord evaluationContexts record
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
  st <-
    newIORef
      ( 0 -- current number of threads
      , 0 -- number of evaluated scripts
      , 0 -- average processing time (millis)
      , 0 -- average evaluation time (millis)
      )
  evalContexts <- newIORef Map.empty -- cashed evaluation contexts
  Db.withScriptEvaluationEvents conn startBlock () \_unit record -> do
    startProcessing <- liftIO getCurrentTime
    waitForAFreeThread maxThreads st
    atomicModifyIORef' st \(threads, n, a, s) ->
      ((threads + 1, n, a, s), ())
    let work = do
          input <- inputFromRecord evalContexts record
          startEvaluation <- liftIO getCurrentTime
          callback input
          end <- liftIO getCurrentTime
          pure
            ( nominalDiffTimeToMillis (end `diffUTCTime` startProcessing)
            , nominalDiffTimeToMillis (end `diffUTCTime` startEvaluation)
            )
    _threadId <- forkFinally work \case
      Left err -> liftIO do
        putStrLn $ "Failed to evaluate script: " <> show err
        exitWith (ExitFailure 1)
      Right (!dtp, !dte) -> do
        atomicModifyIORef' st \(threads, n, pt, et) ->
          let pt' =
                if pt == 0
                  then dtp
                  else
                    round @Double @Word32 $
                      fromIntegral (pt * (n - 1) + dtp) / fromIntegral n
              et' =
                if et == 0
                  then dte
                  else
                    round @Double @Word32 $
                      fromIntegral (et * (n - 1) + dte) / fromIntegral n
           in ((threads - 1, n + 1, pt', et'), ())
    pure ()
 where
  {-
  (_, n, pt, et) <- readIORef st
  when (n `mod` 100 == 0) $ liftIO do
    putStrLn $ "Average time: processing " <> show pt <> "ms, "
      <> "evaluation " <> show et <> "ms"
  -}

  waitForAFreeThread :: Int -> IORef (Int, Word32, Word32, Word32) -> m ()
  waitForAFreeThread maxThreads counter = do
    (threadCount, _, _, _) <- readIORef counter
    when (threadCount >= maxThreads) do
      threadDelay 1_000 -- wait for 1ms
      waitForAFreeThread maxThreads counter

  nominalDiffTimeToMillis :: NominalDiffTime -> Word32
  nominalDiffTimeToMillis dt = round (1000 * nominalDiffTimeToSeconds dt)

inputFromRecord
  :: (MonadFail m, MonadIO m)
  => IORef (Map Hash64 EvaluationContext)
  -> Db.ScriptEvaluationRecord
  -> m ScriptEvaluationInput
inputFromRecord evalCtxRef MkScriptEvaluationRecord'{..} = do
  let mkEvalCtx f =
        runExceptT (runWriterT f) >>= \case
          Left e -> fail $ "Failed to create evaluation context: " <> show e
          Right (ctx, _warnings) -> pure ctx
  seiEvaluationContext <- do
    keyedEvalCtxs <- liftIO $ readIORef evalCtxRef
    case Map.lookup seCostModelKey keyedEvalCtxs of
      Just ctx -> pure ctx
      Nothing -> do
        ctx <- mkEvalCtx case seLedgerLanguage of
          PlutusV1 -> V1.mkEvaluationContext seCostModelParams
          PlutusV2 -> V2.mkEvaluationContext seCostModelParams
          PlutusV3 -> V3.mkEvaluationContext seCostModelParams
        let keyedEvalCtxs' = Map.insert seCostModelKey ctx keyedEvalCtxs
        liftIO $ writeIORef evalCtxRef keyedEvalCtxs'
        pure ctx
  seiScript <-
    case deserialiseScript
      seLedgerLanguage
      seMajorProtocolVersion
      (BSS.toShort seScript) of
      Left err -> fail $ "Failed to deserialise script: " <> show err
      Right script -> pure script

  let seiData :: [Data]
      seiData =
        let addRedeemerDatum =
              case seLedgerLanguage of
                PlutusV3 -> id
                _ -> maybe id (:) seDatum . maybe id (:) seRedeemer
         in deserialise . BSL.fromStrict <$> addRedeemerDatum [seScriptContext]
  pure
    MkScriptEvaluationInput
      { seiPlutusLedgerLanguage = seLedgerLanguage
      , seiMajorProtocolVersion = seMajorProtocolVersion
      , seiEvaluationContext
      , seiScript
      , seiData
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

  let b = unBlockNo seiBlock
  when (b `mod` 100 == 0) (print b)

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
