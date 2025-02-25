{-# LANGUAGE StrictData #-}

module Evaluate where

import Codec.Serialise (deserialise)
import Control.Concurrent (getNumCapabilities)
import Control.Monad (when)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Data.ByteString qualified as BSL
import Data.ByteString.Short qualified as BSS
import Data.Either (isRight)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Word (Word32)
import Database qualified as Db
import Database.PostgreSQL.Simple qualified as Postgres
import Database.PostgreSQL.Simple.Types (PGArray (fromPGArray))
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
  { seiPlutusLedgerLanguage :: PlutusLedgerLanguage
  , seiMajorProtocolVersion :: MajorProtocolVersion
  , seiEvaluationContext :: EvaluationContext
  , seiData :: [Data]
  , seiScript :: ScriptForEvaluation
  , seiExBudget :: ExBudget
  , seiEvaluationPk :: Int64
  , seiEvaluationSuccess :: Bool
  , seiBlock :: Int64
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
    ++ let ScriptNamedDeBruijn uplc = deserialisedScript seiScript
        in Pretty.display uplc

evaluateScripts
  :: Postgres.Connection
  -- ^ Database connection
  -> Int64
  -- ^ Primary key value to start from
  -> (ScriptEvaluationInput -> IO ())
  -- ^ Callback
  -> IO ()
evaluateScripts conn startFrom callback = do
  maxThreads <- liftIO getNumCapabilities
  st <-
    newIORef
      ( 0 -- current number of threads
      , 0 -- number of evaluated scripts
      , 0 -- average processing time (millis)
      , 0 -- average evaluation time (millis)
      )
  evalContexts <- newIORef Map.empty -- cashed evaluation contexts
  Db.withScriptEvaluationRecords conn startFrom () \_unit record -> do
    startProcessing <- liftIO getCurrentTime
    waitForAFreeThread maxThreads st
    atomicModifyIORef' st \(threads, n, a, s) -> ((threads + 1, n, a, s), ())
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
  waitForAFreeThread :: Int -> IORef (Int, Word32, Word32, Word32) -> IO ()
  waitForAFreeThread maxThreads counter = do
    (threadCount, _, _, _) <- readIORef counter
    when (threadCount >= maxThreads) do
      threadDelay 1_000 -- wait for 1ms
      waitForAFreeThread maxThreads counter

  nominalDiffTimeToMillis :: NominalDiffTime -> Word32
  nominalDiffTimeToMillis dt = round (1000 * nominalDiffTimeToSeconds dt)

inputFromRecord
  :: (MonadFail m, MonadIO m)
  => IORef (Map Int64 EvaluationContext)
  -> Db.ScriptEvaluationRecord
  -> m ScriptEvaluationInput
inputFromRecord evalCtxRef Db.MkScriptEvaluationRecord{..} = do
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
          PlutusV1 -> V1.mkEvaluationContext (fromPGArray seCostModelParams)
          PlutusV2 -> V2.mkEvaluationContext (fromPGArray seCostModelParams)
          PlutusV3 -> V3.mkEvaluationContext (fromPGArray seCostModelParams)
        let keyedEvalCtxs' = Map.insert seCostModelKey ctx keyedEvalCtxs
        liftIO $ writeIORef evalCtxRef keyedEvalCtxs'
        pure ctx
  seiScript <-
    case deserialiseScript
      seLedgerLanguage
      seMajorProtocolVersion
      (BSS.toShort seScript) of
      Left err ->
        fail $
          "Failed to deserialise script ("
            <> show sePk
            <> "): "
            <> show err
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
      , seiEvaluationPk = fromMaybe (-1) sePk
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

  when (seiBlock `mod` 100 == 0) (print seiEvaluationPk)

  let evaluationSuccess = isRight evaluationResult

  when (evaluationSuccess /= seiEvaluationSuccess) do
    let msg =
          "Script evaluation (pk = "
            ++ show seiEvaluationPk
            ++ ") result ("
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
    let logFile = show seiBlock ++ "_" ++ show nonce ++ ".log"
    putStrLn $ "Writing log to " ++ logFile
    TIO.writeFile logFile (Text.pack msg)

  case evaluationResult of
    Right _spentExBudget -> pure ()
    Left err ->
      putStrLn $
        "Script evaluation (pk = "
          <> show seiEvaluationPk
          <> ") was not successful: "
          <> show err
