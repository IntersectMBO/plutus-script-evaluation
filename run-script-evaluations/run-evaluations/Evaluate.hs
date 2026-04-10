{-# LANGUAGE StrictData #-}

module Evaluate where

import Codec.Serialise (deserialise)
import Control.Concurrent (getNumCapabilities)
import Control.Lens (traverseOf, (&), (.~))
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
import Data.SatInt
import Database qualified as Db
import Database.PostgreSQL.Simple qualified as Postgres
import Database.PostgreSQL.Simple.Types (PGArray (fromPGArray))
import PlutusCore.Version
import PlutusCore.Default.Builtins
import PlutusLedgerApi.Common (
  Data,
  EvaluationContext (..),
  ExBudget (..),
  ExCPU (..),
  ExMemory (..),
  MajorProtocolVersion,
  PlutusLedgerLanguage (..),
  ScriptForEvaluation,
  ScriptNamedDeBruijn (..),
  VerboseMode (Quiet),
  deserialiseScript,
  deserialisedScript,
  serialiseUPLC,
  serialisedScript,
  evaluateScriptRestricting, evaluateScriptCounting,
 )
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V2 qualified as V2
import PlutusLedgerApi.V3 qualified as V3
import PlutusCore.Quote
import System.Exit (ExitCode (..), exitWith)
import Text.PrettyBy qualified as Pretty
import UnliftIO (IORef, MonadIO, atomicModifyIORef', liftIO, newIORef, readIORef, writeIORef)
import UnliftIO.Concurrent (forkFinally, threadDelay)
import UntypedPlutusCore qualified as UPLC
import System.IO.Unsafe

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
  waitForAllThreads st
 where
  waitForAllThreads :: IORef (Int, Word32, Word32, Word32) -> IO ()
  waitForAllThreads counter = do
    (threadCount, _, _, _) <- readIORef counter
    when (threadCount > 0) do
      threadDelay 1000
      waitForAllThreads counter

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

data Stat = Stat
  { statCpuAbs :: Integer
  , statMemAbs :: Integer
  , statSizeAbs :: Integer
  , statCpuRel :: Double
  , statMemRel :: Double
  , statSizeRel :: Double
  }

statsRef :: IORef [Stat]
statsRef = unsafePerformIO $ newIORef []
{-# NOINLINE statsRef #-}

counterRef :: IORef Int
counterRef = unsafePerformIO $ newIORef 0
{-# NOINLINE counterRef #-}

fromBudget :: ExBudget -> (Integer, Integer)
fromBudget (ExBudget (ExCPU cpu) (ExMemory mem)) = (fromSatInt cpu, fromSatInt mem)

onScriptEvaluationInput :: ScriptEvaluationInput -> IO ()
onScriptEvaluationInput input@MkScriptEvaluationInput{..} = do
  cnt <- atomicModifyIORef' counterRef (\old -> let new = old + 1 in (new, new))
  let (_, resultBefore) =
        evaluateScriptCounting
          seiPlutusLedgerLanguage
          seiMajorProtocolVersion
          Quiet
          seiEvaluationContext
          seiScript
          seiData
  (cpuBefore, memBefore) <- case resultBefore of
    Left _ -> fail "evaluation failed"
    Right budget -> pure $ fromBudget budget
  let sizeBefore = BSS.length $ serialisedScript seiScript

  let ScriptNamedDeBruijn scriptNDB = deserialisedScript seiScript

  scriptName <- case runQuote $ runExceptT $ traverseOf UPLC.progTerm UPLC.unDeBruijnTerm scriptNDB of
    Left _ -> fail "debruijn failure"
    Right x -> pure x
  let simplOpts = if UPLC._progVer scriptName == plcVersion100
        then
          (UPLC.defaultOptimizeOpts
            & UPLC.ooPreserveLogging .~ False
            & UPLC.ooApplyToCase .~ False
            -- & UPLC.ooInlineUnconditionalGrowth .~ 5
            -- & UPLC.ooInlineCallsiteGrowth .~ 10
          )
        else
          (UPLC.defaultOptimizeOpts
            & UPLC.ooPreserveLogging .~ False
            -- & UPLC.ooInlineUnconditionalGrowth .~ 5
            -- & UPLC.ooInlineCallsiteGrowth .~ 10
          )

      simplified = runQuote $
        UPLC.optimizeProgram
          simplOpts
          DefaultFunSemanticsVariantC
          scriptName

  simplifiedNDB <- case runQuote $ runExceptT $ traverseOf UPLC.progTerm UPLC.deBruijnTerm simplified of
    Left _ -> fail "debruijn failure"
    Right x -> pure x
  let simplifiedDB = UPLC.programMapNames UPLC.unNameDeBruijn simplifiedNDB

      simplifiedSerialised = serialiseUPLC simplifiedDB
      sizeAfter = BSS.length simplifiedSerialised
  simplifiedReady <- case deserialiseScript seiPlutusLedgerLanguage seiMajorProtocolVersion simplifiedSerialised of
    Right s -> pure s
    Left err -> fail $ "deserialization failure" <> show err
  let (_, resultAfter) =
        evaluateScriptCounting
          seiPlutusLedgerLanguage
          seiMajorProtocolVersion
          Quiet
          seiEvaluationContext
          simplifiedReady
          seiData

  (cpuAfter, memAfter) <- case resultAfter of
    Left _ -> fail "evaluation failed"
    Right budget -> pure $ fromBudget budget

  let cpuAbs = cpuBefore - cpuAfter
      memAbs = memBefore - memAfter
      cpuRel :: Double = fromIntegral cpuAbs / fromIntegral cpuBefore * 100
      memRel :: Double = fromIntegral memAbs / fromIntegral memBefore * 100
      sizeAbs = sizeBefore - sizeAfter
      sizeRel :: Double = fromIntegral sizeAbs / fromIntegral sizeBefore * 100

  atomicModifyIORef' statsRef
    (\xs -> (Stat cpuAbs memAbs (fromIntegral sizeAbs) cpuRel memRel sizeRel : xs, ()))
  print (cnt, cpuAbs, memAbs, sizeAbs, cpuRel, memRel, sizeRel)
