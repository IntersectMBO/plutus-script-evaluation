{- | This module analyzes script contexts from the database
and collects statistics on the number of policies and tokens
in transaction output Values.
-}
module Main where

import Codec.Serialise (deserialise)
import Control.Concurrent (myThreadId)
import Control.Exception (AsyncException (UserInterrupt), bracket, catch, throwIO, throwTo, try)
import Control.Monad (forM_, when)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (foldl')
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Orphans ()
import Database.PostgreSQL.Simple.Types (Only (..))
import Main.Utf8 (withUtf8)
import Options (Options (..), parserInfo)
import Options.Applicative (execParser)
import PlutusLedgerApi.Common (Data, PlutusLedgerLanguage (..))
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V2 qualified as V2
import PlutusLedgerApi.V3 qualified as V3
import System.Exit (exitFailure)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdin, stdout)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT)
import Text.Printf (printf)
import ValueStats (
  StatsAccumulator (..),
  analyzeValue,
  emptyAccumulator,
  loadCheckpoint,
  printReport,
  saveCheckpoint,
  updateAccumulator,
  writeTextReport,
 )

--------------------------------------------------------------------------------
-- Data Types ------------------------------------------------------------------

-- | Strict accumulator state for PostgreSQL fold
data FoldState = MkFoldState
  { fsAccumulator :: !StatsAccumulator
  , fsRowCount :: !Int64
  }

-- | Strict state for checkpointing
data CheckpointState = MkCheckpointState
  { csAccumulator :: !StatsAccumulator
  , csLastPk :: !Int64
  , csTotalRows :: !Int64
  }

--------------------------------------------------------------------------------
-- Main Entry Point ------------------------------------------------------------

main :: IO ()
main = withUtf8 do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  Options{optsDatabaseConnStr, optsSamplePercent, optsCheckpointFile, optsTextOutput} <-
    execParser parserInfo

  -- Validate option combinations and dispatch to appropriate mode
  case (optsDatabaseConnStr, optsCheckpointFile) of
    -- Report-only mode: checkpoint file without database
    (Nothing, Just checkpointPath) ->
      generateReportFromCheckpoint checkpointPath optsTextOutput
    -- Database mode: with or without checkpoint for resuming
    (Just connStr, _) -> do
      putStrLn "Starting value statistics collection..."
      displaySqlError $
        bracket
          (PG.connectPostgreSQL connStr)
          (\conn -> PG.close conn `catch` \(_ :: PG.SqlError) -> pure ())
          \conn -> do
            acc <- case optsSamplePercent of
              Just percent -> collectStatisticsSample conn percent
              Nothing -> collectStatisticsFull conn optsCheckpointFile
            printReport acc

            -- Write output file if specified
            forM_ optsTextOutput \path -> writeTextReport path acc

            putStrLn "Done collecting statistics"

    -- Error: neither database nor checkpoint provided
    (Nothing, Nothing) -> do
      putStrLn "ERROR: Must specify either --database-conn-str or --checkpoint-file"
      putStrLn "Use --help for usage information"
      exitFailure

displaySqlError :: IO () -> IO ()
displaySqlError action =
  action `catch` \case
    PG.SqlError
      { sqlState
      , sqlExecStatus
      , sqlErrorMsg
      , sqlErrorDetail
      , sqlErrorHint
      } -> do
        let toStr = Text.unpack . decodeUtf8
        putStrLn $ "SQL State: " <> toStr sqlState
        putStrLn $ "SQL Exec Status: " <> show sqlExecStatus
        putStrLn $ "SQL Error Message: " <> toStr sqlErrorMsg
        putStrLn $ "SQL Error Detail: " <> toStr sqlErrorDetail
        putStrLn $ "SQL Error Hint: " <> toStr sqlErrorHint
        exitFailure

--------------------------------------------------------------------------------
-- Report Generation from Checkpoint -------------------------------------------

-- | Generate report from checkpoint file without database access
generateReportFromCheckpoint :: FilePath -> Maybe FilePath -> IO ()
generateReportFromCheckpoint checkpointPath maybeTextOutput = do
  putStrLn "Report-only mode: generating report from checkpoint..."
  result <- loadCheckpoint checkpointPath
  case result of
    Nothing -> do
      putStrLn $ "ERROR: Failed to load checkpoint from: " <> checkpointPath
      putStrLn "Ensure the checkpoint file exists and is valid JSON"
      exitFailure
    Just (acc, lastPk, rowCount) -> do
      printf "Successfully loaded checkpoint (last pk: %d, rows: %d)\n" lastPk rowCount
      printReport acc

      -- Write output file if specified
      forM_ maybeTextOutput \path -> writeTextReport path acc

      putStrLn "Done generating report"

--------------------------------------------------------------------------------
-- Statistics Collection -------------------------------------------------------

-- | Collect statistics using random sampling
collectStatisticsSample :: PG.Connection -> Double -> IO StatsAccumulator
collectStatisticsSample conn samplePercent = do
  -- Query total count first
  putStrLn "Counting total script contexts in database..."
  [Only (totalCount :: Int64)] <-
    PG.query_ conn "SELECT COUNT(*) FROM script_evaluation_events"

  let sampleRate = samplePercent / 100.0
      expectedSampleSize = round (fromIntegral totalCount * sampleRate) :: Int64

  putStrLn $ "Total script contexts: " <> show totalCount
  printf "Using %.1f%% random sampling (~%d records)\n\n" samplePercent expectedSampleSize
  putStrLn "Streaming sampled script contexts from database..."

  MkFoldState{fsAccumulator = finalAcc} <-
    PG.fold
      conn
      "SELECT see.pk, ss.ledger_language, see.script_context \
      \FROM script_evaluation_events see \
      \TABLESAMPLE SYSTEM (?) \
      \JOIN serialised_scripts ss ON see.script_hash = ss.hash \
      \ORDER BY see.pk ASC"
      (Only samplePercent)
      (MkFoldState emptyAccumulator 0)
      (processRow expectedSampleSize)
  pure finalAcc
 where
  processRow
    :: Int64
    -> FoldState
    -> (Int64, PlutusLedgerLanguage, BS.ByteString)
    -> IO FoldState
  processRow expectedSize (MkFoldState acc rowCount) (pk, ledgerLang, contextBytes) = do
    let !newRowCount = rowCount + 1
    when (newRowCount `mod` 1_000 == 0) do
      let percent = (100.0 :: Double) * fromIntegral newRowCount / fromIntegral (max 1 expectedSize)
      printf "Processing pk %d (%.2f%%)\n" pk percent

    case deserialiseAndExtractValues ledgerLang contextBytes of
      Nothing -> do
        putStrLn $
          "ERROR: Failed to parse script context at pk="
            <> show pk
            <> " with ledger language "
            <> show ledgerLang
        exitFailure
      Just values -> do
        -- Process values eagerly to avoid building up lazy list of ValueStats
        let !newAcc = foldl' (\a v -> updateAccumulator a (analyzeValue v)) acc values
        pure $! MkFoldState newAcc newRowCount

-- | Collect statistics from all rows with optional checkpointing
collectStatisticsFull :: PG.Connection -> Maybe FilePath -> IO StatsAccumulator
collectStatisticsFull conn maybeCheckpointFile = do
  -- Try to load checkpoint if file specified
  (initialAcc, startPk, alreadyProcessedRows) <-
    case maybeCheckpointFile of
      Nothing -> do
        putStrLn "Full scan mode (no checkpointing)"
        pure (emptyAccumulator, 0, 0)
      Just checkpointPath ->
        loadCheckpoint checkpointPath >>= \case
          Nothing -> do
            putStrLn $ "Starting fresh (checkpoint file: " <> checkpointPath <> ")"
            pure (emptyAccumulator, 0, 0)
          Just (acc, lastPk, rowCount) -> do
            putStrLn $ "Resuming from checkpoint (last pk: " <> show lastPk <> ")"
            pure (acc, lastPk, rowCount)

  -- Query total count and remaining count
  putStrLn "Counting total script contexts in database..."
  [Only (totalCount :: Int64)] <-
    PG.query_ conn "SELECT COUNT(*) FROM script_evaluation_events"
  [Only (remainingCount :: Int64)] <-
    PG.query conn "SELECT COUNT(*) FROM script_evaluation_events WHERE pk > ?" (Only startPk)

  putStrLn $ "Total script contexts: " <> show totalCount
  putStrLn $ "Already processed: " <> show alreadyProcessedRows
  putStrLn $ "Remaining to process: " <> show remainingCount
  putStrLn "Streaming script contexts from database...\n"

  -- Create IORef to track current state (accumulator, last pk, row count)
  stateRef <- newIORef (MkCheckpointState initialAcc startPk alreadyProcessedRows)

  -- Install signal handler for graceful Ctrl+C
  mainThreadId <- myThreadId
  case maybeCheckpointFile of
    Just checkpointPath -> do
      _ <- installHandler sigINT (Catch $ handleInterrupt stateRef checkpointPath mainThreadId) Nothing
      pure ()
    Nothing -> pure ()

  -- Run fold with UserInterrupt handling
  result <-
    try $
      PG.fold
        conn
        "SELECT see.pk, ss.ledger_language, see.script_context \
        \FROM script_evaluation_events see \
        \JOIN serialised_scripts ss ON see.script_hash = ss.hash \
        \WHERE see.pk > ? \
        \ORDER BY see.pk ASC"
        (Only startPk)
        (MkFoldState initialAcc 0)
        (processRowFull totalCount alreadyProcessedRows stateRef maybeCheckpointFile)

  case result of
    Left UserInterrupt -> do
      -- UserInterrupt caught, checkpoint already saved by signal handler
      -- Note: Connection may be in a bad state but bracket will handle cleanup
      -- The libpq error during cleanup is unavoidable but harmless
      MkCheckpointState{csAccumulator = finalAcc} <- readIORef stateRef
      pure finalAcc
    Left otherException ->
      -- Re-throw other async exceptions (StackOverflow, HeapOverflow, etc.)
      throwIO otherException
    Right (MkFoldState finalAcc _) -> pure finalAcc
 where
  -- Signal handler to save checkpoint and exit gracefully
  handleInterrupt stateRef checkpointPath mainThreadId = do
    putStrLn "\n\nInterrupted. Saving checkpoint..."
    MkCheckpointState{csAccumulator = acc, csLastPk = lastPk, csTotalRows = rowCount} <- readIORef stateRef
    saveCheckpoint checkpointPath acc lastPk rowCount
    putStrLn "Checkpoint saved. Run again to resume from this point."
    throwTo mainThreadId UserInterrupt

  processRowFull
    :: Int64
    -> Int64
    -> IORef CheckpointState
    -> Maybe FilePath
    -> FoldState
    -> (Int64, PlutusLedgerLanguage, BS.ByteString)
    -> IO FoldState
  processRowFull totalCount alreadyProcessedRows stateRef maybeCheckpointPath (MkFoldState acc rowCount) (pk, ledgerLang, contextBytes) = do
    let !newRowCount = rowCount + 1
        !totalRowsProcessed = alreadyProcessedRows + newRowCount

    -- Progress tracking
    when (newRowCount `mod` 1_000 == 0) do
      let percent = (100.0 :: Double) * fromIntegral totalRowsProcessed / fromIntegral (max 1 totalCount)
      printf "Processing pk %d (%d / %d = %.2f%%)\n" pk totalRowsProcessed totalCount percent

    -- Checkpoint saving
    when (newRowCount `mod` 100_000 == 0) do
      forM_ maybeCheckpointPath \checkpointPath ->
        saveCheckpoint checkpointPath acc pk totalRowsProcessed

    case deserialiseAndExtractValues ledgerLang contextBytes of
      Nothing -> do
        putStrLn $
          "ERROR: Failed to parse script context at pk="
            <> show pk
            <> " with ledger language "
            <> show ledgerLang
        exitFailure
      Just values -> do
        -- Process values eagerly to avoid building up lazy list of ValueStats
        let !newAcc = foldl' (\a v -> updateAccumulator a (analyzeValue v)) acc values
        -- Update state ref for signal handler
        writeIORef stateRef $! MkCheckpointState newAcc pk totalRowsProcessed
        pure $! MkFoldState newAcc newRowCount

--------------------------------------------------------------------------------
-- Script Context Parsing ------------------------------------------------------

-- | Deserialize script context and extract Values from transaction outputs
deserialiseAndExtractValues
  :: PlutusLedgerLanguage
  -> BS.ByteString
  -> Maybe [V1.Value]
deserialiseAndExtractValues ledgerLang contextBytes = do
  let contextData = deserialise @Data (BSL.fromStrict contextBytes)
  case ledgerLang of
    PlutusV1 -> do
      ctx <- V1.fromData contextData :: Maybe V1.ScriptContext
      let outputs = V1.txInfoOutputs $ V1.scriptContextTxInfo ctx
      pure $ V1.txOutValue <$> outputs
    PlutusV2 -> do
      ctx <- V2.fromData contextData :: Maybe V2.ScriptContext
      let outputs = V2.txInfoOutputs $ V2.scriptContextTxInfo ctx
      pure $ V2.txOutValue <$> outputs
    PlutusV3 -> do
      ctx <- V3.fromData contextData :: Maybe V3.ScriptContext
      let outputs = V3.txInfoOutputs $ V3.scriptContextTxInfo ctx
      -- V3 uses V2.Value
      pure $ V3.txOutValue <$> outputs
