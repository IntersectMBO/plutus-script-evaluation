{- | This module contains the main entry point
into the program which CEK-evaluates scripts using
the information recorded in the database in a
streaming fashion.
-}
module Main where

import Control.Exception (bracket, catch)
import Data.Text qualified as Text
import Data.List (sort)
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple qualified as PG
import Evaluate (evaluateScripts, onScriptEvaluationInput, statsRef, Stat(..), counterRef)
import Main.Utf8 (withUtf8)
import Options (Options (..), parserInfo)
import Options.Applicative (execParser)
import System.Exit (exitFailure)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdin, stdout)
import UnliftIO (IORef, MonadIO, atomicModifyIORef',  liftIO, newIORef, readIORef, writeIORef)
import Text.Printf

median :: (Ord a) => [a] -> a
median [] = error "median: empty list"
median xs = sorted !! (length xs `div` 2)
  where sorted = sort xs

main :: IO ()
main = withUtf8 do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  Options{optsDatabaseConnStr, startFrom} <- execParser parserInfo
  displaySqlError $
    bracket (PG.connectPostgreSQL optsDatabaseConnStr) PG.close \conn -> do
      evaluateScripts conn startFrom onScriptEvaluationInput
      putStrLn "Done evaluating scripts"
  stats <- readIORef statsRef
  cntActual <- readIORef counterRef
  let cnt = length stats
  let cntD :: Double
      cntD = fromIntegral cnt
      avgCpuAbs :: Double
      avgCpuAbs = fromIntegral (sum $ statCpuAbs <$> stats) / cntD

      avgMemAbs :: Double
      avgMemAbs = fromIntegral (sum $ statMemAbs <$> stats) / cntD

      avgSizeAbs :: Double
      avgSizeAbs = fromIntegral (sum $ statSizeAbs <$> stats) / cntD

      medianCpuAbs :: Integer
      medianCpuAbs = median $ statCpuAbs <$> stats

      medianMemAbs :: Integer
      medianMemAbs = median $ statMemAbs <$> stats

      medianSizeAbs :: Integer
      medianSizeAbs = median $ statSizeAbs <$> stats

      maxCpuAbs :: Integer
      maxCpuAbs = maximum $ statCpuAbs <$> stats

      maxMemAbs :: Integer
      maxMemAbs = maximum $ statMemAbs <$> stats

      maxSizeAbs :: Integer
      maxSizeAbs = maximum $ statSizeAbs <$> stats

      avgCpuRel :: Double
      avgCpuRel = (sum $ statCpuRel <$> stats) / cntD

      avgMemRel :: Double
      avgMemRel = (sum $ statMemRel <$> stats) / cntD

      avgSizeRel :: Double
      avgSizeRel = (sum $ statSizeRel <$> stats) / cntD

      medianCpuRel :: Double
      medianCpuRel = median $ statCpuRel <$> stats

      medianMemRel :: Double
      medianMemRel = median $ statMemRel <$> stats

      medianSizeRel :: Double
      medianSizeRel = median $ statSizeRel <$> stats

      maxCpuRel :: Double
      maxCpuRel = maximum $ statCpuRel <$> stats

      maxMemRel :: Double
      maxMemRel = maximum $ statMemRel <$> stats

      maxSizeRel :: Double
      maxSizeRel = maximum $ statSizeRel <$> stats
  putStrLn  $ "count, len = " <> show (cntActual, cnt)
  printf "CPU units: avg = %d, median = %d, max = %d\n" (round avgCpuAbs :: Integer) medianCpuAbs maxCpuAbs

  printf "Mem units: avg = %d, median = %d, max = %d\n" (round avgMemAbs :: Integer) medianMemAbs maxMemAbs

  printf "Cbor size: avg = %d, median = %d, max = %d\n" (round avgSizeAbs :: Integer) medianSizeAbs maxSizeAbs
  printf "CPU percentage: avg = %.2f%%, median = %.2f%%, max = %.2f%%\n" avgCpuRel medianCpuRel maxCpuRel
  printf "Mem percentage: avg = %.2f%%, median = %.2f%%, max = %.2f%%\n" avgMemRel medianMemRel maxMemRel
  printf "Cbor size percentage: avg = %.2f%%, median = %.2f%%, max = %.2f%%\n" avgSizeRel medianSizeRel maxSizeRel

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
