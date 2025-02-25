{- | This module contains the main entry point
into the program which CEK-evaluates scripts using
the information recorded in the database in a
streaming fashion.
-}
module Main where

import Control.Exception (bracket, catch)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple qualified as PG
import Evaluate (evaluateScripts, onScriptEvaluationInput)
import Main.Utf8 (withUtf8)
import Options (Options (..), parserInfo)
import Options.Applicative (execParser)
import System.Exit (exitFailure)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdin, stdout)

main :: IO ()
main = withUtf8 do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  Options{optsDatabaseConnStr, startFrom} <- execParser parserInfo
  displaySqlError $
    bracket (PG.connectPostgreSQL optsDatabaseConnStr) PG.close \conn -> do
      evaluateScripts conn startFrom onScriptEvaluationInput
      putStrLn "Done evaluating scripts"

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
