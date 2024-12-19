module Main where

import Control.Exception (bracket)
import Database.PostgreSQL.Simple qualified as PG
import Evaluate (ScriptEvaluationInput, evaluateScripts)
import Main.Utf8 (withUtf8)
import Options (Options (..), parserInfo)
import Options.Applicative (execParser)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdin, stdout)

main :: IO ()
main = withUtf8 do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  Options{optsDatabaseConnStr} <- execParser parserInfo
  bracket (PG.connectPostgreSQL optsDatabaseConnStr) PG.close \conn -> do
    _result <- evaluateScripts conn 0 onScriptEvaluationInput
    putStrLn "Done evaluating scripts"
    pure ()
