module Main (main) where

import Control.Exception (bracket)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Deserialise (deserialiseScripts)
import Main.Utf8 (withUtf8)
import Options (Options (..), parserInfo)
import Options.Applicative (execParser)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdin, stdout)

main :: IO ()
main = withUtf8 do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  Options{optsDatabaseConnStr} <- execParser parserInfo
  bracket (connectPostgreSQL optsDatabaseConnStr) close deserialiseScripts
