module Main (main) where

import Control.Exception (bracket)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Deserialise (deserialiseScripts)
import Load (Options (..), loadScriptEvents)
import Main.Utf8 (withUtf8)
import Materialise (materialiseViews)
import Options (parserInfo)
import Options.Applicative qualified as O
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdin, stdout)
import Text.Show.Pretty (pPrint)

main :: IO ()
main = withUtf8 do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering

  opts@Options{optsDatabaseConnStr} <- O.execParser parserInfo
  pPrint opts

  bracket (connectPostgreSQL optsDatabaseConnStr) close \conn -> do
    loadScriptEvents conn opts
    deserialiseScripts conn
    materialiseViews conn
