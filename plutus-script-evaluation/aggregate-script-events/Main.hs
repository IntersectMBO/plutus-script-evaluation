module Main (main) where

import Aggregate (aggregateScriptEvents)
import Main.Utf8 (withUtf8)
import Options (parserInfo)
import Options.Applicative (execParser)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdin, stdout)

{- Example:

cabal run aggregate-script-events -- --event-dir dumps/events

-}
main :: IO ()
main = withUtf8 do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  aggregateScriptEvents =<< execParser parserInfo