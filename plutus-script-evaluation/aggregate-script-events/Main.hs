module Main (main) where

import Aggregate (aggregateScriptEvents)
import Options (parserInfo)
import Options.Applicative (execParser)

{- Example:

cabal run aggregate-script-events -- --events-dir dumps/events

-}
main :: IO ()
main = aggregateScriptEvents =<< execParser parserInfo
