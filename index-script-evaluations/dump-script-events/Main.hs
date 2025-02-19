module Main (main) where

import Dump (dumpScriptEvents)
import Main.Utf8 (withUtf8)
import Options (parserInfo)
import Options.Applicative qualified as O
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdin, stdout)

{- Example:

cabal run dump-script-events -- \
  --mainnet \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --config "$CARDANO_NODE_CONFIG_PATH" \
  --events-per-file 50000 \
  --event-dir dumps/events \
  --checkpoint-dir dumps/checkpoints
-}
main :: IO ()
main = withUtf8 do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  dumpScriptEvents =<< O.execParser parserInfo
