module Main (main) where

import Load (loadScriptEvents)
import Main.Utf8 (withUtf8)
import Options (parserInfo)
import Options.Applicative qualified as O
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdin, stdout)

{- Example:

cabal run load-script-events -- \
  --mainnet \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --config "$CARDANO_NODE_CONFIG_PATH" \
  --checkpoint-dir dumps/checkpoints
-}
main :: IO ()
main = withUtf8 do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  loadScriptEvents =<< O.execParser parserInfo
