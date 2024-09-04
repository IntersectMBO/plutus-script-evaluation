module Main (main) where

import Dump (dumpScriptEvents)
import Options (parserInfo)
import Options.Applicative qualified as O

{- Example:

cabal run dump-script-events -- \
  --mainnet \
  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
  --config "$CARDANO_NODE_CONFIG_PATH" \
  --blocks-per-file 50000 \
  --events-per-file 50000 \
  --dump-dir dumps \
  --checkpoint-dir dumps/checkpoints
-}
main :: IO ()
main = dumpScriptEvents =<< O.execParser parserInfo
