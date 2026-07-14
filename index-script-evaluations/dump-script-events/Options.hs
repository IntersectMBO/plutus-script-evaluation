{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StrictData #-}

module Options (
  Options (..),
  options,
  parserInfo,
)
where

import Cardano.Api (File (File))
import Cardano.Api qualified as Cardano
import Data.Word (Word64)
import Dump (Options (..))
import Options.Applicative qualified as O
import Path (Dir, SomeBase, parseSomeDir)

options :: O.Parser Options
options = do
  optsSocketPath <-
    File
      <$> O.strOption
        ( mconcat
            [ O.long "socket-path"
            , O.metavar "SOCKET_PATH"
            , O.help "Node socket path"
            ]
        )
  optsConfigPath <-
    File
      <$> O.strOption
        ( mconcat
            [ O.long "config"
            , O.metavar "CONFIG_PATH"
            , O.help "Node config path"
            ]
        )
  optsNetworkId <- networkIdParser
  optsEventsPerFile <-
    O.option positiveWord64 $
      mconcat
        [ O.long "events-per-file"
        , O.metavar "EVENTS_PER_FILE"
        , O.value maxBound
        , O.help "Write approximately this many events per file"
        ]
  optsDumpDir <-
    O.option absDirParser $
      mconcat
        [ O.long "event-dir"
        , O.metavar "EVENT_DIR"
        , O.help "Directory to dump the events to"
        ]

  optsCheckpointDir <-
    O.option absDirParser $
      mconcat
        [ O.long "checkpoint-dir"
        , O.metavar "CHECKPOINT_DIR"
        , O.help "Directory to store the checkpoint files"
        ]

  pure Options{..}

absDirParser :: O.ReadM (SomeBase Dir)
absDirParser = O.str >>= either (fail . show) pure . parseSomeDir

{- | Parse a positive 'Word64', rejecting 0 so the events buffer is never
flushed on an empty list (which would crash on @NE.fromList []@).
-}
positiveWord64 :: O.ReadM Word64
positiveWord64 =
  O.auto >>= \n ->
    if n == 0
      then O.readerError "events-per-file must be at least 1"
      else pure n

parserInfo :: O.ParserInfo Options
parserInfo =
  O.info
    (options O.<**> O.helper)
    mempty

networkIdParser :: O.Parser Cardano.NetworkId
networkIdParser =
  pMainnet' O.<|> fmap Cardano.Testnet testnetMagicParser
 where
  pMainnet' :: O.Parser Cardano.NetworkId
  pMainnet' =
    O.flag'
      Cardano.Mainnet
      $ mconcat
        [ O.long "mainnet"
        , O.help "Use the mainnet magic id."
        ]

testnetMagicParser :: O.Parser Cardano.NetworkMagic
testnetMagicParser =
  Cardano.NetworkMagic
    <$> O.option
      O.auto
      ( mconcat
          [ O.long "testnet-magic"
          , O.metavar "NATURAL"
          , O.help "Specify a testnet magic id."
          ]
      )
