{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Plutus.Script.Evaluation.Options (
  Options (..),
  options,
  parserInfo,
)
where

import Cardano.Api (File (File), FileDirection (In), NodeConfigFile, SocketPath)
import Cardano.Api qualified as Cardano
import Data.Word (Word64)
import Options.Applicative qualified as O

data Options = Options
  { optsConfigPath :: NodeConfigFile In
  , optsSocketPath :: SocketPath
  , optsNetworkId :: Cardano.NetworkId
  , optsBlocksPerFile :: Word64
  , optsEventsPerFile :: Word64
  , optsDumpDir :: FilePath
  , optsCheckpointDir :: FilePath
  }
  deriving (Show)

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
  optsBlocksPerFile <-
    O.option O.auto $
      mconcat
        [ O.long "blocks-per-file"
        , O.metavar "BLOCKS_PER_FILE"
        , O.help "Write events in this many blocks per file (unless events-per-file is exceeded)"
        ]
  optsEventsPerFile <-
    O.option O.auto $
      mconcat
        [ O.long "events-per-file"
        , O.metavar "EVENTS_PER_FILE"
        , O.value maxBound
        , O.help "Write approximately this many events per file (unless blocks-per-file is exceeded)"
        ]
  optsDumpDir <-
    O.strOption
      ( mconcat
          [ O.long "dump-dir"
          , O.metavar "DUMP_DIR"
          , O.help "Directory to dump the events to"
          ]
      )
  optsCheckpointDir <-
    O.strOption
      ( mconcat
          [ O.long "checkpoint-dir"
          , O.metavar "CHECKPOINT_DIR"
          , O.help "Directory to store the checkpoint files"
          ]
      )
  pure Options{..}

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
