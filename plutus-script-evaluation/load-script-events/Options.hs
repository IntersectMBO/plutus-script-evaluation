{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StrictData #-}

module Options (
  Options (..),
  options,
  parserInfo,
)
where

import Cardano.Api (File (File), FileDirection (In), NodeConfigFile, SocketPath)
import Cardano.Api qualified as Cardano
import Data.ByteString (ByteString)
import Options.Applicative qualified as O
import Path (Dir, SomeBase, parseSomeDir)

data Options = Options
  { optsConfigPath :: NodeConfigFile In
  , optsSocketPath :: SocketPath
  , optsNetworkId :: Cardano.NetworkId
  , optsCheckpointDir :: SomeBase Dir
  , optsDatabaseConnStr :: ByteString
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

  optsCheckpointDir <-
    O.option absDirParser $
      mconcat
        [ O.long "checkpoint-dir"
        , O.metavar "CHECKPOINT_DIR"
        , O.help "Directory to store the checkpoint files"
        ]

  optsDatabaseConnStr <-
    O.strOption
      ( mconcat
          [ O.long "database-conn-str"
          , O.metavar "CONN_STR"
          , O.help
              "Database connection string, see \
              \https://www.postgresql.org\
              \/docs/current/libpq-connect.html#LIBPQ-CONNSTRING"
          ]
      )
  pure Options{..}

absDirParser :: O.ReadM (SomeBase Dir)
absDirParser = O.str >>= either (fail . show) pure . parseSomeDir

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
