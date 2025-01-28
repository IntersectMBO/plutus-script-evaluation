{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StrictData #-}

module Options (
  Options (..),
  options,
  parserInfo,
)
where

import Cardano.Slotting.Block (BlockNo (BlockNo))
import Data.ByteString (ByteString)
import Options.Applicative qualified as O

data Options = Options
  { optsDatabaseConnStr :: ByteString
  , startBlock :: BlockNo
  }
  deriving (Show)

options :: O.Parser Options
options = do
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
  startBlock <-
    O.option
      (O.maybeReader (Just . BlockNo . read))
      ( mconcat
          [ O.long "start-block"
          , O.metavar "BLOCK_NO"
          , O.help "Block number to start from"
          ]
      )
  pure Options{..}

parserInfo :: O.ParserInfo Options
parserInfo = O.info (options O.<**> O.helper) mempty
