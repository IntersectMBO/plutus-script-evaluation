{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StrictData #-}

module Options (
  Options (..),
  options,
  parserInfo,
)
where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Options.Applicative qualified as O

data Options = Options
  { optsDatabaseConnStr :: ByteString
  , startFrom :: Int64
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
  startFrom <-
    O.option
      (O.maybeReader (Just . read))
      ( mconcat
          [ O.long "start-from"
          , O.metavar "START_FROM"
          , O.help "Primary key value to start evaluation from"
          ]
      )
  pure Options{..}

parserInfo :: O.ParserInfo Options
parserInfo = O.info (options O.<**> O.helper) mempty
