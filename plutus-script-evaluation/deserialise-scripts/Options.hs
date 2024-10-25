{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StrictData #-}

module Options (
  Options (..),
  options,
  parserInfo,
)
where

import Deserialise (Options (..))
import Options.Applicative qualified as O

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
  pure Options{..}

parserInfo :: O.ParserInfo Options
parserInfo = O.info (options O.<**> O.helper) mempty
