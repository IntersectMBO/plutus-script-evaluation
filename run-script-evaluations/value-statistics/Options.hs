{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StrictData #-}

module Options (
  Options (..),
  options,
  parserInfo,
) where

import Data.ByteString (ByteString)
import Options.Applicative qualified as O

data Options = Options
  { optsDatabaseConnStr :: Maybe ByteString
  , optsSamplePercent :: Maybe Double
  , optsCheckpointFile :: Maybe FilePath
  , optsTextOutput :: Maybe FilePath
  }
  deriving (Show)

options :: O.Parser Options
options = do
  optsDatabaseConnStr <-
    O.optional $
      O.strOption
        ( mconcat
            [ O.long "database-conn-str"
            , O.metavar "CONN_STR"
            , O.help
                "Database connection string (omit for report-only mode), see \
                \https://www.postgresql.org\
                \/docs/current/libpq-connect.html#LIBPQ-CONNSTRING"
            ]
        )
  optsSamplePercent <-
    O.optional $
      O.option
        O.auto
        ( mconcat
            [ O.long "sample-percent"
            , O.metavar "PERCENT"
            , O.help "Percentage of records to sample (omit for full scan)"
            ]
        )
  optsCheckpointFile <-
    O.optional $
      O.strOption
        ( mconcat
            [ O.long "checkpoint-file"
            , O.metavar "FILE"
            , O.help "JSON checkpoint file for resuming full scans"
            ]
        )
  optsTextOutput <-
    O.optional $
      O.strOption
        ( mconcat
            [ O.long "text-output"
            , O.metavar "FILE"
            , O.help "Output path for plain text report"
            ]
        )
  pure Options{..}

parserInfo :: O.ParserInfo Options
parserInfo =
  O.info
    (options O.<**> O.helper)
    ( mconcat
        [ O.fullDesc
        , O.progDesc "Collect statistics on Plutus Value structures from script contexts"
        , O.header "value-statistics - analyze token quantities in Cardano transactions"
        ]
    )
