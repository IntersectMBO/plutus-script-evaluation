{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StrictData #-}

module Options (
  Options (..),
  options,
  parserInfo,
) where

import Data.ByteString (ByteString)
import Options.Applicative qualified as O

newtype Options = Options
  { optsDatabaseConnStr :: ByteString
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
              "postgresql connection string"
          ]
      )
  pure Options{..}

parserInfo :: O.ParserInfo Options
parserInfo =
  O.info
    (options O.<**> O.helper)
    ( mconcat
        [ O.fullDesc
        , O.progDesc
            "Check all serialised scripts against the vanRossemPV \
            \deserialisation bounds (constant type header size, \
            \constr field count)"
        , O.header "check-decoder-bounds - verify maxBoundsByPV against on-chain scripts"
        ]
    )
