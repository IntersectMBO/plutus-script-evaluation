{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StrictData #-}

module Options (
  Options (..),
  options,
  parserInfo,
)
where

import Options.Applicative qualified as O
import Path (Dir, SomeBase, parseSomeDir)

newtype Options = Options
  { optsEventsDir :: SomeBase Dir
  }
  deriving stock (Show)

options :: O.Parser Options
options = do
  optsEventsDir <-
    O.option absDirParser $
      mconcat
        [ O.long "event-dir"
        , O.metavar "EVENT_DIR"
        , O.help "Directory to read events from"
        ]

  pure Options{..}

absDirParser :: O.ReadM (SomeBase Dir)
absDirParser = O.str >>= either (fail . show) pure . parseSomeDir

parserInfo :: O.ParserInfo Options
parserInfo =
  O.info
    (options O.<**> O.helper)
    mempty
