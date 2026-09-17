module Main where

import Codec.CBOR.Read qualified as CBOR
import Codec.Extras.SerialiseViaFlat (decodeViaFlatWith)
import Control.Exception (bracket, catch)
import Control.Monad (forM_, unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int64)
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Some (withSome)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Orphans ()
import Database.PostgreSQL.Simple.Types (Only (..))
import GHC.Generics (Generic)
import Main.Utf8 (withUtf8)
import NoThunks.Class (NoThunks, unsafeNoThunks)
import Options (Options (..), parserInfo)
import Options.Applicative (execParser)
import PlutusCore (ValueOf (..))
import PlutusCore.DeBruijn.Internal (FakeNamedDeBruijn)
import PlutusCore.Default (defaultUniSize)
import PlutusLedgerApi.Common (PlutusLedgerLanguage, vanRossemPV)
import PlutusLedgerApi.Common.Versions (MaxBounds (..), maxBoundsByPV)
import System.Exit (ExitCode (ExitFailure), die, exitFailure, exitWith)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import Text.Printf (printf)
import UntypedPlutusCore qualified as U

-- | The bounds we are testing against, taken from plutus rather than copied.
strictBounds :: MaxBounds
strictBounds = maxBoundsByPV vanRossemPV

maxHeaderBound, maxConstrBound :: Int
maxHeaderBound = mbHeader strictBounds
maxConstrBound = mbConstr strictBounds

{- | How many violations and decode failures to keep for the report. The counts
are always exact; only the listed examples are capped, so a systemic decode
break cannot grow the heap with the table.
-}
maxReportedSamples :: Int64
maxReportedSamples = 100

{- | How often to print progress and assert that the accumulator carries no
thunks. 'assertNoThunks' walks the whole accumulator, so it must not run per
row.
-}
progressInterval :: Int64
progressInterval = 10_000

data ScriptMeasures = MkScriptMeasures
  { smHeaderSize :: !Int
  , smConstrFields :: !Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (NoThunks)

data Extreme = MkExtreme
  { exValue :: !Int
  , exHash :: !Text
  }
  deriving stock (Generic)
  deriving anyclass (NoThunks)

data LangStats = MkLangStats
  { lsCount :: !Int64
  , lsMaxHeader :: !Extreme
  , lsMaxConstr :: !Extreme
  }
  deriving stock (Generic)
  deriving anyclass (NoThunks)

-- | A script that exceeds at least one of the bounds.
data Violation = MkViolation
  { vlHash :: !Text
  , vlLanguage :: !PlutusLedgerLanguage
  , vlMeasures :: !ScriptMeasures
  }
  deriving stock (Generic)
  deriving anyclass (NoThunks)

{- | A row we could not decode. The message is 'Text' rather than 'String': the
cons cells of a lazily produced 'String' are themselves thunks, which both
retains the underlying failure and trips 'assertNoThunks'.
-}
data DecodeFailure = MkDecodeFailure
  { dfHash :: !Text
  , dfError :: !Text
  }
  deriving stock (Generic)
  deriving anyclass (NoThunks)

data FoldState = MkFoldState
  { fsRowCount :: !Int64
  , fsPerLanguage :: !(Map PlutusLedgerLanguage LangStats)
  , fsViolationCount :: !Int64
  -- ^ exact count, unlike 'fsViolations'
  , fsViolations :: ![Violation]
  -- ^ at most 'maxReportedSamples' entries, newest first
  , fsDecodeFailureCount :: !Int64
  -- ^ exact count, unlike 'fsDecodeFailures'
  , fsDecodeFailures :: ![DecodeFailure]
  -- ^ at most 'maxReportedSamples' entries, newest first
  }
  deriving stock (Generic)
  deriving anyclass (NoThunks)

initialState :: FoldState
initialState = MkFoldState 0 Map.empty 0 [] 0 []

{- | Fail loudly if the accumulator has grown a thunk. Every 'FoldState' field is
strict, and the bang on @newRowCount@ in 'processRow' forces the incoming state
each row, so the structure should stay fully evaluated. That reasoning is four
steps long and one careless edit breaks it silently, hence the assertion.
-}
assertNoThunks :: FoldState -> IO ()
assertNoThunks st =
  case unsafeNoThunks st of
    Nothing -> pure ()
    Just info ->
      error $ "Thunk detected in the fold accumulator: " <> show info

{- | Prepend an element, forcing it. A plain list has a lazy head, so a bang on
the list alone forces only the cons cell: the element stays a thunk and the
strict fields inside it never fire. 'assertNoThunks' reports exactly that as
@["DecodeFailure","List","fsDecodeFailures","FoldState"]@.
-}
strictCons :: a -> [a] -> [a]
strictCons !x xs = x : xs

main :: IO ()
main = withUtf8 do
  hSetBuffering stdout LineBuffering
  Options{optsDatabaseConnStr} <- execParser parserInfo
  displaySqlError $
    bracket
      (PG.connectPostgreSQL optsDatabaseConnStr)
      (\conn -> PG.close conn `catch` \(_ :: PG.SqlError) -> pure ())
      \conn -> do
        totalCount <- countScripts conn
        printf "Checking %d distinct scripts...\n" totalCount
        finalState <-
          PG.fold_
            conn
            "SELECT encode(hash, 'hex'), ledger_language, serialised \
            \FROM serialised_scripts"
            initialState
            (processRow totalCount)
        printReport finalState

{- | Total row count, used only for the progress display. An explicit case
rather than a one-row pattern bind: a pattern-match failure here would be an
opaque @user error@ that walks straight past 'displaySqlError'.
-}
countScripts :: PG.Connection -> IO Int64
countScripts conn = do
  rows :: [Only Int64] <-
    PG.query_ conn "SELECT COUNT(*) FROM serialised_scripts"
  case rows of
    [Only n] -> pure n
    _ ->
      die $
        "SELECT COUNT(*) FROM serialised_scripts returned "
          <> show (length rows)
          <> " rows, expected exactly 1"

displaySqlError :: IO () -> IO ()
displaySqlError action =
  action `catch` \case
    PG.SqlError
      { sqlState
      , sqlExecStatus
      , sqlErrorMsg
      , sqlErrorDetail
      , sqlErrorHint
      } -> do
        let toStr = Text.unpack . decodeUtf8
        putStrLn $ "SQL State: " <> toStr sqlState
        putStrLn $ "SQL Exec Status: " <> show sqlExecStatus
        putStrLn $ "SQL Error Message: " <> toStr sqlErrorMsg
        putStrLn $ "SQL Error Detail: " <> toStr sqlErrorDetail
        putStrLn $ "SQL Error Hint: " <> toStr sqlErrorHint
        exitFailure

processRow
  :: Int64
  -> FoldState
  -> (Text, PlutusLedgerLanguage, BS.ByteString)
  -> IO FoldState
processRow totalCount st@MkFoldState{..} (hashHex, lang, serialised) = do
  let !newRowCount = fsRowCount + 1
      !next = case decodeScriptTerm serialised of
        Left err ->
          let !newFailureCount = fsDecodeFailureCount + 1
              !newFailures
                | newFailureCount <= maxReportedSamples =
                    MkDecodeFailure hashHex (Text.pack err) `strictCons` fsDecodeFailures
                | otherwise = fsDecodeFailures
           in st
                { fsRowCount = newRowCount
                , fsDecodeFailureCount = newFailureCount
                , fsDecodeFailures = newFailures
                }
        Right term ->
          let !measures = measureTerm term
              violates =
                smHeaderSize measures > maxHeaderBound
                  || smConstrFields measures > maxConstrBound
              !newPerLanguage =
                Map.insertWith
                  (<>)
                  lang
                  (langStats hashHex measures)
                  fsPerLanguage
              !newViolationCount = if violates then fsViolationCount + 1 else fsViolationCount
              !newViolations
                | violates
                , newViolationCount <= maxReportedSamples =
                    MkViolation hashHex lang measures `strictCons` fsViolations
                | otherwise = fsViolations
           in st
                { fsRowCount = newRowCount
                , fsPerLanguage = newPerLanguage
                , fsViolationCount = newViolationCount
                , fsViolations = newViolations
                }
  when (newRowCount `mod` progressInterval == 0) do
    let percent = (100.0 :: Double) * fromIntegral newRowCount / fromIntegral (max 1 totalCount)
    printf "Processed %d / %d scripts (%.2f%%)\n" newRowCount totalCount percent
    assertNoThunks next
  pure next

decodeScriptTerm
  :: BS.ByteString
  -> Either String (U.Term FakeNamedDeBruijn U.DefaultUni U.DefaultFun ())
decodeScriptTerm serialised =
  let acceptAll :: a -> Maybe String
      acceptAll _ = Nothing
      decoder = decodeViaFlatWith (U.decodeProgram acceptAll acceptAll acceptAll)
   in case CBOR.deserialiseFromBytes decoder (BSL.fromStrict serialised) of
        Left err -> Left (show err)
        Right (remainder, uplc)
          | remainder /= mempty -> Left "non-empty remainder after decoding"
          | otherwise -> Right (U._progTerm uplc)

measureTerm :: U.Term name U.DefaultUni fun () -> ScriptMeasures
measureTerm = go (MkScriptMeasures 0 0)
 where
  go !acc = \case
    U.Var{} -> acc
    U.LamAbs _ _ body -> go acc body
    U.Apply _ fun arg -> go (go acc fun) arg
    U.Force _ term -> go acc term
    U.Delay _ term -> go acc term
    U.Constant _ someValue ->
      withSome someValue \(ValueOf uni _) ->
        acc{smHeaderSize = max (smHeaderSize acc) (defaultUniSize uni)}
    U.Builtin{} -> acc
    U.Error{} -> acc
    U.Constr _ _ fields ->
      foldl' go acc{smConstrFields = max (smConstrFields acc) (length fields)} fields
    U.Case _ scrutinee branches ->
      foldl' go (go acc scrutinee) branches

langStats :: Text -> ScriptMeasures -> LangStats
langStats hashHex MkScriptMeasures{smHeaderSize, smConstrFields} =
  MkLangStats
    { lsCount = 1
    , lsMaxHeader = MkExtreme smHeaderSize hashHex
    , lsMaxConstr = MkExtreme smConstrFields hashHex
    }

instance Semigroup LangStats where
  a <> b =
    MkLangStats
      { lsCount = lsCount a + lsCount b
      , lsMaxHeader = maxExtreme (lsMaxHeader a) (lsMaxHeader b)
      , lsMaxConstr = maxExtreme (lsMaxConstr a) (lsMaxConstr b)
      }
   where
    maxExtreme x y = if exValue y > exValue x then y else x

printReport :: FoldState -> IO ()
printReport MkFoldState{..} = do
  printf "\nScripts checked: %d\n" fsRowCount
  printf
    "Bounds from vanRossemPV onwards: mbHeader = %d, mbConstr = %d\n\n"
    maxHeaderBound
    maxConstrBound

  forM_ (Map.toAscList fsPerLanguage) \(lang, MkLangStats{..}) -> do
    printf "%s (%d scripts):\n" (show lang) lsCount
    printf
      "  max constant type header size: %d (script %s)\n"
      (exValue lsMaxHeader)
      (Text.unpack (exHash lsMaxHeader))
    printf
      "  max constr field count:        %d (script %s)\n"
      (exValue lsMaxConstr)
      (Text.unpack (exHash lsMaxConstr))

  let globalMax f = maximum (0 : (exValue . f <$> Map.elems fsPerLanguage))
  printf "\nGlobal max constant type header size: %d (bound: %d)\n" (globalMax lsMaxHeader) maxHeaderBound
  printf "Global max constr field count:        %d (bound: %d)\n" (globalMax lsMaxConstr) maxConstrBound

  unless (fsDecodeFailureCount == 0) do
    printSampleHeader "DECODE FAILURES" fsDecodeFailureCount (length fsDecodeFailures)
    forM_ (reverse fsDecodeFailures) \MkDecodeFailure{..} ->
      printf "  %s: %s\n" (Text.unpack dfHash) (Text.unpack dfError)

  if fsViolationCount == 0
    then putStrLn "\nNo script exceeds the vanRossemPV bounds."
    else do
      printSampleHeader "VIOLATIONS" fsViolationCount (length fsViolations)
      forM_ (reverse fsViolations) \MkViolation{vlHash, vlLanguage, vlMeasures} ->
        printf
          "  %s (%s): header size %d, constr fields %d\n"
          (Text.unpack vlHash)
          (show vlLanguage)
          (smHeaderSize vlMeasures)
          (smConstrFields vlMeasures)

  -- A violation and a decode failure are different outcomes, so they get
  -- different exit codes: 1 means a script exceeds the bounds, 2 means the
  -- evidence is incomplete because some rows did not decode.
  if fsViolationCount > 0
    then exitWith (ExitFailure 1)
    else unless (fsDecodeFailureCount == 0) do
      putStrLn "Evidence is incomplete: some rows failed to decode."
      exitWith (ExitFailure 2)

printSampleHeader :: String -> Int64 -> Int -> IO ()
printSampleHeader label total shown
  | fromIntegral shown >= total = printf "\n%s (%d):\n" label total
  | otherwise = printf "\n%s (%d, showing the first %d):\n" label total shown
