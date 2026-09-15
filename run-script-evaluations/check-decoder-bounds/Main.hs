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
import Main.Utf8 (withUtf8)
import Options (Options (..), parserInfo)
import Options.Applicative (execParser)
import PlutusCore (ValueOf (..))
import PlutusCore.DeBruijn.Internal (FakeNamedDeBruijn)
import PlutusCore.Default (defaultUniSize)
import PlutusLedgerApi.Common (PlutusLedgerLanguage, vanRossemPV)
import PlutusLedgerApi.Common.Versions (MaxBounds (..), maxBoundsByPV)
import System.Exit (exitFailure)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import Text.Printf (printf)
import UntypedPlutusCore qualified as U

-- | The bounds we are testing against, taken from plutus rather than copied.
strictBounds :: MaxBounds
strictBounds = maxBoundsByPV vanRossemPV

maxHeaderBound, maxConstrBound :: Int
maxHeaderBound = mbHeader strictBounds
maxConstrBound = mbConstr strictBounds

data ScriptMeasures = MkScriptMeasures
  { smHeaderSize :: !Int
  , smConstrFields :: !Int
  }
  deriving stock (Show)

data Extreme = MkExtreme
  { exValue :: !Int
  , exHash :: !Text
  }

data LangStats = MkLangStats
  { lsCount :: !Int64
  , lsMaxHeader :: !Extreme
  , lsMaxConstr :: !Extreme
  }

data FoldState = MkFoldState
  { fsRowCount :: !Int64
  , fsPerLanguage :: !(Map PlutusLedgerLanguage LangStats)
  , fsViolations :: ![(Text, PlutusLedgerLanguage, ScriptMeasures)]
  , fsDecodeFailures :: ![(Text, String)]
  }

initialState :: FoldState
initialState = MkFoldState 0 Map.empty [] []

main :: IO ()
main = withUtf8 do
  hSetBuffering stdout LineBuffering
  Options{optsDatabaseConnStr} <- execParser parserInfo
  displaySqlError $
    bracket
      (PG.connectPostgreSQL optsDatabaseConnStr)
      (\conn -> PG.close conn `catch` \(_ :: PG.SqlError) -> pure ())
      \conn -> do
        [Only (totalCount :: Int64)] <-
          PG.query_ conn "SELECT COUNT(*) FROM serialised_scripts"
        printf "Checking %d distinct scripts...\n" totalCount
        finalState <-
          PG.fold_
            conn
            "SELECT encode(hash, 'hex'), ledger_language, serialised \
            \FROM serialised_scripts"
            initialState
            (processRow totalCount)
        printReport finalState

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
  when (newRowCount `mod` 10_000 == 0) do
    let percent = (100.0 :: Double) * fromIntegral newRowCount / fromIntegral (max 1 totalCount)
    printf "Processed %d / %d scripts (%.2f%%)\n" newRowCount totalCount percent
  case decodeScriptTerm serialised of
    Left err ->
      pure
        st
          { fsRowCount = newRowCount
          , fsDecodeFailures = (hashHex, err) : fsDecodeFailures
          }
    Right term -> do
      let !measures = measureTerm term
          !newPerLanguage =
            Map.insertWith
              (<>)
              lang
              (langStats hashHex measures)
              fsPerLanguage
          !newViolations =
            if smHeaderSize measures > maxHeaderBound
              || smConstrFields measures > maxConstrBound
              then (hashHex, lang, measures) : fsViolations
              else fsViolations
      pure
        st
          { fsRowCount = newRowCount
          , fsPerLanguage = newPerLanguage
          , fsViolations = newViolations
          }

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

  unless (null fsDecodeFailures) do
    printf "\nDECODE FAILURES (%d):\n" (length fsDecodeFailures)
    forM_ fsDecodeFailures \(hashHex, err) ->
      printf "  %s: %s\n" (Text.unpack hashHex) err

  if null fsViolations
    then putStrLn "\nNo script exceeds the vanRossemPV bounds."
    else do
      printf "\nVIOLATIONS (%d):\n" (length fsViolations)
      forM_ fsViolations \(hashHex, lang, MkScriptMeasures{..}) ->
        printf
          "  %s (%s): header size %d, constr fields %d\n"
          (Text.unpack hashHex)
          (show lang)
          smHeaderSize
          smConstrFields

  unless (null fsViolations && null fsDecodeFailures) exitFailure
