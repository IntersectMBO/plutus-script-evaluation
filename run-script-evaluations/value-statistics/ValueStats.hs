{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module ValueStats (
  StatsAccumulator (..),
  ValueStats,
  Checkpoint (..),
  analyzeValue,
  emptyAccumulator,
  updateAccumulator,
  printReport,
  writeTextReport,
  saveCheckpoint,
  loadCheckpoint,
) where

import Control.Exception (catch, IOException)
import Control.Monad (forM_, when)
import Data.Aeson (
  FromJSON,
  FromJSONKey,
  ToJSON,
  ToJSONKey,
  eitherDecodeFileStrict,
 )
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int64)
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value qualified as V1
import PlutusTx.AssocMap qualified as AssocMap
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Data Types ------------------------------------------------------------------

-- | Statistics for a single Value
data ValueStats = MkValueStats
  { vsPolicyCount :: Int
  -- ^ Number of unique policies (currency symbols)
  , vsTokenCount :: Int
  -- ^ Total number of tokens (policy + token name pairs)
  , vsQuantities :: [Integer]
  -- ^ All token quantities in this Value
  }
  deriving stock (Show, Eq)

-- | Power-of-2 boundaries for quantity analysis
data QuantityBoundary
  = NegOverflow -- < -2^127
  | Neg127 -- [-2^127, -2^64)
  | Neg64 -- [-2^64, -2^32)
  | Neg32 -- [-2^32, -2^16)
  | Neg16 -- [-2^16, -2^8)
  | Neg8 -- [-2^8, 0)
  | Zero -- = 0
  | Pos8 -- (0, 2^8]
  | Pos16 -- (2^8, 2^16]
  | Pos32 -- (2^16, 2^32]
  | Pos64 -- (2^32, 2^64]
  | Pos127 -- (2^64, 2^127]
  | PosOverflow -- > 2^127
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | Accumulator for aggregate statistics
data StatsAccumulator = MkStatsAccumulator
  { saCount :: Int64
  -- ^ Total number of Values analyzed
  , saPolicyMin :: Int
  , saPolicyMax :: Int
  , saPolicySum :: Int64
  -- ^ Sum for computing mean
  , saPolicyDistribution :: Map Int Int64
  -- ^ Distribution: policy count -> frequency
  , saTokenMin :: Int
  , saTokenMax :: Int
  , saTokenSum :: Int64
  -- ^ Sum for computing mean
  , saTokenDistribution :: Map Int Int64
  -- ^ Distribution: token count -> frequency
  , saQuantityBoundaries :: Map QuantityBoundary Int64
  -- ^ Distribution of quantities by power-of-2 boundaries
  , saQuantityCount :: Int64
  -- ^ Total number of quantities analyzed
  , saQuantitiesNear2Pow64 :: Int64
  -- ^ Count of quantities >= 99% of 2^64
  , saQuantitiesNear2Pow128 :: Int64
  -- ^ Count of quantities >= 99% of 2^128
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Checkpoint wrapper with metadata for resuming analysis
data Checkpoint = MkCheckpoint
  { checkpointAccumulator :: StatsAccumulator
  , checkpointLastPk :: Int64
  , checkpointRowCount :: Int64
  -- ^ Number of rows (script evaluation events) processed
  , checkpointTimestamp :: String
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Initial empty accumulator
emptyAccumulator :: StatsAccumulator
emptyAccumulator =
  MkStatsAccumulator
    { saCount = 0
    , saPolicyMin = maxBound
    , saPolicyMax = 0
    , saPolicySum = 0
    , saPolicyDistribution = Map.empty
    , saTokenMin = maxBound
    , saTokenMax = 0
    , saTokenSum = 0
    , saTokenDistribution = Map.empty
    , saQuantityBoundaries = Map.empty
    , saQuantityCount = 0
    , saQuantitiesNear2Pow64 = 0
    , saQuantitiesNear2Pow128 = 0
    }

--------------------------------------------------------------------------------
-- Analysis Functions ----------------------------------------------------------

-- | Analyze a Plutus Value and extract statistics
analyzeValue :: V1.Value -> ValueStats
analyzeValue (V1.Value valueMap) =
  let policyCount = length $ AssocMap.toList valueMap
      -- Extract all token quantities from nested AssocMaps
      quantities =
        [ quantity
        | (_, tokenMap) <- AssocMap.toList valueMap
        , (_, quantity) <- AssocMap.toList tokenMap
        ]
      tokenCount = length quantities
   in MkValueStats
        { vsPolicyCount = policyCount
        , vsTokenCount = tokenCount
        , vsQuantities = quantities
        }

-- | Classify a quantity by its power-of-2 boundary range
classifyQuantity :: Integer -> QuantityBoundary
classifyQuantity q
  | q < -(2 ^ (127 :: Integer)) = NegOverflow
  | q < -(2 ^ (64 :: Integer)) = Neg127
  | q < -(2 ^ (32 :: Integer)) = Neg64
  | q < -(2 ^ (16 :: Integer)) = Neg32
  | q < -(2 ^ (8 :: Integer)) = Neg16
  | q < 0 = Neg8
  | q == 0 = Zero
  | q <= 2 ^ (8 :: Integer) = Pos8
  | q <= 2 ^ (16 :: Integer) = Pos16
  | q <= 2 ^ (32 :: Integer) = Pos32
  | q <= 2 ^ (64 :: Integer) = Pos64
  | q <= 2 ^ (127 :: Integer) = Pos127
  | otherwise = PosOverflow

-- | Check if quantity is >= 99% of 2^64
isNear2Pow64 :: Integer -> Bool
isNear2Pow64 q =
  let threshold = (99 * 2 ^ (64 :: Integer)) `div` 100
   in q >= threshold && q <= 2 ^ (64 :: Integer)

-- | Check if quantity is >= 99% of 2^128
isNear2Pow128 :: Integer -> Bool
isNear2Pow128 q =
  let threshold = (99 * 2 ^ (128 :: Integer)) `div` 100
   in q >= threshold

-- | Update accumulator with new statistics
updateAccumulator :: StatsAccumulator -> ValueStats -> StatsAccumulator
updateAccumulator acc MkValueStats{vsPolicyCount, vsTokenCount, vsQuantities} =
  let quantityBoundaryUpdates =
        foldr
          (\q m -> Map.insertWith (+) (classifyQuantity q) 1 m)
          (saQuantityBoundaries acc)
          vsQuantities
      near64Count = fromIntegral $ length $ filter isNear2Pow64 vsQuantities
      near128Count = fromIntegral $ length $ filter isNear2Pow128 vsQuantities
   in acc
        { saCount = saCount acc + 1
        , saPolicyMin = min (saPolicyMin acc) vsPolicyCount
        , saPolicyMax = max (saPolicyMax acc) vsPolicyCount
        , saPolicySum = saPolicySum acc + fromIntegral vsPolicyCount
        , saPolicyDistribution =
            Map.insertWith (+) vsPolicyCount 1 (saPolicyDistribution acc)
        , saTokenMin = min (saTokenMin acc) vsTokenCount
        , saTokenMax = max (saTokenMax acc) vsTokenCount
        , saTokenSum = saTokenSum acc + fromIntegral vsTokenCount
        , saTokenDistribution =
            Map.insertWith (+) vsTokenCount 1 (saTokenDistribution acc)
        , saQuantityBoundaries = quantityBoundaryUpdates
        , saQuantityCount = saQuantityCount acc + fromIntegral (length vsQuantities)
        , saQuantitiesNear2Pow64 = saQuantitiesNear2Pow64 acc + near64Count
        , saQuantitiesNear2Pow128 = saQuantitiesNear2Pow128 acc + near128Count
        }

-- | Compute percentiles from a distribution
computePercentiles :: Map Int Int64 -> [Int] -> [(Int, Int)]
computePercentiles distribution percentiles =
  let
    -- Create sorted list of (value, cumulative frequency)
    sortedEntries = sort $ Map.toList distribution
    totalCount = sum $ Map.elems distribution
    -- Compute cumulative frequencies
    cumulative =
      scanl1 (\(_, cum) (val, freq) -> (val, cum + freq)) sortedEntries
    -- Find value at each percentile
    findPercentile p =
      let threshold = (totalCount * fromIntegral p) `div` 100
          result = case dropWhile (\(_, cum) -> cum < threshold) cumulative of
            (val, _) : _ -> val
            [] -> case reverse cumulative of
              (val, _) : _ -> val
              [] -> 0
       in (p, result)
   in
    map findPercentile percentiles

-- | Create a text-based histogram for a distribution in range [0..500]
createHistogram :: Map Int Int64 -> [(Int, Int64, String)]
createHistogram distribution =
  let
    -- Create bins: 0, 1, 2, ..., 9, 10, 20, 30, ..., 490, 500+
    bins = [0..9] ++ [10, 20 .. 500]
    maxCount = 500

    -- Bin the distribution
    binCounts =
      Map.fromListWith
        (+)
        [ (binStart, count)
        | (value, count) <- Map.toList distribution
        , let binStart
                | value >= maxCount = maxCount
                | value <= 9 = value
                | otherwise = (value `div` 10) * 10
        ]

    -- Find max frequency for scaling
    maxFreq = maximum (0 : Map.elems binCounts)
    barWidth = 50 :: Int64 -- Max width of histogram bar

    -- Create histogram rows
    makeRow binStart =
      let
        freq = Map.findWithDefault 0 binStart binCounts
        barLength :: Int
        barLength =
          if maxFreq > 0
            then fromIntegral $ (freq * barWidth) `div` maxFreq
            else 0
        bar = replicate barLength '#'
        binLabel
          | binStart >= maxCount = show maxCount ++ "+"
          | binStart <= 9 = show binStart
          | otherwise = show binStart ++ "-" ++ show (binStart + 9)
       in
        (binStart, freq, printf "  %-8s: %10d %s" binLabel freq bar)
   in
    filter (\(_, freq, _) -> freq > 0) (map makeRow bins)

--------------------------------------------------------------------------------
-- Reporting -------------------------------------------------------------------

-- | Print a comprehensive statistics report
printReport :: StatsAccumulator -> IO ()
printReport MkStatsAccumulator{..} = do
  putStrLn ""
  putStrLn "====================================================================="
  putStrLn "                   Value Statistics Report"
  putStrLn "====================================================================="
  putStrLn ""

  printf "Total Values analyzed: %d\n\n" saCount

  when (saCount > 0) do
    let policyMean =
          if saCount > 0
            then fromIntegral saPolicySum / fromIntegral saCount :: Double
            else 0.0
        tokenMean =
          if saCount > 0
            then fromIntegral saTokenSum / fromIntegral saCount :: Double
            else 0.0

    putStrLn "---------------------------------------------------------------------"
    putStrLn "Policies per Value:"
    putStrLn "---------------------------------------------------------------------"
    printf "Min:  %d\n" saPolicyMin
    printf "Max:  %d\n" saPolicyMax
    printf "Mean: %.2f\n\n" policyMean

    let policyPercentiles = computePercentiles saPolicyDistribution [50, 90, 95, 99]
    putStrLn "Percentiles:"
    forM_ policyPercentiles \(p, val) -> do
      printf "  P%d:  %d\n" p val
    putStrLn ""

    putStrLn "Distribution (0-9: individual bins, 10-500: bins of 10):"
    let policyHistogram = createHistogram saPolicyDistribution
    forM_ policyHistogram \(_, _, bar) -> putStrLn bar
    putStrLn ""

    putStrLn "---------------------------------------------------------------------"
    putStrLn "Tokens per Value:"
    putStrLn "---------------------------------------------------------------------"
    printf "Min:  %d\n" saTokenMin
    printf "Max:  %d\n" saTokenMax
    printf "Mean: %.2f\n\n" tokenMean

    let tokenPercentiles = computePercentiles saTokenDistribution [50, 90, 95, 99]
    putStrLn "Percentiles:"
    forM_ tokenPercentiles \(p, val) -> do
      printf "  P%d:  %d\n" p val
    putStrLn ""

    putStrLn "Distribution (0-9: individual bins, 10-500: bins of 10):"
    let tokenHistogram = createHistogram saTokenDistribution
    forM_ tokenHistogram \(_, _, bar) -> putStrLn bar
    putStrLn ""

    putStrLn "---------------------------------------------------------------------"
    putStrLn "Token Quantities by Power-of-2 Boundaries:"
    putStrLn "---------------------------------------------------------------------"
    printf "Total quantities analyzed: %d\n\n" saQuantityCount

    let allBoundaries = [minBound .. maxBound] :: [QuantityBoundary]
        negativeBoundaries = [NegOverflow, Neg127, Neg64, Neg32, Neg16, Neg8]
        negativeCount = sum [Map.findWithDefault 0 b saQuantityBoundaries | b <- negativeBoundaries]

        boundaryLabels :: Map QuantityBoundary String =
          Map.fromList
            [ (NegOverflow, "  < -2^127         ")
            , (Neg127, "  [-2^127, -2^64)  ")
            , (Neg64, "  [-2^64, -2^32)   ")
            , (Neg32, "  [-2^32, -2^16)   ")
            , (Neg16, "  [-2^16, -2^8)    ")
            , (Neg8, "  [-2^8, 0)        ")
            , (Zero, "  = 0              ")
            , (Pos8, "  (0, 2^8]         ")
            , (Pos16, "  (2^8, 2^16]      ")
            , (Pos32, "  (2^16, 2^32]     ")
            , (Pos64, "  (2^32, 2^64]     ")
            , (Pos127, "  (2^64, 2^127]    ")
            , (PosOverflow, "  > 2^127          ")
            ]

    when (negativeCount == 0) do
      putStrLn "  No negative quantities found.\n"

    forM_ allBoundaries \boundary -> do
      let count = Map.findWithDefault 0 boundary saQuantityBoundaries
          label = Map.findWithDefault "???" boundary boundaryLabels
          percentage =
            if saQuantityCount > 0
              then (100.0 :: Double) * fromIntegral count / fromIntegral saQuantityCount
              else 0.0
      when (count > 0) do
        printf "%s: %10d (%6.2f%%)\n" label count percentage

    putStrLn ""
    let near64Pct =
          if saQuantityCount > 0
            then (100.0 :: Double) * fromIntegral saQuantitiesNear2Pow64 / fromIntegral saQuantityCount
            else 0.0
        near128Pct =
          if saQuantityCount > 0
            then (100.0 :: Double) * fromIntegral saQuantitiesNear2Pow128 / fromIntegral saQuantityCount
            else 0.0
    printf "  Quantities >= 99%% of 2^64:  %10d (%6.2f%%)\n" saQuantitiesNear2Pow64 near64Pct
    printf "  Quantities >= 99%% of 2^128: %10d (%6.2f%%)\n" saQuantitiesNear2Pow128 near128Pct

  putStrLn ""
  putStrLn "====================================================================="
  putStrLn ""

--------------------------------------------------------------------------------
-- File Output -----------------------------------------------------------------

-- | Write statistics report to a plain text file
writeTextReport :: FilePath -> StatsAccumulator -> IO ()
writeTextReport filePath acc = do
  let content = formatTextReport acc
  writeFile filePath content
  printf "Text report written to: %s\n" filePath

-- | Format statistics as plain text
formatTextReport :: StatsAccumulator -> String
formatTextReport MkStatsAccumulator{..} =
  let
    policyMean =
      if saCount > 0
        then fromIntegral saPolicySum / fromIntegral saCount :: Double
        else 0.0
    tokenMean =
      if saCount > 0
        then fromIntegral saTokenSum / fromIntegral saCount :: Double
        else 0.0
    policyPercentiles = computePercentiles saPolicyDistribution [50, 90, 95, 99]
    tokenPercentiles = computePercentiles saTokenDistribution [50, 90, 95, 99]
    policyHistogram = createHistogram saPolicyDistribution
    tokenHistogram = createHistogram saTokenDistribution

    allBoundaries = [minBound .. maxBound] :: [QuantityBoundary]
    negativeBoundaries = [NegOverflow, Neg127, Neg64, Neg32, Neg16, Neg8]
    negativeCount = sum [Map.findWithDefault 0 b saQuantityBoundaries | b <- negativeBoundaries]

    boundaryLabels =
      Map.fromList
        [ (NegOverflow, "  < -2^127         " :: String)
        , (Neg127, "  [-2^127, -2^64)  ")
        , (Neg64, "  [-2^64, -2^32)   ")
        , (Neg32, "  [-2^32, -2^16)   ")
        , (Neg16, "  [-2^16, -2^8)    ")
        , (Neg8, "  [-2^8, 0)        ")
        , (Zero, "  = 0              ")
        , (Pos8, "  (0, 2^8]         ")
        , (Pos16, "  (2^8, 2^16]      ")
        , (Pos32, "  (2^16, 2^32]     ")
        , (Pos64, "  (2^32, 2^64]     ")
        , (Pos127, "  (2^64, 2^127]    ")
        , (PosOverflow, "  > 2^127          ")
        ]

    formatBoundary boundary =
      let count = Map.findWithDefault 0 boundary saQuantityBoundaries
          label = Map.findWithDefault "???" boundary boundaryLabels
          percentage =
            if saQuantityCount > 0
              then (100.0 :: Double) * fromIntegral count / fromIntegral saQuantityCount
              else 0.0
       in if count > 0
            then Just $ printf "%s: %10d (%6.2f%%)" label count percentage
            else Nothing

    near64Pct =
      if saQuantityCount > 0
        then (100.0 :: Double) * fromIntegral saQuantitiesNear2Pow64 / fromIntegral saQuantityCount
        else 0.0
    near128Pct =
      if saQuantityCount > 0
        then (100.0 :: Double) * fromIntegral saQuantitiesNear2Pow128 / fromIntegral saQuantityCount
        else 0.0

   in unlines $
        [ "====================================================================="
        , "                   Value Statistics Report"
        , "====================================================================="
        , ""
        , printf "Total Values analyzed: %d" saCount
        , ""
        , "---------------------------------------------------------------------"
        , "Policies per Value:"
        , "---------------------------------------------------------------------"
        , ""
        , printf "Min:  %d" saPolicyMin
        , printf "Max:  %d" saPolicyMax
        , printf "Mean: %.2f" policyMean
        , ""
        , "Percentiles:"
        ]
        ++ [printf "  P%d:  %d" p val | (p, val) <- policyPercentiles]
        ++ ["", "Distribution (0-9: individual bins, 10-500: bins of 10):"]
        ++ [bar | (_, _, bar) <- policyHistogram]
        ++ [ ""
           , "---------------------------------------------------------------------"
           , "Tokens per Value:"
           , "---------------------------------------------------------------------"
           , ""
           , printf "Min:  %d" saTokenMin
           , printf "Max:  %d" saTokenMax
           , printf "Mean: %.2f" tokenMean
           , ""
           , "Percentiles:"
           ]
        ++ [printf "  P%d:  %d" p val | (p, val) <- tokenPercentiles]
        ++ ["", "Distribution (0-9: individual bins, 10-500: bins of 10):"]
        ++ [bar | (_, _, bar) <- tokenHistogram]
        ++ [ ""
           , "---------------------------------------------------------------------"
           , "Token Quantities by Power-of-2 Boundaries:"
           , "---------------------------------------------------------------------"
           , ""
           , printf "Total quantities analyzed: %d" saQuantityCount
           , ""
           ]
        ++ (if negativeCount == 0 then ["No negative quantities found.", ""] else [])
        ++ mapMaybe formatBoundary allBoundaries
        ++ [ ""
           , printf "Quantities >= 99%% of 2^64:  %10d (%6.2f%%)" saQuantitiesNear2Pow64 near64Pct
           , printf "Quantities >= 99%% of 2^128: %10d (%6.2f%%)" saQuantitiesNear2Pow128 near128Pct
           , ""
           , "====================================================================="
           , ""
           ]

--------------------------------------------------------------------------------
-- Checkpoint Persistence ------------------------------------------------------

-- | Save checkpoint to a JSON file with timestamp
saveCheckpoint :: FilePath -> StatsAccumulator -> Int64 -> Int64 -> IO ()
saveCheckpoint filePath acc lastPk rowCount = do
  now <- getCurrentTime
  let timestamp = iso8601Show now
      checkpoint =
        MkCheckpoint
          { checkpointAccumulator = acc
          , checkpointLastPk = lastPk
          , checkpointRowCount = rowCount
          , checkpointTimestamp = timestamp
          }
  BSL.writeFile filePath (encodePretty checkpoint)
  printf "Checkpoint saved at pk %d to: %s\n" lastPk filePath

-- | Load checkpoint from a JSON file
loadCheckpoint :: FilePath -> IO (Maybe (StatsAccumulator, Int64, Int64))
loadCheckpoint filePath = do
  result <- (eitherDecodeFileStrict filePath :: IO (Either String Checkpoint))
    `catch` \(_ :: IOException) -> pure (Left "File not found")
  case result of
    Left err -> do
      putStrLn $ "Could not load checkpoint: " <> err
      pure Nothing
    Right MkCheckpoint{checkpointAccumulator, checkpointLastPk, checkpointRowCount, checkpointTimestamp} -> do
      printf
        "Loaded checkpoint from %s (saved at %s, last pk: %d, rows: %d)\n"
        filePath
        checkpointTimestamp
        checkpointLastPk
        checkpointRowCount
      pure $ Just (checkpointAccumulator, checkpointLastPk, checkpointRowCount)
