module Aggregate where

import Cardano.Api (SlotNo)
import Data.Foldable (foldlM)
import Data.List (foldl')
import FileStorage (Order (..))
import FileStorage qualified
import Numeric.Natural (Natural)
import Path (Abs, Dir, File, Path, SomeBase)
import Path.IO (makeAbsolute)
import PlutusLedgerApi.Common (PlutusLedgerLanguage (..))
import PlutusLedgerApi.Test.EvaluationEvent (
  ScriptEvaluationEvent (..),
  ScriptEvaluationEvents (..),
  ScriptEvaluationResult (..),
 )

aggregateScriptEvents :: SomeBase Dir -> IO Metrics
aggregateScriptEvents optsEventsDir = do
  eventsDir <- makeAbsolute optsEventsDir
  FileStorage.listFilesSorted Asc eventsDir
    >>= foldlM aggregateFile mempty

aggregateFile :: Metrics -> (SlotNo, Path Abs File) -> IO Metrics
aggregateFile !metrics (_slotNo, path) = do
  events <- FileStorage.readEventsFile path
  let metrics' = aggregateEvents metrics events
  putStrLn $ "Aggregated events from " <> show path <> ": " <> show metrics'
  pure metrics'

data Metrics = MkMetrics
  { countV1Evaluations :: !Natural
  , countV2Evaluations :: !Natural
  , countV3Evaluations :: !Natural
  , countSuccesses :: !Natural
  , countFailures :: !Natural
  }
  deriving stock (Show)

instance Semigroup Metrics where
  MkMetrics a1 b1 c1 d1 e1 <> MkMetrics a2 b2 c2 d2 e2 =
    MkMetrics (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2) (e1 + e2)

instance Monoid Metrics where
  mempty =
    MkMetrics
      { countV1Evaluations = 0
      , countV2Evaluations = 0
      , countV3Evaluations = 0
      , countSuccesses = 0
      , countFailures = 0
      }

countEvaluation :: PlutusLedgerLanguage -> Metrics -> Metrics
countEvaluation = \case
  PlutusV1 ->
    \metrics -> metrics{countV1Evaluations = countV1Evaluations metrics + 1}
  PlutusV2 ->
    \metrics -> metrics{countV2Evaluations = countV2Evaluations metrics + 1}
  PlutusV3 ->
    \metrics -> metrics{countV3Evaluations = countV3Evaluations metrics + 1}

countResult :: ScriptEvaluationResult -> Metrics -> Metrics
countResult = \case
  ScriptEvaluationSuccess ->
    \metrics -> metrics{countSuccesses = countSuccesses metrics + 1}
  ScriptEvaluationFailure ->
    \metrics -> metrics{countFailures = countFailures metrics + 1}

{-

data ScriptEvaluationEvents = ScriptEvaluationEvents
  { eventsCostParamsV1 :: Maybe [Int64]
  , eventsCostParamsV2 :: Maybe [Int64]
  , eventsEvents       :: NonEmpty ScriptEvaluationEvent
  }

data ScriptEvaluationEvent
  = PlutusEvent PlutusLedgerLanguage ScriptEvaluationData ScriptEvaluationResult

data ScriptEvaluationData = ScriptEvaluationData
  { dataProtocolVersion :: MajorProtocolVersion
  , dataBudget          :: ExBudget
  , dataScript          :: SerialisedScript
  , dataInputs          :: [PLC.Data]
  }

-}
aggregateEvents :: Metrics -> ScriptEvaluationEvents -> Metrics
aggregateEvents !metrics ScriptEvaluationEvents{eventsEvents} =
  foldl' aggregateEvent metrics eventsEvents

aggregateEvent :: Metrics -> ScriptEvaluationEvent -> Metrics
aggregateEvent !metrics (PlutusEvent ledgerLanguage _evalData evalResult) =
  (countEvaluation ledgerLanguage . countResult evalResult) metrics
