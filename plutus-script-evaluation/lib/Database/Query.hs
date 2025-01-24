module Database.Query where

import Cardano.Slotting.Slot (SlotNo)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.Function ((&))
import Data.Profunctor.Product.Default (Default)
import Database.Orphans ()
import Database.PostgreSQL.Simple (Connection)
import Database.Schema
import Numeric.Natural (Natural)
import Opaleye (
  Delete (..),
  Insert (Insert, iOnConflict, iReturning, iRows, iTable),
  ToFields,
  asc,
  doNothing,
  limit,
  maybeFields,
  optional,
  orderBy,
  rCount,
  runDelete,
  runInsert,
  runSelect,
  runSelectFold,
  selectTable,
  toFields,
  where_,
  (.==),
  (.>=),
 )

insertCostModelValues
  :: (Default ToFields CostModelValuesRecord CostModelValuesRecordFields)
  => Connection
  -> [CostModelValuesRecord]
  -> IO Int
insertCostModelValues conn hs =
  fromIntegral -- Convert from Int64 to Int as we don't expect many rows
    <$> runInsert
      conn
      Insert
        { iTable = costModelValues
        , iRows = map toFields hs
        , iReturning = rCount
        , iOnConflict = Just doNothing
        }

selectCostModelValues :: Connection -> IO [CostModelValuesRecord]
selectCostModelValues conn = runSelect conn (selectTable costModelValues)

insertSerialisedScripts
  :: (Default ToFields SerialisedScriptRecord SerialisedScriptRecordFields)
  => Connection
  -> [SerialisedScriptRecord]
  -> IO Int
insertSerialisedScripts conn records =
  fromIntegral -- Convert from Int64 to Int as we don't expect many rows
    <$> runInsert
      conn
      Insert
        { iTable = serialisedScripts
        , iRows = map toFields records
        , iReturning = rCount
        , iOnConflict = Just doNothing
        }

selectSerialisedScripts :: Connection -> IO [SerialisedScriptRecord]
selectSerialisedScripts conn = runSelect conn (selectTable serialisedScripts)

insertDeserialisedScripts
  :: Connection -> [DeserialisedScriptRecord] -> IO Int
insertDeserialisedScripts conn records =
  fromIntegral --  Convert from Int64 to Int as we don't expect many rows
    <$> runInsert
      conn
      Insert
        { iTable = deserialisedScripts
        , iRows = map toFields records
        , iReturning = rCount
        , iOnConflict = Just doNothing
        }

selectSerialisedScriptsBatch
  :: Connection -> Natural -> IO [SerialisedScriptRecord]
selectSerialisedScriptsBatch conn count =
  runSelect conn $ limit (fromIntegral count) do
    serialised@(MkSerialisedScriptRecord hash _ledgerLang _script) <-
      selectTable serialisedScripts
    maybeDeserialised <- optional do
      row@(MkDeserialisedScriptRecord dsHash _) <-
        selectTable deserialisedScripts
      row <$ where_ (hash .== dsHash)
    where_ do
      maybeFields (toFields True) (const (toFields False)) maybeDeserialised
    pure serialised

withScriptEvaluationEvents
  :: (MonadUnliftIO m)
  => Connection
  -> a
  -> (a -> ScriptEvaluationRecord -> m a)
  -> m a
withScriptEvaluationEvents conn a f = do
  let select =
        selectTable scriptEvaluations
          & orderBy (asc seBlockNo)
  withRunInIO \runInIO ->
    runSelectFold conn select a \accum record ->
      runInIO (f accum record)

insertScriptEvaluationEvents
  :: (Default ToFields EvaluationEventRecord WriteEvaluationEventRecordFields)
  => Connection
  -> [EvaluationEventRecord]
  -> IO Int
insertScriptEvaluationEvents conn events =
  fromIntegral -- Convert from Int64 to Int as we don't expect many rows
    <$> runInsert
      conn
      Insert
        { iTable = scriptEvaluationEvents
        , iRows = map toFields events
        , iReturning = rCount
        , iOnConflict = Nothing
        }

deleteFromSlotOnwards :: Connection -> SlotNo -> IO Int
deleteFromSlotOnwards conn slotNo =
  fromIntegral -- Convert from Int64 to Int as we don't expect many rows
    <$> runDelete
      conn
      Delete
        { dTable = scriptEvaluationEvents
        , dWhere = \r -> eeSlotNo r .>= toFields slotNo
        , dReturning = rCount
        }
