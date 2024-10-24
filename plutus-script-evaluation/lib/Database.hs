{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Database where

import Cardano.Slotting.Slot (SlotNo)
import Data.Aeson qualified as Json
import Data.ByteString (ByteString)
import Data.Digest.Murmur64 (Hash64)
import Data.Int (Int16, Int64)
import Data.Profunctor.Product.Default (Default)
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Database.Orphans ()
import Database.PostgreSQL.Simple (Connection)
import Numeric.Natural (Natural)
import Opaleye (
  Delete (..),
  Field,
  FieldNullable,
  InferrableTableField (tableField),
  Insert (Insert, iOnConflict, iReturning, iRows, iTable),
  Select,
  SqlArray,
  SqlBool,
  SqlBytea,
  SqlInt2,
  SqlInt8,
  SqlJsonb,
  Table,
  ToFields,
  Unpackspec,
  doNothing,
  limit,
  maybeFields,
  optional,
  rCount,
  runDelete,
  runInsert,
  runSelect,
  selectTable,
  showSql,
  table,
  toFields,
  where_,
  (.==),
  (.>=),
 )
import Ouroboros.Consensus.Block (BlockNo)
import PlutusLedgerApi.Common (PlutusLedgerLanguage (..))

type DbTable f = Table f f

--------------------------------------------------------------------------------
-- cost_model_params -----------------------------------------------------------

data CostModelValuesRecord' hash64 paramValues = MkCostModelValues
  { cmPk :: hash64
  , cmParamValues :: paramValues
  }
  deriving (Show, Eq)

type CostModelValuesRecord =
  CostModelValuesRecord'
    Hash64 -- pk
    [Int64] -- param_values

type CostModelValuesRecordFields =
  CostModelValuesRecord'
    (Field SqlInt8) -- pk
    (Field (SqlArray SqlInt8)) -- param_values

$(makeAdaptorAndInstanceInferrable "pCostModelValues" ''CostModelValuesRecord')

costModelValues :: DbTable CostModelValuesRecordFields
costModelValues =
  table "cost_model_params" $
    pCostModelValues
      MkCostModelValues
        { cmPk = tableField "pk"
        , cmParamValues = tableField "param_values"
        }

selectCostModelValues :: Select CostModelValuesRecordFields
selectCostModelValues = selectTable costModelValues

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

--------------------------------------------------------------------------------
-- serialised_scripts ----------------------------------------------------------

data
  SerialisedScriptRecord'
    hash64
    ledgerLang
    majorProtoVer
    serialised = MkSerialisedScriptRecord
  { ssHash :: hash64
  , ssLedgerLanguage :: ledgerLang
  , ssMajorProtocolVersion :: majorProtoVer
  , ssSerialised :: serialised
  }
  deriving (Show, Eq)

type SerialisedScriptRecord =
  SerialisedScriptRecord'
    ByteString -- hash
    PlutusLedgerLanguage -- ledger_language
    Int16 -- major_protocol_version
    ByteString -- serialised

type SerialisedScriptRecordFields =
  SerialisedScriptRecord'
    (Field SqlBytea) -- hash
    (Field SqlInt2) -- ledger_language
    (Field SqlInt2) -- major_protocol_version
    (Field SqlBytea) -- serialised

$( makeAdaptorAndInstanceInferrable
    "pSerialisedScript"
    ''SerialisedScriptRecord'
 )

serialisedScripts :: DbTable SerialisedScriptRecordFields
serialisedScripts =
  table "serialised_scripts" $
    pSerialisedScript
      MkSerialisedScriptRecord
        { ssHash = tableField "hash"
        , ssLedgerLanguage = tableField "ledger_language"
        , ssMajorProtocolVersion = tableField "major_protocol_ver"
        , ssSerialised = tableField "serialised"
        }

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

--------------------------------------------------------------------------------
-- Deserialised scripts --------------------------------------------------------

data DeserialisedScriptRecord' hash64 deserialised = MkDeserialisedScriptRecord
  { dsHash :: hash64
  , dsDeserialised :: deserialised
  }
  deriving (Show, Eq)

type DeserialisedScriptRecord =
  DeserialisedScriptRecord'
    ByteString -- hash
    Json.Value -- deserialised

type DeserialisedScriptRecordFields =
  DeserialisedScriptRecord'
    (Field SqlBytea) -- hash
    (Field SqlJsonb) -- deserialised

$( makeAdaptorAndInstanceInferrable
    "pDeserialisedScript"
    ''DeserialisedScriptRecord'
 )

deserialisedScripts :: DbTable DeserialisedScriptRecordFields
deserialisedScripts =
  table "deserialised_scripts" $
    pDeserialisedScript
      MkDeserialisedScriptRecord
        { dsHash = tableField "hash"
        , dsDeserialised = tableField "deserialised"
        }

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

selectSerialisedScriptsToDeserialise
  :: Connection -> Natural -> IO [SerialisedScriptRecord]
selectSerialisedScriptsToDeserialise conn count =
  runSelect conn $ limit (fromIntegral count) do
    serialised@(MkSerialisedScriptRecord hash _ _ _) <-
      selectTable serialisedScripts
    maybeDeserialised <- optional do
      row@(MkDeserialisedScriptRecord dsHash _) <-
        selectTable deserialisedScripts
      row <$ where_ (hash .== dsHash)
    where_ do
      maybeFields (toFields True) (const (toFields False)) maybeDeserialised
    pure serialised

--------------------------------------------------------------------------------
-- script_evaluation_events ----------------------------------------------------

data
  EvaluationEventRecord'
    slotNo
    blockNo
    evaluatedSuccessfully
    budgetCpu
    budgetMem
    scriptHash
    datum
    redeemer
    scriptContext
    costModel = MkEvaluationEvent
  { eeSlotNo :: slotNo
  , eeBlockNo :: blockNo
  , eeEvaluatedSuccessfully :: evaluatedSuccessfully
  , eeExecBudgetCpu :: budgetCpu
  , eeExecBudgetMem :: budgetMem
  , eeScriptHash :: scriptHash
  , eeDatum :: datum
  , eeRedeemer :: redeemer
  , eeScriptContext :: scriptContext
  , eeCostModelParams :: costModel
  }
  deriving (Show, Eq)

type EvaluationEventRecord =
  EvaluationEventRecord'
    SlotNo -- slot
    BlockNo -- block
    Bool -- evaluated_successfully
    Int64 -- exec_budget_cpu
    Int64 -- exec_budget_mem
    ByteString -- script_hash
    (Maybe ByteString) -- datum
    (Maybe ByteString) -- redeemer
    ByteString -- script_context
    (Maybe Hash64) -- cost_model_params

type EvaluationEventRecordFields =
  EvaluationEventRecord'
    (Field SqlInt8) -- block
    (Field SqlInt8) -- slot
    (Field SqlBool) -- evaluated_successfully
    (Field SqlInt8) -- exec_budget_cpu
    (Field SqlInt8) -- exec_budget_mem
    (Field SqlBytea) -- script_hash
    (FieldNullable SqlBytea) -- datum
    (FieldNullable SqlBytea) -- redeemer
    (Field SqlBytea) -- script_context
    (FieldNullable SqlInt8) -- cost_model_params

$(makeAdaptorAndInstanceInferrable "pEvaluationEvent" ''EvaluationEventRecord')

scriptEvaluationEvents :: DbTable EvaluationEventRecordFields
scriptEvaluationEvents =
  table "script_evaluation_events" $
    pEvaluationEvent
      MkEvaluationEvent
        { eeSlotNo = tableField "slot"
        , eeBlockNo = tableField "block"
        , eeEvaluatedSuccessfully = tableField "evaluated_successfully"
        , eeExecBudgetCpu = tableField "exec_budget_cpu"
        , eeExecBudgetMem = tableField "exec_budget_mem"
        , eeScriptHash = tableField "script_hash"
        , eeDatum = tableField "datum"
        , eeRedeemer = tableField "redeemer"
        , eeScriptContext = tableField "script_context"
        , eeCostModelParams = tableField "cost_model_params"
        }

selectScriptEvaluationEvents :: Select EvaluationEventRecordFields
selectScriptEvaluationEvents = selectTable scriptEvaluationEvents

insertScriptEvaluationEvents
  :: (Default ToFields EvaluationEventRecord EvaluationEventRecordFields)
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

--------------------------------------------------------------------------------
-- Utility ---------------------------------------------------------------------

printSql :: (Default Unpackspec a a) => Select a -> IO ()
printSql = putStrLn . maybe "Empty select" id . showSql
