{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Database where

import Cardano.Slotting.Slot (SlotNo)
import Data.ByteString (ByteString)
import Data.Digest.Murmur64 (Hash64)
import Data.Int (Int16, Int64)
import Data.Profunctor.Product.Default (Default)
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Database.Orphans ()
import Database.PostgreSQL.Simple (Connection)
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
  Table,
  ToFields,
  Unpackspec,
  doNothing,
  rCount,
  runDelete,
  runInsert,
  selectTable,
  showSql,
  table,
  toFields,
  (.>=),
 )
import Ouroboros.Consensus.Block (BlockNo)
import PlutusLedgerApi.Common (PlutusLedgerLanguage (..))

type DbTable f = Table f f

--------------------------------------------------------------------------------
-- cost_model_params -----------------------------------------------------------

data
  CostModelValuesRecord'
    hash64
    ledgerLang
    protoVer
    paramValues = MkCostModelValues
  { cmPk :: hash64
  , cmLedgerLanguage :: ledgerLang
  , cmMajorProtocolVersion :: protoVer
  , cmParamValues :: paramValues
  }
  deriving (Show, Eq)

type CostModelValuesRecord =
  CostModelValuesRecord'
    Hash64 -- pk
    PlutusLedgerLanguage -- ledger_language
    Int16 -- major_protocol_version
    [Int64] -- param_values

type CostModelValuesRecordFields =
  CostModelValuesRecord'
    (Field SqlInt8) -- pk
    (Field SqlInt2) -- ledger_language
    (Field SqlInt2) -- major_protocol_version
    (Field (SqlArray SqlInt8)) -- param_values

$(makeAdaptorAndInstanceInferrable "pCostModelValues" ''CostModelValuesRecord')

costModelValues :: DbTable CostModelValuesRecordFields
costModelValues =
  table "cost_model_params" $
    pCostModelValues
      MkCostModelValues
        { cmPk = tableField "pk"
        , cmLedgerLanguage = tableField "ledger_language"
        , cmMajorProtocolVersion = tableField "major_protocol_ver"
        , cmParamValues = tableField "param_values"
        }

selectCostModelValues :: Select CostModelValuesRecordFields
selectCostModelValues = selectTable costModelValues

insertCostModelValues
  :: (Default ToFields CostModelValuesRecord CostModelValuesRecordFields)
  => Connection
  -> [CostModelValuesRecord]
  -> IO Int64
insertCostModelValues conn hs =
  runInsert
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
    protoVer
    serialised = MkSerialisedScriptRecord
  { ssHash :: hash64
  , ssLedgerLanguage :: ledgerLang
  , ssMajorProtocolVersion :: protoVer
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
  -> IO Int64
insertSerialisedScripts conn records = do
  let insert =
        Insert
          { iTable = serialisedScripts
          , iRows = map toFields records
          , iReturning = rCount
          , iOnConflict = Just doNothing
          }
  runInsert conn insert

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
  -> IO Int64
insertScriptEvaluationEvents conn events = do
  let insert =
        Insert
          { iTable = scriptEvaluationEvents
          , iRows = map toFields events
          , iReturning = rCount
          , iOnConflict = Nothing
          }
  runInsert conn insert

deleteFromSlotOnwards :: Connection -> SlotNo -> IO Int64
deleteFromSlotOnwards conn slotNo = do
  let delete =
        Delete
          { dTable = scriptEvaluationEvents
          , dWhere = \r -> eeSlotNo r .>= toFields slotNo
          , dReturning = rCount
          }
  runDelete conn delete

--------------------------------------------------------------------------------
-- Utility ---------------------------------------------------------------------

printSql :: (Default Unpackspec a a) => Select a -> IO ()
printSql = putStrLn . maybe "Empty select" id . showSql
