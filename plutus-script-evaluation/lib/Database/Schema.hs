{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Database.Schema where

import Cardano.Slotting.Slot (SlotNo)
import Data.Aeson qualified as Json
import Data.ByteString (ByteString)
import Data.Digest.Murmur64 (Hash64)
import Data.Int (Int64)
import Data.Profunctor.Product.Default (Default)
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Database.Orphans ()
import Opaleye (
  Field,
  FieldNullable,
  InferrableTableField (tableField),
  Select,
  SqlArray,
  SqlBool,
  SqlBytea,
  SqlInt2,
  SqlInt4,
  SqlInt8,
  SqlJsonb,
  Table,
  Unpackspec,
  optionalTableField,
  showSql,
  table,
 )
import Ouroboros.Consensus.Block (BlockNo)
import PlutusLedgerApi.Common (
  ExCPU,
  ExMemory,
  MajorProtocolVersion,
  PlutusLedgerLanguage (..),
 )

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

--------------------------------------------------------------------------------
-- serialised_scripts ----------------------------------------------------------

data SerialisedScriptRecord' hash64 ledgerLang serialised
  = MkSerialisedScriptRecord
  { ssHash :: hash64
  , ssLedgerLanguage :: ledgerLang
  , ssSerialised :: serialised
  }
  deriving (Show, Eq)

type SerialisedScriptRecord =
  SerialisedScriptRecord'
    ByteString -- hash
    PlutusLedgerLanguage -- ledger_language
    ByteString -- serialised

type SerialisedScriptRecordFields =
  SerialisedScriptRecord'
    (Field SqlBytea) -- hash
    (Field SqlInt2) -- ledger_language
    (Field SqlBytea) -- serialised

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

--------------------------------------------------------------------------------
-- script_evaluation_events ----------------------------------------------------

data
  EvaluationEventRecord'
    pk
    slotNo
    blockNo
    majorProtocolVersion
    evaluatedSuccessfully
    budgetCpu
    budgetMem
    scriptHash
    datum
    redeemer
    scriptContext
    costModel
  = MkEvaluationEventRecord'
  { eePk :: pk
  , eeSlotNo :: slotNo
  , eeBlockNo :: blockNo
  , eeMajorProtocolVersion :: majorProtocolVersion
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
    (Maybe Int64) -- pk
    SlotNo -- slot
    BlockNo -- block
    MajorProtocolVersion -- major_protocol_version
    Bool -- evaluated_successfully
    ExCPU -- exec_budget_cpu
    ExMemory -- exec_budget_mem
    ByteString -- script_hash
    (Maybe ByteString) -- datum
    (Maybe ByteString) -- redeemer
    ByteString -- script_context
    (Maybe Hash64) -- cost_model_params

type WriteEvaluationEventRecordFields =
  EvaluationEventRecord'
    (Maybe (Field SqlInt8)) -- pk
    (Field SqlInt8) -- slot
    (Field SqlInt8) -- block
    (Field SqlInt2) -- major_protocol_version
    (Field SqlBool) -- evaluated_successfully
    (Field SqlInt8) -- exec_budget_cpu
    (Field SqlInt8) -- exec_budget_mem
    (Field SqlBytea) -- script_hash
    (FieldNullable SqlBytea) -- datum
    (FieldNullable SqlBytea) -- redeemer
    (Field SqlBytea) -- script_context
    (FieldNullable SqlInt8) -- cost_model_params

type ReadEvaluationEventRecordFields =
  EvaluationEventRecord'
    (Field SqlInt8) -- pk
    (Field SqlInt8) -- slot
    (Field SqlInt8) -- block
    (Field SqlInt2) -- major_protocol_version
    (Field SqlBool) -- evaluated_successfully
    (Field SqlInt8) -- exec_budget_cpu
    (Field SqlInt8) -- exec_budget_mem
    (Field SqlBytea) -- script_hash
    (FieldNullable SqlBytea) -- datum
    (FieldNullable SqlBytea) -- redeemer
    (Field SqlBytea) -- script_context
    (FieldNullable SqlInt8) -- cost_model_params

--------------------------------------------------------------------------------
-- script_evaluations (view) ---------------------------------------------------

data
  ScriptEvaluationRecord'
    pk
    slotNo
    blockNo
    ledgerLang
    majorProtoVer
    evaluatedSuccessfully
    budgetCpu
    budgetMem
    script
    datum
    redeemer
    scriptContext
    costModel
  = MkScriptEvaluationRecord'
  { sePk :: pk
  , seSlotNo :: slotNo
  , seBlockNo :: blockNo
  , seLedgerLanguage :: ledgerLang
  , seMajorProtocolVersion :: majorProtoVer
  , seEvaluatedSuccessfully :: evaluatedSuccessfully
  , seExecBudgetCpu :: budgetCpu
  , seExecBudgetMem :: budgetMem
  , seScript :: script
  , seDatum :: datum
  , seRedeemer :: redeemer
  , seScriptContext :: scriptContext
  , seCostModelParams :: costModel
  }
  deriving (Show, Eq)

type ScriptEvaluationRecord =
  ScriptEvaluationRecord'
    (Maybe Int64) -- pk
    SlotNo -- slot
    BlockNo -- block
    PlutusLedgerLanguage -- ledger_language
    MajorProtocolVersion -- major_protocol_version
    Bool -- evaluated_successfully
    ExCPU -- exec_budget_cpu
    ExMemory -- exec_budget_mem
    ByteString -- serialised
    (Maybe ByteString) -- datum
    (Maybe ByteString) -- redeemer
    ByteString -- script_context
    [Int64] -- cost_model_params

type ScriptEvaluationRecordFields =
  ScriptEvaluationRecord'
    (FieldNullable SqlInt8) -- pk
    (Field SqlInt8) -- slot
    (Field SqlInt8) -- block
    (Field SqlInt2) -- ledger_language
    (Field SqlInt2) -- major_protocol_version
    (Field SqlBool) -- evaluated_successfully
    (Field SqlInt4) -- exec_budget_cpu
    (Field SqlInt4) -- exec_budget_mem
    (Field SqlBytea) -- script_hash
    (FieldNullable SqlBytea) -- datum
    (FieldNullable SqlBytea) -- redeemer
    (Field SqlBytea) -- script_context
    (Field (SqlArray SqlInt8)) -- cost_model_params

--------------------------------------------------------------------------------
-- TH Splices ------------------------------------------------------------------

makeAdaptorAndInstanceInferrable "pCostModelValues" ''CostModelValuesRecord'
makeAdaptorAndInstanceInferrable "pEvaluationEvent" ''EvaluationEventRecord'
makeAdaptorAndInstanceInferrable "pScriptEvaluation" ''ScriptEvaluationRecord'
makeAdaptorAndInstanceInferrable "pSerialisedScript" ''SerialisedScriptRecord'
makeAdaptorAndInstanceInferrable "pDeserialisedScript" ''DeserialisedScriptRecord'

--------------------------------------------------------------------------------
-- DB Relations ----------------------------------------------------------------

type DbTable f = Table f f

scriptEvaluationEvents
  :: Table
      WriteEvaluationEventRecordFields
      ReadEvaluationEventRecordFields
scriptEvaluationEvents =
  table "script_evaluation_events" $
    pEvaluationEvent
      MkEvaluationEventRecord'
        { eePk = optionalTableField "pk"
        , eeSlotNo = tableField "slot"
        , eeBlockNo = tableField "block"
        , eeMajorProtocolVersion = tableField "major_protocol_version"
        , eeEvaluatedSuccessfully = tableField "evaluated_successfully"
        , eeExecBudgetCpu = tableField "exec_budget_cpu"
        , eeExecBudgetMem = tableField "exec_budget_mem"
        , eeScriptHash = tableField "script_hash"
        , eeDatum = tableField "datum"
        , eeRedeemer = tableField "redeemer"
        , eeScriptContext = tableField "script_context"
        , eeCostModelParams = tableField "cost_model_params"
        }

scriptEvaluations :: DbTable ScriptEvaluationRecordFields
scriptEvaluations =
  table "script_evaluations" $
    pScriptEvaluation
      MkScriptEvaluationRecord'
        { sePk = tableField "pk"
        , seSlotNo = tableField "slot"
        , seBlockNo = tableField "block"
        , seLedgerLanguage = tableField "ledger_language"
        , seMajorProtocolVersion = tableField "major_protocol_version"
        , seEvaluatedSuccessfully = tableField "evaluated_successfully"
        , seExecBudgetCpu = tableField "exec_budget_cpu"
        , seExecBudgetMem = tableField "exec_budget_mem"
        , seScript = tableField "serialised"
        , seDatum = tableField "datum"
        , seRedeemer = tableField "redeemer"
        , seScriptContext = tableField "script_context"
        , seCostModelParams = tableField "cost_model_param_values"
        }

costModelValues :: DbTable CostModelValuesRecordFields
costModelValues =
  table "cost_model_params" $
    pCostModelValues
      MkCostModelValues
        { cmPk = tableField "pk"
        , cmParamValues = tableField "param_values"
        }

serialisedScripts :: DbTable SerialisedScriptRecordFields
serialisedScripts =
  table "serialised_scripts" $
    pSerialisedScript
      MkSerialisedScriptRecord
        { ssHash = tableField "hash"
        , ssLedgerLanguage = tableField "ledger_language"
        , ssSerialised = tableField "serialised"
        }

deserialisedScripts :: DbTable DeserialisedScriptRecordFields
deserialisedScripts =
  table "deserialised_scripts" $
    pDeserialisedScript
      MkDeserialisedScriptRecord
        { dsHash = tableField "hash"
        , dsDeserialised = tableField "deserialised"
        }

--------------------------------------------------------------------------------
-- Utility ---------------------------------------------------------------------

printSql :: (Default Unpackspec a a) => Select a -> IO ()
printSql = putStrLn . maybe "Empty select" id . showSql
