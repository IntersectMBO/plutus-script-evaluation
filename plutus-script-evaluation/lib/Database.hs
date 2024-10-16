{-# LANGUAGE UndecidableInstances #-}

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

data CostModelValues' hash64 ledgerLang protoVer paramValues = MkCostModelValues
  { cmPk :: hash64
  , cmLedgerLanguage :: ledgerLang
  , cmMajorProtocolVersion :: protoVer
  , cmParamValues :: paramValues
  }
  deriving (Show, Eq)

type CostModelValues =
  CostModelValues'
    Hash64 -- pk
    PlutusLedgerLanguage -- ledger_language
    Int16 -- major_protocol_version
    [Int64] -- param_values

type CostModelValuesFields =
  CostModelValues'
    (Field SqlInt8) -- pk
    (Field SqlInt2) -- ledger_language
    (Field SqlInt2) -- major_protocol_version
    (Field (SqlArray SqlInt8)) -- param_values

$(makeAdaptorAndInstanceInferrable "pCostModelValues" ''CostModelValues')

costModelValues :: DbTable CostModelValuesFields
costModelValues =
  table "cost_model_params" $
    pCostModelValues
      MkCostModelValues
        { cmPk = tableField "pk"
        , cmLedgerLanguage = tableField "ledger_language"
        , cmMajorProtocolVersion = tableField "major_protocol_ver"
        , cmParamValues = tableField "param_values"
        }

selectCostModelValues :: Select CostModelValuesFields
selectCostModelValues = selectTable costModelValues

insertCostModelValues
  :: (Default ToFields CostModelValues CostModelValuesFields)
  => Connection
  -> [CostModelValues]
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
-- script_evaluation_events ----------------------------------------------------

data EvaluationEvent' a b c d e f g h i j k l = MkEvaluationEvent
  { eeSlotNo :: a
  , eeBlockNo :: b
  , eeEvaluatedSuccessfully :: c
  , eeExecBudgetCpu :: d
  , eeExecBudgetMem :: e
  , eeSerialisedScript :: f
  , eeDatum :: g
  , eeRedeemer :: h
  , eeScriptContext :: i
  , eeLedgerLanguage :: j
  , eeMajorProtocolVersion :: k
  , eeCostModelParams :: l
  }
  deriving (Show, Eq)

type EvaluationEvent =
  EvaluationEvent'
    SlotNo -- slot
    BlockNo -- block
    Bool -- evaluated_successfully
    Int64 -- exec_budget_cpu
    Int64 -- exec_budget_mem
    ByteString -- serialised_script
    (Maybe ByteString) -- datum
    (Maybe ByteString) -- redeemer
    ByteString -- script_context
    PlutusLedgerLanguage -- ledger_language
    Int16 -- major_protocol_version
    Hash64 -- cost_model_params

type EvaluationEventFields =
  EvaluationEvent'
    (Field SqlInt8) -- block
    (Field SqlInt8) -- slot
    (Field SqlBool) -- evaluated_successfully
    (Field SqlInt8) -- exec_budget_cpu
    (Field SqlInt8) -- exec_budget_mem
    (Field SqlBytea) -- serialised_script
    (FieldNullable SqlBytea) -- datum
    (FieldNullable SqlBytea) -- redeemer
    (Field SqlBytea) -- script_context
    (Field SqlInt2) -- ledger_language
    (Field SqlInt2) -- major_protocol_version
    (Field SqlInt8) -- cost_model_params

$(makeAdaptorAndInstanceInferrable "pEvaluationEvent" ''EvaluationEvent')

scriptEvaluationEvents :: DbTable EvaluationEventFields
scriptEvaluationEvents =
  table "script_evaluation_events" $
    pEvaluationEvent
      MkEvaluationEvent
        { eeSlotNo = tableField "slot"
        , eeBlockNo = tableField "block"
        , eeEvaluatedSuccessfully = tableField "evaluated_successfully"
        , eeExecBudgetCpu = tableField "exec_budget_cpu"
        , eeExecBudgetMem = tableField "exec_budget_mem"
        , eeSerialisedScript = tableField "serialised_script"
        , eeDatum = tableField "datum"
        , eeRedeemer = tableField "redeemer"
        , eeScriptContext = tableField "script_context"
        , eeLedgerLanguage = tableField "ledger_language"
        , eeMajorProtocolVersion = tableField "major_protocol_ver"
        , eeCostModelParams = tableField "cost_model_params"
        }

selectScriptEvaluationEvents :: Select EvaluationEventFields
selectScriptEvaluationEvents = selectTable scriptEvaluationEvents

insertScriptEvaluationEvents
  :: (Default ToFields EvaluationEvent EvaluationEventFields)
  => Connection
  -> [EvaluationEvent]
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
