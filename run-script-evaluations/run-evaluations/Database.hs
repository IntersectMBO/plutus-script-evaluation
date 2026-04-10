module Database where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Database.PostgreSQL.Simple qualified as PostgreSQL
import Database.PostgreSQL.Simple.Orphans ()
import Database.PostgreSQL.Simple.Types (Only (..), PGArray)
import GHC.Generics (Generic)
import PlutusLedgerApi.Common (
  ExCPU,
  ExMemory,
  MajorProtocolVersion,
  PlutusLedgerLanguage,
 )
import Prelude

data ScriptEvaluationRecord = MkScriptEvaluationRecord
  { sePk :: Maybe Int64
  , seSlotNo :: Int64
  , seBlockNo :: Int64
  , seLedgerLanguage :: PlutusLedgerLanguage
  , seMajorProtocolVersion :: MajorProtocolVersion
  , seEvaluatedSuccessfully :: Bool
  , seExecBudgetCpu :: ExCPU
  , seExecBudgetMem :: ExMemory
  , seScript :: ByteString
  , seDatum :: Maybe ByteString
  , seRedeemer :: Maybe ByteString
  , seScriptContext :: ByteString
  , seCostModelKey :: Int64
  , seCostModelParams :: PGArray Int64
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PostgreSQL.FromRow)

withScriptEvaluationRecords
  :: PostgreSQL.Connection
  -> Int64
  -> a
  -> (a -> ScriptEvaluationRecord -> IO a)
  -> IO a
withScriptEvaluationRecords connection endsAt accumulator callback = do
  PostgreSQL.fold
    connection
    "SELECT DISTINCT ON (script_serialised) \
    \ pk, \
    \ slot, \
    \ block, \
    \ ledger_language, \
    \ major_protocol_version, \
    \ evaluated_successfully, \
    \ exec_budget_cpu, \
    \ exec_budget_mem, \
    \ script_serialised, \
    \ datum, \
    \ redeemer, \
    \ script_context, \
    \ cost_model_key, \
    \ cost_model_param_values \
    \FROM script_evaluations \
    \WHERE block BETWEEN 13370000 AND ? \
    \ORDER BY script_serialised, pk DESC;"
    (Only endsAt)
    accumulator
    callback

-- withScriptEvaluationRecords
--   :: PostgreSQL.Connection
--   -> Int64
--   -> a
--   -> (a -> ScriptEvaluationRecord -> IO a)
--   -> IO a
-- withScriptEvaluationRecords connection startFrom accumulator callback = do
--   PostgreSQL.fold
--     connection
--     "SELECT \
--     \ pk, \
--     \ slot, \
--     \ block, \
--     \ ledger_language, \
--     \ major_protocol_version, \
--     \ evaluated_successfully, \
--     \ exec_budget_cpu, \
--     \ exec_budget_mem, \
--     \ script_serialised, \
--     \ datum, \
--     \ redeemer, \
--     \ script_context, \
--     \ cost_model_key, \
--     \ cost_model_param_values \
--     \FROM script_evaluations \
--     \ORDER BY block DESC \
--     \LIMIT ?"
--     (Only startFrom)
--     accumulator
--     callback

-- 13300000 to 13377277 (1913) scripts

-- withScriptEvaluationRecords
--   :: PostgreSQL.Connection
--   -> Int64
--   -> a
--   -> (a -> ScriptEvaluationRecord -> IO a)
--   -> IO a
-- withScriptEvaluationRecords connection startFrom accumulator callback = do
--   PostgreSQL.fold
--     connection
--     "SELECT \
--     \ pk, \
--     \ slot, \
--     \ block, \
--     \ ledger_language, \
--     \ major_protocol_version, \
--     \ evaluated_successfully, \
--     \ exec_budget_cpu, \
--     \ exec_budget_mem, \
--     \ script_serialised, \
--     \ datum, \
--     \ redeemer, \
--     \ script_context, \
--     \ cost_model_key, \
--     \ cost_model_param_values \
--     \FROM ( \
--     \  SELECT DISTINCT ON (script_serialised) * \
--     \  FROM ( \
--     \    SELECT * \
--     \    FROM script_evaluations \
--     \    ORDER BY pk DESC \
--     \    LIMIT 70000 \
--     \  ) top_rows \
--     \  ORDER BY script_serialised, pk DESC \
--     \) sub \
--     \ORDER BY pk DESC \
--     \LIMIT ?"
--     (Only startFrom)
--     accumulator
--     callback
