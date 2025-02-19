module Database where

import Cardano.Slotting.Block (BlockNo (unBlockNo))
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
  -> BlockNo
  -> a
  -> (a -> ScriptEvaluationRecord -> IO a)
  -> IO a
withScriptEvaluationRecords connection blockNo accumulator callback = do
  PostgreSQL.fold
    connection
    "SELECT \
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
    \WHERE block > ? \
    \ORDER BY block ASC"
    (Only (unBlockNo blockNo))
    accumulator
    callback
