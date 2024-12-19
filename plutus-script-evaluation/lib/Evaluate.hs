{-# LANGUAGE PartialTypeSignatures #-}

module Evaluate where

import Control.Exception (throwIO)
import Control.Lens.Operators ((^.))
import Control.Lens.Traversal (traverseOf)
import Data.ByteString.Short qualified as BSS
import Data.Digest.Murmur64 (Hash64)
import Data.Map (Map)
import Data.Map qualified as Map
import Database qualified as Db
import Database.PostgreSQL.Simple qualified as Postgres
import Database.Schema (
  EvaluationEventRecord' (..),
  ScriptEvaluationRecord' (..),
 )
import PlutusCore.Builtin.Debug (BuiltinSemanticsVariant (..))
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (
  defaultCekParametersForVariant,
 )
import PlutusCore.Evaluation.Machine.MachineParameters (MachineParameters)
import PlutusCore.Quote (runQuote, runQuoteT)
import PlutusLedgerApi.Common (
  ScriptForEvaluation,
  ScriptNamedDeBruijn (..),
  deserialisedScript,
 )
import PlutusLedgerApi.V3 (
  ExBudget (..),
  ExCPU (..),
  ExMemory (ExMemory),
  deserialiseScript,
 )
import UntypedPlutusCore (
  DefaultFun,
  DefaultUni,
  FakeNamedDeBruijn,
  FreeVariableError,
  Name,
  NamedDeBruijn (..),
  progTerm,
  unDeBruijnTerm,
 )
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek (
  CekEvaluationException,
  CekMachineCosts,
  CekValue,
  CountingSt (..),
  ExBudgetMode,
  counting,
  noEmitter,
  runCek,
 )

data ScriptEvaluationInput = MkScriptEvaluationInput
  { seiExCpu :: ExCPU
  , seiExMem :: ExMemory
  , seiUplc :: UPLC.Program NamedDeBruijn DefaultUni DefaultFun ()
  }

evaluateScripts
  :: Postgres.Connection
  -- ^ Database connection
  -> a
  -- ^ Initial accumulator
  -> (ScriptEvaluationInput -> a -> IO a)
  -- ^ Accumulation function
  -> IO a
evaluateScripts conn initialAccum accumulate =
  Db.withScriptEvaluationEvents conn initialAccum \accum record ->
    accumulate (inputFromRecord record) accum

inputFromRecord :: Db.ScriptEvaluationRecord -> ScriptEvaluationInput
inputFromRecord MkScriptEvaluationRecord'{..} =
  MkScriptEvaluationInput
    { seiExCpu = seExecBudgetCpu
    , seiExMem = seExecBudgetMem
    , seiUplc =
        case deserialiseScript seMajorProtocolVersion (BSS.toShort seScript) of
          Left err -> error $ "Failed to deserialise script: " <> show err
          Right (deserialisedScript -> ScriptNamedDeBruijn uplc) -> uplc
    }

data Accum = MkAccum
  { winCpu :: ExCPU
  , winMem :: ExMemory
  }

onScriptEvaluationInput :: ScriptEvaluationInput -> Accum -> IO Accum
onScriptEvaluationInput MkScriptEvaluationInput{..} accum = do
  uplcTerm :: UPLC.Term Name DefaultUni DefaultFun () <-
    case runQuoteT (traverseOf progTerm unDeBruijnTerm seiUplc) of
      Left (err :: FreeVariableError) ->
        throwIO . userError $ "Failed to convert script: " <> show err
      Right program -> pure $ program ^. progTerm

  let
    budgetMode :: ExBudgetMode CountingSt DefaultUni DefaultFun
    budgetMode = counting

    ( result
        :: Either
            (CekEvaluationException UPLC.Name DefaultUni DefaultFun)
            (UPLC.Term Name DefaultUni DefaultFun ())
      , CountingSt ExBudget{exBudgetCPU, exBudgetMemory}
      , _logs
      ) = runCek machineParameters budgetMode noEmitter uplcTerm

    _isSuccessful =
      case result of
        Left _exception -> False
        Right _term -> True -- todo: check return value
  pure
    accum
      { winCpu = exBudgetCPU - seiExCpu
      , winMem = exBudgetMemory - seiExMem
      }

machineParameters
  :: MachineParameters
      CekMachineCosts
      DefaultFun
      (CekValue DefaultUni DefaultFun ())
machineParameters = defaultCekParametersForVariant DefaultFunSemanticsVariantC
