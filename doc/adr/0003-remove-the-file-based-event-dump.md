# 3. Remove the file-based event dump

Date: 2026-08-27

## Status

Accepted

Amends [2. Persist script evaluation events in a database](0002-store-script-evaluation-events-in-a-database.md)

## Context

ADR 0002 moved script evaluation events into PostgreSQL and kept the "dump" job
operational as a backup. Two years on, the backup has not been used, and keeping
it carries costs that ADR 0002 did not anticipate.

The two jobs are not a primary and a copy. They are two independent replays of
the chain. `load-script-events` does not consume the `*.event` files: it applies
blocks itself and maintains its own checkpoints. Nothing in the repository reads
a dump file back — `FileStorage.readEventsFile` has no caller.

The dump format is owned by another repository. `ScriptEvaluationEvents` comes
from `plutus-ledger-api:plutus-ledger-api-testlib` and carries fixed fields for
PlutusV1 and PlutusV2 cost model parameters only. PlutusV3 parameters are
collected and then discarded on write, because the record has nowhere to put
them. A dump file holding PlutusV3 events therefore cannot be re-evaluated: the
evaluation context cannot be rebuilt from it. Every future Plutus language
version repeats this.

That dependency also blocks local development. The Nix package database provides
`plutus-ledger-api` but not its `plutus-ledger-api-testlib` sub-library, so the
solver declines to reuse it and rebuilds the whole Cardano stack from source.

## Decision

We remove the file-based event dump: the `dump-script-events` executable, the
`Dump` and `LedgerEvents.FileWriter` modules, and the event read and write
functions in `FileStorage`.

The checkpoint files stay. They are a separate, working mechanism, written and
read by `load-script-events` so a restart resumes instead of replaying from
genesis.

## Consequences

- The database is the only record of script evaluation events. It has no
  file-system backup. Rebuilding from an empty database means replaying the
  chain, which the `load` job already supports from any checkpoint.

- The `plutus-ledger-api-testlib` dependency is gone. The indexer no longer
  depends on data structures defined in the Plutus repository.

- Existing `*.event` files on disk become inert. Nothing reads them; nothing
  deletes them. They can be removed by hand.

- Adding a Plutus language version no longer requires a new fixed cost-params
  field in a record we do not own.
