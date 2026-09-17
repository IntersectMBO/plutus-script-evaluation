# check-decoder-bounds

Checks whether any script ever evaluated on mainnet exceeds the flat-decoder
bounds that `PlutusLedgerApi.Common.Versions.maxBoundsByPV` applies from
protocol version 11 (`vanRossemPV`) onwards:

- `mbHeader = 32` — the maximum `defaultUniSize` of a constant's type.
  Enforced per constant by `checkConstant` in
  `PlutusLedgerApi.Common.SerialisedScript.scriptCBORDecoder`.
- `mbConstr = 1024` — the maximum number of *fields* of a UPLC `constr` term.
  Enforced by `checkConstr`, threaded into `UntypedPlutusCore.decodeProgram`.

If no script in `serialised_scripts` (every distinct script ever evaluated on
mainnet) exceeds the strict bounds, that lenient branch can be removed from
plutus.

## What it does

Streams every row of `serialised_scripts` (constant memory, server-side
cursor), decodes the CBOR-wrapped flat-encoded UPLC program with all decoder
checks disabled and computes per script:

- the maximum `defaultUniSize` over its constants,
- the maximum field count over its `constr` terms.

Exit codes: `0` if nothing exceeds the bounds, `1` if some script does, `2` if
nothing does but some rows failed to decode, which makes the evidence
incomplete. Counts are exact; the report lists at most the first 100 violations
and the first 100 decode failures.

## Usage

```sh
cabal run exe:check-decoder-bounds -- \
  --database-conn-str "host=localhost port=5432 dbname=mainnet_plutus_events user=plutus-reader"
```

## Caveats

- The database covers **mainnet only**; preview/preprod history also replays
  with `pv < 11` but is not checked by this tool.
- The actual removal of the lenient branch is a change in
  `IntersectMBO/plutus` (`Versions.hs` / `SerialisedScript.hs`); this tool
  only produces the evidence.
