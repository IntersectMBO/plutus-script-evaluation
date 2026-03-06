#!/usr/bin/env bash

set -euo pipefail

# Default values
PLUTUS_DIR=""
START_FROM=0
REMOTE_HOST="plutus"
REMOTE_TMP=""
ATTACH=false
STATUS=false
RESUME_DIR=""
TMUX_SESSION="plutus-eval"

usage() {
  echo "Usage: $0 --plutus-dir <path> [options]" >&2
  echo "       $0 --resume <remote-tmp> [options]" >&2
  echo "" >&2
  echo "Run Plutus script evaluations on the remote plutus-node server inside a tmux session." >&2
  echo "" >&2
  echo "Options:" >&2
  echo "  --plutus-dir <path>     Local path to Plutus checkout (required for new runs)" >&2
  echo "  --resume <remote-tmp>   Resume using existing remote temp dir (skip rsync/patch)" >&2
  echo "  --start-from <n>        Start evaluation from record n (default: 0)" >&2
  echo "  --pgpassword <password> PostgreSQL password (or set PGPASSWORD env var)" >&2
  echo "  --attach                Attach to tmux session (can be used standalone)" >&2
  echo "  --status [remote-tmp]   Check evaluation status, mismatches, and suggest cleanup" >&2
  exit 1
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --plutus-dir)
      PLUTUS_DIR="$2"
      shift 2
      ;;
    --resume)
      RESUME_DIR="${2:?Error: --resume requires a remote temp dir path}"
      shift 2
      ;;
    --start-from)
      START_FROM="${2:?Error: --start-from requires a number}"
      shift 2
      ;;
    --pgpassword)
      PGPASSWORD="${2:?Error: --pgpassword requires a password}"
      export PGPASSWORD
      shift 2
      ;;
    --attach)
      ATTACH=true
      shift
      ;;
    --status)
      STATUS=true
      if [[ -n "${2:-}" && ! "$2" =~ ^-- ]]; then
        STATUS_DIR="$2"
        shift 2
      else
        STATUS_DIR=""
        shift
      fi
      ;;
    *)
      echo "Error: Unknown option $1" >&2
      usage
      ;;
  esac
done

# Attach-only mode: just connect to existing tmux session
if [[ "$ATTACH" == true && -z "$PLUTUS_DIR" && -z "$RESUME_DIR" ]]; then
  exec ssh -t "$REMOTE_HOST" "tmux attach -t $TMUX_SESSION"
fi

# Status mode: check progress, mismatches, and suggest cleanup
if [[ "$STATUS" == true ]]; then
  if [[ -z "$STATUS_DIR" ]]; then
    # Auto-discover from tmux session's eval.sh path
    STATUS_DIR=$(ssh "$REMOTE_HOST" "tmux display-message -t $TMUX_SESSION -p '#{pane_start_command}' 2>/dev/null" | grep -oE '/tmp/plutus-eval-[^/]+' || true)
    if [[ -z "$STATUS_DIR" ]]; then
      # Fallback: find the most recent plutus-eval temp dir
      STATUS_DIR=$(ssh "$REMOTE_HOST" "ls -dt /tmp/plutus-eval-* 2>/dev/null | head -1")
    fi
    if [[ -z "$STATUS_DIR" ]]; then
      echo "Error: could not find a plutus-eval directory on the server" >&2
      exit 1
    fi
    echo "Discovered remote dir: $STATUS_DIR"
  fi
  REMOTE_TMP="$STATUS_DIR"
  EVAL_DIR="$REMOTE_TMP/plutus-script-evaluation/run-script-evaluations"

  # Check tmux session
  echo "=== Session status ==="
  if ssh "$REMOTE_HOST" "tmux has-session -t $TMUX_SESSION 2>/dev/null"; then
    echo "tmux session '$TMUX_SESSION': RUNNING"
  else
    echo "tmux session '$TMUX_SESSION': NOT RUNNING"
  fi

  # Show progress
  echo ""
  echo "=== Progress ==="
  LAST_PK=$(ssh "$REMOTE_HOST" "tail -20 '$EVAL_DIR/evaluation.log' 2>/dev/null | grep -E '^[0-9]+$' | sort -n | tail -1")
  if [[ -n "$LAST_PK" ]]; then
    echo "Last processed PK: $LAST_PK"
    # Get total records and block info from DB
    PGPASS=$(ssh "$REMOTE_HOST" "awk -F: '/plutus-reader/{print \$5}' /home/cardano-node/.pgpass")
    DB_INFO=$(ssh "$REMOTE_HOST" "PGPASSWORD='$PGPASS' psql -h localhost -U plutus-reader -d mainnet_plutus_events -t -A -c \
      \"SELECT max(pk), max(block) FROM script_evaluations\"" 2>/dev/null)
    MAX_PK=$(echo "$DB_INFO" | cut -d'|' -f1)
    MAX_BLOCK=$(echo "$DB_INFO" | cut -d'|' -f2)
    CURRENT_BLOCK=$(ssh "$REMOTE_HOST" "PGPASSWORD='$PGPASS' psql -h localhost -U plutus-reader -d mainnet_plutus_events -t -A -c \
      \"SELECT block FROM script_evaluations WHERE pk = (SELECT max(pk) FROM script_evaluations WHERE pk <= $LAST_PK)\"" 2>/dev/null)
    if [[ -n "$MAX_PK" ]]; then
      PCT=$(awk "BEGIN { printf \"%.1f\", 100.0 * $LAST_PK / $MAX_PK }")
      echo "Records: $LAST_PK / $MAX_PK ($PCT%)"
    fi
    if [[ -n "$CURRENT_BLOCK" && -n "$MAX_BLOCK" ]]; then
      # Get mainnet tip block
      TIP_BLOCK=$(ssh "$REMOTE_HOST" "cardano-cli query tip --immutable-tip --mainnet \
        --socket-path /var/run/cardano-node/node.socket 2>/dev/null | grep '\"block\"' | grep -oE '[0-9]+'" 2>/dev/null)
      if [[ -n "$TIP_BLOCK" ]]; then
        BLOCK_PCT=$(awk "BEGIN { printf \"%.1f\", 100.0 * $CURRENT_BLOCK / $TIP_BLOCK }")
        echo "Blocks:  $CURRENT_BLOCK / $TIP_BLOCK ($BLOCK_PCT% of mainnet tip)"
      else
        echo "Block:   $CURRENT_BLOCK (indexed up to $MAX_BLOCK)"
      fi
    fi
  else
    echo "(no progress data in log)"
  fi

  # Check for mismatch log files
  echo ""
  echo "=== Mismatch report ==="
  MISMATCH_FILES=$(ssh "$REMOTE_HOST" "find '$EVAL_DIR' -maxdepth 1 -name '*.log' ! -name 'evaluation.log' -type f 2>/dev/null")
  if [[ -z "$MISMATCH_FILES" ]]; then
    echo "No mismatches found."
  else
    MISMATCH_COUNT=$(echo "$MISMATCH_FILES" | wc -l)
    echo "Found $MISMATCH_COUNT mismatch log file(s):"
    echo "$MISMATCH_FILES" | while read -r f; do
      echo ""
      echo "--- $(basename "$f") ---"
      ssh "$REMOTE_HOST" "head -5 '$f'"
    done
  fi

  # Suggest cleanup
  echo ""
  echo "=== Cleanup ==="
  echo "  # Remove remote temp directory"
  echo "  ssh $REMOTE_HOST rm -rf $REMOTE_TMP"
  exit 0
fi

if [[ -z "$PLUTUS_DIR" && -z "$RESUME_DIR" ]]; then
  echo "Error: either --plutus-dir or --resume is required" >&2
  usage
fi

if [[ -n "$PLUTUS_DIR" && -n "$RESUME_DIR" ]]; then
  echo "Error: --plutus-dir and --resume are mutually exclusive" >&2
  usage
fi

# --- New run: rsync project and plutus, patch cabal.project ---
if [[ -n "$PLUTUS_DIR" ]]; then
  # Validate plutus directory
  PLUTUS_DIR=$(realpath "$PLUTUS_DIR")
  for subdir in plutus-tx plutus-core plutus-ledger-api; do
    if [[ ! -d "$PLUTUS_DIR/$subdir" ]]; then
      echo "Error: $PLUTUS_DIR/$subdir not found. Is this a valid Plutus checkout?" >&2
      exit 1
    fi
  done

  # Get the project root (parent of run-script-evaluations/)
  SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
  PROJECT_DIR=$(dirname "$SCRIPT_DIR")

  # Create temp directory on server
  echo "Creating temp directory on $REMOTE_HOST..."
  REMOTE_TMP=$(ssh "$REMOTE_HOST" "mktemp -d /tmp/plutus-eval-XXXXXX")
  echo "Remote temp: $REMOTE_TMP"

  # Sync project to server
  echo "Syncing plutus-script-evaluation to server..."
  rsync -az --exclude='.git' --exclude='dist-newstyle' --exclude='dumps' \
    "$PROJECT_DIR/" "$REMOTE_HOST:$REMOTE_TMP/plutus-script-evaluation/"

  # Sync plutus checkout to server
  echo "Syncing Plutus checkout to server..."
  rsync -az --exclude='.git' --exclude='dist-newstyle' \
    "$PLUTUS_DIR/" "$REMOTE_HOST:$REMOTE_TMP/plutus/"

  # Patch cabal.project to use local plutus packages
  echo "Patching cabal.project with local Plutus paths..."
  ssh "$REMOTE_HOST" "cat >> '$REMOTE_TMP/plutus-script-evaluation/cabal.project' << EOF

-- Added by run_on_server.sh
packages:
  $REMOTE_TMP/plutus/plutus-tx
  $REMOTE_TMP/plutus/plutus-core
  $REMOTE_TMP/plutus/plutus-ledger-api
EOF"
fi

# --- Resume: reuse existing remote temp dir ---
if [[ -n "$RESUME_DIR" ]]; then
  REMOTE_TMP="$RESUME_DIR"
  echo "Resuming with remote temp: $REMOTE_TMP"

  # Verify the directory exists
  if ! ssh "$REMOTE_HOST" "test -d '$REMOTE_TMP/plutus-script-evaluation/run-script-evaluations'"; then
    echo "Error: $REMOTE_TMP/plutus-script-evaluation/run-script-evaluations not found on server" >&2
    exit 1
  fi
fi

# Resolve password: use explicit PGPASSWORD, or read from server's ~/.pgpass
if [[ -z "${PGPASSWORD:-}" ]]; then
  echo "Reading password from server's ~/.pgpass..."
  PGPASSWORD=$(ssh "$REMOTE_HOST" "awk -F: '/plutus-reader/{print \$5}' /home/cardano-node/.pgpass")
  if [[ -z "$PGPASSWORD" ]]; then
    echo "Error: No password found in server's ~/.pgpass for plutus-reader" >&2
    exit 1
  fi
fi

DB_CONN_STRING="host=localhost port=5432 dbname=mainnet_plutus_events user=plutus-reader password=$PGPASSWORD"

# Write eval.sh helper script on server
echo "Writing eval.sh on server..."
EVAL_SCRIPT="#!/usr/bin/env bash
set -euo pipefail
cd '${REMOTE_TMP}/plutus-script-evaluation/run-script-evaluations'
nix develop --no-warn-dirty --accept-flake-config --impure \\
  --command bash -c 'cabal clean && cabal update && \\
    cabal run exe:run-script-evaluations -- \\
    --start-from=${START_FROM} \\
    --database-conn-str \"${DB_CONN_STRING}\" \\
    2>&1 | tee evaluation.log'
"
printf '%s' "$EVAL_SCRIPT" | ssh "$REMOTE_HOST" "cat > '$REMOTE_TMP/eval.sh' && chmod +x '$REMOTE_TMP/eval.sh'"

# Kill existing tmux session if any
ssh "$REMOTE_HOST" "tmux kill-session -t $TMUX_SESSION 2>/dev/null || true"

# Start detached tmux session
echo "Starting tmux session '$TMUX_SESSION'..."
ssh "$REMOTE_HOST" "tmux new-session -d -s $TMUX_SESSION '$REMOTE_TMP/eval.sh'"

echo ""
echo "=== Evaluation started in tmux session '$TMUX_SESSION' ==="
echo ""
echo "Remote temp dir: $REMOTE_TMP"
echo ""
echo "Useful commands:"
echo "  # Check if running"
echo "  ssh $REMOTE_HOST tmux ls"
echo ""
echo "  # Attach to watch output"
echo "  ssh -t $REMOTE_HOST tmux attach -t $TMUX_SESSION"
echo ""
echo "  # Detach: press Ctrl-b d"
echo ""
echo "  # View logs without attaching"
echo "  ssh $REMOTE_HOST cat $REMOTE_TMP/plutus-script-evaluation/run-script-evaluations/evaluation.log"
echo ""
echo "  # Check status and mismatches"
echo "  $0 --status $REMOTE_TMP"
echo ""
echo "  # Resume with different start-from"
echo "  $0 --resume $REMOTE_TMP --start-from <n>"
echo ""
echo "  # Clean up when done"
echo "  ssh $REMOTE_HOST rm -rf $REMOTE_TMP"

if [[ "$ATTACH" == true ]]; then
  echo ""
  echo "Attaching to tmux session..."
  exec ssh -t "$REMOTE_HOST" "tmux attach -t $TMUX_SESSION"
fi
