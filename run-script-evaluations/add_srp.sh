#!/usr/bin/env bash

set -euo pipefail

# Default values
REPO="IntersectMBO/plutus"
BRANCH="master"
QUIET=false

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --repo)
      REPO="$2"
      shift 2
      ;;
    --branch)
      BRANCH="$2"
      shift 2
      ;;
    --quiet)
      QUIET=true
      shift
      ;;
    *)
      echo "Error: Unknown option $1" >&2
      echo "Usage: $0 [--repo <owner/repo>] [--branch <branch>] [--quiet]" >&2
      exit 1
      ;;
  esac
done

# Validate inputs (defense in depth — these may originate from workflow_dispatch inputs).
# An allowlist rejects shell metacharacters, so the values cannot inject commands even if
# a caller forgets to quote them.
if [[ ! "$REPO" =~ ^[A-Za-z0-9._-]+/[A-Za-z0-9._-]+$ ]]; then
  echo "Error: Invalid repository '$REPO' (expected owner/repo)" >&2
  exit 1
fi

if [[ ! "$BRANCH" =~ ^[A-Za-z0-9._/-]+$ ]]; then
  echo "Error: Invalid branch name '$BRANCH'" >&2
  exit 1
fi

# Check dependencies
if ! command -v jq >/dev/null 2>&1; then
  echo "Error: jq is required but not installed" >&2
  exit 1
fi

if ! command -v nix-prefetch-git >/dev/null 2>&1; then
  echo "Error: nix-prefetch-git is required but not installed" >&2
  exit 1
fi

# Log function
log() {
  if [[ "$QUIET" != "true" ]]; then
    echo "$@"
  fi
}

REPO_URL="https://github.com/$REPO"

# Fetch latest commit with error handling
log "Fetching latest commit from $REPO_URL (branch: $BRANCH)..."
if ! PREFETCHED=$(nix-prefetch-git --quiet --branch-name "$BRANCH" --url "$REPO_URL" --no-deepClone); then
  echo "Error: Failed to fetch from $REPO_URL" >&2
  exit 2
fi

# Extract commit hash and nix hash with validation
if ! LAST_COMMIT=$(echo "$PREFETCHED" | jq -r .rev); then
  echo "Error: Failed to extract commit hash" >&2
  exit 3
fi

if ! NIX_SHA=$(echo "$PREFETCHED" | jq -r .hash); then
  echo "Error: Failed to extract nix hash" >&2
  exit 3
fi

# Validate extracted data
if [[ -z "$LAST_COMMIT" || "$LAST_COMMIT" == "null" ]]; then
  echo "Error: Invalid commit hash extracted" >&2
  exit 4
fi

if [[ -z "$NIX_SHA" || "$NIX_SHA" == "null" ]]; then
  echo "Error: Invalid nix hash extracted" >&2
  exit 4
fi

log "Found commit: $LAST_COMMIT"
log "Nix hash: $NIX_SHA"

# Create backup of cabal.project
cp cabal.project cabal.project.backup

# Sync the CHaP/Hackage index-state to the one the pinned Plutus commit was tested
# against, so we resolve the same package set Plutus itself uses (e.g. the aeson >=2.3
# / cardano-base >=0.1.5 bumps). We read it from the already-prefetched checkout
# ($PREFETCHED .path), so this uses the exact pinned commit with no extra network call.
# The flake inputs still need `nix flake update hackage CHaP` (done in CI) so haskell.nix's
# snapshots cover these dates.
PLUTUS_SRC=$(echo "$PREFETCHED" | jq -r .path)
if [[ -n "$PLUTUS_SRC" && "$PLUTUS_SRC" != "null" && -f "$PLUTUS_SRC/cabal.project" ]]; then
  DATE_RE='[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:]+Z'
  PLUTUS_HACKAGE_IDX=$(grep -oE "hackage\.haskell\.org[[:space:]]+$DATE_RE" "$PLUTUS_SRC/cabal.project" | grep -oE "$DATE_RE" | head -1)
  PLUTUS_CHAP_IDX=$(grep -oE "cardano-haskell-packages[[:space:]]+$DATE_RE" "$PLUTUS_SRC/cabal.project" | grep -oE "$DATE_RE" | head -1)
  if [[ -n "$PLUTUS_HACKAGE_IDX" && -n "$PLUTUS_CHAP_IDX" ]]; then
    log "Syncing index-state from Plutus (hackage: $PLUTUS_HACKAGE_IDX, CHaP: $PLUTUS_CHAP_IDX)"
    sed -i -E "s|(hackage\.haskell\.org[[:space:]]+)$DATE_RE|\1$PLUTUS_HACKAGE_IDX|" cabal.project
    sed -i -E "s|(cardano-haskell-packages[[:space:]]+)$DATE_RE|\1$PLUTUS_CHAP_IDX|" cabal.project
  else
    log "Warning: could not parse index-state from Plutus cabal.project; leaving ours unchanged"
  fi
else
  log "Warning: prefetched Plutus checkout has no cabal.project; leaving index-state unchanged"
fi

# Remove existing Plutus SRP entries to make script idempotent
log "Removing existing Plutus source-repository-package entries..."
sed -i '/-- Added by add_srp.sh script/,/plutus-ledger-api/d' cabal.project

# Create the new section
NEW_SECTION="-- Added by add_srp.sh script
source-repository-package
  type: git
  location: $REPO_URL
  tag: $LAST_COMMIT
  --sha256: $NIX_SHA
  subdir:
    plutus-tx
    plutus-core
    plutus-ledger-api"

# Append new section to cabal.project
echo "$NEW_SECTION" >> cabal.project

# Verify the file is still readable (basic syntax check)
if ! head -1 cabal.project >/dev/null 2>&1; then
  echo "Error: cabal.project appears to be corrupted, restoring backup" >&2
  mv cabal.project.backup cabal.project
  exit 5
fi

# Clean up backup on success
rm cabal.project.backup

log "Successfully added new source-repository-package section:"
if [[ "$QUIET" != "true" ]]; then
  echo
  echo "$NEW_SECTION"
fi

exit 0
