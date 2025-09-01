#!/usr/bin/env bash

set -euo pipefail

# Default values
REPO="IntersectMBO/plutus"
BRANCH="master"
QUIET=false

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
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
      echo "Usage: $0 [--branch <branch>] [--quiet]" >&2
      exit 1
      ;;
  esac
done

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
