#! /usr/bin/env bash

REPO="IntersectMBO/plutus"
REPO_URL="https://github.com/$REPO"
PREFETCHED=$(nix-prefetch-git --quiet --branch-name master --url $REPO_URL --no-deepClone)
LAST_COMMIT=$(echo $PREFETCHED | jq -r .rev)
NIX_SHA=$(echo $PREFETCHED | jq -r .hash)

# Create the new section to append
read -r -d '' NEW_SECTION << EOM
source-repository-package
  type: git
  location: $REPO_URL
  tag: $LAST_COMMIT
  --sha256: $NIX_SHA
  subdir:
    plutus-tx
    plutus-core
    plutus-ledger-api
EOM

# Append to cabal.project
echo "$NEW_SECTION" >> cabal.project

# Output the values for potential later use
echo "commit=$LAST_COMMIT" >> $GITHUB_OUTPUT
echo "sha256=$NIX_SHA" >> $GITHUB_OUTPUT
