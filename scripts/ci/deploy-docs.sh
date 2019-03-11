#!/usr/bin/env bash

set -euo pipefail

function setup_git_config {
  git config user.email "ci-build@thomascrevoisier.net"
  git config user.name "ci-build"
}

function deploy_docs {
  if [ -n "$(git status --porcelain docs/)" ]; then
    git add docs/
    git commit -m "Update documentation"
    git push --quiet origin master
  else
    echo "No documentation update to deploy"
  fi
}

setup_git_config
deploy_docs
