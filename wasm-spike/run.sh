#!/usr/bin/env bash
# WASM spike driver (Plan 1.0): copies the core modules, builds with the
# wasm32-wasi GHC and runs the resulting module under wasmtime with the
# data/ directory preopened.
set -euo pipefail
cd "$(dirname "$0")"

./copy.sh

export PATH="$HOME/.ghc-wasm/bin:$PATH"
wasm32-wasi-cabal build exe:spike-main

# Locate the built module and run it.
BIN="$(find dist-newstyle -name 'spike-main.wasm' -type f | head -n 1)"
[ -n "$BIN" ] || { echo "no spike-main.wasm produced"; exit 1; }

mkdir -p data
wasmtime run --dir . "$BIN"