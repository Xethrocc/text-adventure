#!/usr/bin/env bash
# WASM spike driver (Plan 1.0): builds the unmodified production library from
# ../src with the wasm32-wasi GHC and runs the resulting module under wasmtime
# with the data/ directory preopened.
#
# The obsolete first attempt (copy.sh + src-gen/ + stubs/) is NOT used any more:
# the real library builds for wasm32-wasi as-is (see docs/wasm-spike-2026-09.md).
set -euo pipefail
cd "$(dirname "$0")"

# The toolchain is not on PATH by default — ghc-wasm-meta ships an env file.
# An earlier version of this script exported "$HOME/.ghc-wasm/bin", which does
# not exist, so wasm32-wasi-cabal and wasmtime were never found.
if [ -f "$HOME/.ghc-wasm/env" ]; then
    # shellcheck disable=SC1090,SC1091
    . "$HOME/.ghc-wasm/env"
else
    echo "warning: $HOME/.ghc-wasm/env not found — wasm toolchain installed?" >&2
fi

command -v wasm32-wasi-cabal >/dev/null || { echo "wasm32-wasi-cabal not on PATH"; exit 1; }
command -v wasmtime >/dev/null || { echo "wasmtime not on PATH"; exit 1; }

# The input files are not checked in (see .gitignore).
if [ ! -f data/world.json ] || [ ! -f data/save.json ]; then
    echo "data/{world.json,save.json} missing — compile the fixture first:" >&2
    echo "  cabal run worldbuilder -- compile examples/fixtures/dark-feelable.yaml -o wasm-spike/data" >&2
    exit 1
fi

wasm32-wasi-cabal build exe:spike-main

# Locate the built module and run it.
BIN="$(find dist-newstyle -name 'spike-main.wasm' -type f | head -n 1)"
[ -n "$BIN" ] || { echo "no spike-main.wasm produced"; exit 1; }

wasmtime run --dir . "$BIN"