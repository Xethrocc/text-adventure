#!/usr/bin/env bash
# WASM Spike (Plan 1.0) — OBSOLETE first attempt, kept as a documented fallback.
# It copies a subset of the core modules from ../src into src-gen/ for the
# stub-based variant. The current spike does NOT use this: spike.cabal compiles
# the unmodified production library straight from ../src, and the wasm32-wasi
# GHC distribution ships haskeline, directory, filepath and process (WASI-patched),
# so no stub separation is needed. Kept for reference only.
set -euo pipefail
cd "$(dirname "$0")"

rm -rf src-gen
mkdir -p src-gen/Types

for m in Types.hs Types/Core.hs Types/Core.hs-boot Types/Cards.hs Types/Vehicles.hs Types/Combat.hs \
         Verbs.hs Game.hs GameLoop.hs Parser.hs Combat.hs Quests.hs Vehicles.hs \
         Effects.hs Cards.hs Completion.hs Ansi.hs Sample.hs Messages.hs; do
    cp "../src/$m" "src-gen/$m"
done

echo "copied 17 modules (+ hs-boot) to src-gen/"