#!/usr/bin/env bash
# WASM Spike (Plan 1.0): copies the pure engine core from ../src into
# src-gen/. Frontend.hs and SaveLoad.hs are NOT copied — the spike replaces
# them with IO stubs in stubs/ (haskeline / directory are not available on
# wasm32-wasi). This keeps the spike independent of the production tree.
set -euo pipefail
cd "$(dirname "$0")"

rm -rf src-gen
mkdir -p src-gen/Types

for m in Types.hs Types/Core.hs Types/Core.hs-boot Types/Cards.hs Types/Vehicles.hs Types/Combat.hs \
         Verbs.hs Game.hs GameLoop.hs Parser.hs Combat.hs Quests.hs Vehicles.hs \
         Effects.hs Cards.hs Completion.hs Ansi.hs Sample.hs; do
    cp "../src/$m" "src-gen/$m"
done

echo "copied 17 modules (+ hs-boot) to src-gen/"