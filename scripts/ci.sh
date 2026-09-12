#!/usr/bin/env bash
# Phase 6c: CI pipeline for the text-adventure engine.
#
#   1. build both packages
#   2. run the unit test suites (engine + worldbuilder)
#   3. validate every shipped adventure (demo, thefog, 6 genre fixtures)
#   4. E2E: compile each fixture and drive it to a known ending
#
# Usage: scripts/ci.sh
set -euo pipefail

cd "$(dirname "$0")/.."

WORLDBUILDER=(cabal run -v0 worldbuilder --)
GAME=(cabal run -v0 text-adventure --)

echo "== 1. build =="
cabal build all

echo "== 2. unit tests =="
cabal test all

echo "== 3. validate adventures =="
adv=(examples/demo.yaml examples/thefog.yaml)
for f in examples/genres/*.yaml; do adv+=("$f"); done
for f in examples/modules/*.yaml; do adv+=("$f"); done
for f in "${adv[@]}"; do
    echo "-- validate $f"
    "${WORLDBUILDER[@]}" validate "$f"
done

echo "== 4. e2e playthroughs =="
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

for name in thefog pure-if fantasy cyberpunk space-opera detective horror factions trade encounters survival stealth combat-off combat-narrative combat-classic party starship combo; do
    case "$name" in
        thefog)             src=examples/thefog.yaml ;;
        factions|trade|encounters|survival|stealth|combat-off|combat-narrative|combat-classic|party|starship|combo)     src="examples/modules/$name.yaml" ;;
        *)                  src="examples/genres/$name.yaml" ;;
    esac
    "${WORLDBUILDER[@]}" compile "$src" -o "$tmp/$name" >/dev/null
    out="$("${GAME[@]}" --world "$tmp/$name/world.json" --save "$tmp/$name/save.json" \
            < "ci/e2e/$name.in" 2>&1 || true)"
    expect="$(cat "ci/e2e/$name.expect")"
    if grep -qF "$expect" <<<"$out"; then
        echo "OK   $name  (reached: $expect)"
    else
        echo "FAIL $name  (expected: $expect)"
        echo "---- last output ----"
        tail -20 <<<"$out"
        exit 1
    fi
done

echo
echo "All checks passed."
