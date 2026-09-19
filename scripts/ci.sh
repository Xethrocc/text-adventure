#!/usr/bin/env bash
# Phase 6c: CI pipeline for the text-adventure engine.
#
#   1. build all packages
#   2. run the unit test suites (engine, worldbuilder, img2ascii, text2ascii)
#   3. validate every shipped adventure (demo, thefog, 6 genre + 14 module fixtures)
#   4. E2E: compile each fixture and drive it to a known ending
#
# Usage: scripts/ci.sh
set -euo pipefail
chcp.com 65001 >/dev/null 2>&1 || true

cd "$(dirname "$0")/.."

WORLDBUILDER=(cabal run -v0 worldbuilder --)
GAME=(cabal run -v0 text-adventure --)

echo "== 1. build =="
# Review P2-4/P2-5: warnings used to bury the real `-Wmissing-fields` findings,
# so the build log is now checked instead of eyeballed. `-Werror=name-shadowing`
# and `-Werror=missing-fields` (see the .cabal files) cover the specific classes.
build_log="$(mktemp)"
cabal build all 2>&1 | tee "$build_log"
if grep -qi "warning:" "$build_log"; then
    echo "FAIL: the build produced warnings (see above)"
    exit 1
fi
rm -f "$build_log"

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

# Compile one adventure and drive it through one input file, then check the
# expected marker. Shared by the happy-path and failure-path stages.
run_e2e() {
    local name="$1" src="$2" out expect
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
}

for name in thefog pure-if fantasy cyberpunk space-opera detective horror factions trade encounters survival stealth combat-off combat-narrative combat-classic combat-tactical party starship combo ship-duel banner-art hotspot; do
    case "$name" in
        thefog)             src=examples/thefog.yaml ;;
        factions|trade|encounters|survival|stealth|combat-off|combat-narrative|combat-classic|combat-tactical|party|starship|combo|ship-duel)     src="examples/modules/$name.yaml" ;;
        banner-art)         src="examples/fixtures/banner-art.yaml" ;;
        hotspot)            src="examples/fixtures/hotspot.yaml" ;;
        *)                  src="examples/genres/$name.yaml" ;;
    esac
    run_e2e "$name" "$src"
done

echo "== 5. e2e non-victory paths =="
# Review L12: stage 4 only covers one happy path per fixture. Each entry here
# drives the same compiled world into a failure path — no funds, refused attack,
# starvation, unknown station, invalid dialogue choice, and the ship loss
# (`hull_failure`, reachable in `starship-loss.yaml` but not in `starship.yaml`).
# `combat-tactical-defend` is not a failure but a *behaviour* path: it pins that
# `defend` actually prevents the enemy counter (via the `combat.action` text
# comparison), which no other stage would catch.
for name in trade-fail combat-off-fail survival-fail starship-fail starship-loss-fail combo-fail combat-tactical-fail ship-duel-fail combat-tactical-defend; do
    case "$name" in
        trade-fail)        src=examples/modules/trade.yaml ;;
        combat-off-fail)   src=examples/modules/combat-off.yaml ;;
        survival-fail)     src=examples/modules/survival.yaml ;;
        starship-fail)     src=examples/modules/starship.yaml ;;
        starship-loss-fail) src=examples/modules/starship-loss.yaml ;;
        combo-fail)        src=examples/modules/combo.yaml ;;
        combat-tactical-fail) src=examples/modules/combat-tactical.yaml ;;
        ship-duel-fail)    src=examples/modules/ship-duel.yaml ;;
        combat-tactical-defend) src=examples/modules/combat-tactical.yaml ;;
    esac
    run_e2e "$name" "$src"
done

echo
echo "All checks passed."
