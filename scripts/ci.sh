#!/usr/bin/env bash
# Phase 6c: CI pipeline for the text-adventure engine.
#
#   1. build all packages
#   2. run the unit test suites (engine, worldbuilder, img2ascii, text2ascii,
#      video2ascii)
#   3. validate every shipped adventure (demo, thefog, 6 genre + 14 module fixtures)
#   4. E2E: compile each fixture and drive it to a known ending
#
# Usage: scripts/ci.sh
set -euo pipefail
chcp.com 65001 >/dev/null 2>&1 || true

cd "$(dirname "$0")/.."

WORLDBUILDER=(cabal run -v0 worldbuilder --)
GAME=(cabal run -v0 text-adventure-cli --)

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
# The warnings gate above only sees libraries and executables: `cabal build all`
# does not build test components, they are compiled here. So check this log too —
# with the compiler's own marker (`warning: [-W…]`), so the runtime message
# "Warning: This save was made with a different world version" is not mistaken
# for a compiler warning. `set -e`/`pipefail` already abort on a failing suite.
test_log="$(mktemp)"
cabal test all 2>&1 | tee "$test_log"
if grep -q 'warning: \[-W' "$test_log"; then
    echo "FAIL: a test component produced compiler warnings (see above)"
    exit 1
fi
rm -f "$test_log"

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
    # Rogue Phase 0: hermetic saves — every run gets its own TA_SAVES_DIR so
    # save/load commands cannot leak between runs or pollute the repo.
    mkdir -p "$tmp/$name-saves"
    "${WORLDBUILDER[@]}" compile "$src" -o "$tmp/$name" >/dev/null
    out="$(TA_SAVES_DIR="$tmp/$name-saves" "${GAME[@]}" --world "$tmp/$name/world.json" --save "$tmp/$name/save.json" \
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

for name in thefog pure-if fantasy cyberpunk space-opera detective horror factions trade encounters survival stealth patrol combat-off combat-narrative combat-classic combat-tactical party starship combo ship-duel banner-art hotspot ascii-state combat-screen; do
    case "$name" in
        thefog)             src=examples/thefog.yaml ;;
        factions|trade|encounters|survival|stealth|patrol|combat-off|combat-narrative|combat-classic|combat-tactical|party|starship|combo|ship-duel)     src="examples/modules/$name.yaml" ;;
        banner-art)         src="examples/fixtures/banner-art.yaml" ;;
        hotspot)            src="examples/fixtures/hotspot.yaml" ;;
        ascii-state)        src="examples/fixtures/ascii-state.yaml" ;;
        combat-screen)      src="examples/fixtures/combat-screen.yaml" ;;
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
# `patrol-attack` and `patrol-death` are not failures either but *behaviour*
# paths: they pin that a patrolling NPC actually closes in, warns, bites — and
# that its damage can kill (turn-consuming commands only; `look` is free).
for name in trade-fail combat-off-fail survival-fail starship-fail starship-loss-fail combo-fail combat-tactical-fail ship-duel-fail combat-tactical-defend patrol-attack patrol-death combat-screen-round; do
    case "$name" in
        trade-fail)        src=examples/modules/trade.yaml ;;
        combat-off-fail)   src=examples/modules/combat-off.yaml ;;
        patrol-attack|patrol-death) src=examples/modules/patrol.yaml ;;
        combat-screen-round) src=examples/fixtures/combat-screen.yaml ;;
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

# ---------------------------------------------------------------------------
# Rogue Phase 4: pre-run world generator (detail plan section 6, test 5).
# Determinism check (same seed => byte-identical world.json) plus a scripted
# playthrough of the generated dungeon up to the boss kill — with a fixed
# seed and the template in the repo, the path and markers are stable.
# ---------------------------------------------------------------------------
echo "== 6. worldgen (Rogue Phase 4) =="
wg_src="examples/templates/dungeon_template.yaml"
"${WORLDBUILDER[@]}" generate "$wg_src" --seed 42 -o "$tmp/worldgen-a" >/dev/null
"${WORLDBUILDER[@]}" generate "$wg_src" --seed 42 -o "$tmp/worldgen-b" >/dev/null
if cmp -s "$tmp/worldgen-a/world.json" "$tmp/worldgen-b/world.json" \
   && cmp -s "$tmp/worldgen-a/save.json" "$tmp/worldgen-b/save.json"; then
    echo "OK   worldgen-determinism  (same seed, byte-identical output)"
else
    echo "FAIL worldgen-determinism  (same seed, output differs)"
    exit 1
fi
mkdir -p "$tmp/worldgen-saves"
out="$(TA_SAVES_DIR="$tmp/worldgen-saves" "${GAME[@]}" \
        --world "$tmp/worldgen-a/world.json" --save "$tmp/worldgen-a/save.json" \
        < "ci/e2e/worldgen.in" 2>&1 || true)"
while IFS= read -r marker; do
    if grep -qF "$marker" <<<"$out"; then
        echo "OK   worldgen  (reached: $marker)"
    else
        echo "FAIL worldgen  (expected: $marker)"
        echo "---- last output ----"
        tail -20 <<<"$out"
        exit 1
    fi
done < "ci/e2e/worldgen.expect"

echo
echo "All checks passed."
