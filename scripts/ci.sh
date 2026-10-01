#!/usr/bin/env bash
# Phase 6c: CI pipeline for the text-adventure engine.
#
#   1. build all packages
#   2. run the unit test suites (engine, worldbuilder, img2ascii, text2ascii,
#      video2ascii)
#   2b. message catalog gate (no unused key, no key missing from the catalog)
#   3. validate every shipped adventure (demo, thefog, 6 genre + 14 module fixtures)
#   4. E2E: compile each fixture and drive it to a known ending
#   8. save/load round-trip (the persistence wiring no .in fixture exercises)
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
# with the compiler's own marker (`warning: [-W…]` on older GHC, `warning: [GHC-…]`
# from GHC 9.6 on), so the runtime message
# "Warning: This save was made with a different world version" is not mistaken
# for a compiler warning. `set -e`/`pipefail` already abort on a failing suite.
test_log="$(mktemp)"
cabal test all 2>&1 | tee "$test_log"
if grep -qE 'warning: \[(-W|GHC-)' "$test_log"; then
    echo "FAIL: a test component produced compiler warnings (see above)"
    exit 1
fi
rm -f "$test_log"

echo "== 2b. message catalog gate =="
# Phase 1.1 follow-up: the catalog is the single source of truth for player-facing
# engine text, but two failure modes are invisible to the unit tests — a key that
# no call site renders (dead catalog entry) and a call site whose key is missing
# from the catalog (renders the loud '<msg:key>' fallback instead of a message).
# POSIX shell only: this script also runs on the Windows runner (Git Bash).
./scripts/check-msg-catalog.sh

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
    local name="$1" src="$2" out marker failed=0
    # Rogue Phase 0: hermetic saves — every run gets its own TA_SAVES_DIR so
    # save/load commands cannot leak between runs or pollute the repo.
    mkdir -p "$tmp/$name-saves"
    "${WORLDBUILDER[@]}" compile "$src" -o "$tmp/$name" >/dev/null
    out="$(TA_SAVES_DIR="$tmp/$name-saves" "${GAME[@]}" --world "$tmp/$name/world.json" --save "$tmp/$name/save.json" \
            < "ci/e2e/$name.in" 2>&1 || true)"
    # One marker per line; every single one has to appear. A one-line .expect
    # behaves exactly as before, so the older cases stay untouched.
    # The `|| [ -n "$marker" ]` catches a final line without trailing newline:
    # plain `while read` silently DROPS it — which left 16 of 50 .expect files
    # unchecked (14 of them had only that one marker). Fixed 2026-09-29 (B1).
    while IFS= read -r marker || [ -n "$marker" ]; do
        [ -n "$marker" ] || continue
        if grep -qF "$marker" <<<"$out"; then
            echo "OK   $name  (reached: $marker)"
        else
            echo "FAIL $name  (expected: $marker)"
            echo "---- last output ----"
            tail -20 <<<"$out"
            failed=1
        fi
    done < "ci/e2e/$name.expect"
    [ "$failed" -eq 0 ] || exit 1
}

for name in thefog pure-if fantasy cyberpunk space-opera detective horror economy_hamurabi deckbuilder_spire sandbox_wilderness factions trade encounters survival stealth patrol combat-off combat-narrative combat-classic combat-tactical party starship combo ship-duel banner-art hotspot ascii-state combat-screen dark-feelable bomb waechter traglast disambiguation procedures gespraeche npc-besitz stroeme; do
    case "$name" in
        thefog)             src=examples/thefog.yaml ;;
        factions|trade|encounters|survival|stealth|patrol|combat-off|combat-narrative|combat-classic|combat-tactical|party|starship|combo|ship-duel)     src="examples/modules/$name.yaml" ;;
        banner-art)         src="examples/fixtures/banner-art.yaml" ;;
        dark-feelable)     src="examples/fixtures/dark-feelable.yaml" ;;
        hotspot)            src="examples/fixtures/hotspot.yaml" ;;
        ascii-state)        src="examples/fixtures/ascii-state.yaml" ;;
        combat-screen)      src="examples/fixtures/combat-screen.yaml" ;;
        bomb)               src="examples/fixtures/bomb.yaml" ;;
        waechter)           src="examples/fixtures/waechter.yaml" ;;
        traglast)           src="examples/fixtures/traglast.yaml" ;;
        disambiguation)     src="examples/fixtures/disambiguation.yaml" ;;
        procedures)         src="examples/fixtures/procedures.yaml" ;;
        gespraeche)         src="examples/fixtures/gespraeche.yaml" ;;
        npc-besitz)         src="examples/fixtures/npc-besitz.yaml" ;;
        stroeme)            src="examples/fixtures/stroeme.yaml" ;;
        *)                  src="examples/genres/$name.yaml" ;;
    esac
    run_e2e "$name" "$src"
done

echo "== 4b. author content tests (B1) =="
# Content tests as data: the `tests:` section of an adventure runs through
# `worldbuilder test` (ordered markers). Extend the list as adventures grow
# their own tests.
for name in procedures wissen kapitel krypta quest_rpg pursuit include_demo mengen massen behaelter stroeme; do
    case "$name" in
        procedures) src="examples/fixtures/procedures.yaml" ;;
        wissen)     src="examples/fixtures/wissen.yaml" ;;
        kapitel)    src="examples/fixtures/kapitel.yaml" ;;
        krypta)     src="examples/fixtures/krypta.yaml" ;;
        quest_rpg)  src="examples/fixtures/quest_rpg.yaml" ;;
        pursuit)    src="examples/fixtures/pursuit.yaml" ;;
        include_demo) src="examples/fixtures/include_demo.yaml" ;;
        mengen)     src="examples/fixtures/mengen.yaml" ;;
        massen)     src="examples/fixtures/massen.yaml" ;;
        behaelter)  src="examples/fixtures/behaelter.yaml" ;;
        stroeme)    src="examples/fixtures/stroeme.yaml" ;;
    esac
    "${WORLDBUILDER[@]}" test "$src" || exit 1
done

echo "== 4c. content fuzzer (B5) =="
# Deterministic fuzz runs (fixed seed) over every shipped adventure and
# fixture: engine crashes, non-terminating steps and frozen loops (veto
# soft-locks: 25 steps without any state/turn progress) fail the gate. Every
# finding prints its exact command sequence and the --replay recipe.
for f in "${adv[@]}" examples/fixtures/*.yaml; do
    case "$f" in
        *include_lib*) continue ;;   # include library, not a main adventure
    esac
    echo "-- fuzz $f"
    "${WORLDBUILDER[@]}" fuzz "$f" --seed 42 --runs 10 --steps 120
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
for name in trade-fail combat-off-fail survival-fail starship-fail starship-loss-fail combo-fail combat-tactical-fail ship-duel-fail combat-tactical-defend patrol-attack patrol-death combat-screen-round disambiguation-fallback; do
    case "$name" in
        trade-fail)        src=examples/modules/trade.yaml ;;
        disambiguation-fallback) src="examples/fixtures/disambiguation.yaml" ;;
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
while IFS= read -r marker || [ -n "$marker" ]; do
    if grep -qF "$marker" <<<"$out"; then
        echo "OK   worldgen  (reached: $marker)"
    else
        echo "FAIL worldgen  (expected: $marker)"
        echo "---- last output ----"
        tail -20 <<<"$out"
        exit 1
    fi
done < "ci/e2e/worldgen.expect"

# ---------------------------------------------------------------------------
# Rogue Phase 4c: run-regeneration E2E (detail plan section 11, test 4).
# Two consecutive runs on the same template: run 1 generates run_1, advances
# meta.runs from 0 to 1; run 2 generates run_2 with a different seed/world,
# and advances meta.runs to 2.
# ---------------------------------------------------------------------------
echo "== 7. run-regeneration (Rogue Phase 4c) =="
run_saves="$tmp/run-saves"
mkdir -p "$run_saves"
"${WORLDBUILDER[@]}" run "$wg_src" --saves-dir "$run_saves" --no-launch >/dev/null
if [ -f "$run_saves/katakomben_von_vhal/run_1/world.json" ]; then
    echo "OK   run-1-preparation (run_1 directory created)"
else
    echo "FAIL run-1-preparation (run_1 directory missing)"
    exit 1
fi
TA_META_DIR="$run_saves" TA_SAVES_DIR="$run_saves/katakomben_von_vhal/run_1" "${GAME[@]}" \
    --world "$run_saves/katakomben_von_vhal/run_1/world.json" \
    --save "$run_saves/katakomben_von_vhal/run_1/save.json" \
    --saves-dir "$run_saves/katakomben_von_vhal/run_1" --no-color <<< "quit" >/dev/null 2>&1 || true

if grep -q '"contents": 1' "$run_saves/katakomben_von_vhal_meta.json" 2>/dev/null; then
    echo "OK   run-1-meta (meta.runs is 1 after run 1)"
else
    echo "FAIL run-1-meta (meta.runs != 1 after run 1)"
    exit 1
fi

"${WORLDBUILDER[@]}" run "$wg_src" --saves-dir "$run_saves" --no-launch >/dev/null
if [ -f "$run_saves/katakomben_von_vhal/run_2/world.json" ]; then
    echo "OK   run-2-preparation (run_2 directory created)"
else
    echo "FAIL run-2-preparation (run_2 directory missing)"
    exit 1
fi

if cmp -s "$run_saves/katakomben_von_vhal/run_1/world.json" "$run_saves/katakomben_von_vhal/run_2/world.json"; then
    echo "FAIL run-divergence (run 1 and run 2 have identical worlds)"
    exit 1
else
    echo "OK   run-divergence (run 1 and run 2 have distinct worlds)"
fi

TA_META_DIR="$run_saves" TA_SAVES_DIR="$run_saves/katakomben_von_vhal/run_2" "${GAME[@]}" \
    --world "$run_saves/katakomben_von_vhal/run_2/world.json" \
    --save "$run_saves/katakomben_von_vhal/run_2/save.json" \
    --saves-dir "$run_saves/katakomben_von_vhal/run_2" --no-color <<< "quit" >/dev/null 2>&1 || true

if grep -q '"contents": 2' "$run_saves/katakomben_von_vhal_meta.json" 2>/dev/null; then
    echo "OK   run-2-meta (meta.runs is 2 after run 2)"
else
    echo "FAIL run-2-meta (meta.runs != 2 after run 2)"
    exit 1
fi

echo "== 8. save/load round-trip =="
# Coverage gap (found in the Phase 1.3 review): no stage-4/5 fixture ever issues
# `save` or `load`, so the persistence wiring — loopGame's save/load branch and
# the death menu's load branch — had no end-to-end coverage at all. Each case
# drives it in two runs against one isolated saves dir: run 1 writes the slot,
# run 2 loads it and has to report the load.
save_load_case() {
    local name="$1" src="$2" saves="$tmp/$1-saves" run_dir="$tmp/$1" out marker failed=0
    mkdir -p "$saves" "$run_dir"
    "${WORLDBUILDER[@]}" compile "$src" -o "$run_dir" >/dev/null
    TA_SAVES_DIR="$saves" "${GAME[@]}" \
        --world "$run_dir/world.json" --save "$run_dir/save.json" \
        < "ci/e2e/$name-write.in" >/dev/null 2>&1 || true
    if [ -f "$saves/myslot.json" ]; then
        echo "OK   $name-write (myslot.json written)"
    else
        echo "FAIL $name-write (no myslot.json in $saves)"
        exit 1
    fi
    out="$(TA_SAVES_DIR="$saves" "${GAME[@]}" \
            --world "$run_dir/world.json" --save "$run_dir/save.json" \
            < "ci/e2e/$name-read.in" 2>&1 || true)"
    while IFS= read -r marker || [ -n "$marker" ]; do
        [ -n "$marker" ] || continue
        if grep -qF "$marker" <<<"$out"; then
            echo "OK   $name-read (reached: $marker)"
        else
            echo "FAIL $name-read (expected: $marker)"
            echo "---- last output ----"
            tail -20 <<<"$out"
            failed=1
        fi
    done < "ci/e2e/$name.expect"
    [ "$failed" -eq 0 ] || exit 1
}

save_load_case save-load examples/thefog.yaml
save_load_case save-load-death examples/modules/patrol.yaml

# ---------------------------------------------------------------------------
# B6: game export — a finished game leaves the repo as one playable bundle:
# world/save (byte-identical to `compile`), the referenced assets and both
# launchers. The bundle is then played through its own play.sh.
# ---------------------------------------------------------------------------
echo "== 9. export bundle (B6) =="
eng="$(cabal list-bin "text-adventure-cli:exe:text-adventure" 2>/dev/null)"
PATH="$PATH:$(dirname "$eng")" "${WORLDBUILDER[@]}" export examples/fixtures/buendel.yaml \
    -o "$tmp/buendel-bundle" --with-engine > "$tmp/export.log" 2>&1
for f in world.json save.json play.sh play.bat bin/text-adventure \
         assets/theme.xm assets/ton.wav assets/README-spiel.txt; do
    if [ -f "$tmp/buendel-bundle/$f" ]; then
        echo "OK   export-file ($f)"
    else
        echo "FAIL export-file ($f missing)"
        cat "$tmp/export.log"
        exit 1
    fi
done
"${WORLDBUILDER[@]}" compile examples/fixtures/buendel.yaml -o "$tmp/buendel-plain" >/dev/null
if cmp -s "$tmp/buendel-bundle/world.json" "$tmp/buendel-plain/world.json" \
   && cmp -s "$tmp/buendel-bundle/save.json" "$tmp/buendel-plain/save.json"; then
    echo "OK   export-bytes (bundle world/save byte-identical to compile)"
else
    echo "FAIL export-bytes (bundle world/save differ from compile)"
    exit 1
fi
mkdir -p "$tmp/buendel-saves"
out="$(TA_SAVES_DIR="$tmp/buendel-saves" bash "$tmp/buendel-bundle/play.sh" \
        < ci/e2e/buendel.in 2>&1 || true)"
export_failed=0
while IFS= read -r marker || [ -n "$marker" ]; do
    [ -n "$marker" ] || continue
    if grep -qF "$marker" <<<"$out"; then
        echo "OK   export-play (reached: $marker)"
    else
        echo "FAIL export-play (expected: $marker)"
        echo "---- last output ----"
        tail -20 <<<"$out"
        export_failed=1
    fi
done < "ci/e2e/buendel.expect"
[ "$export_failed" -eq 0 ] || exit 1
if command -v zip >/dev/null 2>&1 || command -v 7z >/dev/null 2>&1; then
    "${WORLDBUILDER[@]}" export examples/fixtures/buendel.yaml -o "$tmp/buendel-zip" --zip >/dev/null 2>&1
    if [ -f "$tmp/buendel-zip.zip" ]; then
        echo "OK   export-zip (archive written next to the bundle)"
    else
        echo "FAIL export-zip (no archive written)"
        exit 1
    fi
else
    echo "OK   export-zip (skipped: no zip tool available)"
fi

echo
echo "All checks passed."
