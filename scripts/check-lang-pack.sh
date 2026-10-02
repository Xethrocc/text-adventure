#!/usr/bin/env bash
# Language-pack gate (Phase 4.3).
#
# lang/<code>.json is the single source of truth for a language pack; the
# generated module src/Messages/Lang<Code>.hs is what the engine actually
# compiles in (the runtime never reads the JSON). This gate catches:
#
#   1. a stale generated module — the JSON changed but
#      `python3 scripts/gen-lang-pack.py` was not rerun, so the engine would
#      silently ship old translations (checked via the sha256 the generator
#      embeds in the generated header)
#   2. an unknown message key   — a pack entry whose key is not in the engine
#      catalog (nothing would ever render it)
#   3. an incomplete pack       — catalog keys the pack does not translate
#      (they fall back to the English template; tolerated for partial packs,
#      fatal with --require-complete once a pack claims to be complete)
#
# POSIX shell + grep/sed/sha256sum only: scripts/ci.sh also runs on the
# Windows runner (Git Bash), which has no guaranteed python3. The generator is
# a developer tool and is not part of CI.
set -euo pipefail
cd "$(dirname "$0")/.."

require_complete=0
if [ "${1:-}" = "--require-complete" ]; then
    require_complete=1
fi

file_hash() {
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$1" | cut -d' ' -f1
    else
        shasum -a 256 "$1" | cut -d' ' -f1
    fi
}

catalog_keys="$(sed -n '/^catalogEntries =/,/^defaultCatalog/p' src/Messages.hs \
    | grep -oE '^[[:space:]]*[,(]?[[:space:]]*\("[^"]+"' \
    | grep -oE '"[^"]+"' | tr -d '"' | sort -u)"

shopt -s nullglob
packs=(lang/*.json)
if [ "${#packs[@]}" -eq 0 ]; then
    echo "FAIL: no lang/*.json language packs found"
    exit 1
fi

status=0
for json in "${packs[@]}"; do
    lang_code="$(basename "$json" .json)"
    suffix="$(printf '%s' "$lang_code" | cut -c1 | tr '[:lower:]' '[:upper:]')$(printf '%s' "$lang_code" | cut -c2-)"
    gen="src/Messages/Lang${suffix}.hs"

    if [ ! -f "$gen" ]; then
        echo "FAIL: $json has no generated module $gen (run python3 scripts/gen-lang-pack.py)"
        status=1
        continue
    fi

    want="$(file_hash "$json")"
    got="$(sed -n 's/.*sha256: \([0-9a-f]*\).*/\1/p' "$gen" | head -1)"
    if [ "$want" != "$got" ]; then
        echo "FAIL: $gen is stale — $json changed after generation (run python3 scripts/gen-lang-pack.py)"
        status=1
        continue
    fi

    pack_keys="$(sed -n '/MESSAGES BEGIN/,/MESSAGES END/p' "$gen" \
        | grep -oE '\("[a-z0-9_.]+",[[:space:]]*"' \
        | sed 's/^("//; s/",[[:space:]]*"//' | sort -u)"
    n_pack="$(printf '%s\n' "$pack_keys" | grep -c . || true)"

    unknown="$(comm -13 <(printf '%s\n' "$catalog_keys") <(printf '%s\n' "$pack_keys") | grep . || true)"
    missing="$(comm -23 <(printf '%s\n' "$catalog_keys") <(printf '%s\n' "$pack_keys") | grep . || true)"

    if [ -n "$unknown" ]; then
        echo "FAIL: $json translates keys the engine catalog does not know"
        printf '%s\n' "$unknown" | sed 's/^/  - /'
        status=1
    fi
    if [ -n "$missing" ]; then
        n_missing="$(printf '%s\n' "$missing" | grep -c . || true)"
        if [ "$require_complete" -eq 1 ]; then
            echo "FAIL: $json does not translate $n_missing catalog keys (--require-complete)"
            printf '%s\n' "$missing" | sed 's/^/  - /'
            status=1
        else
            echo "WARN: $json does not translate $n_missing of $(printf '%s\n' "$catalog_keys" | grep -c .) catalog keys (English fallback)"
        fi
    fi
    if [ "$status" -eq 0 ]; then
        echo "OK   language pack '$lang_code': $n_pack translated keys, no unknown keys"
    fi
done
exit "$status"