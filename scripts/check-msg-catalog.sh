#!/usr/bin/env bash
# Message-catalog gate (Phase 1.1 follow-up).
#
# The catalog in src/Messages.hs is the single source of truth for player-facing
# engine text. Two failure modes are invisible at runtime and in the existing
# unit tests, so this gate catches both:
#
#   1. an unused key  — dead weight in the catalog, never rendered anywhere
#   2. an unknown key — a call site whose key is not in the catalog (renders
#                       the loud "<msg:key>" fallback instead of a message)
#
# It compares the catalog against the keys that production code passes as string
# literals to renderMsg/msgPayload/evMsg. Only src/ is scanned (tests may use
# deliberate dummy keys such as "no.such.key").
#
# POSIX shell + grep/sed only: scripts/ci.sh also runs on the Windows runner
# (Git Bash), which has no guaranteed python3.
set -euo pipefail
cd "$(dirname "$0")/.."

catalog_keys="$(sed -n '/^catalogEntries =/,/^defaultCatalog/p' src/Messages.hs \
    | grep -oE '^[[:space:]]*[,(]?[[:space:]]*\("[^"]+"' \
    | grep -oE '"[^"]+"' | tr -d '"' | sort -u)"
used_keys="$(grep -rhoE '\b(renderMsg|msgPayload|evMsg)[[:space:]]+"[^"]+"' src/ \
    --include='*.hs' --exclude='Messages.hs' \
    | grep -oE '"[^"]+"' | tr -d '"' | sort -u)"

n_catalog="$(printf '%s\n' "$catalog_keys" | grep -c . || true)"
n_used="$(printf '%s\n' "$used_keys" | grep -c . || true)"

unused="$(comm -23 <(printf '%s\n' "$catalog_keys") <(printf '%s\n' "$used_keys") | grep . || true)"
unknown="$(comm -13 <(printf '%s\n' "$catalog_keys") <(printf '%s\n' "$used_keys") | grep . || true)"

status=0
if [ -n "$unknown" ]; then
    echo "FAIL: keys used in src/ but missing from the catalog (render as '<msg:key>')"
    printf '%s\n' "$unknown" | sed 's/^/  - /'
    status=1
fi
if [ -n "$unused" ]; then
    echo "FAIL: catalog keys without any call site in src/ (dead catalog entries)"
    printf '%s\n' "$unused" | sed 's/^/  - /'
    status=1
fi
if [ "$status" -eq 0 ]; then
    echo "OK   message catalog: $n_catalog keys, all referenced from src/ ($n_used call-site keys)"
fi
exit "$status"
