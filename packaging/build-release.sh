#!/usr/bin/env bash
# Build the Windows release ZIP (Phase W4).
#
#   packaging/build-release.sh <out.zip>
#
# Collects the four executables plus the author-facing material into a flat
# layout and wraps it in a zip. Intended for the windows-latest CI job; it
# works on any machine where the four executables have already been built.
#
# Designed to survive the Git Bash environment on windows-latest: no unix2dos
# (not shipped there), no reliance on `zip` being on PATH (falls back to 7z,
# then to tar), and a dry-run mode for quick diagnosis:
#
#   packaging/build-release.sh --dry-run   # list the four exe paths and exit
set -euo pipefail
cd "$(dirname "$0")/.."   # repo root, no matter where it is called from

if [ "${1:-}" = "--dry-run" ]; then
    for exe in text-adventure worldbuilder img2ascii text2ascii text-adventure-tui; do
        printf "%s -> " "$exe"
        # the main CLI now lives in the text-adventure-cli package (Phase T
        # follow-up: --tui lives there); the other exes keep their package
        case "$exe" in
            text-adventure) cabal list-bin "text-adventure-cli:exe:text-adventure" 2>&1 | tail -1 ;;
            *)              cabal list-bin "exe:$exe" 2>&1 | tail -1 ;;
        esac
    done
    exit 0
fi

out="${1:?usage: packaging/build-release.sh <out.zip>  (executables must be built)}"

# Resolve the output path BEFORE anything chdirs elsewhere, and make sure the
# target directory exists. (A relative $out combined with the `cd "$stage"`
# further down sent the first CI attempts' zip into the staging directory while
# `ls` looked in the repo — the archive was written, the step still failed.)
stage="$(mktemp -d)"
trap 'rm -rf "$stage"' EXIT
mkdir -p "$(dirname "$out")"
out="$(realpath "$out")"   # absolute; survives the cd below
bundle="$stage/text-adventure"
mkdir -p "$bundle/bin"

# The four executables, one path per invocation. On Windows, `cabal list-bin`
# prints Windows paths with a CRLF line ending: the `$( )` capture strips the
# trailing LF but keeps the `\r`, so the path must be cleaned or `cp` fails
# with "No such file" (this cost one failed CI run — the step died within
# seconds, far too fast to reach the packaging itself). MSYS tools cope with
# `C:\…` only sometimes; cygpath turns it into a form cp always understands.
for exe in text-adventure worldbuilder img2ascii text2ascii; do
    path="$(cabal list-bin "exe:$exe" | tr -d '\r')"
    if [ -z "$path" ]; then
        echo "ERROR: cabal list-bin exe:$exe returned nothing" >&2
        exit 1
    fi
    if command -v cygpath >/dev/null 2>&1; then
        path="$(cygpath -u "$path")"
    fi
    if [ ! -f "$path" ]; then
        echo "ERROR: exe:$exe not built or not found at: $path" >&2
        exit 1
    fi
    echo "bundling exe:$exe"
    cp "$path" "$bundle/bin/"
done

# Author-facing material: flat at the top level of the bundle.
cp packaging/windows/START-HERE.txt "$bundle/"
cp packaging/windows/WRITING-ADVENTURES.txt "$bundle/"
cp packaging/windows/check.bat "$bundle/"
cp packaging/windows/play.bat "$bundle/"
cp -r examples "$bundle/examples"
mkdir -p "$bundle/adventures" "$bundle/worlds"
cp examples/demo.yaml "$bundle/adventures/demo.yaml"
cp docs/adventure-schema.md "$bundle/"

# Windows batch helpers must be CRLF. Git Bash on windows-latest does not ship
# unix2dos, and a plain `sed -i` adds \r only where missing — so do it with an
# explicit marker check instead: convert only files that still have a bare LF.
for f in "$bundle/"*.bat; do
    if ! grep -q $'\r' "$f"; then
        sed -i 's/$/\r/' "$f"
    fi
done

# Bundle it. `zip` is not guaranteed in Git Bash; 7z is present on windows-latest;
# tar (bsdtar) handles zip by extension as a last resort. Guarded so a missing
# tool fails the step with its own message instead of a cryptic exit code.
cd "$stage"
if command -v zip >/dev/null 2>&1; then
    zip -r -q "$out" text-adventure
elif command -v 7z >/dev/null 2>&1; then
    7z a -tzip -bso0 "$out" text-adventure
else
    # bsdtar writes a real zip when the extension says so.
    tar -caf "$out" text-adventure
fi
cd - >/dev/null

echo "wrote $out"
ls -l "$out"
