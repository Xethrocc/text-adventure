#!/usr/bin/env bash
# Build the Windows release ZIP (Phase W4).
#
#   packaging/build-release.sh <repo-root> <out.zip>
#
# Collects the four executables plus the author-facing material into a flat
# layout and wraps it in a zip. Intended for the windows-latest CI job, but it
# works on any machine where the four executables have already been built.
set -euo pipefail
cd "$(dirname "$0")/.."   # repo root, no matter where it is called from

repo_root="$(pwd -P)"
out="${1:?usage: packaging/build-release.sh <out.zip>  (executables must be built)}"

stage="$(mktemp -d)"
trap 'rm -rf "$stage"' EXIT
bundle="$stage/text-adventure"
mkdir -p "$bundle/bin"

# The four executables. cabal list-bin prints one path per invocation.
exes=(
    "$(cabal list-bin exe:text-adventure)"
    "$(cabal list-bin exe:worldbuilder)"
    "$(cabal list-bin exe:img2ascii)"
    "$(cabal list-bin exe:text2ascii)"
)
for exe in "${exes[@]}"; do
    cp "$exe" "$bundle/bin/"
done

# Author-facing material: flat at the top level of the bundle.
cp packaging/windows/START-HERE.txt "$bundle/"
cp packaging/windows/check.bat "$bundle/"
cp packaging/windows/play.bat "$bundle/"
cp -r examples "$bundle/examples"
mkdir -p "$bundle/adventures" "$bundle/worlds"
cp examples/demo.yaml "$bundle/adventures/demo.yaml"
cp docs/adventure-schema.md "$bundle/"

# Windows batch helpers must be CRLF (W1 marks *.bat accordingly; the copy here
# guarantees it even if a checkout step somewhere along the way did not).
unix2dos "$bundle/"*.bat 2>/dev/null || sed -i 's/$/\r/' "$bundle/"*.bat

( cd "$stage" && zip -r -q "$out" text-adventure )
echo "wrote $out"
unzip -l "$out" | tail -5
