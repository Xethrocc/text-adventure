#!/usr/bin/env python3
"""Regenerate the catalog table in docs/message-catalog.md from src/Messages.hs.

The catalog is the single source of truth; the doc's table is derived, never
hand-edited. Rewrites

    <!-- BEGIN GENERATED ... -->
    ...table...
    <!-- END GENERATED -->

and keeps the documented size in the header line in sync.

Only generation needs python3 (a manual step); the CI gate
`scripts/check-msg-catalog.sh` stays POSIX shell, since a CI run also happens on
the Windows runner (Git Bash, no guaranteed python3).
"""
import re
import sys

ROOT = __file__.rsplit("/", 2)[0]
SRC = f"{ROOT}/src/Messages.hs"
DOC = f"{ROOT}/docs/message-catalog.md"

src = open(SRC, encoding="utf-8").read()
block = src.split("catalogEntries =", 1)[1].split("\ndefaultCatalog", 1)[0]


def resolve_var(name):
    """Resolve a template held in a top-level binding (e.g. helpTemplate).

    Line-based on purpose: the `intercalate "\\n" [ … ]` form is easy to get
    wrong with a regex (the escape inside the Haskell literal), and a silent
    miss here would write a wrong template into the doc."""
    lines = src.splitlines()
    start = None
    for i, l in enumerate(lines):
        if l.startswith(f"{name} :: "):
            start = i
            break
    if start is None:
        return None
    parts, collecting = [], False
    for l in lines[start:start + 200]:
        if not collecting:
            if not l.startswith(f"{name} ="):
                continue
            collecting = True
            if f"intercalate" not in l:
                m = re.search(r'"((?:[^"\\]|\\.)*)"', l)
                return m.group(1) if m else None
            continue
        if l.strip().startswith("]"):
            break
        parts.append(l)
    if not parts:
        return None
    return "\\n".join(re.findall(r'"((?:[^"\\]|\\.)*)"', "\n".join(parts)))


entries = []
for m in re.finditer(r'^[ \t]*[,(]?[ \t]*\("([^"]+)"[ \t]*,[ \t]*(?:"((?:[^"\\]|\\.)*)"|([a-zA-Z][\w\']*))\s*[),]',
                     block, re.M):
    key, literal, var = m.group(1), m.group(2), m.group(3)
    tmpl = literal if literal is not None else resolve_var(var)
    if tmpl is None:
        sys.exit(f"cannot resolve template for key {key!r} (binding {var!r})")
    entries.append((key, tmpl))

dupes = {k for k, _ in entries if [x for x, _ in entries].count(k) > 1}
if dupes:
    sys.exit(f"duplicate catalog keys: {sorted(dupes)}")

LIMIT = 120


def cell(t):
    """Keep the table readable: very long templates (help.text) would otherwise
    blow up a single row. Truncation is marked, the source stays the truth."""
    t = t.replace("|", "\\|")
    return t if len(t) <= LIMIT else t[:LIMIT - 1] + "…"


rows = "\n".join(
    f"| {i} | `{k}` | `{cell(t)}` |"
    for i, (k, t) in enumerate(sorted(entries), start=1))

doc = open(DOC, encoding="utf-8").read()
pattern = re.compile(r"(<!-- BEGIN GENERATED.*?-->)(.*?)(<!-- END GENERATED -->)", re.S)
if not pattern.search(doc):
    sys.exit("generated markers not found in docs/message-catalog.md")

table = f"\n| # | MsgId | Template |\n|---|---|---|\n{rows}\n\n"
doc = pattern.sub(lambda m: m.group(1) + table + m.group(3), doc)
doc = re.sub(r"(\*\*Kataloggröße: )\d+( Keys\*\*)", rf"\g<1>{len(entries)}\g<2>", doc)
open(DOC, "w", encoding="utf-8").write(doc)
print(f"docs/message-catalog.md: {len(entries)} Keys geschrieben")
