#!/usr/bin/env python3
"""Generate a language-pack data module from lang/<code>.json (Phase 4.3).

The JSON file is the single source of truth for a language pack; the generated
Haskell module is what the engine actually compiles in (the runtime never reads
the JSON — see the WASM spike: the library runs without file access). The
generator embeds a sha256 of the JSON bytes in the generated header so
scripts/check-lang-pack.sh can detect a stale module.

Usage:
    python3 scripts/gen-lang-pack.py [lang/de.json ...]

Default: every lang/*.json. Output: src/Messages/Lang<Code>.hs.

JSON schema (all sections except "language" and "messages" are optional):
    {
      "language":     "de",                       -- must match the file name
      "messages":     { "<key>": "<template>" },  -- catalog overrides (non-empty)
      "terms":        { "<slot>.<value>": "<translated value>" },
      "verbs":        { "<canonical>": ["<alias>", ...] },
      "directions":   { "<canonical>": ["<alias>", ...] },
      "commands":     { "<canonical>": ["<alias>", ...] },
      "prepositions": { "<canonical>": ["<alias>", ...] }
    }

Determinism: entries keep the order of the JSON file (stable JSON -> stable
output). Values are emitted as UTF-8 Haskell string literals with \\, \", \n,
\t, \r escapes; everything else is written literally.
"""

import glob
import hashlib
import json
import os
import sys

ALIAS_SECTIONS = ("verbs", "directions", "commands", "prepositions")


def die(msg):
    print("gen-lang-pack: FAIL: " + msg, file=sys.stderr)
    sys.exit(1)


def check_string_map(pairs, where, require_nonempty=True):
    """Validate a {String: String} section; `pairs` is the raw pair list
    (duplicate JSON keys are visible here and rejected)."""
    seen = {}
    out = []
    for key, value in pairs:
        if not isinstance(key, str) or not key:
            die("%s: empty or non-string key" % where)
        if key in seen:
            die("%s: duplicate key '%s'" % (where, key))
        seen[key] = True
        if not isinstance(value, str):
            die("%s: '%s' must map to a string" % (where, key))
        if require_nonempty and not value:
            die("%s: '%s' must not be empty" % (where, key))
        out.append((key, value))
    return out


def check_alias_map(pairs, where):
    """Validate a {String: [String]} section; a bare string is accepted as a
    one-element alias list."""
    seen = {}
    out = []
    for key, value in pairs:
        if not isinstance(key, str) or not key:
            die("%s: empty or non-string key" % where)
        if key in seen:
            die("%s: duplicate key '%s'" % (where, key))
        seen[key] = True
        if isinstance(value, str):
            value = [value]
        if not isinstance(value, list) or not value:
            die("%s: '%s' must map to a non-empty list of strings" % (where, key))
        for alias in value:
            if not isinstance(alias, str) or not alias:
                die("%s: '%s' has an empty alias" % (where, key))
        out.append((key, value))
    return out


def hs_string(s):
    out = []
    for ch in s:
        if ch == "\\":
            out.append("\\\\")
        elif ch == '"':
            out.append('\\"')
        elif ch == "\n":
            out.append("\\n")
        elif ch == "\t":
            out.append("\\t")
        elif ch == "\r":
            out.append("\\r")
        elif ord(ch) < 0x20:
            out.append("\\%d\\&" % ord(ch))
        else:
            out.append(ch)
    return '"' + "".join(out) + '"'


def hs_list(items, render):
    """Render a Haskell list literal; `render` turns one entry into its
    parenthesised pair form."""
    if not items:
        return "      []"
    lines = ["      [ " + render(items[0])]
    for item in items[1:]:
        lines.append("      , " + render(item))
    lines.append("      ]")
    return "\n".join(lines)


def hs_pairs_strings(pairs):
    return hs_list(pairs, lambda kv: "(%s, %s)" % (hs_string(kv[0]), hs_string(kv[1])))


def hs_pairs_aliases(pairs):
    return hs_list(
        pairs,
        lambda kv: "(%s, [%s])" %
            (hs_string(kv[0]), ", ".join(hs_string(a) for a in kv[1])))


def gen_module(path):
    with open(path, "rb") as fh:
        raw = fh.read()
    digest = hashlib.sha256(raw).hexdigest()
    try:
        doc = json.loads(
            raw.decode("utf-8"),
            object_pairs_hook=lambda pairs: pairs)
    except ValueError as exc:
        die("%s: invalid JSON: %s" % (path, exc))

    sections = {}
    language = None
    for key, value in doc:
        if key in sections or key == "language" and language is not None:
            die("%s: duplicate top-level key '%s'" % (path, key))
        if key == "language":
            if not isinstance(value, str) or not value:
                die("%s: 'language' must be a non-empty string" % path)
            language = value
        else:
            sections[key] = value

    if language is None:
        die("%s: missing 'language'" % path)
    stem = os.path.splitext(os.path.basename(path))[0]
    if language != stem:
        die("%s: 'language' is '%s' but the file name says '%s'" % (path, language, stem))
    for key in sections:
        if key not in ("messages", "terms") + ALIAS_SECTIONS:
            die("%s: unknown top-level section '%s'" % (path, key))

    messages = check_string_map(sections.get("messages", []), path + ": messages")
    if not messages:
        die("%s: 'messages' must not be empty" % path)
    terms = check_string_map(sections.get("terms", []), path + ": terms")
    aliases = {name: check_alias_map(sections.get(name, []), path + ": " + name)
               for name in ALIAS_SECTIONS}

    mod_suffix = language[0].upper() + language[1:]
    module = "Messages.Lang" + mod_suffix
    out_path = os.path.join("src", "Messages", "Lang%s.hs" % mod_suffix)

    def pair_block(title, body):
        return "    , -- %s BEGIN\n%s\n      -- %s END" % (title, body, title)

    lines = [
        "-- | Generated from %s (sha256: %s) by scripts/gen-lang-pack.py."
        % (path, digest),
        "--   DO NOT EDIT by hand - rerun the generator after changing the JSON.",
        "--",
        "--   Language-pack data (Phase 4.3): message templates keyed like the",
        "--   engine catalog, term translations for enumerable argument values,",
        "--   and input alias tables (verbs / directions / commands /",
        "--   prepositions) mapping alias words to their canonical English token.",
        "module %s" % module,
        "    ( lang%s" % mod_suffix,
        "    ) where",
        "",
        "-- | The pack as a flat tuple:",
        "--   (language, messages, terms, verbs, directions, commands, prepositions).",
        "lang%s :: (String, [(String, String)], [(String, String)]," % mod_suffix,
        "           [(String, [String])], [(String, [String])], [(String, [String])], [(String, [String])])",
        "lang%s =" % mod_suffix,
        "    ( %s" % hs_string(language),
        pair_block("MESSAGES", hs_pairs_strings(messages)),
        pair_block("TERMS", hs_pairs_strings(terms)),
        pair_block("VERBS", hs_pairs_aliases(aliases["verbs"])),
        pair_block("DIRECTIONS", hs_pairs_aliases(aliases["directions"])),
        pair_block("COMMANDS", hs_pairs_aliases(aliases["commands"])),
        pair_block("PREPOSITIONS", hs_pairs_aliases(aliases["prepositions"])),
        "    )",
        "",
    ]

    os.makedirs(os.path.dirname(out_path), exist_ok=True)
    with open(out_path, "w", encoding="utf-8") as fh:
        fh.write("\n".join(lines))
    print("gen-lang-pack: wrote %s (%d messages, %d terms, %d alias entries)"
          % (out_path, len(messages), len(terms),
             sum(len(v) for v in aliases.values())))


def main(argv):
    paths = argv[1:] or sorted(glob.glob(os.path.join("lang", "*.json")))
    if not paths:
        die("no lang/*.json files found")
    for path in paths:
        gen_module(path)


if __name__ == "__main__":
    main(sys.argv)
