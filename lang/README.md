# Sprachpakete (`lang/`)

Sprachpakete übersetzen die **Engine-Meldungen** (Message-Katalog, siehe
`docs/message-catalog.md`) und bringen **Eingabe-Aliase** mit. Inhalte
(Namen, Beschreibungen, `msg:`-Texte) schreibt der Autor in einer Sprache —
ein Sprachpaket ist kein mehrsprachiges Content-System.

## Format (`lang/<code>.json` = Quelle der Wahrheit)

```json
{
  "language": "de",
  "messages":     { "take.ok": "Du nimmst {article_acc} {item}.", "…": "…" },
  "terms":        { "dir.north": "Norden", "slot.head": "Kopf", "card_type.attack": "[Angriff]" },
  "verbs":        { "take": ["nimm"], "drop": ["lege"] },
  "directions":   { "north": ["nord", "norden"], "up": ["hoch", "oben"] },
  "commands":     { "inventory": ["inventar"], "hand": ["karten"] },
  "prepositions": { "from": ["aus", "von"], "to": ["an", "zu"], "about": ["nach", "von"] }
}
```

- **`messages`** — Übersetzung **aller** Katalog-Keys (Gate: Bijektion mit
  `catalogEntries` in `src/Messages.hs`; fehlende Keys fallen zur Laufzeit auf
  den englischen Text zurück, das Sync-Gate macht sie aber build-fatal).
  Platzhalter müssen zur englischen Vorlage passen — **ausgenommen** die
  Grammatik-Platzhalter (`{article_nom}`/`{article_acc}`/`{article_dat}`/
  `{gender}` und die `<slot>`-Varianten, z.B. `{item_article_acc}`): die
  englischen Vorlagen haben ihre Artikel fest eingeschrieben, die deutschen
  holen sie aus den `article:`-Feldern der Items/NPCs (siehe
  `docs/adventure-schema.md`, „Grammatikfelder").
- **`terms`** — Übersetzung von Aufzählungs-Werten (`slot.value`): `dir.*`
  (Richtungen), `slot.*` (Ausrüstungsplätze), `card_type.*` (Kartentypen).
- **`verbs`/`directions`/`commands`/`prepositions`** — zusätzliche
  Eingabe-Aliase auf die **kanonischen englischen Tokens**. Sie erweitern die
  englische Syntax, sie ersetzen sie nicht. Aktiv nur in Welten mit
  `language: <code>` (Verhaltensänderung 4.3.4). Präpositionen sind
  rollenbezogen (`from`/`to`/`about`/`on`/`in`) — bewusst keine globale
  Wort-Ersetzung.

## Werkzeuge

- **Generieren:** `python3 scripts/gen-lang-pack.py` erzeugt das eingebettete
  Datenmodul `src/Messages/Lang<Code>.hs` (Header mit sha256 — **nie von Hand
  bearbeiten**). Validiert unbekannte Keys und Platzhalter-Differenzen.
- **Gate:** `scripts/check-lang-pack.sh --require-complete` (CI-Stufe 2c):
  JSON↔Modul-Synchron, keine unbekannten Keys, Vollständigkeit.
- **Doku-Tabelle:** `python3 scripts/gen-msg-catalog.py` schreibt die
  `de`-Spalte nach `docs/message-catalog.md`.

## Neue Sprache anlegen

1. `lang/<code>.json` nach obigem Schema (Volltext inkl. `help.text`).
2. `python3 scripts/gen-lang-pack.py` laufen lassen.
3. In `src/Messages.hs` das generierte Modul importieren und den Tupel-Eintrag
   in `langPacks` ergänzen (`knownLanguages` leitet sich ab).
4. `bash scripts/check-lang-pack.sh --require-complete` muss grün sein.

Bekannte Sprachen: `en` (Default, kein Paket nötig) und `de`.
