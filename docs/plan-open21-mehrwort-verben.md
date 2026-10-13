# Plan — OPEN-21: Mehrwort-Verben & Präpositionalphrasen (Parser)

**Stand:** 2026-10-13 · **Zweck:** Engine-Ticket für `OPEN-21` (befunde
`befunde-text-adventure-engine.md` §7) — Parser-Unterstützung für Mehrwort-Verben
(`put out`, `blow out`) und Präpositionalphrasen (`fill <x> with <y>`,
`put putty on leak`). Abnahmekriterium ist der Zork-I-Port C1
(`plan-zork-mechanik-entscheidungen.md`).

**Verwandte Dokumente:**
- `befunde-text-adventure-engine.md` — §7 `OPEN-21`, Befunde `P-02`/`Z-06` (verb_map)
- `plan-zork-mechanik-entscheidungen.md` — Zeile C1 („Ein-Wort-Verben")
- `../examples/zork1.yaml` — Abnahme-Abenteuer
- `adventure-schema.md` — Doku-Ziel (*Parametrisierte Befehle*, `verbs:`)

**Status:** 📋 geplant · **Aufwandsschätzung:** A+B je ~1 Tag, C+D ~½ Tag, E ~½ Tag

---

## 1. Ziel / Nicht-Ziel

**Ziel:**
1. Mehrwort-Verben sind deklarierbar (`- name: put out`) und matchbar — als
   Phrase, bevor das einzelne Wort (`put` → `drop`) zieht.
2. Präpositionalphrasen `V X PREP Y` sind für **alle** Verben ausdrückbar:
   X = Ziel, Y = Werkzeug/Empfänger, mit sauberer Rollen-Bindung
   (`cmd.target` = X, `cmd.tool` = Y).
3. Der Zork-Port kann C1 originalgetreu abbilden: `put out`, `blow out`,
   `fill X with Y`, `put putty on leak`.

**Nicht-Ziel:**
- Keine echte Zork-Grammatik (OBJ-SPEC-Parser, `ONBIT`-Grammatikflags usw.).
- Keine Änderung an Kern-Befehlen außer `put X on Y` (Arbeitspaket B4).
- Kein neuer `verb_map`-Phasen-Mechanismus; die bestehenden Phasen
  (`before:`/`instead:`/`after:`) + Zustands-Keys bleiben wie sie sind.

**Verträge (unverletzt):**
- **Byte-Vertrag:** bestehende `world.json`-Dateien bleiben byte-identisch;
  neue Felder kodieren nur bei Benutzung (Repo-Konvention, wie `tags_when`/
  `locations:`).
- **Rückwärtskompatibilität:** alle heute parsenden Eingaben behalten ihr
  Command-Ergebnis; `use X on Y` (`VUseOn`) bleibt funktional identisch;
  `look at`, `pick up`, `talk to`, `put down`, `take X from Y`, `put X in Y`
  bleiben unverändert (607 Engine-Tests als Regressionstest).

---

## 2. Ist-Zustand (verifiziert 2026-10-13)

Gegen das kompilierte `zork1.yaml` (`parseCommandFor`):

| Eingabe | heute | Bewertung |
|---|---|---|
| `put out candle` | `Interact VDrop "out candle"` | ❌ falsch („drop" frisst die Phrase) |
| `put putty on leak` | `Interact VDrop "putty on leak"` | ❌ falsch |
| `blow out candles` | `Unknown` | ❌ kein Phrase-Verb |
| `fill bottle with water` | `ActionWithArgs (VCustom "fill") ["bottle","with","water"]` | ⚠️ parse ok; Werkzeug-Y (`water`) wird nicht aufgelöst/geprüft (nur `cmd.argN`) |
| `turn off torch` | `ActionWithArgs (VCustom "turn") ["off","torch"]` | ⚠️ läuft nur über Port-Workaround `turn-off-alias` |
| `put gem in box` | `PutInCmd "gem" "box"` | ✅ (bleibt) |

### Wurzelursachen im Code

1. **`resolveVerb` matcht nur ein Wort** (`src/Verbs.hs`): Lookup über
   `verbAliasMap` mit einem Token; `VerbDef`-`name`/`aliases` sind implizit
   Einzelwörter. Mehrwort-Phrasen gibt es nur als Language-Pack-Aliase
   (`canonicalHead`, `src/Parser.hs` — `aeHead :: Map [String] [String]`).
2. **`put` ist Core-Alias von `drop`** (`coreVerbDefs`):
   `VerbDef "drop" ["put"]` — der generische Zweig
   (`v : targetParts | parseVerbWith defs v`) zieht `put out …` als Drop.
3. **Präpositionalphrasen sind hartcodiert**: nur `use X on Y`
   (`parseUse` → `InteractWith VUseOn`) und die Container-Formen
   (`take X from Y`, `put X in Y`, `give X to Y`) sind spezialisiert.
   Custom-Verben landen als `ActionWithArgs` mit rohen Tokens.
4. **`InteractWith` hat keinen generischen Dispatch**:
   `dispatchCommandCoreEv (InteractWith _ _ _) = evMsg "use.nothing" []`
   (`src/Parser.hs`, ~Zeile 1739) — nur `VUseOn` wird behandelt.
5. **`put X on Y` hat keinen Zweig** (nur `in` → `PutInCmd`, `down` → `VDrop`).
6. OPEN-08 löst bei `ActionWithArgs` das *primäre* Ziel über „längste
   Nomenphrase vor `with`/`to`" auf (`verb_map`-Lookup) — die Präposition
   dient nur als Trennung; das Werkzeug Y ist `cmd.argN`, ohne Auflösung.

---

## 3. Arbeitspakete

### A — Phrase-Verben (Engine)

1. **`verbs:` erlaubt Mehrwort-Namen:** `- name: put out`, `aliases: [blow out]`.
   - Worldbuilder-Validierung: Phrase gegen `reservedVerbWords` — **Entscheidung
     D3**: Phrase darf ein reserviertes Einzelwort enthalten (Longest-Match
     schützt); Warnung nur bei *exakter* Kollision (`name: put`).
   - Compile → `VerbDef`: JSON bleibt `{"name": …, "aliases": […]}` (Strings).
2. **Parser Longest-Match:** vor den Single-Word-Patterns von
   `parseSimpleCommandWith` den **längsten** Token-Präfix gegen die
   Verb-Registry matchen (Analogie: `canonicalHead`; Reihenfolge
   Phrase(n, n-1, …, 1)). Der Rest ist das Ziel/Argument:
   `put out candle` → `Interact (VCustom "put out") "candle"`.
   - Reihenfolge-Regel: **Phrasen schlagen Einzelwörter**, auch wenn das
     Einzelwort Core-Alias ist (`put out` schlägt `put`→`drop`).
   - Nicht anfassen: die hartcodierten Mehrwort-Zweige (`look at`, `pick up`,
     `talk to`, `put down`, `drive to`, `take all from`, …) matchen weiter zuerst
     (ihre Patterns stehen oben; Phrase-Match nur für die generische Reste).
3. **Phrase trägt durch:** `cmd.verb` (via `verbCanonicalName` =
   `"put out"`), `verb_map`-Keys `(Phase, Verb, Zustand)`, Trigger
   (`on: before put out`, `on: command put out` — `OnBefore`/`OnCommand`
   nehmen `extractCommandArgs`-vName), `GameLoop.commandVerbName` (aus
   derselben Quelle — Kommentar dort: „Keep the two in step").
4. **Edge Cases:** Compound (`take lamp and put out torch` — Split an `and`,
   beide Teile phrase-matchen), bare Form (`put out` ohne Objekt →
   `ITBareVerb`-Pfad, OPEN-16), `take all`-Massenformen (`put out all`? —
   **Entscheidung D4**: bewusst `Unknown`/Ziel-Auflösung überlassen),
   Disambiguierung, Sprach-Pack-Aliase (aeHead-Phrasen + `verbs:`-Phrasen
   zusammen: longest over both).

### B — Präpositionalphrasen mit Rollen (Engine)

1. **Generisches `V X PREP Y`** für Custom-Verben: Präpositionen aus
   `prepsOf env` (`on/with/in/from/to/about` + Pack-Aliase):
   → `InteractWith (VCustom v) X Y`. Der Split am **ersten** Präpositionstoken
   (wie `splitPrep`); Stopwörter auf beiden Seiten strippen.
2. **Generischer `InteractWith`-Dispatch** (ersetzt `use.nothing`):
   - X = primäres Ziel wie `Interact` (Auflösung + `verb_map` inkl. OPEN-08).
   - Y wird als **Werkzeug** gebunden: neue Variable `cmd.tool`
     (`VVText`, Roh-String; `cmd.tool_kind` analog `cmd.target_kind` optional).
   - `bindCommandVars`/`extractCommandArgs` erweitern (2. Argument),
     `cmd.raw_args` bleibt die volle Phrase.
3. **Werkzeug-Matching in `verb_map`** (Entscheidung D1): Vorschlag —
   optionaler Zusatz-Schlüssel `verb,tool:<id>` bzw. ein `tool:`-Feld am
   Entry; Default: Tool nur lesbar, kein Match-Kriterium (klein, kompatibel).
4. **`put X on Y` als Core-Zweig** (Entscheidung D2):
   - Variante *Core-Zweig* (Vorschlag): `"put" : rest | splitPrep (prepsOf env "on")`
     → `PutOnCmd X Y` (Dispatch wie `use X on Y`: `verb_map` auf Y,
     Rollen: X = Item, Y = Unterstützer).
   - Variante *Pattern-Syntax*: `- name: put`, `pattern: "<x> on <y>"` —
     deckt auch `fill X with Y`, `plug X with Y` generisch ab (größer,
     aber ein Mechanismus).
5. **Kollision `put X in Y` vs. `put X on Y` vs. `put X down`:** Split am
   ersten Präpositionstoken; `put down X`/`put X down` (Drop) zuerst,
   danach `in`, dann `on`. Durch `splitPrep` am ersten Treffer gegeben;
   Regressionstests nötig (`put gem in box on table` — Edge Case,
   dokumentieren).

### C — Tests

- Parser: Phrase-Matches + Konflikte (`put out candle` ≠ `drop`,
  `put X down` bleibt Drop, `look at`/`pick up` unverändert), Longest-Match
  über `verbs:` + Sprach-Pack, bare Formen, Compound.
- Semantik: `cmd.verb` = Phrase, Trigger `on: before/command put out`,
  `verb_map`-Dispatch auf Phrase, `cmd.tool`-Bindung + Auflösung, Metakommandos.
- Regression: 607 Engine-Tests + 320 worldbuilder-Tests + `scripts/ci.sh`
  (515 E2E) byte-identisch wo unberührt.
- Byte-Vertrag: `world.json`-Roundtrip (neue Felder nur bei Benutzung).

### D — Doku

- `adventure-schema.md`: `verbs:` (Mehrwort), Präpositionalphrasen,
  `cmd.tool`; EN-Liste ergänzen.
- `befunde-text-adventure-engine.md`: `OPEN-21` → ✅, §8 Port-Zeilen C1.
- `plan-zork-mechanik-entscheidungen.md`: C1 → ✅.
- `CHANGELOG.md`.

### E — Zork-Port (C1) als Abnahme

- `put out` / `blow out` / `douse` / `extinguish` → Kerzen/Fackel/Lampe
  (Original `V-PUT`/`V-BLOW`, `PUT OUT`-Objekte), Fackel-/Kerzenregeln
  (`torch-out`, `candles-*`) auf die Phrase umhängen.
- `put putty on leak` (ersetzt den `use putty on leak`-Workaround).
- `fill <x> with <y>` (Original `FILL` — Flasche/Maschine/„You may know how…").
- Optional: `turn off torch` als Phrase statt `turn-off-alias`-Hack.
- `worldbuilder validate` sauber + Spieltest + `win.py`-Lauf.

---

## 4. Offene Entscheidungen

| # | Frage | Vorschlag | Status |
|---|---|---|---|
| D1 | Werkzeug-Y in `verb_map`: nur lesbar (`cmd.tool`) oder Match-Kriterium (`verb,tool:<id>`) | erst lesbar, Match-Kriterium optional nachreichen | ☐ offen |
| D2 | `put X on Y` / `fill X with Y`: Core-Zweig (`PutOnCmd`) oder generische Pattern-Syntax (`pattern: "<x> on <y>"`) | Pattern-Syntax (ein Mechanismus für alle C1-Fälle) | ☐ offen |
| D3 | Phrase mit reserviertem Einzelwort (`put out` enthält `put`) erlauben? | ja (Longest-Match schützt), Warnung nur bei exakter Kollision | ☐ offen |
| D4 | Massenformen mit Phrase-Verben (`put out all`) | bewusst unsupported → `Unknown`; Ziel-Auflösung über OPEN-16, wo sinnvoll | ☐ offen |

**Ablauf:** D1–D4 festklopfen → A → B → C → D → E (E als Abnahme; Port-Texte
wie gehabt gegen `1actions.zil` spiegeln).

---

## 5. Abnahmekriterien

1. `put out candle` / `blow out candles` / `fill bottle with water` /
   `put putty on leak` parsen zu den in §2 genannten *korrekten* Commands und
   feuern `verb_map`/Trigger mit Phrase in `cmd.verb`.
2. Zork-Port: Kerzen/Fackel per `put out`, Leck per `put putty on leak`,
   `fill X with Y` spielbar; `worldbuilder validate` sauber.
3. Alle Regressionstests grün, `scripts/ci.sh` ohne FAIL; Byte-Vertrag gehalten.
4. Befunde/Plan/Schema-Doku aktualisiert (`OPEN-21` ✅).
