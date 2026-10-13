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

**Status:** 📋 geplant · **Entscheidungen D1–D4:** ✅ entschieden 2026-10-13 (§4)
· **Aufwandsschätzung:** A+B je ~1 Tag, C+D ~½ Tag, E ~½ Tag

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
- Keine Änderung an Kern-Befehlen: `put X in Y` / `put X down` bleiben wie sie
  sind; `put X on Y` ist mit D2 eine **Pattern-Deklaration** des Abenteuers,
  kein neuer Core-Zweig.
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
   `ITBareVerb`-Pfad, OPEN-16), Disambiguierung, Sprach-Pack-Aliase
   (aeHead-Phrasen + `verbs:`-Phrasen zusammen: longest over both).
5. **Massenformen (D4 entschieden):** `put out all` wendet das Verb auf
   alle passenden Ziele im Scope an — Massen-Dispatch wie `take all`,
   `cmd.succeeded` ist die Konjunktion aller Teile (wie bei Sammelkommandos,
   `take sword and lamp`).

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
3. **Werkzeug-Matching in `verb_map` (D1 entschieden: Lesbarkeit + Match-
   Kriterium):** `cmd.tool`/`cmd.tool_kind` sind lesbar **und**
   `verb_map`-Einträge dürfen auf dem Werkzeug matchen — Vorschlag als
   Zusatz-Schlüssel `verb,tool:<id>` am Entry (analog `wind,intact` aus
   `Z-06`); Validierung: unbekanntes Tool in `tool:`-Keys → B4-Warnung.
4. **Grammatik-Erweiterung (D2 entschieden: generische Pattern-Syntax):**
   `verbs:` bekommt `pattern:`, z. B.:
   ```yaml
   - name: put out
     aliases: [blow out]
   - name: put
     pattern: "<x> on <y>"        # Zork PUT ON (PUT PUTTY ON LEAK)
   - name: fill
     pattern: "<x> with <y>"      # FILL X WITH Y
   ```
   Der Parser matched Pattern-Formen **vor** der Core-Auflösung (längste
   Phrase + Shape zuerst); nicht passende Eingaben fallen wie gehabt durch
   (`put gem in box` hat kein `on` → Core `PutInCmd` bleibt).
   - **D2-Nachtrag (Kollision reservierter Wörter):** `pattern:`-Deklarationen
     auf ein Core-/reserviertes Wort (`- name: put`) gelten als
     **Erweiterung** (der Shape `put X on Y` wird ergänzt, `put X in Y`/
     `put X down` bleiben Core) — nicht als Kollision. Die D3-Warnung trifft
     nur `name: put` **ohne** `pattern:` (würde durch Core-Union nie matchen).
5. **Rollensplit:** Pattern-Gruppen (`<x>`, `<y>`) + Präpositionstoken
   (`on`/`with`/`in`/`from`/`to`/`about` + Pack-Aliase) →
   `InteractWith (VCustom v) X Y`; Stopwörter auf beiden Seiten strippen.

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

| # | Frage | **Entscheid (2026-10-13)** | Status |
|---|---|---|---|
| D1 | Werkzeug-Y in `verb_map` | **Lesbar + Match-Kriterium** — `cmd.tool`/`cmd.tool_kind` lesbar, `verb_map`-Entries matchen zusätzlich auf dem Werkzeug (`verb,tool:<id>`), Validierung wie `Z-06` | ✅ |
| D2 | `put X on Y` / `fill X with Y` | **Generische Pattern-Syntax** (`pattern: "<x> on <y>"`), Core-Formen (`put X in Y`, `put X down`) bleiben unberührt; Pattern-Deklaration auf reservierte Wörter = Erweiterung (D2-Nachtrag, s. B4) | ✅ |
| D3 | Phrase mit reserviertem Einzelwort (`put out` enthält `put`) | **Ja, aber mit Warnung** — Phrase erlaubt (Longest-Match), Compiler-Warnung (B4-Kanal); exakte Kollision (`name: put` ohne `pattern:`) ebenfalls Warnung | ✅ |
| D4 | Massenformen mit Phrase-Verben (`put out all`) | **Masse + `cmd.succeeded`-Konjunktion** — alle Ziele im Scope, Erfolg nur wenn alle Teile gelingen (wie Sammelkommandos) | ✅ |

**Ablauf:** A → B → C → D → E (E als Abnahme; Port-Texte wie gehabt gegen
`1actions.zil` spiegeln).

---

## 5. Abnahmekriterien

1. `put out candle` / `blow out candles` / `fill bottle with water` /
   `put putty on leak` parsen zu den in §2 genannten *korrekten* Commands und
   feuern `verb_map`/Trigger mit Phrase in `cmd.verb`.
2. `verb_map`-Entries matchen auf dem Werkzeug (`verb,tool:<id>`) und
   `put out all` arbeitet als Masse mit `cmd.succeeded`-Konjunktion (D1/D4).
3. Zork-Port: Kerzen/Fackel per `put out`, Leck per `put putty on leak`,
   `fill X with Y` spielbar; `worldbuilder validate` sauber (inkl. D3-Warnungen).
4. Alle Regressionstests grün, `scripts/ci.sh` ohne FAIL; Byte-Vertrag gehalten.
5. Befunde/Plan/Schema-Doku aktualisiert (`OPEN-21` ✅).
