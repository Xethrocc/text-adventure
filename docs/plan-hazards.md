# Plan: `hazards:` — generalisierte Gefahrenlagen (OPEN-23)

**Stand:** 2026-10-15 · **Status:** Stufe 1, Stufe 2 (AP A–E) und Stufe 2b (AP A–D) vollständig umgesetzt (CI grün, Byte-Vertrag: 5 Abenteuer SHA256-identisch) ·
**Gültig:** Das ist **der** Plan zu `OPEN-23` (Entscheidung 2026-10-13). Zunächst als Analyse entstanden,
nach einem Wechsel des Repo-Stands wiederhergestellt.

> **Zum Unwettermodul:** `/root/workspace/plan-unwettermodul.md` (Zwischenentwurf einer parallelen Sitzung)
> ist **abgelöst** durch dieses Dokument. Brauchbare Ideen daraus (gewichtete Zufallsereignisse per RNG-Stream,
> vordefinierte Wirktypen) sind als Option für Stufe 2 in §10 notiert.

Herkunft: `plan-zork-mechanik-entscheidungen.md` E2 (Tide/Reservoir) · Befunde-Ticket `OPEN-23`.

## 1. Ziel

Kein „Wasserstandsmodul", sondern **ein** Segment `hazards:`, mit dem Autoren zeitabhängige, räumlich begrenzte Gefahren beschreiben:
Wasser/Flut, Lava, Feuer, Sturm, Erdrutsch, einstürzende Höhle/Haus, Gas, Seuche …

Jede Gefahr = **Verlauf × Ausdehnung × Wirkung**:

| Achse | Frage | Wasser/Lava | Feuer | Sturm/Wetter | Einsturz/Erdrutsch |
|---|---|---|---|---|---|
| **Verlauf** (`track`) | Wie entwickelt sie sich über Züge? | Pegel steigt/fällt | Intensität wächst | Zustände wechseln | Countdown mit Phasen |
| **Ausdehnung** (`footprint`) | Welche Räume sind betroffen? | Schwelle je Raum | per Ausbreitung (Stufe 2) | Raum-Tag `outdoor` | feste Liste je Phase |
| **Wirkung** (`hooks`) | Was passiert? | Meldungen, Schaden, Treibgut, Ausgänge | Schaden, Items brennen | Text, Sicht | Raum blockiert, Items verschüttet |

## 2. Entscheidungen (vom Nutzer getroffen, 2026-10-13)

| # | Frage | Entscheidung |
|---|---|---|
| E1 | Reversibel? | **Beides** — `target` + `rate`: Pegel steigt *und* fällt, Feuer kann gelöscht werden |
| E2 | Spieler in der Zone | **Nur frei per Effekt** — die Engine macht nichts automatisch; Schaden/Tod/Schutz schreibt der Autor in die Hooks (Schutz = Prädikat im `when:`) |
| E3 | `environment.weather` | **Langfristig in `hazards:` aufgehen** (Alias/Ersatz); Bestandsabenteuer dürfen nicht brechen |
| E4 | Umfang Stufe 1 | **Verlauf + Footprint `elevation`/`list`/`tag` + Hooks**, reines Worldbuilder-Sugar (~3–4 d) |
| E5 | Zork (OPEN-23a) | **Wartet auf `hazards:`** — der Dam wird erster Anwendungsfall; der `low_tide`-Hack bleibt bis dahin |
| E6 | Umkehr (D3) | **Restdistanz (`continue`)** — Level läuft vom aktuellen Stand zurück. Zork weicht damit minimal vom Original ab (dort startet jede Umkehr einen frischen 8-Zug-Timer). Optionales `track.reverse: restart` bleibt später additiv möglich |
| E7 | Raumhöhe (D2) | **Option A: Tabelle im Footprint** (`rooms: {raum: schwelle}`). Ein Raumfeld `elevation:` wäre nur bei echtem Engine-Vorteil sinnvoll (der Kern kennt keine Höhe) → nicht jetzt; additiv nachrüstbar (die Tabelle würde dann Override) |

## 3. Verifizierte Grundlagen (Code-Check)

| Frage | Befund | Beleg |
|---|---|---|
| Lässt sich ein Segment ohne Kern-Eingriff bauen? | **Ja.** `environment:` kompiliert zu Variable `env.weather` + `on: turn`-Triggern (`TriggerDef … OnTurn`); Stealth (7e) expandiert sogar **pro Raum** Trigger. | `worldbuilder/src/Worldbuilder/Compile.hs` `compileEnvironment` (~1478), `compileWeather`, `compileStealth` (~1555) |
| Reservierter Namensraum + Fehler | `env.*` reserviert, `EnvironmentVariableClash`, `UnknownWeatherState`, `UnknownDrainVariable`; `mergeEnvironmentVars` | `Compile.hs` ~1495–1545 |
| `when:`-Prüfungen des Segments | werden in einer Sammelliste zur Prädikat-Validierung registriert (B4) | `Compile.hs` ~355–370 |
| Pro-Zug-Takt | `OnTurn` feuert in `GameLoop.hs` nach Fahrzeug-Tick (Z. 283); Trigger haben Cooldown-/Max-Fires-Felder | `GameLoop.hs` 283, `TriggerDef … False 0 1 [] []` |
| Zug-Timer mit End-Effekt | `ApplyCondition name turns tick end hidden` / `ClearCondition`, im Save (`conditions`), `tickConditions` | `Types/Core.hs` 1171, `Effects.hs` ~1450, `Core.hs` 2880 |
| Raum-Eigenschaften | `Room` hat `roomTags` (statisch), `roomFloor :: Maybe Int`, **kein** `elevation`, **keine** Raum-Variablen, **kein** Laufzeit-Tag-Effekt | `Types/Core.hs` 2208 |
| Raum-Prädikate | `RoomHasTag`, `Location actor room`, `RoomHasTaggedItem`, `CompareVar` | `Core.hs` 943–957 |
| Massen-Effekte | `CountSpec{what, where, tag}`; `where` = `in: <raum>` / `here` / `room_of` / `by` / `in_container` — **nur ein Raum**, kein Raum-Set | `Core.hs` 570–599 |
| Ausgänge zur Laufzeit | `SetExit`/`RemoveExit` existieren (Rogue Phase 3); alternativ `Guarded`-Exit mit Prädikat (statisch, bevorzugt) | `Core.hs` 63–64, `Exit` 15–17 |
| Spieler-/NPC-Schaden | `ModifyValue VRPlayerHealth`, `DamageAll CountSpec n`, `GameEnd`, `MoveEntity`, `MoveAll` | `Core.hs` 47, 87–91, 57 |
| Zustandslose Zufallsentscheidung | `wanderHash seed turn name` (stateless, `save.runSeed`) in `Game.hs` 861; `RandomChoiceOn <stream>` (deterministisch über `rng.<name>`) | `Game.hs` 856–862, `Core.hs` 45 |
| BFS/Distanz | `bfsDistances :: Map Room [(Dir,Room)] -> Room -> Map Room Int` | `Pursuit.hs` 48 |
| Text je Zustand | `CondText`-Varianten mit `compare_var` (so schon beim Wetter, z. B. Eispass-Kamm) | `examples/modules/survival.yaml` 89 |
| Kern-Wettermodul? | **Nein.** Nur Modul 7d `environment:` (Worldbuilder-Sugar), `grep weather` in `src/` ohne Treffer | `docs/modules.md` §7d |

**Folgerungen:**

1. **Alles geht als Compile-Time-Expansion**, weil der Raumgraph beim Kompilieren feststeht (Stealth macht es vor). Kein neuer Kern-Typ nötig für Stufe 1.
2. Mass-Effekte müssen **pro betroffenem Raum expandiert** werden (`move_all … in: <raum>`), nicht über ein Raum-Set. Für generierte Räume (Sandbox-Worldgen, zur Laufzeit) greift das nicht → **ausdrücklich außerhalb des Umfangs**.
3. **Spread (Stufe 2) ist ebenfalls ohne Kern machbar:** pro Kante `(r → n)` ein Trigger „r brennt ∧ n nicht → `RandomChoiceOn "hazard.<id>"` setzt Raum-Variable `hazard.<id>.zone.<n>`". Ein stateless `wanderHash`-artiger Effekt wäre nur eleganter (kein `rngState`-Verbrauch) — Kern-Eingriff optional.
4. **Raum-Höhe:** `elevation` existiert nicht → Tabelle im Footprint (E7).

## 4. Autoren-Syntax (Entwurf)

```yaml
hazards:
  reservoir:                              # Wasser (Zork-Dam)
    track:
      max: 8                              # Level 0..8
      start: 8
      rate: 1                             # Schritte pro Takt (Richtung folgt target)
      every: 1                            # Takt in Zügen
      target: 8                           # Autor-Effekt hazard_target setzt es um
      when: {...}                         # optional: Pause-Bedingung
    footprint:
      by: elevation                       # elevation | list | tag
      rooms: { reservoir: 0, reservoir-north: 3, reservoir-south: 3 }  # Zone wenn level >= Schwelle
    stages:                               # Hooks beim ERREICHEN eines Levels (egal ob rauf/runter)
      0: [ { msg: "Der Pegel ist jetzt niedrig." }, { reveal: trunk } ]
      8: [ { msg: "Das Wasser hat seinen Höchststand erreicht." } ]
    on_zone:                              # je Takt für Räume in der Zone (freie Effekte, E2)
      - if: { at: player, room: reservoir } then: [ ... ]
    telegraph:                            # Meldung, wenn der Spieler in/neben der Zone ist
      adjacent: "Du hörst Wasser rauschen."
```

Autor-Effekte (neu, im Worldbuilder-Vokabular): `hazard_target: {hazard, to}` · `hazard_jump: {hazard, to}` (setzt Level hart) · `hazard_pause`/`hazard_resume`.
Autor-Prädikate (Sugar): `hazard_level: {hazard, gte|lte|eq: n}` · `in_hazard: {hazard, room: <id>|here}`.

Footprint-Varianten:
- `by: elevation` — `zone(r) ⇔ level ≥ schwelle(r)` (Wasser, Lava, Gas, Nebel).
- `by: list` — `stages_rooms: {1: [halle], 2: [halle, schiff]}` (Einsturz, Erdrutsch).
- `by: tag` — alle Räume mit Raum-Tag (z. B. `outdoor`), aktiv sobald `level > 0` (Sturm).
- `by: spread` *(Stufe 2)* — `from:`, `via: exits`, `chance:`, `needs_tag:`; Raum-Variable je Raum.

## 5. Kompilierung (Sugar → vorhandene Primitive)

| Autor-Konstrukt | Erzeugt |
|---|---|
| `track` | Variablen `hazard.<id>.level`, `.target`, `.clock`, `.prev` (Int, `hazard.*` reserviert, `HazardVariableClash`); Trigger `hazard.<id>.tick` (`on: turn`): `clock++`, bei `clock mod every == 0 ∧ ¬paused` → Level um `rate` Richtung `target`, geklammert |
| `stages` | Trigger `hazard.<id>.stage` **nach** `tick`: je Level n `when level==n ∧ prev≠n` → Hook-Effekte; danach `prev := level` (fängt auch `hazard_jump` ab) |
| `footprint` + `on_zone` | pro Zonenraum ein `on: turn`-Trigger: `at:player room r ∧ zone(r)` → Hook-Effekte (Stealth-Muster) |
| Mass-Items (Treibgut, Verbrennen) | `move_all`/`consume_all`/`set_state_all` je Raum aus `stages`/`on_zone` expandiert (`tag:` + `in:`) |
| Ausgänge | `Guarded`-Exit mit `hazard_level` (statisch), kein Laufzeit-Rewire nötig |
| Raumtexte | `CondText`-Varianten mit `hazard_level` → `compare_var` |
| `hazard_target`/`_jump` | `SetValue` auf `.target`/`.level` (`.prev` bleibt, damit Hooks feuern) |

Validierung: `UnknownHazard`, `HazardLevelOutOfRange`, `HazardUnknownRoom`, `HazardFootprintEmpty`, `HazardVariableClash`, Hook-`when:` über die B4-Sammelliste. Byte-Vertrag: `hazards:` nur bei Benutzung serialisieren.

## 6. Arbeitspakete Stufe 1 (E4) — ca. 3–4 Tage

| AP | Inhalt | Aufwand |
|---|---|---|
| A | Typen + JSON (`AHazard`, `ATrack`, `AFootprint`, Stages/Hooks) in `Worldbuilder/Types.hs`, Feld `advHazards` (+ `Generate.hs` `Nothing`) | 0,5 d |
| B | `compileHazards`: Variablen, `tick`/`stage`-Trigger, Reservierung/Merge (analog `mergeEnvironmentVars`), B4-Registrierung | 1 d |
| C | Footprint `elevation`/`list`/`tag` + `on_zone`/`telegraph` + Autor-Effekte/-Prädikate | 1 d |
| D | Validierung + Fehlercodes + Fuzz-/Locate-Anbindung | 0,5 d |
| E | Tests (Worldbuilder + Engine-Golden: steigen, fallen, Umkehr mitten im Verlauf, `hazard_jump`, Save/Load), Fixture `examples/modules/hazards.yaml` (Wasser + Sturm + Einsturz), Performance-Messung (D6) | 1 d |
| F | Docs: `adventure-schema.md` („Hazards"), `modules.md`, CHANGELOG, Befunde-Ticket | 0,5 d |

**Stufe 2 (nicht beschlossen):** `spread` (~2 d), Item-Helfer wie `flow:`/`burn:` (~1–2 d). Wettermigration (E3) separat, siehe §8.

## 7. Zork-Anwendungsfall (E5)

`hazards.reservoir`: `max: 8`, `target: 8` (zu) bzw. `0` (offen), `every: 1`, `rate: 1` ergibt einen 8-Zug-Verlauf — **mit gestaffelten Zwischenmeldungen gratis**. `stages` ersetzen `I-REMPTY`/`I-RFILL`; ortsabhängige Meldungen/Tod/Boot/Loud-Room in den Hooks (`if: at: player room: …`). `bolt`: `set_flag low_tide` → `hazard_target`; `low_tide` bleibt als abgeleitetes Prädikat (`hazard_level lte 0`), damit die ~53 Fundstellen im Port unverändert weiterlaufen.

**Original-Verhalten zum Vergleich (`zork1/1actions.zil`, `BOLT-F`):** Zudrehen nach 3 von 8 Zügen bricht `I-REMPTY` ab (`QUEUE I-REMPTY 0`) und startet `I-RFILL` mit frischen 8 Zügen; `LOW-TIDE` wurde nie gesetzt (nur am Ende von `I-REMPTY`), es gibt also keinen Zwischenzustand — nur eine ortsabhängige, teils unpassende Meldung nach 8 Zügen. Entscheidung E6: das Level-Modell läuft stattdessen in der Restdistanz zurück.

## 8. Wetter-Migration (E3)

`environment.weather` bleibt zunächst unverändert. Spätere Zusammenführung als **Alias**: `environment.weather` kompiliert intern zu einem `hazards`-Eintrag vom Typ „Zustandsliste", Variable heißt weiter `env.weather`, Fehlercodes (`UnknownWeatherState`, `EnvironmentVariableClash`) bleiben, Golden-Tests (`survival.yaml`) ändern sich nicht. Voraussetzung: `track` kann **Zustandsnamen + bedingte Übergänge** (nicht nur Zahlen) — ein eigener Schritt nach Stufe 1 (~1 d + Regressionslauf).

## 9. Offene Detailfragen

| # | Frage | Stand |
|---|---|---|
| D1 | Name: `hazards:` | Empfehlung `hazards:` (englische Schema-Keys wie `environment:`) |
| D2 | ~~Raumhöhe~~ | ✅ entschieden → E7 (Tabelle im Footprint) |
| D3 | ~~Zork-Fidelity bei Umkehr~~ | ✅ entschieden → E6 (`continue`) |
| D4 | Mehrere Gefahren gleichzeitig: Reihenfolge der Effekte | Deklarationsreihenfolge; Doku-Hinweis |
| D5 | Generierte Räume (Sandbox-Worldgen) | außerhalb Stufe 1; `tag`-Footprint könnte später Laufzeit-Räume erfassen |
| D6 | Performance | `N Räume × M Gefahren` Trigger je Zug; Stealth hat dasselbe Muster. Messung mit Fixture (~500 Trigger) im AP E |

## 10. Übernommenes aus dem Unwettermodul (abgelöst, Stufe-2-Optionen)

Das Zwischenentwurf-Unwettermodul (`environment.severe:` an 7d) löst OPEN-23 anders: ereignisgetriebenes Pegelheben (`flood:{delta/set}`), vordefinierte Wirktypen (`flood:`, `landslide:`, `damage:`), Zufallsereignisse per RNG-Stream/Periode. **Entschieden ist: `hazards:` ist der gültige Plan.** Zwei Ideen daraus bleiben wertvoll und sind für Stufe 2 notiert:

- **Zufallsereignisse** (`every: {period, stream, choices}` nach `RandomChoiceOn`) für nicht vorhersagbare Gefahren (Erdrutsch, Sturmböen) — passend neben `spread`.
- **Vordefinierte Wirktypen** als späteres Sugar über den freien Hooks (E2), z. B. `flood:`, `landslide:`, `damage:` als Compiler-Kürzel auf `move_all`/`SetExit`/`ModifyValue VRPlayerHealth`.

## 11. Reihenfolge / Konsequenzen

1. Stufe 1 (AP A–F). 2. Zork-Dam umbauen (OPEN-23a, wartet laut E5). 3. Stufe 2 (`spread`, Item-Helfer, Zufallsereignisse nach §10) nach erstem Autoreneinsatz. 4. Wetter-Alias (E3).

## 12. Stufe 2 — entschieden 2026-10-14 (Patrick)

**Umfang (2a):** `spread` + Zufallsereignisse + Wirktypen `flood:`/`landslide:`/`damage:`
in einem Durchlauf. **Item-Helfer (`flow:`/`burn:`, §6) bleiben separat (2b)** —
eigene Entscheidung, eigene Stufe.

### E8 — Spread ist level-gebunden (Entscheidung 2026-10-14, abweichend vom
boolean-Kanten-Wurf aus §3 Folgerung 3)

- Footprint-Variante `by: spread` mit `from: <raum>`, `via: exits`,
  `chance: <0–100>`, `needs_tag: <tag>?`.
- **Zone bei Level L = alle Räume, die von `from:` in ≤ L Schritten über
  `via:`-Kanten erreichbar sind.** Jede traversierte Kante durchläuft einen
  `chance:`-Wurf auf dem Stream `hazard.<id>.spread` (`RandomChoiceOn`, B8,
  deterministisch pro Run-Seed) — ein fehlgeschlagener Wurf schneidet diesen
  Zweig ab; die Würfe sind pro Run stabil (gleicher Seed → gleiche Zone).
- Steigendes Level erweitert die Zone ringweise (pro Level-Einheit), fallendes
  zieht sie in derselben Granularität zurück (Reversibilität E1 analog).
- Raumzustand `hazard.<id>.zone.<raum>` (0/1) wie in Stufe 1 geplant;
  `in_hazard:` und `on_zone:`/`telegraph:` arbeiten darauf weiter.
- `from:` ist immer in der Zone (Quelle); `needs_tag:` schließt Räume ohne das
  Tag von der Expansion aus (z. B. nur `outdoor`).
- Kombination mit `track` ist die Regel (der Pegel steuert den Radius); ein
  Spread ohne `track` ist zulässig (Level wird dann per `hazard_jump`/
  Wirktypen gesetzt).

### E9 — Zufallsereignisse (§10 übernommen)

- Auf Gefahren-Ebene neben `track`/`stages`/`on_zone`:
  `every: {period: P, stream: name, choices: [[gewicht, [effekte]], …]}`.
- Pro P Züge ein Wurf auf `rng.<name>` (B8, `RandomChoiceOn`-Semantik);
  Null-Gewicht/Erfolg ohne Ziel = kein Effekt. Deterministisch pro Run-Seed.

### E10 — Wirktypen als Compiler-Kürzel (keine neue Runtime)

| Sugar | Expansion | Zweck |
|---|---|---|
| `flood: {hazard, delta: N}` | `hazard_target` (Ziel um N verschieben, Level läuft mit `rate` hin) | Pegelbewegung wie E5 |
| `flood: {hazard, set: N}` | `hazard_jump` (hart setzen) | sofortige Pegelstellung |
| `landslide: {from, dir}` / `landslide: {from, dir, reopen: true}` | `remove_exit` bzw. `set_exit` | Ausgang sperren/freigeben |
| `damage: {player: N}` / `damage: {npc: <id>, amount: N}` | `ModifyValue VRPlayerHealth` bzw. `damage_npc`-Äquivalent | Schaden |

- Validierung/Fehlercodes analog Stufe 1 (unbekannte Hazard-/Raum-/NPC-Referenzen),
  Hook-`when:` über die B4-Sammelliste.
- **Byte-Vertrag:** neue Felder/Keys nur serialisiert wenn benutzt; bestehende
  Welten byte-identisch (Vorbild Stufe 1).

### Stufe-2-Arbeitspakete (agy-Stufe, extern verifizieren)

| AP | Inhalt |
|---|---|
| A | Typen: `AFootprintType`-Variante `Spread`, `AEventEvery` (`every:`-Block), Wirktyp-Outcome `AOFlood`/`AOLandslide`/`AODamage` + JSON, `advHazards` erweitert |
| B | Compile: Spread-Ringlogik (Level↔Radius, Kanten-Würfe auf `hazard.<id>.spread`, `zone.<raum>`-Pflege pro Ringstufe), `every:`-Trigger mit Periode + Stream, Wirktyp-Desugaring |
| C | Validierung: `UnknownSpreadFrom`, `UnknownSpreadTag`, `EveryWithoutStream`, Wirktyp-Refchecks; Walker-Abgleich (Catch-alls wie Stufe 1 AP D) |
| D | Tests: Spread steigen/fallen, Kanten-Wurf-Stabilität (gleicher Seed), `needs_tag:`-Filter, `every:`-Perioden/Streams, Wirktyp-Mapping, Byte-Vertrag-Wächter; Fixture `examples/modules/hazards.yaml` um einen Spread-Fall + Zufallsereignis erweitern |
| E | Docs: `adventure-schema.md` (Spread-Footprint, `every:`, Wirktypen), `modules.md` 7j, CHANGELOG, Befunde-Eintrag |

**Abnahme:** `scripts/ci.sh` grün inkl. erweiterter Fixture; Spread-Determinismus
(gleicher Seed → gleiche Zonen) im Test belegt; Welten ohne Stufe-2-Features
byte-identisch.

## 13. Stufe 2b — Item-Helfer `flow:`/`burn:` (entschieden 2026-10-14, Patrick)

Drei Entscheidungen (alle Empfehlungen angenommen):

- **E11 — Verankerung auf der Gefahr:** `items: {flow: …, burn: …}` mit
  Tag-Bezug (`tag: treibgut`) — nutzt die vorhandene Tag-Mechanik, **kein**
  Item-Schema, keine neuen Item-Felder, kein neuer Save-State.
- **E12 — `flow:`-Richtung: vom Ursprung weg** — Items treiben den
  Spread-Ringen folgend zum Zonenrand (bei `elevation`/`list`/`tag`-Footprints
  in einen beliebigen Nachbarraum in der Zone mit größerem Abstand zu `from:`;
  ohne solchen Nachbarn bleibt das Item liegen).
- **E13 — `burn:` zufallsbasiert pro Zug** (stateless): `chance: 0–100` pro Zug
  in der Zone, deterministisch pro Stream/Seed — **Byte-Vertrag bleibt gewahrt**
  (keine Expositions-Zähler im Save).

### Autoren-Syntax

```yaml
hazards:
  flut:
    # … track/footprint/stages wie gehabt …
    items:
      flow: { tag: treibgut, chance: 30, msg: "{item} treibt davon." }   # msg optional
  feuer:
    items:
      burn: { tag: brennbar, chance: 20, msg: "{item} verglüht." }
```

### Semantik

- Beide Helfer arbeiten **pro Zug und Zonenraum** auf dem aktuellen
  `zone`-Zustand (alle Footprint-Typen): für jedes **direkt im Raum liegende**
  Item mit passendem Tag wird ein `chance:`-Wurf auf dem Stream
  `hazard.<id>.items` geworfen (B8, deterministisch pro Run-Seed).
- `flow` (Erfolg): Item wechselt in einen Nachbarraum in der Zone, der eine
  Ringstufe weiter von `from:` entfernt liegt (vom Ursprung weg); liegt keiner
  vor, bleibt das Item. Container-Inhalt und getragene Items sind **nicht**
  betroffen (Semantik wie `move_all` auf Räume).
- `burn` (Erfolg): Item wird verbraucht (`consume`-Semantik, FIX-02-konform —
  keine Wiederbelebung).
- `msg:` ist optional (Autorentext mit `{item}`/`{room}`-Platzhaltern); **ohne
  `msg:` ist das Ereignis still** — keine neuen Engine-Messages-Keys
  (Katalog-Gate bleibt unberührt, reines Worldbuilder-Sugar).
- Reihenfolge bei mehreren Helfern: erst `flow`, dann `burn` (Doku-Hinweis;
  D4-Muster Deklarationsreihenfolge).

### Validierung / Fehlercodes

Strukturelle Fehler analog Stufe 2 (`UnknownItemHelperTag` als Warning wenn der
Tag nirgends vorkommt, `HazardItemHelperChance` bei `chance` außerhalb 0–100,
unbekannte Felder = Schema-Warnung). Walker-Catch-alls abgleichen (Stufe 2 AP C).

### Arbeitspakete (agy-Stufe)

| AP | Inhalt |
|---|---|
| A | Typen: `AHazardItems` (`flow`/`burn` mit `tag`, `chance`, `msg?`) + JSON, omittierbar; `hazItems` an `AHazard` |
| B | Compile: pro Zug/Zonenraum Würfe auf `hazard.<id>.items`, `flow`-Ring-Suche (vom Ursprung weg), `burn`-Consume; Trigger-Muster wie Stufe 1/2 |
| C | Validierung + Tests: Flow-Richtung/-Rand, Burn-Consume, Chance-Determinismus (gleicher Seed), Container/Getragenes unberührt, Fußpunkt-Verhalten; Fixture um `items:`-Helfer erweitern |
| D | Docs: `adventure-schema.md` (Abschnitt Item-Helfer), `modules.md` 7j, CHANGELOG, Befunde-Eintrag; Byte-Vertrag-Wächter |

**Abnahme:** `scripts/ci.sh` grün; Byte-Vertrag (Welten ohne `items:` byte-identisch);
Determinismus-Test (gleicher Seed → gleiche Fluss-/Brand-Ereignisse).
