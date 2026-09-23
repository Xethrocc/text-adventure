# Pre-Run World Generator (Rogue Phase 4)

Der World Generator erzeugt ein komplettes Dungeon **vor** Spielstart als
eigenständigen Worldbuilder-Schritt: Ein YAML-Template wird zu einem normalen
`Adventure`-Record generiert, der durch die bestehende `compileAdventure`-
Pipeline läuft. Der Engine-Kern (`GameLoop`, `applyOutcomeWith`) bleibt
unberührt — ein generiertes Dungeon ist eine **statische Adventure-Datei wie
jede andere**.

Detailplan: `~/workspace/Rogue_phase4_worldgen_detailplan.md`.

## CLI

```bash
cabal run worldbuilder -- generate examples/templates/dungeon_template.yaml \
    --seed 42 -o /tmp/run
```

* **Seed-Pflicht:** `--seed N` (CLI, gewinnt) oder `seed: N` im Template.
  Ohne beides bricht der Lauf ab — bewusst, kein versteckter Uhrzeit-Seed.
* **Determinismus:** Gleicher Seed + gleiches Template ⇒ **byte-identisches**
  `world.json` + `save.json` (in der CI geprüft). Die Laufzeit-Zufälle des
  generierten Dungeons (`RandomChoice`, Encounters) starten mit einem aus dem
  Seed abgeleiteten `rngState` — auch der Spielverlauf ist pro Seed
  reproduzierbar.
* Exit-Codes wie `compile`: 0 ok, 1 bei Template-/Generierungs-/Validierungs-
  fehlern; `--force` schreibt trotz Validierungswarnungen.

## Template-Schema

Ein Beispiel-Template liegt im Repo:
`examples/templates/dungeon_template.yaml`.

```yaml
template:
  name: "Katakomben von Vhal"        # wird worldName (Slug via Phase-2-Regel)
  description: "…"
  seed: 42

layout:
  rooms: { min: 10, max: 16 }        # Zielanzahl; Generator darf darunter bleiben
  depth: 3                           # maximale Graphentiefe (Start = Tiefe 1)
  branching: 0.4                     # P(Verzweigung) pro neuem Raum (0 = Kette, 1 = Baum)
  loops: 2                           # zusätzliche diagonale Kanten (Zyklen)

room_templates:
  - id: junction                     # Archetyp-ID; Raum-IDs (<id>_<n>) vergibt der Generator
    weight: 3                        # Gewicht der Auswahl (>= 1)
    depth_range: [1, 3]              # nur in dieser Tiefe verwenden (1..layout.depth)
    savezone: true                   # optional: Instanz wird Savezone (Rogue Phase 1)
    room:                            # vollständiges ARoom-Fragment (bekanntes Schema)
      name: "Kreuzung"
      desc: "Ein gewölbter Kreuzgang. Knochen bedecken den Boden."
      tags: [crossroads]

special:
  start: { template: camp }          # Start-Archetyp (auch: irgendein savezone-Archetyp)
  boss: { template: crypt, depth: max }   # immer maximale Tiefe, Ziel des Runs
  treasure: { template: vault, locked: true }  # optional: verschlossener Schatzraum

oneway_hints:                        # optional: Falltüren (pro Eintrag GENAU eine Kante)
  - { from: pit_shaft, to: any, dir: down }

item_pool:
  - { item: { id: potion, name: "Heiltrank", portable: true }, count: { min: 2, max: 4 } }
  - { item: { id: key_iron, name: "Schlüssel" }, boss_lock: key_iron }  # reservierter Schlüssel

npc_pool:
  - { npc: { id: skeleton, name: "Skelett", max_hp: 8, attack: 2 }, count: { min: 1, max: 3 }, depth_range: [2, 3] }
  - { npc: { id: wraith, name: "Grabwicht", max_hp: 12, attack: 3 }, count: { min: 1, max: 1 }, boss: true }

combat:                              # optional: 1:1-Passthrough nach advCombat
  profile: classic
  screen: { bar_width: 20, scene: ">>Kampf in den Katakomben<<" }
                                     # die Kampfbildschirm-Syntax (screen.art/
                                     # scene/footer) ist in docs/adventure-schema.md
                                     # dokumentiert (Abschnitt combat.screen);
                                     # TheFog zeigt ein Authentizitäts-Beispiel:
                                     # Kunst per CondText auf den Wolf-Kampf begrenzt

player: { max_hp: 30 }
variables: []
```

Reuse-Prinzip: `room:`, `item:` und `npc:` sind die **existierenden**
Worldbuilder-Typen (`ARoom`/`AItem`/`ANPC`) — kein Parallel-Schema. NPC-Felder
heißen wie im Adventure-Schema (`max_hp`, nicht `max_health`).

## Generierungs-Garantien

1. **Jeder Raum ist erreichbar** — BFS auf dem gerichteten Graphen (einseitige
   Kanten werden in Laufrichtung befolgt); unerreichbare Zellen werden
   an erreichbare Nachbarn angedockt.
2. **Lock/Key-Ordnung:** Der `boss_lock`-Schlüssel liegt in einem Raum, der
   vor dem Schloss in Generationstiefe liegt (`keyDepth < lockDepth`, eine
   Obergrenze der BFS-Distanz). Beide Eingänge zum verschlossenen
   Treasure-Raum werden `locked`; das Lock-Entity wird als verstecktes,
   nicht-portierbares Item erzeugt; der Schlüssel trägt das Standard-Regelset
   `on_take → set_state unlocked` (Phase-7-Mechanik, kein neuer Engine-Mechanismus).
3. **One-way-Ausgänge (Falltüren):** Pro `oneway_hints`-Eintrag wandelt der
   Generator genau eine Kante um (Forward-Richtung wird auf den autorierten
   Namen umbenannt, z. B. `down`). Der Rückweg ist bewusst nicht garantiert
   (roguelike-typisch); wer Rückkehr will, legt zwei entgegengesetzte
   One-way-Kanten (Treppe runter + Treppe hoch).
4. **Savezones:** Jede `savezone: true`-Instanz landet in `game.save_zones`;
   der Generator setzt `ironman: true` (ohne wäre das Feld toter Code).
5. **Boss-Raum:** Liegt immer auf maximale Tiefe. Hat `npc_pool` genau einen
   Eintrag mit `boss: true`, wird er in den Boss-Raum platziert; sonst bleibt
   der Boss-Raum ein Rätsel-/Fallen-Raum (Review-Frage 4, keine Zwangsvalidierung).

## Fehler & Warnungen

Template-Fehler (`TemplateIssue`, mit Locate-Zeilen in der Template-Datei):
unbekannter `special.*.template`, `depth_range` außerhalb von
`1..layout.depth`, `boss_lock` ohne reservierten Schlüssel, `rooms.min < 3`,
invalides `combat:`-Fragment (Parse-Fehler), mehr als ein `boss: true`-NPC …

Warnungen (laufen durch, Schreiben mit `--force` oder ganz ohne Fehler):
`GeneratorEarlyAbort` (Gitter vor `layout.depth` erschöpft — Dungeon wird
mit erreichtem Maximum abgegeben), `GeneratorPopulationSkipped`,
`OnewayHintUnmatched`, `GeneratorTreasureSkipped`.

`GENoSpace` (harter Fehler) nur, wenn nicht einmal `rooms.min` platzierbar
sind — typischerweise Template-Parameter, die zusammen nicht passen
(z. B. reine Kette bei `branching: 0.0` mit `rooms.min > depth`).

## Multi-Level Dungeons (Rogue Phase 4b)

Für mehrstöckige Dungeons unterstützt das Template einen optionalen `levels:`-Block:

```yaml
layout:
  rooms: { min: 12, max: 18 }
  depth: 3                           # Anzahl der Ebenen (1..3)

levels:
  - rooms: { min: 4, max: 6 }
    depth: 4                         # Tiefenbudget innerhalb Ebene 1
    branching: 0.4
    return_stairs: true              # Treppe aufwärts ('up') zurück zur Vor-Ebene
  - rooms: { min: 4, max: 6 }
    depth: 4
    branching: 0.3
    return_stairs: true
  - rooms: { min: 4, max: 6 }
    depth: 4
    branching: 0.0
    return_stairs: false             # Point of no Return vor dem Bossraum
```

### Eigenschaften und Garantien

1. **3D-Gitter `(x, y, z)`:** Jede Ebene `z ∈ {1 .. N}` erzeugt ein eigenes Teilgitter.
2. **Treppenverbindungen:** Die tiefste Zelle der Ebene `i` wird über eine gerichtete Kante
   (`down`) mit dem Einstiegspunkt `(0, 0, i+1)` der Ebene `i+1` verbunden.
3. **Rückweg:** Wenn `return_stairs: true` für Ebene `i` gesetzt ist, wird automatisch eine
   Gegenkante (`up`) von `(0, 0, i+1)` zurück zur Treppenzelle von Ebene `i` generiert.
4. **Ebenen-Filterung:** `depth_range: [zMin, zMax]` an Raum- und NPC-Archetypen bezieht sich
   im Multi-Level-Modus auf die Ebenennummer (z. B. `[1, 1]` nur für Ebene 1, `[2, 3]` für Ebenen 2 und 3).
5. **Boss-Platzierung:** Der Boss-Raum wird stets auf der letzten Ebene (`z = N`) bei maximaler
   Tiefe platziert.
6. **Adventure/Engine-Feld `floor:`:** Generierte Räume tragen `floor: z` (in Haskell: `roomFloor :: Maybe Int`),
   wodurch die Ebenenzugehörigkeit für Frontend und Logik erhalten bleibt. Bei Single-Level-Dungeons
   wird das Feld weggelassen (M2 Default-Invariante).

### TUI-Minimap & Ebenen-Wechsel

In der Terminal-UI (TUI) filtert die Minimap automatisch auf die Räume der betrachteten Ebene:
* **Aktuelle Ebene (Default):** Der Titel zeigt `[Karte: Ebene <n> (hier)]` und markiert den
  Spieler mit `◆`.
* **Ebenen-Wechsel:** Mit `F2` oder `Ctrl-F` kann der Spieler durch alle bisher besuchten
  Ebenen blättern, um bereits erkundete Stockwerke zu prüfen (`[Karte: Ebene <k>]`).
* **Auto-Reset:** Bei der nächsten Spielerbewegung oder Eingabe springt die Minimap automatisch
  wieder auf die aktuelle Ebene des Spielers zurück.

## Nicht-Ziele

* **Vollständige Solvability** („ist der Boss mit den gefundenen Items
  besiegbar?") — der Generator garantiert Erreichbarkeit + Lock/Key-Ordnung;
  die Gegenwert-Abwägung liegt beim Template-Autor (Pools).
## Run-Regeneration: `worldbuilder run` (Phase 4c)

Für Roguelike-/Roguelite-Loops bietet das Tooling den Runner `worldbuilder run`.
Er generiert für jeden Durchlauf deterministisch ein frisches Dungeon basierend
auf dem globalen Fortschrittszähler `meta.runs` und startet die Engine.

### CLI-Syntax & Optionen

```bash
worldbuilder run <template.yaml> [options]
```

* `--seed <n>`: Überschreibt die automatische Seed-Ableitung für reproduzierbare Tests/E2E.
* `--saves-dir <dir>`: Basisverzeichnis für Speicherstände (Default: `saves` oder `$TA_SAVES_DIR`).
* `--keep-runs <n>`: Behält nur die `n` neuesten Runs im Slug-Verzeichnis und räumt ältere `run_<k>`-Ordner auf (Default: unbegrenzt).
* `--no-launch`, `--dry-run`: Bereitet die Welt und das Run-Verzeichnis vor, startet die Engine jedoch nicht.
* `--force`: Startet auch bei Validierungsfehlern.
* `--tui`: Übergibt an das Terminal-UI Frontend (Brick).
* `--no-color`: Deaktiviert ANSI-Farbausgaben.

### Seed-Ableitung & Determinismus

Der Generation-Seed jedes Runs wird deterministisch aus dem Adventure-Slug und dem aktuellen Run-Index berechnet:
```
seed = splitmix64(salt(slug) * GOLDEN1 + runIndex * GOLDEN2)
```
* `GOLDEN1 = 0x9E3779B97F4A7C15`, `GOLDEN2 = 0xBF58476D1CE4E5B9`.
* Gleicher Run-Index und Slug ergeben auf jeder Plattform und Maschine exakt dieselbe Welt.
* Jeder neue Run (`meta.runs` steigt um 1) liefert eine neue, unverbrauchte Dungeon-Topologie ohne Abhängigkeit von Systemuhren.

### Verzeichnisstruktur & Checkpoint-Bindung

```
saves/
├── katakomben_von_vhal_meta.json      # Globaler Fortschritt (meta.runs, meta.souls, etc.)
└── katakomben_von_vhal/
    ├── run_1/
    │   ├── world.json                 # Generierte Welt für Run 1
    │   ├── save.json                  # Start-Zustand für Run 1
    │   └── checkpoint.json            # Run-spezifischer Checkpoint-Save
    └── run_2/
        ├── world.json                 # Generierte Welt für Run 2
        ├── save.json
        └── checkpoint.json
```

1. **Globale Meta-Variablen:** `$TA_META_DIR` zielt auf `<saves-dir>/`, sodass `<slug>_meta.json` run-übergreifend erhalten bleibt.
2. **Hermetische Checkpoint-Bindung:** `$TA_SAVES_DIR` zielt auf das Run-Verzeichnis `<saves-dir>/<slug>/run_<n>/`. In-Game-Saves landen dort. Ein Checkpoint aus Run 1 kann niemals fälschlicherweise in Run 2 geladen werden (World-Checksum-Schutz greift und verhindert Verfälschungen).

## CI

`scripts/ci.sh` prüft die Generierung und Run-Regeneration automatisiert:
* **Stufe 6 (Worldgen Phase 4):** Generiert das Repo-Template zweimal mit gleichem Seed (byte-identischer Output), spielt den Dungeon-Durchlauf bis zum Boss-Kill durch (`ci/e2e/worldgen.in` / `worldgen.expect`) und prüft die Kampfbildschirm-Zeilen.
* **Stufe 7 (Run-Regeneration Phase 4c):** Führt zwei aufeinanderfolgende Runs über denselben Template-Pfad aus, verifiziert die Erstellung der `run_1`- und `run_2`-Verzeichnisse, prüft die Divergenz der generierten Welten und stellt sicher, dass der Run-Zähler `meta.runs` korrekt von 1 auf 2 fortgeschrieben wird.

