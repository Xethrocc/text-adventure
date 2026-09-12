# Text-Adventure YAML Schema

## Minimal Adventure (einfachster Einstieg)

```yaml
name: My Adventure
start_room: start

rooms:
  - id: start
    name: Starting Room
    description: A small room with one exit to the north.
    exits:
      north: hallway

  - id: hallway
    name: Dark Hallway
    description: A long, dark hallway.
```

Das war's. Der Worldbuilder füllt den Rest mit Defaults.

---

## Room (Raum)

| Feld | Typ | Default | Beschreibung |
|---|---|---|---|
| `id` | String | **required** | Eindeutige ID, Referenzen in exits |
| `name` | String | **required** | Anzeigename |
| `description` | String / Object | `""` | Raumbeschreibung. String → `{default: ...}`; Object → `{default, variants}` (siehe CondText) |
| `exits` | Object | `{}` | `{ richtung: zielraum }` oder `{ richtung: { to: ziel, locked_by: entity } }` |
| `tags` | [String] | `[]` | `"dark"`, `"safe"`, `"vehicle", benutzbar in Predicates |
| `light_flag` | String | — | Wenn gesetzt und `"true"`, wird ein `dark`-Raum erhellt |
| `on_enter` | [AActionOutcome] | — | Effekte beim Betreten |
| `on_look` | [AActionOutcome] | — | Effekte beim Anschauen |
| `on_exit` | [AActionOutcome] | — | Effekte beim Verlassen |
| `search` | [AActionOutcome] | — | Effekte bei `search` |
| `ascii` | String | — | ASCII-Art-Banner (optional) |

### Beschreibung mit Varianten (CondText)

```yaml
description:
  default: "Der Brunnen ist trocken."
  variants:
    - when: { has_flag: water_shrine_active }
      text: "Klares Wasser sprudelt im Brunnen."
```

---

## Item (Gegenstand)

| Feld | Typ | Default | Beschreibung |
|---|---|---|---|
| `id` | String | **required** | Eindeutige ID |
| `name` | String | **required** | Anzeigename |
| `description` | String / Object | `""` | CondText (siehe Room) |
| `keys` | [String] | `[]` | Aliase für Autovervollständigung |
| `tags` | [String] | `[]` | `lightsource`, `weapon`, `vehicle`, … |
| `location` | String | `"start"` | Start-Raum-ID |
| `state` | String | `"intact"` | Start-Status |
| `equip_slot` | String | — | `weapon`, `body`, `accessory` |
| `equip_effects` | [String] | `[]` | `attack+5`, `defense+3`, `maxhp+20` |
| `hidden` | Bool | `false` | Nur via `search` findbar |
| `discover` | String | — | Text bei Entdeckung |
| `props` | Object | `{}` | `{ uses: 3 }` — Integer-Eigenschaften |
| `on_take` | [AActionOutcome] | — | Effekte beim Aufheben (wird in `verb_map` gemerged) |
| `verb_map` | Object | `{}` | `{ "verb,state": [effects] }` |

---

## AActionOutcome (Effekte im YAML)

| Kurzform | Effekt |
|---|---|
| `"text"` oder `{ msg: "Hallo" }` | SendMessage |
| `{ heal: 10 }` | ModifyValue VRPlayerHealth +10 |
| `{ damage: 5 }` | ModifyValue VRPlayerHealth -5 |
| `{ give: item_id }` | MoveEntity to CarriedBy "player" |
| `{ consume: item_id }` | MoveEntity Removed |
| `{ set_flag: name, val: "true" }` | SetValue (VRFlag name) "true" |
| `{ check_flag: name, then: [...], else: [...] }` | Conditional HasFlag |
| `{ start_quest: id }` | QuestOp StartQuest |
| `{ advance_quest: id }` | QuestOp AdvanceQuest |
| `{ complete_quest: id }` | QuestOp CompleteQuest |
| `{ equip: item_id }` | MoveEntity to EquippedBy "player" "weapon" |
| `{ move: room_id }` | SetValue (VRProperty "player" "room") — bewegt den Spieler |
| `{ move_npc: npc_id, to: room_id }` | MoveEntity — bewegt einen NPC |
| `{ game_end: victory }` / `{ game_end: death, msg: "…" }` | GameEnd |
| `{ if: <predicate>, then: […], else: […] }` | Conditional — Prädikat-gesteuerter Zweig |
| `{ msg: "Text", then: [...], else: [...] }` | Sequence [SendMessage, Conditional...] |
| `{ standing: { faction: id, add: N } }` | ModifyValue (VRVariable "faction.id") +N — Module 7a |
| `{ standing: { faction: id, set: N } }` | SetValue (VRVariable "faction.id") N — Module 7a |
| `{ set_state: entity, to: state }` | SetValue (VRProperty entity "state") — z. B. `locked_by`-Tore öffnen |

Item-Felder für Container:

| Feld | Typ | Beschreibung |
|---|---|---|
| `in_container` | String | ID des Container-Items; das Item startet darin (`InContainer`). Mit `search <container>` herausnehmbar. |

---

## NPC

| Feld | Typ | Default | Beschreibung |
|---|---|---|---|
| `id` | String | **required** | Eindeutige ID |
| `name` | String | **required** | Anzeigename |
| `description` | String / Object | `""` | CondText (siehe Room) |
| `keys` | [String] | `[]` | Aliase |
| `location` | String | `"start"` | Start-Raum |
| `state` | String | `"alive"` | Start-Status |
| `max_hp` | Int | — | Maximale Trefferpunkte (optional) |
| `attack` | Int | 0 | Angriffswert |
| `defense` | Int | 0 | Verteidigungswert |
| `dialogue` | Object | `{}` | Dialogbäume (siehe unten) |
| `verb_map` | Object | `{}` | `{ "verb,state": [effects] }` |

### Dialogue Tree

```yaml
dialogue:
  alive:   # Status, in dem dieser Dialog aktiv ist
    entry: greeting
    nodes:
      greeting:
        text: "Hallo, Reisender!"
        choices:
          - text: "Wer bist du?"
            next: who
          - text: "Tschüss."
  who:
    text: "Ich bin der Wächter."
    choices:
      - text: "Leb wohl."
```

---

## Quest

| Feld | Typ | Default | Beschreibung |
|---|---|---|---|
| `id` | String | **required** | |
| `name` | String | **required** | |
| `desc` | String | `""` | |
| `prereqs` | [String] | `[]` | Flag-Namen, die gesetzt sein müssen |
| `stages` | [Object] | **required** | `[{ id, text, hint? }]` |
| `reward` | [Effect] | — | Effekte beim Abschluss |
| `on_complete` | String | — | QuestID, die dann startet |

---

## Vehicle

```yaml
- id: carriage
  name: carriage
  desc: "A horse-drawn carriage."
  entry_room: meadow
  stops: [meadow, village, forest]
  fuel: { prop: hay, max: 10 }
  conditions:
    broken_wheel: "Wheel wobbles dangerously."
```

---

## Variables

```yaml
variables:
  - name: mana
    type: int         # int, bool, text, enum
    initial: 50
    min: 0
    max: 100
```

## initial_variables / initial_flags / active_quests

```yaml
initial_variables:
  mana: 30          # überlagert declared initial
initial_flags:
  started: "true"
active_quests:
  - find_treasure
```

## Player

```yaml
player:
  max_hp: 50
  attack: 8
  defense: 3
  skills:
    lockpick: 5
```

## Verbs (Custom)

```yaml
verbs:
  - name: cast
    aliases: [magic, zauber]
```

## Rules (Trigger)

```yaml
rules:
  - id: falltrap
    on: enter trap_room
    when: { not: { has_flag: trap_disarmed } }
    effects:
      - damage: 5
      - msg: "A spike trap wounds you!"
    once: true
```

Events: `enter room`, `leave room`, `look room`, `search room`, `take item`, `drop item`, `use item`, `state entity`, `command verb`, `custom name`, `turn`.

## Interactions (Item-auf-Entity / Crafting)

```yaml
interactions:
  # use <item> on <target> -> setzt den Ziel-Status (z. B. Truhe/Exits entriegeln)
  entity:
    - {item: key, target: door, state: unlocked, msg: "The key turns."}
  # Item-auf-Item (Crafting): use <item1> on <item2>
  item:
    - item1: herb
      item2: mortar
      effects:
        - {msg: "You grind the herb into a paste."}
        - {set_flag: paste_made, val: "true"}
```

## Factions (Standing — Module 7a)

```yaml
factions:
  - id: smugglers
    name: Schmuggler-Gilde
    initial: 0                # Start-Standing (Default 0)
    levels:                    # Autoren-Hinweise (Schwellen-Metadaten)
      - { at: 20,  name: ally }
      - { at: -20, name: hunted }
```

Jede Faktion erzeugt die Variable `faction.<id>` (int) mit dem Startwert
`initial`. Standings werden über Effekte verändert und über Prädikate abgefragt
— siehe `docs/modules.md`:

- Effekt: `{ standing: { faction: smugglers, add: 20 } }` bzw. `{ set: N }`
- Prädikat: `standing: { faction: smugglers, at_least: 20 }` (auch
  `at_most`, `equals`) — einsetzbar überall dort, wo Prädikate stehen
  (`when:`, `visible_when:`, `if:`).

**Fehler:** doppelte Faktions-IDs; `faction.X` als gewöhnliche Variable
deklariert; Referenz auf nicht deklarierte Faktion (wenn das `factions:`-Segment
existiert).

---

## Encounter-Tabellen (Module 7c)

Gewichtete Zufallsereignisse: jede Tabelle kompiliert zu einem Trigger
(`encounter.<id>`), dessen Effekt ein `RandomChoice` über die gewichteten
Einträge ist. `on`, `when` und `cooldown` sind normale Triggerfelder.

```yaml
encounter_tables:
  - id: wilds
    on: turn                      # wie Rules: turn / enter <room> / command <verb> …
    cooldown: 2                   # nach dem Feuern N Feuerversuche stumm
    when: { not: { has_flag: camp_cleared } }
    entries:
      - { weight: 3, effects: [ { msg: "Ein Wolf!" }, { damage: 1 } ] }
      - weight: 1                 # optionales Gate pro Eintrag
        when: { has_item: torch }
        effects: [ { msg: "Glühwürmchen." } ]
```

- `weight` muss eine positive ganze Zahl sein; `cooldown` Default 0 (kein
  Cooldown). Gezogen wird aus dem Save-`rngState` — gleicher Seed →
  identische Zugfolge.
- **Fehler:** `DuplicateEncounterTable` (doppelte IDs), `EmptyEncounterTable`
  (keine Einträge), `BadEncounterWeight` (Gewicht < 1).

---

## Environment: Wetter und Drains (Module 7d)

Automatik pro Zug: Wetterübergänge und Variablen-Drains (Hunger, Temperatur…).

```yaml
environment:
  weather:
    states: [clear, storm]
    initial: clear
    transitions:
      - when: { compare_var: { name: day, op: gte, value: 3 } }
        to: storm
        effects: [ { msg: "Ein Sturm zieht auf." } ]
  drains:
    - { var: hunger, per_turn: -1, when: { not: { has_flag: fed } },
        at_zero: [ { game_end: death, msg: "Du verhungerst." } ] }
```

- Wetter wird als Int-Variable `env.weather` gespeichert (Index in `states`,
  `initial` bestimmt den Start; der Namespace `env.*` ist reserviert).
  Raumtexte wechseln per CondText-Varianten, z. B.
  `when: {compare_var: {name: env.weather, op: eq, value: 1}}`.
- Jede Transition kompiliert zu einem `on: turn`-Trigger (`when` als
  Bedingung); jeder Drain zu `environment.drain.<var>` mit `ModifyValue
  per_turn` + `Conditional (var ≤ 0)` auf die `at_zero`-Effekte.
- **Fehler:** `UnknownWeatherState` (initial/`to` nicht in `states`),
  `UnknownDrainVariable` (Drain-Var nicht deklariert),
  `EnvironmentVariableClash` (Autor deklariert `env.*`).

---

## Stealth: Lärm und Beobachter (Module 7e)

Lärm erzeugen und Wachen reagieren lassen — alles auf Trigger/Variablen.

```yaml
stealth:
  noise:
    var: noise        # Name der Lärm-Variable (Default "noise")
    on_move: 2        # +Lärm bei jedem Raumwechsel
    decay_per_turn: -1
    max: 10
  observers:
    - npc: guard
      hears_at: 5
      cooldown: 3     # optional: N Turns bis Wache wieder hört (Default 0)
      on_hear: [ { set_flag: alarmed, val: "true" } ]
```

- Kompiliert zu: einer Int-Variable, einem `on: enter <raum>`-Trigger je Raum
  (Clamp auf `max`), einem `on: turn`-Observer je NPC (`compare_var noise
  gte hears_at`, `cooldown` als Trigger-Cooldown) und einem letzten
  `on: turn`-Decay-Trigger (Clamp auf 0). Beobachter laufen VOR dem Decay.
- Schleichen = beliebiges Custom-Verb + `on: command`-Regel, das die
  Variable senkt (7b-Muster); Dunkelheit/Wahrnehmung via `tags: [dark]`,
  `light_flag` und `lightsource`-Items — alles bestehende Prädikate.
- **Fehler:** `UnknownObserverNPC` (NPC nicht unter `npcs:` deklariert),
  `StealthVariableClash` (Autor deklariert die noise-Variable separat).

---

## Combat: Kampfprofile (Module 7f)

Kampf ist eine über Daten gewählte Policy; ohne `combat:`-Block gilt
`classic` (exakt das Verhalten vor 7f).

```yaml
combat:
  profile: classic            # off | narrative | classic
  attack_refused: "..."       # off: Abgelehnt-Meldung (Default generisch)
  difficulty: 3               # narrative: Bonus auf die NPC-Verteidigung
  on_win:  [ ... ]            # narrative: Effekte bei Sieg
  on_lose: [ ... ]            # narrative: Effekte bei Niederlage
```

- `off`: `attack` wird abgelehnt (`attack_refused`, Default-Text), keine
  Seite verliert HP.
- `narrative`: ein Wurf `Spieler-Angriff >= NPC-Verteidigung + difficulty`;
  `on_win`/`on_lose` sind normale Effekt-Listen, die entscheiden, was
  passiert — keine automatische HP-Attrition.
- `classic`: bit-identisch zur Vor-7f-Logik (Schaden `attack − defense`,
  Konter im selben Befehl, Tod via HP ≤ 0). Default ohne `combat:`-Block.
- `tactical` ist bis Phase 7f-3 deaktiviert (`CombatProfileNotSupported`).
- **Fehler:** `UnknownCombatProfile` (unbekannter Name),
  `CombatProfileNotSupported` (tactical).

Umsetzung: `src/Combat.hs` — `resolveCombat :: CombatProfile ->
[CombatActor] -> CombatTarget -> GameState -> ([Effect], [String])`,
pure Effekt-Erzeugung durch den einen Interpreter.

Siehe `docs/modules.md` (7f) für Details und die Referenz-Fixtures.