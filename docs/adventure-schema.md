# Text-Adventure YAML Schema

## Minimal Adventure (einfachster Einstieg)

```yaml
name: My Adventure              # Titel — erscheint als Banner beim Spielstart
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
| `{ if: { has_flag: name }, then: [...], else: [...] }` | Conditional HasFlag — Flag-Test (ersetzt das entfernte, nie dekodierbare `check_flag`, P1-18) |
| `{ start_quest: id }` | QuestOp StartQuest |
| `{ advance_quest: id }` | QuestOp AdvanceQuest |
| `{ complete_quest: id }` | QuestOp CompleteQuest |
| `{ equip: item_id }` | MoveEntity to EquippedBy "player" |
| `{ move: room_id }` | SetValue (VRProperty "player" "room") — bewegt den Spieler |
| `{ move_npc: npc_id, to: room_id }` | MoveEntity — bewegt einen NPC |
| `{ game_end: victory }` / `{ game_end: death, msg: "…" }` | GameEnd |
| `{ if: <predicate>, then: […], else: […] }` | Conditional — Prädikat-gesteuerter Zweig |
| `{ msg: "Text", then: [...], else: [...] }` | Sequence [SendMessage, Conditional...] |
| `{ standing: { faction: id, add: N } }` | ModifyValue (VRVariable "faction.id") +N — Module 7a |
| `{ standing: { faction: id, set: N } }` | SetValue (VRVariable "faction.id") N — Module 7a |
| `{ set_state: entity, to: state }` | SetValue (VRProperty entity "state") — z. B. `locked_by`-Tore öffnen |
| `{ damage_npc: { npc: id, amount: N } }` | ModifyValue (VRProperty id "hp") −N — Module 7g |
| `{ narrative: ["Zeile 1", "Zeile 2"], then: [...] }` | Narrative — interaktive, seitenweise Ausgabe (`[Press Enter to continue]`); `then` sind Folge-Effekte nach der letzten Zeile |
| `{ condition: { name: id, turns: N, tick: [...], end: [...] } }` | ApplyCondition — zeitlich begrenzter Status-Effekt (`tick` je Zug, `end` beim Ablauf) |
| `{ clear_condition: id }` | ClearCondition — Status-Effekt vorzeitig entfernen |
| `{ skill: { name: id, delta: N } }` | ModifySkill — Skill um `N` verändern (auch negativ) |
| `{ random: [[gewicht, [effekte]], ...] }` | RandomChoice — gewichtete Zufallsauswahl (Gewicht ≥ 1) |
| `{ raise: name }` | RaiseEvent — feuert alle Regeln `on: custom <name>` (P1-20) |

Flags sind für Prädikate faktisch boolesch: `has_flag` prüft, ob ein Flag gesetzt
ist (`"true"`). Ein Vergleich gegen einen *anderen* String-Wert ist über Flags
nicht ausdrückbar — dafür gibt es Text-Variablen.

**Text-Variablen** (`variables:` mit `type: text`) werden mit `set_var` gesetzt
und mit `{ var: <name>, is: <text> }` abgefragt:

```yaml
variables:
  - { name: weather, type: text, initial: "klar" }
rules:
  - id: storm_check
    on: turn
    when: { var: weather, is: "sturm" }
    effects: [ { msg: "Der Sturm peitscht über den Pass." } ]
```

Der Vergleich ist exakt und verlangt eine **Text**-Variable: ein Int-Wert `1`
matcht *nicht* gegen `is: "1"`. `not` und die übrigen Verknüpfungen funktionieren
wie bei jedem Prädikat. Die Engine selbst nutzt genau diese Form für
`combat.action` / `combat.ability` (taktischer Kampf).

Das frühere `check_flag`-Kürzel wurde entfernt
(P1-18), weil es nie dekodierbar war und den Erwartungswert still verwarf.

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
| `party` | Object | — | Begleiter-Block (Module 7g, siehe unten) |

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
  fuel: { item: hay, max: 10 }
  conditions:
    broken_wheel: "Wheel wobbles dangerously."
```

Befehle: `enter/board <vehicle>`, `disembark` (nicht `exit` — das beendet
das Spiel), `drive to <label>`, `wait` (Automatikroute), `refuel`,
`repair <condition>`.

### Fahrzeug mit Systemen (Module 7h)

`systems:` und `stations:` machen aus einem Fahrzeug ein Schiff:

```yaml
vehicles:
  - id: kestrel
    name: Kestrel
    type: player
    entry_room: ks_bridge     # = cockpit
    cockpit: ks_bridge
    interior:
      - { id: ks_bridge, name: Brücke, desc: "…", exits: { north: {to: ks_engine} } }
      - { id: ks_engine, name: Maschinenraum, desc: "…" }
    stops:
      "Dock 7": ks_dock        # Kurzform: Label -> Außenraum
      "Zentrum":               # Langform mit Fahrpreis (P1-19)
        room: ks_center
        cost: { item: ticket, refused: "Ohne Ticket kommst du nicht mit." }
    start_stop: "Dock 7"
    systems:                   # -> VarMap ship.kestrel.<name>
      power:   { initial: 6, max: 6 }
      shields: { initial: 8, max: 8 }
      hull:    { initial: 12, max: 12 }
      weapons: { initial: 2, max: 5 }
    stations:                  # Interior-Raum + Verb, nur dort benutzbar
      - room: ks_engine
        verb: umleiten         # muss unter `verbs:` deklariert sein
        effects: [ { add_var: ship.kestrel.power, delta: 3 } ]
        when: { has_flag: reaktor_ok }   # optionales Zusatz-Gate
```

| Feld | Typ | Default | Beschreibung |
|---|---|---|---|
| `systems.<name>.initial` | Int | 0 | Startwert der Variable `ship.<vehicleId>.<name>` |
| `systems.<name>.max` | Int | 0 | Obergrenze (0 = keine); wird seit P1-8 beim Setzen/Ändern der Variable erzwungen |
| `stops.<label>` | String **oder** Object | **required** | Außenraum (Kurzform) oder `{ room, cost: { item, refused } }` (Fahrpreis, P1-19; `item` muss deklariert sein) |
| `stations[].room` | String | **required** | Interior-Raum des Fahrzeugs |
| `stations[].verb` | String | **required** | deklariertes Custom-Verb |
| `stations[].effects` | [Effekt] | `[]` | läuft wie jeder andere Effekt-Baum |
| `stations[].when` | Predicate | — | zusätzliches Gate |

- Das Verb wirkt **nur** im angegebenen Raum (der Compiler baut den Gate
  `{ at: player, room: <room> }`); überall sonst passiert nichts.
- Die vier Namen `power`, `shields`, `hull`, `weapons` haben Kampfbedeutung
  (siehe `docs/modules.md`, 7h) — andere Systemnamen sind freie Vorräte für
  Rules. Werte werden **nicht** automatisch geklemmt: Obergrenzen als
  `if: { compare_var: … gt N }, then: [ { set_var: …, value: N } ]` nachziehen.
- **Fehler:** `UnknownStationRoom`, `UnknownStationVerb`, `ShipVariableClash`.

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
    levels:                    # Schwellen-Metadaten (s. u.)
      - { at: 20,  name: ally }
      - { at: -20, name: hunted }
```

`levels:` ordnet Schwellen Namen zu. Die Liste wird nicht sortiert erwartet (die
Fixtures ordnen nach Beziehungsqualität, nicht nach Schwelle); der Compiler
lehnt aber **doppelte Schwellen** (`DuplicateFactionLevel`) und **leere Namen**
(`BadFactionLevel`) ab.

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
- **`cooldown` zählt passende Ereignisse, nicht Runden** (P2-14): heruntergezählt
  wird nur, wenn das Ereignis eintritt, an das die Regel gebunden ist.
  `cooldown: 5` auf `on: turn` bedeutet daher „fünf Runden", auf
  `on: enter <raum>` dagegen „die nächsten fünf Betretungen". Für `on: turn`
  fällt beides zusammen, für raumbezogene Ereignisse nicht. Ein zweites Feld für
  echte Zeit-Semantik (`cooldown_turns:`) gibt es bewusst **nicht** — es ist
  nicht implementiert; wer Rundenzählung braucht, bindet die Regel an `on: turn`.
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
      cooldown: 3     # optional: N passende Ereignisse stumm (Default 0)
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
  profile: tactical           # off | narrative | classic | tactical
  attack_refused: "..."       # off: Abgelehnt-Meldung (Default generisch)
  difficulty: 3               # narrative: Bonus auf die NPC-Verteidigung
  on_win:  [ ... ]            # narrative: Effekte bei Sieg
  on_lose: [ ... ]            # narrative: Effekte bei Niederlage
  initiative: by_speed        # tactical: player_first | enemy_first | by_speed
  flee_allowed: true          # tactical: Flucht per 'flee' erlaubt
  max_rounds: 10              # tactical: optionales Rundenlimit
  speed_attribute: agility    # tactical: Attribut für Initiative-Wurf
```

- `off`: `attack` wird abgelehnt (`attack_refused`, Default-Text), keine
  Seite verliert HP.
- `narrative`: ein Wurf `Spieler-Angriff >= NPC-Verteidigung + difficulty`;
  `on_win`/`on_lose` sind normale Effekt-Listen, die entscheiden, was
  passiert — keine automatische HP-Attrition.
- `classic`: bit-identisch zur Vor-7f-Logik (Schaden `attack − defense`,
  Konter im selben Befehl, Tod via HP ≤ 0). Default ohne `combat:`-Block.
- `tactical` (Phase 7f-3): rundenbasierter Taktikkampf. Befehle: `attack`,
  `defend`, `flee`, `use-ability <id>` / `ability <id>`. Rundenbegrenzung,
  Initiative-Wurf über `speed_attribute` und konfigurierbare Flucht.
  **Ein Kampf beginnt mit `attack`**: `defend` und `flee` greifen nur, solange
  `combat.engaged` gesetzt ist (sonst „You are not in combat.").
  **Was `defend` bewirkt** ist Autoren-Daten — der Resolver setzt nur
  `combat.action`; die Gegnerregel entscheidet. Beispiel (so macht es
  `combat-tactical.yaml`):

```yaml
rules:
  - id: counter            # Konter nur, wenn NICHT verteidigt wurde
    on: turn
    when:
      all:
        - compare_var: { name: combat.engaged, op: gte, value: 1 }
        - not: { var: combat.action, is: defend }
    effects:
      - { damage: 2 }
      - { msg: "Der Champion kontert und trifft dich für 2 Schaden!" }
  - id: counter_blocked    # Gegenstück: verteidigt → kein Schaden
    on: turn
    when: { all: [ { compare_var: { name: combat.engaged, op: gte, value: 1 } },
                   { var: combat.action, is: defend } ] }
    effects: [ { msg: "Du fängst den Hieb auf der Deckung ab — kein Schaden." } ]
```
- **Fehler:** `UnknownCombatProfile` (unbekannter Name),
  `UnknownInitiativeRule` (ungültige Initiativregel).
- **Reserviert: der `combat.`-Namespace** (Phase 7f-3). Der
  Rundenzustand eines taktischen Kampfes lebt in der VarMap (`combat.round`,
  `combat.engaged`, `combat.initiative.<actorId>`, `combat.action`,
  `combat.ability`) — wie `faction.`/`party.`/
  `ship.`, also kein neues Save-Feld und Save/Load gratis. Eine selbst
  deklarierte Variable mit diesem Prefix wird abgelehnt (`CombatVariableClash`).
  `combat.*` darf in Regeln frei per `compare_var`/`set_var`/`{ var: …, is: … }`
  verwendet werden; die Gegnerreaktion ist eine gewöhnliche `on: turn`-Regel mit
  `when: { compare_var: { name: combat.engaged, op: gte, value: 1 } }` — kein
  Engine-Sonderpfad.

Umsetzung: `src/Combat.hs` — `resolveCombat :: CombatProfile -> [CombatActor] ->
CombatTarget -> CombatAction -> GameState -> ([Effect], [String])`, pure
Effekt-Erzeugung durch den einen Interpreter. `CombatAction` sagt, was der Spieler
in der Runde tut (`CAAttack`/`CADefend`/`CAFlee`/`CAUseItem`/`CAAbility`);
`CombatTarget` unterstützt sowohl NPCs (`TargetNPC`) als auch feindliche
Schiffe (`TargetShip`, Phase 7h-2). Mit einem Begleiter in der
Gruppe enthält die Aktor-Liste zusätzlich `CompanionActor <npc>` (Module 7g),
an Bord eines Schiffs mit Systemen `ShipActor <ship>` (Module 7h).

---

## Abilities: Spieler-Fähigkeiten (Phase 7f-3)

Im taktischen Kampf (`profile: tactical`) kann der Spieler spezielle Fähigkeiten einsetzen:

```yaml
abilities:
  - id: power_strike
    name: Kraftschlag
    cost_var: stamina         # Ressourcen-Variable in der VarMap
    cost: 15                  # Benötigte Mindestmenge
    cooldown: 2               # Runden Abklingzeit
    effect:
      - { damage_npc: { npc: gladiator, amount: 20 } }
      - { msg: "Dein wuchtiger Hieb trifft den Champion!" }
```

In-game Aufruf: `use-ability power_strike` oder `ability power_strike`. Bei
Erfolg wird `cost` von `cost_var` abgezogen, der Cooldown gesetzt und die
Effektliste ausgeführt. Fehlen Ressourcen oder ist der Cooldown aktiv, wird die
Aktion abgelehnt.

---

## Party: Begleiter (Module 7g)

Ein NPC mit `party:`-Block lässt sich anwerben, folgt dem Spieler und kämpft im
`classic`-Profil als zusätzlicher Aktor mit.

```yaml
verbs:
  - { name: folgen, aliases: [follow] }

npcs:
  - id: alwin
    name: Knappe Alwin
    location: lager
    max_hp: 20
    attack: 4
    defense: 2
    party:
      can_join: true        # false: Block ist inert (kein Verb, keine Variable)
      order_verb: folgen    # Custom-Verb, das Beitritt/Verlassen toggelt
      hp_tracked: true      # verlangt max_hp
      follow_msg: "Alwin fällt hinter dir in Schritt."
      stay_msg: "Alwin bleibt zurück."
```

| Feld | Typ | Default | Beschreibung |
|---|---|---|---|
| `can_join` | Bool | `true` | `false`: der Block tut nichts (NPC bleibt unanwerbbar) |
| `order_verb` | String | `"follow"` | Custom-Verb aus `verbs:` (Core-Verben werden abgelehnt) |
| `hp_tracked` | Bool | `true` | Begleiter-HP wird geführt (verlangt `max_hp`) |
| `follow_msg` | String | generisch | Meldung beim Anwerben |
| `stay_msg` | String | generisch | Meldung beim Entlassen |

- **Mitgliedschaft = `VarMap`-Eintrag `party.<npcId>`** (1 = folgt). Der
  Compiler deklariert die Variable aus dem Block; sie darf **nicht** selbst
  unter `variables:` stehen (`PartyVariableClash`) — genau wie `faction.<id>`
  (7a) und `env.*` (7d). Kein neues Save-Feld: der Roster überlebt Save/Load
  automatisch.
- **Order-Verb:** ein Toggle. Derselbe Befehl (`folgen alwin`) setzt die
  Variable auf 1, ein zweiter Aufruf auf 0. Autoren können denselben
  `verb_map`-Schlüssel zusätzlich belegen; ihre Effekte laufen zuerst, der
  Toggle danach.
- **Folgen:** `followParty` verschiebt jeden lebenden Begleiter auf jedem
  Raumwechsel des Spielers mit (Gehen, Teleport, Ein-/Aussteigen und Fahrt im
  Vehicle). Tote Begleiter bleiben liegen.
- **Kampf:** im `classic`-Profil schlägt jeder Begleiter, der dort steht, wo
  das Ziel steht, mit (Schaden = `attack − defense`, min 1) — es sei denn, der
  Spieler hat das Ziel im selben Schlag schon getötet. Der Konter des Ziels
  trifft weiter den Spieler; `narrative`/`off` ignorieren Begleiter. Ohne
  Begleiter bleibt die Ausgabe bit-identisch zu 7f.
- **Tod ist ein Event:** ein toter Begleiter feuert `OnStateChange <npc>`,
  also greift die normale `on: state <npc>`-Rule (Fixture: sein Tod öffnet per
  `set_state` das Runentor). Zum Auslösen von NPC-Schaden im YAML gibt es
  `damage_npc`.
- **Fehler:** `PartyOrderVerbUnknown` (Verb nicht deklariert oder Core-Verb),
  `PartyHealthMissing` (`hp_tracked` ohne `max_hp`), `PartyVariableClash`,
  `UnknownDamageNPC` (`damage_npc` auf unbekannten NPC).

Fixture: `examples/modules/party.yaml` („Der Knappenzug") — anwerben mit
`folgen alwin`, Wolfskampf mit zwei Aktoren, Steinschlag tötet den Knappen,
sein Tod öffnet das Runentor.

Siehe `docs/modules.md` (7f/7g) für Details und die Referenz-Fixtures.