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
| `exits` | Object | `{}` | `{ richtung: zielraum }`, `{ richtung: { to: ziel, locked_by: entity } }`, or `{ richtung: { to: ziel, when: predicate, msg: failure_text } }` (Guarded Exit, Phase 2.2) |
| `tags` | [String] | `[]` | `"dark"`, `"safe"`, `"vehicle"`, benutzbar in Predicates |
| `light_flag` | String | — | Wenn gesetzt und `"true"`, wird ein `dark`-Raum erhellt |
| `dark_msg` / `dark_message` | String | — | Eigene Meldung bei Dunkelheit (Default: `"It's pitch black. You can't see anything."`) |
| `on_enter` | [AActionOutcome] | — | Effekte beim Betreten |
| `on_look` | [AActionOutcome] | — | Effekte beim Anschauen |
| `on_exit` | [AActionOutcome] | — | Effekte beim Verlassen |
| `search` | [AActionOutcome] | — | Effekte bei `search` |
| `ascii` | String / Object | — | Zustandsabhängige ASCII-Kunst (String = fester Banner, Object = CondText; siehe unten) |

### Guarded Exits (`when:`, `msg:` / `message:`, Phase 2.2)

Exits can be dynamically guarded by an authored predicate condition and custom failure message:

```yaml
rooms:
  - id: courtyard
    exits:
      north:
        to: throne_room
        when:
          has_flag: pass_granted
        msg: "The castle guard blocks the northern archway: 'Halt! No entry without a royal permit.'"
```

- If `when:` evaluates to `true`, the player moves to the target room (`to:`).
- If `when:` evaluates to `false`, movement is blocked, the player stays in the current room, and the custom `msg` (or `message:`, or default `move.blocked`) is displayed.
- The failure message supports `{placeholder}` variable interpolations.
- In protocol snapshots, guarded exits are reported as locked (`locked: true`).

### Dunkelheit (`dark`, `light_flag`, `dark_msg`, `feelable`)

Ein Raum mit dem Tag `"dark"` gilt als dunkel, solange der Spieler kein getragenes
Item mit dem Tag `"lightsource"` besitzt und das im Raum konfigurierte `light_flag`
nicht auf `"true"` gesetzt ist.

Im Dunkeln gilt:
- `look` zeigt die Dunkelheitsmeldung (Standard: `"It's pitch black. You can't see anything."`
  bzw. der konfigurierte Text aus `dark_msg` / `dark_message`).
- Interaktionen auf Objekte im Raum (`take <item>`, `take all`, `examine <item/npc>`,
  `search`, `use <item>`, `use <getragen> on <raum-objekt>`) werden blockiert und geben
  die Dunkelheitsmeldung zurück — **außer** das Ziel trägt den Tag `"feelable"`.
- **`feelable`:** Ein Item mit diesem Tag lässt sich im Dunkeln ertasten und daher
  normal behandeln (`take`, `examine`, `search`, `use`). Der Autor entscheidet damit
  pro Item, was in einem unbeleuchteten Raum erreichbar ist — Fackel, Schlüssel, Hebel,
  gleich welches. `take all` nimmt im Dunkeln nur die `feelable`-Items mit; ziel-loses
  `search` sowie Interaktionen mit NPCs und Fahrzeugen bleiben gesperrt.
- Getragene Items im Inventar können weiterhin untersucht (`examine`), benutzt (`use`),
  kombiniert (`use X on Y`) und abgelegt (`drop`) werden.
- **Validierungs-Warnung (`DarkRoomDeadEnd`):** Wenn ein Raum als dunkel (`dark: true` oder Tag `"dark"`)
  deklariert ist und Items enthält (oder erreichbare dunkle Nebenräume Items enthalten), aber weder ein
  `light_flag` am Raum gesetzt ist, noch ein erreichbares Item mit dem Tag `"lightsource"` existiert und
  kein Item im Raum als `"feelable"` markiert ist, meldet der Worldbuilder eine nicht-fatale Warnung
  (`DarkRoomDeadEnd`). Dies warnt Autoren vor potenziellen Sackgassen, in denen Items unbeleuchtet und
  unertastbar gefangen sind.

### ASCII-Kunst (`ascii`)

Das Feld nimmt die Kunst als String wörtlich und gibt sie bei Spielstart und bei
`look` über der Beschreibung aus. Für mehrzeilige Kunst ist der **Block-Skalar**
die richtige Form:

```yaml
rooms:
  - id: halle
    name: Halle
    desc: Eine weite Halle.
    ascii: |
      /\_/\
      ( o.o )
      > ^ <
```

#### Zustandsabhängige Kunst (Phase B)

Wie `description` ist `ascii` ein **CondText**. Statt eines Strings kann ein
Object `{ default, variants }` stehen; die erste Variante, deren `when`-Prädikat
zutrifft, gewinnt, sonst der `default`. Die Auswertung passiert gegen den
aktuellen Spielzustand, ohne Engine-Sonderfall:

```yaml
rooms:
  - id: hoehle
    name: Höhle
    desc: Eine kalte Höhle.
    ascii:
      default: |
        +---------+
        |  ,---,  |
        +---------+
      variants:
        - when: { has_flag: lamp_lit }
          text: |
            *=========*
            | \o/  !! |
            *=========*
```

Dasselbe Feld gibt es auf **Items** (`ascii`) und **NPCs** (`ascii`). Eine
Item-Kunst wird bei `look at <item>`, eine NPC-Kunst bei `look at <npc>` über
der Beschreibung ausgegeben. Damit lässt sich z. B. ein Gegner lebend/tot anders
darstellen (`when: { state: troll, is: dead }`) oder eine Fackel erloschen:

```yaml
items:
  - id: fackel
    name: Fackel
    desc: Eine flackernde Fackel.
    ascii:
      default: "( * )\n \\|/"
      variants:
        - when: { state: fackel, is: burned }
          text: "( x )\n \\|/"
```

Lebend/tot ist damit auch **im Spiel** sichtbar: jede Kampfrunde gibt die Kunst
des Gegners aus, und zwar *nach* dem Anwenden der Runde — der tödliche Treffer
zeigt also bereits die `dead`-Variante. Ein getöteter NPC bleibt als Leiche im
Raum: `look at <name>` und `watch <name>` finden ihn weiter, gemeldet wird er aber
nicht mehr als anwesend — `look` schreibt *„The body of X lies here."*, `attack`
antwortet *„X is already dead."*, `talk` *„X is dead and says nothing."*.
Autoren-Verben aus `verb_map:` wirken weiterhin auf eine Leiche (z. B. `search`).

Ist `ascii` leer oder fehlt, wird nichts ausgegeben (rückwärtskompatibel zum
früheren `ascii: <string>`).

#### Bewegte Kunst (Phase D)

Ein `ascii`-Objekt kann `frames` (Liste von Frames) und `every` (Takt in Zügen)
enthalten. Jeder Frame ist selbst ein CondText, kann also zusätzlich vom
Spielzustand abhängen:

```yaml
rooms:
  - id: halle
    name: Halle
    desc: Eine Halle mit flackernder Fackel.
    ascii:
      frames:
        - |
          |~|
          | |
        - |
          |=|
          | |
      every: 2
```

Bei jedem `look` wird der passive Frame aus der Zugnummer gewählt
(`turnCount div every mod Anzahl`), rein und deterministisch aus dem
Spielzustand — kein Timer im Kern. `watch [ziel]` spielt **alle** Frames
nacheinander ab; die Verzögerung zwischen den Frames liegt nur im IO-Loop, die
Engine liefert die fertige Frame-Liste. `watch` ohne Ziel zeigt die Kunst des
Raums, `watch <item>`/`watch <npc>` die des Ziels. Ein `every: 0` schaltet den
passiven Takt ab (nur noch `watch`).

#### Ambiente-Loop mit eigener Rate (Phase H, H1)

Ein `ascii`-Objekt kann daneben einen `ambient`-Block tragen: Inline-Frames
plus die eigene Rate (`fps`, Frames pro Sekunde). Der Kern liefert Frames
**und** Rate als reine Funktion (`asciiPlayback`); das Frontend wartet — im
TUI läuft der Loop zeitgesteuert neben der Eingabe, im Haskeline-CLI spielt
`watch` ihn sequenziell ab:

```yaml
ascii:
  frames:          # Zug-Takt wie oben, bleibt unverändert daneben
    - |
      |~|
    - |
      |=|
  every: 2
  ambient:         # Zeit-Takt: die Kunst trägt ihre Rate
    frames:
      - |
        ~  ~  ~
      - |
         ~ ~ ~
    fps: 4
```

Regeln: `fps` muss positiv sein und `ambient.frames` darf nicht leer sein —
der Compiler meldet `AmbientFpsInvalid` bzw. `AmbientFramesEmpty`. Kunst ohne
`ambient` spielt bei `watch` mit der Standardrate (350 ms je Frame, der
früheren festen Konstante entsprechend); `ambient`-Kunst spielt **ihre**
Rate. In Pipes bleibt die Ausgabe statisch (D22).

#### Cutscenes: `clips`, `intro` und `play_clip` (Phase H, H4)

Cutscenes sind Clips: einmal abgespielte Frame-Folgen mit eigener Rate. Sie
liegen in einem eigenen `clips:`-Segment — inline (kleine Szenen) oder in einer
Begleitdatei (`file: art/pan.json`, JSON-Array von Strings), die der Compiler
beim Kompilieren **einbettet**; die kompilierte Welt braucht zur Laufzeit keine
weitere Datei (D14):

```yaml
clips:
  - id: strand-pan
    fps: 12
    file: art/pan.json        # oder inline `frames:` für kleine Szenen

rooms:
  - id: strand
    name: Strand
    desc: ...
    intro: strand-pan         # einmal beim Betreten
```

Zwei Auslöser (D19): `intro:` am Raum (beim Betreten) und der Effekt
`play_clip: <clip-id>` in Regeln/Auslösern — ein `play_clip` in `on_enter`
gewinnt gegen das `intro` des Raums. Im Haskeline-CLI läuft die Cutscene
sequenziell ab (blockierend, wie `watch`); im TUI wird sie im Kunst-Panel
in-place abgespielt und geht in den Ambiente-Loop der Szene über.

Validierung: `DuplicateClip` (doppelte id), `UnknownClip` (unbekannte id in
`intro:` oder `play_clip:` — geprüft in Regeln, Raum-Hooks, Dialogen und
Verb-Maps), `ClipFpsInvalid` (fps <= 0) und `ClipFramesEmpty` (keine Frames).
`fps` muss positiv sein, Frames dürfen nicht leer sein.

#### Material erzeugen: das `video2ascii`-Werkzeug (Phase F)

Für echte bewegte Kunst erzeugt `video2ascii` beide Formen oben aus einem
kurzen Video (ffmpeg/ffprobe laufen **nur** im Werkzeug als externe Prozesse,
D6 — die Engine und das TUI kennen kein ffmpeg):

```sh
# Ambient-Loop: 6 Frames gleichmäßig über 0–1,5 s, eigene Rate (~4 fps),
# Loop-Naht wird gemessen und gemeldet (D15); Ausgabe ist ein einfügbare
# `ambient:`-YAML-Snippet:
video2ascii --ambient --frames 6 --from 0 --to 1.5 --width 24 strand.mp4

# Cutscene: das ganze Material bei 6 fps als Clip-Datei (D14-Format,
# JSON-Array von Frame-Strings) plus das passende `clips:`-Snippet:
video2ascii --cutscene --fps 6 --width 32 strand.mp4 --out art/pan.json
```

Ambiente-Loops sind auf 4–30 Frames begrenzt (D11); die Naht-Report warnt,
wenn der letzte-erste-Frame-Abstand doppelt so sichtbar ist wie ein normaler
Frameschritt — dann Fenster härter schneiden oder eine Stelle wählen, die
dort wieder anfängt, wo sie aufhört.

#### Anfassbare Kunst: Hotspots (Phase E)

Ein `ascii`-Objekt kann `hotspots` enthalten: Marker-Glyphen, die an ein Ziel
(Item- oder NPC-ID) gebunden sind. Der Marker wird bei `look` **hervorgehoben**
(farbig, wird bei `--no-color`/Pipe automatisch entfernt); `map` (Alias
`legend`) gibt dieselbe Kunst mit **Nummern** statt Markern plus Legende aus.
Über die Nummer adressiert `look at <n>` das Ziel, der normale Name
funktioniert weiter:

```yaml
rooms:
  - id: hoehle
    name: Höhle
    desc: Eine Höhle mit Hebel und schlafendem Troll.
    ascii:
      default: |
        +----------+
        |   *  T   |
        +----------+
      hotspots:
        - { glyph: "*", target: lever }
        - { glyph: "T", target: troll }
```

```
> map
+----------+
|   1  2   |
+----------+

Legend:
  1: lever
  2: troll
> look at 1
A rusty lever, warm to the touch.
> pull lever
You pull the lever. Somewhere a chain rattles.
```

Regeln, die der Worldbuilder beim Kompilieren prüft:

- Das Ziel muss ein deklariertes Item oder NPC sein (`UnknownHotspotTarget`).
- Der Marker muss in der Kunst vorkommen (`HotspotGlyphMissing`).
- Marker sind innerhalb einer Kunst eindeutig (`DuplicateHotspotGlyph`).
- Ziffern und Leerraum sind reserviert (`ReservedHotspotGlyph`) — Ziffern
  werden für die Nummerierung gebraucht.

Zwei Format-Fallen bei Block-Skalaren:

- Die **erste** Zeile muss die am geringsten eingerückte sein. YAML nimmt ihre
  Einrückung als Blockeinrückung; eine spätere Zeile mit *weniger* Einrückung
  macht die Datei ungültig (der Compiler meldet einen Parse-Fehler). Kunst, die
  oben schmal und unten breit ist, gehört deshalb so ausgerichtet, dass die
  oberste Zeile den kleinsten Rand hat.
- Die gemeinsame Einrückung wird beim Einlesen **entfernt**; die *relative*
  Einrückung innerhalb der Kunst bleibt erhalten. Kunst also nie mit der
  absoluten Spalte planen, sondern mit den Leerzeichen, die sie *innerhalb* der
  Figur braucht.

Erzeugt wird die Kunst mit dem Werkzeug `img2ascii` (eigenes Paket im Repo):

```
cabal run img2ascii -- -w 60 bild.png          # Zeichen-Rampe, 60 Zeichen breit
cabal run img2ascii -- -H 20 bild.png          # 20 Zeilen, Breite abgeleitet
cabal run img2ascii -- -w 60 -m half bild.png  # Halbblock, zwei Pixel pro Zelle
cabal run img2ascii -- --color -w 60 bild.png  # Zeichen-Rampe in 24-Bit-Farbe
```

`--color` färbt die Zeichenrampe mit 24-Bit-ANSI-Farben (opt-in), `--no-color`
schaltet sie wieder ab. Im Halbblock-Modus ist Farbe strukturell und daher
standardmäßig an; `--no-color -m half` erzeugt reine `▀`-Zeichen ohne Schattierung.
Die Kunst wird **mit** den ANSI-Sequenzen als String in die Welt geschrieben.
Die Engine gibt sie wörtlich aus und entfernt die Sequenzen, wenn stdout kein
Terminal ist oder `text-adventure --no-color` gesetzt wurde — der Kern kennt
keine Farbe.

Die Zeichenzelle eines Terminals ist etwa doppelt so hoch wie breit; das
Werkzeug leitet die Zeilenzahl daraus ab, damit ein quadratisches Bild
quadratisch bleibt. `-m half` braucht einen Farbterminal und trägt zwei Pixel je
Zelle (doppelte Vertikalauflösung, dafür keine reine Textausgabe).

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
| `tags` | [String] | `[]` | `lightsource`, `feelable`, `weapon`, `vehicle`, … |
| `location` | String | `"start"` | Start-Raum-ID |
| `state` | String | `"intact"` | Start-Status |
| `equip_slot` | String | — | `weapon`, `body`, `accessory` |
| `equip_effects` | [String] | `[]` | `attack+5`, `defense+3`, `maxhp+20` |
| `hidden` | Bool | `false` | Nur via `search` findbar |
| `discover` | String | — | Text bei Entdeckung |
| `props` | Object | `{}` | `{ uses: 3 }` — Integer-Eigenschaften |
| `on_take` | [AActionOutcome] | — | Effekte beim Aufheben (wird in `verb_map` gemerged als historischer `take`-Eintrag) |
| `verb_map` | Object | `{}` | `{ "[before:\|instead:]verb[,state]": [effects] }` — Phasen siehe Veto Stufe 2 |
| `ascii` | String / Object | — | Zustandsabhängige ASCII-Kunst (CondText, siehe Room) |

- **Validierungs-Warnung (`KeywordCollision`):** Teilen sich zwei Gegenstände oder ein Gegenstand und ein NPC im selben Raum dieselben Keywords (Namen, IDs oder `keys`-Aliase), meldet der Worldbuilder eine nicht-fatale Warnung (`KeywordCollision`). Dadurch wird frühzeitig auf mehrdeutige Spielerbefehle wie `take <name>` oder `examine <name>` hingewiesen.

---

## AActionOutcome (Effekte im YAML)

| Kurzform | Effekt |
|---|---|
| `"text"` oder `{ msg: "Hallo" }` | SendMessage |
| `{ heal: 10 }` | ModifyValue VRPlayerHealth +10 |
| `{ damage: 5 }` | ModifyValue VRPlayerHealth -5 |
| `{ give: item_id }` | MoveEntity to CarriedBy "player" (String-Form) |
| `{ give: {item: id, to: actor} }` | MoveEntity to CarriedBy actor (B7: `"player"` oder NPC-ID) |
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
| `{ condition: { name: id, turns: N, tick: [...], end: [...], hidden: bool } }` | ApplyCondition — timed condition/status effect (`tick` each turn, `end` upon expiration, optional `hidden: true` suppresses status/HUD display) |
| `{ clear_condition: id }` | ClearCondition — remove a condition/timer early |
| `{ skill: { name: id, delta: N } }` | ModifySkill — Skill um `N` verändern (auch negativ) |
| `{ random: [[gewicht, [effekte]], ...] }` | RandomChoice — gewichtete Zufallsauswahl (Gewicht ≥ 1) |
| `{ random: {stream: name, choices: [[gewicht, [effekte]], ...]} }` | RandomChoiceOn (B8) — gewichtete Zufallsauswahl auf dem benannten Zufallsstrom `rng.name` (s.u.) |
| `{ raise: name }` | RaiseEvent — feuert alle Regeln `on: custom <name>` (P1-20) |
| `{ sfx: "pfad/datei.wav" }` | PlaySfx — Sound-Effekt einmalig asynchron abspielen (Audio Phase 1) |
| `{ music: "pfad/datei.xm" }` | PlayMusic — Hintergrundmusik-Loop starten/wechseln (Audio Phase 2; `.xm`, `.mid`, `.wav/.ogg/.mp3`) |
| `{ stop_music: true }` | StopMusic — Hintergrundmusik stoppen (Audio Phase 2) |
| `{ block: "reason" }`, `{ block: true }`, or `{ block: { msg: "…", turn: true } }` | Block — Veto a command before execution (Phase 2.2). Stops command action. Default: `turn: false` (no turn consumed). Opt-in `turn: true` advances turn and ticks conditions. |

Flags sind für Prädikate faktisch boolesch: `has_flag` prüft, ob ein Flag gesetzt
ist (`"true"`). Ein Vergleich gegen einen *anderen* String-Wert ist über Flags
nicht ausdrückbar — dafür gibt es Text-Variablen.

**Benannte Zufallsströme (B8):** `random:` zieht standardmäßig aus dem
unbenannten Default-Strom — die alte Listenform bleibt unverändert (byte-identisch),
`encounter:`-Tabellen ebenfalls. Mit der Objektform
`random: {stream: <name>, choices: …}` zieht die Auswahl aus dem eigenen Strom
`<name>`: Ziehungen auf einem Strom verschieben weder den Default-Strom noch
andere Ströme — Inhaltsänderungen an einem System (Loot, Wetter …) bleiben ohne
Wirkung auf die Zufallsfolge eines anderen. Der Zustand eines Stroms lebt in der
VarMap unter `rng.<name>` (Hex-Text) und wird beim ersten Zugriff aus
Name-Hash + aktuellem Default-Strom initialisiert (ohne den Default-Strom zu
verbrauchen; gleicher Name = gleiche Folge, verschiedene Namen = verschiedene
Folgen). `rng.*` ist ein Engine-Reservierter Namensraum: `set_var`,
`set_text_var`, `add_var` und `compute_var` darauf, `variables:`-Deklarationen,
`initial_variables:`-Einträge und Prozedur-Parameter sind Compile-Fehler
(`RngVarWrite`) — sonst wäre die Reproduzierbarkeit deterministischer Läufe
(Fuzzer-Funde, `tests:`-Marker) nicht garantiert.

**Text-Variablen** (`variables:` mit `type: text`) werden mit
`{ var: <name>, is: <text> }` abgefragt; ihren Startwert setzt die Deklaration
(`initial:`) oder `initial_variables:`:

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

**Schreiben zur Laufzeit:** Text-Variablen setzt ausschließlich die Engine
(`combat.action` / `combat.ability`). Ein Autoren-Effekt dafür fehlt — `set_var`
nimmt nur Ganzzahlen (`AOSetVar String Int`), ein
`{ set_var: weather, value: "sturm" }` ist deshalb ein Schemafehler
(„Unknown outcome type"). Für Zustände, die zur Laufzeit umschalten sollen,
bleiben heute numerische Variablen (`compare_var`) oder Flags. Den *Startwert*
darf man über `initial:` in der Deklaration oder über `initial_variables:`
setzen (dort sind Strings erlaubt).

Das frühere `check_flag`-Kürzel wurde entfernt
(P1-18), weil es nie dekodierbar war und den Erwartungswert still verwarf.

### Conditions and Timers (Phase 2.1)

Conditions represent active status effects or countdown timers on the player:

- **Applying a Condition (`condition:` Outcome):**
  ```yaml
  - condition:
      name: bomb_fuse
      turns: 4
      hidden: true          # optional: when true, hidden from 'stats' command and TUI HUD
      tick:
        - msg: "The fuse burns down."
      end:
        - msg: "BOOM!"
        - game_end: defeat
  ```
  `hidden: true` allows running background timers or invisible debuffs without cluttering the player's status display.

- **Querying Active Conditions (`has_condition` Predicate):**
  Check whether a condition is currently active on the player:
  ```yaml
  when:
    has_condition: bomb_fuse
  ```
  Evaluates to `true` while the condition is active, and `false` once expired or cleared.

- **Checking Remaining Turns (`condition_turns` ValueRef):**
  Inspect the remaining turns of an active condition in comparisons (`compare` / `condition_turns:`):
  ```yaml
  when:
    compare:
      lhs:
        condition_turns: bomb_fuse
      op: "<="
      rhs: 2
  ```
  Or using the shorthand syntax:
  ```yaml
  when:
    condition_turns:
      condition: bomb_fuse
      op: "<="
      value: 2
  ```
  If the condition is not currently active, `condition_turns` evaluates to `0`.

- **Clearing a Condition (`clear_condition` Outcome):**
  ```yaml
  - clear_condition: bomb_fuse
  ```

Item-Felder für Container und NPC-Besitz:

| Feld | Typ | Beschreibung |
|---|---|---|
| `in_container` | String | ID des Container-Items; das Item startet darin (`InContainer`). Mit `search <container>` herausnehmbar. |
| `carried_by` | String | B7: NPC-ID (oder `"player"`); das Item startet in dessen Besitz (`CarriedBy`). Nicht zusammen mit `in_container`. |

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
| `verb_map` | Object | `{}` | `{ "[before:\|instead:]verb[,state]": [effects] }` — Phasen siehe Veto Stufe 2 |
| `ascii` | String / Object | — | Zustandsabhängige ASCII-Kunst (CondText, siehe Room) |
| `party` | Object | — | Begleiter-Block (Module 7g, siehe unten) |
| `topics` | Object | `{}` | Topic-Tabelle für `ask`/`tell` (Key=Topic, Value=Effekt) |
| `barks` | [Object] | `[]` | Ambient-One-Liner (Compiler-Sugar für `on: turn` mit Cooldown) |
| `on_talk` | Effekt | — | Zusätzlicher Effekt bei jedem `ask`/`tell` auf diesen NPC (Compiler-Sugar für `on: talk`) |

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
            outcome:
              - dialogEnd
      who:
        text: "Ich bin der Wächter."
        choices:
          - text: "Leb wohl."
            outcome:
              - dialogEnd
```

- `nodes.<id>.text` — Text des Knotens.
- `choices` — Liste von Optionen.
  - `text` — Anzeigetext.
  - `next` — ID des nächsten Knotens (optional; bei Fehlen oder `dialogEnd` endet der Dialog).
  - `visible_when` — Prädikat, das erfüllt sein muss, damit die Option angezeigt wird (optional).
  - `outcome` — Effekte, die bei Wahl dieser Option ausgeführt werden (optional; kann `say_node: <id>` oder `dialogEnd` enthalten).

#### Dialogue Sugars (`say_node`, `dialogEnd`)

- `say_node: <node-id>` — Setzt den aktiven Dialogknoten auf `<node-id>` (Sugar für `set_var dialog_node`).
- `dialogEnd: true` — Beendet den Dialog, indem `dialog_node` geleert wird (Sugar für `set_var dialog_node` mit leerem String).

### Topics, Barks und `on_talk` (4.5)

#### Topics (`topics:`)

Eine Tabelle von Themen, die der Spieler mit `ask <npc>` / `tell <npc>` über ein Thema ansprechen kann:

```yaml
npcs:
  - id: wirt
    name: Wirt
    topics:
      bier: "Unser Bier ist das beste im ganzen Land!"
      preis: "Ein Krug kostet zwei Kupfermuenzen."
```

- Key = Topic-Name (wird case-insensitiv gematcht).
- Value = Effekt (String wird zu `msg:`, komplexe Effekte als Liste/Object).
- `ask wirt about bier` und `tell wirt about bier` führen denselben Effekt aus.
- Unbekanntes Topic → `dialog.no_topic` (Template: `{npc} has nothing to say about {topic}.`).
- Unbekannter NPC → `dialog.no_npc` (`There is no one by that name.`).

#### Barks (`barks:`)

Ambient-One-Liner, die der NPC zufällig in den Raum wirft (Compiler-Sugar für `on: turn`-Trigger mit Cooldown):

```yaml
    barks:
      - text: "Der Wirt wischt ein Glas ab."
      - text: "Kauft euch was, setzt euch!"
        cooldown: 5
        when: { has_flag: offen }
```

- `text` — Nachricht (required).
- `cooldown` — Züge zwischen zwei Ausgaben (Default 20).
- `when` — Prädikat, unter dem der Bark feuern darf (optional).
- Der Compiler erzeugt `bark.<npc>.N`-Trigger (`on: turn`, `SendMessage`).

#### `on_talk:`

Ein Effekt, der bei jedem `ask`/`tell` über diesen NPC zusätzlich zum Topic-Effekt feuert (Compiler-Sugar für `on: talk`-Trigger):

```yaml
    on_talk:
      msg: "Der Wirt nickt freundlich."
```

- Kompiliert zu einem Trigger `talk.<npc>` mit `on: talk`.
- Feuert für jedes `ask`/`tell`-Kommando, das auf diesen NPC zielt — unabhängig davon, ob das Topic bekannt ist.
- Globale `on: talk`-Regeln (`rules:`) feuern ebenfalls; `talk.<npc>` filtert auf den NPC.

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

### Variablen vergleichen (`compare_var`)

`compare_var` vergleicht eine Variable mit einem **Literal** oder mit einer
**anderen Variable**:

```yaml
# gegen ein Literal
when: { compare_var: { name: gold, op: gte, value: 100 } }

# gegen eine andere Variable (Langform des Schlüssels: `other_var`)
when: { compare_var: { name: gold, op: ">=", var: kosten } }
```

- `op` akzeptiert `eq`/`ne`/`lt`/`lte`/`gt`/`gte` **oder** die Symbole
  `=` / `!=` / `<` / `<=` / `>` / `>=`.
- Verglichen werden **Ganzzahlen**. Text-Variablen prüft man mit
  `{ var: <name>, is: <text> }`.
- Der Winkel-Schlüssel `gte_var` o. ä. existiert **nicht** — die Variable steht
  unter `var:` (`other_var:` ist die gleichwertige Langform).
- In der kompilierten Welt steht dafür die generische Form
  `{ compare: { lhs, op, rhs } }` mit `ValueRef`s; `compare_var … var:` ist der
  Zucker darauf.

### Formeln & dynamische Berechnungen (`compute_var`)

Mit `compute_var` können Variablen zur Laufzeit durch mathematische Formeln berechnet
und aktualisiert werden:

```yaml
# In Outcomes / Effects:
- compute_var:
    var: gold
    expr: "gold - (menge * stueckpreis)"
```

#### Unterstützte Formel-Syntax (AST-Operatoren)
Der mathematische Ausdruck (`expr`) unterstützt ganzzahlige Arithmetik (64-Bit `Int`):
- **Grundrechenarten:** `+` (Addition), `-` (Subtraktion), `*` (Multiplikation),
  `/` (ganzzahlige Division `div`), `%` (Modulo)
- **Operator-Vorrang:** Punktrechnung (`*`, `/`, `%`) bindet stärker als Strichrechnung (`+`, `-`)
- **Klammerung:** Beliebig tief geschachtelte Klammern `( ... )`
- **Unäre Vorzeichen:** `-wert`, `+wert`
- **Funktionen:**
  - `min(a, b)`: Liefert das Minimum zweier Ausdrücke
  - `max(a, b)`: Liefert das Maximum zweier Ausdrücke
  - `clamp(lo, hi, val)`: Begrenzt `val` auf den Bereich `[lo, hi]`
- **Nullteiler-Sicherheit:** Division und Modulo durch 0 liefern deterministisch `0` (kein Absturz)
- **Variablen-Auflösung:**
  - Beliebige Spielvariablen (`gold`, `kosten`, `stadt.einkommen`)
  - Parametrisierte Befehlsargumente (`cmd.arg1`, `cmd.arg2`, `cmd.count`)
  - Systemwerte: `player.hp`, `player.max_hp`, `turn.count`

### Parametrisierte Befehle (`cmd.argN`, `cmd.count`, `cmd.raw_args`)

Custom-Verben können mit nachfolgenden Argumenten eingegeben werden (z. B. `kaufe 50 land`
oder `steuern 15`). Vor dem Ausführen von `on: command`-Triggern bindet die Engine
automatisch temporäre Variablen in den Spielzustand:

- `cmd.verb`: Der kanonische Name des aufgerufenen Verbs (z. B. `"kaufe"`).
- `cmd.arg1`, `cmd.arg2`, …: Einzelne Token nach dem Verb. Zahlen werden automatisch
  als ganzzahlige `VTInt` typisiert, Wörter als `VTText`.
- `cmd.count`: Anzahl der übergebenen Argumente (als `VTInt`).
- `cmd.raw_args`: Der gesamte unzerlegte Rest-String nach dem Verb.

### Text-Interpolation (`{var:name}` / `{name}`)

Texte in Dialogen, Raumbeschreibungen, Ereignismeldungen (`msg`/`text`) und
ASCII-Art können dynamisch Variablen und Spielwerte einbetten:

- `{name}` oder `{var:name}`: Ersetzt den Platzhalter durch den aktuellen Wert der Variable.
- **Vorzeichen-Modifikator:** `{name:+}` erzwingt bei nicht-negativen Zahlen ein Pluszeichen
  (z. B. `+15`, `-8`).
- **Breiten-Padding:**
  - `{name:6}`: Rechtsbündig auf 6 Zeichen formatiert (ideal für Tabellen & Dashboards).
  - `{name:-6}`: Linksbündig auf 6 Zeichen formatiert.
- **Systemvariablen:**
  - `{player.hp}` / `{player.max_hp}`: Aktuelle und maximale Lebenspunkte.
  - `{turn.count}`: Bisher vergangene Züge.
  - `{room.name}` / `{room.id}`: Name und ID des aktuellen Raums.
- **Entity Properties (`{item.<id>.<prop>}` / `{npc.<id>.<prop>}`):**
  - Read integer state properties of items or NPCs, e.g. `{item.torch.fuel}`, `{npc.guard.mood}`, `{npc.guard.hp}`.
- **Inline Conditionals (`{if <cond>|a|b}` / `{if <cond>|a}`):**
  - Branching inside text templates based on flags or variable comparisons.
  - Syntax: `{if <flag/var-cond>|then_branch|else_branch}` (if else branch is omitted, defaults to empty).
  - Flag conditions: `{if has_torch|Lit!|Darkness.}` or `{if !has_torch|Darkness.|Lit!}`.
  - Variable comparisons: `{if gold > 0|Coins: {gold}|Broke}`, `{if gold >= 100|Wealthy|Average}`, `{if weather == rain|Raining|Clear}`. Supported operators: `==`, `!=`, `/=`, `>=`, `<=`, `>`, `<`, `=`.
  - Nested expressions and placeholders in branches are recursively evaluated. Use `\|` for literal pipes.
- **Expressions (`{= <expr>}`):**
  - Dynamic mathematical calculations via the engine's `Expr` parser: `{= gold * 2}`, `{= (gold + bonus) * 2}`, `{= item.torch.fuel + 5}`.
  - Built-in functions: `min(a, b)`, `max(a, b)`, `clamp(lo, hi, val)`.
  - Format modifiers can be appended: `{= gold * 2:+}` (forces sign), `{= gold * 2:6}` (padded width). Division by zero returns `0`.
- **Deterministic Error Handling:**
  - Missing prop on item: `<error: unknown prop '<prop>' on item '<item>'>`
  - Missing item: `<error: unknown item '<item>'>`
  - Missing prop on NPC: `<error: unknown prop '<prop>' on npc '<npc>'>`
  - Missing NPC: `<error: unknown npc '<npc>'>`
  - Unknown variable in expression: `<error: unknown variable '<name>'>`
  - Expression parse error: `<error: expr: <reason>>`
  - Malformed if syntax: `<error: invalid if syntax: expected {if <cond>|a|b}>`
  - Malformed if condition: `<error: invalid if condition: <condition>>`
- **Escaping:** Geschweifte Klammern können mit `\{literal\}` oder `{{literal}}`
  maskiert werden.
- **Validierungs-Warnung (`UnknownPlaceholder`):** Enthält ein Text Platzhalter wie `{foo}` oder `{var:foo}`,
  die weder als Variable (`variables`), Quest (`quests`) noch als bekannte Systemvariable (`player.*`, `turn.*`,
  `room.*`, `cmd.*`, `item.*`, `npc.*`, `{x}`, `{y}`, `{z}`) deklariert sind und nicht via `\{foo\}` oder `{{foo}}` maskiert
  wurden, erzeugt der Worldbuilder eine nicht-fatale Warnung (`UnknownPlaceholder`).

### Praxisbeispiele aus `economy_hamurabi.yaml`

#### 1. Parametrisierter Handel mit Validierung und Formeln (`rule_kaufe`)

```yaml
  - id: "rule_kaufe"
    on: "command kaufe"
    effects:
      - if: { compare_var: { name: "cmd.count", op: ">=", value: 2 } }
        then:
          - if: { var: "cmd.arg2", is: "land" }
            then:
              - compute_var: { var: "kosten", expr: "cmd.arg1 * land_preis" }
              - if: { compare_var: { name: "gold", op: ">=", var: "kosten" } }
                then:
                  - compute_var: { var: "gold", expr: "gold - kosten" }
                  - compute_var: { var: "land", expr: "land + cmd.arg1" }
                  - text: "Erfolgreich erworben: {cmd.arg1} Hektar Land für {kosten} Gold."
                else:
                  - text: "Nicht genug Gold in der Schatzkammer! Du benötigst {kosten} Gold."
            else:
              - text: "Auf dem Markt kannst du nur 'land' kaufen (kaufe <menge> land)."
        else:
          - text: "Verwendung: kaufe <menge> land"
```

**Gerenderter Output im Spiel:**
```text
> kaufe 50 land
Erfolgreich erworben: 50 Hektar Land für 1000 Gold.

> kaufe 1 land
Nicht genug Gold in der Schatzkammer! Du benötigst 20 Gold.
```

#### 2. Status-Dashboard & Wertbegrenzung (`rule_status` & `rule_steuern`)

```yaml
  - id: "rule_status"
    on: "command status"
    effects:
      - text: "╔════════════════════════ PROVINZ KÖNIGSBERG ════════════════════════╗\n║ Jahr: {jahr:3}                 Bürger: {bevoelkerung:6} Seelen                      ║\n║────────────────────────────────────────────────────────────────────║\n║ Schatzkammer:    {gold:6} Gold                                     ║\n║ Kornkammer:      {korn:6} Scheffel                                 ║\n║ Landbesitz:      {land:6} Hektar  (Marktpreis: {land_preis} Gold/ha)         ║\n║ Steuersatz:      {steuersatz:3}%     (Kornpreis:  {korn_preis} Gold/Scheffel)    ║\n╚═════════════════════════════════════════════════════════════════════╝"

  - id: "rule_steuern"
    on: "command steuern"
    effects:
      - if: { compare_var: { name: "cmd.count", op: ">=", value: 1 } }
        then:
          - compute_var: { var: "steuersatz", expr: "clamp(0, 100, cmd.arg1)" }
          - text: "Der Steuersatz wurde auf {steuersatz}% festgesetzt."
        else:
          - text: "Aktueller Steuersatz: {steuersatz}%. Verwendung: steuern <prozent>"
```

**Gerenderter Output im Spiel:**
```text
> status
╔════════════════════════ PROVINZ KÖNIGSBERG ════════════════════════╗
║ Jahr:   1                 Bürger:    100 Seelen                      ║
║────────────────────────────────────────────────────────────────────║
║ Schatzkammer:      1000 Gold                                     ║
║ Kornkammer:        2800 Scheffel                                 ║
║ Landbesitz:        1000 Hektar  (Marktpreis: 20 Gold/ha)         ║
║ Steuersatz:          10%     (Kornpreis:  2 Gold/Scheffel)    ║
╚═════════════════════════════════════════════════════════════════════╝

> steuern 120
Der Steuersatz wurde auf 100% festgesetzt.
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

Events: `enter room`, `leave room`, `look room`, `search room`, `take item`, `drop item`, `use item`, `state entity`, `command verb`, `before verb`, `custom name`, `turn`.

### Command Veto and `before <verb>` Rules (Phase 2.2)

Rules can intercept player commands before they are executed using `on: before <verb>` (e.g. `before take`, `before go`, `before use`, or custom verbs):

```yaml
rules:
  - id: guard_stops_theft
    on: before take
    when:
      all:
        - var: cmd.target
          is: "royal_crown"
        - var: cmd.target_kind
          is: "item"
    effects:
      - block: "The guard firmly grasps your arm: 'Hands off the crown!'"
```

#### Pre-Execution Command Variables

Before a command is executed, the engine automatically extracts arguments and resolves the target, binding them to `cmd.*` variables:

- `cmd.target` — The resolved target entity ID or raw input string (e.g. `"royal_crown"` for an item/NPC, or `"throne_room"` for a room exit).
- `cmd.target_kind` — The type of the resolved target: `"item"`, `"npc"`, `"vehicle"`, `"room"`, `"choice"`, `"all"`, `"ambiguous"`, or `"none"`.
- `cmd.verb` — The canonical verb name (e.g. `"take"`, `"go"`, `"use"`).
- `cmd.raw_args` — The unparsed arguments string following the verb.
- `cmd.count` — The number of argument tokens (integer).
- `cmd.arg1..N` — Individual argument tokens (integer if numeric, otherwise text).

When a verb's target matches several entities, the engine asks `Which do you mean: [1] …, [2] …?`
and binds `cmd.target_kind = "ambiguous"` with the raw input as `cmd.target`. The player answers with
a number or a distinguishing word; the resolved command then runs with `cmd.target` set to the chosen
entity ID. The question and the answer cost no turn of their own.

#### Veto Semantics and Execution Order

- All matching `before <verb>` rules execute in definition order.
- The first `block` outcome stops command execution immediately.
- Trailing effects within the blocking rule and any remaining `before` rules do **not** run.
- By default, a blocked command consumes no turn (`turn: false`): the turn counter does not advance, condition timers do not tick, and patrol enemies do not move.
- To make a blocked action consume a turn, specify `turn: true`:
  ```yaml
  effects:
    - block:
        msg: "The jammed lever resists your pull, wasting precious time!"
        turn: true
  ```

### Veto Stufe 2: `before:` / `instead:` in `verb_map` (Phase 4.2)

Items und NPCs können Aktionen direkt am Objekt abfangen oder ersetzen — ohne lose Regel. Die
`verb_map`-Schlüssel tragen optional ein Phasen-Präfix:

| Schlüssel | Bedeutung |
|---|---|
| `take,intact:` | **Historisch (`PhaseAfter`):** auf `take` laufen Standard-Aktion (Aufheben + `take.ok`) **und** die Effekte; bei allen anderen Verben ersetzen die Effekte die Standardaktion. |
| `before:take,intact:` | Effekte laufen **vor** der Standardaktion. Ein `block:` darin vetot die Aktion (wie eine `on: before`-Regel); ohne `block:` läuft die Standardaktion danach weiter. |
| `instead:take,intact:` | Effekte **ersetzen** die Standardaktion vollständig (kein Aufheben, keine Standardmeldung — alles selbst bauen, z. B. mit `give:`). |

```yaml
items:
  - id: glued_idol
    verb_map:
      before:take:
        - {msg: "The idol is fused to the shelf."}
        - {block: "It will not come loose."}
  - id: crown
    verb_map:
      instead:take,intact:
        - {give: crown}
        - {msg: "You lift the crown. The tower shudders once, and is still."}
        - {set_flag: crown_taken, val: "true"}
```

Regeln und Feinheiten:

- **State-Suffix** wie gehabt: `verb` oder `verb,state` (Default `intact`) — das Präfix steht davor,
  also `before:use,primed:`. Der Vollständigkeit halber: `on_take:` ist der historische
  `take`-Eintrag (`PhaseAfter`) und läuft mit `instead:take` **nicht** mit.
- **Lookup-Reihenfolge** pro `(verb, state)`-Paar: `instead:` → `before:` → alter Eintrag →
  Standardaktion. Ein `(verb, state)`-Paar darf nur in **einer** Phase belegt sein
  (`VerbPhaseClash`, harter Compile-Fehler).
- **Guards zuerst:** `take.already`, `take.not_portable` und `inventory.full` laufen vor allen
  Phasen und gelten für alle — sie beschreiben den Zustand, nicht die Aktion. Ein
  `instead:take`-Eintrag kann deshalb bei einem bereits getragenen Item nicht erneut feuern
  (die Wiederholungssperre kommt von selbst, sobald die Effekte das Item mit `give:` mitführen).
- **Zugverbrauch:** Ein `verb_map`-Veto geschieht mitten im Kommando — ein turnförmiges Kommando
  (`take`, `use`, …) verbraucht den Zug wie jeder gescheiterte Versuch. Das `turn:`-Feld von
  `block:` steuert nur Regel-Vetos (`on: before`), nicht die `verb_map`-Phasen.
- **Verhaltensänderung 4.2 (Migration):** Die historischen `take`-Einträge in `fantasy`,
  `space-opera` und `cyberpunk` laufen bewusst neu als `instead:` + `give:` (echtes Ersetzen).
  Sichtbarer Unterschied: bei diesen drei Items entfällt die Standard-Zeile `You take the …`.


## Dynamische Ausgaenge: `set_exit` / `remove_exit` (Rogue Phase 3)

Effekte, mit denen Regeln und Trigger Raumausgaenge zur Laufzeit öffnen, umleiten oder schliessen — Labyrinth-Shifts, Einstuerze, verborgene Boss-Türen:

```yaml
rooms:
  - id: cellar
    on_enter:
      - msg: "Der Felsbewacher erbebt — eine Tür erscheint im Osten."
      - set_exit: { from: cellar, dir: east, to: boss, locked_by: boss_seal }
      - remove_exit: { from: hall, dir: south }   # Rückweg fällt hinter dir zu
```

- `set_exit` ersetzt die statische Verbindung für `(from, dir)`; `to` muss
  existieren (`MissingRoom`, hart), `dir` eine gültige Richtung sein
  (`UnknownDirection`, hart).
- `locked_by` (optional): die neue Tür verhält sich wie eine statisch
  verriegelte — der Entity-State wird beim Setzen automatisch als `locked`
  angelegt; ein `set_state: unlocked` (oder Interaction) öffnet sie.
- `remove_exit` entfernt den Ausgang in diese Richtung — auch einen statisch
  existierenden (Einsturz, Einbahn nach Falltür).
- Gültigkeit: Overrides leben im SaveState (`exitOverrides`), werden also
  gespeichert/geladen; die Erreichbarkeitsprüfung akzeptiert Räume, die nur
  per `set_exit` erreicht werden.
- Alle Konsumenten (Bewegung, Tür-Aliase im Parser, Tab-Completion) sehen
  dynamische Ausgänge wie statische.

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
  identische Zugfolge. **Nach einem `restart` wird der `rngState` frisch
  abgeleitet** (Systemzeit im IO-Pfad), damit ein Roguelike nicht jede Runde
  dieselbe Zufallsfolge wiederholt.
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

## Patrol: umherziehende und angreifende NPCs (Modul 7i)

```yaml
patrol:
  hostiles:
    - npc: wolf                 # muss unter `npcs:` deklariert sein
      path: [loc_1, loc_2]      # Rundkurs; `path !! start_index` ist der Startraum
      start_index: 0
      guardian: false           # true = steht still, nur warn/attack
      warn: "WARNING: A wolf is nearby!"
      attack:                    # beliebige Effekte, z. B. Schaden
        - { msg: "The wolf bites!" }
        - { damage: 3 }
```

- Der NPC zieht bei jedem **zugverbrauchenden** Befehl einen Raum weiter
  (`path !! ((i+1) mod n)`). `look`, `watch`, `help`, `inventory`, `stats`,
  `journal` kosten laut `consumesTurn` keinen Zug und bewegen ihn nicht.
- `warn` und `attack` feuern, sobald Spieler und NPC im selben Raum stehen.
  `attack` sind gewöhnliche Effekte — der Schaden kann also töten
  (`end_art.death`).
- Ein toter Feind zieht nicht und greift nicht an.
- `guardian: true` lässt den NPC stehen; `path` listet dann nur die Räume, in
  denen er warnt/zuschlägt.
- Der Rundkurs lebt in Variablen: `patrol.<npc>.index` (Position) und
  `patrol.<npc>.moved` (Zug-Gate). Beide sind **reserviert** und dürfen nicht
  unter `variables:` deklariert werden (`PatrolVariableClash`). Ein Wächter hat
  keine dieser Variablen.
- Der Index ist nicht bloß Kosmetik: Trigger-Bedingungen werden live in *einem*
  Fold ausgewertet, deshalb gibt es pro Feind eine `moved`-Gate-Variable. Ohne
  sie würden alle zutreffenden Schritte im selben Zug feuern und der NPC seinen
  ganzen Rundkurs in einem Zug abreißen.
- **Fehler:** `UnknownPatrolNPC`, `EmptyPatrolPath`, `UnknownPatrolRoom`,
  `PatrolVariableClash`.

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

### Kampfbildschirm (`combat.screen`, nur Profil `classic`)

Das Original-TheFog zeichnete vor jedem Schlag einen Schirm; `combat.screen`
gibt diesen Aufbau als Daten. Nur das Profil `classic` nutzt ihn — dort ist ein
`attack` genau eine Runde. **Ohne `screen`-Block bleibt die Ausgabe
unverändert**: das Profil ist dann `CombatClassic Nothing`, und der Schlüssel
wird gar nicht erst serialisiert (Checksummen bestehender Saves bleiben gleich).

```yaml
combat:
  profile: classic
  screen:
    art: |                       # optional — Kunst über dem Block (String/CondText)
      /\
      ||
    bar_width: 8                 # optional, Default 10, mindestens 1
    scene: "== Eine Runde =="    # optional; ohne Angabe ">>You are in a Fight!<<"
    footer: "attack or flee."    # optional; ohne Angabe der Original-Flucht-Hinweis
```

Gerendert wird **vor** der Rundenauflösung, also aus dem Zustand *vor* dem
Schlag — genau wie im Original. Reihenfolge:

```
________________________________________________________________________________   (2×)
<art>
                    You are fighting a <npcName>
                    <scene>
(leer)
Your Atk: <attack>
Your Def: <defense>
(leer)
Your HP:  <kp> [<balken>]
Your Steps:   <turnCount>
(leer)
HP of the <npcName>: <kp> [<balken>]
(leer)
<footer>
```

- **Balken:** `filled = kp * bar_width / max`, geklemmt auf `0 … bar_width`,
  Zeichen `█`. Zwei Original-Artefakte sind bewusst behoben: eine negative
  Füllung überläuft den Balken nicht mehr, und ein Maximum ≤ 0 zeichnet einen
  leeren Balken statt eines verwaisten `]`.
- **Gegner-KP** kommen aus dem NPC-State (`npcHealth`), das **Maximum** aus
  `npcMaxHealth`. Fehlt das Maximum (oder gibt es keinen State), gilt der
  aktuelle Wert als Maximum → voller Balken, bei 0 KP ein leerer.
- Die Zeilen zum Gegner nutzen den **NPC-Namen** (`name:`), während die
  Rundenmeldungen weiterhin das vom Spieler getippte Wort zitieren.
- **Der Schirm erscheint nur, wenn der Gegner angreifbar ist** (KP vorhanden):
  ein toter NPC bleibt bei „… is already dead." — ohne Schirm.
- **„Your Steps"** ist der Zugzähler des Spielstands (`turnCount`); die Engine
  führt kein eigenes Schrittzähler-Feld. `scene: ""` bzw. `footer: ""`
  unterdrückt die jeweilige Zeile.
- **Fehler:** `BadScreenBarWidth` (`bar_width` kleiner 1).
- Die Gegner-Kunst pro Runde (`npc.ascii`) bleibt unberührt: wer beides setzt,
  sieht die Kampfkunst oben am Schirm **und** die Zustandskunst nach der Runde.
- Beispiel: `examples/fixtures/combat-screen.yaml`, gepinnt von
  `ci/e2e/combat-screen.expect` (Sieg) und `ci/e2e/combat-screen-round.expect`
  (schrumpfender Balken im Statusblock).

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

Der Cooldown wird als **Condition** `cooldown_<abilityId>` geführt (also
`cooldown_power_strike`, mit `cooldown:` als Turn-Zahl). Das Präfix `cooldown_`
ist damit reserviert: eine eigene `apply_condition`/`clear_condition` mit diesem
Namen wird beim Kompilieren als `CooldownConditionClash` abgelehnt, sonst würden
Engine-Cooldown und Autoren-Condition stillschweigend dieselbe Markierung teilen.

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
---

## Game: Roguelike-Policy (Rogue Phase 1)

Optionaler `game:`-Block für Roguelike-/Roguelite-Adventures. Ohne Block gilt
`defaultGamePolicy` — das Verhalten aller bestehenden Adventures bleibt
bit-identisch.

```yaml
game:
  permadeath: true        # Tod bietet kein Undo/Load, nur [R]estart | [Q]uit
  allow_undo: false       # 'undo' generell abweisen
  ironman: true           # Speichern nur in Savezones; Checkpoint wird bei Tod gelöscht
  save_zones: [camp, tavern]  # Räume, in denen im Ironman-Modus gespeichert werden darf
  meta_slug: my_dungeon   # optional: Slug für die Meta-Datei (Rogue Phase 2, M8)
```

- `permadeath`: beim Tod gibt es kein `[U]ndo`/`[L]oad` mehr — nur
  `[R]estart` und `[Q]uit`. Eingaben `u`/`l` werden abgewiesen.
- `allow_undo: false`: der `undo`-Befehl wird generiert abgewiesen
  ("Undo is disabled in this adventure."); die Undo-Historie wird dann gar
  nicht erst aufgebaut.
- `ironman` (Checkpoint-Modell):
  - Speichern ist nur in `save_zones`-Räumen erlaubt ("You can only rest at
    a savezone."); es gibt genau **einen** Checkpoint-Slot (`checkpoint`),
    der bei jedem In-Zone-Save überschrieben wird.
  - Beim Tod wird der Checkpoint-Slot automatisch von der Festplatte
    gelöscht; `load` ist im Ironman-Modus gesperrt. Die per `--save`
    geladene Startdatei bleibt als neutraler Wiedereinstieg erhalten.
- Validierung: `save_zones` müssen existierende Raum-IDs sein
  (`MissingRoom`, hart); `ironman` ohne `save_zones` ist legal (Hardcore:
  nie speichern) warnt aber (`IronmanWithoutSavezones`).

## Game: Roguelike-Policy (Rogue Phase 1)

Optionaler `game:`-Block für Roguelike-/Roguelite-Adventures. Ohne Block gilt
`defaultGamePolicy` — das Verhalten aller bestehenden Adventures bleibt
bit-identisch.

```yaml
game:
  permadeath: true        # Tod bietet kein Undo/Load, nur [R]estart | [Q]uit
  allow_undo: false       # 'undo' generell abweisen
  ironman: true           # Speichern nur in Savezones; Checkpoint wird bei Tod gelöscht
  save_zones: [camp, tavern]  # Räume, in denen im Ironman-Modus gespeichert werden darf
  meta_slug: my_dungeon   # optional: Slug für die Meta-Datei (Rogue Phase 2, M8)
```

- `permadeath`: beim Tod gibt es kein `[U]ndo`/`[L]oad` mehr — nur
  `[R]estart` und `[Q]uit`. Eingaben `u`/`l` werden abgewiesen.
- `allow_undo: false`: der `undo`-Befehl wird generiert abgewiesen
  ("Undo is disabled in this adventure."); die Undo-Historie wird dann gar
  nicht erst aufgebaut.
- `ironman` (Checkpoint-Modell):
  - Speichern ist nur in `save_zones`-Räumen erlaubt ("You can only rest at
    a savezone."); es gibt genau **einen** Checkpoint-Slot (`checkpoint`),
    der bei jedem In-Zone-Save überschrieben wird.
  - Beim Tod wird der Checkpoint-Slot automatisch von der Festplatte
    gelöscht; `load` ist im Ironman-Modus gesperrt. Die per `--save`
    geladene Startdatei bleibt als neutraler Wiedereinstieg erhalten.
- Validierung: `save_zones` müssen existierende Raum-IDs sein
  (`MissingRoom`, hart); `ironman` ohne `save_zones` ist legal (Hardcore:
  nie speichern) warnt aber (`IronmanWithoutSavezones`).

## Meta-Progression: `meta.*`-Variablen (Rogue Phase 2)

Variablen mit dem Präfix `meta.` (z. B. `meta.souls`, `meta.unlocked_class`)
sind das Roguelite-Fortschrittskonto: Sie überleben Tod, Restart und Neuladen
in `saves/<slug>_meta.json` — alles andere resettet pro Run.

```yaml
variables:
  - var: meta.souls        # dauerhafte Währung (z. B. beim Boss gesammelt)
    type: int
    initial: 0
  - var: meta.unlocked_class
    type: text
    initial: ""
```

- **Persistenz:** beim Spielende (Sieg, Tod, Custom, Quit) schreibt die
  Engine alle `meta.*`-Variablen automatisch in die Meta-Datei.
- **`meta.runs` (Engine-eigen):** Jeder frische Run zählt — beim Spielstart und
  bei jedem Restart erhöht die Engine `meta.runs` automatisch (auch ein
  verlassener Run zählt) und persistiert sofort. Autoren können Unlocks an den
  Zähler koppeln (z. B. `meta.runs >= 3` als Freischalt-Bedingung). Der
  Zähler entsteht nur für Adventures, die tatsächlich Meta-Progression nutzen
  (deklarierte `meta.*`-Variable oder vorhandene Meta-Datei) — für alle
  anderen bleibt das Verhalten unverändert.
- **Ablageort:** `saves/<slug>_meta.json` pro Adventure. Der Slug wird aus
  dem Adventure-Titel abgeleitet (lowercase, `[a-z0-9_-]`); ein explizites
  `game.meta_slug:` gewinnt — dann überlebt ein Titel-Umbenennen den
  Fortschritt. Der Pfad folgt `TA_SAVES_DIR`/`--saves-dir`.
- **Restart (R, Rogue-Style):** der neue Run startet aus dem initialen
  Zustand, aber die `meta.*`-Werte werden in den frischen Run hinübergetragen.
- **Vorrangregel (M5):** die Meta-Datei ist die fortschrittsautoritative
  Quelle — auch ein `load <slot>` überschreibt sie nie rückwärts.
- Tests/E2E: `meta.souls` nach Restart prüfen; die Datei erscheint erst mit
  der ersten Meta-Variablen.

## Banner: `title_art` und `end_art` (Phase G)

Zwei optionale Top-Level-Felder ersetzen die fest eingebauten Textrahmen der
Engine. Beide nehmen dieselbe Kunstform wie `ascii` (String, CondText oder
animiert) und werden mit `text2ascii` erzeugt, dem Werkzeug für
Text→Banner-Kunst.

```yaml
name: Mein Abenteuer

# Titelbildschirm. Fehlt das Feld, erscheint wie bisher "=== <name> ===".
title_art: |
  ####   ###  #   # #   # ##### ####
  #   # #   # ##  # ##  # #     #   #
  ####  ##### # # # # # # ####  ####
  #   # #   # #  ## #  ## #     #  #
  ####  #   # #   # #   # ##### #   #

# Endbildschirme je Grund: "death", "victory" oder ein eigener
# `game_end:`-Text. Fehlt der Eintrag, erscheint der bisherige Rahmen.
end_art:
  victory: |
    *** YOU WIN ***
  death: |
    *** YOU DIED ***
```

Erzeugen lässt sich die Kunst mit dem eigenen Paket `text2ascii`:

```
cabal run text2ascii -- "MEIN ABENTEUER"            # Block-Font (Standard)
cabal run text2ascii -- -f slant "SIEG"             # Schräge
cabal run text2ascii -- -f outline "GAME OVER"      # Outline
cabal run text2ascii -- --color "Titel"             # 24-Bit-Verlauf
```

Die eingebauten Fonts (Block/Slant/Outline) brauchen keine Datenfiles und
keine Lizenzklärung. `--color` färbt mit einem vertikalen 24-Bit-Verlauf und
setzt am Zeilenende zurück; die Engine entfernt die Sequenzen automatisch,
wenn stdout kein Terminal ist oder `--no-color` gesetzt wurde (siehe
`ascii`-Abschnitt).

Ohne `end_art` bleiben `YOU HAVE DIED` / `VICTORY!` / `Game Over: <msg>` und
die Steuerhinweise unverändert — reine Rückwärtskompatibilität.

---

## Deckbuilder & Kartensystem (Phase 2)

Adventures können rundenbasierte Deckbuilding-Mechaniken definieren (inspiriert von *Slay the Spire*). Karten besitzen Ressourcenkosten (z. B. Energie oder Mana), Zielvorgaben, optionale Erschöpfung (`exhaust`) und Effektketten.

### Kartendefinition (`cards:`)

Karten werden unter dem Top-Level-Schlüssel `cards:` deklariert — entweder als Liste von Objekten oder als Mapping von Card-ID auf Kartendefinition:

```yaml
cards:
  - id: strike
    name: "Schlag"
    type: attack            # attack | skill | power | curse
    cost: { energy: 1 }     # Ressourcenkosten (Variable muss existieren oder wird initialisiert)
    target: single_enemy    # single_enemy | all_enemies | self | none
    exhaust: false          # Wandert bei true nach dem Ausspielen auf den Exhaust-Stapel
    description: "Fügt einem Ziel 6 Schaden zu."
    outcomes:
      - damage: 6

  - id: bash
    name: "Schmetterschlag"
    type: attack
    cost: { energy: 2 }
    target: single_enemy
    description: "Fügt 8 Schaden zu. +4 Bonusschaden, wenn 'defend' auf der Hand liegt."
    outcomes:
      - damage: 8
      - if: { card_in_hand: defend }
        then:
          - damage: 4
          - text: "Synergie! Das Zusammenspiel mit 'defend' zertrümmert die Abwehr (+4 Schaden)!"
```

| Feld | Typ | Default | Beschreibung |
|---|---|---|---|
| `id` | String | **required** | Eindeutige Kennung der Karte (z. B. `strike`, `defend`) |
| `name` | String | = `id` | Angezeigter Kartenname im Hand-Display und HUD |
| `type` | String | `skill` | Kartentyp: `attack`, `skill`, `power`, `curse` |
| `cost` | Map (String -> Int) | `{}` | Benötigte Ressourcen (z. B. `energy: 1`, `mana: 2`) |
| `target` | String | `single_enemy` | Zielmodus: `single_enemy`, `all_enemies`, `self`, `none` |
| `exhaust` | Bool | `false` | Falls `true`, landet die Karte nach dem Ausspielen im `exhaust`-Stapel statt im `discard` |
| `description` | String | `""` | Kurzbeschreibung für den ASCII-Kartenrahmen |
| `outcomes` / `effects` | [Effekt] | `[]` | Liste auszuführender Effekte beim erfolgreichen Ausspielen |

### Startdeck (`deck:`) & Hand-Limit (`handLimit:`)

Das Startdeck kann top-level unter `deck:` oder unter `player.deck:` definiert werden:

```yaml
# Variante A: Liste von Karten-IDs
deck: [strike, strike, strike, defend, defend, bash]

# Variante B: Häufigkeits-Mapping mit Handkarten-Limit
deck:
  strike: 4
  defend: 4
  bash: 1
handLimit: 5

# Variante C: Geschachteltes Deck-Objekt
deck:
  handLimit: 5
  cards:
    strike: 4
    defend: 4
    bash: 1
```

- **`handLimit:` / `hand_limit:` (Option des Autors):**
  - Ohne Angabe (oder `<= 0`) ist die Handgröße **unbegrenzt** (volle Rückwärtskompatibilität).
  - Ist ein Limit gesetzt (z. B. `handLimit: 5`), zieht der Spieler nur so viele Karten, bis das Limit erreicht ist.
  - Das Nachmischen des Ablagestapels (`discard`) in den Nachziehstapel (`draw`) erfolgt wie gewohnt, wenn der Nachziehstapel leer wird; das Ziehen wird jedoch gestoppt, sobald die Hand voll ist.

### Kartensynergien & Combos (`card_in_hand` / `combo`)

Bedingte Karteneffekte und Synergien können prüfen, ob sich bestimmte Karten aktuell auf der Spielerhand befinden:

```yaml
# Shorthand 1: Einzelne Karte auf der Hand
if: { card_in_hand: defend }
then:
  - damage: 4

# Shorthand 2: Combo mit mehreren Karten
if: { combo: [strike, defend] }
then:
  - msg: "Perfekte Klingenkombination!"

# Standard-Prädikat über Entity-State:
if: { state: defend, is: in_hand }
then:
  - modify_value: { actor: player, prop: health, delta: 3 }
```

Folgende Kartenzustände können mit `state: <cardId>, is: <zustand>` geprüft werden:
- `in_hand` / `hand`: Karte befindet sich auf der aktiven Hand
- `draw`: Karte befindet sich im Nachziehstapel
- `discard`: Karte befindet sich im Ablagestapel
- `exhaust`: Karte befindet sich im Erschöpft-Stapel

### Deck-Variablen & Text-Interpolation

Zur Anzeige in Beschreibungen, Dashboards und HUDs stellt die Engine dynamische Zähler zur Verfügung:
- `{hand.count}`: Anzahl Karten auf der aktuellen Hand
- `{deck.count}`: Anzahl verbleibender Karten im Nachziehstapel
- `{discard.count}`: Anzahl Karten auf dem Ablagestapel
- `{exhaust.count}`: Anzahl Karten im Erschöpft-Stapel

### Befehle im Spiel

- `hand` / `karten`: Zeigt die aktuelle Hand in hübschen ASCII-Rahmen mit Typ, Kosten, Beschreibung und Synergien an.
- `play <card>` / `spiele <card>`: Spielt eine Karte aus der Hand aus (prüft Ressourcen, zieht Kosten ab, führt Effekte aus).
- `endturn` / `zugende`: Beendet den Spielzug, legt verbliebene Handkarten auf den Ablagestapel, stellt Energie wieder her und zieht eine frische Hand.

---

## Endlos-Sandbox & Prozedurale Zonen (Phase 3)

Adventures können offene, unendliche Sandbox-Welten oder prozedurale Minen/Wildnisse deklarieren (`sandbox_zones:`). Betritt der Spieler eine noch ungenerierte Koordinate $(x, y, z)$, erzeugt die Engine die Zelle deterministisch (via SplitMix64-Seed), verdrahtet automatisch den Gegen-Exit und speichert den Raum im `dynamicRooms`-Overlay des Spielstands.

### Sandbox-Zonendefinition (`sandbox_zones:`)

```yaml
sandbox_zones:
  wildnis:
    origin: [0, 0, 0]      # Einstiegskoordinate bei Betreten von 'sandbox_wildnis'
    floor: 1               # Ebene für TUI-Minimap
    biomes:
      - id: "forest"
        weight: 60
        name_pattern: "Dichter Wald [{x}, {y}]"
        tags: ["outdoor", "forest"]
        passable_dirs: ["north", "south", "east", "west"]
        description:
          default: "Uralte Kiefern und Fichten ragen empor bei Koordinate [{x}, {y}]."
          variants:
            - when: { var: "weather", is: "regen" }
              text: "Schwere Regentropfen prasseln durch das Nadelholzdach bei [{x}, {y}]."
            - when: { var: "time_of_day", is: "nacht" }
              text: "Die finstere Nacht hüllt den Wald in tiefe Schatten bei [{x}, {y}]."
        ascii_art:
          default: |
            / \ / \ / \  [Tag: {x}, {y}]
            | | | | | |
          variants:
            - when: { var: "weather", is: "regen" }
              text: |
                / / / \ / /  [Regen: {x}, {y}]
                / / | | / /
            - when: { var: "time_of_day", is: "nacht" }
              text: |
                * . / \ . *  [Nacht: {x}, {y}]
                . * | | * .

      - id: "clearing"
        weight: 30
        name_pattern: "Sonnige Waldlichtung [{x}, {y}]"
        tags: ["outdoor", "clearing"]
        passable_dirs: ["north", "south", "east", "west"]
        ascii_art:
          default: |
            .  *  .  [Tag: {x}, {y}]
            \|/|/|/
            ---+---
          variants:
            - when: { var: "weather", is: "regen" }
              text: |
                '  '  '  [Regen: {x}, {y}]
                \|/|/|/
                ---+---

      - id: "cliff"
        weight: 10
        name_pattern: "Schroffe Klippe [{x}, {y}]"
        tags: ["outdoor", "mountain"]
        passable_dirs: ["south", "east", "west"]   # Blockiert nach Norden
        ascii_art: |
          /\/\/\/\
          | WAND |
```

| Feld | Typ | Default | Beschreibung |
|---|---|---|---|
| `origin` | [Int, Int, Int] | `[0, 0, 0]` | Startkoordinate $(x, y, z)$ der Zone beim ersten Eintritt |
| `floor` | Int | `1` | Kartenebene für die Minimap |
| `biomes[].id` | String | **required** | Eindeutige ID des Bioms |
| `biomes[].weight` | Int | `1` | Relative Zufallshäufigkeit (SplitMix64 gewichteter Wurf) |
| `biomes[].name_pattern` | String | `"Wildnis ({x}, {y})"` | Raumnamens-Muster mit `{x}`, `{y}`, `{z}` Platzhaltern |
| `biomes[].description` | String / CondText | `"Unberührte Wildnis."` | Statischer Text oder `{default, variants}` mit Tag/Wetter-Bedingungen |
| `biomes[].tags` | [String] | `[]` | Raum-Tags für Rules (z. B. `forest`, `clearing`, `cave`) |
| `biomes[].passable_dirs` | [String] | alle 4 | Richtungen, die passierbar sind (ermöglicht Sackgassen und Klippen) |
| `biomes[].ascii_art` | String / AsciiArt | `""` | Kunstwerk mit `{x}`, `{y}`-Interpolation und Tag/Wetter-Varianten |

### Prozedurale Landschaftskunst & Zustandsvarianten (`btAsciiArt`)

Die ASCII-Landschaftskunst (`ascii_art`) von Biom-Templates nutzt das bewährte Varianten-System der Engine:
- **Koordinaten-Interpolation:** Platzhalter `{x}`, `{y}` und `{z}` werden sowohl im Standardtext als auch in allen Varianten automatisch durch die Koordinaten der generierten Zelle ersetzt.
- **Wetter- & Tageszeitvarianten:** Über `variants:` mit Bedingungen (`when: { var: weather, is: regen }`, `when: { var: time_of_day, is: nacht }` oder `{ state: ..., is: ... }`) schaltet das Bild beim Rasten oder Wetterwechsel deterministisch um.

### Ressourcen-Knoten, Harvesting & Respawn-Zähler

In Sandbox- und Survival-Welten können Ressourcen abgebaut oder gesammelt werden. Zur Steuerung von Abbau und Regeneration bieten sich zwei erprobte Autoren-Muster an:

#### 1. Respawn über Variable und Zähler-Trigger (aus `sandbox_wilderness.yaml`)

```yaml
variables:
  - name: berries
    type: int
    initial: 0
  - name: beeren_timer
    type: int
    initial: 0

verbs:
  - name: sammeln
    aliases: [harvest, pfluecken]

rules:
  # Ernten: nur möglich, wenn der Respawn-Timer abgelaufen ist (<= 0)
  - id: rule_foraging
    on: command sammeln
    when:
      all:
        - room: current_room
          has_tag: clearing
        - compare_var: { name: beeren_timer, op: "<=", value: 0 }
    effects:
      - add_var: { variable: berries, delta: 5 }
      - set_var: { var: beeren_timer, value: 3 }    # 3 Runden Cooldown
      - msg: "Du pflückst frische Beeren vom Strauch. (+5 Beeren, Strauch abgeerntet)"

  # Rückmeldung bei noch nicht regeneriertem Vorkommen
  - id: rule_foraging_cooldown
    on: command sammeln
    when:
      all:
        - room: current_room
          has_tag: clearing
        - compare_var: { name: beeren_timer, op: ">", value: 0 }
    effects:
      - msg: "Die Beerensträucher wurden vor kurzem abgeerntet ({beeren_timer} Runden verbleibend)."

  # Respawn-Zähler: verringert den Timer jede Spielrunde um 1
  - id: rule_beeren_respawn
    on: turn
    when: { compare_var: { name: beeren_timer, op: ">", value: 0 } }
    effects:
      - add_var: { variable: beeren_timer, delta: -1 }
```

#### 2. Physischer Ressourcen-Knoten als Item mit Zuständen (`state:`)

Für gezielte Abbauknoten (z. B. eine spezifische Erzader oder einen Obstbaum in einem Raum) kann das Vorkommen als unbewegliches Item (`portable: false`) mit eigener `verb_map:` und Zuständen deklariert werden:

```yaml
items:
  - id: erzader
    name: "Glitzernde Erzader"
    location: "stollen_1"
    portable: false
    state: "rich"
    description:
      default: "Eine vielversprechende Quarzader mit dicken Silberadern."
      variants:
        - when: { state: erzader, is: depleted }
          text: "Die Ader ist bis auf den tauben Fels ausgebeutet."
    verb_map:
      schuerfen:
        depleted:
          msg: "Hier ist kein verwertbares Erz mehr zu finden."
        rich:
          effects:
            - set_state: { entity: erzader, state: depleted }
            - add_var: { variable: ore, delta: 3 }
            - set_var: { var: erz_respawn, value: 10 }
            - msg: "Du brichst glänzende Erzbrocken aus dem Stein! (+3 Erz, Ader erschöpft)"

rules:
  # Respawn-Trigger regeneriert die Ader, wenn der Zähler 0 erreicht
  - id: rule_erz_regeneration
    on: turn
    when:
      all:
        - state: erzader
          is: depleted
        - compare_var: { name: erz_respawn, op: "<=", value: 0 }
    effects:
      - set_state: { entity: erzader, state: rich }
      - msg: "Durch Gebirgsdruck sind neue Erzkristalle an der Ader zutage getreten!"
```

---

## Content-Tests: `tests:` (B1)

Autoren schreiben eigene Regressionstests als Daten — `worldbuilder test <adventure.yaml>`
kompiliert das Adventure, spielt jeden Test in einem **frischen Zustand** durch und prüft die
**geordneten Marker**.

```yaml
tests:
  - name: belohnung
    input: [dank, doppel]          # Eingabefolge (Zeilen wie am Prompt)
    expect:
      - "REWARD: 5 gold for the rescue (total 5)."
      - "DOPPEL: total 10."
```

**Regeln:**

- **Ein Zustand pro Test:** Tests laufen unabhängig voneinander; der Start-Look gehört
  zur Ausgabe, Befehle werden mit den Adventure-Verben geparst (Custom-Verben
  funktionieren).
- **Geordnete Marker:** `expect` ist eine Teilfolge der gerenderten Ausgabe — die
  Reihenfolge ist Vertrag. Ein nicht erreichter Marker wird benannt.
- **Reiner Lauf:** `save`/`load` erzeugen ihre Meldungen, aber keine Dateien; nach Game
  Over werden keine Befehle mehr gefüttert.
- **Gate:** `worldbuilder test` bricht mit Exit 1 ab; die CI (Stufe 4b) führt es für
  Abenteuer mit `tests:`-Sektion aus. Locals läuft dasselbe Kommando.

## Procedures: `procedures:` und `call:` (Phase 2.5)

Benannte, parametrisierte Effekt-Bündel gegen Copy-Paste: der Autor deklariert eine
Prozedur einmal und ruft sie von überall mit `call:` auf.

```yaml
procedures:
  - id: belohnen
    params: [betrag, grund]
    effects:
      - compute_var: { var: gold, expr: "gold + betrag" }
      - msg: "REWARD: {betrag} gold for {grund} (total {gold})."

rules:
  - id: rule_dank
    on: "command dank"
    effects:
      - call: { proc: belohnen, args: [5, "the rescue"] }   # oder ohne args: call: belohnen
```

**Regeln:**

- **Parameter sind literale Argumente** (int/bool/text) — keine berechneten Aufrufe:
  Aufrufstellen bleiben statisch prüfbar (wie `raise: name`).
- **Scope:** Parameter und Locals liegen in einem Prozedur-Scope, der die
  Adventure-Variablen **schattet** (lesend wie schreibend). Ein Schreiben auf einen
  Parameternamen ändert nur die lokale Kopie und wird beim Verlassen des Aufrufs
  verworfen; Schreiben auf andere Namen geht in die `variables:`-Map wie gewohnt.
  `msg`-Templates lesen Parameter direkt (`{betrag}`), Ausdrücke ebenso (`gold + betrag`).
- **Keine Rekursion:** Zyklen im Call-Graph — auch indirekte über mehrere Prozeduren —
  sind **Compile-Fehler** (`ProcRecursion`). Verschachtelte, nicht-zyklische Aufrufe sind
  erlaubt; pro Call entsteht ein neuer Scope.
- **Weitere Checks:** `UnknownProc` (unbekannter Name), `ProcArity` (falsche
  Argumentanzahl), `DuplicateProc` (doppelte `id`), `ProcParamReserved` (Parameter in
  engine-eigenen Namensräumen: `cmd.`, `combat.`, `env.`, `faction.`, `party.`,
  `patrol.`, `ship.`, `stealth.`).
- **Veto:** `block` im Prozedurkörper stoppt die restlichen Körpereffekte **und** die
  restlichen Effekte des Aufrufers — dieselbe Semantik wie in einer `effects:`-Liste.

## Wissensmodell: `facts:`, `combine:`, `learn`/`forget`/`knows` (W1)

Fakten als Daten statt Flags: der Autor deklariert Wissen einmal, die Engine verwaltet
Lernen, Ableitung und Nachschlagen.

```yaml
facts:
  - id: brief_gelesen
    keys: [brief, notiz]                  # Wörter, mit denen der Spieler es anspricht
    text: "Der Brief erwähnt eine Verabredung am Hafen."
    source: "gefunden in der Bibliothek"   # optional, Notizbuch-Zusatz
    tag: beweise                           # optional, Notizbuch-Gruppierung
    learn_msg: "Du liest den Brief aufmerksam."  # optional, Meldung beim Lernen
    silent: false                          # optional, überschreibt alles

combine:
  - facts: [brief_gelesen, tagebuch_gelesen]
    yields: verabredung
    msg: "Brief und Tagebuch zusammen: die Verabredung am Hafen."

journal: notes          # optional: aktiviert den `notizen`-Befehl (Default: messages)
combine_verb: kombiniere # optional: das Wort fürs Kombinieren (Default: kombiniere)
```

**Regeln:**

- **Wissen ist akteursbezogen:** `learn: <fact>` (Spieler) bzw.
  `learn: {fact: <f>, actor: <npc>}` — Prädikat `knows: <fact>`/
  `{knows: <actor>, fact: <f>}`. Jeder Akteur hat seinen eigenen Wissensstand; das
  Notizbuch zeigt nur Spieler-Wissen (NPC-Wissen ist Detektivarbeit — Verhör).
- **Lernen ist idempotent** (Set-Semantik); pro neu gelerntem Fakt feuert `OnLearn`
  (Trigger `on: learn <fact>`) einmal, in Lernreihenfolge.
- **`combine:`-Kaskade:** alle Prämissen müssen dem **denselben Akteur** bekannt sein;
  die Ableitung läuft als geschlossene Operation in der `Learn`-Anwendung (terminierend,
  kein Kontrollfluss). `yields`-Fakten müssen deklariert sein.
- **Kein automatisches Vergessen:** `forget: <fact>` entfernt nur den genannten Fakt;
  abgeleitete Fakten bleiben (keine Rückwärts-Kaskade).
- **Meldungen:** nur Spieler-Lernen zeigt Meldungen (Katalog-Default `learn.default`);
  `learn_msg`/`combine`-`msg` schlagen sie; `silent: true` unterdrückt.
- **`kombiniere`-Befehl** (generiert, sobald `combine:`-Einträge existieren):
  `kombiniere <X> <Y>` oder `kombiniere <X> mit <Y>` — beide Reihenfolgen; Matching über
  die `keys` der Fakten (`cmd.arg*`). Voraussetzung: beide Prämissen sind **bekannt**.
  Ohne Match: „You cannot combine these like that.“
- **Notizbuch** (nur bei `journal: notes`): `notizen`/`notes` zeigt gelernte, nicht-silente
  Fakten in **Deklarationsreihenfolge** — die ist Gameplay-Vertrag (nicht umsortieren).
- **Checks:** `UnknownFact`, `DuplicateFact`, `YieldsWithoutPremises`,
  `KnownVariableClash` (`known.` gehört der Engine), Kollisionen der generierten Verben.

## Kapitel: `chapters:` (W3)

Narrative Struktur als Daten: benannte Kapitel mit optionaler Auto-Gate-Bedingung und
Intro-Text. Der Wechsel ist ein Zustandsübergang; der Szenenwechsel bleibt Autorenregel
am `OnChapter`-Event.

```yaml
chapters:
  - id: ankunft
    intro: "Kapitel eins: Die Ankunft."
  - id: sanatorium
    when: { has_flag: habe_ticket }     # Auto-Gate (optional)
    intro: "Kapitel zwei: Das Sanatorium."

rules:
  - id: szenenwechsel
    on: "chapter sanatorium"            # OnChapter - Ortswechsel, Zustände, Meldungen
    effects:
      - move: bahnhof_vorhalle
```

**Regeln:**

- **Deklarationsreihenfolge = Vertrag** (nicht umsortieren): sie ist die `next_chapter`-Richtung
  und der Auto-Gate-Tie-Break.
- **Auto-Gate:** höchstens **ein** Wechsel pro Zug, geprüft nach dem Trigger-Fold; Kandidat =
  das erste Kapitel mit erfülltem `when:`, das weder besucht noch aktuell ist. Besuchte
  Kapitel werden nie automatisch wieder betreten.
- **Effekte:** `next_chapter` (das folgende Kapitel) und `goto_chapter: <id>` — **Rücksprünge
  werden verweigert** („This chapter is behind you."): ein besuchtes Kapitel wird nie wieder
  betreten (keine Rückblenden, W3-Vertrag). Unbekannte Ziele sind Compile-Fehler
  (`UnknownChapter`), statisch erkennbare Rückwärts-Sprünge ebenfalls
  (`ChapterBackwardsJump`), unerreichbare Kapitel eine Warnung (`UnreachableChapter`).
- **Event `OnChapter <id>`** (Trigger `on: chapter <id>`): hier hängt der Szenenwechsel.
- Kapitelabhängige Raumtexte laufen über die Inline-Bedingungen der Phase 2.4
  (`{if <var> == chapter...}`-artig mit `chapter.current` als Text-Variable — das
  `chapter.current`-Format ist eine Text-Variable, Werte = Kapitel-IDs).

## Fortschritt & Stufen: `progression:` (W2)

Genre-neutrale Fortschritts-Tabelle für Erfahrungspunkte (XP), Stufenaufstiege und Charakterentwicklung.
Das System ist rein datengetrieben: Autoren definieren Stufenschwellen (`xp:`), Rangnamen (`name:`) und beim Stufenaufstieg feuernde Effekte (`effects:`).

```yaml
progression:
  levels:
    - level: 1
      name: Novize
      xp: 0
    - level: 2
      name: Abenteurer
      xp: 100
      msg: "Aufstieg zu Level 2! Deine Kampfkraft steigt."
      effects:
        - set_var:
            var: bonus.attack
            val: 3
    - level: 3
      name: Held
      xp: 250
      msg: "Aufstieg zu Level 3! Du bist ein wahrer Held."
      effects:
        - set_var:
            var: bonus.defense
            val: 2
```

### Felder unter `progression.levels`

| Feld | Typ | Default | Beschreibung |
|---|---|---|---|
| `level` | Int | Index (1, 2, ...) | 1-basierter Level-Index |
| `name` | String | **required** | Titel / Rangname (z. B. `Novize`, `Abenteurer`, `Held`) |
| `xp` | Int | **required** | Schwellenwert für dieses Level. Level 1 muss `xp: 0` haben; Werte müssen streng monoton steigen |
| `msg` / `level_msg` | String | Default-Meldung | Optionale Belohnungs-Meldung beim Erreichen der Stufe |
| `effects` | [AActionOutcome] | `[]` | Effekte, die beim Aufstieg auf diese Stufe ausgeführt werden |

### Effekte & Events

- **Effekt `gain_xp: <int>`:**
  - Erhöht oder verringert `xp.current`. Negative Werte (`gain_xp: -50`) sind erlaubt, werden jedoch am Boden nach unten auf `0` geclamped (`xp.clamped`).
  - **Stufen-Garantie (Anti-De-Level):** Ein Verlust von Erfahrungspunkten führt **niemals** zu einem Absinken des erreichten Levels (`level.current` ist strikt monoton wachsend).
  - Überschreitet der neue XP-Wert Schwellen zukünftiger Stufen, werden alle übersprungenen Stufen in streng aufsteigender Reihenfolge abgearbeitet (Level 2, dann Level 3).
- **Event `OnLevelUp <n>` (Trigger `on: levelup <n>` / `on: level_up <n>`):**
  - Feuert beim Erreichen der jeweiligen Stufe `n`. Erlaubt Autoren, Quests, NPCs, Dialogbäume oder Weltzustände an Level-Aufstiege zu koppeln.

### State & Kampf-Boni (Zero New `SaveState` Fields)

- Das Fortschrittsmodell benötigt keine neuen `SaveState`-Felder. Alle Zustände werden in der regulären `variables`-VarMap verwaltet:
  - `xp.current`: Aktuelle Erfahrungspunkte (Default: 0).
  - `level.current`: Aktuelle Stufe (Default: 1).
  - `bonus.attack`: Additiver Bonus auf Spieler-Angriff (`effectiveAttack`).
  - `bonus.defense`: Additiver Bonus auf Spieler-Verteidigung (`effectiveDefense`).
  - `bonus.hp`: Additiver Bonus auf maximale Spieler-Lebenspunkte (`effectiveMaxHealth`).
- Die Präfixe `xp.`, `level.` und `bonus.` sind für die Engine reserviert; das manuelle Deklarieren unter `variables:` wird vom Compiler abgewiesen (`ProgressionVariableClash`).

### Anzeige im `stats`-Befehl

Wenn `progression:` im Abenteuer definiert ist, blendet `stats` die Fortschrittszeile ein:
- Vor Maximalstufe: `Level 1 — Novize (42/100 XP)`
- Auf Maximalstufe: `Level 3 — Held (250 XP)`
Ohne `progression:`-Sektion bleibt die `stats`-Ausgabe unverändert.

## Vorrichtungen & Halterungen: `devices:` (W4)

Explizite Zustands- und Einbaumechanik für Hebel und Halterungen als genre-neutrale Primitive (Schlösser, Fackelhalterungen, Sicherungskästen, Konsolen, Altäre). Hebel und Halterung sind zwei Ausprägungen desselben Konzepts: Eine Vorrichtung ist eine ortsfeste Entität im Raum mit Zuständen, geschlossenen Effektlisten und optionaler Aufnahme für genau ein Item.

```yaml
devices:
  - id: fackelhalterung
    name: "Eiserne Wandhalterung"
    desc: "Eine geschmiedete Wandhalterung für Fackeln."
    keys: [halterung, wandhalterung]
    location: krypta
    fits_tag: lichtquelle               # oder: fits: [fackel_a, fackel_b]
    insert_msg: "Die Fackel knistert in der Halterung."
    remove_msg: "Das Licht stirbt, als du die Fackel herausziehst."
    on_insert:
      - set_flag: krypta_beleuchtet
        val: "true"
    on_remove:
      - set_flag: krypta_beleuchtet
        val: "false"

  - id: hebel_tor
    name: "Messinghebel"
    desc: "Ein massiver Hebel mit zwei Rastungen."
    keys: [hebel, messinghebel]
    location: krypta
    flip_verb: umlegen
    flip_states: [unten, oben]
    on_flip_oben:
      - msg: "Ein fernes Rollen erschüttert den Boden."
      - set_state: tortraverse
        to: offen
    on_flip_unten:
      - set_state: tortraverse
        to: zu
```

### Felder

| Feld | Typ | Default | Beschreibung |
|---|---|---|---|
| `id` | String | **required** | Eindeutige Kennung der Vorrichtung |
| `name` | String | `id` | Spieler-sichtbarer Name |
| `desc` / `description` | String | `name` | Beschreibung beim Untersuchen (`examine <device>`) |
| `keys` | [String] | `[id]` | Schlüsselwörter für Parser-Matching |
| `location` | String | **required** | Raum-ID, in dem sich die Vorrichtung befindet |
| `fits_tag` | String | — | Tag für passende Gegenstände (z. B. `lichtquelle`, `sicherung`) |
| `fits` | [String] | `[]` | Liste expliziter Item-IDs, die eingesteckt werden können |
| `insert_msg` | String | Default | Meldung beim Einbau des Items |
| `remove_msg` | String | Default | Meldung beim Ausbau des Items |
| `on_insert` | [AActionOutcome] | `[]` | Effekte beim erfolgreichen Einbau |
| `on_remove` | [AActionOutcome] | `[]` | Effekte beim erfolgreichen Ausbau |
| `flip_verb` | String | — | Verb zum Hin-/Herschalten (z. B. `umlegen`, `flip`) |
| `flip_states` | [String] | `[]` | Liste der Zustände (mindestens 2; bei 2 Zuständen: Toggle) |
| `on_flip_<state>` / `on_flip:` | [AActionOutcome] / Map | `[]` | Effekte beim Erreichen des jeweiligen Zustands |

### Vokabel & Primitives (Tür I)

- **Prädikat `ActorHas ActorRef ItemID`:** Prüft, ob ein Actor (Spieler `ActorPlayer`, NPC `ActorNPC <id>` oder Vorrichtung `ActorEntity <id>`) ein Item besitzt.
- **Effekt `Mount ItemID ActorRef`:** Bewegt ein Item in den Besitz des angegebenen Actors (z. B. `CarriedBy (ActorEntity "fackelhalterung")`).
- **Effekt `Unmount ItemID`:** Entfernt das eingesteckte Item aus der Vorrichtung und legt es zurück in den Raum der Vorrichtung.
- **Persistenz:** Keine neuen `SaveState`-Felder. Eingesteckte Items liegen in der bestehenden `itemStates`-Map mit `itemLocation = CarriedBy (ActorEntity devId)`.

### Fit-Kontrakt & Veto-Muster

- **Einbau:** `stecke <item> in <halterung>` (bzw. `insert <item> into <halterung>`).
  - Item passt, wenn `fits_tag` übereinstimmt **oder** die Item-ID in `fits` gelistet ist.
  - Bereits belegt: Ablehnung mit Meldung, **kein Zugverbrauch** (2.2-Veto-Muster).
  - Unpassendes Item: Ablehnung mit Meldung, **kein Zugverbrauch**.
  - Item nicht getragen: Ablehnung mit Meldung, **kein Zugverbrauch**.
  - Erfolg: Item wird gemountet, `insert_msg` ausgegeben, `on_insert`-Effektliste ausgeführt, Zug wird verbraucht.
- **Ausbau:** `ziehe <item> aus <halterung>` bzw. `ziehe <halterung>` (oder `remove ...`).
  - Vorrichtung leer: Ablehnung mit Meldung, kein Zugverbrauch.
  - Erfolg: Item landet im Raum, `remove_msg` ausgegeben, `on_remove`-Effektliste ausgeführt, Zug wird verbraucht.
- **Sichtbarkeit:** `examine <halterung>` zeigt die Beschreibung und bei montiertem Item automatisch den Hinweis `Mounted: <Item-Name>.` an. Ausgebaute Items erscheinen in der Raum-Itemliste.

### Das Beleuchtungsmuster (Ersatz der Licht-Propagation)

W4 ersetzt bewusst aufwändige Licht-Physik/Emergenz durch **explizite Zustände und Effekte**:
- Raum als dunkel deklarieren: `tags: [dark]`, `light_flag: krypta_beleuchtet`.
- Fackelhalterung im Raum schaltet bei `on_insert` das Flag `krypta_beleuchtet` auf `"true"`.
- Bei `on_remove` schaltet die Halterung das Flag `krypta_beleuchtet` auf `"false"`.
- Die Dunkelheitsprüfung des Spiels (`isDark`) wertet das `light_flag` des Raumes aus — ohne Emergenz, rein deterministisch und transparent für Autoren und Spieler.

## Container (4.4)

Container fassen Items — **tragbar** (Items mit `capacity:`) und **ortsfest** (die
`containers:`-Sektion, wie `devices:`). Beide teilen dieselbe Mechanik: der Zustand lebt
in den Entity-States (`open`/`closed`/`locked`), der Inhalt in den Item-Locations
(`in_container:`), die Kapazität ist zählbasiert (Anzahl Items).

```yaml
items:
  - id: truhe
    name: Truhe
    capacity: 3            # dieses Item ist ein Container
    location: halle
  - id: amulett
    name: Amulett
    in_container: truhe    # startet in der Truhe (Verschachtelung beliebig tief)

containers:
  - id: kiste
    name: Kiste
    location: halle
    capacity: 2
    locked: true           # startet verschlossen (open: true = offen, Default: geschlossen)

player:
  inventory_limit: 5       # zählbasiertes Inventarlimit (VarMap `inventory.limit`)

effects:
  - set_inventory_limit: 8 # zur Laufzeit änderbar (z.B. nach einer Quest)
```

**Gebaute Verben** (immer verfügbar): `open`/`close`/`lock`/`unlock` (Zustandswechsel),
`take X from Y` und `put X in Y` (einzeln, Kapazität geprüft). `take X` findet X auch in
**offenen** Containern — geschlossene schneiden den Zweig ab, die Verschachtelung geht
beliebig tief (`look` zeigt den Inhalt offener Container).

**Verträge:**

- Item-Container starten **offen** (kein Entity-State); `containers:`-Einträge starten mit
  `open:`/`locked:`. Alle Zustände sind jederzeit per `set_state:` setzbar.
- **Key-Bindung ist Autoren-Vokabel:** `lock`/`unlock` wechseln nur den Zustand. Wer einen
  Schlüssel verlangt, sperrt per Regel (z.B. `on: "command unlock"` + `when: {not: {actor_has:
  {actor: player, item: schluessel}}}` + `block: "…"`).
- Das Inventarlimit ist eine Zahl in der VarMap (`inventory.limit`) — die Welt setzt es über
  `player: {inventory_limit:}`, der Laufzeit-Effekt `set_inventory_limit:` ändert es. Beim
  Überschreiten verweigert `take`/`take from` (`inventory.full`).
- Die Kapazität (`capacity:`) zählt die **direkt** enthaltenen Items (zählbasiert, kein
  Gewicht/Volumen).

## NPC-Besitz (B7)

NPCs können Items **tragen** — der Wächter hat den Schlüssel, die Händlerin ihre Ware,
der tote Räuber seine Beute. Der Zustand lebt wie bei Containern in den Item-Locations:
`CarriedBy (ActorNPC …)`. **Kein neues SaveState-Feld** (Zero-New-Fields-Vertrag).

```yaml
items:
  - id: schluessel
    name: Schluessel
    location: halle          # Raum, in dem der NPC steht (Scope/Keyword-Prüfung)
    carried_by: waechter     # startet im Besitz des Waechters
```

**Autorenform:** `carried_by: <npc-id>` am Item (wie `in_container:`) — ein Item startet
höchstens **einer** Quelle: `in_container:` + `carried_by:` zusammen ist ein harter
Compiler-Fehler (`CarriedByConflict`). `carried_by: player` ist erlaubt (= Start im
Spielerinventar). Unbekannte NPC-IDs sind ein harter Fehler (`UnknownNpc`).

**Befehle** (gebaut, immer verfügbar):

- `take X from <npc>` — nimmt ein getragenes Item an sich (Bestehlen, Leichen plündern;
  `inventory.full` wird geprüft). Hat der NPC kein passendes Item: `npc.no_item`.
- `give X to <npc>` / `gib X an <npc>` — übergibt ein eigenes Item; der NPC trägt es danach.
  Beide kosten einen Zug (L13-Urteil `GiveCmd` = `True`).

Reihenfolge der Zielauflösung bei `take X from Y`: erst Container (`findContainerRef`),
dann NPC im Raum. Ein toter NPC bleibt ein gültiger Träger („die Leiche behält ihre
Beute“).

**Sichtbarkeit:** `look at <npc>` zeigt die getragenen Items als Katalog-Zeile
(`npc.carries`, „Carrying: …“ — wie die „Mounted:“-Zeile bei `devices:`). Versteckte Items
(`hidden: true`) erscheinen erst nach `discover`. Das Protokoll-Snapshot führt die Liste im
`carried`-Feld von `NpcSummary` (leer = Feld wird weggelassen, byte-kompatibel).

**Effekte:** `give: <item>` gibt weiterhin **an den Spieler** (byte-kompatibel); die
Objekt-Form `give: {item: X, to: <npc-id>}` überreicht an NPCs (Validierung wie
`carried_by:`). `mount:`/`unmount:` bleiben die Geräte-Vokabel (W4). `move_npc:` bewegt
NPCs samt Besitz (Items hängen am Actor, nicht am Raum).

**Diebstahl-Sperren** setzt der Autor per Veto-Regeln (`on: before` + `block:`, siehe
Phase 2.2) — die Before-Phase läuft für alle Kommandos, `cmd.verb` ist `take` bzw. `give`.

**Grenzen (bewusst):** keine NPC-**Ausrüstung** (`EquippedBy ActorNPC` ist im
Zustandsmodell vorgesehen, aber noch nicht autorenseitig), kein automatisches Fallenlassen
beim Tod, kein `take all from <npc>`.

## Geschlossene Mengen-Operationen (B3)

Die 20-Zweig-Kaskaden („Schaden an alle im Raum, dann alle Schlüssel entfernen, dann …")
sind jetzt fünf **geschlossene** Effekte. „Geschlossen" ist der Vertrag: der Autor liefert
**nie** eine Effektliste als Schleifenkörper — jede Operation hat ihre feste Semantik
(keine Turing-Vollständigkeit, kein Autoren-Kontrollfluss).

```yaml
effects:
  - damage_all:   {what: alive_npcs, in: halle, amount: 5}      # Schaden an alle
  - move_all:     {what: items, in: halle, to: {in: keller}}    # alle bewegen
  - move_all:     {what: items, in: halle, to: {by: player}}    # alles einsammeln
  - reveal_all:   {what: items, in: halle}                      # alle versteckten aufdecken
  - consume_all:  {what: items, in: halle, tag: schwer}         # alle entfernen
  - set_state_all: {what: items, in: halle, tag: licht, state: brennend}
```

Die **Zielmengen** sind dieselbe Sprache wie die B2-Abfragen (`what` × `in:`/`by:` ×
optionaler `tag:`) — Abfrage und Wirkung sprechen eine Sprache. Die Wirkung pro Zielart:

| Operation | Items | NPCs |
|---|---|---|
| `damage_all` | — | Health (wie `damage:`, keine Klemmung) |
| `move_all` | Location (Raum oder `by:`-Akteur) | Position |
| `reveal_all` | `itemDiscovered` | — |
| `consume_all` | entfernt (`Removed`) | — |
| `set_state_all` | `itemStatus` | `npcStatus` |

**Wichtig:** die Menge ist **jedes** Item am Ort — auch versteckte (sonst fände
`reveal_all` seine Ziele nie). Die B2-Zählung zählt damit auch versteckte Items: sie ist
eine Autoren-Abfrage, keine Sichtbarkeits-Abfrage.

## Mengen- und Tag-Abfragen (B2)

Die größte Autorenqual war die **Aufzählung** („hat der Spieler irgendeinen Schlüssel?",
„ist hier noch jemand?"). B2 exponiert das als allgemeine Vokabel — ohne Zwei-Wege-Katalog
und genre-neutral.

**Tag-Prädikate:**

```yaml
when: { actor_has_tag: {actor: player, tag: licht} }   # trägt etwas mit Tag X
when: { room: halle, has_item_tag: licht }             # liegt hier etwas mit Tag X?
```

**Die Zähl-Familie** (Int-Werte, überall nutzbar, wo Werte stehen — `compare_var`,
`compare`, `compute_var`):

```yaml
when: { compare_var: {name: count.items.in.halle, op: gte, value: 2} }   # Anzahl ≥ n
when: { compare_var: {name: count.alive_npcs.in.halle, op: eq, value: 0} }  # alle tot
# String-Form: count.<was>.<in|by>.<id>[.<tag>]
#   count.items.in.halle   count.npcs.in.halle   count.alive_npcs.in.halle
#   count.items.by.player  count.items.tag.licht.in.halle
when: { compare: { lhs: {count: {what: items, in: halle, tag: licht}}, op: gte, value: 1 } }
```

| Dimension | Werte | Anmerkung |
|---|---|---|
| **was** | `items`, `npcs` (alle), `alive_npcs` (lebende) | `npcs` zählt alle im Raum, `alive_npcs` nur lebende — „alle tot" ist `alive_npcs == 0` |
| **wo** | `in: <raum>` / `by: <actor>` | `by:` zählt Getragenes (Inventar + Ausrüstung); für NPCs ist `by:` immer 0 (getragene NPCs gibt es nicht) |
| **tag** (optional) | `tag: <tag>` | filtert auf Items mit diesem Tag; nur bei `items` |

**Anmerkungen:**

- `compare_var` liest auch die `distance.`- und `count.`-Werte (Tür IV / B2) — wie
  `condition_turns.` zuvor.
- Fehlende Räume/IDs zählen als **0** (wie bei den bestehenden Prädikaten); eine
  Referenzprüfung für Prädikat-Räume gibt es bisher nicht (offene Härterung, siehe
  `plan-autorenfaehigkeiten.md`).

## Bibliotheken: `include:` (5.3)

Ein Abenteuer kann **Bibliotheksdateien** einbinden (Prozeduren, Verben, Regeln, Inhalte —
„Pakete"). Damit wird ein umfangreiches Spiel wartbar: wiederverwendbare Sektionen liegen
in eigenen Dateien statt als Kopie in jeder Welt.

```yaml
# hauptspiel.yaml
name: Mein Spiel
start_room: halle
rooms: [ {id: halle, name: Halle, desc: …} ]
include: [lib/basis.yaml, lib/kampf.yaml]

# lib/basis.yaml — eine Bibliothek
procedures:
  - id: warten
    effects: [ … ]
rules:
  - id: basis_tick
    on: "turn"
    effects: [ … ]
```

**Merge-Vertrag (fest, weil YAML-Objekte keine Reihenfolge haben und Trigger-Reihenfolge
semantisch wirksam ist):**

1. **Eigene Sektionen zuerst, dann Includes** — die Hauptdatei hat Vorrang (ihr erster
   `block:` stoppt die Bibliothek). Includes mergen in `include:`-Listenreihenfolge.
2. **Transitiv**: Bibliotheken dürfen selbst `include:` schreiben (Tiefensuche; eine
   Datei kommt vor ihren eigenen Includes). Ein **Kreisbezug ist ein harter Fehler**;
   eine Datei, die über zwei Wege erreichbar ist (Diamond), wird **einmal** geladen —
   an der Position ihres ersten Vorkommens.
3. **Single-Value-Felder sind der Hauptdatei vorbehalten** (`name`, `start_room`,
   `player`, `combat`, `game`, `journal`, `progression`, `interactions`, …). Eine
   Bibliothek, die eines setzt, ist ein **harter Fehler** (mit Pfad).
4. **Doppelte IDs über Dateigrenzen sind ein harter Fehler**, der beide Dateien nennt
   (`rooms: 'halle' defined in a.yaml and b.yaml`). Prozeduren, Räume, Items, NPCs,
   Regeln, Facts, Kapitel, Vorrichtungen, Karten, Verben, Variablen … — alle
   ID-tragenden Sektionen; `tests:`-Namen sind keine referenzierbaren IDs.
5. **Byte-Identität**: derselbe Inhalt kompiliert zur identischen Welt, egal ob er in
   einer Datei liegt oder in Bibliotheken aufgeteilt ist (getestet).

## Verfolgung: `pursuit:` und die Distanz-Vokabel (Tür IV)

Verfolgung/Pfadsuche als **Kern-Abfrage über den Laufzeit-Graphen**: der Autor schreibt nur
die `pursuit:`-Sektion und nutzt die Vokabel — die Suche selbst liegt in der Engine
(`src/Pursuit.hs`, stateless, rein, deterministisch).

```yaml
pursuit:
  - npc: wolf            # der Verfolger (NPC)
    target: player       # optional, Default: player — auch: <npc-id>, "ship:<id>", "room:<id>"
    ignores: [locked]    # optional: "locked" / "guarded" Ausgänge passierbar
    msg: "Der Wolf folgt."   # optional: eigene Meldung pro Schritt

rules:
  - id: flucht
    on: "command fliehe"
    effects:
      - step_away_from: [taeter, npc_wolf]   # eine Kante weg
  - id: alarm
    when: { compare: { var: "distance.wolf.player", op: "<=", value: 2 } }
    effects:
      - msg: "Du hörst Pfoten!"
```

**Wortschatz:**

- **`distance.<sucher>.<ziel>`** — Int-Wert (Hops), **`-1` = unerreichbar** (auch wenn Sucher oder
  Ziel gerade keinen Raum haben). In `compare_var`/`compare`/`compute_var` nutzbar.
  Ziel-Formen: `player`, NPC-ID, `ship:<id>`, `room:<id>`.
- **`step_toward: [sucher, ziel, msg?]`** / **`step_away_from: [sucher, ziel, msg?]`** — bewegt
  den Sucher **genau eine Kante** Richtung Ziel (bzw. streng davon weg), nie den ganzen Pfad.
  Ohne `msg` gilt der Katalog (`pursuit.step`/`pursuit.flee` mit `{name}`/`{room}`/`{dir}`);
  ohne Schritt `pursuit.no_path`. Nur **NPCs und Schiffe** dürfen Sucher sein.
- **`pursuit:`** erzeugt pro Eintrag einen `on: turn`-Trigger (ein Schritt pro Zug),
  deterministisch in NPC-ID-Reihenfolge emittiert.

**Regeln (Verträge):**

- **Tie-Break fest und sichtbar:** bei Gleichstand gewinnt die **kleinste Ziel-Raum-ID**, dann
  eine feste Richtungspriorität (`Pursuit.directionPriority`, „Gameplay-Vertrag, nicht
  umsortieren"). Unabhängig von `Direction`s abgeleitetem `Ord` — ein Refactor dort darf
  keine Läufe ändern.
- **Default „wie der Spieler":** der Verfolger sieht dieselben Ausgänge wie der Spieler
  (offen, aufgeschlossene Türen, erfüllte Wachen-Prädikate). `ignores: [locked|guarded]`
  macht Ausnahmen explizit (Fairness bleibt Autorenentscheidung).
- **Stateless und rein:** kein Feld im Save, kein `rngState`-Verbrauch — speichern/laden
  mitten in der Verfolgung rechnet identisch weiter.
- **`-1` ist der einzige Unerreichbar-Sentinel** (dokumentiert und getestet).

## Spiel-Export: `assets:` und `worldbuilder export` (B6)

Ein fertiges Spiel verlässt das Repo als **ein** Verzeichnis: die kompilierte Welt, alle
referenzierten Asset-Dateien und ein Launcher.

```yaml
# spielwurzel-relativ wie alle Laufzeit-Pfade
assets:
  - handbuch.txt          # Extra-Dateien (README, Cover, Handbücher)
  - audio/hinweis.wav
```

**Asset-Pfade** (`sfx:`/`music:`-Effekte **und** die `assets:`-Liste) sind
**spielwurzel-relativ** — aufgelöst gegen das Verzeichnis der Hauptdatei beim Export und
gegen das Bundle-Wurzelverzeichnis (Arbeitsverzeichnis des Launchers) zur Laufzeit. Die
Pfadstrings in der kompilierten Welt ändern sich dadurch **nie**. Kunst und Clips brauchen
keine Dateien: sie sind zur Compile-Zeit eingebettet (D14).

```bash
worldbuilder export mein-spiel.yaml -o dist/mein-spiel [--with-engine] [--zip] [--force]
```

**Bundle-Layout:**

```
dist/mein-spiel/
  world.json        # byte-identisch zu `worldbuilder compile`
  save.json
  play.sh           # Unix/macOS: cd ins Bundle, Engine-Suche, Audio-Helper-Autodetect
  play.bat          # Windows (wie packaging/windows/play.bat)
  bin/              # nur mit --with-engine: text-adventure Binary
  audio/...         # referenzierte Assets (relatives Layout unverändert)
```

**Verträge:**

- `world.json`/`save.json` sind **byte-identisch** zum `compile`-Output — der Export legt
  nur Dateien darum herum.
- Fehlende Assets sind **Warnungen** (das Bundle wird trotzdem geschrieben); Pfade, die
  über das Bundle hinausweisen (`..`, absolute Pfade), werden verworfen und gemeldet.
- Der Launcher sucht die Engine zuerst in `bin/` (nur mit `--with-engine`), dann auf `PATH`
  (`text-adventure`). Extra-Argumente werden durchgereicht (z.B. `play.sh --tui`).
- `--zip` legt `<dir>.zip` neben dem Bundle an (benötigt `zip` oder `7z`).

## Regel-Diagnostik (B4)

Der Worldbuilder beantwortet vier Fragen zum **toten Inhalt** — beim Kompilieren,
nicht beim Spielen. Die Analyse ist **konservativ** (dreiwertig, „nie" = garantiert):
eine Warnung bedeutet, dass der Inhalt definitiv nie erreicht wird.

| Frage | Warn-Code | Erkennung |
|---|---|---|
| „Welche Regel feuert nie?" | `UnreachableTrigger` | `on:`-Referenzen ohne Gegenstück (unbekannte Räume/Items, `custom X` ohne `raise: X`, `chapter` ohne Kapitel, `levelup` ohne `progression:`); dazu jeder `when:`-Widerspruch (`UnsatisfiableCondition` auf `rules.<id>`) |
| „Welche Bedingung ist unerfüllbar?" | `UnsatisfiableCondition` | Widersprüche in `all:` (Bedingung + Negation, widersprüchliche Zahlen-Schranken einer Variablen, zwei Textwerte für eine Variable), `not: {true: true}`, `has_flag` auf **nie gesetzte** Flags (nur Flags: `set_flag`/`initial_flags` als Setz-Seite), `any:` nur-Falsch |
| „Welcher Guard ist widersprüchlich?" | `DeadExit` | Ausgangs-`when:` garantiert falsch; `locked_by:`-Entity, die nie aufgeschlossen werden kann (Unlock-Pfade: NPC-Tod, Container-`unlock`-Verb, `interactions` mit `state: unlocked`, `set_state … to: unlocked`) |
| „Welcher Ausgang ist nie passierbar?" | `DeadExit` / `UnreachableRoom` | Guard-Analyse plus BFS ab `start_room`: dynamische Kanten (`set_exit`, `generate_room`) und explizite Ankünfte (`move:`, Haltestellen) gelten als erreichbar — Ausgänge in/to solcher Räume sind nie passierbar, pro Raum einmal gemeldet |

**Nicht analysiert** (bewusst): Variablen-Setz-Seiten (die Engine schreibt viele
Namespaces selbst: `cmd.*`, `distance.*`, `combat.*` …), Bedingungen in Texten
(`{if …}`), Laufzeit-Zufall (`random:`). `on: command/before <verb>` mit unbekanntem
Verb ist ohnehin bereits ein harter Fehler (`UnknownCommandVerb`).

## Validierungs-Warnungen (Compiler-Diagnosen)

Der Worldbuilder unterscheidet strikt zwischen **harten Fehlern** (`ciSeverity = SError`) und **nicht-fatalen Warnungen** (`ciSeverity = SWarning`):
- **Harte Fehler** (z. B. Syntaxfehler, unbekannte Raum-IDs in Exits, ungültige Item-Referenzen) verhindern das Kompilieren und lassen `worldbuilder validate` bzw. `worldbuilder compile` mit Exit-Code 1 abbrechen.
- **Nicht-fatale Warnungen** weisen Autoren auf potenzielle logische Probleme, Mehrdeutigkeiten oder Sackgassen hin. Sie führen zu verständlichen Hinweisen und Reparatur-Vorschlägen im Terminal, brechen die Validierung/Kompilierung aber **nicht** ab (Exit-Code 0).

Übersicht stabiler Warn-Codes (`ciCode`):

| Warn-Code | Ursache | Reparatur-Empfehlung |
|---|---|---|
| `KeywordCollision` | Zwei Items oder ein Item und ein NPC im selben Raum teilen sich dieselben Keywords (`keys`, `id`, `name`). | Namen oder `keys` der Entitäten im Raum eindeutig machen, um Mehrdeutigkeiten bei Spielerbefehlen zu vermeiden. |
| `UnknownPlaceholder` | Ein `{name}`- oder `{var:name}`-Platzhalter in Texten referenziert eine nicht deklarierte Variable. | Variable in `variables:` deklarieren, Schreibfehler korrigieren oder wörtliche geschweifte Klammern maskieren (`\{...\}`, `{{...}}`). |
| `DarkRoomDeadEnd` | Ein dunkler Raum (`dark: true` oder Tag `"dark"`) enthält Items, hat aber weder ein `light_flag`, noch existiert eine erreichbare Lichtquelle (`lightsource`), noch ist ein Item als `feelable` getaggt. | Raum mit `light_flag:` versehen, ein erreichbares Item als `tags: [lightsource]` deklarieren oder ertastbare Items mit `tags: [feelable]` kennzeichnen. |
| `IronmanWithoutSavezones` | `ironman: true` ist im `game:`-Block aktiviert, aber `save_zones` ist leer. | `save_zones: [room1, ...]` angeben oder den Hardcore-Modus (ohne Speichermöglichkeit) bewusst beibehalten. |
| `UnknownDeviceTag` | Das Feld `fits_tag` einer Vorrichtung matcht keinen Tag deklarierter Items. | Schreibweise des Tags prüfen oder sicherstellen, dass passende Items diesen Tag tragen. |
| `DeviceWithoutEffects` | Eine Vorrichtung besitzt weder Insert-, Remove- noch Flip-Effekte. | Effekte für Interaktionen hinterlegen oder Vorrichtung bei rein dekorativem Zweck belassen. |
| `GainXpWithoutProgression` | Ein `gain_xp`-Effekt wird verwendet, aber das Abenteuer deklariert keine `progression:`-Sektion. | `progression:` mit Stufentabelle deklarieren oder den `gain_xp`-Aufruf entfernen. |
| `UnreachableTrigger` | Eine Regel hört auf ein Ereignis, das nie eintreten kann (unbekannter Raum/Item in `on:`, `custom X` ohne `raise: X`, unbekanntes Kapitel, `levelup` ohne `progression:`). | `on:`-Referenz korrigieren, den `raise:`-Effekt ergänzen oder die Regel entfernen. |
| `UnsatisfiableCondition` | Eine Bedingung (`when:`, `if:`, Tore wie `visible_when:`) kann nie wahr sein: Widerspruch in `all:` oder ein Flag, das nie gesetzt wird. | Widerspruch aufteilen, Flag per `set_flag`/`initial_flags` setzbar machen oder die Bedingung vereinfachen. |
| `DeadExit` | Ein Ausgang ist nie passierbar: sein Guard kann nie zutreffen oder seine `locked_by:`-Entity kann nie aufgeschlossen werden. | Guard reparieren, einen Unlock-Pfad (`set_state … to: unlocked`, `interactions`, `unlock`-Verb) ergänzen oder den Ausgang entfernen. |
| `UnreachableRoom` | Ein Raum ist ab `start_room` nicht erreichbar — alle seine Ausgänge sind nie passierbar. | Verbindenden Ausgang ergänzen, per `move:`/`set_exit` erreichbar machen oder den Raum entfernen (Sackgassen-Content wie Test-Fixtures sind legitim). |


