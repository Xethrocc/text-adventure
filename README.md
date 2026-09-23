# text-adventure

A data-driven text adventure engine in Haskell. The **engine** plays the game;
the **worldbuilder** turns authored YAML/JSON into the data the engine loads.
There is no engine code per genre — rooms, items, NPCs, dialogue, rules, combat
profiles and gameplay modules are all content.

## Features

**Engine (`src/`, package `text-adventure`)**

- Rooms with exits and locks (`locked_by`), darkness with light sources
- **ASCII art as data:** state-dependent (`CondText`), animated (`frames`/`every`, played by `watch`), coloured (24-bit ANSI, automatically stripped when stdout is not a terminal or `--no-color` is set), and interactive — marker glyphs bound to items/NPCs (`hotspots`, numbered by `map`/`legend`, addressed with `look at <n>`)
- **Title and game-over banners** from `title_art` / `end_art` (generate them with `text2ascii`), falling back to the built-in frames when absent
- Items: containers, equipment with stat bonuses, portability, hidden items found via `search`
- NPCs: dialogue trees with conditional choices, per-NPC verb maps, state-dependent behaviour
- **Rule core:** an effect DSL (`SetValue`, `ModifyValue`, `MoveEntity`, `Conditional`, `RandomChoice`, `GameEnd`, `Narrative`, `ApplyCondition`, `RaiseEvent`, …) plus triggers on events (`enter`, `leave`, `look`, `search`, `take`, `drop`, `use`, `state`, `command`, `turn`, `custom`) with `when` / `once` / `cooldown`
- Variables as a `VarMap` for counters, faction standing and supplies, plus predicates such as `standing`, `compare_var`, `has_item`, `has_flag`, `at: player, room: …`
- Quests with stages and rewards; conditions with tick and end effects
- Vehicles: player-steered, automatic routes and paid rides — fuel, interior rooms, stops — and **ships with systems and stations** (power/shields/hull/weapons, station verbs, ship-to-ship combat)
- Combat profiles `off` | `narrative` | `classic` | `tactical` (turn-based with initiative, flee, defend, and player abilities), resolved by a pure combat module that also drives companions and spaceship duels
- **Rogue platform:** permadeath/ironman policies with savezones, persistent meta-progression (`meta.*` variables), dynamic exits (`set_exit`/`remove_exit`) — all backward compatible, opt-in per adventure
- Deterministic RNG **in the save**, save/load (`saves/<slot>.json`), undo history, restart, tab completion
- **Brick TUI** (`--tui`): scrollable history, tab completion, animated art panel, and a Roguelike dashboard — ASCII minimap of visited rooms (dynamic exits included), HP/variable gauges, status effects, equipment, and a combat panel that appears when a fight is engaged
- Validation: `validateWorld` + `validateGameState` (used by the CLI and the tests)

**Worldbuilder (`worldbuilder/`)**

- Authoring schema for everything above, in YAML or JSON, with structured diagnostics
- CLI: `validate`, `compile`, `check`
- Optional gameplay modules (Phase 7): factions/standing, trade, encounter tables, survival/weather, stealth, tactical combat & abilities, party/companions, starships with duels — see `docs/modules.md`

## Quick start

```bash
cabal build all

# play the bundled sample adventure
cabal run text-adventure-cli   # das Haupt-CLI (Haskeline; `--tui` startet die Brick-Oberflaeche)

# author: check and compile an adventure
cabal run worldbuilder -- validate examples/thefog.yaml
cabal run worldbuilder -- compile examples/thefog.yaml -o /tmp/thefog

# play what you compiled
cabal run text-adventure-cli   # das Haupt-CLI (Haskeline; `--tui` startet die Brick-Oberflaeche) -- --world /tmp/thefog/world.json --save /tmp/thefog/save.json
```

Engine flags: `--world FILE` (compiled GameWorld), `--save FILE` (initial
SaveState, must exist), `--allow-invalid`, `--no-color` / `--color`
(`--no-color` is the default whenever stdout is not a terminal), `--help`.
In-game saves are written to `saves/<slot>.json`. The directory can be
redirected with `--saves-dir DIR` or the `TA_SAVES_DIR` environment variable
(the flag wins) — used by tests and CI for hermetic runs.

Whole pipeline — build, all four test suites, validation of 22 shipped
adventures, 32 scripted playthroughs (23 happy paths + 9 non-victory runs):

```bash
bash scripts/ci.sh
```

### Windows without Haskell

CI runs the same pipeline on `windows-latest` and uploads a ready-to-use
release ZIP as the artifact `text-adventure-win64` (see the [Actions
tab](https://github.com/Xethrocc/text-adventure/actions)). Unpack it,
double-click `play.bat`, type `demo`, press Enter — no Haskell, no WSL, no
admin rights. Authoring works the same way: put a YAML file into
`adventures\`, double-click `check.bat`, then `play.bat`. `START-HERE.txt`
inside the ZIP walks through it step by step.

## In-game commands

- Movement: `go` / `move` / `walk <direction>`, or just the direction
- `look`, `look at` / `examine <target>`, `search`
- `look at <n>` (address the n-th object marked in the art; `map` numbers them), `watch [target]` (play animation frames), `map` / `legend` (art with numbered markers + legend)
- `take` / `get` / `grab <item>`, `take all`, `take <item> and <item>`, `drop <item>`, `drop all`
- `use <item>`, `use <item> on <target>`
- `talk to <npc>`, `choose <n>` (pick a dialogue option)
- Combat: `attack` / `hit` / `fire <target>` (NPCs and enemy ships at the same stop), `defend`, `flee`, `use-ability <id>` / `ability <id>`
- `equip` / `wear` / `wield <item>`, `unequip` / `remove`, `unequip all`, `stats`
- Vehicles: `enter` / `board <vehicle>`, `disembark`, `drive to <station>`, `wait`, `refuel`, `repair <condition>` — note that `exit` quits the game
- System: `inventory`, `undo`, `save [name]`, `load [name]`, `saves`, `restart`, `help`, `quit`
- `Tab` completes commands, directions and reachable targets

## Repository layout

| Path | Content |
|---|---|
| `src/` | engine: types, game logic, parser, combat, validation, save/load |
| `app/` | CLI entry point (flags, validation gate, game loop) |
| `worldbuilder/` | authoring schema, compiler, validator, CLI |
| `img2ascii/` | helper tool: images → ASCII art for room banners |
| `text2ascii/` | helper tool: text → banner art (block/slant/outline fonts) for title and end screens |
| `examples/` | `thefog.yaml` (reference game), `demo.yaml`, `genres/` (6 genre fixtures), `modules/` (7a–7h fixtures + `combo.yaml` composition proof), `fixtures/` (small feature fixtures), `templates/` (dungeon template for the world generator) |
| `ci/e2e/` | scripted playthroughs: `<name>.in` plus the expected marker in `<name>.expect` |
| `docs/` | `adventure-schema.md`, `modules.md`, `genres.md`, `worldgen.md`, `thefog-playthrough.md` |
| `scripts/ci.sh` | the whole pipeline in one command |

## Authoring

Start with `docs/adventure-schema.md`. Minimal adventure:

```yaml
name: "My Adventure"
start_room: hall

rooms:
  - id: hall
    name: Hall
    desc: "A cold stone hall. A corridor leads north."
    exits:
      north: {to: cellar}

npcs:
  - id: cat
    name: cat
    location: hall
    keys: [cat, katti]
    max_hp: 9
    attack: 1
    defense: 1

variables:
  - {name: courage, type: int, initial: 0}

rules:
  - id: first_visit
    on: enter hall
    once: true
    effects:
      - {msg: "The door falls shut behind you."}
      - {add_var: courage, delta: 1}
```

Custom verbs are authored, not compiled in: declare `verbs: [{name: pray}]`,
then attach effects to it with a `rules:` entry (`on: command pray`), an item's
`verb_map`, or an NPC's `verb_map` — no Haskell changes needed.

Gameplay modules (factions, trade, encounters, survival, stealth, party,
starships) are optional YAML segments that follow one rule: no new interpreter,
no own state file. Their state lives in the existing `VarMap`
(`faction.<id>`, `party.<npc>`, `ship.<id>.<system>`) — see `docs/modules.md`.

## Architecture

- **GameWorld** (static blueprint) vs **SaveState** (dynamic play data) vs
  **GameState** (both together). Saves persist only the `SaveState`, never the
  world.
- **One interpreter:** every effect — parser dispatch, rule triggers, dialogue
  choices, quest rewards, room hooks, vehicle conditions — runs through
  `applyOutcomeWith`. Modules compile down to existing concepts instead of
  adding code paths.
- **State lives in the existing `SaveState`**, preferably as `VarMap` entries,
  so no feature needs its own save file or state silo.
- Main modules: `Types.hs`, `Game.hs`, `GameLoop.hs`, `Parser.hs`, `Combat.hs`,
  `Validate.hs`, `SaveLoad.hs`, `Verbs.hs`, `World.hs`, `Sample.hs`, `Ansi.hs`
  plus `worldbuilder/src/Worldbuilder/{Types,Compile,CLI,ParseFile}.hs`.

## Development

```bash
cabal build all
cabal test all --test-show-details=direct    # 259 engine tests, 83 worldbuilder tests, plus the img2ascii/text2ascii tool suites
bash scripts/ci.sh                           # build + tests + validation + 32 E2E playthroughs
cabal run worldbuilder -- check examples/thefog.yaml   # content statistics
```

Adding a feature end to end: an adventure in `examples/` (with a fixture under
`examples/modules/` for optional systems), an `.in`/`.expect` pair in `ci/e2e/`,
an entry in `scripts/ci.sh`, tests in `test/Tests.hs` or
`worldbuilder/test/Tests.hs`, and a docs update.

## Requirements

- GHC 9.6 (developed with 9.6.7) and cabal-install 3.14+
- Dependencies are resolved by cabal: `aeson`, `aeson-pretty`, `bytestring`,
  `containers`, `text`, `haskeline`, `time`, `directory` (engine);
  `HsYAML-aeson`, `filepath` (worldbuilder); `JuicyPixels`, `vector` (img2ascii);
  `text2ascii` needs only `base`

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Run `bash scripts/ci.sh`
6. Submit a pull request

## License

MIT — see [LICENSE](LICENSE).

## Acknowledgments

- Inspired by classic text adventure games
- Built with Haskell's type safety and functional programming principles
