# text-adventure

A data-driven text adventure engine in Haskell. The **engine** plays the game;
the **worldbuilder** turns authored YAML/JSON into the data the engine loads.
There is no engine code per genre — rooms, items, NPCs, dialogue, rules, combat
profiles and gameplay modules are all content.

## Features

**Engine (`src/`, package `text-adventure`)**

- Rooms with exits and locks (`locked_by`), darkness with light sources, optional ASCII-art banners
- Items: containers, equipment with stat bonuses, portability, hidden items found via `search`
- NPCs: dialogue trees with conditional choices, per-NPC verb maps, state-dependent behaviour
- **Rule core:** an effect DSL (`SetValue`, `ModifyValue`, `MoveEntity`, `Conditional`, `RandomChoice`, `GameEnd`, …) plus triggers on events (`enter`, `leave`, `look`, `search`, `take`, `drop`, `use`, `state`, `command`, `turn`, `custom`) with `when` / `once` / `cooldown`
- Variables as a `VarMap` for counters, faction standing and supplies, plus predicates such as `standing`, `compare_var`, `has_item`, `has_flag`, `at: player, room: …`
- Quests with stages and rewards; conditions with tick and end effects
- Vehicles: player-steered, automatic routes and paid rides — fuel, interior rooms, stops — and **ships with systems and stations** (power/shields/hull/weapons, station verbs)
- Combat profiles `off` | `narrative` | `classic` (default), resolved by a pure combat module that also drives companions and the player's ship
- Deterministic RNG **in the save**, save/load (`saves/<slot>.json`), undo history, restart, tab completion
- Validation: `validateWorld` + `validateGameState` (used by the CLI and the tests)

**Worldbuilder (`worldbuilder/`)**

- Authoring schema for everything above, in YAML or JSON, with structured diagnostics
- CLI: `validate`, `compile`, `check`
- Optional gameplay modules (Phase 7): factions/standing, trade, encounter tables, survival/weather, stealth, party/companions, starships — see `docs/modules.md`

## Quick start

```bash
cabal build all

# play the bundled sample adventure
cabal run text-adventure

# author: check and compile an adventure
cabal run worldbuilder -- validate examples/thefog.yaml
cabal run worldbuilder -- compile examples/thefog.yaml -o /tmp/thefog

# play what you compiled
cabal run text-adventure -- --world /tmp/thefog/world.json --save /tmp/thefog/save.json
```

Engine flags: `--world FILE` (compiled GameWorld), `--save FILE` (initial
SaveState, must exist), `--allow-invalid`, `--help`. In-game saves are written
to `saves/<slot>.json`.

Whole pipeline — build, both test suites, validation of every shipped
adventure, 18 scripted playthroughs:

```bash
bash scripts/ci.sh
```

## In-game commands

- Movement: `go` / `move` / `walk <direction>`, or just the direction
- `look`, `look at` / `examine <target>`, `search`
- `take` / `get` / `grab <item>`, `take all`, `take <item> and <item>`, `drop <item>`, `drop all`
- `use <item>`, `use <item> on <target>`
- `talk to <npc>`, `choose <n>` (pick a dialogue option)
- `attack` / `hit <target>`
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
| `examples/` | `thefog.yaml` (reference game), `demo.yaml`, `genres/` (6 genre fixtures), `modules/` (7a–7h fixtures + `combo.yaml` composition proof), `fixtures/` (small feature fixtures) |
| `ci/e2e/` | scripted playthroughs: `<name>.in` plus the expected marker in `<name>.expect` |
| `docs/` | `adventure-schema.md`, `modules.md`, `genres.md`, `thefog-playthrough.md` |
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
  `Validate.hs`, `SaveLoad.hs`, `Verbs.hs`, `World.hs`, `Sample.hs` plus
  `worldbuilder/src/Worldbuilder/{Types,Compile,CLI,ParseFile}.hs`.

## Development

```bash
cabal build all
cabal test all --test-show-details=direct    # 192 engine tests, 64 worldbuilder tests
bash scripts/ci.sh                           # build + tests + validation + 18 E2E playthroughs
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
  `HsYAML-aeson`, `filepath` (worldbuilder); `JuicyPixels`, `vector` (img2ascii)

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
