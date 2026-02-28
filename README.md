# Text Adventure Game

A simple text-based adventure game engine written in Haskell.

## Features

- **Flexible Game Engine**: Create rooms, items, NPCs, and player interactions
- **Data-Driven Architecture**: Static definitions (ItemDef/NPCDef) separated from dynamic state (ItemState/NPCState)
- **Dynamic Verb System**: Verbs as an ADT with synonym parsing and per-item/NPC verb maps
- **Command Parser**: Natural language command processing with synonyms
- **Save & Load**: Persist and restore game progress to/from pretty-printed JSON files
- **Extensible Architecture**: Easy to add new commands and game logic
- **Sample Adventure**: Complete working example included
- **Haskell Best Practices**: Clean, modular code with proper type safety

## Installation

### From Source

1. Clone the repository:

   ```bash
   git clone https://github.com/zeroclaw/text-adventure.git
   cd text-adventure
   ```

2. Build with Cabal:

   ```bash
   cabal update
   cabal build
   ```

3. Run the game:

   ```bash
   cabal run
   ```

### Using Stack

1. Build with Stack:

   ```bash
   stack build
   ```

2. Run the game:

   ```bash
   stack exec text-adventure
   ```

## Usage

### Playing the Game

Once running, use these commands:

- `go north/south/east/west/up/down` - Move between rooms
- `look` - Examine current location
- `look at <item>` - Examine specific items
- `take <item>` - Pick up items
- `inventory` - Check what you're carrying
- `use <item>` - Use items from inventory
- `talk to <npc>` - Interact with characters
- `attack <npc>` - Attack an enemy in the room
- `save [name]` - Save game progress (default: `savegame.json`)
- `load [name]` - Load a saved game (default: `savegame.json`)
- `help` - Show available commands
- `quit` - Exit the game

### Game Commands

The parser supports multiple synonyms:

- Movement: `go`, `move`, `walk` + direction
- Taking items: `take`, `pick up`, `grab`, `get`
- Combat: `attack`, `hit`, `kill`
- Looking: `look`, `examine`, `inspect`
- Inventory: `inventory`, `inv`, `i`
- Saving: `save`, `save <name>`
- Loading: `load`, `load <name>`
- Quitting: `quit`, `exit`, `q`

## Architecture

### Core Data Types

- **GameState**: Complete game state including rooms, player, entity states, and inventory
- **Room**: Individual locations with descriptions and connections (no longer holds dynamic items/NPCs directly)
- **Player**: The player's current health and combat stats
- **NPCDef** / **NPCState**: Static definitions and dynamic states for characters
- **ItemDef** / **ItemState**: Static definitions and dynamic states for items (supports dynamic verbs and actions)
- **Verb** / **ActionOutcome**: Dynamic actions and their results
- **Command**: Parsed player actions

### Main Modules

- `Main.hs`: Entry point and game initialization
- `Game.hs`: Core game logic and state management
- `GameLoop.hs`: Main game loop and user interaction
- `Parser.hs`: Command parsing and execution
- `Types.hs`: Data type definitions

## Creating Your Own Adventure

### Adding Rooms

To create custom adventures, modify the `initSampleGame` function in `GameLoop.hs`:

```haskell
initMyGame :: GameState
initMyGame = GameState
    { rooms = Map.fromList
        [ ("start", Room "start" "Starting Room" "Your starting location description"
            (Map.fromList [(North, Open "room2")]) True)
        , ("room2", Room "room2" "Second Room" "Another room description"
            (Map.fromList [(South, Open "start")]) False)
        ]
    , player = Player 100 100 10 5
    , currentRoom = "start"
    , inventory = []
    , itemStates = Map.fromList 
        [ ("item_1", ItemState "start" "intact") ]
    , itemDefs = Map.fromList
        [ ("item_1", ItemDef "item_1" "item_name" "Item description." ["keyword1", "keyword2"] Map.empty) ]
    , npcStates = Map.fromList
        [ ("goblin_1", NPCState "start" "alive" (Just 20)) ]
    , npcDefs = Map.fromList
        [ ("goblin_1", NPCDef "goblin_1" "goblin" "A small green goblin." (Map.singleton "alive" "Grrr!") ["goblin", "monster"] (Just 20) 5 1 Map.empty) ]
    , entityStates = Map.empty
    , entityInteractions = Map.empty
    , gameOver = False
    }
```

### Adding Custom Commands

Thanks to the dynamic verb system, adding custom commands is often as simple as updating `Types.hs` and `Parser.hs`:

1. Add your new verb to the `Verb` enum in `Types.hs`:

```haskell
data Verb = VGo | VLook | VTake | VDrop | VUse | VAttack | VCustom -- Added VCustom
```

1. Map a word to your verb in `parseVerb` in `Parser.hs`:

```haskell
parseVerb :: String -> Maybe Verb
parseVerb "custom" = Just VCustom
-- ...
```

Then you can assign this verb to an `ItemDef` or `NPCDef` via their `itemVerbMap` or `npcVerbMap` to trigger specific actions (like transitioning rooms or changing states) without writing custom `executeCommand` logic for every interaction!

## Development

### Building

```bash
cabal build
```

### Running Tests

```bash
cabal test
```

### Running Benchmarks

```bash
cabal bench
```

### Code Formatting

```bash
cabal-fmt --inplace *.hs src/*.hs
```

## Dependencies

- `base` (standard Haskell library)
- `containers` (for Map data structure)
- `aeson` (JSON serialization/deserialization)
- `aeson-pretty` (pretty-printed JSON output)
- `bytestring` (efficient byte-level I/O)
- `text` (Unicode text handling)

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass
6. Submit a pull request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Inspired by classic text adventure games
- Built with Haskell's type safety and functional programming principles
