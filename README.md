# Text Adventure Game

A simple text-based adventure game engine written in Haskell.

## Features

- **Flexible Game Engine**: Create rooms, items, NPCs, and player interactions
- **Command Parser**: Natural language command processing with synonyms
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
- `help` - Show available commands
- `quit` - Exit the game

### Game Commands

The parser supports multiple synonyms:

- Movement: `go`, `move`, `walk` + direction
- Taking items: `take`, `pick up`, `grab`, `get`
- Combat: `attack`, `hit`, `kill`
- Looking: `look`, `examine`, `inspect`
- Inventory: `inventory`, `inv`, `i`
- Quitting: `quit`, `exit`, `q`

## Architecture

### Core Data Types

- **GameState**: Complete game state including rooms, player, entity states, and inventory
- **Room**: Individual locations with descriptions, items, NPCs, and exits
- **Player**: The player's current health and combat stats
- **NPC**: Characters that can be talked to or fought
- **Item**: Objects that can be picked up, used, and specify allowed interactions
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
        [ ("start", Room
            { roomName = "Starting Room"
            , roomDescription = "Your starting location description"
            , roomItems = [Item "item_name" "Item description." ["keyword1", "keyword2"] ["take", "examine", "drop"] True]
            , roomNPCs = [NPC "goblin" "A small green goblin." "Grrr!" ["goblin", "monster"] (Just 20) 5 1]
            , roomConnections = Map.fromList [(North, Open "room2")]
            , roomVisited = True
            })
        , ("room2", Room
            { roomName = "Second Room"
            , roomDescription = "Another room description"
            , roomItems = []
            , roomNPCs = []
            , roomConnections = Map.fromList [(South, Open "start")]
            , roomVisited = False
            })
        ]
    , player = Player 100 100 10 5
    , currentRoom = "start"
    , inventory = []
    , entityStates = Map.empty
    , entityInteractions = Map.empty
    , gameOver = False
    }
```

### Adding Custom Commands

To add new commands, modify the `parseCommand` function in `Parser.hs`:

```haskell
parseCommand :: String -> Command
parseCommand input = case words (map toLower input) of
    ["custom", arg] -> CustomCommand arg
    _               -> Unknown input
```

Then implement the command in `executeCommand`:

```haskell
executeCommand (CustomCommand arg) state = (state, "Custom command executed with " ++ arg ++ "!")
```

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
