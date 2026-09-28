# Protocol v1 Specification (Plan 1.4)

**Status:** 2026-09-28 · **Module:** `src/Types/Protocol.hs` (`Types.Protocol`, re-exported via `Types`) · **Version:** `1`

This document specifies the transport-agnostic JSON protocol between clients (CLI, TUI, WebUI, server, WASM host) and the text adventure engine. It covers message schemas, event serialization, state snapshots, versioning rules, and boundary decisions.

---

## 1. Overview & Architecture

Protocol v1 defines a bidirectional, versioned message protocol:

- **Client Messages (`ClientMsg`):** High-level actions sent by the client (`command`, `choose`, `continue`, `load_world`, `save`, `load`).
- **Server Messages (`ServerMsg`):** Responses emitted by the engine (`events`, `snapshot`, `error`).
- **Transport Agnostic:** The protocol carries no transport-layer semantics. It can run over WebSockets, HTTP POST/SSE, Unix domain sockets, or direct WASM memory buffers.
- **Deterministic Encoding:** All serialized JSON objects have keys sorted lexicographically at all nesting depths via `encodeSorted` (`aeson-pretty` with `confCompare = compare`). This guarantees byte-identical reproducibility across runs, platforms, and architectures.

Every message in both directions **must** include an integer `version` field (currently `1`).

---

## 2. Protocol Versioning & Error Handling

### 2.1 Version Field

Both `ClientMsg` and `ServerMsg` include top-level:
```json
"version": 1
```

### 2.2 Version Mismatch Behavior

If a client sends a message where:
1. `version` is not equal to `1` (e.g. `0`, `2`):
   - The engine rejects the message and responds with a `ServerError` carrying code `"version_mismatch"`.
2. `version` is missing entirely:
   - The engine rejects the message and responds with a `ServerError` carrying code `"malformed_payload"`.

Similarly, client decoders validate `version == 1` upon receiving server messages and reject mismatched payloads.

### 2.3 Error Codes (`ProtocolErrorCode`)

| Code | Meaning | Emitted by the v1 codec? |
|---|---|---|
| `"version_mismatch"` | Message carries an unsupported protocol version. | yes |
| `"unknown_type"` | The `"type"` discriminator is not recognized. | yes — an unrecognized discriminator is reported as this code, not as a malformed payload. |
| `"malformed_payload"` | The JSON structure does not match the schema for the declared type. | yes |
| `"session_error"` | Action rejected due to session state or game policy (e.g. save blocked in Ironman mode). | **no — reserved.** No session layer is wired to the codec yet (the engine has no transport; Phase 3 connects one). |

---

## 3. Client Messages (`ClientMsg`)

A client message is a JSON object with `"version": 1` and a `"type"` discriminator.

### 3.1 `command`
Executes a player text command through the engine parser (`look`, `take torch`, `go north`, etc.).

```json
{
    "version": 1,
    "type": "command",
    "command": "look"
}
```

- `command` (`String`, required): Raw user command string.

### 3.2 `choose`
Selects an active dialogue option or interactive choice by its 1-based index.

```json
{
    "version": 1,
    "type": "choose",
    "choice": 2
}
```

- `choice` (`Int`, required): 1-based index of the selected choice.

### 3.3 `continue`
Acknowledges a narrative continuation pause (Enter key press during cutscenes or multi-part narrative transitions).

```json
{
    "version": 1,
    "type": "continue"
}
```

### 3.4 `load_world`
Requests loading a compiled world or adventure fixture from the specified file path.

```json
{
    "version": 1,
    "type": "load_world",
    "world_path": "examples/thefog.yaml"
}
```

- `world_path` (`String`, required): Path to the adventure file.

### 3.5 `save`
Requests saving the active game to a named slot.

```json
{
    "version": 1,
    "type": "save",
    "slot": "slot1"
}
```

- `slot` (`String`, required): Save slot identifier.

### 3.6 `load`
Requests loading a saved game from a named slot.

```json
{
    "version": 1,
    "type": "load",
    "slot": "slot1"
}
```

- `slot` (`String`, required): Save slot identifier.

---

## 4. Server Messages (`ServerMsg`)

Server messages carry `"version": 1` and a `"type"` discriminator (`"events"`, `"snapshot"`, `"error"`).

### 4.1 `events`
Transports structured output events emitted during command execution 1:1.

```json
{
    "version": 1,
    "type": "events",
    "events": [
        {
            "payload": {
                "args": [
                    {
                        "key": "room",
                        "val": "Entrance Hall"
                    }
                ],
                "key": "game.look",
                "text": "You are in an entrance hall."
            },
            "type": "message"
        },
        {
            "type": "quest_update"
        }
    ]
}
```

### 4.2 `snapshot`
Provides a complete, self-contained state snapshot for rendering player HUD, minimap, quests, active dialogue, combat, and game over screens.

```json
{
    "version": 1,
    "type": "snapshot",
    "snapshot": { ... }
}
```

### 4.3 `error`
Reports protocol or session errors to the client.

```json
{
    "version": 1,
    "type": "error",
    "error": {
        "code": "version_mismatch",
        "message": "Unsupported protocol version 2, expected 1"
    }
}
```

---

## 5. OutputEvent Catalog

The engine produces output as an ordered stream of `OutputEvent` values. The protocol serializes all 12 events as follows:

| Event Type (`type`) | Description | Fields |
|---|---|---|
| `"message"` | Catalog message with stable key, arguments, and rendered text. | `payload`: `{ key: String?, args: [{key, val}], text: String }` |
| `"text"` | Prose text with optional styling spans (ANSI-free). | `styled`: `{ text: String, spans: [{start, length, style}] }` |
| `"art"` | Monospace ASCII art with raw text and structured hotspots. | `payload`: `{ raw: String, hotspots: [{index, glyph, target}] }` |
| `"anim"` | Multi-frame ASCII animation. | `delay_micros`: `Int`, `frames`: `[String]` |
| `"sfx"` | Sound effect playback trigger. | `path`: `FilePath` |
| `"music_start"` | Background music start or change. | `path`: `FilePath` |
| `"music_stop"` | Background music stop trigger. | *(no extra fields)* |
| `"room_changed"` | Player moved to a new room (additive state event). | `room_id`: `String` |
| `"quest_update"` | Quests status changed (additive state event). | *(no extra fields)* |
| `"dialogue"` | Dialogue engaged with NPC (choices in snapshot). | *(no extra fields)* |
| `"combat"` | Combat engagement flag changed. | `engaged`: `Bool` |
| `"game_over"` | Game reached death, victory, or custom ending. | *(no extra fields)* |

### 5.1 Styling Span Model

Prose text is ANSI-free. Colour and formatting are declared via `spans`:

```json
{
    "type": "text",
    "styled": {
        "text": "A cold draft blows from the north.",
        "spans": [
            {
                "start": 2,
                "length": 10,
                "style": {
                    "bold": true,
                    "color": "blue",
                    "dim": false,
                    "italic": false,
                    "underline": false
                }
            }
        ]
    }
}
```

Colours: `"default"`, `"black"`, `"red"`, `"green"`, `"yellow"`, `"blue"`, `"magenta"`, `"cyan"`, `"white"`.

---

## 6. Snapshot Data Structure

The `Snapshot` record is constructed purely from `GameState` via `makeSnapshot :: GameState -> Snapshot`.

```json
{
    "turn": 4,
    "player": {
        "health": 100,
        "max_health": 100,
        "attack": 12,
        "defense": 6,
        "gold": 25,
        "conditions": [
            {
                "name": "Poison",
                "remaining_turns": 3
            }
        ],
        "equipment": [
            {
                "slot": "Body",
                "item": "leather_armor"
            }
        ],
        "inventory": [
            {
                "id": "torch",
                "name": "torch",
                "description": "A wooden torch soaked in pitch."
            }
        ]
    },
    "room": {
        "id": "start",
        "name": "Cave Entrance",
        "description": "A dark cave entrance with jagged rocks.",
        "exits": [
            {
                "direction": "North",
                "target": "hallway",
                "locked": false
            }
        ],
        "items": [
            {
                "id": "rock",
                "name": "heavy rock",
                "description": "A plain stone."
            }
        ],
        "npcs": [
            {
                "id": "guide",
                "name": "Old Guide",
                "alive": true
            }
        ],
        "vehicle": null
    },
    "quests": {
        "active": [
            {
                "id": "escape",
                "name": "Find a Way Out",
                "stage_index": 0,
                "stage_description": "Search the cave for an exit."
            }
        ],
        "completed": []
    },
    "dialogue": null,
    "combat": {
        "engaged": false
    },
    "game_over": null,
    "visited_rooms": ["start"]
}
```

When dialogue is active, `"dialogue"` carries:
```json
{
    "npc_id": "guide",
    "npc_name": "Old Guide",
    "node_id": "greeting",
    "text": "Greetings, traveler! What brings you here?",
    "choices": [
        {
            "index": 1,
            "text": "I am looking for shelter.",
            "next_node": "shelter"
        },
        {
            "index": 2,
            "text": "Farewell.",
            "next_node": null
        }
    ]
}
```

When game over is triggered, `"game_over"` carries:
```json
{
    "reason": "victory",
    "menu": ["r", "q"]
}
```

---

## 7. Protocol Boundary Decision: Session Transitions (Constraint 6)

In Phase 1.3, pure session transitions (`transitionSave`, `transitionLoad`, `transitionRestart`, `transitionGameOver`, `transitionDeathInput`, `transitionVictoryInput`, `advanceNarrative`) return `[String]` within the engine loop.

To ensure that the protocol wire format remains uniformly `[OutputEvent]`:

1. **Protocol Boundary Adaptation:** The function `sessionLinesToEvents :: [String] -> [OutputEvent]` wraps each prose string into an `EvText (styledText line)` event.
2. **Unified Wire Representation:** When `ServerEvents` messages are transmitted over the wire, all session feedback is formatted as standard `[OutputEvent]` items.
3. **Internal Contract Preservation:** Internal engine functions continue returning `[String]` for CLI/TUI backwards-compatibility and unit test stability without breaking existing test assertions.
