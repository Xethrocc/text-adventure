# Changelog

## [0.2.0.0] — Unreleased

### Added
- Richer `ActionOutcome` constructors: `GiveItem`, `MoveItem`, `ConsumeItem`, `MoveNPC`, `SetRoomVisited`, `SetFlag`, `CheckFlag`, `RandomChoice`, `GameEnd`
- General-purpose flag system in `SaveState` for data-driven conditionals
- Deterministic hash-based pseudo-random outcomes (no external RNG dependency)
- `GameOverReason` (Victory / Death / Custom) with death→load/restart flow
- Named save slots with timestamps and world checksum validation
- `take all` / `drop all` commands
- Compound commands via `and`-splitting (`take torch and key`)
- Stop-word stripping (`the`, `a`, `an`) for more natural input
- `saves` command to list saved games
- `restart` command
- GitHub Actions CI workflow with coverage reporting

### Fixed
- Combat now correctly uses `npcDefenseBase` for damage reduction (was using `npcAttackBase`)
- NPC health clamped to `npcMaxHealth` when healed
- `use <weapon> on <npc>` now routes to attack instead of "Nothing happens"

### Changed
- Cabal file restructured with a `library` stanza to avoid double-compilation
- `SaveState` now includes `turnCount`, `flags`, and `gameOverReason` fields

## [0.1.0.0] — 2026

### Added
- Initial release
- Flexible game engine with rooms, items, NPCs
- GameWorld / SaveState architectural separation
- Dynamic verb system with `(Verb, State) → ActionOutcome` maps
- Command parser with synonym support and multi-word targets
- JSON save/load (SaveState only)
- Tab completion via Haskeline
- Sample adventure included
