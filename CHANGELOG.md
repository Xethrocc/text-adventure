# Changelog

## [0.8.1.0] — Unreleased

Phase 4.6 (Core Polish): Dialogue-Tree-Interaktivität, CLI-Validierung und Room-ASCII-Art.

### Added
- **Interaktives Dialogue-Tree-System**:
  - `ChooseCmd Int` im Command-Parser: unterstützt `choose <n>`, `pick <n>`, `option <n>`, `select <n>` und bare Zahlen (`1`, `2`, ...).
  - `SaveState.activeDialogue :: Maybe NPCID`: Trackt den aktiven Gesprächspartner zur Laufzeit (rückwärtskompatibel, default `Nothing`).
  - `DialogueChoice`: Neues Feld `dcNextNode :: Maybe String` für nahtlose Navigation im Dialogbaum (`Nothing` beendet das Gespräch).
  - Automatisches Rendern des nächsten Dialogknotens nach Ausführen des Choice-Outcomes.
  - `setDialogueNode` erzeugt initialen State, falls der NPC noch nicht in `npcStates` existiert.
  - Verlassen des Dialogs bei Bewegung oder Verlassen des Gesprächs.
  - Help-Text und Tab-Completion (`choose`, `option`) aktualisiert.
  - Sample-Adventure: `oldman` hat jetzt einen vollwertigen verzweigten Dialogbaum mit Quest-Hint und Flag-Setzung.
- **World-Validierung in CLI (`app/Main.hs`)**:
  - `validateWorld` wird bei `--world` vor dem Spielstart ausgeführt und listet gefundene Konsistenzfehler als Warnung auf.
- **Dialogue-Tree-Validierung in `Validate.hs`**:
  - Neue Fehler: `MissingDialogueNode` (fehlender Einstiegsknoten) und `DanglingDialogueChoice` (ungültiger Folgeknoten).
  - `allOutcomes` erfasst jetzt auch alle ActionOutcomes aus Dialogue-Choices.
- **ASCII-Art Unterstützung in Räumen**:
  - `Room.roomAscii :: Maybe String`: Optionales Banner-Feld (z. B. aus `img2ascii`), das bei `look` über dem Raumnamen angezeigt wird.
- 8 neue Tests für Dialoge, ASCII-Art und Dialogue-Validierung (jetzt 95 Tests, alle grün).

## [0.9.0.0] — Unreleased

Phase 5 (Worldbuilder + Ports): Worldbuilder YAML/JSON-Compiler und TheFog-Portierung.

### Added
- **Worldbuilder-Package (`worldbuilder/`)**:
  - `Worldbuilder.Types`: Authoring-Schema (Adventure, ARoom, AItem, ANPC, AQuest, AVehicle, ADialogueTree, AActionOutcome) mit FromJSON-Instanzen für JSON und YAML.
  - `Worldbuilder.Compile`: Schema → engine GameWorld + SaveState (Räume, Exits, Items, NPCs/Dialoge, Quests, Vehicles, Interaktionen, Outcomes).
  - `Worldbuilder.CLI`: CLI-Befehle `validate`, `compile`, `check` mit JSON/YAML-Unterstützung.
  - `Worldbuilder.ParseFile`: Auto-Erkennung von .json/.yaml/.yml, YAML-Parsing via HsYAML-aeson.
- **Engine-Erweiterungen für TheFog-Kompatibilität**:
  - `Southeast` in `Direction` (für Geheimgänge, Kanonensprünge, Schrein-Eingänge).
  - `activate` als Synonym für `VUse` (Schrein-Aktivierung per `activate <target>`).
  - `swim`, `crawl`, `dig`, `game`, `se` als Aliase für `Go Southeast`.
- **Beispiel: TheFog-Portierung** (`examples/thefog.yaml`):
  - 55 Räume mit komplettem Wegenez (Home, Garden, Forest, Graveyard, Mountain, Canyon, Castle, 4 Schrein-Locations).
  - 10 Items (Paper, Map, Apple, Shield, Crystal, Sword, 4 Schreine) mit Platzierungen aus dem Original.
  - 2 NPCs: Wolf (Kampf, 30 HP) und Princess.
  - 5 Quests (4 Schrein-Aktivierungen + Wolf besiegen).
  - 4 Schrein-Interaktionen: `use crystal on <shrine>` aktiviert den Schrein und schaltet die Quest weiter.
  - Wird via `worldbuilder compile examples/thefog.yaml` in spielbare Engine-Dateien übersetzt.

## [0.8.0.0] — Unreleased

Phase 4.5 (Engine-Qualität): World-Validierung.

### Added
- **Neues Modul `Validate`**: `ValidationError`-ADT (MissingRoom, MissingItem,
  MissingNPC, MissingQuest, MissingVehicle, DanglingExit, UnreachableRoom,
  DuplicateID, MissingSetFlag) und `validateWorld :: GameWorld -> [ValidationError]`.
- Prüfungen:
  - Exit-Ziele existieren
  - Alle Räume sind (abseits von Vehicle-Räumen) via BFS über offene + verschlossene
    Exits erreichbar
  - Items/NPCs/Quests/Vehicles aus ActionOutcome-Bäumen sind in den Definitionen
    vorhanden (typ-spezifische Collector-Funktionen vermeiden False Positives)
  - Doppelte IDs zwischen Kategorien (Vehicle-Item-Paarungen erlaubt)
  - Flags aus CheckFlag, die nie per SetFlag gesetzt werden
- Cabal: `Validate` in `exposed-modules` aufgenommen.
- 4 neue Tests (Sample-Welt ist gültig, hängender Exit, Duplikat, unerreichbarer Raum).

## [0.7.0.0] — Unreleased

Phase 4.4 (Engine-Qualität): Narrative Inserts.

### Added
- **Narrative Outcome**: `Narrative [String] ActionOutcome` — eine Liste von
  Zeilen, die nacheinander mit `[Press Enter to continue]` angezeigt werden,
  gefolgt von einem Folge-Outcome. Der reine Pfad gibt alle Zeilen auf einmal
  zurück und speichert das Follow-Up in `pendingNarrative` (kein Seiteneffekt
  bis zur interaktiven Anzeige).
- `GameState.pendingNarrative :: Maybe ([String], ActionOutcome)` — nicht
  serialisiert, nur zur Laufzeit. Die GameLoop rendert es zeilenweise mit
  `getLine`-Pause, wendet dann das Follow-Up an.
- Sample-Adventure: Das `meadow` führt jetzt eine kleine Narrative beim
  Betreten aus.
- 3 neue Tests (Lines-Rückgabe, Pending-Flag, JSON-Roundtrip).

## [0.6.0.0] — Unreleased

Phase 4.3 (Engine-Qualität): Undo.

### Added
- **Undo-System**: `LoopState` hält den aktuellen `GameState` und bis zu 50
  vorherige Zustände (neuester zuerst). `undo` stellt den kompletten Zustand
  inklusive Room, Inventory, Conditions, Quests, Vehicles und Turn-Counter
  wieder her.
- `undo` verbraucht selbst keinen Zug; bei leerer Historie erscheint
  `Nothing to undo.`.
- Save/Load/ListSaves/Restart/Help/Quit erzeugen keine Undo-Einträge; ein
  erfolgreicher Load startet bewusst eine neue History.
- Nach einem tödlichen Zug bietet der Death-Screen `[U]ndo` an, sodass der
  letzte Zustand direkt wiederhergestellt werden kann.
- Help-Text und Tab-Completion enthalten `undo`.
- 7 neue Undo-Tests: Restore, leere History, mehrfaches Undo, 50er-Limit,
  Meta-Commands und Wiederherstellung nach Tod.

## [0.5.0.0] — Unreleased

Phase 3 (Vehicles): First-Class-Fahrzeuge mit eigenen Innenräumen, Routen
und vehicle-weiten Conditions.

### Added
- **Vehicle-System**: `VehicleDef`/`VehicleState`, drei Typen
  (`PlayerControlled`, `AutomaticRoute`, `PaidVehicle`). Innenräume sind
  reguläre Rooms im World-Map (Hooks/Tags/Lighting funktionieren dort);
  `vehicleRooms` listet sie. Stops verbinden Außenräume mit Labels und
  optionalen Kosten (`stopCost` für PaidVehicles).
- **SaveState**: `vehicleStates`, `currentVehicle` (beide mit Defaults,
  alte Saves bleiben ladbar). **GameWorld**: `vehicleDefs`.
- **Commands**: `enter`/`board <vehicle>`, `exit`/`disembark`,
  `drive to <station>` (nur vom Cockpit, PlayerControlled),
  `wait` (AutomaticRoute: nächste Station), `refuel [vehicle]` (Status),
  `repair <condition>` (cleart Vehicle-Condition).
- **Tanken**: `use <fuel-item> on <vehicle>` — das Item-Prop `fuel` (default 1)
  wird gutgeschrieben, Item wird verbraucht; Cap via `vehicleFuelProp`.
- **Vehicle-weite Conditions**: `vsActiveConditions` feuern ihre
  `vehicleConditionEffects`-Outcomes einmal pro Zug (game loop), solange man
  an Bord ist. `vsRoomOverrides` ersetzen Raum-Beschreibungen in `look`;
  `look` zeigt zusätzlich Fuel/Conditions-Status.
- Sample-Adventure: Pferdekutsche (PlayerControlled) mit Cabin, Cockpit,
  zwei Stationen (`start`, `meadow`) und Heu als Treibstoff.
- 9 neue Tests (Enter/Exit/Drive/Refuel/Condition-Tick/JSON-Roundtrip).

## [0.4.0.0] — Unreleased

Phase 2 (Spiel-Systeme): Skills, Conditions, Quests.

### Added
- **Skill system**: `Player.playerSkills` and `CheckSkill` (skill + d6 vs DC,
  salt-threaded RNG so rolls differ), `ModifySkill`. Stats shows skills.
- **Conditions (status effects)**: `Condition` (name, remaining turns, tick and
  end outcomes), `SaveState.conditions`, outcomes `ApplyCondition`,
  `ClearCondition`, `HasCondition`. Ticked once per command in the game loop;
  tick messages are printed before the command's own output.
- **Quests**: `Quest`/`QuestStage` definitions in `GameWorld.questDefs`,
  `SaveState.activeQuests`/`completedQuests`, outcomes `StartQuest` (gated by
  `questPrereqs`), `AdvanceQuest`, `CompleteQuest` (fires the quest reward).
  `journal`/`quests` command shows active and completed quests.
- Sample adventure now includes a 3-stage quest (`find_treasure`, started by
  taking the key) and a prereq-gated quest (`gated_quest`) plus a lockpick skill.

### Fixed
- `CompleteQuest` swallowed the quest reward message; the reward outcome's
  message is now shown after the completion message.

## [0.3.0.0] — Unreleased

Phase 0 (Fundament) + Phase 1 (Authoring-Essentials).

### Added
- **Equipment system**: `EquipSlot` (Head/Body/Hands/Feet/Weapon/Offhand/Accessory),
  `EquipEffect` (AttackBonus/DefenseBonus/MaxHealthBonus), `SaveState.equipment`.
  Effective stats are computed on the fly via `effectiveAttack`,
  `effectiveDefense`, `effectiveMaxHealth`.
- **New commands**: `equip`/`wear`/`wield`, `unequip`/`remove`, `unequip all`,
  `stats`, `search` and `search <target>`.
- **New `ActionOutcome`s**: `EquipItem`, `UnequipItem`.
- **Room hooks**: `roomOnEnter`, `roomOnLook`, `roomOnExit`, `roomSearchOutcome`.
- **Room tags & lighting**: `roomTags`, `roomLightFlag`. A room tagged `dark`
  hides its contents unless the player carries a `lightsource` item or the
  room's `lightFlag` is set to `"true"`.
- **Alternative room descriptions**: `roomAltDescriptions` (flag → description).
- **Hidden items**: `itemHidden` / `itemDiscoverText` / `itemDiscovered`,
  revealed by `search`.
- **Dialogue trees**: `DialogueNode`, `DialogueChoice`, `DialogueTree` and
  `NPCDef.npcDialogueTrees`. `npcDialogue` remains as a fallback.
- **Item-on-item interactions**: `GameWorld.itemInteractions` (crafting).
- **World loading**: `World.loadGameWorld`, `loadSaveState`, `loadGame` plus
  CLI flags `--world FILE` / `--save FILE` / `--help`.
- **ID type aliases**: `NPCID`, `EntityID`, `VehicleID`, `QuestID`, `SkillID`,
  `FlagID`, `FactionID`.
- New module `Sample` holding the bundled demo adventure.
- New package `worldbuilder` (placeholder) wired up via `cabal.project`.

### Changed
- **`roomVisited` moved from `Room` to `SaveState.visitedRooms`** (breaking).
  `Room` is static world data and must not be mutated at runtime.
- `ItemDef` gained `itemTags`, `itemEquipSlot`, `itemEquipEffects`,
  `itemHidden`, `itemDiscoverText`.
- `ItemState` gained `itemDiscovered`.
- `NPCDef` gained `npcDialogueTrees`; `NPCState` gained `npcDialogueNode`.
- `Room` lost `roomVisited`; gained `roomTags`, `roomAltDescriptions`,
  `roomLightFlag`, `roomOnEnter`, `roomOnLook`, `roomOnExit`, `roomSearchOutcome`.
- `SaveState` gained `visitedRooms` and `equipment`.
- `applyOutcome` now threads an RNG salt and a recursion depth
  (`maxOutcomeDepth = 20`) to prevent runaway recursion from malformed content.
- `GameLoop.hs` split: save/load moved to `SaveLoad.hs`.
- `initSampleGame` moved out of `GameLoop` into `Sample`.
- Save schema version bumped `1 → 2`.

### Fixed
- **`RandomChoice` salt bug**: every random draw within the same turn used
  `salt = 0`, so consecutive `RandomChoice`s always produced the same result.
  The salt is now threaded through `MultipleOutcomes` and incremented.
- `use <item> on <target>` no longer swallows the "can't reach" message when an
  item-on-item interaction is undefined.
- `search` with no target parsed to `Unknown`; it is now its own `SearchCmd`.
- Combat uses `effectiveAttack` / `effectiveDefense` (equipment-aware).

## [0.2.0.0] — Unreleased

### Added
- Richer `ActionOutcome` constructors: `GiveItem`, `MoveItem`, `ConsumeItem`,
  `MoveNPC`, `SetRoomVisited`, `SetFlag`, `CheckFlag`, `RandomChoice`, `GameEnd`
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
- Combat now correctly uses `npcDefenseBase` for damage reduction (was using
  `npcAttackBase`)
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
