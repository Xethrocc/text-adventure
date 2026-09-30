# Engine-Message-Katalog (Phase 1.1)

**Stand:** 2026-09-28 · **Kataloggröße: 202 Keys** (`src/Messages.hs`, `catalogEntries`)

## Was diese Stufe leistet

Alle player-facing Meldungen der Engine sind aus den Modulen (`Parser`, `GameLoop`,
`Cards`, `Combat`, `Vehicles`, `Effects`, `Quests`, `Game`, `SaveLoad`, `World`,
`Frontend`) in einen zentralen Katalog mit stabilen, dotted Keys (`area.name`)
gezogen worden. Rendering läuft über `Messages.renderMsg` und damit über dieselbe
`{var}`-Interpolation (`formatStringWith`, aus `Game.hs` hierher verlagert), die
auch YAML-Texte nutzen. Sprache ist weiterhin Englisch als Default-Katalog;
Sprachpakete (`language: de`, `messages:`-Overrides) sind Phase 4.3 und legen
sich dann als Overlay auf genau diese Keys.

**Nachweis Byte-Identität:** Alle E2E-Eingaben aus `scripts/ci.sh` (41 Loop-Einträge
+ Worldgen) wurden vor dem Refactor als Golden-Capture gesichert und nach jedem
Teil-Commit erneut gespielt; `world.json`, `save.json` und die vollständige
Ausgabe sind über alle **42** Eingaben byte-identisch (`diff -r` leer).
Zusätzlich 363 Engine-Tests grün (0 Warnungen).

**Nachtrag (2026-09-28):** Vier Keys wurden nachträglich entfernt, weil sie keinen
Aufruf hatten — `map.dark`/`watch.dark` (im Dunkeln rendert `map`/`watch` die
autorenkonfigurierbare `darkRoomEv`-Meldung) sowie `map.legend_header`/`card.play.on`
(Dubletten bewusst geteilter Inline-Fragmente: `nl2 ++ evRaw "Legend:\n"` bzw.
`evRaw (" on " ++ target)`). `scripts/check-msg-catalog.sh` prüft das jetzt als
CI-Stufe in beide Richtungen: kein Katalog-Key ohne Aufrufstelle, keine
Aufrufstelle mit unbekanntem Key.

## Inventar-Methodik

1. Grep über alle Engine-Module nach String-Literalen mit großem Anfangsbuchstaben
   (das von der Review verwendete Rough-Count-Verfahren, ergab dort 102–130).
2. Manueller Call-Site-Sweep je Modul: *jede* Zeichenkette, die den Spieler
   erreicht — inklusive kleingeschriebener Fragmente (`"Undone."`, `"  (Empty)"`,
   `"compatible"`, Header-Zeilen), die der Rough-Grep nicht zählt.
3. Klassifikation (siehe unten) — nur Klasse **M/H** wurde katalogisiert.

### Ergebnis der Klassifikation

| Klasse | Bedeutung | Behandlung | Umfang (Appr.) |
|---|---|---|---|
| **M** | narrative Meldung an den Spieler | Katalog-Key | 182 Keys (inkl. H) |
| **H** | Header-/Label-/Menü-Zeile (Inventar:, === Journal ===, Menüs, Help) | Katalog-Key | in den 182 enthalten |
| **S** | Screen-/Art-Content (Kampfbildschirm, Kartenboxen, HP-Balken, `combatScreenDefaults`, deutsche HUD-Labels) | **bewusst nicht** — zieht mit 1.2 (EvArt/EvScreen, Styling-Modell) in strukturierte Payloads | ~20 Literale |
| **W** | Worldgen-Content-Templates (`Wildnis [{x}, {y}]`, Biome-Beschreibungen in `Game.hs`) | nicht Engine-UI, sondern generierter Weltinhalt | 2 Literale |
| **X** | interne Assertions (`error "Bad verb: "…`, JSON-Tags wie `"MetaFile"`, Env-Namen `TA_SAVES_DIR`, Typ-/Konstruktor-Namen in Show-Instanzen) | Programmierfehler-Indikatoren, keine Spieler-UI | ~80 Literale, überwiegend Types/*.hs |
| — | `Sample.hs` | in-code Adventure-Content (äquivalent zu YAML, nicht Engine-Text) | 54 Literale |

Die 182 Keys entstehen aus ~158 Rough-Grep-Literalen + ~30 kleingeschriebenen
Fragmenten; zusammengesetzte Meldungen wurden in Teil-Templates zerlegt
(z. B. `card.play` / `card.play.exhausted`), damit Sprachpakete
Satzstellung frei wählen können.

## Konventionen

- **Key-Format:** `bereich.name`, Kleinbuchstaben + `_`, keine Leerzeichen.
  Stabil = Key ändert sich nie; der *Text* kann sich ändern (auch per Override).
- **Args:** `[(String, String)]`; Template-Platzhalter `{arg}` mit denselben
  Modifikatoren wie bei YAML-Texten (`{n:+}`, `{n:6}`). Unbekannte Platzhalter
  bleiben als `{name}` stehen (einheitliches Verhalten mit `formatStringWith`).
- **Fehlender Key:** `renderMsg` rendert laut als `<msg:key>` — nie still
  ersetzten; die 363 Tests und die E2E-Goldens decken alle Pfade ab, in denen
  das auftauchen könnte.
- **Kein Zugriff auf Spielvariablen in Engine-Templates** (bewusst): `renderMsg`
  substituiert nur Args. Damit kann eine YAML-Variable namens `item` keine
  Engine-Meldung umlenken. Ob Engine-Texte später Variablen interpolieren dürfen,
  entscheidet 4.3 (Sprachpakete) — dann mit klarem Namensraum.

## Katalog (generiert aus `catalogEntries`, sortiert)

Sehr lange Templates (z. B. `help.text`) sind in der Tabelle bei 120 Zeichen
abgeschnitten mit `…`; maßgeblich ist `src/Messages.hs`.

<!-- BEGIN GENERATED (aus src/Messages.hs via scripts/gen-msg-catalog.py — nicht von Hand editieren) -->
| # | MsgId | Template |
|---|---|---|
| 1 | `attack.cant_here` | `You can't attack the {target} here.` |
| 2 | `attack.cant_target` | `You can't attack the {target}.` |
| 3 | `attack.none_here` | `There's nothing to fight here.` |
| 4 | `card.cost_insufficient` | `Not enough {res} (need {need}, have {have}).` |
| 5 | `card.count_suffix` | ` (x{count})` |
| 6 | `card.discard_pile_header` | `=== Discard Pile ({n} cards) ===` |
| 7 | `card.draw_pile_header` | `=== Draw Pile ({n}/{total} cards) ===` |
| 8 | `card.hand_empty` | `Your hand is empty.` |
| 9 | `card.invalid_number` | `Invalid card number {idx}. You have {n} card(s) in hand.` |
| 10 | `card.no_deck` | `You don't have a deck.` |
| 11 | `card.no_deck_endturn` | `You don't have a deck to end your turn.` |
| 12 | `card.no_deck_play` | `You don't have a deck to play cards from.` |
| 13 | `card.no_enemies` | `There are no living enemies here to target.` |
| 14 | `card.no_match` | `No living enemy matches '{target}'.` |
| 15 | `card.pile_empty` | `  (Empty)` |
| 16 | `card.pile_line` | `  - {name}` |
| 17 | `card.piles_footer` | `Hand: {hand} \| Discard: {discard} \| Exhaust: {exhaust}` |
| 18 | `card.play` | `You play {card}` |
| 19 | `card.play.exhausted` | ` (Exhausted).` |
| 20 | `card.specify_target` | `Please specify a target (e.g. 'play <n> <target>'). Available: {names}` |
| 21 | `card.turn_ended` | `Turn ended. Energy restored to {max}. Drew {n} cards.` |
| 22 | `card.unknown` | `Unknown card: '{id}'.` |
| 23 | `chapter.no_next` | `There is no next chapter.` |
| 24 | `chapter.refuse_back` | `This chapter is behind you.` |
| 25 | `chapter.unknown` | `There is no such chapter.` |
| 26 | `choice.invalid` | `Invalid choice. Please select a number from 1 to {max}.` |
| 27 | `combat.ability_cooldown` | `Ability is on cooldown.` |
| 28 | `combat.ability_unknown` | `Unknown ability '{id}'.` |
| 29 | `combat.ability_use` | `Round {round}: You use {ability}!` |
| 30 | `combat.ally_strike` | `{ally} strikes for {dmg}.` |
| 31 | `combat.attack_hit` | `Round {round}: You hit the {target} for {dmg}.` |
| 32 | `combat.attack_kill` | `Round {round}: You attack the {target} and kill it!` |
| 33 | `combat.attack_ship` | `Round {round}: You attack the {target}.` |
| 34 | `combat.attack_ship_destroy` | `Round {round}: You attack the {target} and destroy it!` |
| 35 | `combat.classic_attack` | `You attack the {target}.` |
| 36 | `combat.classic_destroy` | `You attack the {target} and destroy it!` |
| 37 | `combat.classic_exchange` | `You hit for {dmg}, it hits you for {npc_dmg}.` |
| 38 | `combat.classic_kill` | `You attack the {target} and kill it!` |
| 39 | `combat.defend` | `Round {round}: You brace yourself against the {target}.` |
| 40 | `combat.flee` | `Round {round}: You flee from the {target}!` |
| 41 | `combat.flee_denied` | `You can't flee from the {target}!` |
| 42 | `combat.lose` | `You lose the fight against the {target}. Your attack is turned aside.` |
| 43 | `combat.not_engaged` | `You are not in combat.` |
| 44 | `combat.not_enough_resources` | `Not enough resources.` |
| 45 | `combat.not_yet` | `You can't do that in combat yet.` |
| 46 | `combat.ship_destroyed` | `The {target} is already destroyed.` |
| 47 | `combat.strikes_back_kill` | `The {target} strikes back and kills you!` |
| 48 | `combat.win` | `You win the fight against the {target}! Your attack lands cleanly.` |
| 49 | `container.move_refused` | `You can't move an item into a container that way.` |
| 50 | `dark.default` | `It's pitch black. You can't see anything.` |
| 51 | `death.title` | `  YOU HAVE DIED` |
| 52 | `device.cant_flip` | `You cannot flip that.` |
| 53 | `device.cant_insert` | `You cannot insert that.` |
| 54 | `device.cant_remove` | `You cannot remove that.` |
| 55 | `device.examine_mounted` | `\nMounted: {item}.` |
| 56 | `device.flip` | `You flip the {device} to {state}.` |
| 57 | `device.insert` | `You insert the {item} into the {device}.` |
| 58 | `device.not_carried` | `You are not carrying {item}.` |
| 59 | `device.not_in_device` | `There is no {item} in the {device}.` |
| 60 | `device.occupied` | `There is already something in the {device}.` |
| 61 | `device.reject` | `The {item} does not fit into the {device}.` |
| 62 | `device.remove` | `You remove the {item} from the {device}.` |
| 63 | `dialogue.choice_line` | `  [{i}] {text}` |
| 64 | `dialogue.ended` | `Dialogue ended.` |
| 65 | `dialogue.line` | `{name}: \"{text}\"` |
| 66 | `dialogue.none_active` | `You are not in a conversation right now.` |
| 67 | `dialogue.nothing_more` | `{name} has nothing more to say.` |
| 68 | `dialogue.nothing_to_say` | `{name} has nothing to say.` |
| 69 | `dialogue.partner_gone` | `The person you were talking to is gone.` |
| 70 | `disambiguate.option` | `[{n}] {name}` |
| 71 | `disambiguate.prompt` | `Which do you mean: {names}?` |
| 72 | `drop.nothing` | `You're not carrying anything to drop.` |
| 73 | `drop.ok` | `You drop the {item}.` |
| 74 | `end.rule_line` | `=========================================` |
| 75 | `enter.not_seen` | `You don't see '{target}' here to enter.` |
| 76 | `equip.header` | `Equipment:\n` |
| 77 | `equip.line` | `  {slot}: {name}` |
| 78 | `equip.need_carried` | `You need to be carrying the {item}.` |
| 79 | `equip.not_equippable` | `You cannot equip the {item}.` |
| 80 | `equip.nothing` | `You have nothing equipped.` |
| 81 | `equip.ok` | `You equip the {item}.` |
| 82 | `equip.slot_occupied` | `You already have the {item} equipped there. Unequip it first.` |
| 83 | `fuel.status` | `{vehicle} fuel ({item}): {f}/{max}` |
| 84 | `fuel.status_zero` | `{vehicle} fuel ({item}): 0/{max}` |
| 85 | `game.restart_start` | `Starting a new game...\n` |
| 86 | `gameover.custom` | `Game Over: {msg}` |
| 87 | `help.text` | `=== Available Commands ===\n\nMovement:\n  go/move/walk <direction>   - Move north/south/east/west/up/down\n  <directio…` |
| 88 | `inv.empty` | `You're not carrying anything.` |
| 89 | `inv.header` | `Inventory: {items}` |
| 90 | `item.cant_do` | `You can't do that to the {item} right now.` |
| 91 | `item.no_id` | `There is no item '{id}'.` |
| 92 | `learn.default` | `Noted.` |
| 93 | `load.ironman_blocked` | `Loading is disabled in ironman mode.` |
| 94 | `load.permadeath` | `No load after death (permadeath).` |
| 95 | `load.prompt` | `Enter save name to load (or press Enter for 'savegame'):` |
| 96 | `look.corpse_many` | `\nBodies lie here: {names}.` |
| 97 | `look.corpse_one` | `\nThe body of {name} lies here.` |
| 98 | `look.fuel` | `\nFuel ({item}: {f}/{max})` |
| 99 | `look.fuel_out` | `\nOut of {item} (0/{max})` |
| 100 | `look.items` | `\nYou see: {names}.` |
| 101 | `look.npcs` | `\nAlso here: {names}.` |
| 102 | `look.see_nothing` | `\nYou see nothing of interest.` |
| 103 | `look.void` | `You're in a void. There's nothing here.` |
| 104 | `map.legend_line` | `  {i}: {label}` |
| 105 | `map.no_marks` | `There is nothing marked on the map.` |
| 106 | `map.void` | `You're in a void. There's nothing to map.` |
| 107 | `menu.death_full` | `  [U]ndo  \|  [L]oad last save  \|  [R]estart  \|  [Q]uit` |
| 108 | `menu.restart_quit` | `  [R]estart  \|  [Q]uit` |
| 109 | `meta.corrupted` | `Warning: meta file '{path}' is corrupted — starting fresh.` |
| 110 | `meta.unreadable` | `Warning: meta file '{path}' is unreadable — starting fresh.` |
| 111 | `move.blocked` | `You can't go that way.` |
| 112 | `move.door_locked` | `The door is locked.` |
| 113 | `move.no_exit` | `There's nothing in that direction.` |
| 114 | `move.ok` | `You move {dir}.` |
| 115 | `notes.empty` | `Your notes are empty.` |
| 116 | `notes.header` | `Your notes:` |
| 117 | `npc.already_dead` | `The {npc} is already dead.` |
| 118 | `npc.cant_do` | `You can't do that to {npc}.` |
| 119 | `npc.dead_silent` | `The {npc} is dead and says nothing.` |
| 120 | `parse.unknown` | `I don't understand '{input}'. Type 'help' for available commands.` |
| 121 | `proc.arity` | `That procedure cannot be called this way.` |
| 122 | `proc.unknown` | `That procedure does not exist.` |
| 123 | `quest.cannot_start` | `You cannot start that quest right now.` |
| 124 | `quest.not_active` | `That quest is not active.` |
| 125 | `quests.active_header` | `Active:` |
| 126 | `quests.completed_header` | `Completed:` |
| 127 | `quests.journal_empty` | `Your journal is empty.` |
| 128 | `quests.journal_header` | `=== Journal ===\n` |
| 129 | `quit.bye` | `Goodbye!` |
| 130 | `quit.thanks` | `Thanks for playing!` |
| 131 | `refuel.no_vehicle` | `There is no vehicle to refuel.` |
| 132 | `refuel.not_needed` | `The {vehicle} doesn't need fuel.` |
| 133 | `repair.nothing_broken` | `There is nothing broken about the {vehicle} that matches '{target}'.` |
| 134 | `repair.ok` | `You repair the {vehicle} ({problem}).` |
| 135 | `repair.problems` | ` Problems: {list}.` |
| 136 | `save.compatible` | `compatible` |
| 137 | `save.corrupted` | `Error: Save file is corrupted or incompatible.` |
| 138 | `save.entry` | `  {name} — {timestamp} ({compat})` |
| 139 | `save.list_header` | `=== Saved Games ===` |
| 140 | `save.loaded` | `Game loaded from {path} (saved: {timestamp}).` |
| 141 | `save.loaded_legacy` | `Game loaded (legacy format).` |
| 142 | `save.loaded_legacy2` | `Game loaded from legacy format.` |
| 143 | `save.none_found` | `No saved games found.` |
| 144 | `save.not_found` | `Error: Save file '{path}' not found.` |
| 145 | `save.saved` | `Game saved to {path} ({timestamp}).` |
| 146 | `save.savezone_only` | `You can only rest at a savezone.` |
| 147 | `save.slot_delete_failed` | `Warning: could not delete save slot {path}.` |
| 148 | `save.slot_deleted` | `Save slot deleted: {path}` |
| 149 | `save.unreadable` | `Error: Could not read file '{path}'.` |
| 150 | `save.version_warning` | `Warning: This save was made with a different world version. Results may be unpredictable.` |
| 151 | `save.world_mismatch` | `world mismatch!` |
| 152 | `search.nothing` | `You find nothing of interest.` |
| 153 | `search.nothing_item` | `You find nothing special about the {item}.` |
| 154 | `search.nothing_npc` | `You find nothing on {npc}.` |
| 155 | `search.reveal` | `You find the {item}.` |
| 156 | `search.void` | `You're in a void. There's nothing to search.` |
| 157 | `stats.attack` | `Attack:  {atk} (base {base})` |
| 158 | `stats.conditions` | `Conditions: {conds}\n` |
| 159 | `stats.defense` | `Defense: {def} (base {base})` |
| 160 | `stats.health` | `Health:  {hp} / {max}` |
| 161 | `stats.skills` | `Skills: {skills}\n` |
| 162 | `take.already` | `You already have the {item}.` |
| 163 | `take.none_here` | `There's nothing here to take.` |
| 164 | `take.not_portable` | `You can't take the {item}.` |
| 165 | `take.ok` | `You take the {item}.` |
| 166 | `target.not_carried` | `You don't have '{target}'.` |
| 167 | `target.not_seen` | `You don't see '{target}' here.` |
| 168 | `ui.press_enter` | `  [Press Enter to continue]` |
| 169 | `undo.disabled` | `Undo is disabled in this adventure.` |
| 170 | `undo.done` | `Undone.` |
| 171 | `undo.ironman` | `No undo in ironman mode.` |
| 172 | `undo.nothing` | `Nothing to undo.` |
| 173 | `undo.permadeath` | `No undo after death (permadeath).` |
| 174 | `unequip.all` | `You remove all equipment.` |
| 175 | `unequip.not_equipped` | `The {item} is not equipped.` |
| 176 | `unequip.ok` | `You unequip the {item}.` |
| 177 | `use.not_carried` | `You need to be carrying '{item}' to use it.` |
| 178 | `use.nothing` | `Nothing happens.` |
| 179 | `use.ok` | `You use the {item}. {msg}` |
| 180 | `use.unreachable` | `You can't reach '{entity}' from here.` |
| 181 | `vehicle.board` | `You board the {vehicle}.` |
| 182 | `vehicle.cant_drive` | `You can't drive there from here. Stations: {stations}` |
| 183 | `vehicle.cant_steer` | `You can't steer the {vehicle}; it follows its own route.` |
| 184 | `vehicle.disembark` | `You disembark from the {vehicle}.` |
| 185 | `vehicle.drive_to` | `You drive to {stop}.` |
| 186 | `vehicle.fuelled` | `The {vehicle} is fuelled ({f}/{max}).` |
| 187 | `vehicle.manual_only` | `This vehicle only moves when you drive it.` |
| 188 | `vehicle.need_controls` | `You need to be at the controls to drive.` |
| 189 | `vehicle.no_here_enter` | `There is no '{id}' here to enter.` |
| 190 | `vehicle.no_id` | `There is no '{id}'.` |
| 191 | `vehicle.not_here` | `The {vehicle} is not here.` |
| 192 | `vehicle.not_in` | `You are not in a vehicle.` |
| 193 | `vehicle.not_on` | `You are not on a vehicle.` |
| 194 | `vehicle.route_end` | `The route has no further stops.` |
| 195 | `vehicle.travel_on` | `You travel on to {stop}.` |
| 196 | `vehicle.warning` | `Warning: {list}!` |
| 197 | `victory.title` | `  VICTORY!` |
| 198 | `watch.nothing` | `There is nothing to watch about {label}.` |
| 199 | `watch.start` | `Watching {label}...` |
| 200 | `watch.void` | `You're in a void. There's nothing to watch.` |
| 201 | `world.file_unreadable` | `Could not read world file '{path}': {err}` |
| 202 | `world.save_unreadable` | `Could not read save file '{path}': {err}` |

<!-- END GENERATED -->