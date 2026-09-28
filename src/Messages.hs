-- | Engine message catalog (Phase 1.1).
--
--   Every player-facing engine message lives here as a stable, dotted key
--   (@area.name@) with an English template. The catalog is plain data so a
--   later language-pack layer (D4, Phase 4.3: @language: de@ plus
--   @messages:@ overrides) can replace templates without touching call
--   sites. Rendering reuses the engine's @{var}@ interpolation.
--
--   Leaf module on purpose: it must be importable from everywhere (Parser,
--   GameLoop, Cards, Combat, Vehicles, Effects, Quests, SaveLoad, World,
--   Frontend) without cycles — Game.hs only re-exports 'formatStringWith'
--   for the YAML-text path ('Game.formatWithVars').
module Messages
    ( MsgId
    , renderMsg
    , catalogEntries
    , defaultCatalog
    , formatStringWith
    , msgPayload
    , evMsg
    ) where

import Data.Char (isDigit)
import Data.List (intercalate, isPrefixOf)
import qualified Data.List as List (lookup)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Types.Output (MsgPayload (..), OutputEvent (..))

-- | Stable message key. Plain alias: the catalog is data, and Phase 4.3
--   overlays YAML-provided keys on the same namespace.
type MsgId = String

-- | Render a catalog message: substitute @{arg}@ placeholders from the
--   argument list (same syntax and modifiers as 'formatStringWith'). An
--   unknown key renders as @\<msg:key\>@ — loud on purpose, unit tests and
--   the E2E goldens must never see it.
renderMsg :: MsgId -> [(String, String)] -> String
renderMsg key args =
    case Map.lookup key defaultCatalog of
        Nothing  -> "<msg:" ++ key ++ ">"
        Just tmpl -> formatStringWith tmpl (\k -> List.lookup k args)

-- | A catalog message as a structured payload: key, args and the rendered
--   text (byte-identical to 'renderMsg'). Phase 1.2.
msgPayload :: MsgId -> [(String, String)] -> MsgPayload
msgPayload key args = MsgPayload (Just key) args (renderMsg key args)

-- | A catalog message as a one-element event fragment (the form command
--   handlers return). Phase 1.2.
evMsg :: MsgId -> [(String, String)] -> [OutputEvent]
evMsg key args = [EvMessage (msgPayload key args)]

-- | The catalog as an association list — the single source of truth.
--   'defaultCatalog' is derived from it; a unit test pins that the list
--   contains no duplicate keys (a 'Map.fromList' would silently drop them).
--
--   Templates reproduce the previously hardcoded strings byte for byte;
--   @\n@ inside a template is the same literal newline the old
--   concatenation produced. Generated companion: docs/message-catalog.md.
catalogEntries :: [(MsgId, String)]
catalogEntries =
    [ -- -- movement (Parser.hs) ------------------------------------------------
      ("move.ok",             "You move {dir}.")
    , ("move.door_locked",    "The door is locked.")
    , ("move.no_exit",        "There's nothing in that direction.")
    , ("move.blocked",        "You can't go that way.")
    -- -- look / map / watch ---------------------------------------------------
    , ("look.void",           "You're in a void. There's nothing here.")
    , ("look.see_nothing",    "\nYou see nothing of interest.")
    , ("look.items",          "\nYou see: {names}.")
    , ("look.npcs",           "\nAlso here: {names}.")
    , ("look.corpse_one",     "\nThe body of {name} lies here.")
    , ("look.corpse_many",    "\nBodies lie here: {names}.")
    , ("map.void",            "You're in a void. There's nothing to map.")
    , ("map.no_marks",        "There is nothing marked on the map.")
    , ("map.legend_line",     "  {i}: {label}")
    , ("watch.void",          "You're in a void. There's nothing to watch.")
    , ("watch.nothing",       "There is nothing to watch about {label}.")
    , ("watch.start",         "Watching {label}...")
    -- -- inventory / stats ---------------------------------------------------
    , ("inv.empty",           "You're not carrying anything.")
    , ("inv.header",          "Inventory: {items}")
    , ("stats.skills",        "Skills: {skills}\n")
    , ("stats.conditions",    "Conditions: {conds}\n")
    , ("stats.health",        "Health:  {hp} / {max}")
    , ("stats.attack",        "Attack:  {atk} (base {base})")
    , ("stats.defense",       "Defense: {def} (base {base})")
    -- -- undo / quit / parse ------------------------------------------------
    , ("undo.nothing",        "Nothing to undo.")
    , ("quit.bye",            "Goodbye!")
    , ("parse.unknown",       "I don't understand '{input}'. Type 'help' for available commands.")
    , ("help.text",           helpTemplate)
    -- -- dialogue ------------------------------------------------------------
    , ("dialogue.none_active",   "You are not in a conversation right now.")
    , ("dialogue.partner_gone",   "The person you were talking to is gone.")
    , ("dialogue.nothing_more",   "{name} has nothing more to say.")
    , ("dialogue.nothing_to_say", "{name} has nothing to say.")
    , ("dialogue.ended",          "Dialogue ended.")
    , ("dialogue.line",          "{name}: \"{text}\"")
    , ("dialogue.choice_line",   "  [{i}] {text}")
    , ("choice.invalid",          "Invalid choice. Please select a number from 1 to {max}.")
    -- -- equipment ----------------------------------------------------------
    , ("equip.ok",              "You equip the {item}.")
    , ("equip.nothing",         "You have nothing equipped.")
    , ("unequip.ok",            "You unequip the {item}.")
    , ("unequip.not_equipped",  "The {item} is not equipped.")
    , ("unequip.all",           "You remove all equipment.")
    -- -- take / drop / use --------------------------------------------------
    , ("take.none_here",      "There's nothing here to take.")
    , ("take.already",        "You already have the {item}.")
    , ("take.not_portable",   "You can't take the {item}.")
    , ("take.ok",             "You take the {item}.")
    , ("drop.ok",             "You drop the {item}.")
    , ("drop.nothing",        "You're not carrying anything to drop.")
    , ("use.not_carried",     "You need to be carrying '{item}' to use it.")
    , ("use.unreachable",     "You can't reach '{entity}' from here.")
    , ("use.ok",              "You use the {item}. {msg}")
    , ("use.nothing",         "Nothing happens.")
    -- -- search / reveal ----------------------------------------------------
    , ("search.void",          "You're in a void. There's nothing to search.")
    , ("search.nothing_item",  "You find nothing special about the {item}.")
    , ("search.nothing_npc",   "You find nothing on {npc}.")
    , ("search.reveal",        "You find the {item}.")
    , ("search.nothing",       "You find nothing of interest.")
    -- -- targets / interaction fallbacks ------------------------------------
    , ("target.not_carried",   "You don't have '{target}'.")
    , ("target.not_seen",      "You don't see '{target}' here.")
    , ("enter.not_seen",       "You don't see '{target}' here to enter.")
    , ("item.cant_do",         "You can't do that to the {item} right now.")
    , ("npc.already_dead",     "The {npc} is already dead.")
    , ("npc.dead_silent",      "The {npc} is dead and says nothing.")
    , ("npc.cant_do",          "You can't do that to {npc}.")
    , ("disambiguate.prompt",  "Which do you mean: {names}?")
    , ("disambiguate.option",  "[{n}] {name}")
    -- -- combat --------------------------------------------------------------
    , ("combat.not_engaged",  "You are not in combat.")
    , ("attack.cant_target",  "You can't attack the {target}.")
    , ("attack.none_here",    "There's nothing to fight here.")
    -- -- darkness (B3, Phase 0.3 default) ------------------------------------
    , ("dark.default",        "It's pitch black. You can't see anything.")
    -- -- vehicles (Parser half; Vehicles.hs reuses in its own commit) ---------
    , ("vehicle.not_in",      "You are not in a vehicle.")
    , ("refuel.no_vehicle",   "There is no vehicle to refuel.")
    , ("refuel.not_needed",   "The {vehicle} doesn't need fuel.")
    , ("fuel.status",         "{vehicle} fuel ({item}): {f}/{max}")
    , ("fuel.status_zero",    "{vehicle} fuel ({item}): 0/{max}")
    , ("repair.nothing_broken", "There is nothing broken about the {vehicle} that matches '{target}'.")
    , ("repair.problems",     " Problems: {list}.")
    , ("repair.ok",           "You repair the {vehicle} ({problem}).")
    -- -- game loop / menus (GameLoop.hs, Frontend.hs) ------------------------
    , ("undo.disabled",       "Undo is disabled in this adventure.")
    , ("undo.done",           "Undone.")
    , ("undo.permadeath",     "No undo after death (permadeath).")
    , ("undo.ironman",        "No undo in ironman mode.")
    , ("load.permadeath",     "No load after death (permadeath).")
    , ("load.ironman_blocked", "Loading is disabled in ironman mode.")
    , ("load.prompt",         "Enter save name to load (or press Enter for 'savegame'):")
    , ("save.savezone_only",  "You can only rest at a savezone.")
    , ("game.restart_start",  "Starting a new game...\n")
    , ("quit.thanks",         "Thanks for playing!")
    , ("ui.press_enter",      "  [Press Enter to continue]")
    , ("menu.restart_quit",   "  [R]estart  |  [Q]uit")
    , ("menu.death_full",     "  [U]ndo  |  [L]oad last save  |  [R]estart  |  [Q]uit")
    , ("end.rule_line",       "=========================================")
    , ("death.title",         "  YOU HAVE DIED")
    , ("victory.title",       "  VICTORY!")
    , ("gameover.custom",     "Game Over: {msg}")
    -- -- cards (Cards.hs; narrative lines — card boxes/screens stay authored
    -- -- art, they move to structured screen payloads in Phase 1.2) ----------
    , ("card.cost_insufficient", "Not enough {res} (need {need}, have {have}).")
    , ("card.no_enemies",      "There are no living enemies here to target.")
    , ("card.specify_target",  "Please specify a target (e.g. 'play <n> <target>'). Available: {names}")
    , ("card.no_match",        "No living enemy matches '{target}'.")
    , ("card.no_deck",         "You don't have a deck.")
    , ("card.no_deck_play",    "You don't have a deck to play cards from.")
    , ("card.no_deck_endturn", "You don't have a deck to end your turn.")
    , ("card.invalid_number",  "Invalid card number {idx}. You have {n} card(s) in hand.")
    , ("card.unknown",         "Unknown card: '{id}'.")
    , ("card.play",            "You play {card}")
    , ("card.play.exhausted",  " (Exhausted).")
    , ("card.turn_ended",      "Turn ended. Energy restored to {max}. Drew {n} cards.")
    , ("card.hand_empty",      "Your hand is empty.")
    , ("card.pile_line",       "  - {name}")
    , ("card.count_suffix",    " (x{count})")
    , ("card.pile_empty",      "  (Empty)")
    , ("card.draw_pile_header",    "=== Draw Pile ({n}/{total} cards) ===")
    , ("card.discard_pile_header", "=== Discard Pile ({n} cards) ===")
    , ("card.piles_footer",    "Hand: {hand} | Discard: {discard} | Exhaust: {exhaust}")
    -- -- combat (Combat.hs, narrative resolver messages; the classic combat
    -- -- screen stays authored art, Phase 1.2) -------------------------------
    , ("attack.cant_here",       "You can't attack the {target} here.")
    , ("combat.win",             "You win the fight against the {target}! Your attack lands cleanly.")
    , ("combat.lose",            "You lose the fight against the {target}. Your attack is turned aside.")
    , ("combat.defend",          "Round {round}: You brace yourself against the {target}.")
    , ("combat.flee",            "Round {round}: You flee from the {target}!")
    , ("combat.flee_denied",     "You can't flee from the {target}!")
    , ("combat.ability_unknown", "Unknown ability '{id}'.")
    , ("combat.ability_cooldown", "Ability is on cooldown.")
    , ("combat.not_enough_resources", "Not enough resources.")
    , ("combat.ability_use",     "Round {round}: You use {ability}!")
    , ("combat.attack_kill",     "Round {round}: You attack the {target} and kill it!")
    , ("combat.attack_hit",      "Round {round}: You hit the {target} for {dmg}.")
    , ("combat.ship_destroyed",  "The {target} is already destroyed.")
    , ("combat.attack_ship_destroy", "Round {round}: You attack the {target} and destroy it!")
    , ("combat.attack_ship",     "Round {round}: You attack the {target}.")
    , ("combat.not_yet",         "You can't do that in combat yet.")
    , ("combat.classic_kill",    "You attack the {target} and kill it!")
    , ("combat.classic_attack",  "You attack the {target}.")
    , ("combat.classic_destroy", "You attack the {target} and destroy it!")
    , ("combat.ally_strike",     "{ally} strikes for {dmg}.")
    , ("combat.strikes_back_kill", "The {target} strikes back and kills you!")
    , ("combat.classic_exchange", "You hit for {dmg}, it hits you for {npc_dmg}.")
    -- -- vehicles (Vehicles.hs) ----------------------------------------------
    , ("vehicle.no_here_enter",  "There is no '{id}' here to enter.")
    , ("vehicle.not_here",        "The {vehicle} is not here.")
    , ("vehicle.board",           "You board the {vehicle}.")
    , ("vehicle.disembark",       "You disembark from the {vehicle}.")
    , ("vehicle.no_id",           "There is no '{id}'.")
    , ("vehicle.cant_steer",      "You can't steer the {vehicle}; it follows its own route.")
    , ("vehicle.cant_drive",      "You can't drive there from here. Stations: {stations}")
    , ("vehicle.need_controls",   "You need to be at the controls to drive.")
    , ("vehicle.drive_to",        "You drive to {stop}.")
    , ("vehicle.not_on",          "You are not on a vehicle.")
    , ("vehicle.manual_only",     "This vehicle only moves when you drive it.")
    , ("vehicle.route_end",       "The route has no further stops.")
    , ("vehicle.travel_on",       "You travel on to {stop}.")
    , ("vehicle.fuelled",         "The {vehicle} is fuelled ({f}/{max}).")
    , ("vehicle.warning",         "Warning: {list}!")
    , ("look.fuel_out",           "\nOut of {item} (0/{max})")
    , ("look.fuel",               "\nFuel ({item}: {f}/{max})")
    -- -- quests / effects / journal -----------------------------------------
    , ("container.move_refused", "You can't move an item into a container that way.")
    , ("quest.cannot_start",      "You cannot start that quest right now.")
    , ("quest.not_active",        "That quest is not active.")
    , ("quests.journal_empty",    "Your journal is empty.")
    , ("quests.journal_header",   "=== Journal ===\n")
    , ("quests.active_header",    "Active:")
    , ("quests.completed_header", "Completed:")
    -- -- equipment internals (Game.hs) ----------------------------------------
    , ("item.no_id",              "There is no item '{id}'.")
    , ("equip.not_equippable",    "You cannot equip the {item}.")
    , ("equip.need_carried",      "You need to be carrying the {item}.")
    , ("equip.slot_occupied",     "You already have the {item} equipped there. Unequip it first.")
    , ("equip.header",            "Equipment:\n")
    , ("equip.line",              "  {slot}: {name}")
    -- -- save / meta / world files (SaveLoad.hs, World.hs) -------------------
    , ("meta.unreadable",   "Warning: meta file '{path}' is unreadable — starting fresh.")
    , ("meta.corrupted",    "Warning: meta file '{path}' is corrupted — starting fresh.")
    , ("save.saved",        "Game saved to {path} ({timestamp}).")
    , ("save.not_found",    "Error: Save file '{path}' not found.")
    , ("save.unreadable",   "Error: Could not read file '{path}'.")
    , ("save.version_warning", "Warning: This save was made with a different world version. Results may be unpredictable.")
    , ("save.loaded",       "Game loaded from {path} (saved: {timestamp}).")
    , ("save.loaded_legacy",   "Game loaded (legacy format).")
    , ("save.loaded_legacy2",  "Game loaded from legacy format.")
    , ("save.corrupted",    "Error: Save file is corrupted or incompatible.")
    , ("save.slot_deleted", "Save slot deleted: {path}")
    , ("save.slot_delete_failed", "Warning: could not delete save slot {path}.")
    , ("save.none_found",   "No saved games found.")
    , ("save.list_header",  "=== Saved Games ===")
    , ("save.entry",        "  {name} — {timestamp} ({compat})")
    , ("save.compatible",   "compatible")
    , ("save.world_mismatch", "world mismatch!")
    , ("world.file_unreadable",   "Could not read world file '{path}': {err}")
    , ("world.save_unreadable",   "Could not read save file '{path}': {err}")
    ]

-- | The full help screen, byte-identical to the former inline intercalate
--   block in Parser.hs. Kept as its own definition for readability; contains
--   no @{...}@ placeholders.
helpTemplate :: String
helpTemplate = intercalate "\n"
    [ "=== Available Commands ==="
    , ""
    , "Movement:"
    , "  go/move/walk <direction>   - Move north/south/east/west/up/down"
    , "  <direction>                - Shorthand (e.g., just 'north')"
    , ""
    , "Interaction:"
    , "  look                       - Examine current room"
    , "  look at / examine <target> - Examine an item or NPC"
    , "  look at <n>                - Examine the n-th marked object (`map`)"
    , "  watch [target]             - Play an item's/NPC's animation frames"
    , "  map / legend               - Show the art with numbered marked objects"
    , "  search                     - Search the room for hidden things"
    , "  take / get / grab <item>   - Pick up an item"
    , "  take all                   - Pick up all items in the room"
    , "  drop <item>                - Drop an item"
    , "  drop all                   - Drop everything you're carrying"
    , "  use <item>                 - Use an item from inventory"
    , "  use <item> on <target>     - Use an item on something"
    , "  talk to / speak with <npc> - Talk to a character"
    , "  choose <n> / <n>           - Select a dialogue option"
    , "  attack / hit <npc>         - Attack an enemy"
    , ""
    , "Equipment:"
    , "  equip / wear / wield <item>- Equip an item"
    , "  unequip / remove <item>    - Unequip an item"
    , "  unequip all                - Remove all equipment"
    , "  stats                      - Show health, attack, defense and equipment"
    , ""
    , "Cards & Decks:"
    , "  hand / karten              - View cards in hand"
    , "  play <n> [target]          - Play the n-th card (e.g. 'play 1 goblin')"
    , "  deck                       - View draw pile"
    , "  discard / ablage           - View discard pile"
    , "  end turn / pass            - End combat turn"
    , ""
    , "Vehicles:"
    , "  enter / board <vehicle>    - Board a vehicle at your stop"
    , "  disembark                  - Leave the current vehicle ('exit' quits!)"
    , "  drive to <station>         - Steer (PlayerControlled, from the cockpit)"
    , "  wait                       - Advance to the next stop (AutomaticRoute)"
    , "  refuel [vehicle]           - Show fuel status"
    , "  repair <problem>           - Fix a vehicle condition"
    , ""
    , "Multi-item:"
    , "  take <item> and <item>     - Take multiple items"
    , ""
    , "System:"
    , "  inventory / inv / i        - Check what you're carrying"
    , "  undo                       - Restore the previous game state (up to 50)"
    , "  save [name]                - Save game (default: savegame)"
    , "  load [name]                - Load a saved game"
    , "  saves                      - List all saved games"
    , "  restart                    - Start a new game"
    , "  help                       - Show this help"
    , "  quit / exit / q            - Exit the game"
    , ""
    , "Tip: Press Tab to auto-complete commands and targets."
    ]

-- | English default catalog.
defaultCatalog :: Map MsgId String
defaultCatalog = Map.fromList catalogEntries

-- ---------------------------------------------------------------------------
-- Template interpolation (moved verbatim from Game.hs in Phase 1.1 so the
-- catalog can render without depending on Game — Game re-exports it and
-- keeps 'Game.formatWithVars' unchanged)
-- ---------------------------------------------------------------------------

-- | Apply a @:modifier@ to an interpolated value: @+@ forces a sign, a
--   number pads (right-aligned, or left-aligned after a minus).
applyVarModifier :: String -> String -> String
applyVarModifier str modif =
    let forceSign = '+' `elem` modif
        widthPart = filter (/= '+') modif
        signedStr = if forceSign
                    then case str of
                        ('-':_) -> str
                        _       -> '+' : str
                    else str
    in case widthPart of
        ('-':digits) | not (null digits) && all isDigit digits ->
            let w = read digits :: Int
            in signedStr ++ replicate (max 0 (w - length signedStr)) ' '
        digits | not (null digits) && all isDigit digits ->
            let w = read digits :: Int
            in replicate (max 0 (w - length signedStr)) ' ' ++ signedStr
        _ -> signedStr

-- | Interpolate @{var}@ / @{var:mod}@ through a resolver; @\\{@, @\\}@ and
--   doubled braces escape. Verbatim from Game.hs (Phase 1.1 move).
formatStringWith :: String -> (String -> Maybe String) -> String
formatStringWith [] _ = []
formatStringWith ('\\':'{':cs) env = '{' : formatStringWith cs env
formatStringWith ('\\':'}':cs) env = '}' : formatStringWith cs env
formatStringWith ('{':'{':cs) env = '{' : formatStringWith cs env
formatStringWith ('}':'}':cs) env = '}' : formatStringWith cs env
formatStringWith ('{':cs) env =
    case span (/= '}') cs of
        (inside, '}':rest) ->
            let (isExplicitVar, clean) = if "var:" `isPrefixOf` inside
                                        then (True, drop 4 inside)
                                        else (False, inside)
                (varName, modif) = case break (== ':') clean of
                    (name, ':':m) -> (name, m)
                    (name, _)     -> (name, "")
            in case env varName of
                Just val -> applyVarModifier val modif ++ formatStringWith rest env
                Nothing
                    | isExplicitVar -> applyVarModifier "0" modif ++ formatStringWith rest env
                    | otherwise     -> '{' : inside ++ "}" ++ formatStringWith rest env
        _ -> '{' : formatStringWith cs env
formatStringWith (c:cs) env = c : formatStringWith cs env