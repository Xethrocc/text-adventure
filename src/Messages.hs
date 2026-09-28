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
    ) where

import Data.Char (isDigit)
import Data.List (intercalate, isPrefixOf, lookup)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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
        Just tmpl -> formatStringWith tmpl (`lookup` args)

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
    , ("map.dark",            "It's pitch black. You can't see a map.")
    , ("map.no_marks",        "There is nothing marked on the map.")
    , ("map.legend_header",   "\n\nLegend:\n")
    , ("map.legend_line",     "  {i}: {label}")
    , ("watch.void",          "You're in a void. There's nothing to watch.")
    , ("watch.dark",          "It's pitch black. You can't watch anything.")
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