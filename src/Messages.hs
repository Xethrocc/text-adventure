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
    , MsgCatalog
    , renderMsg
    , renderMsgIn
    , msgPayload
    , msgPayloadIn
    , translateTerms
    , effectiveTerms
    , localizeEvents
    , effectiveCatalogFor
    , effectiveTermsFor
    , langPackFor
    , localizeEventsFor
    , renderMsgFor
    , catalogEntries
    , defaultCatalog
    , LangPack (..)
    , emptyLangPack
    , langPacks
    , knownLanguages
    , effectiveCatalog
    , formatStringWith
    , evalCondition
    , handleExpr
    , splitIfPipes
    , matchBrace
    , evMsg
    , trimStr
    , grammarArgs
    , grammarArgKeys
    , isGrammarArgKey
    , templateGrammarKeys
    , itemGrammarMsgKeys
    , npcGrammarMsgKeys
    ) where

import Data.Char (isDigit, isSpace, isAlphaNum, toLower)
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import qualified Data.List as List (lookup)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Read (readMaybe)
import Messages.LangDe (langDe)
import Types.Output (MsgPayload (..), OutputEvent (..))
import Types.Core (Expr (..), parseExpr, GameWorld (..), Grammar (..))

-- | Stable message key. Plain alias: the catalog is data, and Phase 4.3
--   overlays YAML-provided keys on the same namespace.
type MsgId = String

-- | A message catalog: template per key. 'defaultCatalog' is the English one;
--   language packs (Phase 4.3) and per-adventure @messages:@ overrides layer
--   on top of it (see 'effectiveCatalog').
type MsgCatalog = Map MsgId String

-- | Render a catalog message: substitute @{arg}@ placeholders from the
--   argument list (same syntax and modifiers as 'formatStringWith'). An
--   unknown key renders as @\<msg:key\>@ — loud on purpose, unit tests and
--   the E2E goldens must never see it.
renderMsg :: MsgId -> [(String, String)] -> String
renderMsg = renderMsgIn defaultCatalog

-- | 'renderMsg' against an arbitrary catalog (Phase 4.3: the effective
--   catalog of a world, see 'effectiveCatalog').
renderMsgIn :: MsgCatalog -> MsgId -> [(String, String)] -> String
renderMsgIn catalog key args =
    case Map.lookup key catalog of
        Nothing  -> "<msg:" ++ key ++ ">"
        Just tmpl -> formatStringWith tmpl lookupArg
  where
    -- Grammar placeholders (4.3.5) render as an empty string when their arg
    -- is missing: deterministic, never the literal placeholder and never
    -- <error: …>. Everything else keeps 'formatStringWith' behaviour.
    lookupArg k = case List.lookup k args of
        Just v  -> Just v
        Nothing | isGrammarArgKey k -> Just ""
        Nothing -> Nothing

-- | 4.3.5 (Variante A): the flat grammar-placeholder names. They accompany
--   the PRIMARY entity of a message ('grammarArgs' with primary = True);
--   per-slot names (@{item_article_acc}@, @{npc_gender}@, …) accompany any
--   entity argument.
grammarArgKeys :: [String]
grammarArgKeys = ["article_nom", "article_acc", "article_dat", "gender"]

grammarSlotSuffixes :: [String]
grammarSlotSuffixes = ["_article_nom", "_article_acc", "_article_dat", "_gender"]

-- | True for every grammar-placeholder name (flat or per-slot).
isGrammarArgKey :: String -> Bool
isGrammarArgKey k =
    k `elem` grammarArgKeys || any (`isSuffixOf` k) grammarSlotSuffixes

-- | Grammar args for one entity slot (4.3.5). The slot is the message
--   argument the entity fills (@"item"@, @"npc"@, @"name"@, …). Only PRESENT
--   fields become args — missing fields mean missing args, and the renderer
--   defaults grammar placeholders to the empty string. With primary = True
--   the flat article/gender names are emitted as well (they accompany the
--   message's primary entity).
grammarArgs :: Bool -> String -> Grammar -> [(String, String)]
grammarArgs primary slot g =
    slotArgs ++ (if primary then flatArgs else [])
  where
    forms    = [ ("article_nom", gNom g), ("article_acc", gAcc g)
               , ("article_dat", gDat g), ("gender", gGender g) ]
    slotArgs = [ (slot ++ "_" ++ n, v) | (n, Just v) <- forms ]
    flatArgs = [ (n, v) | (n, Just v) <- forms ]

-- | The grammar placeholders a template references (placeholder scan; grammar
--   args inside @{if …}@ conditions are not counted — conservative). Used by
--   the compiler warning for entities without grammar fields.
templateGrammarKeys :: String -> [String]
templateGrammarKeys = filter isGrammarArgKey . templateSlots
  where
    templateSlots [] = []
    templateSlots (c:cs)
        | c == '{' = case matchBrace cs of
            Just (inside, rest) -> slotName inside : templateSlots rest
            Nothing             -> templateSlots cs
        | otherwise = templateSlots cs
    slotName inside =
        let stripped = trimStr inside
            afterVar = if "var:" `isPrefixOf` stripped then drop 4 stripped else stripped
        in takeWhile (\ch -> ch /= ':' && ch /= ' ') afterVar

-- | The catalog keys whose call sites attach ITEM grammar args (Parser.hs:
--   take/drop/use/equip/examine/container plus the item half of the B7 NPC
--   possession messages). Keep in sync with the call sites — the worldbuilder
--   MissingGrammar warning uses these sets.
itemGrammarMsgKeys :: [MsgId]
itemGrammarMsgKeys =
    [ "take.ok", "take.already", "take.not_portable", "drop.ok", "use.ok"
    , "item.cant_do", "search.nothing_item", "search.reveal"
    , "equip.ok", "unequip.ok", "unequip.not_equipped"
    , "npc.took_from", "npc.gave_to"
    , "container.empty", "container.contains", "container.opened"
    , "container.closed", "container.locked", "container.unlocked"
    , "container.is_locked", "container.is_closed", "container.not_locked"
    , "container.already_open", "container.already_closed", "container.full"
    , "container.no_item", "container.took_from", "container.put"
    ]

-- | The catalog keys whose call sites attach NPC grammar args.
npcGrammarMsgKeys :: [MsgId]
npcGrammarMsgKeys =
    [ "search.nothing_npc", "npc.no_item", "npc.took_from", "npc.gave_to" ]

-- | A catalog message as a structured payload: key, args and the rendered
--   text (byte-identical to 'renderMsg'). Phase 1.2.
msgPayload :: MsgId -> [(String, String)] -> MsgPayload
msgPayload = msgPayloadIn defaultCatalog

-- | 'msgPayload' against an arbitrary catalog (Phase 4.3).
msgPayloadIn :: MsgCatalog -> MsgId -> [(String, String)] -> MsgPayload
msgPayloadIn cat key args = MsgPayload (Just key) args (renderMsgIn cat key args)

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
    , ("stats.progression",     "Level {level} — {name} ({xp}/{next} XP)")
    , ("stats.progression_max", "Level {level} — {name} ({xp} XP)")
    , ("levelup.default",       "You reached level {level}: {name}!")
    , ("xp.clamped",            "XP cannot drop below 0.")
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
    , ("dialog.no_topic",        "{npc} has nothing to say about {topic}.")
    , ("dialog.no_npc",          "There is no one by that name.")
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
    , ("use.no_known_recipe", "You don't know a recipe with {item1} and {item2}.")
    , ("craft.no_recipe",     "You don't know a recipe for {target}.")
    , ("craft.no_product",    "There is no recipe that produces {target}.")
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
    -- -- NPC possession (B7) ------------------------------------------------
    , ("npc.carries",          "\nCarrying: {items}.")
    -- -- NPC equipment (B9) -------------------------------------------------
    , ("npc.wears",            "\nWearing: {items}.")
    , ("npc.drops_items",      "\n{npc} drops: {items}.")
    , ("npc.gave_to",          "You give the {item} to {npc}.")
    , ("npc.no_item",          "You find no {item} on {npc}.")
    , ("npc.took_from",        "You take the {item} from {npc}.")
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
    , ("ability.use",            "You use {ability}!")
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
    , ("consume.not_reachable",   "That is not within your reach.")
    , ("container.move_refused", "You can't move an item into a container that way.")
    , ("quest.cannot_start",      "You cannot start that quest right now.")
    , ("quest.not_active",        "That quest is not active.")
    , ("learn.default",           "Noted.")
    , ("recipes.learn.default",   "You learn a recipe: {recipe}.")
    , ("recipes.header",          "Recipes: {known} / {total}")
    , ("recipes.entry",           "{name} — {ingredients}")
    , ("recipes.empty",           "You know no recipes.")
    , ("notes.header",            "Your notes:")
    , ("notes.empty",             "Your notes are empty.")
    , ("chapter.no_next",         "There is no next chapter.")
    , ("chapter.refuse_back",     "This chapter is behind you.")
    , ("pursuit.step",            "{name} moves to {room}.")
    , ("pursuit.flee",            "{name} flees to {room}.")
    , ("pursuit.no_path",         "{name} can find no way.")
    , ("container.not_a_container", "{target} is not a container.")
    , ("container.opened",        "{name} is open now.")
    , ("container.closed",        "{name} is closed now.")
    , ("container.locked",        "{name} is locked now.")
    , ("container.unlocked",      "{name} is unlocked now.")
    , ("container.is_locked",     "{name} is locked.")
    , ("container.not_locked",    "{name} is not locked.")
    , ("container.already_open",  "{name} is already open.")
    , ("container.already_closed", "{name} is already closed.")
    , ("container.is_closed",      "{name} is closed.")
    , ("container.full",          "There is no room in {name}.")
    , ("container.no_item",       "You find no {item} in {name}.")
    , ("container.put",           "You put {item} in {name}.")
    , ("container.took_from",     "You take {item} from {name}.")
    , ("container.contains",      "In {name}: {items}.")
    , ("container.empty",         "{name} is empty.")
    , ("inventory.full",          "You are carrying too much.")
    , ("chapter.unknown",         "There is no such chapter.")
    , ("proc.unknown",            "That procedure does not exist.")
    , ("proc.arity",              "That procedure cannot be called this way.")
    , ("quests.journal_empty",    "Your journal is empty.")
    -- -- devices / fixtures (W4) ----------------------------------------------
    , ("device.insert",           "You insert the {item} into the {device}.")
    , ("device.remove",           "You remove the {item} from the {device}.")
    , ("device.reject",           "The {item} does not fit into the {device}.")
    , ("device.occupied",         "There is already something in the {device}.")
    , ("device.flip",             "You flip the {device} to {state}.")
    , ("device.examine_mounted",  "\nMounted: {item}.")
    , ("device.not_carried",      "You are not carrying {item}.")
    , ("device.not_in_device",    "There is no {item} in the {device}.")
    , ("device.cant_insert",      "You cannot insert that.")
    , ("device.cant_remove",      "You cannot remove that.")
    , ("device.cant_flip",        "You cannot flip that.")
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
-- Language packs (Phase 4.3 / D4)
-- ---------------------------------------------------------------------------

-- | A language pack: translated templates plus input vocabulary. The alias
--   tables map alias words to their canonical English token (verbs,
--   directions, command words, prepositions) — they extend the English input
--   syntax, they never replace it. Term translations cover enumerable
--   argument values (@dir.*@, @slot.*@, @card_type.*@).
data LangPack = LangPack
    { lpLanguage     :: String
    , lpTemplates    :: MsgCatalog
    , lpTerms        :: Map.Map String String
    , lpVerbAliases  :: Map.Map String [String]
    , lpDirAliases   :: Map.Map String [String]
    , lpCmdAliases   :: Map.Map String [String]
    , lpPrepositions :: Map.Map String [String]
    } deriving (Show, Eq)

-- | A pack with no data at all — the starting point for tests.
emptyLangPack :: String -> LangPack
emptyLangPack lang = LangPack lang Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

-- | The built-in language packs, keyed by language code. Currently only
--   @de@ (data from @lang/de.json@ via @scripts/gen-lang-pack.py@); @en@ is
--   the bare 'defaultCatalog' and has no pack.
langPacks :: Map.Map String LangPack
langPacks = Map.fromList
    [ (lpLanguage p, p)
    | p <- [packFromTuple langDe] ]

-- | Language codes the engine knows: the default @en@ plus every pack.
--   Used by the worldbuilder for the @UnknownLanguage@ compile check.
knownLanguages :: [String]
knownLanguages = "en" : Map.keys langPacks

-- | The catalog a world actually renders with: language pack on top of the
--   English default, the adventure's @messages:@ overrides on top of that.
--   A key missing from the pack falls back to the English template (never to
--   the loud @\<msg:...\>@ form); a missing language has no pack and renders
--   plain English.
effectiveCatalog :: Maybe String -> Map.Map MsgId String -> MsgCatalog
effectiveCatalog mLang overrides =
    let pack = maybe Map.empty lpTemplates (mLang >>= (`Map.lookup` langPacks))
    in Map.unions [overrides, pack, defaultCatalog]

-- | Unpack a generated pack module's flat tuple into a 'LangPack'.
packFromTuple :: ( String, [(String, String)], [(String, String)]
                 , [(String, [String])], [(String, [String])]
                 , [(String, [String])], [(String, [String])] ) -> LangPack
packFromTuple (lang, msgs, terms, verbs, dirs, cmds, preps) = LangPack
    { lpLanguage     = lang
    , lpTemplates    = Map.fromList msgs
    , lpTerms        = Map.fromList terms
    , lpVerbAliases  = Map.fromList verbs
    , lpDirAliases   = Map.fromList dirs
    , lpCmdAliases   = Map.fromList cmds
    , lpPrepositions = Map.fromList preps
    }

-- ---------------------------------------------------------------------------
-- Term translation and the localization pass (Phase 4.3)
-- ---------------------------------------------------------------------------

-- | The closed set of argument slots whose values are enumerable engine
--   terms (Phase 4.3 decision: @dir@ = directions, @slot@ = equip slots).
--   Every other argument is authored content in the author's language and is
--   passed through unchanged. @card_type.*@ terms are not args — the card
--   screen resolves them directly (see 'termValue').
termSlots :: [String]
termSlots = ["dir", "slot"]

-- | Translate one enumerable argument value via the @slot.value@ term table;
--   unknown values pass through unchanged (English tokens stay canonical).
termValue :: Map.Map String String -> String -> String -> String
termValue terms slot value =
    Map.findWithDefault value (slot ++ "." ++ map toLower value) terms

-- | Translate the term-slot arguments of one message (Phase 4.3).
translateTerms :: Map.Map String String -> [(String, String)] -> [(String, String)]
translateTerms terms args =
    [ (k, if k `elem` termSlots then termValue terms k v else v) | (k, v) <- args ]

-- | The effective term table of a world (Phase 4.3): the language pack's
--   terms. Term lookups fall back to the untranslated value, so an
--   untranslated pack is the identity.
effectiveTerms :: Maybe String -> Map.Map String String
effectiveTerms mLang = maybe Map.empty lpTerms (mLang >>= (`Map.lookup` langPacks))

-- | The localization pass (Phase 4.3): re-render every keyed message from
--   its key and args against the effective catalog, translating term-valued
--   args first. Structure-preserving — event count, order and every non-keyed
--   event (including @mpKey = Nothing@ payloads: authored @msg:@/@SendMessage@
--   text) stay untouched; with the default catalog and no terms it is the
--   identity. Call it at the output edge before 'Types.Output.renderEvents'
--   or protocol encoding. NOTE (non-empty contract): the fragment algebra
--   ('joinEv' etc.) decides emptiness on the *default-rendered* text, so
--   templates and @messages:@ overrides must never render empty — the
--   compiler enforces this ('EmptyMessageOverride'), the packs are generated
--   non-empty.
localizeEvents :: MsgCatalog -> Map.Map String String -> [OutputEvent] -> [OutputEvent]
localizeEvents cat terms = map go
  where
    go ev = case ev of
        EvMessage p | Just key <- mpKey p ->
            EvMessage (msgPayloadIn cat key (translateTerms terms (mpArgs p)))
        _ -> ev

-- | The effective catalog of a world (Phase 4.3).
effectiveCatalogFor :: GameWorld -> MsgCatalog
effectiveCatalogFor w = effectiveCatalog (worldLanguage w) (worldMessages w)

-- | The effective term table of a world (Phase 4.3).
effectiveTermsFor :: GameWorld -> Map.Map String String
effectiveTermsFor w = effectiveTerms (worldLanguage w)

-- | The language pack of a world (Phase 4.3): 'Nothing' without `language:`.
langPackFor :: GameWorld -> Maybe LangPack
langPackFor w = worldLanguage w >>= (`Map.lookup` langPacks)

-- | 'localizeEvents' with a world's effective catalog and terms.
localizeEventsFor :: GameWorld -> [OutputEvent] -> [OutputEvent]
localizeEventsFor w = localizeEvents (effectiveCatalogFor w) (effectiveTermsFor w)

-- | 'renderMsgIn' with a world's effective catalog (Phase 4.3: the chrome
--   lines the loop and 'SaveLoad' print outside the event stream).
renderMsgFor :: GameWorld -> MsgId -> [(String, String)] -> String
renderMsgFor w = renderMsgIn (effectiveCatalogFor w)

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

-- | Find the matching '}' for an opening '{', taking into account nested braces
--   and escaped braces ('\{', '\}').
matchBrace :: String -> Maybe (String, String)
matchBrace str = go (1 :: Int) [] str
  where
    go 0 acc rest = Just (reverse acc, rest)
    go _ _   []   = Nothing
    go d acc ('\\':'{':rest) = go d ('{':'\\':acc) rest
    go d acc ('\\':'}':rest) = go d ('}':'\\':acc) rest
    go d acc ('{':rest)      = go (d + 1) ('{':acc) rest
    go d acc ('}':rest)
        | d == 1             = Just (reverse acc, rest)
        | otherwise          = go (d - 1) ('}':acc) rest
    go d acc (c:rest)        = go d (c:acc) rest

-- | Split the body of an '{if ...}' expression by unescaped pipes ('|') at brace depth 0.
splitIfPipes :: String -> [String]
splitIfPipes str = go (0 :: Int) [] [] str
  where
    go _ current parts [] = reverse (reverse current : parts)
    go d current parts ('\\':'|':rest) = go d ('|':current) parts rest
    go d current parts ('\\':'{':rest) = go d ('{':'\\':current) parts rest
    go d current parts ('\\':'}':rest) = go d ('}':'\\':current) parts rest
    go d current parts ('{':'{':rest)  = go d ('{':'{':current) parts rest
    go d current parts ('}':'}':rest)  = go d ('}':'}':current) parts rest
    go d current parts ('{':rest)      = go (d + 1) ('{':current) parts rest
    go d current parts ('}':rest)
        | d > 0     = go (d - 1) ('}':current) parts rest
        | otherwise = go d ('}':current) parts rest
    go 0 current parts ('|':rest)      = go (0 :: Int) [] (reverse current : parts) rest
    go d current parts (c:rest)        = go d (c:current) parts rest

trimStr :: String -> String
trimStr = dropWhile isSpace . reverse . dropWhile isSpace . reverse

stripQuotes :: String -> String
stripQuotes str =
    let t = trimStr str
    in case t of
        ('"':rest) | not (null rest) && last rest == '"' -> init rest
        ('\'':rest) | not (null rest) && last rest == '\'' -> init rest
        _ -> t

-- | Evaluate an inline condition: supports comparisons ('==', '!=', '/=', '>=', '<=', '>', '<', '='),
--   negation ('!cond'), or single flag/variable truthiness.
evalCondition :: String -> (String -> Maybe String) -> Either String Bool
evalCondition rawCond env =
    let cond = trimStr rawCond
    in if null cond
        then Left "<error: invalid if condition: empty condition>"
        else if "!" `isPrefixOf` cond
            then let inner = trimStr (drop 1 cond)
                 in if null inner
                     then Left "<error: invalid if condition: empty negated condition>"
                     else not <$> evalCondition inner env
            else case findCondOperator cond of
                Just (rawLhs, opStr, rawRhs) -> do
                    let lhsStr = trimStr rawLhs
                        rhsStr = trimStr rawRhs
                    if null lhsStr || null rhsStr
                        then Left ("<error: invalid if condition: " ++ cond ++ ">")
                        else do
                            valLhs <- resolveOperand lhsStr env
                            valRhs <- resolveOperand rhsStr env
                            evalComparison valLhs opStr valRhs
                Nothing ->
                    if any (\c -> not (isAlphaNum c || c `elem` "._:! ")) cond
                        then Left ("<error: invalid if condition: " ++ cond ++ ">")
                        else evalTruthiness cond env

findCondOperator :: String -> Maybe (String, String, String)
findCondOperator str = search (0 :: Int) [] str
  where
    ops = [">=", "<=", "==", "!=", "/=", "=", ">", "<"]
    search _ _ [] = Nothing
    search d acc s@(c:cs)
        | c == '{'  = search (d + 1) (c:acc) cs
        | c == '}'  = search (max 0 (d - 1)) (c:acc) cs
        | d == 0    = case [op | op <- ops, op `isPrefixOf` s] of
            (op:_) -> Just (reverse acc, op, drop (length op) s)
            []     -> search d (c:acc) cs
        | otherwise = search d (c:acc) cs

resolveOperand :: String -> (String -> Maybe String) -> Either String String
resolveOperand tok env =
    let clean = stripQuotes tok
        lookupName = if "standing_name:" `isPrefixOf` clean
                     then "standing_name." ++ trimStr (drop 14 clean)
                     else clean
    in case env lookupName of
        Just v
            | "<error:" `isPrefixOf` v -> Left v
            | otherwise -> Right v
        Nothing -> Right clean

evalComparison :: String -> String -> String -> Either String Bool
evalComparison v1 op v2 =
    case (readMaybe v1 :: Maybe Int, readMaybe v2 :: Maybe Int) of
        (Just n1, Just n2) -> case op of
            "==" -> Right (n1 == n2)
            "="  -> Right (n1 == n2)
            "!=" -> Right (n1 /= n2)
            "/=" -> Right (n1 /= n2)
            ">=" -> Right (n1 >= n2)
            "<=" -> Right (n1 <= n2)
            ">"  -> Right (n1 > n2)
            "<"  -> Right (n1 < n2)
            _    -> Left ("<error: unknown operator: " ++ op ++ ">")
        _ -> case op of
            "==" -> Right (v1 == v2)
            "="  -> Right (v1 == v2)
            "!=" -> Right (v1 /= v2)
            "/=" -> Right (v1 /= v2)
            ">=" -> Right (v1 >= v2)
            "<=" -> Right (v1 <= v2)
            ">"  -> Right (v1 > v2)
            "<"  -> Right (v1 < v2)
            _    -> Left ("<error: unknown operator: " ++ op ++ ">")

evalTruthiness :: String -> (String -> Maybe String) -> Either String Bool
evalTruthiness tok env =
    case env tok of
        Nothing -> Right False
        Just v
            | "<error:" `isPrefixOf` v -> Left v
            | v == "true"              -> Right True
            | v == "false"             -> Right False
            | otherwise -> case (readMaybe v :: Maybe Int) of
                Just n  -> Right (n /= 0)
                Nothing -> Right (not (null v))

handleIf :: String -> (String -> Maybe String) -> String
handleIf body env =
    case splitIfPipes body of
        []  -> "<error: invalid if syntax: expected {if <cond>|a|b}>"
        [_] -> "<error: invalid if syntax: expected {if <cond>|a|b}>"
        (rawCond : thenBranch : rest) ->
            let elseBranch = case rest of
                    []    -> ""
                    (e:_) -> e
            in case evalCondition rawCond env of
                Left err    -> err
                Right True  -> formatStringWith thenBranch env
                Right False -> formatStringWith elseBranch env

-- | Evaluate an expression string against the environment, returning either formatted
--   result or an error string.
handleExpr :: String -> (String -> Maybe String) -> String
handleExpr content env
    | null (trimStr content) = "<error: expr: empty expression>"
    | otherwise =
        let (rawExpr, modif) = splitExprModifier (trimStr content)
        in case parseExpr rawExpr of
            Left err -> "<error: expr: " ++ err ++ ">"
            Right expr -> case evalExprEnv env expr of
                Left err -> "<error: " ++ err ++ ">"
                Right val -> applyVarModifier (show val) modif

splitExprModifier :: String -> (String, String)
splitExprModifier s =
    case parseExpr s of
        Right _ -> (s, "")
        Left _  -> case breakLastColon s of
            Just (beforeCol, modif)
                | not (null modif) && (modif == "+" || all isDigit (dropWhile (== '-') modif)) ->
                    case parseExpr (trimStr beforeCol) of
                        Right _ -> (trimStr beforeCol, modif)
                        Left _  -> (s, "")
            _ -> (s, "")

breakLastColon :: String -> Maybe (String, String)
breakLastColon s =
    case break (== ':') (reverse s) of
        (revAfter, ':':revBefore) -> Just (reverse revBefore, reverse revAfter)
        _                         -> Nothing

evalExprEnv :: (String -> Maybe String) -> Expr -> Either String Int
evalExprEnv env expr = case expr of
    ELit n -> Right n
    EVar v -> case env v of
        Nothing -> Left ("unknown variable '" ++ v ++ "'")
        Just s
            | "<error:" `isPrefixOf` s ->
                let inner = drop 8 s
                    cleaned = if not (null inner) && last inner == '>' then init inner else inner
                in Left cleaned
            | otherwise -> case readMaybe s of
                Just n  -> Right n
                Nothing -> Left ("variable '" ++ v ++ "' is not an integer: " ++ s)
    EAdd a b -> (+) <$> evalExprEnv env a <*> evalExprEnv env b
    ESub a b -> (-) <$> evalExprEnv env a <*> evalExprEnv env b
    EMul a b -> (*) <$> evalExprEnv env a <*> evalExprEnv env b
    EDiv a b -> do
        va <- evalExprEnv env a
        vb <- evalExprEnv env b
        if vb == 0 then Right 0 else Right (va `div` vb)
    EMod a b -> do
        va <- evalExprEnv env a
        vb <- evalExprEnv env b
        if vb == 0 then Right 0 else Right (va `mod` vb)
    EMin a b -> min <$> evalExprEnv env a <*> evalExprEnv env b
    EMax a b -> max <$> evalExprEnv env a <*> evalExprEnv env b
    EClamp mn mx v -> do
        l <- evalExprEnv env mn
        h <- evalExprEnv env mx
        val <- evalExprEnv env v
        let low = min l h
            high = max l h
        Right (max low (min high val))

-- | Interpolate @{var}@ / @{var:mod}@, @{if cond|a|b}@, @{= expr}@ through a resolver;
--   @\\{@, @\\}@ and doubled braces escape.
formatStringWith :: String -> (String -> Maybe String) -> String
formatStringWith [] _ = []
formatStringWith ('\\':'{':cs) env = '{' : formatStringWith cs env
formatStringWith ('\\':'}':cs) env = '}' : formatStringWith cs env
formatStringWith ('{':'{':cs) env = '{' : formatStringWith cs env
formatStringWith ('}':'}':cs) env = '}' : formatStringWith cs env
formatStringWith ('{':cs) env =
    case matchBrace cs of
        Just (inside, rest) ->
            let stripped = trimStr inside
            in if stripped == "if" || "if " `isPrefixOf` stripped || "if:" `isPrefixOf` stripped || "if|" `isPrefixOf` stripped
                then
                    let ifBody = case stripped of
                            'i':'f':':':r -> trimStr r
                            'i':'f':'|':r -> '|' : r
                            'i':'f':' ':r -> trimStr r
                            'i':'f':r     -> trimStr r
                            _             -> stripped
                    in handleIf ifBody env ++ formatStringWith rest env
                else if "=" `isPrefixOf` stripped
                    then
                        let exprBody = trimStr (drop 1 stripped)
                        in handleExpr exprBody env ++ formatStringWith rest env
                    else
                        let strippedInside = trimStr inside
                            (isStanding, isExplicitVar, clean) =
                                if "standing_name:" `isPrefixOf` strippedInside
                                then (True, False, "standing_name." ++ dropWhile isSpace (drop 14 strippedInside))
                                else if "standing_name." `isPrefixOf` strippedInside
                                then (True, False, strippedInside)
                                else if "var:" `isPrefixOf` strippedInside
                                then (False, True, dropWhile isSpace (drop 4 strippedInside))
                                else (False, False, inside)
                            (varName, modif) = case break (== ':') clean of
                                (name, ':':m) -> (if isExplicitVar || isStanding then trimStr name else name, m)
                                (name, _)     -> (if isExplicitVar || isStanding then trimStr name else name, "")
                        in case env varName of
                            Just val
                                | "<error:" `isPrefixOf` val -> val ++ formatStringWith rest env
                                | otherwise                  -> applyVarModifier val modif ++ formatStringWith rest env
                            Nothing
                                | isExplicitVar -> applyVarModifier "0" modif ++ formatStringWith rest env
                                | isStanding    -> applyVarModifier "" modif ++ formatStringWith rest env
                                | otherwise     -> '{' : inside ++ "}" ++ formatStringWith rest env
        Nothing -> '{' : formatStringWith cs env
formatStringWith (c:cs) env = c : formatStringWith cs env