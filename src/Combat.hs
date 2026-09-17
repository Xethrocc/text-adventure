-- | Combat policy resolution (Phase 7f).
--
--   `resolveCombat` is a PURE function: it takes the authored combat profile,
--   the attacking actors, the target, and the current game state, and returns
--   a list of Effects (to be run through the single outcome interpreter
--   `applyOutcomeWith`) plus a list of player-facing messages.
--
--   This replaces the hardcoded combat inside `Parser.executeAttack`; the
--   parser only wires profile + actors + target into this function and applies
--   the returned effects. There is deliberately no second interpreter.
module Combat
    ( CombatActor (..)
    , CombatTarget (..)
    , ShipSystems (..)
    , resolveCombat
    , shipAbsorb
    ) where

import Types
import Game (effectiveAttack, effectiveDefense, getVariable)
import qualified Data.Map.Strict as Map
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)

-- | Who attacks. Phase 7f only ever fires `PlayerActor`; 7g adds companions,
--   7h the player's own ship (when the vehicle declares `systems:`).
data CombatActor
    = PlayerActor
    | CompanionActor String
    | ShipActor VehicleID
    deriving (Show, Eq)

-- | What is attacked. Carries the NPC id plus the display name the player
--   typed, so messages stay identical to the pre-7f parser output.
data CombatTarget
    = TargetNPC String String   -- ^ npc id, display label (what the player typed)
    deriving (Show, Eq)

-- | Resolve one attack command against the authored combat profile.
--   Returns (effects, messages): the effects are applied by the caller
--   through the single outcome interpreter; the messages are added to the
--   command output. No state is mutated here.
--
--   The `CombatAction` parameter is what 7f-3 needs (a round-based profile has
--   to know whether the player attacks, defends, flees or uses an ability).
--   Step A0 only adds the parameter: `off`, `narrative` and `classic` are
--   deterministic single-shot profiles and ignore it, and `executeAttack`
--   always passes `CAAttack`. A2/A3 wire the other constructors.
resolveCombat :: CombatProfile -> [CombatActor] -> CombatTarget -> CombatAction
              -> GameState -> ([Effect], [String])
resolveCombat profile actors target _action st = case profile of
    -- off: attack is refused, no HP is spent by anyone.
    CombatOff mRefused -> ([], [refusedMsg])
      where
        refusedMsg = case mRefused of
            Just txt -> txt
            Nothing  -> "You can't attack the " ++ label target ++ " here."
    -- narrative: opposed roll (player attack vs defense + difficulty).
    --   No HP attrition — the on_win / on_lose effects decide everything.
    CombatNarrative nc -> resolveNarrative nc actors target st
    -- classic: exactly the behaviour that predates Phase 7f: the player
    --   strikes first, the target retaliates in the same command,
    --   damage = attack - defense (min 1 / min 0), death via HP <= 0.
    CombatClassic -> resolveClassic actors target st
  where
    label (TargetNPC _ disp) = disp

-- | Narrative: `effectiveAttack >= defense + difficulty` wins.
resolveNarrative :: NarrativeCombat -> [CombatActor] -> CombatTarget -> GameState -> ([Effect], [String])
resolveNarrative nc _ (TargetNPC nid disp) st =
    case Map.lookup nid (npcDefs (world st)) of
        Nothing -> ([], ["You can't attack the " ++ disp ++ "."])
        Just npc ->
            let win = effectiveAttack st >= npcDefenseBase npc + ncDifficulty nc
            in if win
               then ([ncOnWin nc], ["You win the fight against the " ++ disp ++ "! Your attack lands cleanly."])
               else ([ncOnLose nc], ["You lose the fight against the " ++ disp ++ ". Your attack is turned aside."])

-- | Classic: bit-identical to `Parser.executeAttack` pre-7f when the actor
--   list is just the player. Phase 7g adds companions: every living
--   `CompanionActor` standing where the target stands strikes the same target
--   after the player's blow (damage = attack - target defense, min 1), unless
--   the player's blow already killed it. Phase 7h adds the player's ship
--   (`ShipActor`): it fires its `weapons` system (one `power` per shot) and its
--   `shields`/`hull` take the return fire instead of the player. Companions
--   never strike themselves, and without companions/ship systems nothing
--   changes.
--   The damage math is fully deterministic, so the outcome (kill / survive,
--   player death / survival) is decided here; the returned effects are plain
--   HP/variable modifications — NPC death (killNPC) and player death (endGame)
--   are handled automatically by `modifyNPCHealth` / `ModifyValue VRPlayerHealth`
--   in the single outcome interpreter.
resolveClassic :: [CombatActor] -> CombatTarget -> GameState -> ([Effect], [String])
resolveClassic actors (TargetNPC nid disp) st =
    case Map.lookup nid (npcStates (save st)) of
        Nothing -> ([], [cannotAttack])
        Just ns ->
            case Map.lookup nid (npcDefs (world st)) of
                Nothing -> ([], [cannotAttack])
                Just npc -> case npcHealth ns of
                    Nothing -> ([], [cannotAttack])
                    Just hp ->
                        let playerDmg = max 1 (effectiveAttack st - npcDefenseBase npc)
                            playerEffects = [ ModifyValue (VRProperty nid "hp") (-playerDmg) ]
                            mShip = firstShip actors st
                        in if hp - playerDmg <= 0
                           then ( playerEffects
                                , [ "You attack the " ++ disp ++ " and kill it!" ] )
                           else
                               let allies = companionHits nid (npcLocation ns) (npcDefenseBase npc) actors st
                                   allyEffects = [ ModifyValue (VRProperty nid "hp") (-d)
                                                 | (_, _, d) <- allies ]
                                   allyMsgs = [ npcName allyNpc ++ " strikes for " ++ show d ++ "."
                                              | (_, allyNpc, d) <- allies ]
                                   allyTotal = sum [ d | (_, _, d) <- allies ]
                                   (shipEffects, shipMsgs, shipTotal) =
                                       case mShip of
                                           Nothing   -> ([], [], 0)
                                           Just ship -> shipStrike nid ship
                                   effects = playerEffects ++ allyEffects ++ shipEffects
                                   msgsBeforeHit = allyMsgs ++ shipMsgs
                               in if hp - playerDmg - allyTotal - shipTotal <= 0
                                  then ( effects
                                       , ("You attack the " ++ disp ++ " and kill it!") : msgsBeforeHit )
                                  else
                                      let npcDmg = max 0 (npcAttackBase npc - effectiveDefense st)
                                          (retalEffects, taken, retalMsgs) = case mShip of
                                              Nothing   -> ([], npcDmg, [])
                                              Just ship -> shipAbsorb ship npcDmg
                                          takenEffects = [ ModifyValue VRPlayerHealth (-taken)
                                                         | taken > 0 ]
                                          playerHpAfter = playerHealth (player (save st)) - taken
                                          withRetaliation = effects ++ retalEffects ++ takenEffects
                                          allMsgs = msgsBeforeHit ++ retalMsgs
                                      in if playerHpAfter <= 0
                                         then ( withRetaliation
                                              , ("The " ++ disp ++ " strikes back and kills you!") : allMsgs )
                                         else ( withRetaliation
                                              , ("You hit for " ++ show playerDmg
                                                 ++ ", it hits you for " ++ show npcDmg ++ ".") : allMsgs )
  where
    cannotAttack = "You can't attack the " ++ disp ++ "."

-- ---------------------------------------------------------------------------
-- Ship systems (Phase 7h)
-- ---------------------------------------------------------------------------

-- | The combat-relevant systems of one ship, read out of the VarMap
--   (`ship.<vehicleId>.<system>`). `Nothing` means the system is not declared.
data ShipSystems = ShipSystems
    { ssShipId  :: VehicleID
    , ssName    :: String
    , ssPower   :: Maybe Int
    , ssWeapons :: Maybe Int
    , ssShields :: Maybe Int
    , ssHull    :: Maybe Int
    }

-- | The VarMap key of a ship system.
shipVar :: VehicleID -> String -> String
shipVar vId system = "ship." ++ vId ++ "." ++ system

-- | Read a system value, if the ship declares it.
shipSystem :: VehicleID -> String -> GameState -> Maybe Int
shipSystem vId system st = case getVariable (shipVar vId system) st of
    Just (VVInt n) -> Just n
    _              -> Nothing

-- | A ship has systems when any `ship.<id>.*` variable exists. Vehicles
--   without them are ordinary vehicles (bit-identical to 7f/7g).
shipHasSystems :: VehicleID -> GameState -> Bool
shipHasSystems vId st = any (isPrefixOf (shipVar vId "")) (Map.keys (variables (save st)))

-- | Systems of a specific vehicle, if it declares any.
shipSystemsFor :: VehicleID -> GameState -> Maybe ShipSystems
shipSystemsFor vId st
    | not (shipHasSystems vId st) = Nothing
    | otherwise = Just ShipSystems
        { ssShipId  = vId
        , ssName    = maybe vId vehicleName (Map.lookup vId (vehicleDefs (world st)))
        , ssPower   = shipSystem vId "power" st
        , ssWeapons = shipSystem vId "weapons" st
        , ssShields = shipSystem vId "shields" st
        , ssHull    = shipSystem vId "hull" st
        }

-- | The player's ship among the actors (at most one, and only while aboard).
firstShip :: [CombatActor] -> GameState -> Maybe ShipSystems
firstShip actors st = listToMaybe
    [ s | ShipActor vId <- actors, Just s <- [shipSystemsFor vId st] ]

-- | The ship's shot: `weapons` damage against the target, costing one unit of
--   `power` per volley. Without power the guns stay silent.
shipStrike :: NPCID -> ShipSystems -> ([Effect], [String], Int)
shipStrike nid ship = case ssWeapons ship of
    Nothing -> ([], [], 0)
    Just w
        | maybe True (>= 1) (ssPower ship) ->
            let powerEffects = [ SetValue (VRVariable (shipVar (ssShipId ship) "power"))
                                   (EVInt (max 0 (p - 1)))
                               | Just p <- [ssPower ship] ]
            in ( powerEffects ++ [ ModifyValue (VRProperty nid "hp") (-w) ]
               , [ ssName ship ++ " fires for " ++ show w ++ "." ]
               , w )
        | otherwise ->
            ([], [ssName ship ++ " has no power for its weapons."], 0)

-- | Where the target's return fire lands. Shields absorb first, the rest goes
--   into the hull; only a ship without shields *and* hull leaves the player
--   exposed (the 7f behaviour). Returns the effects, the damage that reaches
--   the player, and the messages.
shipAbsorb :: ShipSystems -> Int -> ([Effect], Int, [String])
shipAbsorb ship dmg
    | dmg <= 0 = ([], 0, [])
    | otherwise = case (ssShields ship, ssHull ship) of
        (Just s, Just h) ->
            let absorbed = min dmg s
                spill = dmg - absorbed
                effects = [ SetValue (VRVariable (shipVar (ssShipId ship) "shields")) (EVInt (max 0 (s - absorbed))) ]
                          ++ [ SetValue (VRVariable (shipVar (ssShipId ship) "hull")) (EVInt (max 0 (h - spill))) | spill > 0 ]
                msgs = [ ssName ship ++ ": shields absorb " ++ show absorbed ++ "."
                       | absorbed > 0 ]
                       ++ [ "The shields are down — the hull takes " ++ show spill ++ "." | spill > 0 ]
            in (effects, 0, msgs)
        (Just s, Nothing) ->
            let absorbed = min dmg s
                spill = dmg - absorbed
                effects = [ SetValue (VRVariable (shipVar (ssShipId ship) "shields")) (EVInt (max 0 (s - absorbed))) ]
                msgs = [ ssName ship ++ ": shields absorb " ++ show absorbed ++ "." | absorbed > 0 ]
                       ++ [ ssName ship ++ " has no hull plating — " ++ show spill ++ " hits you." | spill > 0 ]
            in (effects, spill, msgs)
        (Nothing, Just h) ->
            ( [ SetValue (VRVariable (shipVar (ssShipId ship) "hull")) (EVInt (max 0 (h - dmg))) ]
            , 0
            , [ ssName ship ++ ": the hull takes " ++ show dmg ++ "." ] )
        (Nothing, Nothing) -> ([], dmg, [])

-- | Companions that strike alongside the player: alive, present where the
--   target stands, and not the target itself. Returns (npc id, def, damage).
companionHits :: NPCID -> Location -> Int -> [CombatActor] -> GameState -> [(NPCID, NPCDef, Int)]
companionHits targetId targetLoc targetDefense actors st =
    [ (cid, cnpc, max 1 (npcAttackBase cnpc - targetDefense))
    | CompanionActor cid <- actors
    , cid /= targetId
    , Just cns <- [Map.lookup cid (npcStates (save st))]
    , npcStatus cns /= "dead"
    , npcLocation cns == targetLoc
    , Just cnpc <- [Map.lookup cid (npcDefs (world st))]
    ]
