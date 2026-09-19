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
    , targetShipSystems
    ) where

import Types
import Game (effectiveAttack, effectiveDefense, getVariable, getVehicleState,
            combatRound, combatRoundKey, combatEngagedKey, combatActionKey,
            combatInitiativePlayerKey, combatInitiativeKey, combatAbilityKey,
            hasCondition)
import qualified Data.Map.Strict as Map
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe, fromMaybe)

-- | Who attacks. Phase 7f only ever fires `PlayerActor`; 7g adds companions,
--   7h the player's own ship (when the vehicle declares `systems:`).
data CombatActor
    = PlayerActor
    | CompanionActor String
    | ShipActor VehicleID
    deriving (Show, Eq)

-- | What is attacked. Carries the NPC or Vehicle id plus the display name the player
--   typed, so messages stay identical to the pre-7f parser output.
data CombatTarget
    = TargetNPC String String       -- ^ npc id, display label (what the player typed)
    | TargetShip VehicleID String   -- ^ vehicle id, display label
    deriving (Show, Eq)

-- | Resolve one attack command against the authored combat profile.
--   Returns (effects, messages): the effects are applied by the caller
--   through the single outcome interpreter; the messages are added to the
--   command output. No state is mutated here.
--
--   The `CombatAction` parameter selects the player's action within a round.
--   `off`, `narrative` and `classic` profiles ignore it (single-shot
--   resolution); `tactical` dispatches on it.
resolveCombat :: CombatProfile -> [CombatActor] -> CombatTarget -> CombatAction
              -> GameState -> ([Effect], [String])
resolveCombat profile actors target action st = case profile of
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
    -- tactical (Phase 7f-3, A2): one action = one round. The enemy reacts
    --   via an on: turn trigger, not in this function.
    CombatTactical tc -> resolveTactical tc actors target action st
  where
    label (TargetNPC _ disp) = disp
    label (TargetShip _ disp) = disp

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
resolveNarrative nc _ (TargetShip vid disp) st =
    case shipSystemsFor vid st of
        Nothing -> ([], ["You can't attack the " ++ disp ++ "."])
        Just targetShip ->
            let targetDef = fromMaybe 0 (ssShields targetShip)
                win = effectiveAttack st >= targetDef + ncDifficulty nc
            in if win
               then ([ncOnWin nc], ["You win the fight against the " ++ disp ++ "! Your attack lands cleanly."])
               else ([ncOnLose nc], ["You lose the fight against the " ++ disp ++ ". Your attack is turned aside."])

-- ---------------------------------------------------------------------------
-- Tactical (Phase 7f-3, step A2)
-- ---------------------------------------------------------------------------

-- | Build initiative effects when tcInitiative is BySpeed (Phase 7f-3, step A3).
initiativeEffects :: TacticalCombat -> NPCID -> GameState -> [Effect]
initiativeEffects tc nid st = initiativeEffectsTarget tc (TargetNPC nid "") st

initiativeEffectsTarget :: TacticalCombat -> CombatTarget -> GameState -> [Effect]
initiativeEffectsTarget tc target st =
    case tcInitiative tc of
        BySpeed ->
            let speedAttr = tcSpeedAttribute tc
                playerSpd = fromMaybe 0 (Map.lookup speedAttr (playerSkills (player (save st))))
                (targetKey, targetSpd) = case target of
                    TargetNPC nid _ ->
                        let spd = case Map.lookup nid (npcStates (save st)) of
                                Nothing -> 0
                                Just ns -> fromMaybe 0 (Map.lookup speedAttr (npcProps ns))
                        in (combatInitiativeKey nid, spd)
                    TargetShip vid _ ->
                        let spd = fromMaybe 0 (shipSystem vid speedAttr st)
                        in (combatInitiativeKey vid, spd)
            in [ SetValue (VRVariable combatInitiativePlayerKey) (EVInt playerSpd)
               , SetValue (VRVariable targetKey) (EVInt targetSpd) ]
        _ -> []

-- | The display label of a combat target (what the player typed).
targetLabel :: CombatTarget -> String
targetLabel (TargetNPC _ disp)  = disp
targetLabel (TargetShip _ disp) = disp

-- | Round bookkeeping for a tactical action: which round, that the fight is
--   running, and what the player did. `extra` is inserted before the initiative
--   effects so each action keeps its authored effect order.
tacticalStateEffects :: TacticalCombat -> CombatTarget -> Int -> String -> [Effect]
                     -> GameState -> [Effect]
tacticalStateEffects tc target round' actionName extra st =
    [ SetValue (VRVariable combatRoundKey)   (EVInt round')
    , SetValue (VRVariable combatEngagedKey) (EVInt 1)
    , SetValue (VRVariable combatActionKey)  (EVString actionName) ]
    ++ extra
    ++ initiativeEffectsTarget tc target st

-- | End-of-fight bookkeeping: the fight is over, the round counter resets.
--   Deliberately without `combat.action` — a kill follows a state block that
--   already set it.
tacticalResetEffects :: [Effect]
tacticalResetEffects =
    [ SetValue (VRVariable combatRoundKey)   (EVInt 0)
    , SetValue (VRVariable combatEngagedKey) (EVInt 0) ]

-- | End-of-fight bookkeeping for an action that never set a state block
--   (a successful flee): the action name is part of it.
tacticalEndEffects :: String -> [Effect]
tacticalEndEffects actionName =
    tacticalResetEffects ++ [ SetValue (VRVariable combatActionKey) (EVString actionName) ]

-- | Shared implementation of the tactical branches that do not depend on the
--   target's *kind* — only on its label and on the initiative key. Keeping one
--   body per action (instead of one per action × target) is what makes
--   `TargetShip` a data extension rather than a second implementation.
tacticalDefend :: TacticalCombat -> CombatTarget -> GameState -> ([Effect], [String])
tacticalDefend tc target st =
    let round' = combatRound st + 1
    in ( tacticalStateEffects tc target round' "defend" [] st
       , ["Round " ++ show round' ++ ": You brace yourself against the "
          ++ targetLabel target ++ "."] )

tacticalFlee :: TacticalCombat -> CombatTarget -> GameState -> ([Effect], [String])
tacticalFlee tc target st =
    let round' = combatRound st + 1
    in if tcFleeAllowed tc
       then ( tacticalEndEffects "flee"
            , ["Round " ++ show round' ++ ": You flee from the "
               ++ targetLabel target ++ "!"] )
       else ( tacticalStateEffects tc target round' "flee" [] st
            , ["You can't flee from the " ++ targetLabel target ++ "!"] )

tacticalAbility :: TacticalCombat -> CombatTarget -> String -> GameState -> ([Effect], [String])
tacticalAbility tc target abId st =
    case Map.lookup abId (abilities (world st)) of
        Nothing -> ([], ["Unknown ability '" ++ abId ++ "'."])
        Just pa ->
            if hasCondition ("cooldown_" ++ abId) st
            then ([], ["Ability is on cooldown."])
            else
                let costVar = paCostVar pa
                    cost = paCost pa
                    curVal = if null costVar
                             then cost
                             else case getVariable costVar st of
                                 Just (VVInt n) -> n
                                 _              -> 0
                in if curVal < cost
                   then ([], ["Not enough resources."])
                   else
                       let round' = combatRound st + 1
                           abilityMark = [ SetValue (VRVariable combatAbilityKey) (EVString abId) ]
                           costEffects =
                               if not (null costVar) && cost > 0
                               then [ ModifyValue (VRVariable costVar) (-cost) ]
                               else []
                           cooldownEffects =
                               if paCooldown pa > 0
                               then [ ApplyCondition ("cooldown_" ++ abId) (paCooldown pa) Nothing Nothing ]
                               else []
                       in ( tacticalStateEffects tc target round' "ability" abilityMark st
                              ++ costEffects ++ cooldownEffects ++ paEffects pa
                          , ["Round " ++ show round' ++ ": You use " ++ paName pa ++ "!"] )

-- | Tactical: one player action per round, enemy reacts via `on: turn`.
--   The resolver:
--   (1) increments `combat.round`
--   (2) sets `combat.engaged = 1`
--   (3) sets `combat.action` to the action name
--   (4) computes damage effects (for CAAttack) or none (CADefend)
--   (5) for CAFlee: clears combat.engaged and combat.round if allowed
--   (6) for CAAbility: validates cooldown and resource cost, executes effects
--
--   The enemy's retaliation is an `on: turn` trigger authored in the
--   adventure YAML — not a second code path.
resolveTactical :: TacticalCombat -> [CombatActor] -> CombatTarget
               -> CombatAction -> GameState -> ([Effect], [String])

-- Attack: player strikes, no retaliation (the trigger does that).
resolveTactical tc _actors (TargetNPC nid disp) CAAttack st =
    case Map.lookup nid (npcDefs (world st)) of
        Nothing  -> ([], ["You can't attack the " ++ disp ++ "."])
        Just npc ->
            case Map.lookup nid (npcStates (save st)) of
                Nothing -> ([], ["You can't attack the " ++ disp ++ "."])
                Just ns -> case npcHealth ns of
                    Nothing -> ([], ["You can't attack the " ++ disp ++ "."])
                    Just hp ->
                        let round'    = combatRound st + 1
                            playerDmg = max 1 (effectiveAttack st - npcDefenseBase npc)
                            stateEffects =
                                [ SetValue (VRVariable combatRoundKey)   (EVInt round')
                                , SetValue (VRVariable combatEngagedKey) (EVInt 1)
                                , SetValue (VRVariable combatActionKey)  (EVString "attack") ]
                                ++ initiativeEffects tc nid st
                            dmgEffects = [ ModifyValue (VRActorProp (ActorNPC nid) PHealth) (-playerDmg) ]
                        in if hp - playerDmg <= 0
                           then ( stateEffects ++ dmgEffects
                                    ++ [ SetValue (VRVariable combatEngagedKey) (EVInt 0)
                                       , SetValue (VRVariable combatRoundKey)   (EVInt 0) ]
                                , ["Round " ++ show round' ++ ": You attack the "
                                   ++ disp ++ " and kill it!"] )
                           else ( stateEffects ++ dmgEffects
                                , ["Round " ++ show round' ++ ": You hit the "
                                   ++ disp ++ " for " ++ show playerDmg ++ "."] )

resolveTactical tc _actors (TargetShip vid disp) CAAttack st =
    case shipSystemsFor vid st of
        Nothing -> ([], ["You can't attack the " ++ disp ++ "."])
        Just targetShip ->
            case ssHull targetShip of
                Just h | h <= 0 -> ([], ["The " ++ disp ++ " is already destroyed."])
                _ ->
                    let round'    = combatRound st + 1
                        playerDmg = max 1 (effectiveAttack st)
                        stateEffects =
                            [ SetValue (VRVariable combatRoundKey)   (EVInt round')
                            , SetValue (VRVariable combatEngagedKey) (EVInt 1)
                            , SetValue (VRVariable combatActionKey)  (EVString "attack") ]
                            ++ initiativeEffectsTarget tc (TargetShip vid disp) st
                        (targetAbsorbEffects, _, targetAbsorbMsgs) = shipAbsorb targetShip playerDmg
                        targetHullAfter = case ssHull targetShip of
                            Just h  ->
                                let s = fromMaybe 0 (ssShields targetShip)
                                in h - max 0 (playerDmg - s)
                            Nothing -> 0
                    in if targetHullAfter <= 0
                       then ( stateEffects ++ targetAbsorbEffects
                                ++ [ SetValue (VRVariable combatEngagedKey) (EVInt 0)
                                   , SetValue (VRVariable combatRoundKey)   (EVInt 0) ]
                            , [ "Round " ++ show round' ++ ": You attack the " ++ disp ++ " and destroy it!" ] )
                       else ( stateEffects ++ targetAbsorbEffects
                            , ("Round " ++ show round' ++ ": You attack the " ++ disp ++ ".") : targetAbsorbMsgs )

-- Defend: no damage, marker for the enemy trigger.
resolveTactical tc _actors (TargetNPC nid disp) CADefend st =
    tacticalDefend tc (TargetNPC nid disp) st

resolveTactical tc _actors (TargetShip vid disp) CADefend st =
    tacticalDefend tc (TargetShip vid disp) st

-- Flee: end the fight if allowed.
resolveTactical tc _actors (TargetNPC nid disp) CAFlee st =
    tacticalFlee tc (TargetNPC nid disp) st

resolveTactical tc _actors (TargetShip vid disp) CAFlee st =
    tacticalFlee tc (TargetShip vid disp) st

-- Ability: player uses an ability (Phase 7f-3, step A3).
resolveTactical tc _actors (TargetNPC nid disp) (CAAbility abId) st =
    tacticalAbility tc (TargetNPC nid disp) abId st

resolveTactical tc _actors (TargetShip vid disp) (CAAbility abId) st =
    tacticalAbility tc (TargetShip vid disp) abId st

-- CAUseItem: future steps.
resolveTactical _tc _actors _target _ _st =
    ([], ["You can't do that in combat yet."])

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
                            playerEffects = [ ModifyValue (VRActorProp (ActorNPC nid) PHealth) (-playerDmg) ]
                            mShip = firstShip actors st
                        in if hp - playerDmg <= 0
                           then ( playerEffects
                                , [ "You attack the " ++ disp ++ " and kill it!" ] )
                           else
                               let allies = companionHits nid (npcLocation ns) (npcDefenseBase npc) actors st
                                   allyEffects = [ ModifyValue (VRActorProp (ActorNPC nid) PHealth) (-d)
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

resolveClassic actors (TargetShip vid disp) st =
    case shipSystemsFor vid st of
        Nothing -> ([], [cannotAttack])
        Just targetShip ->
            case ssHull targetShip of
                Just h | h <= 0 -> ([], ["The " ++ disp ++ " is already destroyed."])
                _ ->
                    let playerDmg = max 1 (effectiveAttack st)
                        mPlayerShip = firstShip actors st
                        (shipPowerEffs, shipFireMsgs, shipDmg) = case mPlayerShip of
                            Nothing   -> ([], [], 0)
                            Just ship -> shipVolley ship
                        targetLoc = vsCurrentStop (getVehicleState vid st)
                        allies = companionHitsShip targetLoc actors st
                        allyMsgs = [ npcName allyNpc ++ " strikes for " ++ show d ++ "."
                                   | (_, allyNpc, d) <- allies ]
                        allyTotal = sum [ d | (_, _, d) <- allies ]
                        totalDmg = playerDmg + allyTotal + shipDmg
                        (targetAbsorbEffects, _, targetAbsorbMsgs) = shipAbsorb targetShip totalDmg
                        targetHullAfter = case ssHull targetShip of
                            Just h  ->
                                let s = fromMaybe 0 (ssShields targetShip)
                                in h - max 0 (totalDmg - s)
                            Nothing -> 0
                        msgsBeforeHit = ("You attack the " ++ disp ++ ".") : (shipFireMsgs ++ allyMsgs ++ targetAbsorbMsgs)
                    in if targetHullAfter <= 0
                       then ( shipPowerEffs ++ targetAbsorbEffects
                            , ("You attack the " ++ disp ++ " and destroy it!") : (shipFireMsgs ++ allyMsgs ++ targetAbsorbMsgs) )
                       else
                           let (enemyPowerEffs, enemyFireMsgs, enemyDmg) = shipVolley targetShip
                           in if enemyDmg <= 0
                              then ( shipPowerEffs ++ targetAbsorbEffects
                                   , msgsBeforeHit ++ enemyFireMsgs )
                              else
                                  let (playerAbsorbEffects, playerTaken, playerAbsorbMsgs) = case mPlayerShip of
                                          Nothing   -> ([], enemyDmg, [])
                                          Just ship -> shipAbsorb ship enemyDmg
                                      playerTakenEffects = [ ModifyValue VRPlayerHealth (-playerTaken) | playerTaken > 0 ]
                                      playerHpAfter = playerHealth (player (save st)) - playerTaken
                                      withRetaliation = shipPowerEffs ++ targetAbsorbEffects ++ enemyPowerEffs ++ playerAbsorbEffects ++ playerTakenEffects
                                      allMsgs = msgsBeforeHit ++ enemyFireMsgs ++ playerAbsorbMsgs
                                  in if playerHpAfter <= 0
                                     then ( withRetaliation
                                          , ("The " ++ disp ++ " strikes back and kills you!") : allMsgs )
                                     else ( withRetaliation
                                          , allMsgs )
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

-- | Ship systems of the combat target, if it is a ship and declares systems.
targetShipSystems :: CombatTarget -> GameState -> Maybe ShipSystems
targetShipSystems (TargetShip vid _) st = shipSystemsFor vid st
targetShipSystems (TargetNPC _ _) _     = Nothing

-- | The player's ship among the actors (at most one, and only while aboard).
firstShip :: [CombatActor] -> GameState -> Maybe ShipSystems
firstShip actors st = listToMaybe
    [ s | ShipActor vId <- actors, Just s <- [shipSystemsFor vId st] ]

-- | The ship's shot: power consumption, fire message, and weapons damage.
--   Without power the guns stay silent.
shipVolley :: ShipSystems -> ([Effect], [String], Int)
shipVolley ship = case ssWeapons ship of
    Nothing -> ([], [], 0)
    Just w
        | maybe True (>= 1) (ssPower ship) ->
            let powerEffects = [ SetValue (VRVariable (shipVar (ssShipId ship) "power"))
                                   (EVInt (max 0 (p - 1)))
                               | Just p <- [ssPower ship] ]
            in ( powerEffects
               , [ ssName ship ++ " fires for " ++ show w ++ "." ]
               , w )
        | otherwise ->
            ([], [ssName ship ++ " has no power for its weapons."], 0)

-- | The ship's shot against an NPC target.
shipStrike :: NPCID -> ShipSystems -> ([Effect], [String], Int)
shipStrike nid ship =
    let (powerEffects, msgs, w) = shipVolley ship
        dmgEffects = [ ModifyValue (VRActorProp (ActorNPC nid) PHealth) (-w) | w > 0 ]
    in (powerEffects ++ dmgEffects, msgs, w)

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

-- | Companions that strike alongside the player against a ship: alive and aboard/present.
companionHitsShip :: RoomID -> [CombatActor] -> GameState -> [(NPCID, NPCDef, Int)]
companionHitsShip _ actors st =
    [ (cid, cnpc, max 1 (npcAttackBase cnpc))
    | CompanionActor cid <- actors
    , Just cns <- [Map.lookup cid (npcStates (save st))]
    , npcStatus cns /= "dead"
    , Just cnpc <- [Map.lookup cid (npcDefs (world st))]
    ]
