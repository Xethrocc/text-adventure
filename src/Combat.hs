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
    , resolveCombat
    ) where

import Types
import Game (effectiveAttack, effectiveDefense, isPlayerDead)
import qualified Data.Map.Strict as Map

-- | Who attacks. Phase 7f only ever fires `PlayerActor`; further actors
--   (party members, 7g) extend this list without changing the signature —
--   that is why the resolver already takes a list.
data CombatActor
    = PlayerActor
    | CompanionActor String
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
resolveCombat :: CombatProfile -> [CombatActor] -> CombatTarget -> GameState -> ([Effect], [String])
resolveCombat profile actors target st = case profile of
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
--   the player's blow already killed it. The target's counterattack still hits
--   the player, and companions never strike themselves.
--   The damage math is fully deterministic, so the outcome (kill / survive,
--   player death / survival) is decided here; the returned effects are plain
--   HP modifications — NPC death (killNPC) and player death (endGame) are
--   handled automatically by `modifyNPCHealth` / `ModifyValue VRPlayerHealth`
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
                                   effects = playerEffects ++ allyEffects
                               in if hp - playerDmg - allyTotal <= 0
                                  then ( effects
                                       , ("You attack the " ++ disp ++ " and kill it!") : allyMsgs )
                                  else
                                      let npcDmg = max 0 (npcAttackBase npc - effectiveDefense st)
                                          playerHpAfter = playerHealth (player (save st)) - npcDmg
                                          withRetaliation = effects ++ [ ModifyValue VRPlayerHealth (-npcDmg) ]
                                      in if playerHpAfter <= 0
                                         then ( withRetaliation
                                              , ("The " ++ disp ++ " strikes back and kills you!") : allyMsgs )
                                         else ( withRetaliation
                                              , ("You hit for " ++ show playerDmg
                                                 ++ ", it hits you for " ++ show npcDmg ++ ".") : allyMsgs )
  where
    cannotAttack = "You can't attack the " ++ disp ++ "."

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
