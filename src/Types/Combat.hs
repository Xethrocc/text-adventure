{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Combat systems, policies, tactical combat and ability definitions
module Types.Combat
    ( PlayerAbility (..)
    , CombatProfile (..)
    , CombatScreen (..)
    , NarrativeCombat (..)
    , TacticalCombat (..)
    , InitiativeRule (..)
    , CombatAction (..)
    ) where

import Control.Applicative ((<|>))
import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.Text as T

import {-# SOURCE #-} Types (Effect, noopEffect, AsciiArt, emptyAscii, isEmptyAscii, ItemID)

-- | Player ability definition for tactical combat (Phase 7f-3, step A3).
data PlayerAbility = PlayerAbility
    { paId       :: String
    , paName     :: String
    , paCostVar  :: String   -- ^ variable to deduct cost from, e.g. "player.mana"
    , paCost     :: Int      -- ^ amount to deduct
    , paCooldown :: Int      -- ^ cooldown duration in turns
    , paEffects  :: [Effect] -- ^ effects executed on use
    } deriving (Show, Eq, Generic)

instance ToJSON PlayerAbility where
    toJSON pa = object
        [ "id"        .= paId pa
        , "name"      .= paName pa
        , "cost_var"  .= paCostVar pa
        , "cost"      .= paCost pa
        , "cooldown"  .= paCooldown pa
        , "effects"   .= paEffects pa
        ]

instance FromJSON PlayerAbility where
    parseJSON = withObject "PlayerAbility" $ \o -> PlayerAbility
        <$> (o .: "id" <|> o .: "paId")
        <*> (o .: "name" <|> o .: "paName")
        <*> (o .:? "cost_var" >>= maybe (o .:? "paCostVar" .!= "") pure)
        <*> (o .:? "cost" >>= maybe (o .:? "paCost" .!= 0) pure)
        <*> (o .:? "cooldown" >>= maybe (o .:? "paCooldown" .!= 0) pure)
        <*> (o .:? "effects" >>= maybe (o .:? "paEffects" .!= []) pure)

-- | Combat policy, chosen by authored data (Phase 7f). `CombatClassic` is
--   the exact pre-7f behaviour and the default when no `combat:` block is
--   present (bit-identical regression gate).
data CombatProfile
    = CombatOff (Maybe String)                 -- ^ attack refused; optional custom message
    | CombatNarrative NarrativeCombat          -- ^ opposed roll -> on_win/on_lose effects
    | CombatClassic (Maybe CombatScreen)       -- ^ today's behaviour: attack vs defense, retaliation
    | CombatTactical TacticalCombat            -- ^ round-based: one action per round (Phase 7f-3)
    deriving (Show, Eq, Generic)

-- | An authored combat screen for the `classic` profile: the layout the
--   original TheFog printed before every strike (rules, art, "You are fighting
--   a X", scene line, attack/defense, both HP bars with the flee hint).
--
--   Absent (or `screen` omitted) means no screen at all — the classic output
--   stays bit-identical to the pre-screen behaviour.
data CombatScreen = CombatScreen
    { csArt      :: AsciiArt        -- ^ optional art above the status block (empty = none)
    , csBarWidth :: Int             -- ^ width of both HP bars in cells (default 10)
    , csScene    :: Maybe String    -- ^ scene line; `Nothing` = the original wording, `Just ""` hides it
    , csFooter   :: Maybe String    -- ^ flee hint; `Nothing` = the original wording, `Just ""` hides it
    } deriving (Show, Eq, Generic)

instance ToJSON CombatScreen where
    toJSON cs = object $
        [ "art" .= csArt cs | not (isEmptyAscii (csArt cs)) ]
        ++ [ "bar_width" .= csBarWidth cs
           , "scene"     .= csScene cs
           , "footer"    .= csFooter cs ]

instance FromJSON CombatScreen where
    parseJSON = withObject "CombatScreen" $ \o -> CombatScreen
        <$> o .:? "art"       .!= emptyAscii
        <*> o .:? "bar_width" .!= 10
        <*> o .:? "scene"
        <*> o .:? "footer"

-- | Narrative combat: the player's effective attack is rolled against the
--   target's defense + a difficulty offset. No HP attrition — the on_win /
--   on_lose effects decide everything.
data NarrativeCombat = NarrativeCombat
    { ncDifficulty :: Int
    , ncOnWin      :: Effect
    , ncOnLose     :: Effect
    } deriving (Show, Eq, Generic)

-- | Tactical combat configuration (Phase 7f-3, step A2/A3).
--
--   One player action = one round. The enemy reacts via an `on: turn` rule
--   gated on `combat.engaged >= 1` — no second interpreter.
data TacticalCombat = TacticalCombat
    { tcInitiative     :: InitiativeRule  -- ^ who strikes first
    , tcFleeAllowed    :: Bool            -- ^ can the player flee?
    , tcMaxRounds      :: Int             -- ^ hard limit on rounds (safety net)
    , tcSpeedAttribute :: String          -- ^ skill/prop name for BySpeed initiative (default "speed")
    } deriving (Show, Eq, Generic)

instance ToJSON TacticalCombat where
    toJSON tc = object
        [ "initiative"      .= tcInitiative tc
        , "flee_allowed"    .= tcFleeAllowed tc
        , "max_rounds"      .= tcMaxRounds tc
        , "speed_attribute" .= tcSpeedAttribute tc
        ]

instance FromJSON TacticalCombat where
    parseJSON = withObject "TacticalCombat" $ \o -> TacticalCombat
        <$> (o .:? "initiative"      <|> o .:? "tcInitiative")      .!= PlayerFirst
        <*> (o .:? "flee_allowed"    <|> o .:? "tcFleeAllowed")     .!= True
        <*> (o .:? "max_rounds"      <|> o .:? "tcMaxRounds")       .!= 100
        <*> (o .:? "speed_attribute" <|> o .:? "tcSpeedAttribute")  .!= "speed"

-- | Initiative order within a tactical round.
--   `BySpeed` is defined for forward-compatibility (A3) but falls back to
--   `PlayerFirst` at runtime until A3 wires it.
data InitiativeRule = PlayerFirst | EnemyFirst | BySpeed
    deriving (Show, Eq, Generic)

instance ToJSON InitiativeRule

instance FromJSON InitiativeRule where
    parseJSON = withText "InitiativeRule" $ \t -> case T.toLower (T.replace "-" "_" t) of
        "player_first" -> pure PlayerFirst
        "playerfirst"  -> pure PlayerFirst
        "enemy_first"  -> pure EnemyFirst
        "enemyfirst"   -> pure EnemyFirst
        "npc_first"    -> pure EnemyFirst
        "npcfirst"     -> pure EnemyFirst
        "by_speed"     -> pure BySpeed
        "byspeed"      -> pure BySpeed
        _              -> fail ("unknown initiative rule '" ++ T.unpack t ++ "'")

-- | What the player does in one combat round.
--
--   Phase 7f-3 (`tactical`): `CAAttack` deals damage, `CADefend` sets
--   `combat.action` for the enemy trigger, `CAFlee` ends the fight if allowed,
--   `CAAbility` spends resources and cooldown and applies the ability effects
--   (step A3).
--
--   `CAUseItem` is a **reserved placeholder, not wired**: the parser never
--   produces it (there is no `use <item>` combat verb), `resolveTactical` answers
--   it with the "can't do that in combat yet" fallback, and nothing constructs it
--   — so it is deliberately kept instead of half-implemented, and a test pins the
--   fallback so the gap stays visible.
--   See `plan-7f3-tactical-7h2-shipduell.md`.
data CombatAction
    = CAAttack
    | CADefend
    | CAFlee
    | CAUseItem ItemID
    | CAAbility String
    deriving (Show, Eq, Generic)

instance ToJSON CombatProfile where
    toJSON (CombatOff mRefused) = object
        [ "profile" .= ("off" :: String)
        , "attack_refused" .= mRefused ]
    toJSON (CombatNarrative nc) = object
        [ "profile" .= ("narrative" :: String)
        , "difficulty" .= ncDifficulty nc
        , "on_win"  .= ncOnWin nc
        , "on_lose" .= ncOnLose nc ]
    toJSON (CombatClassic mScreen) = object $
        [ "profile" .= ("classic" :: String) ]
        ++ [ "screen" .= s | Just s <- [mScreen] ]
    toJSON (CombatTactical tc) = object
        [ "profile"         .= ("tactical" :: String)
        , "initiative"      .= tcInitiative tc
        , "flee_allowed"    .= tcFleeAllowed tc
        , "max_rounds"      .= tcMaxRounds tc
        , "speed_attribute" .= tcSpeedAttribute tc ]

instance FromJSON CombatProfile where
    parseJSON v = withObject "CombatProfile" (\o -> do
        prof <- o .: "profile"
        case prof of
            "off"       -> CombatOff <$> o .:? "attack_refused"
            "narrative" -> CombatNarrative <$>
                (NarrativeCombat
                    <$> o .:? "difficulty" .!= 0
                    <*> o .:? "on_win"  .!= noopEffect
                    <*> o .:? "on_lose" .!= noopEffect)
            "classic"   -> CombatClassic <$> o .:? "screen"
            "tactical"  -> CombatTactical <$>
                (TacticalCombat
                    <$> o .:? "initiative"      .!= PlayerFirst
                    <*> o .:? "flee_allowed"     .!= True
                    <*> o .:? "max_rounds"       .!= 100
                    <*> o .:? "speed_attribute"  .!= "speed")
            other       -> fail ("unknown combat profile '" ++ other ++ "'")) v
