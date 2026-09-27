{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Card games and deckbuilder data types (Phase 2A)
module Types.Cards
    ( CardType (..)
    , CardTarget (..)
    , DeckDestination (..)
    , Card (..)
    , DeckState (..)
    , defaultDeckState
    ) where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import {-# SOURCE #-} Types (CardID, Effect)

-- | Card classification for color coding, filtering and gameplay roles.
data CardType
    = CardAttack       -- ^ Attack card (red)
    | CardSkill        -- ^ Skill / Defense card (blue)
    | CardPower        -- ^ Power card (gold)
    | CardCurse        -- ^ Curse card (purple)
    | CardStatus       -- ^ Temporary status card (grey)
    deriving (Show, Eq, Generic)

instance ToJSON CardType where
    toJSON CardAttack = "attack"
    toJSON CardSkill  = "skill"
    toJSON CardPower  = "power"
    toJSON CardCurse  = "curse"
    toJSON CardStatus = "status"

instance FromJSON CardType where
    parseJSON = withText "CardType" $ \t -> case map toLower (T.unpack t) of
        "attack" -> pure CardAttack
        "skill"  -> pure CardSkill
        "power"  -> pure CardPower
        "curse"  -> pure CardCurse
        "status" -> pure CardStatus
        other    -> fail ("Unknown card type: " ++ other)

-- | Targeting requirement for playing a card.
data CardTarget
    = TargetSelf           -- ^ Affects the player only
    | TargetSingleEnemy    -- ^ Requires a living targeted enemy in the room
    | TargetAllEnemies     -- ^ Affects all enemies in the room automatically
    | TargetNone           -- ^ No target required
    deriving (Show, Eq, Generic)

instance ToJSON CardTarget where
    toJSON TargetSelf        = "self"
    toJSON TargetSingleEnemy = "single_enemy"
    toJSON TargetAllEnemies  = "all_enemies"
    toJSON TargetNone        = "none"

instance FromJSON CardTarget where
    parseJSON = withText "CardTarget" $ \t -> case map toLower (T.unpack t) of
        "self"         -> pure TargetSelf
        "single_enemy" -> pure TargetSingleEnemy
        "single"       -> pure TargetSingleEnemy
        "all_enemies"  -> pure TargetAllEnemies
        "all"          -> pure TargetAllEnemies
        "none"         -> pure TargetNone
        other          -> fail ("Unknown card target: " ++ other)

-- | Target pile when adding a card to the deck.
data DeckDestination
    = DestDraw     -- ^ Top of draw pile
    | DestDiscard  -- ^ Discard pile
    | DestHand     -- ^ Into active hand
    deriving (Show, Eq, Generic)

instance ToJSON DeckDestination where
    toJSON DestDraw    = "draw"
    toJSON DestDiscard = "discard"
    toJSON DestHand    = "hand"

instance FromJSON DeckDestination where
    parseJSON = withText "DeckDestination" $ \t -> case map toLower (T.unpack t) of
        "draw"    -> pure DestDraw
        "discard" -> pure DestDiscard
        "hand"    -> pure DestHand
        other     -> fail ("Unknown deck destination: " ++ other)

-- | Static card definition in GameWorld.
data Card = Card
    { cardId          :: CardID
    , cardName        :: String
    , cardCost        :: Map.Map String Int       -- ^ Resource costs, e.g. { "energy": 1 }
    , cardType        :: CardType
    , cardDescription :: String
    , cardTarget      :: CardTarget
    , cardExhaust     :: Bool                     -- ^ Removed to exhaust pile on play
    , cardEffects     :: [Effect]                 -- ^ Pure Effect DSL
    } deriving (Show, Eq, Generic)

instance ToJSON Card where
    toJSON c = object
        [ "cardId"          .= cardId c
        , "cardName"        .= cardName c
        , "cardCost"        .= cardCost c
        , "cardType"        .= cardType c
        , "cardDescription" .= cardDescription c
        , "cardTarget"      .= cardTarget c
        , "cardExhaust"     .= cardExhaust c
        , "cardEffects"     .= cardEffects c
        ]

instance FromJSON Card where
    parseJSON = withObject "Card" $ \o -> Card
        <$> (o .:? "cardId" >>= maybe (o .:? "id" .!= "") pure)
        <*> (o .:? "cardName" >>= maybe (o .: "name") pure)
        <*> (o .:? "cardCost" >>= maybe (o .:? "cost" .!= Map.empty) pure)
        <*> (o .:? "cardType" >>= maybe (o .:? "type" .!= CardSkill) pure)
        <*> (o .:? "cardDescription" >>= maybe (o .:? "description" >>= maybe (o .:? "desc" .!= "") pure) pure)
        <*> (o .:? "cardTarget" >>= maybe (o .:? "target" .!= TargetNone) pure)
        <*> (o .:? "cardExhaust" >>= maybe (o .:? "exhaust" .!= False) pure)
        <*> (o .:? "cardEffects" >>= maybe (o .:? "effects" >>= maybe (o .:? "outcomes" .!= []) pure) pure)

-- | Runtime state of card decks in SaveState.
data DeckState = DeckState
    { drawPile    :: [CardID]                 -- ^ Ordered draw pile (head = next to draw)
    , hand        :: [CardID]                 -- ^ Current hand
    , discardPile :: [CardID]                 -- ^ Discard pile
    , exhaustPile :: [CardID]                 -- ^ Exhausted cards removed for combat
    , maxHandSize :: Int                     -- ^ Max cards allowed in hand (0 = unlimited)
    } deriving (Show, Eq, Generic)

defaultDeckState :: DeckState
defaultDeckState = DeckState
    { drawPile    = []
    , hand        = []
    , discardPile = []
    , exhaustPile = []
    , maxHandSize = 0
    }

instance ToJSON DeckState where
    toJSON ds = object
        [ "drawPile"    .= drawPile ds
        , "hand"        .= hand ds
        , "discardPile" .= discardPile ds
        , "exhaustPile" .= exhaustPile ds
        , "maxHandSize" .= maxHandSize ds
        ]

instance FromJSON DeckState where
    parseJSON = withObject "DeckState" $ \o -> DeckState
        <$> o .:? "drawPile"    .!= []
        <*> o .:? "hand"        .!= []
        <*> o .:? "discardPile" .!= []
        <*> o .:? "exhaustPile" .!= []
        <*> o .:? "maxHandSize" .!= 0
