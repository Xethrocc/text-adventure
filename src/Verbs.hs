{-# LANGUAGE OverloadedStrings #-}
-- | Unified verb registry: core verbs + custom verb lookup.
--   Single source of truth for all verb parsing (engine + worldbuilder).
module Verbs
    ( coreVerbDefs
    , verbAliasMap
    , resolveVerb
    ) where

import Types
import qualified Data.Map.Strict as Map
import Data.Char (toLower)

-- | Default verb definitions shipped with the engine — the canonical verb-map
--   verbs that the parser recognises. Each entry maps a canonical name to its
--   input aliases.
coreVerbDefs :: [VerbDef]
coreVerbDefs =
    [ VerbDef "take"    ["pick", "grab", "get"]
    , VerbDef "drop"    ["put"]
    , VerbDef "examine" ["inspect", "look", "read"]
    , VerbDef "use"     ["activate"]
    , VerbDef "talk"    ["speak", "chat"]
    , VerbDef "attack"  ["hit", "kill"]
    , VerbDef "search"  []
    ]

-- | Build an alias → canonical-name lookup table from a map of VerbDefs.
verbAliasMap :: Map.Map String VerbDef -> Map.Map String String
verbAliasMap defs =
    Map.unions
        [ Map.singleton (map toLower (vdName def)) (vdName def)
          `Map.union` Map.fromList [ (map toLower a, vdName def) | a <- vdAliases def ]
        | def <- Map.elems defs ]

-- | Resolve an input word to a Verb.  Checks core verbs first, then custom
--   registry.  Unknown words return Nothing.
resolveVerb :: Map.Map String VerbDef -> String -> Maybe Verb
resolveVerb customDefs w =
    case (map toLower w) `Map.lookup` allAliases of
        Nothing -> Nothing
        Just canonical ->
            case canonicalToVerb canonical of
                Just core -> Just core
                Nothing   -> Just (VCustom canonical)
  where
    -- Merge core + custom defs; custom cannot override core (Map.union)
    allDefs = Map.fromList [(vdName d, d) | d <- coreVerbDefs]
              `Map.union` customDefs
    allAliases = verbAliasMap allDefs

-- | Map canonical core verb names to their Verb constructors.
canonicalToVerb :: String -> Maybe Verb
canonicalToVerb "take"    = Just VTake
canonicalToVerb "drop"    = Just VDrop
canonicalToVerb "examine" = Just VLookAt
canonicalToVerb "use"     = Just VUse
canonicalToVerb "talk"    = Just VTalk
canonicalToVerb "attack"  = Just VAttack
canonicalToVerb "search"  = Just VSearch
canonicalToVerb _         = Nothing