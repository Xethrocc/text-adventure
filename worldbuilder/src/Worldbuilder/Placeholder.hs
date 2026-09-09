-- | Placeholder module for the worldbuilder package.
--
--   The real content lives here from Phase 5 onwards:
--
--     Worldbuilder.Types    - authoring schema (WCharacter, WLocation, ...)
--     Worldbuilder.Parse    - YAML/JSON -> schema
--     Worldbuilder.Validate - consistency checks before compiling
--     Worldbuilder.Compile  - schema -> GameWorld + SaveState
--
--   Until then this module only exists so the cabal.project resolves.
module Worldbuilder.Placeholder (placeholderNote) where

-- | Short notice printed by the worldbuilder CLI until Phase 5 lands.
placeholderNote :: String
placeholderNote =
    unlines
        [ "worldbuilder: not implemented yet (Phase 5)."
        , ""
        , "Planned commands:"
        , "  worldbuilder validate ./adventure/          check for consistency errors"
        , "  worldbuilder compile ./adventure/ -o ./out/ emit world.json + initial-save.json"
        , "  worldbuilder check ./adventure/             print content statistics"
        ]
