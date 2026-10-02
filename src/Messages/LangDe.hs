-- | Generated from lang/de.json (sha256: aed6ba7a828c96f290a0cfbde2c823fef11a6d9b365e405c10b480f73b1b8eab) by scripts/gen-lang-pack.py.
--   DO NOT EDIT by hand - rerun the generator after changing the JSON.
--
--   Language-pack data (Phase 4.3): message templates keyed like the
--   engine catalog, term translations for enumerable argument values,
--   and input alias tables (verbs / directions / commands /
--   prepositions) mapping alias words to their canonical English token.
module Messages.LangDe
    ( langDe
    ) where

-- | The pack as a flat tuple:
--   (language, messages, terms, verbs, directions, commands, prepositions).
langDe :: (String, [(String, String)], [(String, String)],
           [(String, [String])], [(String, [String])], [(String, [String])], [(String, [String])])
langDe =
    ( "de"
    , -- MESSAGES BEGIN
      [ ("move.ok", "Du gehst nach {dir}.")
      , ("move.door_locked", "Die Tür ist verschlossen.")
      , ("move.no_exit", "In diese Richtung führt kein Weg.")
      , ("move.blocked", "Dort kannst du nicht hindurch.")
      , ("look.void", "Du befindest dich im Nichts. Hier gibt es nichts zu sehen.")
      , ("look.see_nothing", "\nDu siehst nichts Interessantes.")
      , ("look.items", "\nDu siehst: {names}.")
      , ("look.npcs", "\nAußerdem sind hier: {names}.")
      ]
      -- MESSAGES END
    , -- TERMS BEGIN
      []
      -- TERMS END
    , -- VERBS BEGIN
      []
      -- VERBS END
    , -- DIRECTIONS BEGIN
      []
      -- DIRECTIONS END
    , -- COMMANDS BEGIN
      []
      -- COMMANDS END
    , -- PREPOSITIONS BEGIN
      []
      -- PREPOSITIONS END
    )
