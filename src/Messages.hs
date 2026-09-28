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
import Data.List (isPrefixOf, lookup)
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
catalogEntries :: [(MsgId, String)]
catalogEntries =
    [
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