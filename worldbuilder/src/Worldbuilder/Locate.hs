-- | Locate the source line of a dotted issue path in the author's YAML file.
--
--   W5, Stufe 1 (decision DW5a): `CompileIssue.ciPath` (e.g. @rooms.cave.ascii@
--   or @items.torch.verb_map@) names the offending field, but the author saw only
--   the compiled diagnostic — no line number, so a non-programmer had to scroll
--   a long YAML file and guess.
--
--   Exact positions are not available at this layer: the worldbuilder decodes
--   through @Data.YAML.Aeson@, whose JSON bridge drops the per-node positions
--   (W5 Stufe 2 — a move to the event parser — would lift them). So this module
--   matches the path against the file text instead. It is a heuristic by design:
--   good enough to point at the right neighbourhood; when a later segment cannot
--   be matched, the line of the deepest segment that *did* match is returned —
--   never a wrong line for the field it names.
--
--   Matching rule: the first segment must be a top-level mapping key; each
--   following segment must be a mapping key whose indentation is strictly deeper
--   than the line that declared its parent, appearing after it. Duplicate keys
--   with the same name (e.g. @ascii@ under many rooms) are disambiguated by that
--   nesting rule.
module Worldbuilder.Locate
    ( lineForPath
    ) where

-- | Find the source line of a dotted path like @rooms.cave.ascii@. Returns
--   @(line number, line text)@ of the deepest segment matched.
lineForPath :: String -> String -> Maybe (Int, String)
lineForPath content dotted =
    let ls = zip [1 ..] (lines content)
    in match ls (filter (not . null) (splitDots dotted))
  where
    match _ [] = Nothing
    match ls (seg:rest) =
        case firstTopLevel ls seg of
            Nothing -> Nothing
            Just a@(n, l) -> go ls (n, indentOf l) a rest

    -- Walk the remaining segments; keep the deepest successful anchor.
    go _ _ best [] = Just best
    go ls (pn, pi_) best (seg:rest) =
        case candidates ls pn pi_ seg of
            (c@(cn, cl):_) -> go ls (cn, indentOf cl) c rest
            []             -> Just best

    firstTopLevel ls seg =
        case [ (n, l) | (n, l) <- ls, indentOf l == 0, isKey l, keyOf l == seg ] of
            (a:_) -> Just a
            []    -> Nothing

    -- A segment matches either a mapping key `seg:` or a list item whose `id:`
    -- is that segment (rooms/npcs/items are YAML lists of mappings).
    candidates ls pn pi_ seg =
        [ (n, l)
        | (n, l) <- ls
        , n > pn
        , indentOf l > pi_
        , isKey l
        , keyOf l == seg
        ] ++ listItems ls pn pi_ seg

    listItems ls pn pi_ seg =
        [ (n, l)
        | (n, l) <- ls
        , n > pn
        , indentOf l > pi_
        , isListStart l
        , idOf l == seg
        ]

    isKey l = case dropWhile (== ' ') l of
        ('\t':_) -> False
        rest     -> case break (== ':') rest of
            (k, ':':_) -> not (null k)
            _          -> False

    keyOf = takeWhile (/= ':') . dropWhile (== ' ')

    indentOf = length . takeWhile (== ' ')

splitDots :: String -> [String]
splitDots s = case break (== '.') s of
    (a, '.':b) -> a : splitDots b
    (a, _)     -> [a | not (null a)]

-- | Does this line start a list item (`- …`)?
isListStart :: String -> Bool
isListStart l = case dropWhile (== ' ') l of
    ('-':' ':_) -> True
    _           -> False

-- | The `id:` value of a list-start line, when it declares one on the same line.
idOf :: String -> String
idOf l =
    let rest = dropWhile (== ' ') l
    in case drop 2 rest of
        r -> case break (== ':') r of
            ("id", ':':v) -> trim v
            _             -> ""
  where trim = f . f
        f = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
