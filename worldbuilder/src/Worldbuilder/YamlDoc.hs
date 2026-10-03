{-# LANGUAGE OverloadedStrings #-}

-- | Exact YAML source positions per node, and a position-preserving writer.
--
--   W5, Stufe 2 (decision DW5a, 2026-10-03). Stufe 1 ('Worldbuilder.Locate')
--   guesses a line from the file text, because the worldbuilder decodes through
--   'Data.YAML.Aeson' and that JSON bridge drops the per-node positions. This
--   module takes the other route: parse the *same bytes* a second time as
--   @Node Pos@. 'Data.YAML.decode1' has a @FromJSON (Node Pos)@ instance, so
--   every scalar, mapping key and sequence item carries its exact
--   @posLine@' / @posColumn@ / @posByteOffset@. Measured on
--   @examples/demo.yaml@: 131 nodes with exact positions, obtained without a
--   new dependency and without touching the 'FromJSON' compile path.
--
--   Nothing in the compile path changes: this is a read-only side channel. If
--   the second parse fails for any reason, callers keep the Locate heuristic.
--
--   Reading: 'ydResolveIssuePath' turns a dotted 'CompileIssue.ciPath' into a
--   node path and returns the position of the *key* that names the field — the
--   same line Locate reports, but exact instead of heuristic.
--
--   Writing: 'setScalarAt' replaces one **plain** scalar's bytes and leaves the
--   rest of the document untouched, so comments, key order, quoting and blank
--   lines survive. Everything it cannot do safely is refused with a named
--   'YEditError' — quoted scalars, block scalars (@|@ / @>@), multi-line plain
--   scalars, mappings and sequences, and values that contain a line break.
--
--   Contract for callers: byte offsets are only ever used to slice the original
--   'BL.ByteString' they came from. Never re-encode a node to write it back —
--   that would reformat the file and drop its comments.
module Worldbuilder.YamlDoc
    ( -- * Documents
      YamlDoc
    , parseYamlDoc
    , YamlSeg (..)
      -- * Reading
    , ydPosOf
    , ydResolveIssuePath
    , splitIssuePath
      -- * Writing
    , ScalarSpan (..)
    , ydScalarSpan
    , insertKeys
    , YEditError (..)
    , setScalarAt
    , setScalarsAt
    , renderYEditError
    ) where

import Data.YAML (Node (..), Pos (..), Scalar (..), decode1)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Word (Word8)
import qualified Data.Text.Encoding as TE

-- ---------------------------------------------------------------------------
-- Documents
-- ---------------------------------------------------------------------------

-- | The raw bytes plus the parsed node tree. The bytes are the source of truth
--   for every write; the tree supplies only offsets.
data YamlDoc = YamlDoc
    { ydSource :: BL.ByteString
    , ydRoot   :: Node Pos
    }

-- | Parse YAML into a node tree with positions. A failure is never fatal for the
--   caller: it only means "no exact positions available".
parseYamlDoc :: BL.ByteString -> Either String YamlDoc
parseYamlDoc raw =
    case (decode1 raw :: Either (Pos, String) (Node Pos)) of
        Left (pos, msg) -> Left (msg ++ " at " ++ showPos pos)
        Right root -> Right (YamlDoc raw root)
  where
    showPos p = show (posLine p) ++ ":" ++ show (posColumn p)

-- | One step of a node path: a mapping key, or a sequence index.
data YamlSeg
    = SegKey T.Text
    | SegIndex Int
    deriving (Eq, Show)

-- | Position of a node. 'Data.YAML.Internal.nodeLoc' is not exposed, so the
--   four constructors are matched here.
nodePos :: Node Pos -> Pos
nodePos (Scalar p _) = p
nodePos (Mapping p _ _) = p
nodePos (Sequence p _ _) = p
nodePos (Anchor p _ _) = p

-- | A scalar's own name, when it has a usable one (keys and simple values).
--   Tags and nulls have none, so they are never matched by a path.
scalarName :: Scalar -> Maybe T.Text
scalarName (SStr t) = Just t
scalarName (SInt n) = Just (T.pack (show n))
scalarName (SBool b) = Just (T.pack (show b))
scalarName (SFloat d) = Just (T.pack (show d))
scalarName _ = Nothing

-- ---------------------------------------------------------------------------
-- Reading
-- ---------------------------------------------------------------------------

-- | Resolve a node path and return the position of the key that names it.
--
--   For a mapping key the *key* position is returned — the line an author reads
--   as @key: value@. For a sequence item the item's own position, which for the
--   usual @- id: hall@ shape is exactly the @id:@ line.
ydPosOf :: YamlDoc -> [YamlSeg] -> Maybe Pos
ydPosOf doc path = do
    (node, keyNode) <- resolve (ydRoot doc) path
    pure (maybe (nodePos node) nodePos keyNode)

-- | Resolve a path, returning the value node and — when the last step named a
--   mapping key — the key node that named it.
resolve :: Node Pos -> [YamlSeg] -> Maybe (Node Pos, Maybe (Node Pos))
resolve node [] = Just (node, Nothing)
resolve node (seg:rest) = do
    (stepped, keyNode) <- step node seg
    -- The recursive call ends in the deepest step, so its key node is the one
    -- naming the field — that is the line an author reads (and what Locate
    -- reports for the same path).
    if null rest then Just (stepped, keyNode) else resolve stepped rest

-- | One step: a key inside a mapping, an index inside a sequence, or — what the
--   dotted paths need — a list item addressed by its @id@ (or @name@), which is
--   how @rooms.hall@ reaches a single room.
step :: Node Pos -> YamlSeg -> Maybe (Node Pos, Maybe (Node Pos))
step node seg = case node of
    Mapping _ _ m -> case seg of
        SegKey k ->
            case [ (key, val) | (key, val) <- Map.toList m, scalarNameOf key == Just k ] of
                ((key, val):_) -> Just (val, Just key)
                []             -> Nothing
        SegIndex _ -> Nothing
    Sequence _ _ xs -> case seg of
        SegIndex i | i >= 0, i < length xs -> Just (xs !! i, Nothing)
                   | otherwise             -> Nothing
        SegKey k -> namedItem k xs
    Anchor _ _ inner -> step inner seg
    Scalar _ _ -> Nothing

-- | A sequence item addressed by an id (the schema's convention), falling back
--   to its name. Note that the comparison is on the id's *value*, not on the
--   @id:@ key node — comparing the key would match every list item.
namedItem :: T.Text -> [Node Pos] -> Maybe (Node Pos, Maybe (Node Pos))
namedItem k xs =
    case [ (x, key)
         | x <- xs
         , Just key <- [fieldKey "id" x]
         , Just v <- [fieldValue "id" x]
         , scalarNameOf v == Just k ] of
        ((x, key):_) -> Just (x, Just key)
        [] -> case [ (x, key)
                   | x <- xs
                   , Just key <- [fieldKey "name" x]
                   , Just v <- [fieldValue "name" x]
                   , scalarNameOf v == Just k ] of
            ((x, key):_) -> Just (x, Just key)
            []           -> Nothing

-- | The key node of a mapping field, if the node is a mapping with that key.
fieldKey :: T.Text -> Node Pos -> Maybe (Node Pos)
fieldKey name (Mapping _ _ m) =
    case [ key | (key, _) <- Map.toList m, scalarNameOf key == Just name ] of
        (key:_) -> Just key
        []      -> Nothing
fieldKey _ _ = Nothing

-- | The value node of a mapping field.
fieldValue :: T.Text -> Node Pos -> Maybe (Node Pos)
fieldValue name (Mapping _ _ m) =
    case [ val | (key, val) <- Map.toList m, scalarNameOf key == Just name ] of
        (val:_) -> Just val
        []      -> Nothing
fieldValue _ _ = Nothing

scalarNameOf :: Node Pos -> Maybe T.Text
scalarNameOf (Scalar _ s) = scalarName s
scalarNameOf _ = Nothing

-- | Split a dotted issue path into node segments.
--
--   Two shapes beyond plain dotted keys occur in 'CompileIssue.ciPath':
--
--   * @interactions.npc[verband]@ — a list item addressed by its id, the form
--     the B9 reference checks use;
--   * @foo[0]@ — a plain list index.
--
--   Both fold into the segment list, so @foo[bar][0]@ means "the @foo@ key,
--   then the item with id @bar@, then its first element".
splitIssuePath :: String -> [YamlSeg]
splitIssuePath = concatMap one . splitDots
  where
        -- `break` already moved the '[' into the suffix, so `brackets` gets it
    -- back (`'[':rest` would strip it twice and then find no opening bracket).
    one seg = case break (== '[') seg of
        (name, '[':rest) -> keySeg name ++ brackets ('[':rest)
        _                -> keySeg seg
    -- A segment is *either* a key or an index, never both: emitting both would
    -- make every numeric path unresolvable (the key step fails first).
    keySeg name
        | null name = []
        | otherwise = case asIndex name of
            Just n  -> [SegIndex n]
            Nothing -> [SegKey (T.pack name)]
    brackets s = case s of
        ('[':rest) -> case break (== ']') rest of
            (inner, _:afterRest) -> one inner ++ brackets afterRest
            _                    -> []
        _ -> []
    asIndex s = case reads s of
        [(n, "")] -> Just n
        _         -> Nothing

-- | Split on @.@ without producing empty segments ('Worldbuilder.Locate' has the
--   same shape; kept separate so the two modules can be compared directly).
splitDots :: String -> [String]
splitDots s = case break (== '.') s of
    (a, '.':b) -> a : splitDots b
    (a, _)     -> [a | not (null a)]

-- | Dotted issue path -> exact position. 'Nothing' means "use Locate instead":
--   either the path names something the document does not have, or it uses a
--   shape this adapter does not model. Callers must fall back silently.
ydResolveIssuePath :: YamlDoc -> String -> Maybe Pos
ydResolveIssuePath doc path = ydPosOf doc (splitIssuePath path)

-- ---------------------------------------------------------------------------
-- Writing
-- ---------------------------------------------------------------------------

-- | The byte range one scalar occupies in the original document.
data ScalarSpan = ScalarSpan
    { ssStart :: Int           -- ^ inclusive byte offset
    , ssEnd   :: Int           -- ^ exclusive byte offset
    , ssText  :: BL.ByteString -- ^ the bytes currently there
    } deriving (Eq, Show)

-- | Why a write was refused. Every case means "nothing was changed".
data YEditError
    = YEditPathNotFound String   -- ^ the path does not resolve to a node
    | YEditNotScalar String      -- ^ the node is a mapping or a sequence
    | YEditFlowMapping String    -- ^ flow mapping (@{a: 1}@): no lines to append to
    | YEditQuoted String         -- ^ @'…'@ or @"…"@: re-quoting is the caller's job
    | YEditBlockScalar String    -- ^ @|@ or @>@ block scalar
    | YEditMultiline String      -- ^ plain scalar continued on the next line
    | YEditValueRejected String  -- ^ the replacement would break the line
    deriving (Eq, Show)

-- | Scalar style as far as the writer cares: only 'StylePlain' is writable.
data ScalarStyle = StylePlain | StyleQuoted | StyleBlock
    deriving (Eq, Show)

renderYEditError :: YEditError -> String
renderYEditError e = case e of
    YEditPathNotFound p -> "no YAML node at path '" ++ p ++ "'"
    YEditNotScalar p    -> "'" ++ p ++ "' is a map or sequence, not a scalar"
    YEditFlowMapping p  -> "'" ++ p ++ "' is a flow mapping ({...}) — no line to append to"
    YEditQuoted p       -> "'" ++ p ++ "' is a quoted scalar — refusing to change its quotes"
    YEditBlockScalar p  -> "'" ++ p ++ "' is a block scalar (| or >) — refusing to touch it"
    YEditMultiline p    -> "'" ++ p ++ "' spans several lines — refusing to touch it"
    YEditValueRejected p ->
        "replacement value for '" ++ p ++ "' contains a line break"

-- | The byte range of the scalar at a node path, or a refusal.
--
--   'Node Pos' does not carry the scalar *style* — HsYAML's concrete AST drops
--   it — so the style is read from the source bytes at the scalar's start
--   offset. Only plain scalars are writable: they end at the line end, at a
--   flow delimiter (@,@ @}@ @]@) or before a @#@ comment, and trailing spaces
--   are not part of the value. Everything else is refused rather than guessed.
ydScalarSpan :: YamlDoc -> String -> Either YEditError ScalarSpan
ydScalarSpan doc path = do
    node <- maybe (Left (YEditPathNotFound path)) Right (resolveValue path)
    case node of
        Scalar p _ -> do
            let start = posByteOffset p
            end <- plainSpan path start
            pure ScalarSpan
                { ssStart = start
                , ssEnd   = end
                , ssText  = slice start end
                }
        _ -> Left (YEditNotScalar path)
  where
    src = ydSource doc
    strictSrc = BL.toStrict src
    len = BS.length strictSrc
    resolveValue :: String -> Maybe (Node Pos)
    resolveValue p = fst <$> resolve (ydRoot doc) (splitIssuePath p)
    slice a b = BL.take (fromIntegral (b - a)) (BL.drop (fromIntegral a) src)

    byteAt i = BS.index strictSrc i

    plainSpan p start = do
        style <- scalarStyle start
        case style of
            StyleQuoted -> Left (YEditQuoted p)
            StyleBlock  -> Left (YEditBlockScalar p)
            StylePlain  -> do
                let end = trimBack start (scanPlain start)
                if continuationFollows start
                then Left (YEditMultiline p)
                else Right end

    scalarStyle start = case BS.uncons (BS.drop (fromIntegral start) strictSrc) of
        Just (c, _) | c == 0x27 -> Right StyleQuoted   -- '
                    | c == 0x22 -> Right StyleQuoted   -- "
                    | c == 0x7c -> Right StyleBlock    -- |
                    | c == 0x3e -> Right StyleBlock    -- >
        _ -> Right StylePlain

    -- Stop at the line end, at a flow delimiter, or before a trailing comment.
    scanPlain start = go start
      where
        go i
            | i >= len = i
            | c == 0x0a || c == 0x0d = i
            | c == 0x2c || c == 0x7d || c == 0x5d = i      -- , } ]
            | c == 0x23 && i > start = i                  -- # comment
            | otherwise = go (i + 1)
          where c = byteAt i

    -- A value ends before trailing whitespace (a comment may follow).
    trimBack start end
        | end <= start = end
        | isSpaceByte (byteAt (end - 1)) = trimBack start (end - 1)
        | otherwise = end
      where isSpaceByte c = c == 0x20 || c == 0x09

    -- A plain scalar may continue on the next line (YAML folds long values onto
    -- indented lines). Splicing such a scalar would also have to remove the
    -- continuation, so it is refused instead.
    continuationFollows start
        | start >= len = False
        | isEol (byteAt start) = False
        | otherwise = case followingLine start of
            Just nextLine -> indentOf nextLine > indentOf (lineFrom start)
                             && not (startsNewNode nextLine)
            Nothing -> False

    lineFrom start = trimEndOf (BS.drop (fromIntegral start) strictSrc)
    -- The next line, or Nothing at end of file. `BS.break` stops *at* the first
    -- newline and keeps it in the second half, so that half is dropped by one
    -- byte before the next line is read. (`dropWhile (not . isEol)` would keep
    -- the newline — the predicate is False there, so the scan halts in front of
    -- it — which is how this function returned empty lines once.)
    followingLine start =
        let (_, afterEol) = BS.break isEol (BS.drop (fromIntegral start) strictSrc)
        in if BS.null afterEol then Nothing
           else Just (trimEndOf (BS.drop 1 afterEol))
    isEol c = c == 0x0a || c == 0x0d
    trimEndOf = BS.takeWhile (not . isEol)

-- | Set several scalars under one mapping, one after the other.
--
--   Each step re-parses the result before the next one, because a second
--   'setScalarAt' on the *original* document would compute its offsets against
--   text that the first edit has already changed — two coordinates in the same
--   flow mapping would then silently undo each other.
setScalarsAt :: YamlDoc -> String -> [(String, String)] -> Either YEditError BL.ByteString
setScalarsAt doc basePath pairs = go doc pairs
  where
    go d [] = Right (ydSource d)
    go d ((key, value):rest) = do
        out <- setScalarAt d (basePath ++ "." ++ key) (BLC.pack value)
        case parseYamlDoc out of
            Left err -> Left (YEditValueRejected (basePath ++ "." ++ key ++ ": " ++ err))
            Right d' -> go d' rest

-- | Insert whole key lines into an existing **block** mapping, right after the
--   mapping's last line.
--
--   Needed as soon as an editor adds a field the author never wrote: pinning a
--   room's @map:@ for the first time cannot work by replacing a scalar. The
--   indentation comes from the mapping's own keys, so the result matches the
--   surrounding style; a mapping without any key yet uses the parent's
--   indentation plus two.
--
--   Refused for flow mappings (@rooms.a: {x: 1}@ — everything on one line),
--   because appending a line there would change the document's structure.
insertKeys :: YamlDoc -> String -> [(String, String)] -> Either YEditError BL.ByteString
insertKeys doc path pairs
    | null pairs = Right (ydSource doc)
    | otherwise = do
        node <- maybe (Left (YEditPathNotFound path)) Right (resolveValue path)
        case node of
            -- `else do` on purpose: a `let` directly after an `if/then/else`
            -- at the same indentation is a parse error in a do block.
            Mapping p _ _ ->
                if firstByteAt (posByteOffset p) == 0x7b       -- '{' = flow style
                then Left (YEditFlowMapping path)
                else do
                    let sl = srcLines (ydSource doc)
                        atStart = lineAt sl (posLine p)
                        endLine = blockEnd sl (posLine p) (slIndent atStart)
                        indent = keyIndentOf sl (posLine p) (slIndent atStart)
                        hasNext = slNo (lineAt sl (endLine + 1)) > endLine
                        total = fromIntegral (BL.length (ydSource doc)) :: Int
                        endsWithNl = not (BL.null (ydSource doc))
                                      && BLC.unpack (BL.drop (fromIntegral (total - 1)) (ydSource doc)) == "\n"
                        pad = BS.replicate indent 0x20
                        rendered = [ pad <> TE.encodeUtf8 (T.pack (k ++ ": " ++ v)) | (k, v) <- pairs ]
                        -- Insert before the next line; at the end of the file,
                        -- before a final newline if there is one, else at the
                        -- very end (supplying the newline ourselves).
                        (insertAt, body) = insertPoint sl endLine total endsWithNl hasNext rendered
                        splice = BL.take (fromIntegral insertAt) (ydSource doc)
                                 <> body
                                 <> BL.drop (fromIntegral insertAt) (ydSource doc)
                    pure splice
            _ -> Left (YEditNotScalar path)
  where
    resolveValue :: String -> Maybe (Node Pos)
    resolveValue p = fst <$> resolve (ydRoot doc) (splitIssuePath p)
    firstByteAt :: Int -> Word8
    firstByteAt i = case BS.uncons (BS.take (i + 1) (BL.toStrict (ydSource doc))) of
        Just (c, _) -> c
        Nothing -> 0
    -- Where to splice, and what: before the next line, or before a final
    -- newline at the end of the file, or at the very end.
    insertPoint :: [SrcLine] -> Int -> Int -> Bool -> Bool -> [BS.ByteString]
                -> (Int, BL.ByteString)
    -- A comment at the end of the block's last line is part of that line, so the
    -- insertion always starts at a *line* boundary: before the next line, or at
    -- the (empty) line behind a final newline, or — at a file whose last line has
    -- no newline — after a newline of our own.
    insertPoint sl endLine total endsWithNl hasNext rendered
        | hasNext = (offsetOfLine sl (endLine + 1), joinedWithNls rendered True)
        | endsWithNl = (total - 1, joinedWithNls rendered True)
        | otherwise = (total, BL.concat (BLC.pack "\n" : [ BL.fromStrict b | b <- rendered ])
                                    <> BLC.pack "\n")

    -- Rendered lines with newlines between them, plus a trailing one when the
    -- document does not already provide it.
    joinedWithNls :: [BS.ByteString] -> Bool -> BL.ByteString
    joinedWithNls ls trailing =
        BL.concat ([ BL.fromStrict b <> nl' | b <- init ls ]
                   <> [ BL.fromStrict (last ls) <> (if trailing then nl' else BL.empty) ])
      where nl' = BL.fromStrict (BS.pack [0x0a])

-- | One source line with everything the writer needs to reason about it.
data SrcLine = SrcLine
    { slNo     :: Int             -- ^ 1-based line number
    , slOffset :: Int             -- ^ byte offset of the line start
    , slIndent :: Int             -- ^ leading spaces
    , slBlank  :: Bool            -- ^ empty or only whitespace
    , slKey    :: Bool            -- ^ looks like a mapping key or a list item
    , slBytes  :: BS.ByteString
    } deriving (Eq, Show)

-- | Split the source into lines, once, with offsets and classification. The
--   offsets are what make the insert byte-exact.
srcLines :: BL.ByteString -> [SrcLine]
srcLines bs = go 1 0 (map (fromIntegral :: Word8 -> Int) (BS.unpack (BL.toStrict bs)))
  where
    go :: Int -> Int -> [Int] -> [SrcLine]
    go n off cs = case break isEol cs of
        (before, 10:rest) -> mk n off before : go (n + 1) (off + length before + 1) rest
        (before, 13:13:rest) -> mk n off before : go (n + 1) (off + length before + 2) rest
        (before, _:rest) -> mk n off before : go (n + 1) (off + length before + 1) rest
        (before, []) -> [mk n off before]
    isEol c = c == 10 || c == 13
    mk :: Int -> Int -> [Int] -> SrcLine
    mk n off ls = SrcLine
        { slNo = n
        , slOffset = off
        , slIndent = length (takeWhile (== 0x20) ls)
        , slBlank = null (dropWhile (== 0x20) ls)
        , slKey = not (null (dropWhile (== 0x20) ls)) && looksLikeKeyOrItem ls
        , slBytes = BS.pack (map (fromIntegral :: Int -> Word8) ls)
        }
    looksLikeKeyOrItem ls =
        case dropWhile (== 0x20) ls of
            (0x2d:rest) -> null rest || head rest == 0x20
            _ -> case break (== 0x3a) ls of
                (k, 0x3a:after) ->
                    not (null k) && (null after || head after == 0x20 || head after == 0x09)
                _ -> False

lineAt :: [SrcLine] -> Int -> SrcLine
lineAt sl n = case drop (n - 1) sl of
    (l:_) -> l
    [] -> case sl of
        (l:_) -> l
        [] -> SrcLine 0 0 0 True False BS.empty

offsetOfLine :: [SrcLine] -> Int -> Int
offsetOfLine sl n = slOffset (lineAt sl n)

-- | Indentation for a new key inside a mapping that starts at @startLine@: the
--   indentation of its first own key, or the parent's plus two when it has none.
keyIndentOf :: [SrcLine] -> Int -> Int -> Int
keyIndentOf sl startLine parentIndent =
    case [ slIndent l | l <- sl, slNo l > startLine, slKey l, slIndent l > parentIndent ] of
        (i:_) -> i
        [] -> parentIndent + 2

-- | Last line of the block starting at @startLine@ with the given indentation:
--   the last line before a non-blank line at the same or a lower indentation.
blockEnd :: [SrcLine] -> Int -> Int -> Int
blockEnd sl startLine indent = go startLine startLine
  where
    -- The last *non-blank* line of the block: trailing blank lines must not
    -- extend it, or the insertion point slides onto the line before them (a
    -- comment line, or the last real line of the file).
    go i lastContent
        | i > total = lastContent      -- `>` not `>=`: the last line still counts
        | i > startLine, not (slBlank l), slIndent l <= indent = lastContent
        | slBlank l = go (i + 1) lastContent
        | otherwise = go (i + 1) i
      where
        l = lineAt sl i
    total = length sl

-- | Replace one plain scalar's bytes; every other byte of the document is
--   returned unchanged. A replacement containing a line break is refused — it
--   would change the document's structure rather than a value.
setScalarAt :: YamlDoc -> String -> BL.ByteString -> Either YEditError BL.ByteString
setScalarAt doc path value
    | BS.any isEol strict = Left (YEditValueRejected path)
    | otherwise = do
        sp <- ydScalarSpan doc path
        let src = ydSource doc
        pure $ BL.take (fromIntegral (ssStart sp)) src
               <> value
               <> BL.drop (fromIntegral (ssEnd sp)) src
  where
    strict = BL.toStrict value
    isEol c = c == 0x0a || c == 0x0d

indentOf :: BS.ByteString -> Int
indentOf = BS.length . BS.takeWhile (== 0x20)

-- | Does this line start a new node? Used to tell a folded continuation apart
--   from the next key at the same nesting level.
--
--   A list item is @- …@; a mapping key is a non-empty key followed by @:@ and
--   either a space or the end of the line (the rule 'Worldbuilder.Locate' uses,
--   and the rule the YAML grammar uses — a bare @foo:bar@ is a plain scalar,
--   not a key). Everything else that is indented below its parent is treated as
--   a continuation, which is why the writer refuses those.
startsNewNode :: BS.ByteString -> Bool
startsNewNode bs = case BS.uncons trimmed of
    Nothing -> False
    Just (first, _) -> case first of
        0x2d -> isSpaceOrEnd (BS.drop 1 trimmed)                  -- "- x" list item
        _ -> case BS.elemIndex 0x3a trimmed of                    -- ':'
            Nothing -> False
            Just i -> i > 0 && isSpaceOrEnd (BS.drop (i + 1) trimmed)
  where
    trimmed = BS.dropWhile (\c -> c == 0x20 || c == 0x09) bs
    isSpaceOrEnd after
        | BS.null after = True
        | otherwise = let c = BS.head after in c == 0x20 || c == 0x09