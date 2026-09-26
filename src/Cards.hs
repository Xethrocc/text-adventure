-- | Card gameplay and deck HUD rendering for deckbuilder mode
module Cards
    ( matchesNPCTarget
    , cardStopWords
    , checkResourceCosts
    , validateTarget
    , playCard
    , endTurn
    , visibleWidth
    , padRightVisible
    , wrapWords
    , cardTypeAnsiColor
    , cardTypeLabel
    , renderCardBox
    , hcatBoxes
    , renderDeckCombatHud
    , showHand
    , showDeck
    , showDiscard
    ) where

import Types
import Game
import Effects (applyOutcome)
import Ansi (stripAnsi)
import Data.Char (toLower)
import Data.List (foldl', intercalate, isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

-- | Stop words ignored during target matching for cards.
cardStopWords :: [String]
cardStopWords = ["the", "a", "an", "some", "that", "this", "der", "die", "das", "ein", "eine", "den", "dem"]

-- | Check if a target string matches an NPC definition by ID, name, or keywords.
matchesNPCTarget :: String -> NPCDef -> Bool
matchesNPCTarget tgt npc
    | null (words tgt) = False
    | otherwise =
        let lower = map toLower
            tgtWords = words tgt
            cleanedTgt = lower (unwords (filter (`notElem` cardStopWords) tgtWords))
            rawTgt = lower (unwords tgtWords)
            candidates = npcAliases npc
        in rawTgt `elem` candidates
           || (not (null cleanedTgt) && cleanedTgt `elem` candidates)
           || any (\c -> (not (null rawTgt) && rawTgt `isInfixOf` c)
                      || (not (null cleanedTgt) && cleanedTgt `isInfixOf` c)) candidates

-- | Validate and deduct resource costs for playing a card.
--   Supports "player.<resource>" or "<resource>".
checkResourceCosts :: Map.Map String Int -> GameState -> Either String GameState
checkResourceCosts costs st =
    let costList = Map.toList costs
        resolveVar name =
            let pKey = "player." ++ name
            in case getVariable pKey st of
                Just (VVInt v) -> (pKey, v)
                _ -> case getVariable name st of
                    Just (VVInt v) -> (name, v)
                    _              -> (pKey, 0)
        checkOne (res, req) =
            let (_, cur) = resolveVar res
            in if cur >= req
               then Right ()
               else Left ("Not enough " ++ res ++ " (need " ++ show req ++ ", have " ++ show cur ++ ").")
    in case mapM_ checkOne costList of
        Left err -> Left err
        Right () ->
            let deduct stAcc (res, req) =
                    let (varKey, cur) = resolveVar res
                    in setVariableChecked varKey (VVInt (cur - req)) stAcc
            in Right (foldl' deduct st costList)

-- | Validate card target against living enemies in current room.
validateTarget :: CardTarget -> Maybe String -> GameState -> Either String (String, GameState)
validateTarget targetReq mTarget st =
    let curRoom = currentRoom (save st)
        roomNPCs = getNPCsInRoom curRoom st
        livingEnemies = [npc | npc <- roomNPCs, not (isDeadNPC (npcId npc) st), not (isInParty (npcId npc) st)]
    in case targetReq of
        TargetNone ->
            Right ("", st)
        TargetSelf ->
            Right ("player", setVariableChecked "cmd.target" (VVText "player") st)
        TargetAllEnemies ->
            if null livingEnemies
            then Left "There are no living enemies here to target."
            else Right ("all enemies", setVariableChecked "cmd.target" (VVText "all") st)
        TargetSingleEnemy ->
            if null livingEnemies
            then Left "There are no living enemies here to target."
            else case mTarget of
                Nothing ->
                    if length livingEnemies == 1
                    then let sole = head livingEnemies
                         in Right (npcName sole, setVariableChecked "cmd.target" (VVText (npcId sole)) st)
                    else Left ("Please specify a target (e.g. 'play <n> <target>'). Available: "
                               ++ intercalate ", " (map npcName livingEnemies))
                Just tStr ->
                    let matches = filter (matchesNPCTarget tStr) livingEnemies
                    in case matches of
                        [] -> Left ("No living enemy matches '" ++ tStr ++ "'.")
                        (targetNpc:_) ->
                            Right (npcName targetNpc, setVariableChecked "cmd.target" (VVText (npcId targetNpc)) st)

-- | Play a card from hand by 1-based index, with optional target.
playCard :: Int -> Maybe String -> GameState -> CommandResult
playCard idx mTarget st = case deckState (save st) of
    Nothing -> (st, "You don't have a deck to play cards from.")
    Just ds ->
        let curHand = hand ds
        in if idx < 1 || idx > length curHand
           then (st, "Invalid card number " ++ show idx ++ ". You have "
                     ++ show (length curHand) ++ " card(s) in hand.")
           else
               let cId = curHand !! (idx - 1)
               in case Map.lookup cId (cardDefs (world st)) of
                   Nothing -> (st, "Unknown card: '" ++ cId ++ "'.")
                   Just card ->
                       case validateTarget (cardTarget card) mTarget st of
                           Left err -> (st, err)
                           Right (targetLabel, stTargeted) ->
                               case checkResourceCosts (cardCost card) stTargeted of
                                   Left costErr -> (st, costErr)
                                   Right stCostPaid ->
                                       let dsCurrent = fromMaybe ds (deckState (save stCostPaid))
                                           handAfter = removeAt (idx - 1) (hand dsCurrent)
                                           dsAfter = if cardExhaust card
                                                     then dsCurrent { hand = handAfter
                                                                    , exhaustPile = exhaustPile dsCurrent ++ [cId] }
                                                     else dsCurrent { hand = handAfter
                                                                    , discardPile = discardPile dsCurrent ++ [cId] }
                                           stAfterCard = stCostPaid
                                               { save = (save stCostPaid) { deckState = Just dsAfter } }
                                           (stFinal, effectMsgs) = foldl' (\(sAcc, msgsAcc) eff ->
                                               let (s', m) = applyOutcome eff "" sAcc
                                               in (s', if null m then msgsAcc else msgsAcc ++ [m])
                                               ) (stAfterCard, []) (cardEffects card)
                                           header = "You play " ++ cardName card
                                                    ++ (if null targetLabel then "" else " on " ++ targetLabel)
                                                    ++ (if cardExhaust card then " (Exhausted)." else ".")
                                           allMsg = intercalate "\n" (filter (not . null) (header : effectMsgs))
                                       in (stFinal, allMsg)

-- | End the player's turn:
--   - Discards remaining hand cards
--   - Resets player.block to 0
--   - Restores energy to player.max_energy (default: 3)
--   - Draws cards (default: 5 or player.draw_per_turn)
endTurn :: GameState -> CommandResult
endTurn st = case deckState (save st) of
    Nothing -> (st, "You don't have a deck to end your turn.")
    Just _ds ->
        let st1 = discardHand st
            st2 = setVariableChecked "player.block" (VVInt 0)
                    (if Map.member "block" (variables (save st1))
                     then setVariableChecked "block" (VVInt 0) st1
                     else st1)
            maxE = case getVariable "player.max_energy" st2 of
                Just (VVInt m) -> m
                _ -> case getVariable "max_energy" st2 of
                    Just (VVInt m) -> m
                    _              -> 3
            st3 = setVariableChecked "player.energy" (VVInt maxE)
                    (if Map.member "energy" (variables (save st2))
                     then setVariableChecked "energy" (VVInt maxE) st2
                     else st2)
            drawCount = case getVariable "player.draw_per_turn" st3 of
                Just (VVInt d) -> d
                _ -> case getVariable "draw_per_turn" st3 of
                    Just (VVInt d) -> d
                    _              -> 5
            st4 = drawCards drawCount st3
            msg = "Turn ended. Energy restored to " ++ show maxE
                  ++ ". Drew " ++ show drawCount ++ " cards."
        in (st4, msg)

-- | Visible width of a string, ignoring ANSI CSI escape sequences.
visibleWidth :: String -> Int
visibleWidth = length . stripAnsi

-- | Pad a string on the right to reach the desired visible width.
padRightVisible :: Int -> String -> String
padRightVisible w s =
    let cur = visibleWidth s
    in if cur < w then s ++ replicate (w - cur) ' ' else s

-- | Break a string into words and wrap to lines of at most maxW characters.
wrapWords :: Int -> String -> [String]
wrapWords _ "" = []
wrapWords maxW text = go (words text)
  where
    go [] = []
    go (w : ws) =
        let (lineWords, rest) = takeLine (length w) [w] ws
        in unwords lineWords : go rest
    takeLine _ acc [] = (reverse acc, [])
    takeLine curLen acc (w : ws)
        | curLen + 1 + length w <= maxW = takeLine (curLen + 1 + length w) (w : acc) ws
        | otherwise                      = (reverse acc, w : ws)

-- | 24-bit ANSI styling codes for card types (Ruby Red, Sapphire Blue, Golden Yellow, Shadow Purple, Grey).
cardTypeAnsiColor :: CardType -> String
cardTypeAnsiColor CardAttack = "\ESC[38;2;220;50;50m"
cardTypeAnsiColor CardSkill  = "\ESC[38;2;60;130;240m"
cardTypeAnsiColor CardPower  = "\ESC[38;2;240;190;40m"
cardTypeAnsiColor CardCurse  = "\ESC[38;2;160;60;200m"
cardTypeAnsiColor CardStatus = "\ESC[38;2;150;150;150m"

-- | German localized label for card types.
cardTypeLabel :: CardType -> String
cardTypeLabel CardAttack = "[Angriff]"
cardTypeLabel CardSkill  = "[Fertigkeit]"
cardTypeLabel CardPower  = "[Macht]"
cardTypeLabel CardCurse  = "[Fluch]"
cardTypeLabel CardStatus = "[Status]"

-- | Render a single card as a multi-line box (width 16).
renderCardBox :: Int -> Card -> [String]
renderCardBox idx card =
    let topBorder = "┌──────────────┐"
        botBorder = "└──────────────┘"
        emptyInner = "│              │"

        costStr = case Map.lookup "energy" (cardCost card) of
            Just c  -> "(" ++ show c ++ ")"
            Nothing -> if Map.null (cardCost card) then "(0)" else "(" ++ show (sum (Map.elems (cardCost card))) ++ ")"
        prefix = show idx ++ ". "
        availNameW = max 1 (14 - length prefix - length costStr - 1)
        namePart = take availNameW (cardName card)
        gapLen = max 1 (14 - length prefix - length namePart - length costStr)
        headerLine = "│" ++ take 14 (prefix ++ namePart ++ replicate gapLen ' ' ++ costStr ++ replicate 14 ' ') ++ "│"

        cColor = cardTypeAnsiColor (cardType card)
        cLabel = cardTypeLabel (cardType card)
        rawTag = cColor ++ cLabel ++ "\ESC[0m"
        tagVisible = length cLabel
        leftPad = max 0 ((14 - tagVisible) `div` 2)
        rightPad = max 0 (14 - tagVisible - leftPad)
        typeLine = "│" ++ replicate leftPad ' ' ++ rawTag ++ replicate rightPad ' ' ++ "│"

        wrapped = wrapWords 12 (cardDescription card)
        descLines = case wrapped of
            []        -> [emptyInner, emptyInner]
            [l]       -> ["│ " ++ padRightVisible 12 l ++ " │", emptyInner]
            (l1:l2:_) -> ["│ " ++ padRightVisible 12 l1 ++ " │", "│ " ++ padRightVisible 12 l2 ++ " │"]
    in [topBorder, headerLine, typeLine, emptyInner] ++ descLines ++ [botBorder]

-- | Tile multi-line text boxes horizontally with 2-space padding between boxes.
--   Wraps into a new row of boxes when adding another box would exceed maxWidth.
--   Pads boxes in each row vertically to match the height of the tallest box in that row.
hcatBoxes :: Int -> [[String]] -> [String]
hcatBoxes _ [] = []
hcatBoxes maxW allBoxes =
    let spacing = 2
        normBoxes = [ (maximum (0 : map visibleWidth b), b) | b <- allBoxes ]

        groupRows [] = []
        groupRows ((w, b) : rest) =
            let (row, remainder) = takeRow (w + spacing) [ (w, b) ] rest
            in map snd row : groupRows remainder

        takeRow _ current [] = (reverse current, [])
        takeRow usedWidth current ((w, b) : next)
            | usedWidth + w <= maxW =
                takeRow (usedWidth + w + spacing) ((w, b) : current) next
            | otherwise =
                (reverse current, (w, b) : next)

        rows = groupRows normBoxes

        renderRow [] = []
        renderRow rowBoxes =
            let maxH = maximum (0 : map length rowBoxes)
                boxWidths = map (\b -> maximum (0 : map visibleWidth b)) rowBoxes
                padBoxVert h w b =
                    let extraLines = h - length b
                        padded = map (padRightVisible w) b
                    in padded ++ replicate extraLines (replicate w ' ')
                paddedBoxes = zipWith (padBoxVert maxH) boxWidths rowBoxes
                stitchLines lineIdx =
                    intercalate (replicate spacing ' ') [b !! lineIdx | b <- paddedBoxes]
            in [stitchLines i | i <- [0 .. maxH - 1]]

    in intercalate [""] (map renderRow rows)

-- | Compact combat HUD above hand cards in deckbuilder mode.
renderDeckCombatHud :: GameState -> DeckState -> [String]
renderDeckCombatHud st ds =
    let w = 78
        doubleLine = replicate w '═'
        singleLine = replicate w '─'

        p = player (save st)
        hp = playerHealth p
        maxHp = effectiveMaxHealth st
        blockVal = case getVariable "player.block" st of
            Just (VVInt b) -> b
            _ -> case getVariable "block" st of
                Just (VVInt b) -> b
                _              -> 0
        energyVal = case getVariable "player.energy" st of
            Just (VVInt e) -> e
            _ -> case getVariable "energy" st of
                Just (VVInt e) -> e
                _              -> 3
        maxEnergyVal = case getVariable "player.max_energy" st of
            Just (VVInt m) -> m
            _ -> case getVariable "max_energy" st of
                Just (VVInt m) -> m
                _              -> 3

        deckCnt = length (drawPile ds)
        discCnt = length (discardPile ds)
        exhCnt  = length (exhaustPile ds)
        exhStr  = if exhCnt > 0 then " | Erschöpft: " ++ show exhCnt else ""

        statusBar = " [Deck: " ++ show deckCnt ++ "] ─── HP " ++ show hp ++ "/" ++ show maxHp
                    ++ " | Block: " ++ show blockVal
                    ++ " | Energie: " ++ show energyVal ++ "/" ++ show maxEnergyVal
                    ++ exhStr
                    ++ " ─── [Ablage: " ++ show discCnt ++ "]"

        curRoom = currentRoom (save st)
        roomEnemies = [ def
                      | def <- getNPCsInRoom curRoom st
                      , not (isDeadNPC (npcId def) st)
                      , not (isInParty (npcId def) st)
                      ]

        enemyLines = concatMap formatEnemy roomEnemies
        formatEnemy def =
            let eHp = case Map.lookup (npcId def) (npcStates (save st)) of
                    Just ns -> fromMaybe 0 (npcHealth ns)
                    Nothing -> 0
                eMax = fromMaybe eHp (npcMaxHealth def)
                eLine = " GEGNER: " ++ npcName def ++ " (HP: " ++ show eHp ++ "/" ++ show eMax ++ ")"
                mIntent = case getVariable ("intent." ++ npcId def) st of
                    Just (VVText it) -> Just it
                    _ -> case getVariable ("combat.intent." ++ npcId def) st of
                        Just (VVText it) -> Just it
                        _                -> Nothing
                intentLine = case mIntent of
                    Just it -> [" ABSICHT: " ++ it]
                    Nothing -> []
            in eLine : intentLine
    in [doubleLine, statusBar, singleLine]
       ++ (if null enemyLines then [" (Keine Gegner im Raum)"] else enemyLines)
       ++ [doubleLine]

-- | Display the current hand in horizontal tile layout with combat HUD.
showHand :: GameState -> CommandResult
showHand st = case deckState (save st) of
    Nothing -> (st, "You don't have a deck.")
    Just ds ->
        let curHand = hand ds
            hudLines = renderDeckCombatHud st ds
        in if null curHand
           then (st, intercalate "\n" (hudLines ++ ["Your hand is empty."]))
           else
               let lookupCardBox idx cId = case Map.lookup cId (cardDefs (world st)) of
                       Just c  -> renderCardBox idx c
                       Nothing ->
                           [ "┌──────────────┐"
                           , "│ " ++ padRightVisible 12 (show idx ++ ". " ++ take 8 cId) ++ " │"
                           , "│  [Unbekannt] │"
                           , "│              │"
                           , "│ Nicht        │"
                           , "│ gefunden     │"
                           , "└──────────────┘"
                           ]
                   cardBoxes = zipWith lookupCardBox [1 :: Int ..] curHand
                   tiledHand = hcatBoxes 80 cardBoxes
               in (st, intercalate "\n" (hudLines ++ [""] ++ tiledHand))

-- | Display draw pile summary.
showDeck :: GameState -> CommandResult
showDeck st = case deckState (save st) of
    Nothing -> (st, "You don't have a deck.")
    Just ds ->
        let curDraw = drawPile ds
            curHand = hand ds
            curDisc = discardPile ds
            curExh  = exhaustPile ds
            total = length curDraw + length curHand + length curDisc + length curExh
            nameOf cId = case Map.lookup cId (cardDefs (world st)) of
                Just c  -> cardName c
                Nothing -> cId
            cardCounts = Map.toList (Map.fromListWith (+) [(nameOf cId, 1 :: Int) | cId <- curDraw])
            cardLines = [ "  - " ++ name ++ (if cnt > 1 then " (x" ++ show cnt ++ ")" else "")
                        | (name, cnt) <- cardCounts ]
            header = "=== Draw Pile (" ++ show (length curDraw) ++ "/" ++ show total ++ " cards) ==="
            body = if null cardLines then ["  (Empty)"] else cardLines
            footer = "Hand: " ++ show (length curHand)
                     ++ " | Discard: " ++ show (length curDisc)
                     ++ " | Exhaust: " ++ show (length curExh)
        in (st, intercalate "\n" ([header] ++ body ++ [footer]))

-- | Display discard pile contents.
showDiscard :: GameState -> CommandResult
showDiscard st = case deckState (save st) of
    Nothing -> (st, "You don't have a deck.")
    Just ds ->
        let curDisc = discardPile ds
            nameOf cId = case Map.lookup cId (cardDefs (world st)) of
                Just c  -> cardName c
                Nothing -> cId
            cardCounts = Map.toList (Map.fromListWith (+) [(nameOf cId, 1 :: Int) | cId <- curDisc])
            cardLines = [ "  - " ++ name ++ (if cnt > 1 then " (x" ++ show cnt ++ ")" else "")
                        | (name, cnt) <- cardCounts ]
            header = "=== Discard Pile (" ++ show (length curDisc) ++ " cards) ==="
            body = if null cardLines then ["  (Empty)"] else cardLines
        in (st, intercalate "\n" ([header] ++ body))
