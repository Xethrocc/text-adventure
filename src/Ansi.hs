-- | ANSI escape handling at the I/O boundary.
--
--   The engine core never knows whether it is talking to a terminal: it returns
--   strings that *may* contain ANSI SGR sequences (ASCII art from `img2ascii`).
--   `app/Main` decides once whether colour is allowed and passes the resulting
--   pure `String -> String` mapping into the game loop. Redirecting stdout to a
--   file or passing `--no-color` therefore yields plain text — no state, no
--   terminal access in the core.
module Ansi
    ( stripAnsi
    , ansiFilter
    ) where

-- | Remove CSI escape sequences (the only kind the converter emits). A CSI
--   sequence is @ESC [@ followed by parameter/intermediate bytes and a final
--   byte in the range @0x40..0x7E@.
stripAnsi :: String -> String
stripAnsi = go
  where
    go :: String -> String
    go [] = []
    go ('\ESC' : '[' : rest) = go (dropCsi rest)
    go (c : rest) = c : go rest

    -- Drop until (and including) the final byte of the CSI sequence.
    dropCsi :: String -> String
    dropCsi [] = []
    dropCsi (c : rest)
      | c >= '@' && c <= '~' = rest
      | otherwise = dropCsi rest

-- | Pick the output mapping for the game loop. Colour is only allowed when
--   stdout is a terminal *and* colour was not disabled; otherwise every escape
--   sequence is stripped. Pure, so the policy is testable without a TTY.
ansiFilter :: Bool -> Bool -> String -> String
ansiFilter isTty noColor
    | isTty && not noColor = id
    | otherwise = stripAnsi