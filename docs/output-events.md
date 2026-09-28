# Strukturierte Ausgabe & Styling-Modell (Phase 1.2)

**Stand:** 2026-09-28 · Modul: `src/Types/Output.hs` (Blatt-Modul, über die
`Types`-Fassade exportiert) · Primärpfad: `applyLoopCommandEv` /
`executeCommandEv` (GameLoop.hs / Parser.hs)

## Was 1.2 liefert

Die Engine produziert ihre Ausgabe als **geordneten Event-Strom**
(`[OutputEvent]`) statt als flachen Text:

```haskell
data OutputEvent
    = EvMessage MsgPayload        -- Katalogmeldung: Key + Args + gerenderter Text
    | EvText StyledText           -- Prosa: ANSI-frei, Styles als Spans
    | EvArt ArtPayload            -- Kunstblock: roher Text + Hotspot-Anker
    | EvAnim Int [String]         -- Animation: Framedelay (µs) + Frames
    | EvSfx FilePath
    | EvMusicStart FilePath
    | EvMusicStop
    | EvRoomChanged String        -- State-Events (additiv, ohne Text)
    | EvQuestUpdate
    | EvDialogue                  -- Dialog aktiv (Choices kommen im Snapshot, 1.4)
    | EvCombat Bool               -- combat.engaged geändert (neuer Wert)
    | EvGameOver                  -- Spielende in diesem Befehl (Grund im Snapshot)
```

`applyLoopCommandEv :: Command -> LoopState -> (LoopState, [OutputEvent])` ist der
Primärpfad; `executeCommandEv` dito für den Parser. **Kompatibilitätsform:**
`applyLoopCommand`/`executeCommand` bleiben mit der alten String-Signatur
bestehen und rendern den Strom zurück (`renderEvents`) — byte-identisch zum
Vorher-Stand (deshalb blieben alle 363 Engine-Tests und die CLI-Auxpfade
unverändert). Das Protokoll (1.4) und die WebUI (Phase 3) konsumieren
`applyLoopCommandEv` direkt.

## Styling-Entscheidung (die mit 1.2 zwingend zu treffen war)

**Prosa ist ANSI-frei; Farbe/Attribut kommt als Spans.**

- `StyledText { stText, stSpans }`; ein `Span { spStart, spLength, spStyle }`
  referenziert Indizes im Klartext, `Style` trägt `Maybe Color` plus
  Bold/Dim/Italic/Underline.
- Die CLI rendert Spans **am Rand** zu ANSI (`styleToAnsi`/`renderStyled` in
  `Types/Output.hs`) — das ersetzt langfristig die heute in Strings
  eingebetteten Codes; `ansiFilter` bleibt bis auf Weiteres der
  No-Color-Pfad an der Frontend-Grenze.
- **Kunst ist die dokumentierte Ausnahme:** authoring-seitige Art (img2ascii,
  `ascii:`-Blöcke, Kampf-/Karten-Screens) enthält legitim ANSI (Farben pro
  Zeichen, Hotspot-Marker `\ESC[1;33m…\ESC[0m`). Sie reist als `EvArt` mit
  **rohem** Text (`apRaw`) **plus** strukturierten Hotspot-Ankern
  (`apHotspots :: [ArtHotspot { ahIndex, ahGlyph, ahTarget }]`), sodass
  grafische Frontends nicht parsen müssen: Klick-Ziele kommen aus der
  Payload, der Block geht unverändert in ein `<pre>`-Äquivalent.
- Heute erzeugen alle Event-Erzeuger Prosa **ohne** Spans (die Meldungen der
  Engine sind unstyled); die Span-Infrastruktur ist getestet und wird von
  1.4 (Protokoll) und 3.2 (Vorschau-Tab) befüllt. Bekannter Altlast-Pfad:
  `highlightHotspots` bettet Hotspot-Marker in die Art ein — das ist
  Art-Content und bleibt in `apRaw`.

## Fragment-Algebra (warum die Ausgabe byte-identisch bleibt)

Die alte Pipeline setzte Nachrichten mit vier Join-Idiomen zusammen; die
Fragment-Algebra in `Types/Output.hs` repliziert jedes exakt:

| Idiom (alt, String) | neu (Events) |
|---|---|
| `joinMessages acc m` — `\n` zwischen Nichtleeren | `joinEv acc m` |
| direktes `++` | Listenanhang `++` |
| `unlines xs` — trailing `\n` **inkl. Leerstücken** | `unlinesEv` |
| `intercalate "\n" xs` — **inkl.** Leerstücken | `evIntercalate` |
| `combineMsgs` = `unlines . filter (not . null)` | `combineMsgsEv` |

`renderEvents = concatMap evTextOf` — Nicht-Text-Events (Audio, State-Events)
contribuieren `""` und verschwinden aus der CLI-Ausgabe. Die 39 E2E-Golden-
Playthroughs sind vor/nach der Umstellung byte-identisch (`diff -r` leer).

## Event-Reihenfolge (für 1.4 fixiert)

1. **Tick-Nachrichten** (Bedingungs-/Fahrzeug-Ticks; `unlinesEv`-Semantik,
   d. h. mit trailing `\n` — historisch gewachsen, im Goldens festgehalten)
2. **Befehls-Nachrichten** (Katalog-Meldungen mit Key, Rohtexte, Art-Payloads
   in der Reihenfolge ihrer Erzeugung)
3. **Trigger-Nachrichten** (`fireCommandTriggers`; historisch mit trailing
   `\n` pro Effekt — repliziert)
4. **Side-Events** (additiv, textfrei): `EvRoomChanged`, `EvQuestUpdate`,
   `EvDialogue`, `EvCombat`, `EvGameOver`, dann `EvSfx`/`EvMusicStart`/
   `EvMusicStop`/`EvAnim` aus den Pending-Queues des Zustands.

## Konsequenzen für die Folge-Stufen

- **1.3 (purer Session-Automat):** `applyLoopCommandEv` ist die rein rechnende
  Schnittstelle, die der Session-Automat umbaut; Save/Load/Meta bleiben als
  IO-Wünsche darin.
- **1.4 (Protokoll):** `ServerMsg "events"` serialisiert `[OutputEvent]` 1:1
  (JSON über die vorhandenen `Generic`-Typen); `snapshot` liefert die
  HUD-/Map-/Quest-/Dialog-Zustände, die die State-Events nur ankündigen.
- **2.3 (Disambiguation):** die Rückfrage ist heute `disambiguate.prompt`
  (Katalog-Text); mit Events kann sie später als eigenes Event mit
  strukturierten Kandidaten ausgedrückt werden — Protokoll-Upgrade, kein
  Umbau.
- **Nicht umgestellt (bewusst):** `pendingNarrative`/`pendingCutscene`
  (Loop-seitige Präsentation mit Pausen; zieht mit 1.3 in die Session-Logik),
  die Save-/Load-/Meta-**Drucke** in `SaveLoad.hs` (direktes IO via
  `putStrLn`, kein Teil des Befehlsstroms — mit 1.3 zu IO-Wünschen), und die
  Start-/Titelbanner der CLI (`Main.hs`).
