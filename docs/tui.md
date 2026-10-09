# TUI — Brick-Terminaloberfläche

> Die TUI ist das **optionale** Frontend. Die Haskeline-CLI bleibt der Standard; alles
> hier beschriebene gilt für `--tui` bzw. das Executable `text-adventure-tui`.
> Stand: Oktober 2026 (Rogue Phase 5, Audio Phase 1/2 als No-ops).

## Start

```bash
cabal run text-adventure-cli   -- --tui [--world world.json --save save.json]
cabal run text-adventure-tui   -- [--world world.json] [--save save.json] [Optionen]
```

| Option | Bedeutung |
|---|---|
| `--world FILE` | GameWorld aus einer worldbuilder-JSON laden (gleiche Validierungsgate wie die CLI) |
| `--save FILE` | Start-SaveState laden |
| `--saves-dir DIR` | In-Game-Speicherung (`saves/<slot>.json`) umleiten (gewinnt über `TA_SAVES_DIR`) |
| `--allow-invalid` | Trotz Validierungsproblemen starten |
| `--no-audio` | SFX abschalten — wird nur der Konsistenz halber akzeptiert (die TUI spielt ohnehin kein Audio) |
| `--help` | Kurzhilfe |

Ohne `--world` startet die mitgelieferte Sample-Adventure.

## Layout

```
┌ [Karte: Ebene 0 (hier)] ┐ ┌ [Status] West of House | Score: 12 ┐
│  ASCII-Minimap, Fog     │ │  HP-Balken, Variablen-Gauges,      │
└─────────────────────────┘ │  Conditions, Equipment             │
                            └────────────────────────────────────┘
                            ┌ [Kampf] ┐ (nur bei laufendem Kampf)
┌ West of House ──────────────────────────────────────────────────┐
│ Narrative-History (scrollbarer Viewport)                        │
└─────────────────────────────────────────────────────────────────┘
┌ Szene / <Raumname> ┐     Art-Panel (nur mit Inhalt, D21)
│ animiertes ASCII    │
└─────────────────────┘
  Vorschlagszeile
> _                                                Eingabezeile
  Hilfezeile
```

- **D21-Prinzip:** Ein Panel ohne Inhalt zeichnet **keinen** Kasten — kein HUD, keine
  Karte, kein Kampf, kein Art-Panel verschwindet nie, ohne dass die Fläche wegbricht.
- **Karte (HUD links):** ASCII-Minimap der **besuchten** Räume mit Fog of War,
  `(hier)`-Marker, dynamische Exits. Mehretagige Welten: per **F2/Ctrl-F** zwischen
  erkundeten Ebenen umschalten; der Label zeigt die Ebene.
- **Status (HUD rechts):** Raumtitel, Score-Statuszeile (siehe Konvention unten),
  HP-Balken, numerische Variablen als Balken, aktive Conditions (mit `hidden: true`
  markierte bleiben unsichtbar), Equipment. **Kampf-Panel** darunter, nur im Kampf.
- **Art-Panel:** Cutscene (blockierende Szene) oder Ambient-Loop des aktuellen Raums,
  fps-getaktet aus `room_ascii`.

## Bedienung

| Taste | Wirkung |
|---|---|
| `Enter` | Befehl absenden |
| `Tab` | Vervollständigung (reines `completionFor` der Engine) |
| `↑` / `↓` | Befehlshistorie durchblättern |
| `PgUp` / `PgDn` | History scrollen |
| `F2` / `Ctrl-F` | Karten-Ebene wechseln |
| `Esc` / `Ctrl-C` | Beenden |

Es gibt keine Maus-Unterstützung und keine Farbkonfiguration.

## TUI-Statuszeilen-Konvention

- Existiert eine Variable `score`, hängt das Status-Panel ` | Score: N` an den
  Raumtitel (`[Status] West of House | Score: N`).
- `score` wird aus den HUD-Gauges (`numericBars`) und der Statustabelle
  (`statsTable`) gefiltert — keine Doppelanzeige.
- Der Befehl `score` selbst wird wie `inventory`/`recipes` als Lesebefehl behandelt
  (`consumesTurn = False`).

## Animations-Panel (Phase H)

`PanelState` = `PanelNone | PanelCutscene | PanelAmbient` mit den reinen Funktionen
`panelFrame`/`advancePanel` (ohne UI getestet):

- **Cutscene:** `clip:`/`watch`-Playback, blockiert den Loop bis zum letzten Frame
  (D11), geht dann in den Ambient-Loop des aktuellen Raums über („Kameraschwenk, die
  am Strand stehenbleibt und weiterwinkt") — oder wird geleert.
- **Ambient:** endlose Animation aus `room_ascii`, läuft solange der Raum aktuell ist,
  kostet keinen Scrollback (D17 greift in der TUI nicht).
- Leere Frames (nur Whitespace nach ANSI-Strip) zeichnen keinen Kasten (D21).
- Art wird mit `txt` gerendert — **nie umgebrochen**. Farben kommen als SGR-Sequenzen
  aus der Art-Pipeline (`img2ascii`/`text2ascii`), `TextAdventure.Tui.Color` parst und
  quantisiert sie auf das Terminalfarbschema.

## Architektur (für Modifikationen)

Die Engine ist frontend-agnostisch: `src/Frontend.hs` definiert den `Frontend`-Record
(`feEmitLine`, `feEmitRaw`, `feReadInput`, `feReadPlain`, `feReadPause`,
`fePlayFrames`, `feDiagnostics`, `fePlaySfx`, `feStartMusic`, `feStopMusic`).
`GameLoop.runGameWithFrontend` enthält keinen Terminal-Zugriff — Haskeline-CLI und TUI
sind zwei Implementierungen desselben Records; ein Web-Frontend kann ohne Änderung am
Spiel-Loop dazukommen.

Die TUI ist **Brick-UI-Thread + Worker-Thread** (Game-Loop):

- `TuiShared` (IORefs + MVars) teilt History, Game-Panel-State, Befehls-Queue und
  letzten `GameState`; ein `BChan TuiEvent` (`EvLines`/`EvEnded`/`EvArt`/`EvTick`/
  `EvHud`) weckt die UI.
- `feReadInput`/`feReadPlain` publizieren den GameState, feuern `EvHud` (HUD-Refresh,
  auch auf dem Game-over-Screen) und blockieren auf die nächste Zeile.
- Der Executable baut mit `-threaded` (Ticker-Thread via `forkIO` + BChan — in der
  vty-windows-Handprobe „Phase T" verifiziert).

Module: `TextAdventure.Tui` (Loop, Events, Zeichnung), `TextAdventure.Tui.Hud`
(`buildHud`/`mapGrid`/`statsLines` — reine Snapshot-Berechnung), `TextAdventure.Tui.Color`
(SGR-Parsing/AttrMap). `drawTui` und `TuiState` sind bewusst exportiert, damit Tests
offscreen rendern können.

## Grenzen (Stand)

- **Audio:** `fePlaySfx`/`feStartMusic`/`feStopMusic` sind in der TUI **No-ops** —
  Wiedergabe läuft in der CLI (Audio Phase 1/2).
- Der UI-Chrome (Prompts, Hilfezeile) bleibt absichtlich **unübersetzt** (i18n-Grenze:
  nur Spieltext ist lokalisiert).
- Nicht Teil der TUI: WASM-Export (Phase 5.1) und Synth-Tab (Phase 5.2) — beides
  Autorenplattform-Themen.

## Tests

21 Tests in `text-adventure-tui/test/Tests.hs` (offscreen, ohne Terminal): HUD-Balken,
Karte (statisch, dynamische Exits, mehretagig, Sandbox), Statustabelle, Panels,
Kampf-Deck-Lines, Score-Statuszeile, Panel-Frames (Cutscene/Blank), Ambient-Loops,
Cutscene-Ende, Raum-Auflösung, SGR-Sequenzen/Quantisierung/AttrMap.

```bash
cabal test text-adventure-tui-tests
```
