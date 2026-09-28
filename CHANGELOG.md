# Changelog

## Unreleased

### Aufräumen: vier ungenutzte Katalog-Keys entfernt + Katalog-Gate (Phase 1.1-Nachtrag)

- `map.dark`, `watch.dark`, `map.legend_header` und `card.play.on` standen im
  Katalog, wurden aber von keiner Aufrufstelle gerendert: im Dunkeln liefern
  `map`/`watch` die autorenkonfigurierbare `darkRoomEv`-Meldung, und die beiden
  anderen sind Dubletten bewusst geteilter Inline-Fragmente
  (`nl2 ++ evRaw "Legend:\n"` bzw. `evRaw (" on " ++ target)`). Der Katalog hat
  damit **182 Keys**; verdrahtet wurde bewusst nichts — das hätte eine
  Verhaltensänderung bzw. einen anderen Event-Strom bedeutet.
- Neues Gate `scripts/check-msg-catalog.sh`, eingehängt als CI-Stufe **2b**:
  es meldet (a) Katalog-Keys ohne Aufrufstelle und (b) Aufrufstellen mit
  unbekanntem Key — letzteres rendert sonst still den lauten `<msg:key>`-Fallback.
  Beide Richtungen mit gepflanzten Fehlern belegt (Gate rot, danach grün).
  POSIX-Shell, damit es auch auf dem Windows-Runner läuft.
- `docs/message-catalog.md`: die versprochene Katalog-Tabelle fehlte komplett —
  die Datei endete mit den hineingerutschten Shell-Zeilen des Generator-Versuchs
  (`EOF`, `cat >> …`). Jetzt erzeugt `scripts/gen-msg-catalog.py` die Tabelle
  (182 Zeilen, überlange Templates gekürzt) und hält die Kopfzahl synchron;
  Belegzahlen auf gemessen korrigiert (42 E2E-Eingaben statt 39, 363 statt 359).
- README: Worldbuilder-Testzahl 166 → **169** (gemessen).
- **Build wieder warnungsfrei:** Phase 1.1/1.2 hinterließ **17 Warnungen** —
  14 im Engine-Build (12 tote Import-Einträge bzw. `Types.Output`-Importe doppelt
  neben der `Types`-Fassade, dazu zwei durch den Umbau verwaiste Bindungen
  `Parser.withAscii` und `GameLoop.combineMessages`) und 3 im Test-Build (zwei
  redundante Importe, ein ungenutztes `ls1`). Sie blieben unentdeckt, weil der
  letzte `ci.sh`-Lauf des Batches auf dem Doku-Commit saß: `cabal` kompilierte
  nichts neu, also meldete der Warnungs-Gate nichts. Erst ein erzwungener
  Neuaufbau (`find . -name '*.hs' -exec touch {} +` vor dem Build) zeigt sie —
  der Gate greift eben nur, wenn wirklich kompiliert wird.
- Keine Verhaltensänderung: 363 Engine-Tests, 169 Worldbuilder, 20 TUI, CI grün.

### Strukturierte Ausgabe: OutputEvents + Styling-Modell (Phase 1.2)

- Neues Blatt-Modul `src/Types/Output.hs` (über die `Types`-Fassade exportiert):
  die Engine produziert ihre Ausgabe als **geordneten Event-Strom** statt
  flachem Text — `EvMessage` (Katalog-Key + Args + gerenderter Text),
  `EvText` (ANSI-freie Prosa mit `StyledText`/`Span`-Modell), `EvArt`
  (roher Kunstblock **plus** strukturierte Hotspot-Anker `ArtHotspot`),
  `EvAnim`/`EvSfx`/`EvMusicStart`/`EvMusicStop` und textfreie State-Events
  (`EvRoomChanged`/`EvQuestUpdate`/`EvDialogue`/`EvCombat`/`EvGameOver`,
  abgeleitet in `GameLoop.sideEvents` aus dem Zustandsübergang).
- **Primärpfad event-nativ:** `applyLoopCommandEv :: Command -> LoopState ->
  (LoopState, [OutputEvent])` und `executeCommandEv` (Parser) — der komplette
  Kern (Effects-Interpreter, Trigger, Combat-Resolver, Vehicles, Cards,
  Dialogue) liefert Events. **Kompatibilitätsform:** `applyLoopCommand`/
  `executeCommand` behalten die String-Signatur und rendern den Strom zurück
  (`renderEvents`) — dadurch blieben alle 363 Engine-Tests (4 neue 1.2-Tests:
  Fragment-Algebra, Styling-Renderer, Key-Durchreichung, Side-Events) und die
  Loop-Auxpfade unverändert.
- **Styling-Entscheidung (dokumentiert in `docs/output-events.md`):** Prosa
  ist ANSI-frei; Farbe/Attribute kommen als Spans und werden am Rand zu ANSI
  gerendert (`styleToAnsi`/`renderStyled`; ersetzt langfristig
  string-eingebettete Codes neben `ansiFilter`). **Kunst ist die dokumentierte
  Ausnahme:** authoring-seitige Art (img2ascii, Screens, Hotspot-Marker) enthält
  legitim ANSI und reist roh in `EvArt` — zusammen mit strukturierten Hotspots,
  damit grafische Frontends nicht parsen müssen. Kampf-/Karten-Screens sind
  jetzt `EvArt`-Payloads (die 1.1-Ausnahme ist damit eingesammelt).
- **Fragment-Algebra:** `joinEv`/`unlinesEv`/`evIntercalate`/`combineMsgsEv`
  replizieren die vier Join-Idiome der alten Pipeline byte-exakt
  (`joinMessages`, direktes `++`, `unlines` mit trailing `\n` inkl.
  Leerstücken, `intercalate` inkl. Leerstücken).
- **Keine Verhaltensänderung:** alle 41 E2E-Playthroughs vor/nach byte-identisch
  (39 Golden-Captures, `diff -r` leer — inkl. der Combat-Nachweis-Fixes:
  `combineMsgsEv` filtert leere Pieces, `unlines`-trailing-`\n` erhalten);
  alle 6 Testsuiten PASS (TUI-Tests grün), 0 Warnungen; WASM-Spike läuft mit
  Event-Strom weiterhin byte-identisch.
- Für 1.3/1.4: `applyLoopCommandEv` ist die reine Schnittstelle des Session-
  Automaten; das Protokoll serialisiert `[OutputEvent]` 1:1. Bewusst nicht in
  1.2: `pendingNarrative`/`pendingCutscene` (Loop-Präsentation → 1.3) und die
  Save/Load-Drucke (IO-seitig → 1.3 als IO-Wünsche).

### Message-Katalog (Phase 1.1)

- Neues Blatt-Modul `src/Messages.hs`: alle player-facing Engine-Meldungen sind
  in einen zentralen Katalog mit stabilen dotted Keys (`area.name`) gezogen —
  **186 Keys** (Inventar belegt: ~158 großgeschriebene Literale + ~30 klein-
  geschriebene Fragmente; zusammengesetzte Meldungen in Teil-Templates zerlegt).
  Das ist die Datengrundlage für Sprachpakete (`language: de`, `messages:`-
  Overrides, Phase 4.3) und für strukturierte Events (1.2).
- `Messages.renderMsg` rendert Templates über `formatStringWith` (aus `Game.hs`
  hierher verlagert, `Game.formatWithVars` unverändert): `{arg}`-Substitution mit
  denselben Modifikatoren wie bei YAML-Texten; unbekannte Keys rendern laut als
  `<msg:key>`; Engine-Templates interpolieren bewusst **keine** Spielvariablen.
  `type MsgId = String` (Katalog bleibt offen für YAML-Overlays).
- Umgestellt (je Teil-Commit nach Golden-Vergleich): `Parser.hs` (76 Keys),
  `GameLoop.hs`/`Frontend.hs` (18), `Cards.hs`/`Combat.hs` narrativ (41),
  `Vehicles.hs`/`Effects.hs`/`Quests.hs`/`Game.hs` (32), `SaveLoad.hs`/`World.hs`
  (19).
- Inventar mit Klassifikation: `docs/message-catalog.md` (generierte Tabelle aus
  `catalogEntries`). Bewusst nicht katalogisiert: Kampf-/Karten-**Screen**-Art
  (Boxen, HP-Balken, `combatScreenDefaults` — zieht in 1.2 in strukturierte
  Payloads), Worldgen-Content-Templates, interne `error`-Assertions, JSON-Tags,
  Env-Varnamen, `Sample.hs` (Adventure-Content).
- **Keine Verhaltensänderung:** alle E2E-Playthroughs wurden vor und nach dem
  Refactor gespielt und byte-identisch geprüft (39 Golden-Ausgaben pro Teil-
  Commit gegen den Vorher-Stand, `diff -r` leer); 359 Engine-Tests (2 neue
  Katalog-Invarianten-/renderMsg-Tests), 0 Warnungen, Save-/World-Formate
  unverändert.
- Hinweis: GHC 9.6.7 meldet einen unqualifizierten `Data.List.lookup`-Import
  fälschlich als redundant (`-Wunused-imports`) — `renderMsg` nutzt daher
  `List.lookup` qualifiziert.

### Refactor: Explizite Exportlisten für die vier Kernmodule (Phase 0.7)

- `Parser.hs`, `SaveLoad.hs`, `World.hs` und `Game.hs` haben jetzt eine
  vollständige Exportliste: **139 Funktionen** sind öffentlich,
  **79 interne Helfer** und der Typ `MetaFile` sind es nicht mehr.
- **Warum das mehr ist als Kosmetik:** `-Wunused-top-binds` gehört zu `-Wall`,
  meldet aber nur *nicht* exportierte Bindungen. Solange die Module alles
  exportierten, war toter Code dort strukturell unsichtbar — in Phase 0.5 fielen
  sieben ungenutzte Funktionen deshalb erst beim manuellen Suchen auf.
- `Worldbuilder.Types` bleibt bewusst ohne Liste (100 von 125 Namen werden von
  außen gebraucht; eine Liste dort verbirgt praktisch nichts).
- Keine Verhaltensänderung: 357 Engine-Tests, 19 Worldbuilder-Suiten, 41
  Playthroughs und Worldgen unverändert grün, 0 Warnungen.

### Fix: WASM-Spike-Treiber `run.sh` war nicht reproduzierbar

- `run.sh` exportierte `$HOME/.ghc-wasm/bin` — dieses Verzeichnis existiert nicht,
  `wasm32-wasi-cabal` und `wasmtime` wurden also nie gefunden. Jetzt wird
  `~/.ghc-wasm/env` gesourct, mit klaren Fehlermeldungen und einer Prüfung auf
  `data/`.
- Kopfkommentar von `copy.sh` korrigiert: erster Versuch, obsolet — und die
  Behauptung, `haskeline`/`directory` fehlten unter WASI, war falsch.
- Spike-Notiz: gemessene Modulgröße (5,9 MB) statt Schätzung, Abschnitt
  „Reproduktion“ ergänzt.

### WASM-Machbarkeits-Spike (Phase 1.0)

- Neues, nicht produktionelles Verzeichnis `wasm-spike/`: baut die Engine mit dem
  GHC-WASM-Backend und führt `applyLoopCommand` in einem WASI-Runtime aus
  (wasmtime und Node.js). Abnahme-Dokument: `docs/wasm-spike-2026-09.md`.
- **Kernbefund: machbar, besser als geplant.** Nicht nur die puren Kern-Module —
  die **unveränderte Produktions-Library** (alle 20 Module inkl. `Frontend` mit
  Haskeline, `SaveLoad`, `Audio`, `World`) kompiliert und läuft unter GHC 9.14
  (wasm32-wasi): die Ausgaben des Fahrprogramms (Sample-Spiel + kompiliertes
  `dark-feelable`-Adventure mit aeson-Deserialisierung von `world.json`/`save.json`)
  sind **byte-identisch** zu einem nativen GHC-9.6.7-Lauf.
- Die Bindist bringt `directory`, `filepath`, `process` und `haskeline`
  (WASI-gepatcht) bereits mit; ergänzt wurden nur `aeson`/`aeson-pretty` von Hackage.
- Blocker für den Web-Export (5.1) bleiben dokumentiert: Save/Load braucht im
  Browser JS-FFI statt WASI-Dateisystem, `Audio.hs` (process/ffplay) wird dort
  nicht gelinkt (Web Audio, Phase 5.2), Modulgröße 6 MB unkomprimiert.
- Aufwandskorrektur für 5.1: **eher M als XL** — das Spike-Risiko ist entfallen,
  es bleibt I/O-Glue über das Protokoll aus 1.4.
- Keine Änderung an Engine, Tests oder Adventures; CI unverändert grün.

### Refactoring: Monolithisches `Types.hs` aufgeteilt (Phase 0.6)

- Das 2184 Zeilen lange Modul `src/Types.hs` wurde in vier fokussierte Submodule mit vollständigen, expliziten Exportlisten aufgeteilt:
  - `Types.Core` (`src/Types/Core.hs`): Basis-Typen, ID-Aliase, Zustände, World, Engine-Events, Expressions/Predicates und Hilfsfunktionen.
  - `Types.Cards` (`src/Types/Cards.hs`): Kartentypen, Kartenziele, Deck-Ziele, `Card` und `DeckState`.
  - `Types.Vehicles` (`src/Types/Vehicles.hs`): Fahrzeugtypen, Haltestellen, Kraftstoff-Spezifikationen, `VehicleDef` und `VehicleState`.
  - `Types.Combat` (`src/Types/Combat.hs`): Kampfsysteme (`CombatProfile`, `NarrativeCombat`, `TacticalCombat`), Kampfbildschirme, Initiativregeln, Aktionen und Spieler-Fähigkeiten (`PlayerAbility`).
- **Re-Export-Fassade:** `src/Types.hs` bleibt als Re-Export-Fassade erhalten (`module Types (module Types.Core, module Types.Cards, module Types.Vehicles, module Types.Combat)`). Alle bisherigen Konsumenten können weiterhin unverändert `import Types` verwenden.
- **Zyklusauflösung über eine Boot-Datei:** Die gegenseitige Rekursion zwischen `Types.Core` und den spezialisierten Modulen wird über `src/Types/Core.hs-boot` plus drei `import {-# SOURCE #-}` aufgelöst. Das kompiliert korrekt, hat aber Kosten: über eine `SOURCE`-Grenze inlinet GHC nicht, und die Boot-Datei muss bei Änderungen an `Effect`, `AsciiArt` oder den ID-Aliasen manuell synchron gehalten werden. Zyklusfreie Alternative für später: ein `Types.Base` mit nur IDs/`Effect`/`AsciiArt`, das Core und die Submodule gemeinsam importieren. Als Folge der Boot-Auflösung wurde `noopEffect` als Alias ergänzt (Konstruktoren sind über eine Boot-Datei nicht verfügbar) — keine Verhaltensänderung.
- **Vollständige Semantik- und Format-Invarianz:** Keine veränderten JSON-Formate, Savegame-Strukturen oder Checksummen; alle 41 E2E-Playthroughs und **357 Engine-Tests** (357 PASS / 0 FAIL, vor dem Split wie danach gemessen) laufen unverändert durch. Unabhängig belegt: `world.json`/`save.json` von `thefog` und `world.json` von `demo` sind gegen einen Worktree auf `f941b46` byte-identisch.

### Aufräumen: sieben ungenutzte Engine-Funktionen entfernt (Phase 0.5)

- Ohne jeden Aufrufer in Engine, Tests, Worldbuilder, CLI und TUI — geprüft per
  Referenzzählung über alle `.hs`-Dateien: `getAllItemsInLocation`, `moveItemToRoom`
  und `unequipSlot` (`src/Game.hs`), `parseSimpleCommand` und `parseVerb`
  (`src/Parser.hs` — dünne Wrapper um die `…With`-Varianten, die weiter genutzt
  werden), `savesDirFor` (`src/SaveLoad.hs`, durch `metaDir` abgelöst) sowie
  `computeVar` (`src/Types.hs`).
- Der im Plan unter 0.5 genannte Punkt „`cleanedTgt` in `matchesNPCTarget` nutzen
  oder entfernen" war gegenstandslos: `cleanedTgt` wird dort verwendet
  (`src/Cards.hs:46,48`).
- Ursache für die Langlebigkeit des toten Codes: `Parser.hs`, `Game.hs`,
  `SaveLoad.hs` und `Types.hs` haben keine Exportliste, deshalb warnt `-Wall` dort
  nicht vor ungenutzten Top-Level-Bindungen.

### Fix: `ffmpegAvailable` warf bei fehlendem ffmpeg eine Ausnahme

- `readProcessWithExitCode` **wirft** eine `IOException`, wenn die Exe nicht auf dem
  PATH liegt — es liefert keinen Exit-Code zurück. `ffmpegAvailable` (`video2ascii`)
  hat das nicht abgefangen und stürzte auf Rechnern ohne ffmpeg ab, statt `False` zu
  liefern. Die Testsuiten konnten dadurch **nicht** wie vorgesehen überspringen: die
  `video2ascii`-Testsuite ließ jeden GitHub-CI-Lauf seit dem 22.09. rot werden, obwohl
  das Gate lokal grün war (hier ist ffmpeg installiert).
- Neu: `commandAvailable :: String -> [String] -> IO Bool` fängt die Ausnahme ab und
  ist exportiert; `ffmpegAvailable` baut darauf auf und ist damit total. Neuer Test
  `testMissingCommandIsNotAvailable` prüft ein nicht existierendes Binary.

### Feature: Validierungs-Warnungen im Worldbuilder (Phase 0.4)

- **Drei neue nicht-fatale Compiler-Warnungen (`worldbuilder/src/Worldbuilder/Compile.hs`):**
  - **`KeywordCollision`:** Prüft pro Raum, ob Items und/oder NPCs identische Identifikatoren (ID, Name oder `keys`-Aliase) besitzen. Warnt vor potenziell mehrdeutigen Spielerkommandos wie `take <name>` oder `examine <name>`.
  - **`UnknownPlaceholder`:** Durchsucht alle Textinhalte (Beschreibungen von Räumen, Items, Quests, Karten, Dialogen, Raumnachrichten, Verbbeschreibungen, ASCII-Art und Aktionsergebnisse wie `msg`, `shout`, `teleport`, etc.) nach `{name}`- oder `{var:name}`-Platzhaltern. Gleicht diese mit deklarierten `variables`, Quests sowie bekannten Engine-Variablen (`player.*`, `turn.*`, `room.*`, `cmd.*`, `{x}`, `{y}`, `{z}`) ab. Maskierte Klammern (`\{...\}` bzw. `{{...}}`) werden ignoriert.
  - **`DarkRoomDeadEnd`:** Erkennt potenzielle Autoren-Sackgassen in dunklen Räumen (`dark: true` oder Tag `"dark"`). Warnt, wenn in einer dunklen Raumkomponente Items liegen, aber weder ein `light_flag` am Raum definiert ist, noch eine erreichbare Lichtquelle (`tags: [lightsource]`) existiert und kein Item im Raum als `feelable` gekennzeichnet ist.
- **Nicht-fatale Compiler-Diagnosen:** Alle drei Prüfungen sind als Warnungen mit `ciSeverity = SWarning` und stabilen `ciCode`-Strings realisiert. Weder `worldbuilder validate` noch `worldbuilder compile` schlagen fehl (Exit-Code 0 bleibt erhalten).
- **Reparatur-Hinweise (`worldbuilder/src/Worldbuilder/CLI.hs`):** Spezifische `repairHint`-Meldungen für alle drei neuen Warn-Codes unterstützen Autoren bei der schnellen Behebung im Terminal.
- **Tests & Qualitätssicherung (`worldbuilder/test/Tests.hs`):** 3 neue Worldbuilder-Tests (`testWarningKeywordCollision`, `testWarningUnknownPlaceholder`, `testWarningDarkRoomDeadEnd`), Gesamtanzahl der Worldbuilder-Tests steigt von 166 auf 169 (alle bestanden). Alle 26 mitgelieferten Adventures validieren und kompilieren ohne jede Warnung.
- **Dokumentation (`docs/adventure-schema.md`):** Detaillierte Abschnitte und tabellarische Übersicht über Compiler-Warnungen und Reparaturhinweise ergänzt.

### Feature: `feelable` — Autoren entscheiden, was im Dunkeln erreichbar ist (Phase 0.3-Ergänzung)

- **Neuer Item-Tag `"feelable"` (`src/Parser.hs`):** Ein Item mit diesem Tag lässt sich
  im Dunkeln ertasten und ist damit von der Dunkelheits-Sperre ausgenommen. Der Autor
  entscheidet pro Objekt, was in einem unbeleuchteten Raum erreichbar ist — Fackel,
  Schlüssel, Hebel, gleich welches. Ohne den Tag bleibt die 0.3-Regel unverändert.
- Betroffene Stellen: `ITItem`-Wache in `executeCommand (Interact …)`, der
  Mehrdeutigkeits-Pfad `ITAmbiguous` (Ausnahme nur, wenn alle Kandidaten getragen oder
  `feelable` sind), `use <getragen> on <raum-objekt>` sowie `take all` (nimmt im Dunkeln
  nur die `feelable`-Items) und `search <ziel>` (ziel-loses `search` bleibt gesperrt).
- **Tests:** 5 neue Engine-Tests (jetzt 357 Engine-Tests gesamt):
  `testFeelableItemReachableInDark`, `testTakeAllInDarkTakesOnlyFeelable`,
  `testSearchFeelableTargetInDark`, `testFeelableUseOnInDark`,
  `testFeelableAmbiguityInDark`.
- **Fixture & Doku:** Neues E2E-Fixture `examples/fixtures/dark-feelable.yaml` mit
  `ci/e2e/dark-feelable.{in,expect}` (Stufe 4 in `scripts/ci.sh`, damit 41
  Playthroughs): dunkler Raum, `take relic` (ohne Tag) wird verweigert, `take torch`
  und `use lever` (beide `feelable`, der Hebel ungetragen) funktionieren und der Hebel
  erhellt den Raum über `light_flag`. `docs/adventure-schema.md` dokumentiert den Tag,
  `README.md` nennt die neue Playthrough-Zahl.

### Bugfix & Feature: Licht-Leck schließen und konfigurierbare Dunkelheitsmeldung (Phase 0.3, Bug B3)

- **Licht-Leck schließen (Bug B3) (`src/Parser.hs`, `src/GameLoop.hs`):**
  - Befindet sich der Spieler in einem dunklen Raum (`isDark room state == True`), werden Aktionen auf nicht-getragene Raum-Objekte (`take <item>`, `take all`, `examine <item/npc>`, `search`, `use <item im Raum>`, `use <getragen> on <raum-objekt>`) konsequent verweigert und liefern die Dunkelheitsmeldung.
  - Auch nicht-existente oder ungetragene mehrdeutige Ziele im Dunkeln lecken keine Rauminformationen (kein „You don't see any ..." oder Disambiguierungs-Prompt), sondern geben direkt die Dunkelheitsmeldung zurück.
  - `GameLoop.commandEvents`: Die Events `OnLook` und `OnSearch` werden im Dunkeln unterdrückt; `OnUse` feuert im Dunkeln nur noch für tatsächlich getragene Inventar-Items.
  - Das Durchsuchen (`search` / `searchRoom`) im Dunkeln deckt keine versteckten Items auf und führt den Raum-Search-Hook nicht aus.
  - Getragene Items im Inventar können im Dunkeln weiterhin gefahrlos untersucht (`examine`), benutzt (`use`), kombiniert (`use X on Y`) oder abgelegt (`drop`) werden.
  - `Parser.resolveTarget`: Im Dunkeln priorisiert auch `examine` das Inventar, damit ungesehene Raumobjekte mit gleichem Keyword getragene Gegenstände nicht überschatten.
  - Erhellung: Tragen eines Gegenstands mit Tag `"lightsource"` oder Aktivierung des konfigurierten `light_flag` des Raums auf `"true"` stellt die vollständige Interaktionsfähigkeit wieder her.
- **Konfigurierbare Dunkelheitsmeldung (`src/Types.hs`, `worldbuilder/`):**
  - Neues Feld `roomDarkMsg :: Maybe String` am `Room`-Record.
  - Fallback-Kette: Verwendet `roomDarkMsg room`, andernfalls den Default `"It's pitch black. You can't see anything."`.
  - JSON-Serialisierung: Default-Invariante gewahrt (`Nothing` wird in JSON weggelassen). Abwärtskompatibel zu `"roomDarkMsg"`, `"dark_msg"` und `"dark_message"`.
  - Worldbuilder: Unterstützt `dark_msg` und `dark_message` im Raum-Schema von YAML-Dateien mit Validierung in `knownKeys EntRoom`.
- **Tests & Schema (`test/Tests.hs`, `worldbuilder/test/Tests.hs`, `docs/adventure-schema.md`):**
  - 8 neue Engine-Tests in `test/Tests.hs` (damit 352 Engine-Tests gesamt):
    - `testDarkRoomRefusesTakeAndTakeAll`: Verweigerung von `take` und `take all` im Dunkeln.
    - `testDarkRoomRefusesExamineAndSearch`: Verweigerung von `examine` (Item & NPC) sowie `search` im Dunkeln; Unterdrückung von `OnSearch` und `OnLook`.
    - `testDarkRoomRefusesUseOnRoomEntities`: Verweigerung von `use` auf Raum-Items und `use X on Y` auf Raum-Ziele.
    - `testDarkRoomAllowsCarriedItemInteractions`: `examine`, `use` und `drop` auf getragene Inventar-Items funktionieren auch im Dunkeln.
    - `testDarkRoomCarriedItemNotShadowedByRoomItem`: Getragene Items werden bei `examine` im Dunkeln nicht von ungesehenen Raum-Items mit gleichem Alias verdeckt.
    - `testDarkRoomIlluminationRestoresInteraction`: Erhellung via `lightsource`-Item oder `light_flag` stellt Interaktion wieder her.
    - `testConfigurableDarkMessage`: Eigene Raumnachricht `roomDarkMsg` greift bei allen blockierten Aktionen.
    - `testRoomDarkMsgJsonRoundTrip`: JSON Round-Trip und Abwärtskompatibilität für `dark_msg` / `dark_message`.
  - 1 neuer Test in `worldbuilder/test/Tests.hs` (jetzt 166 Worldbuilder-Tests gesamt):
    - `testRoomDarkMsgYamlParsing`: YAML-Parsing und Kompilierung von `dark_msg` und `dark_message` ohne Warnungen.
  - Dokumentation von `dark_msg` / `dark_message` und der Dunkelheits-Regeln in `docs/adventure-schema.md`.

### Bugfix & Verhaltensänderung: Verbabhängige Suchreihenfolge (Phase 0.2, Bug B2)

- **Verbabhängige Suchreihenfolge `preferInventoryTarget` (`src/Parser.hs`):**
  - Behebt Bug B2: `drop`/`use`/`equip` (sowie `wear`, `wield`, `unequip`, `remove`) priorisieren nun das Spielerinventar vor dem aktuellen Raum. `take` priorisiert wie gewohnt den Raum vor dem Inventar; sonstige Interaktionsverben (`look at`, `examine`, `attack`, etc.) priorisieren ebenfalls den Raum.
  - Befindet sich mindestens ein Treffer im primären Scope, wird der sekundäre Scope gar nicht erst durchsucht. Dadurch scheitert z. B. `drop key` nicht mehr daran, dass ein namensgleicher Gegenstand im Raum liegt (oder dass eine Mehrdeutigkeits-Rückfrage gestellt wird).
  - Findet sich im primären Scope kein Treffer, fällt die Zielauflösung auf den sekundären Scope zurück, sodass kontextbezogene Rückmeldungen („You already have the brass key.“ bei `take key` oder „You need to be carrying the ...“ bei `equip`) erhalten bleiben.
  - `EquipCmd` und `UnequipCmd` in `Parser.executeCommand` wurden an `resolveTarget` angebunden:
    - Befehls-Variablen (`cmd.verb`, `cmd.arg1`, etc.) werden via `bindCommandVars` gebunden.
    - Mehrdeutige Treffer bei `unequip` werden anhand von `isEquipped` gefiltert: ist genau ein Kandidat ausgerüstet, wird dieser direkt abgelegt ohne unnötige Rückfrage zu im Rucksack getragenen Namensvettern. Sind mehrere ausgerüstet, beschränkt sich die Rückfrage auf die ausgerüsteten Gegenstände.
    - Mehrdeutige Treffer bei `equip` werden nach Ausrüstbarkeit (`itemEquipSlot`) gefiltert, sodass nicht ausrüstbare Gegenstände mit selbem Alias (z. B. Klingenöl vs. Stahlklinge) keine störende Rückfrage erzwingen.
- **Tests (`test/Tests.hs`):**
  - 10 neue Testgruppen (jetzt 344 Engine-Tests):
    - `testResolveTargetSearchOrderDirect`: Direkte Prüfung von `preferInventoryTarget` und Auflösung je Verb.
    - `testDropKeyWithRoomNamensvetterFixB2`: `drop key` lässt carried key fallen, wenn Namensvetter im Raum liegt; `OnDrop`-Trigger feuert korrekt.
    - `testTakeKeyWithInventoryNamensvetterFixB2`: `take key` nimmt Raum-Item, wenn Namensvetter im Inventar getragen wird; `OnTake`-Trigger feuert korrekt.
    - `testUseKeyWithRoomNamensvetterFixB2`: `use key` nutzt Inventar-Item vor Raum-Item; `OnUse`-Trigger feuert korrekt.
    - `testEquipWithRoomNamensvetterFixB2`: `equip blade` rüstet getragene Waffe aus, auch wenn Namensvetter im Raum liegt.
    - `testUnequipWithRoomNamensvetterFixB2`: `unequip blade` legt getragene Waffe ab, auch wenn Namensvetter im Raum liegt.
    - `testUnequipAmbiguityFiltersEquipped`: `unequip` filtert Mehrdeutigkeiten nach `isEquipped`-Status.
    - `testEquipAmbiguityFiltersEquippable`: `equip` filtert Mehrdeutigkeiten nach Ausrüstbarkeit.
    - `testSearchOrderFallbacks`: Rückfall auf sekundären Scope liefert saubere Fehlermeldungen bei nicht erfüllten Vorbedingungen.
    - `testSearchOrderAmbiguityScoped`: Disambiguierungs-Fragen für `drop`, `take`, `use` und `equip` beschränken sich auf die Treffer des primären Scopes und schließen irrelevante Namensvetter aus.

### Bugfix & Refactor: Zentrale Zielauflösung `resolveTarget` (Phase 0.1, Bug B1)

- **Zentrale Zielauflösung `resolveTarget` (`src/Parser.hs`):**
  - Neuer Typ `TargetResolution` (`ResolvedItem`, `ResolvedNPC`, `ResolvedVehicle`, `Ambiguous`, `NotFound`, `BareVerb`) mit Pattern-Synonyms (`TargetItem`, `TargetVehicle`, `TargetAmbiguous`, etc.).
  - `resolveTarget :: Verb -> String -> GameState -> TargetResolution` löst Zielobjekte einheitlich über sichtbare/erreichbare Items im aktuellen Raum und Inventar sowie NPCs im Raum und attackierbare Fahrzeuge auf.
  - Mehrdeutige Treffer liefern strukturiert `Ambiguous [EntityID]`, was den Spieler via `Which do you mean:` zur Präzisierung auffordert. Mehrdeutige Fahrzeuge auf `VAttack` werden ebenfalls als `Ambiguous` erkannt.
  - `TakeAll` und `DropAll` wurden darauf umgestellt, die eindeutige `itemId` statt `itemName` bei der rekursiven Interaktionsausführung zu nutzen, sodass Räume oder Inventare mit namensgleichen Gegenständen nicht fälschlich in `Ambiguous`-Rückfragen verfallen.
- **B1 behoben (`src/GameLoop.hs`):**
  - `commandEvents` (`takeDropUseEvents`) nutzte zuvor das globale `findItemIdByAlias`, das unbesehen das erste Item der gesamten Welt mit passendem Alias lieferte. Haben zwei Items in verschiedenen Räumen denselben Alias, wurden `OnTake`/`OnDrop`-Events für das falsche Item gefeuert oder `OnUse` fehlgeleitet.
  - Ersetzt durch `resolvedItemId`, das `resolveTarget` auf `before`- (und Fallback auf `after`-)Zustand anwendet.
  - `findItemIdByAlias` entfernt.
- **Tests (`test/Tests.hs`):**
  - 6 neue Testgruppen/Suiten (8 neue Engine-Tests gesamt, jetzt 336 Engine-Tests): direkte Zielauflösung (`testResolveTargetDirect`), B1 OnTake-Fix mit identischen Keywords in zwei Räumen (`testResolveTargetFixesB1OnTake`), Disambiguation im GameLoop (`testResolveTargetAmbiguousCommandExecution`), Drop/Use-Auflösung und End-to-End Trigger-Ausführung bei weltweitem Namensvetter (`testResolveTargetDropAndUseFixB1`), `TakeAll`/`DropAll` mit geteilten Keywords (`testTakeAllAndDropAllWithSharedAliases`), Mehrdeutigkeit bei Fahrzeug-Angriff (`testResolveTargetVehicleAmbiguity`).

### Refactor: Code-Hygiene R1–R4 — Parser-Dispatcher, `Game.hs`-Split, `ActorRef`

- **R4 — Modul-Leitfaden auditiert** (`docs/modules.md`): Erlaubnisfall für
  Teilmengen (Card, Audio) dokumentiert, Namensraum-Disziplin als Grundsatz
  ergänzt (reservierter Präfix + Clash-Check im selben Pass), Stufe-5-Liste im
  CI-Abschnitt nachgezogen (`9d2ac93`).
- **R3 — `Parser.executeCommand` entflechtet:** die `Interact`-Klausel ist jetzt
  ein Dispatcher über `resolveInteractTarget` mit `interactItem`/`interactNpc`/
  `interactVehicle`/`interactBare`/`interactNotFound`. Neue Tests pinnen die
  Zielauflösung und den „Basisaktion **und** Effekt"-Vertrag von `take`
  (`5435ec6`).
- **R2 — `Game.hs` aufgeteilt**, von ~2.400 auf 1.345 Zeilen, in vier Etappen:
  `Quests.hs` (`8820570`), `Vehicles.hs` (`2e393a8`), `Effects.hs` (`626d7ce`),
  `Cards.hs` (`b6e8651`). Die Basis-Schicht (Zustand, Lookups, Party, VarMap,
  Entity-States) bleibt in `Game.hs`.
- **R1 — `Location` und `Predicate.Location` typisiert:** `CarriedBy`/`EquippedBy`
  und `Predicate.Location` tragen `ActorRef` statt freier Entity-ID-Strings;
  der Worldbuilder kompiliert `AOEquipItem`/`AOGiveItem` zu `E.ActorPlayer`.
  **Kein Save-Bump** — `currentSaveVersion` bleibt 3, Legacy-`"player"` wird beim
  Dekodieren gemappt, das YAML-Format für Autoren ist unverändert. Neue
  Round-Trip- und Typo-Tests (`c7ed1b1`).
- **Keine Verhaltensänderung:** `scripts/ci.sh` nach jeder Etappe grün,
  E2E-Playthroughs byte-identisch.

### Feature: Multi-Panel-TUI — Karte, Status-HUD, Kampf-Panel (Rogue Phase 5)

- Die Brick-Oberflaeche (`--tui`) zeigt ueber dem Narrative-Viewport ein
  HUD: ASCII-Minimap der besuchten Raeume (links), Status-Panel mit
  HP-Balken, deklarierten numerischen Variablen als Gauges, Zustaenden
  und Ausruestung (rechts), konditionales Kampf-Panel darunter
  (sichtbar ab `combat.engaged >= 1` — Runde, Aktion, Gegner-HP).
- D21-Prinzip durchgehalten: Panel leer => keine Box. Ohne Kampf /
  Ausruestung / besuchte Raeume ist das Spiel visuell unveraendert.
- Minimap-Verbindungen laufen ueber Phase-3 `effectiveConnections`:
  `set_exit`/`remove_exit` formen die Karte wie statische Ausgaenge.
- M10 geschlossen: `feReadPlain` erhaelt jetzt den GameState — das HUD
  bleibt auch auf dem Death-/Victory-Screen aktuell (statt einzufrieren).
  Alle 3 Frontend-Implementierungen angepasst; Haskeline-Verhalten
  byte-identisch.
- Tests: 5 neue HUD-Unit-Tests (Balken, Kompass-Lattice, dynamische
  Exits, Panels, Status-Zeilen); TUI-Suite 15 Tests.

### Feature: Dynamische Ausgaenge — `set_exit` / `remove_exit` (Rogue Phase 3)

- Neue Engine-Effects `SetExit <room> <dir> <exit>` und `RemoveExit <room> <dir>`:
  Regeln/Trigger öffnen, verlegen oder schliessen Raumausgaenge zur Laufzeit.
- `SaveState.exitOverrides` (Map über `(RoomID, Direction)` auf `Maybe Exit`):
  `Just exit` ersetzt die statische Verbindung, `Nothing` entfernt sie — auch
  statisch existierende. Kodiert als Objektliste (`exit: null` = entfernt);
  das Feld wird nur bei Nicht-Leere geschrieben (bestehende Saves bleiben
  bit-identisch).
- `Game.effectiveConnections`: die eine Runtime-Lookup für Ausgaenge —
  Bewegung, Parser-Tuer-Aliase und Tab-Completion sehen dynamische Ausgaenge
  identisch. Ein per `set_exit` gesetzter `locked_by`-Ausgang legt seinen
  Entity-State beim Setzen lazy als `locked` an.
- Worldbuilder: `set_exit: {from, dir, to, locked_by?}` und
  `remove_exit: {from, dir}` als Outcomes; `checkSetExitRefs` validiert
  Richtungen (UnknownDirection) und Raeume (MissingRoom) ueber alle
  autorenbaren Outcome-Baeume; die statische Erreichbarkeitspruefung
  akzeptiert nur per `set_exit` erreichbare Raeume.
- Tests: testDynamicExitOverrides (set/remove/rewire, lazy Locked-Seeding,
  Save-Round-Trip), testSetExitCompiles (Compile + beide Validierungen).


### Feature: Meta-Progression — `meta.*`-Variablen (Rogue Phase 2)

- `GamePolicy` um `gpMetaSlug` erweitert: `game.meta_slug:` überschreibt den
  Slug für die Meta-Datei explizit (M8) — Titel-Umbenennungen stranden den
  Fortschritt nicht mehr; die Engine wendet `slugify` (idempotent) an.
- Neu in `SaveLoad`: `adventureSlug`, `metaSavePath` (`saves/<slug>_meta.json`),
  `saveMeta`/`loadMeta` (filtert auf `meta.*`; leere Map schreibt nichts —
  Adventures ohne Meta-Progression erzeugen keine Datei), korrupte Meta-Datei
  warnt und startet neu statt zu crashen.
- GameLoop: `persistMeta` beim Spielende (Sieg/Tod/Custom/Quit);
  `carryMetaVars` trägt beim Restart die `meta.*`-Werte in den frischen Run;
  `mergeMetaFromDisk` erzwingt die M5-Vorrangregel (Meta-Datei gewinnt über
  Slot-Snapshots).
- Worldbuilder: `AGamePolicy` trägt `meta_slug` (durchgereicht); Meta-Doku in
  docs/adventure-schema.md.
- Tests: Restart-Carry (rein), Persistenz beim Tod, Slug-Override.


### Feature: Permadeath, Ironman & Savezones (Rogue Phase 1)

- Neuer optioneller YAML-Block `game:` (Worldbuilder-Schema `AGamePolicy`):
  `permadeath`, `allow_undo`, `ironman`, `save_zones` — alle Felder optional,
  Default = bisheriges Verhalten (Default-Invariante).
- **Permadeath:** beim Tod gibt es kein `[U]ndo`/`[L]oad` mehr, nur
  `[R]estart` | `[Q]uit`; `u`/`l`-Eingaben im Death-Screen werden abgewiesen.
- **`allow_undo: false`:** der `undo`-Befehl wird abgewiesen, die Undo-Historie
  wird dann nicht mehr aufgebaut.
- **Ironman + Savezones:** Speichern nur in autordefinierten Savezone-Räumen
  ("You can only rest at a savezone."), festes Slot-Modell
  (`SaveLoad.ironmanCheckpointSlot = "checkpoint"`). Beim Tod löscht die
  Engine genau diesen Checkpoint (`deleteSaveSlot`, idempotent); `load` ist
  gesperrt. `--save`-Startdateien bleiben unberührt (neutraler Wiedereinstieg).
- Validierung: unbekannte `save_zones`-Räume → `MissingRoom` (hart);
  `ironman` ohne `save_zones` → Warnung `IronmanWithoutSavezones` (legal,
  aber meist Versehen). Nicht-fatale Compiler-Diagnostik reisen neu über
  `CompileResult.crWarnings` zum Autor; der `compile`-Befehl zeigt sie an,
  ohne den Build abzubrechen.
- M2-Schutz: `ToJSON GameWorld` emittiert `"game"` nur bei Nicht-Default —
  World-Checksummen und damit alle bestehenden Saves bleiben bit-identisch.
- Tests: 4 neue Engine-Tests (undo-gate, death menu, savezone gate,
  Checkpoint-Löschung) + Worldbuilder-Tests (policy kompiliert, Warnung,
  MissingRoom).

### Feature: Rogue Phase 0 — Save-Isolation, `deleteSaveSlot`, `slugify`

- `SaveLoad.savesDir` ist jetzt `IO FilePath` und ehrt die Umgebungsvariable
  `TA_SAVES_DIR` (Default bleibt `saves/` relativ zum CWD — bit-identisch ohne
  Variable). Alle Slot-Operationen (`saveGame`, `loadGame`, `listSaves`,
  `saveSlotPath`) laufen durch dasselbe Verzeichnis; `savesDirFor` ist das
  Geschwister für die Meta-Progressions-Pfade (Rogue Phase 2).
- Neu `deleteSaveSlot`: löscht einen Slot (idempotent, `try`-Fehlerbehandlung) —
  die Naht für den Ironman-Checkpoint (Rogue Phase 1).
- Neu `slugify` in `Types.hs`: lowercase, `[a-z0-9_-]` bleibt, Rest wird
  `_`/Leerzeichen (kollabiert), leer/blank → `default` — Basiselement für
  `saves/<slug>_meta.json` (Rogue Phase 2, M8).
- Beide Executables (`text-adventure`, `text-adventure-tui`) akzeptieren
  `--saves-dir DIR`; das Flag gewinnt über `TA_SAVES_DIR`.
- `scripts/ci.sh`: jede E2E-Runde bekommt ihr eigenes `TA_SAVES_DIR` —
  save/load kann nicht mehr zwischen Läufen leaken oder das Repo verschmutzen.
- Tests: `withSavesIsolation`-Seam, `testSavesDirOverride`,
  `testSavesDirDefault`, `testSlugify`.

### Feature: `--tui` im Haupt-CLI — neues Paket `text-adventure-cli`

- Das Haupt-CLI ist nun das eigene Paket `text-adventure-cli` (Executable
  heißt weiter `text-adventure`, `cabal run text-adventure-cli`), mit neuem
  `--tui`-Flag (D20): es lädt Welt/Save identisch (inkl. `--allow-invalid`
  und Geschwister-save.json), lässt die Validierung zuerst laufen und startet
  dann die Brick-Oberfläche statt Haskeline — mit demselben Startbanner als
  initiale Zeilen. Ohne `--world` startet auch `--tui` das Sample.
- Warum das Paket? Ein Paket-Exe darf nicht von einem Paket abhängen, das
  die eigene Lib nutzt (`text-adventure`-Paket → `text-adventure-tui` →
  `text-adventure`-Lib wäre ein Zyklus). Das CLI-Paket hält die Abhängigkeit
  sauber: die Engine-Lib bleibt Brick-frei (D20), nur das Executable zieht
  Brick. Dafür ändert sich der Laufbefehl auf
  `cabal run text-adventure-cli` (docs/genres.md, README angepasst;
  `text-adventure-tui` bleibt als eigenständige TUI-Exe).
- Windows-Konsole-Init (W3, UTF-8/VT) wandert mit dem Exe mit und deckt
  jetzt beide Frontends ab. `scripts/ci.sh` (GAME) und CI-Job laufen über
  das neue Paket; `cabal build all` baut es automatisch mit.

### Feature: Farbe im TUI — SGR nach vty-Attribute (Restposten aus Phase T)

- Neu `TextAdventure.Tui.Color`: parst SGR-Sequenzen in (Text, Zustand)-
  Segmente (`parseSgrLine`) und bildet sie auf vty-Attribute ab
  (`attrOfSgr`). Abgedeckt: Basis-/helle 16 Farben, `38;5;n`/`48;5;n`
  (Palette), `38;2;r;g;b`/`48;2;r;g;b` (Truecolor — quantisiert auf die
  xterm-256-Palette: 6x6x6-Würfel, Graustufenband für r == g == b), Bold,
  Reset; unbekannte Codes und Nicht-SGR-CSI werden ignoriert.
- **Attribute sind benannt, nicht anonym:** Brick löst Farben über eine
  endliche AttrMap auf; der Name kodiert den SGR-Zustand deterministisch
  (`colorAttrName`), und die App baut die Map pro Render aus den Zuständen,
  die in den aktuellen Zeilen und im Panel tatsächlich vorkommen. Rein und
  testbar.
- Das TUI strippt SGR nicht mehr: Hotspot-Highlights (Phase E, bold gelb)
  und halbblockige Kunst (img2ascii, 24-bit) erscheinen farbig — im Verlauf
  als Segment-Zeilen (kein Umbruch, SGR kostet keine Spalten), im Kunst-Panel
  zeilenweise. Leere Zeilen (nur SGR) bleiben Leerzeilen; Leer-Frames zeichnen
  weiterhin keinen Rahmen (D21, Leerprüfung jetzt nach ANSI-Strip).
- Ein Endlosschleifen-Bug im Parser (unbeendetes ESC re-appended) wurde beim
  ersten Testlauf gefunden und gefixt. 5 neue Tests (SGR-Codes, Quantisierung
  inkl. xterm-Referenzwerten 196/46/21, Segment-Parsing, Namens-Injektivität,
  Attribut-Mapping); TUI-Suite jetzt 11 Tests. `scripts/ci.sh` grün,
  `-fforce-recomp`-Check ohne Warnungen.

### Phase F: `video2ascii` — Video als Kunst-Material (D6, D14, D15)

- Neues Werkzeug-Paket `video2ascii` (D9: eigener Stilkopie-Weg, keine
  Engine-Abhängigkeit): ffprobe liest Geometrie/Rate/Dauer, ffmpeg zieht
  Frames als rohes Graustufenmaterial, das Paket wandelt sie in ASCII
  (Zellmittelung statt Punktabtastung — Video-Rauschen flackert sonst).
  ffmpeg/ffprobe sind ausschließlich externe Prozesse **im Werkzeug**;
  an der Engine-Laufzeit ändert sich nichts (D6).
- Zwei Modi passend zu den beiden Abspielorten (D11):
  * `--ambient` — 4–30 Frames gleichmäßig über ein Zeitfenster, abgespielt
    mit der eigenen Rate des Fensters (periodegenauer Loop, D15); die Naht
    (letzte vs. erste Frame relativ zur Ø-Frameschritt-Distanz) wird gemessen
    und gemeldet, Ratio > 2 warnt. Ausgabe: einfügbares `ambient:`-YAML.
  * `--cutscene` — das ganze Material bei `--fps` als Clip-Datei im
    D14-Format (JSON-Array von Frame-Strings) plus `clips:`-Snippet;
    der Worldbuilder bettet sie beim Kompilieren ein.
- Fertig-wif belegt (Handlauf): ein generiertes 2-s-Testvideo liefert
  Material, das die Engine als Ambiente-Loop (`watch`) bzw. als Cutscene
  (`intro:` beim Betreten) in der Pipe abspielt — statisch, sequenzfrei.
  14 Tests (10 rein: Zeilenverhältnis, Rampe, Distanz, Naht, Zeitwahl,
  JSON-/YAML-Formate; 4 ffmpeg-Integration mit generiertem Video, skippen
  sauber ohne ffmpeg im CI).
- Schema-Doku um das Werkzeug-Kapitel ergänzt.

### Fix: `--world` ohne `--save` lädt die Geschwister-save.json

- Befund aus der H-Handprobe: eine kompilierte Welt startete ohne `--save`
  mit `MissingItemState` — die E2E-Läufe nutzen immer `--world + --save`
  (der Compiler schreibt das Paar), der Direktstart war aber der offensichtliche
  Weg. Neu: `World.siblingSavePath` findet die `save.json` neben der
  Weltdatei; `--save` gewinnt weiterhin, ohne Geschwister (In-Code-Welten)
  gilt der Engine-Default wie bisher. Beide CLIs (Haskeline + TUI) nutzen
  das; eine kompilierte Welt ist damit selbsttragend. Test: Auto-Discovery
  (mit/ohne Geschwister).

### Phase H4b: Kunst-Panel im TUI (D21) — in-place-Cutscene + Ambiente-Loop

- Neu `PanelState` (None / Cutscene einmal / Ambient-Loop) mit eigenem
  MVar-geteilten Zustand und Ticker-Thread, der die UI mit der Kunst-Rate
  (aus H1) weckt. Reine Helfer (`panelFrame`, `advancePanel`, `roomAmbient`)
  tragen die Logik und sind in einer eigenen TUI-Test-Suite abgesichert
  (6 Tests: Frame-Anzeige, D21-Leer-Regel, Loop-Wrap, Cutscene-Abschluss,
  Raum-Ambiente-Auflösung, ungültige Raten).
- **D21 erfüllt:** das Panel existiert nur, wenn es etwas zeigt — leere/
  Whitespace-Frames zeichnen keinen Rahmen; ein Spiel ohne Kunst ist im TUI
  vom reinen Textspiel nicht unterscheidbar. Kunst wird im Panel mit `txt`
  gerendert (nie umgebrochen).
- **H4-Übergang:** `fePlayFrames` spielt Cutscene/`watch` in-place im Panel
  ab (der Loop blockiert nur dort — D11 erlaubt das), und geht danach in den
  Ambiente-Loop des aktuellen Raums über bzw. räumt das Panel ab. Räume mit
  `ambient` loopen im Panel, sobald sie aktuell sind; Raumwechsel schaltet um.
- D17 (sticky/non-sticky) entfällt im TUI: das Panel ist isoliert, Ambiente
  kostet keinen Scrollback. D13/D16 sind damit obsolet (nie gebaut worden).
- Plain-CLI unverändert: E2E `ascii-state` läuft mit dem neuen
  hall-pan-Cutscene-Pfad sequenziell und sequenzfrei durch die Pipe (H6-
  Beleg). `scripts/ci.sh` grün (alle sechs Pakete, inkl. neuer TUI-Suite).

### Phase H4: Cutscenes — `clips`, `intro`, `play_clip`

- Neu `Clip` (Frames + `fps`) im `GameWorld.worldClips`; YAML `clips:`-Segment
  mit inline `frames:` oder `file:`-Begleitdatei (D14: beim Kompilieren
  eingebettet, Runtime liest keine Dateien; Pfaden relativ zum Adventure).
- `Room.roomIntro` (`intro: <clip-id>`): beim Betreten wird der Clip einmal
  geparkt (`pendingCutscene`, runtime-only, nicht im Save) — ein `play_clip`
  aus `on_enter` gewinnt gegen das Raum-`intro` (D19: beide Auslöser).
- Neu Effect `PlayClip <id>` (YAML `play_clip: <id>`), interpreter queueing
  the clip; GameLoop spielt queued Cutscenes einmal via `fePlayFrames` mit
  der Clip-Rate ab, dann Clear. Message zuerst, Cutscene danach.
- Validierung (stabile Codes): `DuplicateClip`, `UnknownClip` (in `intro:`
  und `play_clip:` — Referenzen aus der kompilierten Welt gesammelt, also
  inklusive Regeln/Hooks/Dialoge/Verb-Maps), `ClipFpsInvalid`, `ClipFramesEmpty`.
- Fixture `ascii-state.yaml`: `hall-pan`-Clip (fps 6) + `intro: hall-pan` an
  der Halle. Tests: intro beim Betreten parkt, `play_clip` parkt, der Loop
  spielt einmal ab (Canned-Frontend zeichnet mit); Worldbuilder: Kompilierung,
  alle vier Validierungscodes, D14-Datei-Einbettung. `adventure-schema.md`
  dokumentiert das Schema.

### Phase H1: Rate pro Kunst (`ambient` + `asciiPlayback`)

- Neu `Ambient` (Frames + `fps`) als Feld `aaAmbient` am `AsciiArt`: die Kunst
  trägt ihre Rate; YAML/JSON `ambient: {frames, fps}` (TUI und Haskeline-CLI
  konsumenten identisch, Rückwärtskompatibel — fehlendes `ambient` ändert
  nichts am JSON und damit am Welt-Checksum).
- Neu reine Funktion `asciiPlayback :: AsciiArt -> GameState -> ([String], Int)`
  (Game.hs): liefert Frames **und** Verzögerung in µs. Ambient-Kunst spielt
  ihre Rate (`1e6 div fps`); `frames`/`every`-Kunst fällt auf die dokumentierte
  Standardrate `defaultFrameMicros = 350000` zurück (der früheren festen
  Konstante entsprechend — jetzt als Default, nicht als Engine-Konstante).
- `pendingAnimation` trägt `(frames, rate)`; `fePlayFrames :: Int -> [String] ->
  IO ()` nimmt die Rate als Parameter — die 350-ms-Konstante ist aus dem
  Frontend entfallen, Haskeline-CLI und TUI warten auf die Kunst-Rate.
- Worldbuilder kompiliert `ambient:` und validiert: `AmbientFpsInvalid`
  (fps <= 0) und `AmbientFramesEmpty` (leere Frame-Liste) sind Compile-Fehler
  mit stabilen Codes. Die D15-Loop-Naht-Prüfung bleibt bewusst bei der
  Video-Schiene (F).
- Fixture `ascii-state.yaml`: die Halle trägt neben ihrem Zug-Takt einen
  Ambient-Loop (Wellen, fps 4). Tests: playback-Rate-Mathematik, `watch`
  übernimmt die Rate, JSON-Rundlauf; Worldbuilder: Kompilierung + beide
  Validierungscodes.

### Phase T (Grundgerüst): brick-basiertes TUI-Paket `text-adventure-tui`

- Neues Paket `text-adventure-tui` (lib `TextAdventure.Tui` + exe): das
  Abbruchkriterium von T ist geprüft — `vty-windows` baut und läuft auf echtem
  Windows (Handprobe des Autors, 2026-09-22; drei Brick-2.x-API-Anpassungen
  notiert: `appHandleEvent` ohne State-Argument, `mkVty` aus
  `Graphics.Vty.CrossPlatform`, `-threaded` für den Timer-Thread).
- Architektur wie geplant (D20/D22): der Engine-Loop läuft in einem
  Worker-Thread über das Phase-V-`Frontend`-Record; die TUI ist nur ein
  weiteres Frontend, die Loop-Logik bleibt unangetastet. Sharing über
  IORef+MVar: Ausgabezeilen in einen gemeinsamen Puffer, Eingabe über
  Signal-MVar + Pending-Queue (nicht blockierend für die UI).
- Grundgerüst funktional: scrollbarer Verlauf (PgUp/PgDn, Auto-Follow),
  Eingabezeile mit Verlauf (↑/↓) und Tab-Vervollständigung über die reine
  `completionFor` aus Phase V (ein Treffer ersetzt, mehrere: LCP + Liste),
  Diagnose-Kanal als `[Diagnose]`-Zeilen, Ctrl-Q/Esc beendet.
- Brick 2.13-API notiert: `EventM n s a` (State als Parameter),
  `appHandleEvent` nimmt nur das Event, `appChooseCursor` bekommt den State,
  Editor-Events via `nestEventM'` einbetten, `renderEditor` mit Fokus-Bool.
  ANSI wird vorerst gestrippt (SGR→vty-Attrs ist Folgearbeit); Kunst wandert
  vorerst inline mit dem Raustext mit — das Kunst-Panel (D21) kommt mit H.
- `cabal.project` um `text-adventure-tui` erweitert; CI baut das Paket auf
  Linux **und** Windows — die `vty-windows`-Messung läuft damit dauerhaft.

### Phase V: Frontend-Trennung (IO-Politik aus GameLoop ausgelöst)

- Neu `src/Frontend.hs`: das `Frontend`-Record ist die einzige I/O-Fläche des
  Spielloops — Ausgabezeilen, Eingabe (mit Vervollständigung/Verlauf als
  Frontend-Sache), Pause bei Erzählfortsetzungen, Animations-Abspielung
  (inkl. `frameDelayMicros`, jetzt Frontend-Angelegenheit) und der
  Diagnose-Kanal (heute stderr). `haskelineFrontend` ist die heutige
  Terminal-Umsetzung, byte-identisch zum bisherigen Verhalten.
- Neu `src/Completion.hs`: die Vervollständigung ist jetzt eine reine Funktion
  (`completionFor :: GameState -> String -> (String, [String])`) ohne I/O —
  die Haskeline-Hülle (`commandCompletion`) lebt in `Frontend`, ein TUI
  konsumiert `completionFor` direkt.
- `GameLoop` enthält keinen Haskeline-/stdout-/stderr-Zugriff mehr; neuer
  Einstieg `runGameWithFrontend :: Frontend -> GameState -> IO ()` (die alten
  `runGame`/`runGameWith`/`gameLoop` bleiben als Haskeline-Fassaden erhalten).
  Das ist die Voraussetzung für das TUI-Paket (Phase T, D20) und späteres
  WebUI-Backend.
- Neuer Test: ein Canned-Frontend (Skript-Eingaben, Aufzeichnung der Ausgabe)
  treibt `runGameWithFrontend` durch look/take/EOF-Quit — der Loop läuft also
  ohne Terminal. Alle bisherigen Tests, E2E-Läufe und die
  Warnungs-Gates unverändert grün.

### W6: Autoren-Doku "Writing an adventure on Windows"

- `packaging/windows/WRITING-ADVENTURES.txt` (neu): das komplette Tutorial
  (12 Schritte) - von der Kopie der Demo ueber die YAML-Regeln (Tabs verboten,
  Block-Skalar-Einrueckung), Items, NPCs, verschlossene Tueren, Quests, Regeln
  bis zu den W5-Fehlermeldungen (Zeile + `->`-Hinweis erklaert) und dem Teilen
  des YAML. Jedes YAML-Beispiel ist gegen den Compiler verifiziert
  (Minimal-Adventure aus Schritt 4 und die vollstaendige Probe aus Schritt 5-9
  validieren beide sauber). START-HERE.txt verweist darauf als Voll-Version;
  `build-release.sh` buendelt die Datei.
- Dabei zwei echte Doku-Befunde: Quest-Stufen sind Objekte (`{id, desc}`),
  keine Strings; und der Quest-Schluessel heisst `reward:` (singular), nicht
  `rewards:` - letzterer wird vom Worldbuilder **stillschweigend** ignoriert
  (`.:?`-Defaults), ein Autor verliert also die Belohnung ohne Meldung.
  Unbekannte YAML-Schluessel warnen: nachgelagerte Verbesserung, im Plan
  notiert.

## [0.10.0.0] — 2026-09-18

Phase 6 (Genre-Fixtures + CI) und Phase 7 (optionale Gameplay-Module 7a–7h).
Leitprinzip der Phase 7: ein Modul ist **YAML-Segment + Compiler-Pass** auf
bestehende Core-Konzepte (Predicate, Effect, Trigger, VarMap, Location) —
kein eigener Interpreter, kein State-Silo, kein eigenes Package. Ohne das
Modul-Segment bleibt jede bestehende Welt bit-identisch.

### Added
- **Phase 6a — Mini-Genre-Fixtures** (`examples/genres/`, Doku `docs/genres.md`):
  sechs vollständige Adventures (13–18 Räume), die den Kern gegen
  unterschiedliche Genre-Anforderungen prüfen — **ohne** Genre-spezifischen
  Engine-Code. `pure-if` (Exploration, `search`, Container, `visible_when`,
  Custom-Verb, Trigger-Puzzle), `fantasy` (`cast`, `mana`, Crafting, Loot,
  Quest), `cyberpunk` (`hack`, `heat`-Eskalation), `space-opera` (`dock`,
  `oxygen`-Drain, Vehicle mit Stops), `detective` (Predicate-Ketten,
  `accuse` → Victory/Failure), `horror` (`sanity`, Scheduler `on: turn` +
  `cooldown`, vier Enden). Dabei generisch geschlossen (waren
  Abstraktionslücken): `visible_when` für Dialog-Optionen, Bare Custom-Verbs
  + `OnCommand`-Name, `OnUse`/`OnTake`/`OnDrop` tragen die Item-ID statt
  Rohtext, `compare_var`, `set_var`/`add_var`, `in_container`, `move_npc`,
  `game_end`, autorisierbare Entity-/Item-Interactions (Crafting), `take` +
  `on_take`, `portable`/`take_failure`, Vehicle-Erreichbarkeit im Validator,
  `start_stop`.
- **Phase 6b — Invarianten-Tests**: equipped → carried; jede Entity an genau
  einer Location; Container-Refs auflösbar; Save/Load-Round-Trip erhält RNG,
  Variablen, Quests und Scheduler (`triggerStates`); alle
  `verb_map`-Keys lösen zu Core- oder deklarierten Custom-Verbs auf.
- **Phase 6c — CI-Pipeline**: `scripts/ci.sh` (build → `cabal test all` →
  `validate` von demo/thefog/6 Genres/11 Modul-Fixtures → E2E-Playthroughs)
  plus `.github/workflows/ci.yml` (GHC 9.6.7 + cabal-Cache). Eingaben und
  Erwartungen unter `ci/e2e/<name>.in` / `.expect`.
- **Phase 7a — Fraktionen/Reputation**: `factions:`-Segment; Standing ist der
  `VarMap`-Eintrag `faction.<id>` (kein neues Save-Feld). Effect
  `standing: {faction, add|set}`, Predicate
  `standing: {faction, at_least|at_most|equals}`. Compile-Fehler
  `DuplicateFaction`, `FactionVariableClash`, `UnknownFaction`.
- **Phase 7b — Handel/Ökonomie**: `buy`/`sell` als Custom-Verben; Währung und
  Lagerbestand als `VarMap`-Einträge (`credits`,
  `shop.<merchant>.<item>`); Preis-/Bestandslogik als `Conditional` +
  `CompareVar` über die Item-`verb_map`; ein Kauf ohne Deckung verändert
  nachweislich nichts; Member-Preis über 7a.
- **Phase 7c — Encounter-Tabellen**: `encounter_tables:` kompiliert zu
  gewöhnlichen Triggern mit einem gewichteten `RandomChoice`; ein optionales
  `when` pro Eintrag wird ein `Conditional`-Gate. Fehler:
  `DuplicateEncounterTable`, `EmptyEncounterTable`, `BadEncounterWeight`.
  Deterministisch über den `rngState` des Saves (Test
  `testRandomChoiceDeterministic`).
- **Phase 7d — Survival/Wetter/Umweltgefahren**: `environment:`-Segment —
  Wetter ist die Variable `env.weather`, Transitionen und Drains werden
  `on: turn`-Trigger (`SetValue`/`ModifyValue` + `Conditional` für `at_zero`).
  Fehler: `UnknownWeatherState`, `UnknownDrainVariable`,
  `EnvironmentVariableClash`.
- **Phase 7e — Stealth/Lärm**: `stealth:`-Segment — Lärm als Variable
  (`noise`), Observer als Trigger mit `hears_at`-Schwelle und `cooldown`.
  Bewusst **keine** Kernänderung: der Plan erlaubte maximal eine generische
  Predicate-Erweiterung, die Fixture brauchte sie nicht. Fehler:
  `UnknownObserverNPC`, `StealthVariableClash`.
- **Phase 7f — Kampfprofile**: Kampf verlässt `Parser.executeAttack` und wird
  eine datengetriebene Policy im Kern (neue Datei `src/Combat.hs`):
  `resolveCombat :: CombatProfile -> [CombatActor] -> CombatTarget ->
  CombatAction -> GameState -> ([Effect], [String])` erzeugt Effects, die anschließend durch
  `applyOutcomeWith` laufen — kein zweiter Interpreter. Profile: `off`
  (Ablehnung, kein HP-Verbrauch), `narrative` (ein vergleichender Wurf,
  `on_win`/`on_lose`), `classic` (Default, bit-identisch zum Vorzustand),
  `tactical` (eine Aktion = eine Runde, Initiative, Flucht, Aktionen `attack`, `defend`, `flee`, `use-ability`).
- **Phase 7f-3 — Taktischer Runden-Treiber, BySpeed-Initiative & Spieler-Abilities**:
  - Profil `tactical` im Compiler (`ACombat` mit `initiative`, `flee_allowed`, `max_rounds`, `speed_attribute`) und Engine `CombatTactical`.
  - Spieler-Abilities (`abilities:`-Segment im Adventure) mit `cost_var`, `cost`, `cooldown`, `effect`.
  - Parser-Unterstützung für nackte Verben `defend`, `flee` sowie `use-ability <id>` / `ability <id>`.
  - Fixture `examples/modules/combat-tactical.yaml` mit Gladiator-Arena, Fähigkeiten und reaktivem Gegnersystem (`on: turn` gated auf `combat.engaged >= 1`).
  - CI-Integration in `scripts/ci.sh` mit Happy Path (`ci/e2e/combat-tactical.*`) und Flee-Fehlerpfad (`ci/e2e/combat-tactical-fail.*`).
- **Phase 7g — Party/Begleiter**: `party:`-Block am NPC; Mitgliedschaft ist
  der `VarMap`-Eintrag `party.<npcId>` (kein neues Save-Feld). Ein Order-Verb
  toggelt Beitritt/Verlassen; `followParty` zieht lebende Begleiter bei jedem
  Raumwechsel mit (Gehen, Teleport, Fahrzeug-Ein-/Ausstieg, Fahrt);
  Begleiter sind zusätzliche `CompanionActor` im 7f-Resolver.
  `damage_npc: {npc, amount}` als Zucker auf
  `ModifyValue (VRProperty id "hp")`. Fehler: `PartyOrderVerbUnknown`,
  `PartyHealthMissing`, `PartyVariableClash`, `UnknownDamageNPC`.
- **Phase 7h — Raumschiffe**: `systems:` → je System die Variable
  `ship.<vehicleId>.<name>` (`power`, `shields`, `hull`, `weapons`);
  `stations:` → Interior-Raum + Verb, kompiliert zu `on: command`-Triggern
  mit `{ at: player, room: … }`-Gate. `ShipActor` im 7f-Resolver: das Schiff
  feuert `weapons` und kostet 1 `power`, der Konter trifft Schilde → Hülle;
  ohne Systeme bleibt alles bit-identisch. Fehler: `ShipVariableClash`,
  `UnknownStationRoom`, `UnknownStationVerb`.
- **Phase 7h-2 — Schiff-gegen-Schiff-Duell (Teil B: B0–B3)**:
  - `CombatTarget = TargetNPC String String | TargetShip VehicleID String` in `Combat.hs`.
  - Trennung von `shipVolley` und `shipStrike`: Schiffsfeuer und Gegenfeuer arbeiten auf Schilden und Hülle über `shipAbsorb`, sowohl bei Spieler- als auch bei feindlichen Schiffen.
  - Feindliches Schiff wird zerstört, sobald dessen Hülle 0 erreicht (`SetValue (VRActorProp (ActorShip vId) (PCustom "hull")) (VTInt 0)` + Zerstörungsmeldung, kein Gegenfeuer mehr).
  - Parser-Erweiterung: `attack <ship>` / `fire <ship>` zielt auf Schiffe am selben Halt (`outsideStop`), wenn der Spieler sich nicht selbst im Zielschiff befindet. Ablehnung gewöhnlicher Fahrzeuge ohne Systeme (`You can't attack the <name>.`).
  - Validation (`Validate.hs`): `ActorShip vId` in `idsFromOutcomeVehicle` und `idsFromPredicateVehicle` gegen deklarierte Fahrzeuge validiert.
  - Fixture `examples/modules/ship-duel.yaml` (14 Räume, Asteroidenfeld, Kestrel vs. Korsaren-Fregatte).
  - Dual-Path E2E Tests: Happy Path `ci/e2e/ship-duel.*` (`VICTORY`) und Failure Path `ci/e2e/ship-duel-fail.*` (`hull_failure` Zerstörung).
- **Kompositionsbeweis** `examples/modules/combo.yaml` („Der Ring von Tarsis"):
  ein Referenzspiel nutzt **fünf Module gleichzeitig** (7a, 7b, 7d, 7g, 7h),
  verbunden ausschließlich über Autoren-Regeln — kein Modul kennt ein anderes.
- **Doku**: `docs/modules.md` (Referenz je Modul) und `docs/genres.md` neu;
  `docs/adventure-schema.md` und `README.md` auf Phase-7-Stand.

### Changed
- Neue Datei `src/Combat.hs`: `resolveCombat` als reine Funktion;
  `Parser.executeAttack` ist nur noch ein Wrapper (Profil + Aktoren +
  Ziel verdrahten, Effekte anwenden, Meldungen durchreichen).
- `CombatActor` ist eine Liste — `PlayerActor`, `CompanionActor` (7g) und
  `ShipActor` (7h) erweitern sie ohne Signaturänderung.
- `evalPredicate (Location "player" r)` prüft jetzt auch den Spielerraum
  (vorher nur NPC-/Item-Locations).
- **Phase V1 — Typsichere `ActorRef`- und `PropRef`-ADTs für `ValueRef`**:
  - `VRProperty String String` abgelöst durch `VRActorProp ActorRef PropRef`.
  - `ActorRef = ActorPlayer | ActorNPC NPCID | ActorShip VehicleID | ActorRoom RoomID | ActorEntity EntityID`.
  - `PropRef = PHealth | PRoom | PVisited | PState | PCustom String`.
  - Migration via handgeschriebenem `FromJSON ValueRef` (unterstützt sowohl neues Schema als auch altes `VRProperty [target, prop]` Format).
  - Save-Version auf 3 erhöht.
- Tests: **241** Engine- + **75** Worldbuilder-Tests, **29** E2E-Läufe
  (`scripts/ci.sh`).

### Fixed
- `killNPC` ist idempotent — ein Trigger, der denselben NPC erneut tötet,
  konnte vorher eine Endlosschleife auslösen (jetzt per Test abgesichert).
- `take` setzte `portable`/`on_take` nicht durch.
- `OnUse`/`OnTake`/`OnDrop`-Events trugen den Rohtext statt der Item-ID.
- `visible_when` bei Dialog-Optionen war nicht auswertbar.
- Drei generische Parser-/Trigger-Bugs (aus der Pure-IF-Fixture).
- Tod-Event-Meldungen wurden im Interpreter mit `fst` verworfen; `killNPCWithMsg`
  / `modifyNPCHealth` / `modifyValueProp` reichen sie jetzt durch.
- Help-Text: `exit` ist der Quit-Alias — ein Fahrzeug verlässt man mit
  `disembark`.
- `World.defaultSaveState` (Code-Review P0-1) ließ vier `SaveState`-Felder
  unbesetzt (`rngState`, `variables`, `containers`, `triggerStates`) — jedes
  davon war beim ersten Zugriff `undefined` (z. B. `RandomChoice`, `CompareVar`,
  Container-Lookup). Alle Felder werden jetzt initialisiert; `variables`
  übernimmt die deklarierten `VarDef`-Startwerte. Per Regressionstest
  abgesichert.
- `Parser.executeAttack` (P0-2) verwarf in einer eigenen Faltung alle
  Effekt-Meldungen bis auf die letzte und setzte den RNG-Salt je Effekt zurück.
  Nutzt jetzt den gemeinsamen Interpreter `applyOutcomes` — Tod-Event-Meldungen
  (`killNPCWithMsg`) überleben auch mitkämpfende Begleiter (7g) und Schiffe (7h).
- `Validate.flagsInPredicate` (P1-2): die unerreichbare `Compare`-Klausel ließ
  Flag-Referenzen auf der **rechten** Seite eines Vergleichs ungeprüft; die
  Klauseln sind zusammengeführt, beide Seiten werden validiert.
- Worldbuilder-Test-Fixture `minSave` (P0-3) ließ zwei `SaveState`-Felder
  unbesetzt — die Suite war dadurch „falsches Grün".
- Warnungs-Cleanup: **53 → 0** GHC-Warnungen (`-Wname-shadowing`,
  `-Wunused-imports`, `-Wunused-local-binds`, `-Wunused-matches`,
  `-Wunused-top-binds`, `-Wtype-defaults`, `-Woverlapping-patterns`); keine
  Unterdrückung per `OPTIONS_GHC`. `-Werror=missing-fields` ist jetzt in allen
  Paketen (library/executable/test-suite) aktiv, damit diese Feldklasse nicht
  zurückkehrt.
- `scripts/ci.sh` hat das Execute-Bit (vorher `Permission denied`, Exit 126).
- `Validate.allOutcomes` (Code-Review P1-1) erfasste **keine Trigger-Effekte**
  — damit war der Validator für die seit Phase 3f/7 in `rules:` lebende
  Spiellogik blind (`give:`, `start_quest:`, Flag-Referenzen). Alle
  Trigger-Effekte werden jetzt mitgesammelt; die Zuständigkeit ist damit
  dieselbe wie im Worldbuilder (`allWorldEffects`).
- **`Validate.checkFlags` war seit jeher wirkungslos** (beim P1-1-Fix
  entdeckt): der „checked"-Akkumulator wurde in *keinem* Zweig befüllt,
  `MissingSetFlag` konnte deshalb **nie** feuern. Die geprüften Flags stammen
  jetzt aus dem Predicate-Baum (Trigger-Bedingungen, `visible_when`,
  `CondText`) via `flagsInPredicate`; neue Prüfhilfe `allPredicates` spiegelt
  `allWorldPredicates` des Worldbuilders. Exit-Schlüssel aus `locked_by:`
  gelten dabei als gültige Entitäten, sonst würde jedes `set_state` auf einem
  Schloss als fehlende Entität gemeldet.
- `Validate.checkMissingVehiclesInDefs` (P1-3) konnte nie etwas melden
  (`allRefs = []`). Es sammelt jetzt `ship.<vehicleId>.<system>`-Referenzen aus
  Effekten und Prädikaten (rekursiv, inkl. `Narrative`-Folgeeffekt,
  `Conditional`, `ApplyCondition`) und meldet undeklarierte Fahrzeug-IDs.
- `Validate` Erreichbarkeitsprüfung (P1-4) riet den Startraum (bevorzugt
  `start`, sonst alphabetisch kleinster Raum) und ein Test deckte das Ergebnis
  zu. Stattdessen `checkUnreachableFrom : RoomID -> GameWorld -> …`, aufgerufen
  aus `validateGameState` mit `currentRoom` — der echte `start_room` lebt nur
  im `SaveState`. `validateWorld` rät nicht mehr; die Test-Maskierung in
  `worldbuilder/test/Tests.hs` ist entfernt. **Folgefund:** dadurch aufgedeckt,
  dass in `examples/modules/stealth.yaml` die Räume `boiler_room`,
  `stairs_down` und `cellar` keinen Eingang hatten (die alte Prüfung ging nur
  durch, weil der geratene Startraum zufällig `boiler_room` war) — behoben mit
  zwei additiven Exits (`start_hall: down → stairs_down`,
  `junction: east → boiler_room`).
- `Game.fireTriggerList` (P1-5) las `once`/`cooldown` aus einem beim Eintritt
  gebundenen Snapshot des Trigger-States. Eine verschachtelte Ereignisrunde
  (`killNPCWithMsg` → `OnStateChange`) markierte einen Trigger als gefeuert,
  ohne dass die äußere Faltung das sah — `once: true` konnte dadurch erneut
  feuern. Der Zustand wird jetzt aus dem laufenden Fold-State gelesen.
- Worldbuilder: Trigger-IDs wurden nicht auf Eindeutigkeit geprüft (P1-6).
  Neu: `DuplicateTriggerId` (Fehler) und `ReservedTriggerId` für die
  compiler-eigenen Präfixe `encounter.`/`environment.`/`stealth.`/`ship.`/
  `party.`; verifiziert, dass keine ausgelieferte Welt betroffen ist.
- `Game.RandomChoice` (P1-7) zog den Index über `mod` direkt aus dem
  LCG-Zustand und damit aus dessen niederwertigen Bits — bei diesem LCG hat
  Bit 0 die Periode 2, eine Zwei-Wege-Auswahl degenerierte zu A,B,A,B. Der
  Index kommt jetzt aus den hohen Bits (`shiftR 33`, volle Periode). Die
  LCG-Schrittfolge bleibt identisch, Determinismus/Undo/Save-Load unverändert.
- `set_state` setzte den Entity-State, ohne `OnStateChange` zu feuern, während
  jeder andere Mutationspfad (z. B. NPC-Tod) es tut (P1-12). Neu:
  `setEntityStateWithEvents` feuert das Event an der Mutationsstelle; ein
  Schreibvorgang auf den bereits gesetzten State ist ein No-op, und genau diese
  Idempotenz ist die Rekursionsschranke für eine Regel, die ihren eigenen
  Zustand aus dem `on: state`-Handler erneut schreibt. `applySetValue` liefert
  dafür jetzt `(GameState, String)` und reicht die Handler-Meldung durch.
- `set_var`/`modify_value` ignorierten die `min:`/`max:`-Grenzen einer
  `int`-Variablen (Code-Review P1-8) — ein `set_var: 99` auf eine Variable mit
  `max: 10` speicherte 99. Neu: `setVariableChecked` clampt beim Setzen **und**
  beim Modifizieren über `clampToVarDef` auf die deklarierte Spanne; unbeschränkte
  Seiten (`min:`/`max:` weggelassen, `Nothing`) bleiben unberührt. Durch
  `-Werror=missing-fields` und Regressionstests abgesichert.
- Fahrzeug-`look` erzeugte für einen leeren Tank einen kaputten Text
  (P1-9): `if f <= 0 then "Out of " else "Fuel ("` mit anschließendem
  `if f > 0`, kombiniert mit `++ "")` und `++ ")"` produzierte z. B.
  `Out of 0/10)` oder `Fuel (0/10)`. Der Text ist jetzt sauber
  zweizeilig: `Out of hay (0/10)` bzw. `Fuel (hay: 7/10)`.
- Fahrzeug-Stops wurden immer in Sortierreihenfolge der Map-Schlüssel
  zurückgegeben (P1-10) — die Autoren-Reihenfolge der `stops:`-Liste (und die
  daraus abgeleitete Routenrichtung) ging verloren. Neu: optionales
  Schema-Feld `route:` (`VehicleDef.vehicleRoute`); `vehicleStopList`
  respektiert es und fällt ohne Angabe auf `Map.toList` zurück
  (abwärtskompatibel).
- `move … in_container:` war ein **stiller No-op** (P1-11): nichts wurde
  bewegt, keine Meldung, kein Fehler — Autoren konnten eine Container-API
  vermuten, die es nicht gibt. Container sind nur eine **Kompilierzeit-**
  Startplatzierung (`in_container:` auf Items, validiert über
  `checkContainerRefs`); Items bewegen sich zur Laufzeit über
  `give`/`drop`/`consume`. Der Effektzweig meldet jetzt den Fehler statt zu
  schweigen, und das tote Laufzeit-Feld `SaveState.containers` (+
  `ContainerState`) ist entfernt.

- Genre-Verben aus dem Kern (P1-13): `swim`/`crawl`/`dig`/`game` waren im
  Parser hart auf `Go Southeast` verdrahtet, zusätzlich in `commandWords`
  und als reservierte Verbnamen. Sie sind entfernt; TheFog deklariert
  `swim`/`crawl` jetzt selbst (`verbs:` + `on: command …`-Regeln mit
  `{ at: player, room: … }`-Gate) — Verhalten unverändert, aber generisch.
- `on: command examine` feuerte nie (P1-14): `commandVerbName` leitete den
  Namen aus `show` ab (`VLookAt` → `"lookat"`). Neu: `Verbs.verbCanonicalName`
  aus der Registry; der Compiler lehnt unbekannte Verbnamen jetzt als
  `UnknownCommandVerb` ab, statt eine Regel zu akzeptieren, die nie feuert.
- `OnTake`/`OnDrop` feuerten auch bei **abgelehntem** Befehl (P1-15) — ein
  `take` auf ein `portable: false`-Item verbrauchte eine `once: true`-Regel.
  Events werden jetzt aus der tatsächlichen Zustandsänderung abgeleitet;
  `findItemIdByAlias` erfindet keine IDs mehr aus der Roheingabe.
- Eine ungültige Dialogwahl kostete einen Zug (P1-16, Plan-1e-Abweichung):
  Conditions/Vehicle-Ticks liefen, `on: turn` feuerte, die Undo-Historie
  wuchs. Neu: `consumesTurnIn` wertet die Wahl gegen den aktiven Dialogknoten
  aus (`Parser.isValidChoice`).
- Fünf Engine-Effekte waren aus dem Schema nicht erreichbar (P1-17):
  `ApplyCondition`, `ClearCondition`, `ModifySkill`, `RandomChoice` und
  `Narrative` — letzteres wurde zudem **falsch** kompiliert (zusammengeklebter
  Block statt seitenweiser Ausgabe). Neu: `condition:`, `clear_condition:`,
  `skill:`, `random:` und `narrative:` (+ `then:`).
- `check_flag:` war dokumentiert, aber nie dekodierbar; der Konstruktor warf
  den Erwartungswert weg (P1-18). Entfernt — Flag-Tests laufen über
  `if: { has_flag: … }`; die String-Flag-Grenze ist in der Schema-Doku notiert.
- `PaidVehicle` war aus dem Schema nicht erreichbar (P1-19): `stopCost` war
  hart `Nothing`, `type: paid` verhielt sich wie `auto`. Neu: `stops:` erlaubt
  die Langform `{ room, cost: { item, refused } }`; ein nicht deklariertes
  Kosten-Item ist ein Compile-Fehler (`UnknownStopCostItem`).
- `OnCustomEvent` wurde nie gefeuert (P1-20): `on: custom <name>` kompilierte
  und validierte sauber, aber nichts löste es aus. Neu: Effekt `raise: <name>`
  (`RaiseEvent`); die Ereignis-Tiefe wird durch den Trigger-Pass gefädelt, damit
  selbstauslösende Regeln terminieren (Test mit Timeout-Guard).
- Toter Code und Kleinigkeiten aus dem Review-P2-Block (Cluster A):
  `mixHash`/`gameRandom`/`gameRandomIndex` entfernt (P2-1, Plan 1f verlangte
  das bereits — sie luden ein, den expliziten RNG-State zu umgehen);
  `executeCommand Restart` löschte die geladene Welt (P2-2); die
  `modifyValueProp`-Signatur war durch eine fremde Definition von ihren
  Klauseln getrennt (P2-3); `journalText` baute `unlines ("=== Journal ===" : [])`
  (P2-6); `visited` wertete `EVBool` als `False` (P2-7); der ungenutzte
  `EquippedBy`-Slot ist aus `Location` entfernt (P2-19); das Legacy-Feld
  `npcDialogue` (P2-20) war für kompilierte Welten toter Pfad und ist samt
  JSON und Parser-Fallback entfernt.
- Ein `ItemDef` ohne `ItemState`-Eintrag war zur Laufzeit **unsichtbar** (P2-8):
  `take`/`look at` scannen `itemStates`, das Item existierte also einfach nicht.
  Neuer Validator-Fehler `MissingItemState` statt stillem Verschwinden.
- Tests: **201** Engine- + **71** Worldbuilder-Tests, **18** E2E-Playthroughs.

- Schema-/Packaging-Cluster aus dem Review-P2-Block (Cluster B):
  `parseAdventureFile` verschluckte jeden Fehler (P2-15) — die CLI konnte nur
  „Failed to parse adventure file: <path>" sagen, obwohl ein kaputtes
  Adventure der häufigste Autorenfehler ist. Es liefert jetzt
  `IO (Either String Adventure)` mit Grund und bei YAML **Zeile/Spalte**, und
  eine unlesbare Datei wird gemeldet statt als Exception zu fliegen.
  `license-file: ../LICENSE` im worldbuilder (P2-17) wies aus dem Paket
  heraus (`[relative-path-outside]` beim `sdist`); die License-Kopie lag
  bereits, der Verweis zeigt jetzt darauf.
- Verbundene Map-Schlüssel im `world.json` (P2-9): `itemVerbMap`/`npcVerbMap`,
  `entityInteractions` und `itemInteractions` kodierten ihren Schlüssel in
  *einen* String (`"VTake:intact"`, `"a|b"`) und verloren damit still jeden
  Status, Verben- oder Item-Namen, der das Trennzeichen enthielt. Sie sind
  jetzt Listen von Objekten mit getrennten Feldern; die alte Form wird
  weiterhin gelesen, damit bestehende `world.json` laden (Test deckt beide
  Richtungen ab).
- Fahrzeug-Treibstoff (P2-21): `vehicleFuelProp` war ein rohes `(String, Int)`,
  und `vsFuel` startete immer bei `Nothing` — jedes Fahrzeug mit Tank meldete
  „0/10", bis der Spieler tankte. Neu: `FuelSpec { fsItem, fsMax }`, das
  Schema akzeptiert `fuel: { item: hay, max: 10 }` (alte Liste `[hay, 10]`
  bleibt gültig), und ein betanktes Fahrzeug startet mit **vollem** Tank.
- Ungenutzte Schema-Felder (P2-18): `name:` wurde geparst und nirgends
  verwendet — es wird jetzt zu `GameWorld.worldName` und erscheint als
  Startbanner. Die `levels:`-Schwellen waren reine Dekoration; der Compiler
  lehnt nun doppelte Schwellen (`DuplicateFactionLevel`) und leere Namen
  (`BadFactionLevel`) ab. Eine Sortierung der Liste wird bewusst *nicht*
  verlangt — die Fixtures ordnen nach Beziehungsqualität.
- Tests: **202** Engine- + **74** Worldbuilder-Tests, **18** E2E-Playthroughs.

- Performance-Cluster aus dem Review-P2-Block (Cluster C, Teil 1):
  `computeWorldChecksum` faltete die **komplette** Welt-JSON mit dem lazy
  `foldl` — ein Thunk pro Zeichen — und `listSaves` rief es **innerhalb** der
  Schleife auf, also O(Saves × Weltgröße) statt O(Weltgröße) (P2-10). Die
  Prüfsumme wird jetzt einmal pro Auflistung berechnet und
  `formatSaveEntry` bekommt sie als Parameter.
  Alle 15 Stellen mit lazy `foldl` über `GameState`-Akkumulatoren nutzen jetzt
  `foldl'` (P2-11) — betroffen waren `Game`, `Parser`, `GameLoop`, `Validate`
  und `SaveLoad` (u. a. `applyOutcomes`, `tickConditions`, `fireTriggerList`,
  `TakeAll`/`DropAll`).
- Beim Testen von P2-11 gefunden und behoben: `Sequence`/`applyOutcomes`
  hängten Meldungen **bedingungslos** an, ein Effekt ohne Text (z. B.
  `ModifyValue`) erzeugte damit eine Leerzeile im Spieltext. Beide nutzen jetzt
  `joinMessages` mit derselben Regel wie der Trigger-Pfad (`combineMessages`).
- Tests: **204** Engine- + **74** Worldbuilder-Tests, **18** E2E-Playthroughs.

- **Entschieden und verworfen** (P2-12/P2-13, Cluster E): die im Review
  vorgeschlagenen Indizes (`Map RoomID [ItemID]` im State, `Map EventType
  [TriggerDef]` im `GameWorld`) werden **nicht** gebaut. Beides wäre
  serialisierter State, also eine zweite Quelle der Wahrheit, die bei jedem
  Item-/Trigger-Update und jedem Save/Load konsistent bleiben müsste; ein
  veralteter Index fällt still aus (verlorene Items bzw. keine Trigger mehr) —
  ein deutlich schlechterer Fehler als etwas Scan-Zeit. Gemessene Kosten des
  heutigen Verhaltens bei TheFog-Größe (50 Items / 50 Trigger): **12 ns** pro
  Befehl; bei 100× Größe (5000/5000) 130 ns. Die Begründung samt Messwerten
  steht als Kommentar an `getItemsInLocation` und `fireTriggerList`.

- Build-Hygiene-Cluster aus dem Review-P2-Block (Cluster D):
  **P2-4** ist bereits durch den P0-Warnungs-Cleanup behoben
  (`Worldbuilder.Compile` importiert `Types hiding (…)` plus `qualified Types
  as E`, kein Schatten der Feldselektoren mehr). Zusätzlich ist
  `-Werror=name-shadowing` jetzt in beiden Paketen gesetzt — genau die Klasse,
  unter der die `-Wmissing-fields`-Befunde (P0-1/P0-3) untergegangen waren.
  `scripts/ci.sh` prüft das Build-Log zusätzlich auf `warning:` und bricht ab.
  **P2-5** ebenfalls verifiziert: mit `-fforce-recomp -Werror=unused-imports
  -Werror=unused-top-binds` bauen beide Pakete warnungsfrei, es gibt also keine
  ungenutzten Imports oder Bindungen mehr.
- **P2-22**: `pick` ist gleichzeitig Dialog-Keyword (`pick 3` = Option wählen)
  und `take`-Alias; ein numerisches `pick` kann daher nie „nimm Item 3"
  bedeuten. Das war für `pick` ungetestet — jetzt festgeschrieben
  (`testDialoguePickKeywordAlias`: alle vier Keywords, nackte Zahl,
  `pick up <item>` weiterhin als `Interact VTake`) plus Kommentar an der
  Keyword-Liste in `src/Parser.hs`.
- **P2-24 als Entscheidung festgehalten**: IDs, Beschreibungen und Meldungen
  bleiben `String` statt `Text`. Begründung (JSON-/YAML-Grenze, ~12 ns
  Zeichenarbeit pro Befehl, querschnittliche Migration ohne Nutzereffekt) steht
  an den ID-Aliassen in `src/Types.hs`.
- Tests: **205** Engine- + **74** Worldbuilder-Tests, **18** E2E-Playthroughs.

- **P2-23**: Der Depth-Guard meldete einen Content-Fehler
  (`"[ERROR] Maximum outcome depth exceeded."`) als *Spieltext*; über
  `applyTrigEffects` landete er mitten in der Ausgabe, und `executeAttack`
  verwarf ihn ganz. Es gibt jetzt einen runtime-only Diagnose-Kanal:
  `GameState.diagnostics` (`GameState` hat bewusst keinen JSON-Instanz, also ist
  nichts aus dem Save herauszuhalten) und `addDiagnostic` in `src/Game.hs`. Der
  Effekt-Depth-Guard **und** der Trigger-Nesting-Guard (der bisher *stillschweigend*
  abbrach) melden dorthin; `GameLoop` schreibt neue Diagnosen nach **stderr**,
  nie in den Spieltext. End-to-End geprüft: eine Adventure-Fixture mit
  selbst-auslösender Regel erzeugt auf stdout reinen Spieltext und auf stderr
  `[engine] maximum outcome depth exceeded (depth 21 > 20) …`.
- Damit ist der P2-Block aus dem Review abgearbeitet: gefixt (P2-1 bis P2-11,
  P2-15 bis P2-23), als Entscheidung dokumentiert (P2-12, P2-13, P2-24).
- Tests: **208** Engine- + **74** Worldbuilder-Tests, **18** E2E-Playthroughs.

- Testlücken aus dem Review (L2, L3, L5, L7, L10, L11, L13):
  **L10** acht handgerollte Substring-Helfer (`isInfixOfT`, `isInfixT4`–`T6`,
  `isInfixOfV`, `tailsT*`, `isSubOf`, `isPrefixT2`) durch `Data.List.isInfixOf`
  bzw. `isPrefixOf` ersetzt — alle acht waren semantisch „infix", einer davon in
  Wahrheit ein Präfix-Test.
  **L7** `GameWorld`-JSON-Round-Trip (ItemDef/NPCDef/VehicleDef/Room/Predicate/
  CondText plus die Verbundschlüssel-Maps) und je ein Round-Trip pro
  `CombatProfile`.
  **L3** `resolveCombat` wird jetzt direkt getestet — die *Effektlisten* statt
  nur der Meldungen: Spieler allein inkl. Vergeltung, tödlicher Schlag ohne
  Vergeltung, Reihenfolge Spieler → Begleiter → Schiff. Dazu `shipAbsorb` in
  allen vier `(shields, hull)`-Kombinationen und für `dmg <= 0`; `(Nothing,
  Nothing)` war komplett ungetestet. Dafür sind `ShipSystems`/`shipAbsorb`
  exportiert.
  **L5** `commandEvents` pro Befehl als Tabelle festgeschrieben (Look, blockierte
  Bewegung, Bewegung mit Raumwechsel, Suche, Take, Drop, fehlgeschlagenes Take,
  informationslose Befehle) und durch die Loop geprüft, dass `on: enter` vor
  `on: turn` feuert. `commandEvents`, `consumesTurn`, `consumesTurnIn` exportiert.
  **L13** `consumesTurn`-Vollständigkeit: wildcard-freie Verdict-Tabelle plus
  `-Werror=incomplete-patterns` in beiden Paketen. Ein neuer
  `Command`-Konstruktor bricht jetzt den Build, statt still im `_ -> True`-Zweig
  zu landen — genau so ist P1-16 entstanden.
  **L2** `SaveLoad` erstmals getestet (129 Zeilen IO ohne Abdeckung):
  Save/Load-Round-Trip, geänderte Welt (Warnung, lädt trotzdem), Bare-`SaveState`
  über den Legacy-Zweig, und dass ein Bare-`SaveState` nicht als Wrapper
  fehlinterpretiert wird.
  **L11** Zug-Reihenfolge (`incrementTurnCount` → Ticks → `executeCommand`):
  ein tödlicher Condition-Tick stoppte den Befehl bisher **nicht**, er lief auf
  einem State mit `gameOver = True`. Verhaltensänderung: der Befehl wird jetzt
  verworfen, nur der Tick-Text wird ausgegeben — testgesichert inkl. Kontrollfall.
- Tests: **216** Engine- + **74** Worldbuilder-Tests, **23** E2E-Läufe.

- E2E-Fehlerpfade (L12): jeder Fixture-Lauf prüfte bisher nur *einen* glücklichen
  Pfad per Endtext-Grep. Neu sind fünf `.in`/`.expect`-Paare und CI-Stufe 5:
  Kauf ohne Deckung (`trade`, exakte Rabattmeldung), abgelehnter Angriff
  (`combat-off`), Verhungern (`survival`), unbekannte Station (`starship`),
  ungültige Dialogwahl (`combo`). `scripts/ci.sh` teilt sich dafür eine
  `run_e2e`-Funktion zwischen Glücklich- und Fehlerpfad-Stufe.
  Befund am Rande: `hull_failure` in `starship.yaml` (`ship.kestrel.hull <= 0`)
  ist mit den Fixture-Zahlen **unerreichbar** — der Korsar stirbt in Runde 4,
  während die Hülle noch bei 2 steht. Der Todesfall dort ist toter Inhalt
  (Rebalancing wäre eine Inhaltsentscheidung).

- Nachtrag nach einem Abgleich der Befund-IDs gegen die Commit-Historie —
  drei Punkte waren doch offen:
  **P2-16** `scripts/ci.sh` war laut Review in Git nicht ausführbar (Modus
  `100644`); in HEAD steht `100755` (das Bit kam mit dem vorigen Commit in den
  Index, hier nur verifiziert — wer `./scripts/ci.sh` tippt, geht jetzt).
  **P2-14** Die Cooldown-Semantik ist in `docs/adventure-schema.md` jetzt
  ausdrücklich benannt: `cooldown` zählt **passende Ereignisse**, nicht Runden —
  auf `on: turn` also Runden, auf `on: enter <raum>` die nächsten Betretungen.
  Ein `cooldown_turns:` für echte Zeit-Semantik gibt es bewusst nicht
  (nicht implementiert); die irreführende Formulierung „N Turns bis die Wache
  wieder hört" ist korrigiert.
  **L4** `MissingRoom` war deklariert, aber **nirgends erzeugt** — ein `move:`
  auf einen falschen Raum blieb unbemerkt, und der Worldbuilder prüft
  Raum-Referenzen aus Regeln nicht. Jetzt verdrahtet
  (`checkMissingRoomRefs`: `MoveEntity … (InRoom r)` und das `move:`-Ziel des
  Spielers). Dazu Tests für die vier Validator-Konstruktoren, die nirgends
  abgedeckt waren: `MissingRoom`, `MissingNPC`, `MissingEntity`,
  `InvalidVehicleRoom` — je mit Gegenprobe (bekannte IDs lösen nichts aus).
  **L1 / L8** Nachgezogen: `World.loadGameWorld`/`loadSaveState` und sämtliche
  Fehlerzweige (korrupte Weltdatei, korrupter Save, fehlende Datei) waren
  ungetestet, ebenso der `equipmentSummary`-Text.
- Tests: **222** Engine- + **74** Worldbuilder-Tests, **23** E2E-Läufe.

- **Verlustpfad des Starship-Moduls ist jetzt erreichbar** (Nebenfund aus L12):
  `hull_failure` (`ship.kestrel.hull <= 0`) konnte in `starship.yaml` **nie** feuern —
  doppelt blockiert. Erstens fällt die Hülle in genau der Runde auf 0, in der auch der
  Korsar stirbt (`max_hp 30` gegen 9 Schaden pro Runde), zweitens war die Regel an
  `{ state: korsar, is: alive }` gebunden — und dieses Prädikat las **nur**
  `entityStates`.
- **Prädikat-Fix (`{state: X, is: Y}`):** Ein Zustands-Prädikat prüft jetzt alle drei
  Schichten — `entityStates` (Tore/`set_state`), den NPC-Status (`npcStates`, wie ihn
  `killNPC` und der Kampfpfad setzen) und den Item-Status (`itemStates`). Vorher war
  `{ state: <npc>, is: alive }` für NPCs **immer falsch**; `starship.yaml` **und**
  `combo.yaml` verwenden genau diese Form. Reine Leseseite — kein neuer State, kein
  zweiter Interpreter. Abgesichert für NPC (lebend/tot), Item, Tor und unbekannte ID.
- Neues Fixture **`examples/modules/starship-loss.yaml`**: derselbe Schiffstyp, aber ein
  zäher Gegner (`max_hp 100`), damit die Hülle bricht, *bevor* der Gegner stirbt. Die
  Zahlen und der Grund stehen als Kommentar im Fixture. Neuer E2E-Fehlerpfad
  `starship-loss-fail` („Die Hülle der Kestrel bricht auf — du verglühst mit dem Schiff.")
  → **24** E2E-Läufe (18 + 6 Fehlerpfade).
- `-Werror=overlapping-patterns` ergänzt (§4.3-1 damit vollständig umgesetzt).
- Tests: **223** Engine- + **74** Worldbuilder-Tests, **24** E2E-Läufe.

- **7f-3 (`tactical`), Schritt A0 — Signatur vorbereitet** (Plan
  `plan-7f3-tactical-7h2-shipduell.md`): neuer Typ `CombatAction`
  (`CAAttack` / `CADefend` / `CAFlee` / `CAUseItem` / `CAAbility`) und ein
  zusätzlicher Aktionsparameter an `resolveCombat`; `executeAttack` übergibt
  `CAAttack`. Reiner Refactor — `off`/`narrative`/`classic` ignorieren die Aktion
  weiterhin, A2/A3 verdrahten die übrigen Konstruktoren als **Daten**
  (`on: command`-Regeln, Rundenzustand in der VarMap), nicht als zweiten
  Interpreter.
  Verifiziert wurde nicht nur der Endtext-Grep der CI: die **vollen** Ausgaben
  aller sieben kampfnahen E2E-Läufe (combat-off/-narrative/-classic, party,
  starship, starship-loss, combo) sind vor und nach dem Schritt byte-identisch.
- Tests: **223** Engine- + **74** Worldbuilder-Tests, **24** E2E-Läufe.

- **7f-3 (`tactical`), Schritt A1 — Rundenzustand in der VarMap:** Der Kampf-
  Rundenzustand lebt unter dem reservierten Prefix `combat.` in der VarMap
  (`combat.round`, `combat.engaged`) — wie `faction.`/`party.`/`ship.`, also
  **kein neues `SaveState`-Feld** und Save/Load ohne Migration.
  `src/Game.hs` bekommt `combatVarPrefix`/`combatRoundKey`/`combatEngagedKey`
  plus `combatRound`/`setCombatRound`/`isCombatEngaged`; der Worldbuilder lehnt
  eine autor-deklarierte Variable in diesem Namespace ab
  (`CombatVariableClash`) — geprüft gegen die **autor-deklarierten** Variablen,
  damit die Regel noch hält, wenn A4 die Engine-Einträge selbst emittiert.
  Nachgeprüft: es gibt keinen allgemeinen „unbekannte Variable"-Check, `combat.*`
  ist in Regeln also frei per `compare_var`/`set_var` nutzbar — A2 hängt nicht an
  A4. `combat.initiative.<actorId>` folgt in A2, sobald der Treiber es braucht
  (kein Zugriff auf Vorrat).
  Nebenbei korrigiert: die Doku zeigte noch die alte `resolveCombat`-Signatur
  ohne `CombatAction`.
- **7f-3 (`tactical`), Schritt A2 — Runden-Treiber (`resolveTactical`):**
  - Eine Spieleraktion = eine Runde. Der Gegner reagiert über `on: turn`-Trigger.
  - `CombatTactical TacticalCombat` verdrahtet in `resolveCombat`.
  - Aktionen: `CAAttack` (Schaden an NPC), `CADefend` (Markierung `combat.action = defend`),
    `CAFlee` (Flucht, `combat.engaged = 0`, falls `tcFleeAllowed`).
  - Verben `defend` und `flee` im Parser geroutet.

- **7f-3 (`tactical`), Schritt A3 — Player Abilities & BySpeed Initiative:**
  - `PlayerAbility`: `paId`, `paName`, `paCostVar`, `paCost`, `paCooldown`, `paEffects`.
  - `abilities :: Map.Map String PlayerAbility` in `GameWorld`.
  - `tcSpeedAttribute :: String` (Default `"speed"`) in `TacticalCombat`.
  - `resolveTactical` für `CAAbility abId`: Cooldown-Gating via Condition-System (`cooldown_<abId>`),
    Ressourcenkosten-Prüfung und -Abzug via `paCostVar`, Ausführen der `paEffects`.
  - `BySpeed` Initiative: Auswertung von `tcSpeedAttribute` bei Spieler (`playerSkills`) und
    Gegner (`npcProps`), automatische VarMap-Einträge `combat.initiative.player` und
    `combat.initiative.<npcId>`.
  - Parser-Unterstützung für `use-ability <id>`, `use ability <id>`, `ability <id>`.
  - 3 neue Tests in `test/Tests.hs`: `testAbilityCost`, `testAbilityCooldown`, `testBySpeedInitiative`.
- Tests: **241** Engine- + **75** Worldbuilder-Tests, **29** E2E-Läufe.
  (Zahlen nach dem Code-Check korrigiert: hier standen 233 und 24 — gemessen sind
  es 241 registrierte Tests und 29 Läufe in `scripts/ci.sh`.)

- **Doku-Korrekturen aus dem Code-Check nach dem Pull:** `docs/modules.md`
  behauptete für `defend` im taktischen Profil einen „Verteidigungsbonus für eine
  Runde" — den gibt es nicht. Der Resolver erklärt den Zug nicht selbst für
  ungültig, sondern setzt `combat.action = "defend"` und legt ihn in die Hand der
  Gegner-Regel; ein Bonus ist Autoren-Daten. Ebenso versprach
  `docs/adventure-schema.md` Autoren, Text-Variablen mit `compare_var`
  vergleichen zu können: die Prädikat-Sprache kann Text **nicht** lesen
  (`compare_var` verlangt Int, `compare` löst Text zu `0` auf). Beides ist jetzt
  korrekt beschrieben, samt der bisher undokumentierten Schlüssel
  `combat.action` (Werte `attack`/`defend`/`flee`/`ability`) und `combat.ability`.

- **F1/F2 abgeschlossen: Text-Prädikat `{ var: X, is: Y }`** (Befund aus dem
  Code-Check). `combat.action`/`combat.ability` sind Text, und die Prädikat-Sprache
  konnte Text **nicht** vergleichen (`compare_var` verlangt Int, `compare` löst
  Text zu `0` auf) — der Hook war damit write-only, und `defend` im taktischen
  Kampf hatte keine mechanische Wirkung. Neu: `VarIs String String` mit dem
  Kürzel `{ var: <name>, is: <text> }` — exakter Vergleich, nur für
  Text-Variablen (ein Int-Wert `1` matcht nicht gegen `is: "1"`), `not` und die
  übrigen Verknüpfungen funktionieren wie sonst. Damit ist zugleich die
  Doku-Anweisung zu `variables: type: text` wieder wahr: sie verwies für das
  Lesen auf `compare_var`, was für Text nie funktioniert hat.
  `combat-tactical.yaml` macht `defend` jetzt wirksam — die Konterregel ist per
  `not: { var: combat.action, is: defend }` gegated, dazu eine Gegenregel, die
  den geblockten Hieb meldet. Neuer E2E-Lauf `combat-tactical-defend`.
- **F3: taktischer Resolver entdoppelt** (Befund aus dem Code-Check). Die
  `TargetNPC`- und `TargetShip`-Zweige waren Kopien: der Fähigkeiten-Block stand
  wörtlich doppelt (~34 Zeilen), `defend` und `flee` ebenso. Die gemeinsamen
  Rümpfe liegen jetzt einmal in `tacticalDefend` / `tacticalFlee` /
  `tacticalAbility`; die acht Gleichungen sind einzeilige Delegationen, die nur
  noch Ziel und Aktion wählen. Die beiden `attack`-Zweige bleiben getrennt — dort
  unterscheiden sich Schadensmathematik (Verteidigungswert vs. `effectiveAttack`)
  und Meldung („kill" vs. „destroy") echt. Netto −18 Zeilen (110 entfernt,
  92 neu: Signaturen, Kommentare, die drei gemeinsamen Rümpfe).
  **Verhaltensneutral belegt:** die Ausgaben von sieben E2E-Läufen
  (`combat-tactical`, `-fail`, `-defend`, `ship-duel`, `-fail`, `combat-classic`,
  `combat-narrative`) sind vor und nach dem Umbau byteweise identisch.
- **F4: `CAUseItem` bleibt Platzhalter — aber ehrlich** (Befund aus dem
  Code-Check). Der Konstruktor wurde nie erzeugt (kein `use <item>`-Kampfverb),
  der Fallback war damit unerreichbar, und die `ToJSON`/`FromJSON`-Instanzen von
  `CombatAction` wurden nirgends benutzt — der Typ ist nie ein Feld eines
  persistierten Typs. Die Instanzen sind entfernt, der Konstruktor bleibt als
  reservierter Platz bewusst stehen und ist als solcher dokumentiert; der Test
  `CAUseItem is a pinned placeholder` nagelt den Fallback fest, damit die Lücke
  sichtbar bleibt statt still halb verdrahtet zu werden.
- **F5: `combatInitiativeNpcKey` → `combatInitiativeKey`** — der ausgegebene
  VarMap-Schlüssel war schon immer neutral (`combat.initiative.<id>`), nur der
  Haskell-Name sagte „Npc", obwohl Schiffe ihn mitbenutzen. Reine Umbenennung
  (Schlüssel unverändert), dazu ein Kommentar zum geteilten ID-Raum.
- **F6: `cooldown_`-Condition-Namespace geschützt** — Fähigkeits-Cooldowns legt
  die Engine als Condition `cooldown_<abilityId>` an; das Präfix war ungeschützt,
  während `combat.`-Variablen seit A1 per `CombatVariableClash` geschützt sind.
  Neuer Compile-Check `checkCooldownConditionReserved` (scannt alle Effektbäume
  inklusive verschachtelter) meldet `CooldownConditionClash`; dokumentiert in
  `adventure-schema.md` (Fähigkeiten) und `modules.md` (Rundenzustand).
- Tests: **242** Engine- + **76** Worldbuilder-Tests, **29** E2E-Läufe.

### ASCII-Kunst: Werkzeug repariert, Werkzeugkette geschlossen (Phase A)

- **Geometrie korrigiert (Befund B1):** Die Ausgabe war rund doppelt so hoch wie
  sie sein sollte — eine 200×200-Quelle ergab bei Zielbreite 60 *60* Zeilen statt
  30, weil eine Terminalzelle etwa 2:1 hoch ist. `rowsForWidth`/`widthForRows`
  leiten die Zeilenzahl jetzt aus dem Bildseitenverhältnis ab, und `--height`
  erlaubt die Angabe in Zeilen statt Zeichen. Nachgemessen an einem echten
  512×512-Foto: 40 Zeichen × 20 Zeilen.
- **Flächenmittelung statt Punktabtastung (Befund B2):** `scaleToGrid` mittelt den
  Quellbereich jeder Zelle (Box-Filter). Vorher wurde ein einzelnes Pixel
  gesampelt: ein 1-Pixel-Schachbrett lieferte nur die Extreme `" "` und `"@"`,
  heute genau einen Mittelgrau-Wert (127,127,127).
- **Halbblock-Modus (Entscheidung D5):** `-m half` packt zwei Pixel pro Zelle
  (`▀` mit Vorder-/Hintergrund in 24-Bit-Farbe, Farbcodes nur bei Änderung, jede
  Zeile endet mit Reset) — doppelte Vertikalauflösung. Braucht einen
  Farbterminal; bei umgeleiteter Ausgabe warnt das Werkzeug.
- **Erste Test-Suite für `img2ascii`:** acht Gruppen (Geometrie, Mittelung,
  Rampe/Invertierung, alle fünf Zeichensätze, Halbblock, Fehlerpfade, 1×1-Bild,
  ungerade Höhe) mit synthetischen Bildern — läuft über `cabal test all` im CI mit.
- **Hygiene:** die tote Konfigoption `asciiColor` ist entfernt (sie war als
  „future" deklariert und wurde nie gelesen), das von Hand ausgerollte
  `isPrefixOf` in `app/Main.hs` ist durch `Data.List.isPrefixOf` ersetzt, und
  `--width`/`--height` schließen sich exakt aus (der Parser führt Buch, statt am
  Standardwert zu raten).
- **Doku:** `docs/adventure-schema.md` beschreibt die `ascii`-Form jetzt mit
  Beispiel, Werkzeugaufrufen und den beiden YAML-Fallen (die erste Zeile muss die
  geringste Einrückung haben; die gemeinsame Einrückung wird entfernt, die
  relative bleibt).
- Laufzeit (gemessen): ein 3-MP-Foto ergibt bei Breite 80 in 0,43 s Kunst — das
  Werkzeug konvertiert offline, nicht im Spiel.

### ASCII-Kunst: Zustandsabhängige Kunst (Phase B)

- **`ascii` ist jetzt ein `CondText` (Entscheidung D1):** Die Kunst eines Raums
  hängt am Spielzustand. `ascii:` nimmt weiterhin einen String (Kurzform,
  `{default: ...}`) oder ein Object `{default, variants}`; die erste zutreffende
  Variante gewinnt, sonst der Default. Kein neuer Interpreter — dieselbe
  `CondText`/`resolveCondText`-Mechanik wie bei `description`.
- **Neue Felder `npcAscii` / `itemAscii` (Entscheidung D10):** Dieselbe
  Objektform auf NPCs und Items. `look at <npc>` bzw. `look at <item>` gibt die
  zustandsabhängige Kunst über der Beschreibung aus — z. B. ein Gegner
  lebend/tot (`when: { state: troll, is: dead }`).
- **Engine:** `roomAscii` von `Maybe String` auf `CondText` gehoben; die
  Auflösung passiert an der Renderstelle (`Look`), reine Funktion von
  `GameState`. Alte Welten mit `ascii: "<string>"` laden unverändert; fehlendes
  `ascii` ergibt leere Kunst.
- **Worldbuilder:** `ARoom`/`AItem`/`ANPC` kompilieren String- *und* Objektform
  1:1 auf die Engine-`CondText`.
- **Fixture + Tests:** `examples/fixtures/ascii-state.yaml` (Raum dunkel/hell
  über Flag, Item- und NPC-Kunst über Status) wird kompiliert und validiert;
  dazu sechs Engine-Tests (Default, Variante, Variantenreihenfolge, NPC
  lebend/tot, Item-Status, JSON-Round-Trip) und zwei Worldbuilder-Tests
  (String-/Objektform, Fixture).
- **Doku:** `docs/adventure-schema.md` beschreibt die zustandsabhängige Form mit
  Beispielen für Räume, Items und NPCs.

### ASCII-Kunst: Farbe (Phase C)

- **Konverter (Entscheidung D2):** `img2ascii --color` färbt die Zeichenrampe
  mit 24-Bit-ANSI (opt-in), `--no-color` schaltet jede ANSI-Ausgabe ab. Im
  Halbblock-Modus ist Farbe strukturell (Standard an); `--no-color -m half`
  liefert reine `▀`-Zeichen. Farbcodes werden nur bei Änderung emittiert, jede
  Zeile endet mit `ESC[0m` (kein Leck in die Folgezeile).
- **Engine:** neue reine Funktion `Ansi.stripAnsi` entfernt CSI-Sequenzen;
  `Ansi.ansiFilter` wählt `id` nur bei TTY **und** ohne `--no-color`, sonst
  `stripAnsi`. `app/Main` prüft `hIsTerminalDevice stdout` und reicht den
  Filter über `GameLoop.runGameWith` an alle spielerseitigen Ausgaben durch —
  der Kern bleibt farbblind. `--no-color` ist ein neues CLI-Flag.
- **Tests:** sechs neue Fälle — `stripAnsi` (SGR, Reset, mehrzeilig),
  `ansiFilter`-Policy (TTY × Flag), farbige Raumkunst → gefiltert, sowie im
  Konverter farbige Rampe (Escape vorhanden, sichtbarer Text gleich, Reset am
  Zeilenende) und `--no-color`-Halbblock (nur `▀`, kein Escape).
- **Doku:** `adventure-schema.md` beschreibt `--color`/`--no-color` und die
  automatische Bereinigung bei umgeleitetem stdout.

### ASCII-Kunst: Bewegte Kunst (Phase D)

- **Datenmodell:** `ascii` ist jetzt ein `AsciiArt` mit `aaStatic :: CondText`
  (Phase-B-Zustandskunst), `aaFrames :: [CondText]` (Animation, jeder Frame
  selbst zustandsabhängig) und `aaEvery :: Int` (Takt in Zügen; 0 = passiv aus).
  String- und CondText-Kurzform bleiben gültig; eine frame-lose Kunst wird
  weiterhin als CondText-Objekt serialisiert (kein Checksummenbruch).
- **Passiv (Entscheidung D3):** `look` zeigt den Frame `turnCount div every mod
  len(frames)` — eine reine Funktion des Spielzustands, kein Timer. Gilt für
  Räume, Items und NPCs.
- **Aktiv:** neuer Befehl `watch [ziel]`. Die Engine liefert über
  `asciiFrames` die fertige Frame-Liste (reines `GameState`), der IO-Loop spielt
  sie mit Verzögerung ab (`pendingAnimation`, nur zur Laufzeit, nicht im Save).
  `watch` kostet keinen Zug. `watch` ist reserviertes Verb.
- **Worldbuilder:** `AAscii` (String/CondText/frames-Objekt) kompiliert auf
  `AsciiArt`.
- **Fixture + Tests:** `ascii-state.yaml` enthält nun einen animierten Raum
  (flackernde Fackel, `every: 2`); dazu fünf neue Engine-Tests (passiver Frame
  über `turnCount`, Frame-Liste, `watch`-Befehl inkl. „kein Zug“) und ein
  Worldbuilder-Test (frames + every).
- **Doku:** `adventure-schema.md` beschreibt `frames`/`every` und `watch`.

### Banner aus Text (Phase G)

- **Neues Paket `text2ascii` (D9):** Text→Banner-Kunst mit drei eingebauten
  Fonts (Block, Slant, Outline) ohne Datenfiles (D7). Block ist ein
  handgezeichnetes 5-Zeilen-Bitmap; Slant und Outline werden daraus abgeleitet
  (Neigung bzw. dilatiert + Rand). CLI wie `img2ascii`: `-f/--font`,
  `-g/--gap`, `--color`/`--no-color`, Text als Argumente oder stdin. Eigenes
  Test-Suite (Glyphenabdeckung, Fallback, Breite, Fonts, Farbe, Mehrzeiler),
  läuft über `cabal test all` im CI.
- **Engine – Endbildschirme (D8):** `GameWorld.worldEndArt :: Map String
  AsciiArt` mit Schlüsseln `"death"`, `"victory"` und eigenen
  `game_end`-Texten. `handleGameOver` zeigt das Banner statt des festen
  Rahmens; **ohne** Eintrag bleibt der bisherige Rahmen (rückwärtskompatibel).
  Die Steuerhinweise bleiben erhalten.
- **Engine – Titel:** `GameWorld.worldTitleArt :: AsciiArt` ersetzt bei
  gesetztem Feld das einzeilige `bannerFor`; leerer Default = altes Verhalten.
  `app/Main` rendert ihn durch denselben ANSI-Filter wie alles andere.
- **Worldbuilder:** Top-Level `title_art` und `end_art` (String/CondText/
  animated) kompilieren in das `GameWorld`.
- **Fixture + Tests:** `examples/fixtures/banner-art.yaml` (generierter
  Titel + `end_art` für victory/death, per Knopfdruck erreichbar);
  Engine-Tests (`endArtFor` je Grund, Titelauflösung) und zwei Worldbuilder-
  Tests (Kompilierung, Fixture). Neuer E2E-Lauf `banner-art` prüft, dass das
  Ende tatsächlich das `end_art` zeigt.

### Anfassbare Kunst: Hotspots (Phase E)

- **Datenmodell:** `AsciiArt.aaHotspots :: [Hotspot]` mit `hsGlyph` (Marker im
  Bild) und `hsTarget` (Item-/NPC-ID). YAML: `hotspots: [{glyph, target}]`.
  Serialisierung nur, wenn Hotspots vorhanden — kein Checksummenbruch.
- **Hervorhebung (D4):** `look` zeigt die Marker farbig hervorgehoben (SGR wird
  vom Output-Filter bei Pipe/`--no-color` entfernt, der Kern bleibt
  farbblind).
- **Adressierung:** `map`/`legend` gibt die Kunst mit **Nummern** statt Markern
  und eine Legende aus. `look at <n>` löst die n-te Marke auf ihr Ziel auf; der
  normale Name (`pull lever`) funktioniert unverändert. `map` kostet keinen Zug.
- **Parser:** Nummern werden nur als Interaktionsziel aufgelöst (kein Konflikt
  mit Dialogzahlen).
- **Validierung (Compiler):** `UnknownHotspotTarget`, `HotspotGlyphMissing`,
  `DuplicateHotspotGlyph`, `ReservedHotspotGlyph` (Ziffern/Leerraum).
- **Fixture + Tests:** `examples/fixtures/hotspot.yaml` (Hebel + Troll, per
  Nummer und Name erreichbar); drei Engine-Tests (Nummernauflösung,
  Hervorhebung, `map`) und zwei Worldbuilder-Tests (Fehlerpfade, Fixture).
  Neuer E2E-Lauf `hotspot`.

### ASCII-Kunst: Leichen bleiben liegen, Kampfrunden zeigen den Gegner

Nacharbeit zum Code-Check der Phasen B–E. Zwei Lücken, die zusammen die
Gegner-Kunst („lebend/tot") im Spiel unerreichbar machten:

- **Der Tod versetzt den NPC nicht mehr ins Nichts.** `killNPC` setzte
  `npcLocation = Removed`, also fiel jede ortsbasierte Suche aus — `look at
  <name>` meldete „You don't see … here", und die `dead`-Variante der NPC-Kunst
  konnte **nie** erscheinen. Der NPC hat jetzt nur noch den Status `dead`, die
  Leiche bleibt im Raum. Damit sie niemand für einen Gesprächspartner hält,
  filtert die Regel „ein Körper ist kein Partner" an genau einer Stelle
  (`isDeadNPC`, genutzt von Kampf-Zielsuche, Begleiterliste und Raumliste):
  - die Raumliste meldet sie getrennt: *„The body of X lies here."* statt unter
    „Also here",
  - `attack` antwortet *„X is already dead."*, `talk` *„X is dead and says
    nothing."*,
  - `look at`/`watch` und Autoren-Verben (`verb_map`) funktionieren weiter.
- **Jede Kampfrunde rendert die Kunst des Gegners** — und zwar *nach* dem
  Anwenden der Effekte, damit die tödliche Runde bereits den toten Zustand
  zeigt. Das ist der Weg, auf dem „Gegner lebend/tot" sichtbar wird; der alte
  Test prüfte nur den Mechanismus mit von Hand gesetztem Status.
- **Fixture ehrlich gemacht:** `ascii-state.yaml` hat einen Weg zur
  `burned`-Variante der Fackel (`light torch`) und einen neuen E2E-Lauf
  `ascii-state` (31 → **32** Läufe), der die tote Kunst `( x_x )` nachweist.
- **Tests:** `corpse stays findable through the kill path` fährt den *echten*
  Todesweg (Angriff mit 1 HP) und prüft Kunst in der tödlichen Runde, Fundort,
  Ansprechbarkeit und die beiden Verweigerungen.
- **Kleinkram:** Schlusszeilenumbrüche in `img2ascii/app/Main.hs`,
  `img2ascii/src/ImgToAscii.hs` und `text2ascii/app/Main.hs`; der Phase-A-Eintrag
  unten sagt zwar „`asciiColor` entfernt", Phase C führt es aber als lebendiges
  Feld (`Maybe Bool`, auto) wieder ein — beides richtig, in dieser Reihenfolge
  nur missverständlich.

### Stealth: ein toter Beobachter hört nichts mehr

Befund beim Prüfen der Auswirkungen der Leichen-Änderung — vorbestehend und
latent, weil die Stealth-Fixture die Wache nie tötet:

- Der generierte Beobachtungstrigger (`stealth.observe.<npc>`) hatte als
  Bedingung **nur** `noise >= hears_at`. Ein getöteter (oder längst
  verschwundener) Wächter hörte also weiter und rüstete um.
- Der Trigger verlangt jetzt zusätzlich `PNot (EntityHasState <npc> "dead")` —
  dasselbe Kriterium wie `isDeadNPC` in der Engine, ausgedrückt mit vorhandenen
  Prädikaten, kein Kern-Eingriff. Distanz modelliert weiterhin `hears_at`,
  deshalb gibt es bewusst keine Raumprüfung: ein lebender Wächter hört durch
  Wände, ein toter hört nie.
- Der bestehende Test prüfte die Bedingung *formgleich*
  (`CompareVar noise >= 5`) und ist mitgezogen; dazu kommt ein Verhaltenstest,
  der die generierte Bedingung mit demselben Auswerter prüft, den die Engine
  beim Feuern benutzt (`evalPredicate`): bei gleichem Lärm feuert sie für einen
  lebenden Wächter und nicht für eine Leiche.
- Doku: `docs/modules.md` beschreibt die Semantik.

## [0.9.0.0] — Unreleased

Phase 5 (Worldbuilder + Ports): Worldbuilder YAML/JSON-Compiler und TheFog-Portierung.

### Added
- **Worldbuilder-Package (`worldbuilder/`)**:
  - `Worldbuilder.Types`: Authoring-Schema (Adventure, ARoom, AItem, ANPC, AQuest, AVehicle, ADialogueTree, AActionOutcome) mit FromJSON-Instanzen für JSON und YAML.
  - `Worldbuilder.Compile`: Schema → engine GameWorld + SaveState (Räume, Exits, Items, NPCs/Dialoge, Quests, Vehicles, Interaktionen, Outcomes).
  - `Worldbuilder.CLI`: CLI-Befehle `validate`, `compile`, `check` mit JSON/YAML-Unterstützung.
  - `Worldbuilder.ParseFile`: Auto-Erkennung von .json/.yaml/.yml, YAML-Parsing via HsYAML-aeson.
- **Engine-Erweiterungen für TheFog-Kompatibilität**:
  - `Southeast` in `Direction` (für Geheimgänge, Kanonensprünge, Schrein-Eingänge).
  - `activate` als Synonym für `VUse` (Schrein-Aktivierung per `activate <target>`).
  - `swim`, `crawl`, `dig`, `game`, `se` als Aliase für `Go Southeast`.
- **Beispiel: TheFog-Portierung** (`examples/thefog.yaml`):
  - 55 Räume mit komplettem Wegenez (Home, Garden, Forest, Graveyard, Mountain, Canyon, Castle, 4 Schrein-Locations).
  - 10 Items (Paper, Map, Apple, Shield, Crystal, Sword, 4 Schreine) mit Platzierungen aus dem Original.
  - 2 NPCs: Wolf (Kampf, 30 HP) und Princess.
  - 5 Quests (4 Schrein-Aktivierungen + Wolf besiegen).
  - 4 Schrein-Interaktionen: `use crystal on <shrine>` aktiviert den Schrein und schaltet die Quest weiter.
  - Wird via `worldbuilder compile examples/thefog.yaml` in spielbare Engine-Dateien übersetzt.

## [0.8.1.0] — Unreleased

Phase 4.6 (Core Polish): Dialogue-Tree-Interaktivität, CLI-Validierung und Room-ASCII-Art.

### Added
- **Interaktives Dialogue-Tree-System**:
  - `ChooseCmd Int` im Command-Parser: unterstützt `choose <n>`, `pick <n>`, `option <n>`, `select <n>` und bare Zahlen (`1`, `2`, ...).
  - `SaveState.activeDialogue :: Maybe NPCID`: Trackt den aktiven Gesprächspartner zur Laufzeit (rückwärtskompatibel, default `Nothing`).
  - `DialogueChoice`: Neues Feld `dcNextNode :: Maybe String` für nahtlose Navigation im Dialogbaum (`Nothing` beendet das Gespräch).
  - Automatisches Rendern des nächsten Dialogknotens nach Ausführen des Choice-Outcomes.
  - `setDialogueNode` erzeugt initialen State, falls der NPC noch nicht in `npcStates` existiert.
  - Verlassen des Dialogs bei Bewegung oder Verlassen des Gesprächs.
  - Help-Text und Tab-Completion (`choose`, `option`) aktualisiert.
  - Sample-Adventure: `oldman` hat jetzt einen vollwertigen verzweigten Dialogbaum mit Quest-Hint und Flag-Setzung.
- **World-Validierung in CLI (`app/Main.hs`)**:
  - `validateWorld` wird bei `--world` vor dem Spielstart ausgeführt und listet gefundene Konsistenzfehler als Warnung auf.
- **Dialogue-Tree-Validierung in `Validate.hs`**:
  - Neue Fehler: `MissingDialogueNode` (fehlender Einstiegsknoten) und `DanglingDialogueChoice` (ungültiger Folgeknoten).
  - `allOutcomes` erfasst jetzt auch alle ActionOutcomes aus Dialogue-Choices.
- **ASCII-Art Unterstützung in Räumen**:
  - `Room.roomAscii :: Maybe String`: Optionales Banner-Feld (z. B. aus `img2ascii`), das bei `look` über dem Raumnamen angezeigt wird.
- 8 neue Tests für Dialoge, ASCII-Art und Dialogue-Validierung (jetzt 95 Tests, alle grün).

## [0.8.0.0] — Unreleased

Phase 4.5 (Engine-Qualität): World-Validierung.

### Added
- **Neues Modul `Validate`**: `ValidationError`-ADT (MissingRoom, MissingItem,
  MissingNPC, MissingQuest, MissingVehicle, DanglingExit, UnreachableRoom,
  DuplicateID, MissingSetFlag) und `validateWorld :: GameWorld -> [ValidationError]`.
- Prüfungen:
  - Exit-Ziele existieren
  - Alle Räume sind (abseits von Vehicle-Räumen) via BFS über offene + verschlossene
    Exits erreichbar
  - Items/NPCs/Quests/Vehicles aus ActionOutcome-Bäumen sind in den Definitionen
    vorhanden (typ-spezifische Collector-Funktionen vermeiden False Positives)
  - Doppelte IDs zwischen Kategorien (Vehicle-Item-Paarungen erlaubt)
  - Flags aus CheckFlag, die nie per SetFlag gesetzt werden
- Cabal: `Validate` in `exposed-modules` aufgenommen.
- 4 neue Tests (Sample-Welt ist gültig, hängender Exit, Duplikat, unerreichbarer Raum).

## [0.7.0.0] — Unreleased

Phase 4.4 (Engine-Qualität): Narrative Inserts.

### Added
- **Narrative Outcome**: `Narrative [String] ActionOutcome` — eine Liste von
  Zeilen, die nacheinander mit `[Press Enter to continue]` angezeigt werden,
  gefolgt von einem Folge-Outcome. Der reine Pfad gibt alle Zeilen auf einmal
  zurück und speichert das Follow-Up in `pendingNarrative` (kein Seiteneffekt
  bis zur interaktiven Anzeige).
- `GameState.pendingNarrative :: Maybe ([String], ActionOutcome)` — nicht
  serialisiert, nur zur Laufzeit. Die GameLoop rendert es zeilenweise mit
  `getLine`-Pause, wendet dann das Follow-Up an.
- Sample-Adventure: Das `meadow` führt jetzt eine kleine Narrative beim
  Betreten aus.
- 3 neue Tests (Lines-Rückgabe, Pending-Flag, JSON-Roundtrip).

## [0.6.0.0] — Unreleased

Phase 4.3 (Engine-Qualität): Undo.

### Added
- **Undo-System**: `LoopState` hält den aktuellen `GameState` und bis zu 50
  vorherige Zustände (neuester zuerst). `undo` stellt den kompletten Zustand
  inklusive Room, Inventory, Conditions, Quests, Vehicles und Turn-Counter
  wieder her.
- `undo` verbraucht selbst keinen Zug; bei leerer Historie erscheint
  `Nothing to undo.`.
- Save/Load/ListSaves/Restart/Help/Quit erzeugen keine Undo-Einträge; ein
  erfolgreicher Load startet bewusst eine neue History.
- Nach einem tödlichen Zug bietet der Death-Screen `[U]ndo` an, sodass der
  letzte Zustand direkt wiederhergestellt werden kann.
- Help-Text und Tab-Completion enthalten `undo`.
- 7 neue Undo-Tests: Restore, leere History, mehrfaches Undo, 50er-Limit,
  Meta-Commands und Wiederherstellung nach Tod.

## [0.5.0.0] — Unreleased

Phase 3 (Vehicles): First-Class-Fahrzeuge mit eigenen Innenräumen, Routen
und vehicle-weiten Conditions.

### Added
- **Vehicle-System**: `VehicleDef`/`VehicleState`, drei Typen
  (`PlayerControlled`, `AutomaticRoute`, `PaidVehicle`). Innenräume sind
  reguläre Rooms im World-Map (Hooks/Tags/Lighting funktionieren dort);
  `vehicleRooms` listet sie. Stops verbinden Außenräume mit Labels und
  optionalen Kosten (`stopCost` für PaidVehicles).
- **SaveState**: `vehicleStates`, `currentVehicle` (beide mit Defaults,
  alte Saves bleiben ladbar). **GameWorld**: `vehicleDefs`.
- **Commands**: `enter`/`board <vehicle>`, `exit`/`disembark`,
  `drive to <station>` (nur vom Cockpit, PlayerControlled),
  `wait` (AutomaticRoute: nächste Station), `refuel [vehicle]` (Status),
  `repair <condition>` (cleart Vehicle-Condition).
- **Tanken**: `use <fuel-item> on <vehicle>` — das Item-Prop `fuel` (default 1)
  wird gutgeschrieben, Item wird verbraucht; Cap via `vehicleFuelProp`.
- **Vehicle-weite Conditions**: `vsActiveConditions` feuern ihre
  `vehicleConditionEffects`-Outcomes einmal pro Zug (game loop), solange man
  an Bord ist. `vsRoomOverrides` ersetzen Raum-Beschreibungen in `look`;
  `look` zeigt zusätzlich Fuel/Conditions-Status.
- Sample-Adventure: Pferdekutsche (PlayerControlled) mit Cabin, Cockpit,
  zwei Stationen (`start`, `meadow`) und Heu als Treibstoff.
- 9 neue Tests (Enter/Exit/Drive/Refuel/Condition-Tick/JSON-Roundtrip).

## [0.4.0.0] — Unreleased

Phase 2 (Spiel-Systeme): Skills, Conditions, Quests.

### Added
- **Skill system**: `Player.playerSkills` and `CheckSkill` (skill + d6 vs DC,
  salt-threaded RNG so rolls differ), `ModifySkill`. Stats shows skills.
- **Conditions (status effects)**: `Condition` (name, remaining turns, tick and
  end outcomes), `SaveState.conditions`, outcomes `ApplyCondition`,
  `ClearCondition`, `HasCondition`. Ticked once per command in the game loop;
  tick messages are printed before the command's own output.
- **Quests**: `Quest`/`QuestStage` definitions in `GameWorld.questDefs`,
  `SaveState.activeQuests`/`completedQuests`, outcomes `StartQuest` (gated by
  `questPrereqs`), `AdvanceQuest`, `CompleteQuest` (fires the quest reward).
  `journal`/`quests` command shows active and completed quests.
- Sample adventure now includes a 3-stage quest (`find_treasure`, started by
  taking the key) and a prereq-gated quest (`gated_quest`) plus a lockpick skill.

### Fixed
- `CompleteQuest` swallowed the quest reward message; the reward outcome's
  message is now shown after the completion message.

## [0.3.0.0] — Unreleased

Phase 0 (Fundament) + Phase 1 (Authoring-Essentials).

### Added
- **Equipment system**: `EquipSlot` (Head/Body/Hands/Feet/Weapon/Offhand/Accessory),
  `EquipEffect` (AttackBonus/DefenseBonus/MaxHealthBonus), `SaveState.equipment`.
  Effective stats are computed on the fly via `effectiveAttack`,
  `effectiveDefense`, `effectiveMaxHealth`.
- **New commands**: `equip`/`wear`/`wield`, `unequip`/`remove`, `unequip all`,
  `stats`, `search` and `search <target>`.
- **New `ActionOutcome`s**: `EquipItem`, `UnequipItem`.
- **Room hooks**: `roomOnEnter`, `roomOnLook`, `roomOnExit`, `roomSearchOutcome`.
- **Room tags & lighting**: `roomTags`, `roomLightFlag`. A room tagged `dark`
  hides its contents unless the player carries a `lightsource` item or the
  room's `lightFlag` is set to `"true"`.
- **Alternative room descriptions**: `roomAltDescriptions` (flag → description).
- **Hidden items**: `itemHidden` / `itemDiscoverText` / `itemDiscovered`,
  revealed by `search`.
- **Dialogue trees**: `DialogueNode`, `DialogueChoice`, `DialogueTree` and
  `NPCDef.npcDialogueTrees`. `npcDialogue` remains as a fallback.
- **Item-on-item interactions**: `GameWorld.itemInteractions` (crafting).
- **World loading**: `World.loadGameWorld`, `loadSaveState`, `loadGame` plus
  CLI flags `--world FILE` / `--save FILE` / `--help`.
- **ID type aliases**: `NPCID`, `EntityID`, `VehicleID`, `QuestID`, `SkillID`,
  `FlagID`, `FactionID`.
- New module `Sample` holding the bundled demo adventure.
- New package `worldbuilder` (placeholder) wired up via `cabal.project`.

### Changed
- **`roomVisited` moved from `Room` to `SaveState.visitedRooms`** (breaking).
  `Room` is static world data and must not be mutated at runtime.
- `ItemDef` gained `itemTags`, `itemEquipSlot`, `itemEquipEffects`,
  `itemHidden`, `itemDiscoverText`.
- `ItemState` gained `itemDiscovered`.
- `NPCDef` gained `npcDialogueTrees`; `NPCState` gained `npcDialogueNode`.
- `Room` lost `roomVisited`; gained `roomTags`, `roomAltDescriptions`,
  `roomLightFlag`, `roomOnEnter`, `roomOnLook`, `roomOnExit`, `roomSearchOutcome`.
- `SaveState` gained `visitedRooms` and `equipment`.
- `applyOutcome` now threads an RNG salt and a recursion depth
  (`maxOutcomeDepth = 20`) to prevent runaway recursion from malformed content.
- `GameLoop.hs` split: save/load moved to `SaveLoad.hs`.
- `initSampleGame` moved out of `GameLoop` into `Sample`.
- Save schema version bumped `1 → 2`.

### Fixed
- **`RandomChoice` salt bug**: every random draw within the same turn used
  `salt = 0`, so consecutive `RandomChoice`s always produced the same result.
  The salt is now threaded through `MultipleOutcomes` and incremented.
- `use <item> on <target>` no longer swallows the "can't reach" message when an
  item-on-item interaction is undefined.
- `search` with no target parsed to `Unknown`; it is now its own `SearchCmd`.
- Combat uses `effectiveAttack` / `effectiveDefense` (equipment-aware).

## [0.2.0.0] — Unreleased

### Added
- Richer `ActionOutcome` constructors: `GiveItem`, `MoveItem`, `ConsumeItem`,
  `MoveNPC`, `SetRoomVisited`, `SetFlag`, `CheckFlag`, `RandomChoice`, `GameEnd`
- General-purpose flag system in `SaveState` for data-driven conditionals
- Deterministic hash-based pseudo-random outcomes (no external RNG dependency)
- `GameOverReason` (Victory / Death / Custom) with death→load/restart flow
- Named save slots with timestamps and world checksum validation
- `take all` / `drop all` commands
- Compound commands via `and`-splitting (`take torch and key`)
- Stop-word stripping (`the`, `a`, `an`) for more natural input
- `saves` command to list saved games
- `restart` command
- GitHub Actions CI workflow with coverage reporting

### Fixed
- Combat now correctly uses `npcDefenseBase` for damage reduction (was using
  `npcAttackBase`)
- NPC health clamped to `npcMaxHealth` when healed
- `use <weapon> on <npc>` now routes to attack instead of "Nothing happens"

### Changed
- Cabal file restructured with a `library` stanza to avoid double-compilation
- `SaveState` now includes `turnCount`, `flags`, and `gameOverReason` fields

## [0.1.0.0] — 2026

### Added
- Initial release
- Flexible game engine with rooms, items, NPCs
- GameWorld / SaveState architectural separation
- Dynamic verb system with `(Verb, State) → ActionOutcome` maps
- Command parser with synonym support and multi-word targets
- JSON save/load (SaveState only)
- Tab completion via Haskeline
- Sample adventure included
