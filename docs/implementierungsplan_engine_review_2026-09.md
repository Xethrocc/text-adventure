# Implementierungsplan nach Engine-Review (Sep 2026)

Grundlage: `~/workspace/engine_review_and_comparison_sep_27_2026.md`, geprüft gegen
den Code-Stand `3c6fdd0` (27.09.2026). Dieses Dokument fasst die Prüfergebnisse,
die getroffenen Entscheidungen und den Stufenplan zusammen.

---

## 1. Prüfergebnis in Kürze

| Review-Punkt | Befund |
|---|---|
| 2.1 Procedures | `raise`/`on: custom` existiert (parameterlos, Trigger-Semantik) — echte Procedures fehlen |
| 2.2 Interpolation | `{var}`, `{var:+}`, `{var:6}`, System-Vars **existieren**; es fehlen Inline-Conditionals, Ausdrücke, Props |
| 2.3 Konversation | Baum pro NPC-Status, Knoten persistiert über Besuche; es fehlen Topics, `OnTalk`, Barks, NPC↔NPC |
| 2.4 Scope/Container | `InContainer` + `ContainerState` angelegt, aber **nicht verdrahtet** (toter Code) |
| 2.5 Before/Instead/After | After global (`OnCommand`), Instead nur per `verb_map`; **kein Veto**, `take`/`go` nicht blockierbar |
| 2.6 Disambiguation | first-match; zusätzlich 2 Bugs (s.u.) |
| 2.7 Timer | `apply_condition` **ist** ein Named Timer; fehlt: Abfrage, Restzeit, `hidden` |
| 2.8 Welt-Physik | kein Gewicht/Limit/Liquids; Licht nur „getragen + Tag“; Licht-Leck (s.u.) |
| 2.9 I18n | zusätzlich ~80–130 hartkodierte englische Engine-Meldungen (genaue Zahl in 1.1) |
| 2.10–2.12 | stimmen; Architektur aber vorbereitet (`Frontend`-Record, pures `applyLoopCommand`, JSON überall) |
| Zahlen | veraltet: 26 Adventures, 328/165/20 Tests, 40 `.in`/`.expect`-Playthroughs (+ Worldgen-/Run-Regenerations-Checks), CI grün |
| `Text`-Empfehlung | verworfen — widerspricht dokumentierter, gemessener Entscheidung (`Types.hs:23–39`) |

### Neu gefundene Bugs

1. **B1 – Trigger-Fehlzuordnung:** `findItemIdByAlias` (`GameLoop.hs:314`) sucht weltweit;
   bei geteilten Keywords wird `OnTake`/`OnDrop` verschluckt, `OnUse` feuert fürs falsche Item.
2. **B2 – Raum vor Inventar:** `resolveInteractTarget` bevorzugt Raum-Items; `drop key`
   scheitert, wenn ein anderer `key` im Raum liegt.
3. **B3 – Licht-Leck:** Dunkelheit blockiert nur `look`/`watch`/`map`, nicht
   `take`/`examine`/`search`/`use`.
4. **B4 – `take` nicht verhinderbar:** `verb_map`-Eintrag auf `take` läuft *zusätzlich*
   (`Parser.hs:872–878`). → wird in Phase 4.2 gelöst (Semantikänderung, nicht als Hotfix).

### Gegenlesen (2026-09-27)

Alle Befunde im Code nachgeprüft:

| Punkt | Prüfergebnis |
|---|---|
| B1 | bestätigt — `findItemIdByAlias` sucht über `Map.elems (itemDefs (world state))`, also global |
| B2 | bestätigt — `allReachableItems = roomItems ++ invItems` (`Parser.hs:849`) |
| B3 | bestätigt — nur drei `isDark`-Prüfungen: `look` (456), `watch` (634), `map` (656) |
| B4 | bestätigt — im `VTake`-Zweig wird das `vmLookup`-Outcome *zusätzlich* nach `pickupItem` angewendet |
| `ContainerState` | bestätigt — kommt außerhalb von `Types.hs` **gar nicht** vor. (`Effects.hs:102` und `Validate.hs:514` betreffen die *Location* `InContainer`: Ablehnung von `MoveEntity … InContainer` bzw. `InvalidContainer`-Check.) |
| 2.1 / 2.2 / 2.7 | bestätigt — `RaiseEvent`/`OnCustomEvent` parameterlos; `formatStringWith` mit `{var:+}`/`{gold:6}`; `ApplyCondition` existiert, **kein** `HasCondition`-Prädikat |
| Zahlen | bestätigt — 26 Adventures (2 + 9 Genres + 15 Module), 328/165/20 Tests |
| Meldungszahl ~82 | **nicht bestätigt** — grobe Zählungen ergeben 102–130 unique großgeschriebene Literale (je nach Modulauswahl, inkl. Überschriften/Labels); exakte Zahl erst nach Klassifikation (siehe 1.1) |
| `verb_map` mit `take` in Beispielen | **drei** Einträge, alle siegentscheidend: `take,intact:` in `genres/fantasy.yaml:161`, `genres/space-opera.yaml:144`, `genres/cyberpunk.yaml:122` (jeweils `game_end: victory`) — die Migration in 4.2 trifft Bestandsinhalt und E2E-Siegpfade |
| README-Zahlen | in `435c056` bereits korrigiert (six suites, 26 Adventures, 40 Playthroughs = 28 + 12, plus Worldgen-/Run-Regeneration-Checks) |

Aus dem Gegenlesen ergänzt: 0.6 (`Types.hs`-Split, Review §5, fehlte im Plan),
Aufwandstabelle und Nicht-Ziele (Abschnitt 4), Styling-Modell in 1.2,
Protokoll-Spezifikation in 1.4, WASM-Spike (jetzt 1.0), Doku-Lieferung in Phase 3.

Zweites Gegenlesen (2026-09-27): `take`-Befund korrigiert (drei Bestandseinträge),
`ContainerState`-/B2-Belege präzisiert, 0.5 auf Rest reduziert, 0.6 direkt nach 0.5
festgelegt, WASM-Spike von 5.1 nach 1.0 vorgezogen, Rekursionsverbot für Procedures,
zählbasiertes Inventarlimit gegen Welt-Physik-Nicht-Ziel abgegrenzt.

---

## 2. Getroffene Entscheidungen

| # | Thema | Entscheidung |
|---|---|---|
| D1 | WebUI-Backend | **Hybrid:** zuerst Haskell-Server (WebSocket/HTTP), Protokoll so, dass später ein WASM-Build dasselbe bedient. Engine-Ausgabe wird auf **strukturierte Events** umgestellt. |
| D2 | Procedures | **First-Class:** `procedures:` + `call:`, Effekt `CallProc`, eigener Parameter-Scope, Compiler-Checks. `raise` bleibt für Multi-Listener-Events. |
| D3 | Veto-Phase | **Beides auf gemeinsamer Phase:** Stufe 1 `on: before <verb>` + `block` + Exit-`when:`; Stufe 2 Per-Objekt-Shorthand in `verb_map`. Reihenfolge objektbezogen vor global. Blockierte Aktion kostet **keinen Zug** (Opt-in `block: {turn: true}`). |
| D4 | I18n | **Sprachpakete** (`language: de`, inkl. Verb-Aliase) + `messages:`-Overrides, stabile Message-Keys, optionale Grammatikfelder (`article:`). Architektur offen für Multi-Language (später). |
| D5 | Reihenfolge | **Verzahnt in Stufen:** Fundament → kleine Engine-Features → WebUI v1 → große Features jeweils mit Editor-Tab → Distribution & Synth. |

**Umgang mit dem Haskell↔Frontend-Wechsel:** Phasen 0–2 sind reines Haskell, Phase 3
überwiegend Frontend. In Phase 4 wird jedes Feature *erst komplett in Haskell*
(inkl. Protokoll-Erweiterung, Tests, Doku) und danach *am Stück* im Frontend umgesetzt;
optional lassen sich die UI-Teile zweier Features bündeln. Das stabile, versionierte
Protokoll aus Phase 1 macht dieses Batching möglich.

---

## 3. Querschnittsregeln (gelten für jede Stufe)

- `scripts/ci.sh` grün; bestehende E2E-Ausgaben **byte-identisch**, außer die Stufe
  ändert Verhalten bewusst (dann `.expect` im selben Commit anpassen und begründen).
- Default-Invariante wie bei den Modulen: ohne neues YAML-Segment ändert sich nichts —
  **ausgenommen die Stufen, die bewusst Verhalten ändern:** 0.1, 0.2, 0.3 (Bugfixes),
  2.3 (Disambiguation) und 4.2 (`take`-Semantik). Diese dürfen `.expect` anpassen; die
  Änderung wird im selben Commit begründet und im CHANGELOG als Verhaltensänderung
  ausgewiesen.
- Warnungsgate in `scripts/ci.sh`: Das Muster für Test-Komponenten war auf das alte
  GHC-Format zugeschnitten (`warning: [-W…]`), GHC 9.6 schreibt `warning: [GHC-…]` —
  Warnungen aus Test-Komponenten rutschten dadurch durch. Muster auf
  `warning: [(-W|GHC-)` erweitert (2026-09-27); das Gate greift wieder.

- Save-Kompatibilität: neue Felder mit `.:?`-Defaults, Round-Trip-Test je neuem Typ.
- Jede Stufe: Unit-Tests, ggf. neues E2E-Fixture, Update `docs/adventure-schema.md`,
  `CHANGELOG.md`, README-Zahlen.

---

## 4. Stufenplan

### Phase 0 — Bugfixes & Aufräumen *(Haskell, klein)*

| Stufe | Inhalt | Abnahme |
|---|---|---|
| 0.1 | **Zentrale Zielauflösung:** eine Funktion `resolveTarget :: Verb -> String -> GameState -> TargetResolution` (Item-/NPC-/Vehicle-ID oder `Ambiguous [..]`/`NotFound`). `executeCommand` und `commandEvents` nutzen dieselbe aufgelöste ID → **fixt B1**. | Test: zwei Items mit Keyword `key`, `OnTake` feuert für das tatsächlich genommene |
| 0.2 | **Verbabhängige Suchreihenfolge:** `drop`/`use`/`equip` Inventar zuerst, `take` Raum zuerst → **fixt B2**. | Test für `drop key` mit Namensvetter im Raum |
| 0.3 | **Licht-Leck schließen** (B3): `take`/`examine`/`search`/`use` auf nicht-getragene Ziele im Dunkeln verweigern; Meldung konfigurierbar vorbereiten. *(Erledigt: `roomDarkMsg` / `dark_msg` / `dark_message`, Blockade für Raumobjekte im Dunkeln, Unit- & Schema-Tests)* **Nachtrag (2026-09-27) — `feelable`:** Autoren entscheiden pro Item, was im Dunkeln ertastbar ist. `take`/`examine`/`search`/`use` auf `feelable`-Zielen sind erlaubt, `take all` nimmt im Dunkeln nur ertastbare Items, ziel-loses `search` sowie NPCs und Fahrzeuge bleiben gesperrt. Hint-Kanal im Dunkeln ist `on_enter` (Raum-Hook und `on: enter`-Event laufen ohne Dunkelheits-Prüfung), `on_look` fällt weg. *(Erledigt: `aa9cc3a`)* | Tests + ggf. E2E-Anpassung prüfen *(erledigt)* — E2E-Fixture `dark-feelable` pinnt, dass eine Sackgasse Autorenentscheidung ist: ohne Tag ist das Item im Dunkeln nicht erreichbar |
| 0.4 | **Validierungs-Warnungen:** Keyword-Kollision zwischen Items/NPCs im selben Raum; unbekannte `{name}`-Platzhalter in Texten; **dunkler Raum mit erreichbaren Items, aber ohne `light_flag`, ohne `lightsource`-Pfad und ohne `feelable`-Item** (potenzielle Autoren-Sackgasse — siehe 0.3) | Worldbuilder-Tests |
| 0.5 | Aufräumen: `cleanedTgt` in `matchesNPCTarget` nutzen oder entfernen. `ContainerState` bleibt (Phase 4.4). *(README-Zahlen und E2E-Zählweise bereits in `435c056` erledigt.)* **Erledigt (2026-09-27):** Der `cleanedTgt`-Punkt war gegenstandslos — `cleanedTgt` wird in `src/Cards.hs:46,48` verwendet. Stattdessen **sieben ungenutzte Funktionen entfernt** (Referenzzählung über alle `.hs`): `getAllItemsInLocation`, `moveItemToRoom`, `unequipSlot`, `parseSimpleCommand`, `parseVerb`, `savesDirFor`, `computeVar`. Kein Aufrufer, keine geplante Nutzung (Doku-Treffer waren historische Plan-/CHANGELOG-Texte). | `scripts/ci.sh` grün |
| 0.6 | **`Types.hs`-Split** — **Erledigt (2026-09-27):** vier Commits `c0f3a99` (`Types.Cards`) → `b053c0d` (`Types.Vehicles`) → `5e56d10` (`Types.Combat`) → `f2934f6` (`Types.Core` + Fassade), jeweils nach grünem CI. Aufteilung entlang der Abschnittsgrenzen (Cards ab Z. 567, Vehicles ab 1350, Combat ab 1636); `src/Types.hs` ist jetzt eine 12-Zeilen-Fassade, kein `import Types` musste angepasst werden (0 Dateien in Engine, Worldbuilder, TUI, CLI, Tests). Modulgrößen: `Types.Core` 1886, `Types.Combat` 200, `Types.Cards` 158, `Types.Vehicles` 124. **Unabhängig verifiziert:** Engine-Tests 357 PASS / 0 FAIL (vorher wie nachher), `cabal build all --enable-tests` 0 Warnungen, `scripts/ci.sh` grün, und `world.json`/`save.json` von `thefog` sowie `world.json` von `demo` gegen einen Worktree auf `f941b46` **byte-identisch**. **Bekannte Kosten:** der Zyklus `Core` ↔ Submodule wird über `src/Types/Core.hs-boot` + drei `import {-# SOURCE #-}` aufgelöst — über diese Grenze inlinet GHC nicht, und die Boot-Datei ist bei Änderungen an `Effect`/`AsciiArt`/IDs manuell synchron zu halten. Zyklusfreie Alternative für später: ein `Types.Base` mit nur IDs/`Effect`/`AsciiArt`. **Abweichung vom reinen Umzug:** `noopEffect` als Alias neu ergänzt (Folge der Boot-Auflösung), keine Verhaltensänderung. | ✅ grün |
| 0.7 | **Explizite Exportlisten** (Nachtrag aus dem 0.5-Befund): `Parser.hs`, `SaveLoad.hs`, `World.hs` und `Game.hs` bekommen eine vollständige Exportliste. **Erledigt (2026-09-28):** 139 Funktionen sind öffentlich, **79 Funktionen und 1 Typ** (`MetaFile`) sind nicht mehr exportiert. Grund: `-Wunused-top-binds` gehört zu `-Wall`, greift aber nur bei *nicht* exportierten Bindungen — solange alles exportiert war, konnte GHC toten Code in diesen Modulen strukturell nicht melden (in 0.5 fielen sieben ungenutzte Funktionen erst durch manuelles Suchen auf). Commits: `Parser.hs` (43 verborgen), `SaveLoad.hs`/`World.hs` (9 + 0), `Game.hs` (27). `Worldbuilder.Types` **bewusst ausgelassen**: 100 der 125 Namen werden von außen gebraucht. CI grün, 357 Engine-Tests, 0 Warnungen, keine Verhaltensänderung. |

### Phase 1 — Ausgabe- & Loop-Architektur *(Haskell, Fundament für WebUI und I18n)*

| Stufe | Inhalt | Abnahme |
|---|---|---|
| 1.0 | **WASM-Machbarkeits-Spike:** Kern-Module (`Types`, `Game`, `Parser`, `Effects`, … ohne `haskeline`/`process`/`directory`) mit dem GHC-WASM-Backend bauen, ein `applyLoopCommand` im Browser ausführen. Zeitlich begrenzt, kein Produktionscode. Liegt **vor 1.4**, damit Protokoll und Frontend nicht auf einer ungeprüften Annahme aufsetzen; Erkenntnisse (z.B. IO-Abhängigkeiten, Paketprobleme) fließen in 1.3/1.4 ein. **(Erledigt 2026-09-28, positiv und besser als geplant: sogar die unveränderte Produktions-Library inkl. `Frontend`/`SaveLoad`/`Audio` baut und läuft unter wasmtime/Node byte-identisch zum nativen Lauf — `wasm-spike/`, Notiz `docs/wasm-spike-2026-09.md`; Aufwand 5.1 korrigiert auf ~M)** | Spike-Notiz: machbar ja/nein, Blocker, Aufwand für 5.1 *(erledigt)* |
| 1.1 | **Message-Katalog:** zuerst **Inventar** aller hartkodierten Meldungen anlegen (Zahl belegen — grobe Zählungen ergeben 102–130 Literale inkl. Überschriften/Labels, der Plan nannte ~82), dann auf stabile Keys (`MsgId`) + Argumente umstellen; englischer Default-Katalog als Daten. Rendern via `formatStringWith`. **(Erledigt 2026-09-28):** Neues Blatt-Modul `src/Messages.hs` (Katalog als Daten, `renderMsg`, `formatStringWith` aus `Game.hs` verlagert); **186 Keys** belegt (Rough-Grep: ~158 großgeschriebene Literale + ~30 kleingeschriebene Fragmente, zusammengesetzte Meldungen in Teil-Templates zerlegt); alle Engine-Module auf `renderMsg` umgestellt in 6 Teil-Commits (`1867c9b`–`7b7e336`). Inventar mit Klassifikation: `docs/message-catalog.md`. Bewusst nicht katalogisiert: Kampf-/Karten-Screen-Art (→ 1.2 Payloads), Worldgen-Content, interne Assertions/JSON-Tags/Env-Namen, `Sample.hs` (Adventure-Content). | E2E byte-identisch; Inventar-Liste im Commit *(erledigt: alle 42 E2E-Eingaben vor/nach byte-identisch, `diff -r` leer je Teil-Commit; 359 Tests zum Stand der Stufe)*. **Nachtrag (2026-09-28):** vier Keys ohne Aufrufstelle entfernt — `map.dark`/`watch.dark` (im Dunkeln rendert die autorenkonfigurierbare `darkRoomEv`-Meldung) und `map.legend_header`/`card.play.on` (Dubletten geteilter Inline-Fragmente) → **182 Keys**; neues Gate `scripts/check-msg-catalog.sh` (CI-Stufe 2b, beide Richtungen belegt) und echter Generator `scripts/gen-msg-catalog.py` für die Doku-Tabelle. |
| 1.2 | **Strukturierte Ausgabe:** `data OutputEvent` (z.B. `EvMessage MsgId Args`, `EvText String`, `EvRoomChanged`, `EvQuestUpdate`, `EvDialogue [Choice]`, `EvCombat …`, `EvSfx`, `EvMusic`, `EvArt`, `EvGameOver`). `applyLoopCommand` liefert `[OutputEvent]`; CLI/TUI rendern daraus Text. **Zwingend mitentscheiden: Styling-Modell** — heute stecken ANSI-Codes in den Ausgabestrings (`ansiFilter` filtert nur am Rand nach `isTty`/`--no-color`); die Events brauchen Style-/Span-Information und strukturierte Art-/Hotspot-Payloads, sonst muss die WebUI ANSI parsen oder verliert Farbe. **(Erledigt 2026-09-28):** Neues Blatt-Modul `src/Types/Output.hs` (Fassade): `OutputEvent` mit `EvMessage MsgPayload{mpKey,mpArgs,mpText}`, `EvText StyledText{stText,stSpans}` (Span-/Style-/Color-Modell + ANSI-Renderer), `EvArt ArtPayload{apRaw,apHotspots}`, `EvAnim/EvSfx/EvMusicStart/EvMusicStop` und textfreien State-Events (`EvRoomChanged/EvQuestUpdate/EvDialogue/EvCombat/EvGameOver`, abgeleitet in `GameLoop.sideEvents`). Primärpfad `applyLoopCommandEv`/`executeCommandEv` — die alten String-Signaturen bleiben als Kompat-Wrapper (`renderEvents`) für 363 Tests + Loop-Auxpfade. **Styling-Entscheidung:** Prosa ANSI-frei + Spans (Renderer am Rand), Art als dokumentierte Ausnahme roh + strukturierte Hotspots; `docs/output-events.md`. Fragment-Algebra (`joinEv`/`unlinesEv`/`evIntercalate`/`combineMsgsEv`) repliziert die vier Join-Idiome byte-exakt. | E2E byte-identisch *(alle 42 E2E-Eingaben leer im Diff)*; TUI-Tests grün *(alle 6 Suiten PASS)*; Styling-Entscheidung dokumentiert *(erledigt)*. **Korrektur (2026-09-28):** der Batch war **nicht** warnungsfrei — 14 Engine- + 3 Test-Warnungen (tote Import-Einträge, doppelte `Types.Output`-Importe neben der `Types`-Fassade, verwaiste `withAscii`/`combineMessages`); alle behoben, Ursache war ein gecachter `ci.sh`-Lauf ohne Neu-Kompilierung. |
| 1.3 | **Purer Session-Automat:** Save/Load/Restart/GameOver/Death/Victory aus `loopGame`/`deathLoop`/`victoryLoop` als pure Übergänge, die IO-Wünsche (`ReqSave slot`, `ReqLoad slot`, `ReqPersistMeta`, `ReqPause`) zurückgeben. `loopGame` wird ein dünner Interpreter, der diese Wünsche ausführt. **(Erledigt 2026-09-28, Commit `3c77685`):** Session-Request- (`SessionRequest`, `IoRequest`) und Session-State-Typen (`SessionState`), pure Übergänge (`transitionSave`, `transitionLoad`, `transitionLoadSuccess`, `transitionRestart`, `transitionGameOver`, `transitionDeathInput`, `transitionVictoryInput`, `advanceNarrative`); `loopGame`, `deathLoop`, `victoryLoop` und `runRestart` interpretieren diese Wünsche; Spiel-Logik ohne IO testbar (7 neue Unit-Tests in `test/Tests.hs`, 370 Tests gesamt); E2E auf allen 42 Fixtures byte-identisch. | E2E byte-identisch; neue Unit-Tests für Übergänge *(erledigt)* |
| 1.4 | **Protokoll v1:** JSON-Nachrichten `ClientMsg` (`command`, `choose`, `continue`, `load_world`, `save`/`load`) und `ServerMsg` (`events`, `snapshot` für HUD/Map/Quests). Versioniert, Golden-JSON-Tests. Kein Transport — nur Typen + Codec. **(Erledigt 2026-09-28, Commit `65f1cc1`):** Neues Blatt-Modul `src/Types/Protocol.hs` (`ClientMsg`, `ServerMsg`, strukturierte Snapshots für HUD/Map/Quests, `makeSnapshot`, deterministisches JSON via `encodeSorted`, strikt versionierte Decoder `decodeClientMsg`/`decodeServerMsg`); `ToJSON`/`FromJSON` für alle 12 `OutputEvent`-Konstruktoren in `src/Types/Output.hs`; Protokollgrenzen-Brücke `sessionLinesToEvents` für Session-Übergänge (1.3); Spezifikation `docs/protocol-v1.md` (Englisch) als verbindliche Frontend-Schnittstelle; 9 Golden-JSON-Fixtures; 6 neue Tests in `test/Tests.hs` (376 Tests gesamt, Byte-Identität belegt). | Golden-Tests + **Protokoll-Spezifikation in `docs/`** (Nachrichten, Event-Katalog, Versionierung) *(erledigt)* |

### Phase 2 — Kleine Engine-Features mit großer Wirkung *(Haskell)*

| Stufe | Inhalt | Abnahme |
|---|---|---|
| 2.1 | **Timer abfragbar:** Predicate `has_condition`, ValueRef `condition_turns: <name>`, Feld `hidden: true` (nicht in Status-Anzeige). **(Erledigt 2026-09-28, Commit `f1169f6`):** Predicate `HasCondition String`, ValueRef `VRConditionTurns String` mit Punktsyntax-Fallback, Condition-Feld `condHidden :: Bool`; `ApplyCondition` um `Bool` erweitert mit 4-Feld-Kompatibilitäts-Fallback; `hidden: true` aus `stats`-Ausgabe, `PlayerSnapshot.psConditions` und TUI-HUD gefiltert; Abwärtskompatibilität verifiziert (Default `False` beim Dekodieren, Weglassen beim Kodieren, Vorher-Stand-Save und -Welt ohne Checksummen-Warnung geladen); neue E2E-Fixture `bomb` (`examples/fixtures/bomb.yaml`, `ci/e2e/bomb.in`, `ci/e2e/bomb.expect`) in Stufe 4 registriert; 4 neue Unit-Tests in `test/Tests.hs` (380 Tests gesamt); Autoren-Doku in `docs/adventure-schema.md`; CI grün, 0 Warnungen. | Unit + E2E-Fixture (Bombe) *(erledigt)* |
| 2.2 | **Veto Stufe 1 (D3):** Event `OnBefore verb`, Effekt `Block (Maybe msg) consumesTurn`, Variablen `cmd.target`/`cmd.target_kind` vor Ausführung gebunden; Exit-Variante mit `when:`-Predicate + Fehltext. Semantik: alle passenden Before-Regeln laufen in Definitionsreihenfolge, erster `block` stoppt die Aktion, verbleibende Before-Regeln laufen nicht. **(Erledigt 2026-09-28, Commit `737fcf2`):** `OnBefore String` in `EventType`, `Block (Maybe String) Bool` in `Effect`, `Guarded String Predicate (Maybe String)` in `Exit`; `lastVeto :: Maybe Bool` in `GameState`; Vorab-Bindung von `cmd.target` und `cmd.target_kind` in `Parser.bindCommandVars` / `resolveCmdTarget`; `checkBeforeVeto` in `Parser.hs` und `GameLoop.hs` integriert (stoppt bei Veto vor Aktionsausführung, `consumesTurn = False` per Default ohne Zugverbrauch/Tick, `turn: true` verbraucht Zug und tickt); Definitionsreihenfolge und Abbruch beim ersten Block in `Effects.fireTriggerList` und `applyOutcomeWith` realisiert; Guarded-Exit-Unterstützung in `Parser`, `Protocol`, `Validate` und `Worldbuilder`; Schema-Erweiterung (`knownKeys EntExitRef` um `when`, `msg`, `message`; `block:` in `AActionOutcome`); zwei neue E2E-Fixtures `waechter` und `traglast` in CI-Stufe 4 registriert; Autoren-Doku in `docs/adventure-schema.md`; 5 neue Engine-Tests in `test/Tests.hs` (**385 Engine-Tests** gemessen, vorher 380) und 2 neue Compiler-Tests in `worldbuilder/test/Tests.hs` (**171 Tests** gemessen, vorher 169); Abwärtskompatibilität verifiziert (vorher kompiliertes Save/World byte-identisch geladen, keine Checksum-Warnung); CI grün, 0 Warnungen. | Unit + E2E (Wächter, Traglast) *(erledigt)* |
| 2.3 | **Disambiguation:** `Ambiguous`-Ergebnis aus 0.1 → Event `EvDisambiguate [..]`, Rückfrage „Which do you mean: [1] …, [2] …?“; Antwort per Zahl oder unterscheidendem Wort; kostet keinen Zug. **(Erledigt 2026-09-28, Commits `700ca2a` + `53a5940`):** Neues Event `EvDisambiguate [String]` in `src/Types/Output.hs` (textfrei — die CLI-Ausgabe bleibt byte-identisch) mit Protokoll-Typ `"disambiguate"` und Feld `candidates` (Event-Katalog in `docs/protocol-v1.md` auf 13 Events erweitert, Golden `test/golden/protocol/server_events.json` um den Event ergänzt); `interactAmbiguous` liefert das Event plus die numerierte Rückfrage über den neuen Katalog-Key `disambiguate.option` (`[{n}] {name}`, Katalog jetzt **183 Keys**). Die Antwort (1-basierte Zahl bzw. ein Wort, das genau einen Kandidaten beschreibt) wird über `LoopState.lsPendingDisambiguation`/`PendingDisambiguation` (Kandidaten-IDs + auslösendes Kommando) und `GameState.chosenTarget` genau auf die gewählte Entity-ID aufgelöst — die ID wird nicht erneut gegen Keywords aufgelöst, sonst bliebe die Wahl mehrdeutig. Die Antwort läuft über `runCommandNoTurn`: kein `turnCount`-Increment, kein Condition-Tick und kein `on: turn` (`fireCommandTriggersSkipping`); die mehrdeutige Eingabe selbst kostet wie bisher ihren Zug. Nicht als Antwort interpretierbare Eingaben laufen als normales Kommando und schließen die Rückfrage. 5 neue Engine-Tests (**390 gemessen**, vorher 385); neue Fixtures `disambiguation` (Stufe 4, Antwort per Zahl und per Wort, Marker `ZUGZAEHLER-4` belegt den fehlenden Zugverbrauch) und `disambiguation-fallback` (Stufe 5) auf Basis von `examples/fixtures/disambiguation.yaml` — **46 Loop-Einträge** in ci.sh-Stufe 4/5 gemessen (vorher 44) plus Worldgen; `scripts/ci.sh` grün, Byte-Identität aller **45** beiden Revisionen gemeinsamen E2E-Eingaben (`world.json`, `save.json`, stdout+stderr), Abwärtskompatibilität ohne Versionswarnung (`worldChecksum` stabil) und 0 Warnungen im kalten Build; Autoren-Hinweis in `docs/adventure-schema.md`. | Unit + E2E *(erledigt)* |
| 2.4 | **Text-Erweiterung:** Inline-Bedingung `{if <flag/var-cond>|a|b}` (begrenzte Syntax), Ausdrücke `{= gold * 2}` via vorhandenem `Expr`-Parser, Props `{item.torch.fuel}`. **(Erledigt 2026-09-28, Commits `f9c0f98` + `786cb76`):** Inline-Bedingungen `{if <cond>|a|b}` (Flags, Negation `!`, Vergleiche `==, !=, /=, >=, <=, >, <, =`, Text-Variablen, Default-Else `""`, rekursive Zweig-Interpolation); Ausdrücke `{= <expr>}` via vorhandenem `Expr`-Parser (`Types.Core.parseExpr`, Arithmetik, `min/max/clamp`, Modifikatoren `:+`/`:6`, Zero-Division-Schutz); Props `{item.<id>.<prop>}` und `{npc.<id>.<prop>}` aus `itemProps`/`npcProps` in Templates, Ausdrücken und `resolveValueRef (VRVariable)`; deterministische Fehlerbehandlung (`<error: ...>`) bei unbekannten Props/Items/NPCs/Vars, If-Syntaxfehlern und Expr-Parse-Fehlern; 4 neue Engine-Tests in `test/Tests.hs` (**394 Engine-Tests** gemessen, vorher 390); Doku in `docs/adventure-schema.md` und `CHANGELOG.md`; Abwärtskompatibilität verifiziert (vorher-kompilierte Welt und Save via `git worktree add` byte-identisch geladen, keine Checksummen-Warnung); CI grün, 0 Warnungen. | Unit-Tests *(erledigt)* |
| 2.5 | **Procedures (D2):** `procedures:`-Block, `call:`, Effekt `CallProc`, Parameter-Stack im `GameState` (nicht im Save persistiert, da nur während Ausführung), Compiler-Checks (unbekannte Procedure, Arity, **Rekursion statisch verboten**: Zyklen im Call-Graph sind Compile-Fehler; `maxOutcomeDepth` bleibt Laufzeit-Schutz), Doku in `adventure-schema.md`. Protokoll: Procedure-Aufrufe erscheinen nicht als eigene Events (nur deren Effekte). **(Erledigt 2026-09-29):** `AProcDef`/`ProcDef` + `CallProc` mit Scope (`GameState.procScopes`, runtime-only), `call:` (literal, `proc`/`args`-Form), Checks `UnknownProc`/`ProcArity`/`DuplicateProc`/`ProcParamReserved`/`ProcRecursion`; leere `procDefs`-Map wird im `world.json` weggelassen (Byte-Identität). 6 Engine-Tests (**400**), 5 Worldbuilder-Tests (**176**), E2E-Fixture `procedures` (verschachtelter Aufruf); Doku `adventure-schema.md`, CHANGELOG, README-Zahlen; CI-Stufen 1–8 grün. | Unit (Scope, verschachtelte Aufrufe, Arity-/Zyklus-Fehler) + E2E-Fixture *(erledigt)* |

**Entscheidung (2026-09-27):** Procedures sind aus Phase 4 als **2.5** vorgezogen —
reines Haskell (weniger Wechsel), Review-Tier 1 #1. Preis: WebUI v1 startet eine
M–L-Stufe später. Der Editor-Teil bleibt in 4.1.

### Phase 3 — WebUI v1 *(überwiegend Frontend, dünner Haskell-Server)*

Doku-Lieferung je Stufe: `docs/webui.md` (Aufbau, Start, Packaging) — zusätzlich zum
Protokoll-Dokument aus 1.4.

| Stufe | Inhalt | Abnahme |
|---|---|---|
| 3.0 | **Technikentscheidungen** (vor Beginn klären): Frontend-Stack, Server-Bibliothek (z.B. warp + websockets), Repo-Layout (`webui/`, `text-adventure-server/`), Packaging (Windows-Release!). | Entscheidungsnotiz |
| 3.1 | **Server:** Executable `text-adventure-server`; eine Session pro Verbindung auf Basis von 1.3/1.4; HTTP-Endpunkte `validate`/`compile` (Worldbuilder als Library). Bindet standardmäßig nur an `localhost`; öffentlicher Betrieb bräuchte Authentifizierung (eigene Entscheidung). | Protokoll-E2E: `ci/e2e/*.in` über den Server abspielen, gleiche Marker wie CLI |
| 3.2 | **Tab „Spielvorschau“:** Log, Eingabe mit Completion, HUD (HP, Quests, Minimap), Dialog-Choices klickbar, ASCII-Art inkl. Animation/Hotspots, Audio via Web Audio. | manuelle Abnahme mit `thefog` + 2 Genres |
| 3.3 | **Tab „Quelltext & Validierung“:** YAML-Editor, Fehlerliste mit Zeilensprung (heuristisches `Locate`). | Fehler aus Fixtures korrekt markiert |
| 3.4 | **Hot-Reload:** Speichern → compile → Vorschau neu starten (optional mit Replay der bisherigen Befehle). | — |

### Phase 4 — Große Features, jeweils Engine + Editor-Tab

Jede Stufe: **erst Haskell komplett**, dann UI am Stück.

| Stufe | Engine | Editor/WebUI |
|---|---|---|
| 4.1 | **Procedures:** Engine bereits in 2.5 umgesetzt; hier nur ggf. Nachschärfungen aus der Editor-Arbeit (z.B. Metadaten wie `desc:` für Parameter). | Procedure-Liste, Parameter-Signatur, „wird verwendet in…“ |
| 4.2 | **Veto Stufe 2 (D3):** `verb_map`-Shorthand `before:`/`instead:` pro Item/NPC; `take`-Semantik klären → **fixt B4**. Migration: bestehende `take`-Einträge (auch `take,<state>:`) laufen **weiterhin als `after`** — betrifft die siegentscheidenden Einträge in `fantasy`, `space-opera`, `cyberpunk`; Veto/Ersetzen nur über die neuen `before:`/`instead:`-Schlüssel. Alternativ die drei Dateien im selben Commit umstellen. | Regel-Ansicht pro Objekt; E2E-Siegpfade der drei Genres unverändert grün | Regel-Ansicht pro Objekt |
| 4.3 | **Sprachpakete (D4):** `language: de`, Katalog `de` + deutsche Verb-/Richtungs-Aliase, `messages:`-Overrides, `article:`/`gender:` an Items/NPCs. | Tab „Texte“: fehlende/überschriebene Keys |
| 4.4 | **Container** — **Erledigt (2026-09-30, Commit `94db85b`)**: tragbare Container (Items mit `capacity:`) + `containers:`-Sektion, gebaute Verben (`open`/`close`/`lock`/`unlock`, `take X from Y`, `put X in Y`), Scope offener Container (Verschachtelung beliebig tief), zählbare Kapazität + Inventarlimit (Welt + `set_inventory_limit:`). **Abweichung zum Plan:** kein neues `SaveState`-Feld — Zustand in `entityStates`, Inhalt in `itemStates`, Limit in der VarMap (Zero-New-Fields-Vertrag). Ursprünglich geplant: `ContainerState` in `SaveState` verdrahten; `open`/`close`/`lock`/`unlock`, `take X from Y`, `put X in Y`; Inhalt offener Container im Scope; Verschachtelung; Kapazität; einfaches Inventarlimit (Kern + Veto) — beides **zählbasiert** (Anzahl Items), kein Gewicht/Volumen. | Item-Platzierung im Map-Editor |
| 4.5 | **Konversation** — **Erledigt (2026-09-30, Commits `46348c3`/`fae364f` + Review `2772781`):** `ask/tell X about Y` mit Topic-Tabelle (`topics:`) pro NPC, Event `OnTalk` mit Wildcard-Matching (`OnTalk npc ""` feuert für jedes Topic des NPCs), Barks (`barks:` → `on: turn`-Trigger mit Cooldown, Compiler-Sugar), `on_talk:` am NPC (Compiler-Sugar für `on: talk`-Trigger), `say_node`/`dialogEnd` als Effekt-Sugars. Dialogbäume (`DialogueTree`/`DialogueNode`/`DialogueChoice`) waren bereits vollständig implementiert. | Dialog-Graph-Editor |
| 4.6 | **Map- & Quest-Editor:** grafische Räume/Exits, Quests/Stages. Vorher entscheiden: strukturelles YAML-Schreiben mit Kommentarerhalt vs. W5 Stufe 2 (exakte Positionen via Event-Parser). | Map- und Quest-Tab |
| 4.7 | **NPC-Besitz (B7)** — **Erledigt (2026-09-30, Commit `780a3fd`)**: Autorenform `carried_by: <npc-id>` am Item (Start im Besitz, `CarriedBy (ActorNPC …)`, **kein** neues `SaveState`-Feld), Befehle `take X from <npc>` + `give X to <npc>`/`gib X an <npc>` (L13-Urteil `GiveCmd` = Zug), Sichtbarkeit in `look at <npc>` + Protokoll-Snapshot `NpcSummary.carried` (leer = Feld entfällt), Validierung (`UnknownNpc`, `CarriedByConflict`), Effekt-Objektform `give: {item, to}`; `MoveEntity (CarriedBy <actor>)` respektiert jetzt den Ziel-Aktor. Zusätzlich gefixt: `capacity:` fehlte in `knownKeys EntItem` (4.4-Nachtrag). **Bewusst nicht dabei:** NPC-Ausrüstung (`EquippedBy ActorNPC`), automatisches Fallenlassen beim Tod, `take all from <npc>`. | Besitz-Anzeige im NPC-Tab |

### Phase 5 — Distribution, Synth, Erweiterbarkeit

| Stufe | Inhalt |
|---|---|
| 5.1 | **WASM-Build** (nur bei positivem Spike aus 1.0): Kern-Library von IO-Abhängigkeiten (`process`, `directory`, `haskeline`) trennen; GHC-WASM-Backend; gleiches Protokoll wie 1.4 → Web-Export eines Adventures als statische Seite. |
| 5.2 | **Synth-Tab:** zuerst Designphase (Sounds/Musik als Daten im Adventure? Rendern im Browser vs. Export als Datei für CLI/TUI?), dann Umsetzung über die vorhandene Audio-Abstraktion (`PlaySfx`/`PlayMusic`). |
| 5.3 | **Include & Pakete:** `include:` für YAML, Procedure-Bibliotheken und Sprachpakete als wiederverwendbare Pakete. |

### Phase 6 — Autorenwerkzeuge & Nachweise (B-Reihe)

| Stufe | Inhalt | Status |
|---|---|---|
| B1 | **Content-Tests als Daten** (`tests:`-Sektion + `worldbuilder test`, CI-Stufe 4b) | **Erledigt (2026-09-29)** |
| B6 | **Spiel-Export / Bündelung** — `worldbuilder export`: `world.json`/`save.json` byte-identisch zu `compile`, referenzierte Assets (`sfx:`/`music:` + `assets:`-Manifest, spielwurzel-relativ), `play.sh`/`play.bat`, `--with-engine` (Engine nach `bin/`), `--zip` (`zip`/`7z`); fehlende Assets = Warnung. CI-Stufe 9. | **Erledigt (2026-09-30)** |
| B4 | **Regel-Diagnostik** („welche Regel hat nie gefeuert", unerfüllbare Bedingungen, nie passierbare Ausgänge) | offen |
| B5 | **Content-Fuzzer** (Zufalls-/Heuristikläufe: Abstürze, Sackgassen, Endlosschleifen) | offen |

### Aufwand & Haltepunkte

Grobe Größen (Vorschlag, keine Schätzung aus der Review): Phase 0 **S–M** (0.6 = M),
Phase 1 **L** (1.0 = S, 1.1–1.4 je M–L), Phase 2 **M** je Stufe (2.5 = M–L), Phase 3 **L** (3.1/3.2 = L,
3.3/3.4 = M), Phase 4 **L** je Stufe, Phase 5 **XL**.

Haltepunkte, an denen neu entschieden wird statt weiterzubauen:

- vor **3.0**: Stack-/Packaging-Entscheidung (Windows-Release mitdenken)
- nach **1.0**: WASM-Spike-Ergebnis — fällt es negativ aus, entfällt 5.1 (Web-Export), nicht die WebUI; das Protokoll bleibt trotzdem transportunabhängig
- nach jeder Phase: `scripts/ci.sh` grün, CHANGELOG und README-Zahlen aktuell

### Nicht-Ziele

- Kein `Text`-statt-`String`-Umbau (dokumentierte Entscheidung `Types.hs:23-39`)
- Keine Turing-vollständige Skriptsprache — Procedures ersetzen Wiederholung, nicht Kontrollfluss (daher Rekursionsverbot in 2.5, keine Schleifen-Effekte)
- Keine Multi-Language-Voll-Lokalisierung in diesem Plan (D4 bleibt Architektur, Ausbau später)
- Keine Welt-Physik (Gewicht, Volumen, Licht-Propagation, Liquids) — siehe „Später / optional". Zählbasierte Container-Kapazität und Inventarlimits (4.4) sind davon ausgenommen.
- Kein Umbau der Kampf-, Karten- oder Worldgen-Systeme

**Nachtrag (2026-09-29, Nutzer-Entscheidungen zur Genre-Landkarte — ausführlich in
`/root/workspace/plan-zielgenres.md`):**

- ~~Kein Umbau der Kampf-, Karten- oder Worldgen-Systeme~~ — **Umbau ist erlaubt, solange die
  Grundsätze bleiben.** Maßstab ist die Prüfregel „genre-neutrale Primitive + Inhalt"
  („no engine code per genre"), nicht die Unberührtheit des Codes.
- ~~Keine Multi-Language-Voll-Lokalisierung~~ — **später möglich.** D4 hat die Architektur
  vorbereitet (stabile Message-Keys, `language:`, `article:`/`gender:`); der Ausbau bleibt unter
  „Später / optional", Variante C.
- **Welt-Physik präzisiert:** gemeint ist nur noch **Simulations-Physik** (Liquids, Fluss,
  emergente Modelle) — das bleibt Nicht-Ziel. **Gewicht/Volumen ist erlaubt** (halb
  implementiert; nützt Wirtschafts- und Schiffs-Simulation). **Licht-Propagation ist
  gestrichen** (zu teuer) — ersetzt durch den Hebel-/Halterungs-Mechanismus (W4): explizite
  Effekte statt Emergenz.
- **Nicht geändert:** kein Turing-vollständiges Skripting, kein `Text`-statt-`String`-Umbau,
  keine Echtzeit/kein Scheduler-Ticker.
- **Zur Kenntnis:** die W-Reihe (W1 Wissensmodell, W2 XP/Level, W3 Kapitel, W4 Vorrichtungen)
  ist entschieden und als `/root/workspace/plan-*.md` dokumentiert, aber **noch nicht in diesen
  Stufenplan eingeordnet**.

### Später / optional

- Welt-Physik: Gewicht/Volumen, Licht von abgelegten Lichtquellen, an/aus-Zustand, Nachbarraum-Wahrnehmung
- Konfigurierbare `EquipSlot`s und Equip-Boni auf Skills/Variablen
- NPC↔NPC-Dialoge
- Vollständige Multi-Language-Lokalisierung (D4, Variante C)
- Anpassbare Timer (pausieren/fortsetzen, Endlos-Daemons)

---

## 5. Offene Punkte zum jeweiligen Stufenbeginn

- 0.6: Modulschnitt des `Types.hs`-Splits (Zeitpunkt entschieden: direkt nach 0.5)
- 1.0: WASM-Spike — Ergebnis entscheidet über 5.1
- 1.2: Styling-Modell der `OutputEvent`s (Spans/Tags statt eingebetteter ANSI-Codes)
- 3.0: Frontend-Stack, Server-Bibliothek, Packaging
- 4.6: YAML-Schreibstrategie der Editoren (Kommentarerhalt) vs. W5 Stufe 2
- 5.2: Datenmodell und Render-Ort des Synths

