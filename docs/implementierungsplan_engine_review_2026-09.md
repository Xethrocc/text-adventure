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

### Phase 1 — Ausgabe- & Loop-Architektur *(Haskell, Fundament für WebUI und I18n)*

| Stufe | Inhalt | Abnahme |
|---|---|---|
| 1.0 | **WASM-Machbarkeits-Spike:** Kern-Module (`Types`, `Game`, `Parser`, `Effects`, … ohne `haskeline`/`process`/`directory`) mit dem GHC-WASM-Backend bauen, ein `applyLoopCommand` im Browser ausführen. Zeitlich begrenzt, kein Produktionscode. Liegt **vor 1.4**, damit Protokoll und Frontend nicht auf einer ungeprüften Annahme aufsetzen; Erkenntnisse (z.B. IO-Abhängigkeiten, Paketprobleme) fließen in 1.3/1.4 ein. | Spike-Notiz: machbar ja/nein, Blocker, Aufwand für 5.1 |
| 1.1 | **Message-Katalog:** zuerst **Inventar** aller hartkodierten Meldungen anlegen (Zahl belegen — grobe Zählungen ergeben 102–130 Literale inkl. Überschriften/Labels, der Plan nannte ~82), dann auf stabile Keys (`MsgId`) + Argumente umstellen; englischer Default-Katalog als Daten. Rendern via `formatStringWith`. | E2E byte-identisch; Inventar-Liste im Commit |
| 1.2 | **Strukturierte Ausgabe:** `data OutputEvent` (z.B. `EvMessage MsgId Args`, `EvText String`, `EvRoomChanged`, `EvQuestUpdate`, `EvDialogue [Choice]`, `EvCombat …`, `EvSfx`, `EvMusic`, `EvArt`, `EvGameOver`). `applyLoopCommand` liefert `[OutputEvent]`; CLI/TUI rendern daraus Text. **Zwingend mitentscheiden: Styling-Modell** — heute stecken ANSI-Codes in den Ausgabestrings (`ansiFilter` filtert nur am Rand nach `isTty`/`--no-color`); die Events brauchen Style-/Span-Information und strukturierte Art-/Hotspot-Payloads, sonst muss die WebUI ANSI parsen oder verliert Farbe. | E2E byte-identisch; TUI-Tests grün; Styling-Entscheidung dokumentiert |
| 1.3 | **Purer Session-Automat:** Save/Load/Restart/GameOver/Death/Victory aus `loopGame`/`deathLoop`/`victoryLoop` als pure Übergänge, die IO-Wünsche (`ReqSave slot`, `ReqLoad slot`, `ReqPersistMeta`, `ReqPause`) zurückgeben. `loopGame` wird dünner Interpreter. | E2E byte-identisch; neue Unit-Tests für Übergänge |
| 1.4 | **Protokoll v1:** JSON-Nachrichten `ClientMsg` (`command`, `choose`, `continue`, `load_world`, `save`/`load`) und `ServerMsg` (`events`, `snapshot` für HUD/Map/Quests). Versioniert, Golden-JSON-Tests. Kein Transport — nur Typen + Codec. | Golden-Tests + **Protokoll-Spezifikation in `docs/`** (Nachrichten, Event-Katalog, Versionierung) |

### Phase 2 — Kleine Engine-Features mit großer Wirkung *(Haskell)*

| Stufe | Inhalt | Abnahme |
|---|---|---|
| 2.1 | **Timer abfragbar:** Predicate `has_condition`, ValueRef `condition_turns: <name>`, Feld `hidden: true` (nicht in Status-Anzeige). | Unit + E2E-Fixture (Bombe) |
| 2.2 | **Veto Stufe 1 (D3):** Event `OnBefore verb`, Effekt `Block (Maybe msg) consumesTurn`, Variablen `cmd.target`/`cmd.target_kind` vor Ausführung gebunden; Exit-Variante mit `when:`-Predicate + Fehltext. Semantik: alle passenden Before-Regeln laufen in Definitionsreihenfolge, erster `block` stoppt die Aktion, verbleibende Before-Regeln laufen nicht. | Unit + E2E (Wächter, Traglast) |
| 2.3 | **Disambiguation:** `Ambiguous`-Ergebnis aus 0.1 → Event `EvDisambiguate [..]`, Rückfrage „Which do you mean: [1] …, [2] …?“; Antwort per Zahl oder unterscheidendem Wort; kostet keinen Zug. | Unit + E2E |
| 2.4 | **Text-Erweiterung:** Inline-Bedingung `{if <flag/var-cond>|a|b}` (begrenzte Syntax), Ausdrücke `{= gold * 2}` via vorhandenem `Expr`-Parser, Props `{item.torch.fuel}`. | Unit-Tests |
| 2.5 | **Procedures (D2):** `procedures:`-Block, `call:`, Effekt `CallProc`, Parameter-Stack im `GameState` (nicht im Save persistiert, da nur während Ausführung), Compiler-Checks (unbekannte Procedure, Arity, **Rekursion statisch verboten**: Zyklen im Call-Graph sind Compile-Fehler; `maxOutcomeDepth` bleibt Laufzeit-Schutz), Doku in `adventure-schema.md`. Protokoll: Procedure-Aufrufe erscheinen nicht als eigene Events (nur deren Effekte). | Unit (Scope, verschachtelte Aufrufe, Arity-/Zyklus-Fehler) + E2E-Fixture |

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
| 4.4 | **Container:** `ContainerState` in `SaveState` verdrahten; `open`/`close`/`lock`/`unlock`, `take X from Y`, `put X in Y`; Inhalt offener Container im Scope; Verschachtelung; Kapazität; einfaches Inventarlimit (Kern + Veto) — beides **zählbasiert** (Anzahl Items), kein Gewicht/Volumen. | Item-Platzierung im Map-Editor |
| 4.5 | **Konversation:** `ask/tell X about Y` mit Topic-Tabelle pro NPC, Event `OnTalk`, Barks (kontextuelle Einzeiler mit Cooldown). | Dialog-Graph-Editor |
| 4.6 | **Map- & Quest-Editor:** grafische Räume/Exits, Quests/Stages. Vorher entscheiden: strukturelles YAML-Schreiben mit Kommentarerhalt vs. W5 Stufe 2 (exakte Positionen via Event-Parser). | Map- und Quest-Tab |

### Phase 5 — Distribution, Synth, Erweiterbarkeit

| Stufe | Inhalt |
|---|---|
| 5.1 | **WASM-Build** (nur bei positivem Spike aus 1.0): Kern-Library von IO-Abhängigkeiten (`process`, `directory`, `haskeline`) trennen; GHC-WASM-Backend; gleiches Protokoll wie 1.4 → Web-Export eines Adventures als statische Seite. |
| 5.2 | **Synth-Tab:** zuerst Designphase (Sounds/Musik als Daten im Adventure? Rendern im Browser vs. Export als Datei für CLI/TUI?), dann Umsetzung über die vorhandene Audio-Abstraktion (`PlaySfx`/`PlayMusic`). |
| 5.3 | **Include & Pakete:** `include:` für YAML, Procedure-Bibliotheken und Sprachpakete als wiederverwendbare Pakete. |

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

