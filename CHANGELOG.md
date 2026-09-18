# Changelog

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
- Tests: **240** Engine- + **75** Worldbuilder-Tests, **28** E2E-Läufe
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
- Tests: **240** Engine- + **75** Worldbuilder-Tests, **28** E2E-Läufe.
  (Zahlen nach dem Code-Check korrigiert: hier standen 233 und 24 — gemessen sind
  es 240 registrierte Tests und 28 Läufe in `scripts/ci.sh`.)

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
