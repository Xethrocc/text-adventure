# Detailplan: Fahrzeug-Zustandsänderung (`apply_vehicle_condition` / `clear_vehicle_condition`)

| Kopf | Wert |
|---|---|
| **Status** | 🟢 Plan abgeschlossen — **nicht implementiert**, keine Produktivdateien geändert (nur diese Planungsdatei) |
| **Stand** | Erstellt aus Code-Inspektion (Zeilenangaben verifiziert) |
| **Scope** | Setter für `vsActiveConditions`, passende Prädikate, Worldbuilder-YAML-Sugar, Compiler-Refchecks, Tests, Docs |
| **Referenz** | `docs/befunde-text-adventure-engine.md` — **OPEN-24** (Fahrzeug-Zustand-Setter) |
| **Nicht im Scope** | siehe Non-Ziele (Abschnitt 1) |

---

## 1. Kurzfassung, Ziel & Non-Ziele

### Kurzfassung
Fahrzeug-`conditions:` (`vehicleConditionEffects`, `src/Types/Vehicles.hs:76`) sind heute **clear-only**: `vsActiveConditions` (`src/Types/Vehicles.hs:114`) wird ausschließlich via `clearVehicleCondition` (`src/Vehicles.hs:200-203`, aufgerufen vom `repair`-Kommando `src/Parser.hs:1984-2002`) geleert, per JSON-Deserialisierung befüllt (`src/Types/Vehicles.hs:123`) oder hart auf `Set.empty` initialisiert (`worldbuilder/src/Worldbuilder/Compile.hs:4121`). Es existiert **kein `Set.insert`** — ein definierter Fahrzeug-Zustand kann nie aktiv werden. Zusätzlich verschluckt der `set_state`-Catch-all `applySetValueWithDepth _ _ _ state = (state, [])` (`src/Effects.hs:848`) Fahrzeug-Ziele still.

### Ziele
- Zwei neue Effekt-Konstruktoren im Kern (`ApplyVehicleCondition` / `ClearVehicleCondition`) inkl. JSON/YAML-Form, die `vsActiveConditions` setzen/löschen.
- Ein neues Prädikat `VehicleHasCondition` für Bedingungen/Sichtbarkeit (Dialog-Choices).
- Worldbuilder-YAML-Sugar (`apply_vehicle_condition: {vehicle, condition}` / `clear_vehicle_condition: …`) im Shape von `damage_npc` (`{npc, amount}` — `worldbuilder/src/Worldbuilder/Types.hs:2442-2443`).
- Compiler-Refchecks analog `checkDamageNpcRefs` (`worldbuilder/src/Worldbuilder/Compile.hs:3716-3723`) inkl. Fehlercodes.
- Ersatz des `setVehicleState`-Test-Hacks (`test/Tests.hs:3005-3007`) durch die echte API; Byte-Vertrag-Wächter (Save-Roundtrip) gesichert.
- Vollständige Dokumentation (`docs/adventure-schema.md`, `CHANGELOG.md`, `lang/de.json`, OPEN-24-Update).

### Non-Ziele (abgegrenzte offene Punkte)
- **`vsRoomOverrides`** (`src/Types/Vehicles.hs:115`): wird ebenfalls nie befüllt (nur `Set.empty`/`Map.empty`-Initialisierung `Compile.hs:4121-4122`). Eigener Folge-Ticket-Punkt, **nicht Teil dieses Plans** — siehe Abschnitt 8 (Ausblick).
- **Tick-Globalität**: `vehicleConditionTickWith` tickt nur das **aktuelle** Fahrzeug (`src/Vehicles.hs:207-226` via `currentVehicle (save state)`; Aufrufer `src/GameLoop.hs:280,307`). Globales Ticken aller Fahrzeuge ist bewusst **nicht** Teil dieses Plans (offene Entscheidung 5).
- **`set_state`-Support für `ActorShip PState`**: wird nicht nachgerüstet; wir schaffen dedizierte Effekte (der Catch-all `Effects.hs:848` bleibt, bekommt aber ggf. eine Diagnose — siehe Risiken).
- **Keine neuen Savegame-Felder** (Regel 6, Byte-Vertrag — Abschnitt 5).

---

## 2. Designentscheidungen

| # | Entscheidung | Wahl | Begründung |
|---|---|---|---|
| D1 | Effekt-Konstruktoren (Core) | `ApplyVehicleCondition VehicleID String` und `ClearVehicleCondition VehicleID String` in `data Effect` neben `ApplyCondition`/`ClearCondition` (`src/Types/Core.hs:1171-1172`) | Spiegelt die bestehende Condition-API; getrennte Namespace-Semantik (pro Fahrzeug, nicht pro Spieler); kein Turn-Timer nötig (`vsActiveConditions` ist eine `Set String`, kein Countdown). |
| D2 | Prädikat (Core) | `VehicleHasCondition String String` (Vehicle, Condition) in `data Predicate` neben `HasCondition` (`src/Types/Core.hs:949`) | Symmetrie zum Spieler-Prädikat; auswertbar in `evalPredicate` (`src/Game.hs:1796`) und in `isChoiceVisible` (`src/Types/Protocol.hs:877-885`). |
| D3 | JSON-Keys (Kern, gespeichert) | Explizite Keys `apply_vehicle_condition: {vehicle, condition}` und `clear_vehicle_condition: {vehicle, condition}` über **eigene** `ToJSON`/`ToEncoding`-Klauseln (vor dem generischen Fallback `src/Types/Core.hs:1514-1547`) sowie passende `FromJSON`-Klauseln (`:1549-1550`) — Entscheidung 8B | Wie `ApplyCondition`/`ClearCondition` (Legacy-Parser `:1579-1588` behandelt nur alte Welten). Bestehende Serialisierung bestehender Konstruktoren bleibt unangetastet → Byte-Vertrag. |
| D4 | JSON/YAML-Keys (Autoren-Form, Worldbuilder) | `apply_vehicle_condition: {vehicle: <id>, condition: <name>}` und `clear_vehicle_condition: {vehicle: <id>, condition: <name>}` | Konsistent zu `apply_condition`/`clear_condition`; Objektshape analog `damage_npc: {npc, amount}` (`worldbuilder/src/Worldbuilder/Types.hs:2442-2443`). |
| D5 | Fehlercodes (Worldbuilder) | `UnknownVehicleConditionTarget` (Fahrzeug unbekannt → **Error**) und `UnknownVehicleCondition` (Condition nicht in `vehicleConditionEffects` → **Warning**) via `ciError`/`ciWarning` (`Compile.hs:93-97`) | Präzedenz `UnknownDamageNPC` (`Compile.hs:3721`, hart); Condition-Namen dürfen bewusst „kosmetisch" sein (nur `vehicle.warning`-Anzeige + `repair`), daher Warning statt Error. Variante B/C siehe offene Entscheidung 3. |
| D6 | Refcheck-Struktur | Neue `checkVehicleConditionRefs :: E.GameWorld -> [CompileIssue]`, aufgerufen neben `npcRefErrs = checkDamageNpcRefs gwResolved` (`Compile.hs:840`) | Bewährtes Muster (`:3713-3723`); sammelt Referenzen aus `allWorldEffects gw`. |
| D7 | Namenskonvention | **`apply_vehicle_condition` / `clear_vehicle_condition`** (nicht `set_vehicle_condition`) | Konsistenz zu `apply_condition`/`clear_condition`: „apply" = Zustand aktivieren (Set-Insert), „clear" = deaktivieren. `set_*` wird im Codebase für deterministische Zustandssetzung von Props verwendet (`set_state` → `AOSetEntityState`); ein `set_vehicle_condition: active: true|false`-Syntax wäre ein unnötiges drittes Konzept. `set_vehicle_condition` bleibt als Alias **nicht** vorgesehen (YAML-Parser: strikt, unbekannte Keys = Warnung). |
| D8 | Onset-Verhalten | Anwenden **feuert kein** `vehicleConditionEffects`-Outcome sofort; Wirkung ab nächstem Tick (wie Spieler-Conditions, `tickConditions`) | Vorhersehbarkeit; Onset-Sonderlogik wäre ein weiteres offenes Feature (Entscheidung 7). |
| D9 | Dynamic-Target-Token | Prädikat & Effekte akzeptieren das reservierte Token `current` als Vehicle-Auflösung (= `currentVehicle (save state)`, sonst Fahrzeug des Spieler-Raums via `vehicleForRoom`, `src/Vehicles.hs:47-49`) | Analog `damage_npc`-Dynamic-Tokens (`Compile.hs:3719`); Compiler whitelisted `current` und überspringt den Existenz-Check. |

---

## 3. Phasenplan

> **Reihenfolge:** Core-Typen → Engine-Runtime → Game/Protocol-Prädikate → Worldbuilder → Validate → Tests → Docs/lang.
> Aufwände sind Personen-Stunden (Entwicklung + Testlauf, ohne Review).

### Phase 0 — Vorbereitung (Baseline)
**Ziel:** Grüne Baseline, Test-Matrix fixieren.
**Betroffene Dateien:** keine Produktivänderung; `test/Tests.hs`, `worldbuilder/test/Tests.hs` nur referenzieren.
**Änderungen:** `cabal test` (beide Test-Suites) einmalig protokollieren; Test-IDs aus Abschnitt 4 im Ticket verankern.
**Tests, die Phase abschließen:** Baseline 100 % grün.
**Weiterkriterium:** Baseline grün, Test-IDs bestätigt.
**Abbruchkriterium:** eine schon vorher rote Suite → Plan pausieren, Baseline zuerst reparieren.
**Aufwand:** 0,5 h

### Phase 1 — Core-Typen (Serialisierung inkl.)
**Ziel:** Neue Konstruktoren existieren und serialisieren stabil; der Compiler zeigt **alle** Dispatch-Lücken an.
**Betroffene Dateien:**
- `src/Types/Core.hs`: `data Effect` (~`:1171`) erweitern um `ApplyVehicleCondition String String` / `ClearVehicleCondition String String`; `data Predicate` (~`:949`) erweitern um `VehicleHasCondition String String`.
- `src/Types/Core.hs`: Predicate `toJSON` (`:979` — Clause `object ["vehicle_has_condition" .= object ["vehicle" .= …, "condition" .= …]]`) und `FromJSON` (`:1033` — `<|>`-Clause mit Objektform `vehicle_has_condition: {vehicle, condition}`).
- `src/Types/Core.hs`: Effect `toJSON`/`toEncoding` (`:1514-1547`) — **neue Klauseln** für `ApplyVehicleCondition`/`ClearVehicleCondition` mit Objektform `{vehicle, condition}` (vor dem generischen Fallback `:1547`); bestehende Konstruktoren unverändert. `FromJSON` (`:1549-1550`) — passende Klauseln für die neuen Keys ergänzen; prüfen, dass `parseLegacyEffect` (`:1579+`) die neuen Keys nicht falsch schluckt.
**Änderungen:** Konstruktoren + Predicate-JSON; keine Dispatches (gewollt!).
**Tests:** JSON-Roundtrip für beide Effekte + Prädikat (neu in `test/Tests.hs`, Abschnitt 4: VC-01/VC-02).
**Weiterkriterium:** `Types/Core.hs` kompiliert; Roundtrip-Tests grün; `cabal build` scheitert **nur** mit `-Werror=incomplete-patterns`-Fehlern in genau diesen Dateien: `src/Effects.hs`, `src/Game.hs`, `src/Types/Protocol.hs`, `worldbuilder/src/Worldbuilder/Compile.hs`, `src/Validate.hs` (Wildcard-Walker).
**Abbruchkriterium:** irgendeine *andere* Datei taucht in der Fehlerliste auf → vor Phase 2 analysieren (unbekannter Walker); Roundtrip-Tests schlagen fehl → Serialisierungs-Design (D3) prüfen.
**Aufwand:** 2 h

### Phase 2 — Engine-Runtime (Setzen/Löschen + Dispatch)
**Ziel:** `vsActiveConditions` kann zur Laufzeit gesetzt/gelöscht werden; Effekte feuern.
**Betroffene Dateien:**
- `src/Vehicles.hs`: neue Funktion `applyVehicleCondition :: VehicleID -> String -> GameState -> (GameState, [OutputEvent])` (Set-Insert + Nachricht `vehicle.condition_set`) und Erweiterung von `clearVehicleCondition` (`:200-203`) um eine `… -> (GameState, [OutputEvent])`-Variante mit Nachricht `vehicle.condition_cleared` (Altfunktion als `fst`-Wrapper erhalten, da `src/Parser.hs:2000` sie nutzt); Exportliste `:3-21` anpassen.
- `src/Effects.hs`: Dispatch in `applyOutcome` neben `:474-475` (`ApplyCondition`/`ClearCondition`): `ApplyVehicleCondition v c -> …`, `ClearVehicleCondition v c -> …` mit `resolveVarName`-Auflösung für `v` (Dynamic-Token `current` → `currentVehicle (save state)`, wie D9).
- `src/Messages.hs`: neue Default-Keys neben `vehicle.warning` (`:392`): `vehicle.condition_set` (`"{vehicle}: problem detected — {condition}."`), `vehicle.condition_cleared`, `vehicle.condition_unknown` (nur falls Runtime-Warnung gewünscht).
**Tests:** VC-03 (Insert), VC-04 (Clear via Effekt), VC-05 (Idempotenz/`Set`-Semantik, doppeltes Anwenden = 1 Eintrag); optional VC-09 (Diagnose im `set_state`-Catch-all `Effects.hs:848`, nur falls die optionale Mitigation aus Abschnitt 6 mitgenommen wird).
**Weiterkriterium:** `cabal build` grün; VC-03..05 grün; `repair <condition>` (Parser.hs:1984-2002) löscht einen per Effekt gesetzten Zustand (VC-10, Integration).
**Abbruchkriterium:** `repair`-Regression (VC-10 rot) → Phase stoppen (Clear-Pfad defekt); Nachrichten-Keys fehlen in `Messages.hs` → Phase gilt nicht als abgeschlossen (Default-Katalog ist Referenz).
**Aufwand:** 4 h

### Phase 3 — Game/Protocol-Prädikate
**Ziel:** `VehicleHasCondition` wird ausgewertet — in Regeln und in Dialog-Choice-Sichtbarkeit.
**Betroffene Dateien:**
- `src/Game.hs`: `evalPredicate` (`:1796` — neben `HasCondition cn -> hasCondition cn st`) Fall `VehicleHasCondition v c -> c \`Set.member\` vsActiveConditions (getVehicleState (resolveVehicle v st) st)` mit D9-Auflösung.
- `src/Types/Protocol.hs`: `isChoiceVisible` (`:877-885`, Fall `HasCondition` `:885`) Fall `VehicleHasCondition v c` — Zugriff direkt auf `vehicleStates ss` (der `ss`-Wert ist der SaveState; **keine** Snapshot-Strukturänderung nötig, siehe Abschnitt 5).
**Tests:** VC-06 (Prädikat wahr/falsch), VC-07 (Choice-Sichtbarkeit via `vehicle_has_condition`), VC-08 (Token `current`).
**Weiterkriterium:** `cabal build` grün; VC-06..08 grün; bestehende `HasCondition`-Tests unverändert grün.
**Abbruchkriterium:** `isChoiceVisible` müsste Snapshot-Felder ergänzen → Phase stoppen, offene Entscheidung 4/Byte-Vertrag (Abschnitt 5) klären.
**Aufwand:** 3 h

### Phase 4 — Worldbuilder (Types / Compile / Walker)
**Ziel:** YAML-Sugar, Compilierung zu Core-Effekten, keine still verschluckten Referenzen.
**Betroffene Dateien:**
- `worldbuilder/src/Worldbuilder/Types.hs`: `data AActionOutcome` (`:2067+`) um `AOApplyVehicleCondition String String` / `AOClearVehicleCondition String String` erweitern (neben `AOApplyCondition` `:2097` / `AOClearCondition` `:2099`); `FromJSON`-Parser in der `:2222-2226`-Region einordnen (vor der breiten `msg`-Fallback-Chain, Kommentar `:2218-2220` beachten) — Shape `{vehicle, condition}` analog `damage_npc` (`:2442-2443`).
- `worldbuilder/src/Worldbuilder/Compile.hs`:
  - `compileAActionOutcome` (ab `:4364`): Mapping `AOApplyVehicleCondition v c -> E.ApplyVehicleCondition v c` (bzw. mit Token-Erhalt), `AOClearVehicleCondition v c -> E.ClearVehicleCondition v c`.
  - **Walker mit Wildcard-Catch-all — explizit ergänzen:** `conditionNamesInEffect` (`:3544-3551`), `refsInEffect` (`:3679-3690`, Catch-all `_ -> []` bei `:3690`), `refsInPredicate` (`:3693+`, nur falls Prädikat-Variante mit Vehicle-Ref), `resolveNpcStateEffect`/`mapEff`-Alias (`:4325`, greift auch `vehicleConditionEffects` `:4342-4344`), sowie `hpTargetsInEffect`-Analog (`:3727+`) als Vorbild für den neuen Sammler `vehicleTargetsInEffect`.
  - Neue `checkVehicleConditionRefs` (`:3716-3723` als Vorlage), Aufruf neben `npcRefErrs` (`:840`); Dynamic-Token-Whitelist analog `:3719` (`["current"]`).
  - Condition-Sammlung: pro `vehicleConditionEffects`-Map die deklarierten Namen kennen (für `UnknownVehicleCondition`-Warning).
**Tests:** VC-11 (YAML→Core-Mapping), VC-12 (`UnknownVehicleConditionTarget`-Error), VC-13 (`UnknownVehicleCondition`-Warning), VC-14 (Token `current` kompiliert — Muster `worldbuilder/test/Tests.hs:2552-2560`), VC-15 (Walker: eingebettete Effekte werden durch `mapEff`/`refsInEffect` gesehen — heute trivial, Test fixiert das gegen künftige Nested-Effects).
**Weiterkriterium:** `worldbuilder`-Suite grün inkl. neuer Fehlercode-Tests; `compileAdventure` eines Mindestabenteuers mit beiden Outcomes → korrekte `E.Effect`-Werte (`Right`).
**Abbruchkriterium:** Fehlercode-Test VC-12 nicht hart (`Left errs` mit `UnknownVehicleConditionTarget`) → Severity-Entscheidung (D5) prüfen; ein Walker mit `_ ->`-Catch-all nicht erwischt (Grep-Abgleich Catch-all vs. ADT) → Phase nicht abschließen.
**Aufwand:** 6 h

### Phase 5 — Validate (Refchecks)
**Ziel:** `src/Validate.hs` versteht die neuen Effekte (keine stillen Lücken bei Ref-Sammlung).
**Betroffene Dateien:**
- `src/Validate.hs`: `nestedTargets` (`:153`), `effectRefs` (`:525`), `idsFromOutcomeVehicle` (`:549`) — Fälle für `ApplyVehicleCondition`/`ClearVehicleCondition` (Vehicle-ID als Ref aufnehmen); `vehicleConditionEffects`-Einsammlung `:361` bleibt unverändert (greift automatisch die neuen Effekte in den Outcome-Bäumen).
**Tests:** VC-16 (Validate meldet unbekannte Vehicle-Ref in Effekt, analog bestehender Refcheck-Tests).
**Weiterkriterium:** Validate-Tests grün; `cabal build` insgesamt grün (die Phase-1-Breakage ist jetzt endgültig aufgelöst).
**Abbruchkriterium:** VC-16 bleibt ohne neue Fälle grün (d. h. Lücke unsichtbar) → Test explizit auf `notElem`-Verhalten schärfen, sonst Phase stoppen.
**Aufwand:** 2 h

### Phase 6 — Tests (Vollpaket + Byte-Vertrag-Wächter)
**Ziel:** Test-Matrix aus Abschnitt 4 vollständig; Test-Hack ersetzt.
**Betroffene Dateien:**
- `test/Tests.hs`: `testVehicleConditionTick` (`:3001-3014`) — `setVehicleState`-Direktmanipulation (`:3005-3007`) ersetzen durch Ausführen des `ApplyVehicleCondition`-Effekts (Runner über `applyOutcome`/`runOutcome`); `testVehicleStateRoundTrip` (`:3016-3023`) um Save-Roundtrip mit gesetzter Condition **und** Effekt-Bäumen in `vehicleConditionEffects` erweitern (VC-17/VC-18).
- `worldbuilder/test/Tests.hs`: Fehlercode-Tests nach Muster `:2552-2560` (`testSetStateUnknownTargetFails`).
**Weiterkriterium:** beide Suites 100 % grün, inkl. Byte-Vertrag-Wächter (VC-17/VC-18) und Test-Hack-Ersatz VC-19 (gleiche Assertion HP 95 wie der alte `setVehicleState`-Aufbau).
**Abbruchkriterium:** Roundtrip-Ungleichheit (VC-18) → Serialisierung sofort stoppen, Abschnitt 5 prüfen; Ersatz des Hacks verändert Testsemantik (anderer Zustand als vorher) → Test neu begründen, nicht blind umbiegen.
**Aufwand:** 4 h

### Phase 7 — Docs & lang
**Ziel:** Autoren-Doku, Sprachkataloge, Changelog, Ticket-Update.
**Betroffene Dateien:**
- `docs/adventure-schema.md`: neuer Abschnitt „Fahrzeug-Zustände" (Effekte + Prädikat + YAML-Shape + Dynamic-Token `current`); Kompatibilitätsregel `:1314-1317` (Regel 6) zitieren/angleichen.
- `lang/de.json` (+ `lang/README.md`): Übersetzungen der neuen Message-Keys (`vehicle.condition_set`, `vehicle.condition_cleared`) — Default-Englisch aus `src/Messages.hs:390-392` ist Quelle.
- `CHANGELOG.md`: Feature-Eintrag (Vorbild `:3897-3901` Fahrzeug-Conditions-Eintrag).
- `docs/befunde-text-adventure-engine.md`: OPEN-24 (`:218`) auf ✅ setzen mit Kurzbericht und Verweis auf dieses Dokument.
**Tests:** VC-20 (Doc-Spot-Check: Schema-Beispiel compilt per `compileAdventure`); Lang-Pack-Laden ohne Unknown-Key-Warnung.
**Weiterkriterium:** Doku vollständig, `lang/de.json` valides JSON, OPEN-24 abgeschlossen.
**Abbruchkriterium:** Schema-Beispiel kompiliert nicht → Doku korrigieren (Abbruch nur der Doku-Phase, nicht der Features).
**Aufwand:** 2 h

**Gesamtaufwand:** ca. 23,5 h (~3 Personentage).

---

## 4. Testplan (Volltabelle)

| ID | Test | Datei | Phase | Priorität |
|---|---|---|---|---|
| VC-01 | Effect-JSON-Roundtrip `ApplyVehicleCondition`/`ClearVehicleCondition` (explizite Keys `apply_/clear_vehicle_condition`) | `test/Tests.hs` | 1 | P0 |
| VC-02 | Predicate-JSON-Roundtrip `VehicleHasCondition` (`vehicle_has_condition`) | `test/Tests.hs` | 1 | P0 |
| VC-03 | `ApplyVehicleCondition` fügt Bedingung in `vsActiveConditions` ein | `test/Tests.hs` | 2 | P0 |
| VC-04 | `ClearVehicleCondition`-Effekt entfernt Bedingung | `test/Tests.hs` | 2 | P0 |
| VC-05 | Doppeltes Anwenden = idempotent (Set-Semantik) | `test/Tests.hs` | 2 | P1 |
| VC-06 | `evalPredicate (VehicleHasCondition …)` wahr/falsch | `test/Tests.hs` | 3 | P0 |
| VC-07 | `isChoiceVisible` blendet Choice via `vehicle_has_condition` ein/aus | `test/Tests.hs` | 3 | P1 |
| VC-08 | Token `current` löst zum aktuellen Fahrzeug auf (Effekt + Prädikat) | `test/Tests.hs` | 3 | P1 |
| VC-09 | *(optional)* `set_state` auf Vehicle erzeugt Diagnose statt Still-Still (Catch-all `Effects.hs:848`) | `test/Tests.hs` | 2 | P2 |
| VC-10 | `repair <condition>` löscht per Effekt gesetzte Bedingung (Integration) | `test/Tests.hs` | 2 | P0 |
| VC-11 | YAML `apply_vehicle_condition`/`clear_vehicle_condition` → Core-Konstruktoren | `worldbuilder/test/Tests.hs` | 4 | P0 |
| VC-12 | Unbekanntes Fahrzeug → `UnknownVehicleConditionTarget` (Error, `Left`) | `worldbuilder/test/Tests.hs` | 4 | P0 |
| VC-13 | Unbekannte Condition → `UnknownVehicleCondition` (Warning, `Right`) | `worldbuilder/test/Tests.hs` | 4 | P1 |
| VC-14 | Dynamic-Token `current` kompiliert ohne Refcheck-Fehler | `worldbuilder/test/Tests.hs` | 4 | P1 |
| VC-15 | Walker (`refsInEffect`/`mapEff`) verarbeiten neue Konstruktoren (kein Still-Skip) | `worldbuilder/test/Tests.hs` | 4 | P1 |
| VC-16 | `src/Validate.hs` meldet unbekannte Vehicle-Ref im neuen Effekt | `test/Tests.hs` | 5 | P1 |
| VC-17 | **Byte-Vertrag:** `SaveState`-Roundtrip mit gefülltem `vsActiveConditions` (bestehenden Test `:3016-3023` erweitern) | `test/Tests.hs` | 6 | P0 |
| VC-18 | **Byte-Vertrag:** Welt mit neuen Effekten → Encode/Decode-Roundtrip; Welt **ohne** neue Features byte-identisch zu Phase-0-Fixture | `test/Tests.hs` | 6 | P0 |
| VC-19 | **Ersatz Test-Hack:** `testVehicleConditionTick` (`:3001-3014`) läuft über echten Effekt statt `setVehicleState` (`:3005-3007`) — gleiche Assertion (HP 95) | `test/Tests.hs` | 6 | P0 |
| VC-20 | Schema-Beispiel aus `docs/adventure-schema.md` compilt (Doc-Test) | `worldbuilder/test/Tests.hs` | 7 | P2 |

---

## 5. Byte-Vertrag / Serialisierung

| Instanz / Daten | Änderung? | Detail |
|---|---|---|
| `VehicleState` (`src/Types/Vehicles.hs:110-125`) | **nein** | `vsActiveConditions` existiert bereits inkl. JSON (`:123`) — **keine neuen Save-Felder** (Regel 6, `docs/adventure-schema.md:1314-1317`). |
| `SaveState` | **nein** | `vehicleStates :: Map VehicleID VehicleState` existiert. |
| `ToJSON Effect` (`src/Types/Core.hs:1514-1547`) | **ja** | Neue Klauseln für `apply_vehicle_condition`/`clear_vehicle_condition` (Objektform `{vehicle, condition}`, Entscheidung 8B); Serialisierung **bestehender** Konstruktoren unverändert → alte `world.json` byte-identisch. |
| `FromJSON Effect` (`:1549-1588`) | nein (prüfen) | `genericParseJSON` erbt die neuen Tags; `parseLegacyEffect` (`:1579+`) darf sie nicht verschlucken (VC-01). |
| `ToJSON/FromJSON Predicate` (`:979`, `:1033`) | **ja** | Neue Clause `vehicle_has_condition` (Objektform `{vehicle, condition}`). Alte Prädikate unverändert. |
| `VehicleDef` / `vehicleConditionEffects` (`src/Types/Vehicles.hs:76, 92, 108`) | nein | Map `String Effect` serialisiert die neuen Effekte als Values — Welt-JSON wächst nur dort, wo Autoren die Features nutzen. |
| WASM/JSON-Protokoll (`src/Types/Protocol.hs`) | **nein** (im empfohlenen Design) | `isChoiceVisible` (`:877-885`) wertet `VehicleHasCondition` direkt gegen `vehicleStates ss` aus; `Snapshot`-JSON (`:570+`) bleibt unverändert. Nur bei Entscheidung 4-Variante C (Snapshot-Exposure) gäbe es eine Protokolländerung → dann `currentProtocolVersion` (`:635`) hochzählen. |

**Kompatibilitätsregel:** Regel 6 / Zero-New-Fields-Vertrag (`docs/adventure-schema.md:1314-1317`): bestehende Abenteuer und Savegames bleiben zu 100 % byte-identisch; Felder werden nur weggelassen, wenn leer (Vorbild `score_rankings`). Savegames ohne `vsActiveConditions` laden weiterhin mit `Set.empty`-Default (`src/Types/Vehicles.hs:123`).

---

## 6. Risiken & Mitigationen

| Risiko | Impact | Mitigation | Verifikation |
|---|---|---|---|
| **Wildcard-Walker verschlucken neue Konstruktoren** (`refsInEffect` `Compile.hs:3690` `_ -> []`; `conditionNamesInEffect` `:3544`; `refsInPredicate` `:3693`; `mapEff = resolveNpcStateEffect` `:4325`) | hoch — Refchecks finden Fehler nicht mehr | Alle Walker explizit um Fälle ergänzen (Phase 4); Grep-Abgleich Catch-all vs. ADT vor Phasenende; `mapEff` muss `vehicleConditionEffects` (`:4342-4344`) weiterhin mappen | VC-15; manueller Catch-all-Abgleich im Weiterkriterium Phase 4 |
| **Stiller Catch-all `applySetValueWithDepth _ _ _ state = (state, [])` (`src/Effects.hs:848`)** | mittel — Autoren versuchen `set_state` auf Fahrzeuge, nichts passiert, keine Meldung | Feature bewusst über dedizierte Effekte lösen (D1); optional `addDiagnostic "[engine] set_state: unsupported target"` im Catch-all nachrüsten (Kleinständerung, mit Test) | VC-09 (optional); Docs warnen explizit |
| **Tick nur aktuelles Fahrzeug** (`src/Vehicles.hs:207-226`, `GameLoop.hs:280,307`) | mittel — gesetzte Zustände „schlafen" abseits des aktuellen Fahrzeugs | Im Scope bewusst nicht gelöst (Non-Ziel); in `docs/adventure-schema.md` dokumentieren; ggf. Hinweisnachricht bei `apply_vehicle_condition` auf nicht-aktualem Fahrzeug | Doku-Check; VC-05 zeigt Zustand überlebt Fahrzeugwechsel (bleibt gesetzt) |
| Bestehende Effekt-Serialisierung ändert sich durch die neuen Klauseln (Fallback-Reihenfolge, Aeson-Version/Options) | hoch — Byte-Vertrag | Neue Klauseln nur für `ApplyVehicleCondition`/`ClearVehicleCondition`; Fallback `genericToJSON` unverändert; keine `defaultOptions`-Änderungen; Roundtrip- + Byte-Identitäts-Wächter (VC-18) | VC-01, VC-18 |
| Namenskonflikt: Fahrzeug- vs. Spieler-Condition-Namespaces | gering | Namespaces getrennt (`vsActiveConditions` vs. `conditions`); `VehicleHasCondition` liest nur Fahrzeug-Set; Docs benennen den Unterschied | VC-06/VC-07 |
| `conditionNamesInEffect` vermischt Namespaces (Cooldown-Warnung `:3538-3539` nach `cooldown_`-Prefix) | gering — Fehlalarme | Vehicle-Condition-Namen **getrennt** sammeln (neue `vehicleConditionNamesInEffect`), Cooldown-Filter unberührt lassen | VC-13/VC-15 |
| `repair`-Ambiguität (`src/Parser.hs:1984-2002`, `head matches` `:1999`) | gering | unverändert; neuer Setter ändert nur die Quelle der gesetzten Namen | VC-10 |
| Test-Hack-Ersatz ändert Testzustand (Setter statt Record-Update) | gering | Assertion (HP 95) explizit beibehalten; Setup-Vergleich alt/neu im PR | VC-19 |

---

## 7. Offene Entscheidungen (Multiple-Choice; **Empfehlung immer erste Option**)

**Entschieden (Stand 14.10.2026):** 1A, 2A, 3A, 4A, 5A, 6A, 7A, 8B.
Konsequenzen: Serialisierung der neuen Effekte mit expliziten Keys (`apply_vehicle_condition: {vehicle, condition}`, `clear_vehicle_condition: {vehicle, condition}`) über eigene `ToJSON`/`ToEncoding`-Klauseln; höherer Test- und Codeaufwand, dafür lesbare Autorensyntax. Die Optionen unten bleiben zur Nachvollziehbarkeit stehen.

**1. Namenskonvention der YAML-Keys**
- **A. `apply_vehicle_condition` / `clear_vehicle_condition` (empfohlen)** — Pro: konsistent zu `apply_condition`/`clear_condition`, keine dritte Schreibweise, Konstruktoren-Namen spiegeln sich 1:1. Contra: länger als `set_vehicle_condition`.
- B. `set_vehicle_condition: {vehicle, condition, active: true|false}` — Pro: Parallel zu `set_state`, ein Key. Contra: bricht die `apply/clear`-Sprachlichkeit, Bool-Flag-Falle.
- C. `vehicle_condition: {set: …}` (einzelner Objektkey) — Pro: erweiterbar (später `onset:`). Contra: unnötige Hierarchie, abweichend von allen Nachbarn.
- *Begründung A:* Die Condition-API (`ApplyCondition`/`ClearCondition`, `Core.hs:1171-1172`) ist das etablierte Vorbild; Autoren müssen keine neue Systematik lernen.

**2. `clear_vehicle_condition` als eigener Effekt oder nur `repair`?**
- **A. Eigener Effekt + bestehendes `repair` (empfohlen)** — Pro: symmetrisch zum Setter, skriptbare Heilung (Werkstatt-Trigger, Story), `repair` bleibt Spieler-Kommando. Contra: zwei Löschpfade zu warten (aber beide landen in `clearVehicleCondition`).
- B. Nur `repair` (kein Clear-Effekt) — Pro: weniger Code. Contra: keine getriggerte Heilung, Story-Design beschränkt.
- C. Eigener Effekt, aber nur intern (kein YAML-Key) — Pro: kleinste YAML-Oberfläche. Contra: halbes Feature, Worldbuilder-Nutzer blockiert.

**3. Fehlercode-Verhalten (hart fail vs. Warnung)**
- **A. Unbekanntes Fahrzeug = Error (`UnknownVehicleConditionTarget`), unbekannte Condition = Warning (`UnknownVehicleCondition`) (empfohlen)** — Pro: Fahrzeug-IDs sind stabil deklariert (Fehler ist immer ein Bug); Condition-Namen dürfen rein kosmetisch sein (`vehicle.warning`, `repair`). Contra: zwei Severity-Stufen zu merken.
- B. Beides Error — Pro: maximal streng. Contra: legitime „nackte" Zustände (nur Reparatur-/Anzeigezweck) werden fälschlich abgelehnt.
- C. Beides Warning — Pro: niemand wird blockiert. Contra: Tippfehler in Vehicle-IDs überleben still (genau das `UnknownDamageNPC` verhindert, `Compile.hs:3721`).

**4. Prädikat: Fahrzeug-by-Player-Position erlaubt?**
- **A. Dynamic-Token `current` zusätzlich zu Literal-IDs (empfohlen)** — Pro: `damage_npc`-Präzedenz (`Compile.hs:3719`), deckt 90 % der Rätsel ab („Fahrzeug, in dem ich sitze, ist beschädigt"); Compiler whitelisted den Token. Contra: ein reservierter Name mehr.
- B. Nur Literal-IDs — Pro: trivial kompilierbar. Contra: Dialog-Choices für wechselnde Fahrzeuge werden unhandlich.
- C. Separates Prädikat `vehicle_condition: <name>` (implizit aktuelles Fahrzeug) — Pro: kürzer. Contra: drittes Konzept, unsymmetrisch zu `has_condition`.

**5. Tick-Globalität (alle Fahrzeuge pro Zug ticken) in Scope?**
- **A. Nein — Non-Ziel, nur dokumentieren (empfohlen)** — Pro: hält den Plan bei ~23 h; Tick-Globalität hat eigene Fallstricke (Nachrichten für nicht sichtbare Fahrzeuge, Performance, `GameLoop.hs:280,307`-Änderung). Contra: Zustände abseits des aktuellen Fahrzeugs wirken erst bei Wiedereinstieg.
- B. Ja, mit in diesen Plan — Pro: vollständige Simulation. Contra: ca. +6 h, UI-/Nachrichten-Fragen, Scope-Drift.
- C. Global ticken hinter Policy-Flag (`GamePolicy`) — Pro: flexibel. Contra: Konfigurationskomplexität für ein Nischenfeature.

**6. Dokumentations-/lang-Umfang**
- **A. Voll: `docs/adventure-schema.md` + `CHANGELOG.md` + `lang/de.json` + OPEN-24-Update (empfohlen)** — Pro: Feature ist autorenfertig, Sprachlücke schließt sich sofort (`Messages.hs`-Defaults + `de.json`). Contra: ~2 h Zusatzaufwand.
- B. Nur Schema-Doku + Changelog, `lang/de.json` später — Pro: schneller. Contra: deutsche Sitzung zeigt englische Meldungen (Quality-Gap).
- C. Minimal: nur CHANGELOG — Pro: minimal. Contra: OPEN-24-„abgeschlossen" wäre nicht nachvollziehbar.

**7. Onset: feuert das Anwenden sofort das `vehicleConditionEffects`-Outcome?**
- **A. Nein, Wirkung erst ab nächstem Tick (empfohlen)** — Pro: konsistent zu Spieler-Conditions (`tickConditions`), vorhersehbare Zug-Reihenfolge. Contra: Autoren müssen Onset-Effekte selbst als `then:`-Outcomes bauen (das können sie bereits).
- B. Ja, sofortiges Feuern — Pro: direkteres Feedback (Alarmsirene). Contra: Sonderregel, doppelte Outcomes möglich (Onset + Tick im selben Zug).

**8. Serialisierungsform der neuen Core-Effekte**
- **A. Generische tag/contents-Form (empfohlen)** — Pro: null Instanzänderung, byte-kompatibel, wie `ApplyCondition`. Contra: gespeicherte Welt-JSON weniger lesbar.
- B. Explizite Keys (`"apply_vehicle_condition": {…}`) via eigener ToJSON-Clause — Pro: lesbar. Contra: Instanzänderung, Roundtrip-Aufwand, Abweichung vom Nachbarn.

---

## 8. Abnahmekriterien (Definition of Done)

- [ ] `vsActiveConditions` hat mit `applyVehicleCondition` einen echten Setter (`Set.insert`) — `grep -rn "Set.insert.*vsActiveConditions" src/` liefert ≥ 1 Treffer.
- [ ] `apply_vehicle_condition` / `clear_vehicle_condition` sind im YAML nutzbar und werden zu `ApplyVehicleCondition`/`ClearVehicleCondition` compiliert (VC-11).
- [ ] `vehicle_has_condition` steht in `evalPredicate` (`src/Game.hs`) und `isChoiceVisible` (`src/Types/Protocol.hs`) zur Verfügung (VC-06..08).
- [ ] Compiler-Fehlercode `UnknownVehicleConditionTarget` (Error) + `UnknownVehicleCondition` (Warning) existieren inkl. Tests (VC-12/VC-13).
- [ ] Alle Wildcard-Walker (`refsInEffect`, `conditionNamesInEffect`, `refsInPredicate`, `mapEff`/`resolveNpcStateEffect`, `Validate.hs`-Walker) behandeln die neuen Konstruktoren explizit (VC-15/VC-16 + Catch-all-Abgleich).
- [ ] `cabal build` beider Pakete grün trotz `-Werror=incomplete-patterns` (`text-adventure.cabal:41/56`); `cabal test` beider Suites 100 % grün.
- [ ] Test-Hack ersetzt (`test/Tests.hs:3005-3007` → echter Effekt, VC-19); Byte-Vertrag-Wächter grün (VC-17/VC-18): keine neuen Save-Felder, alte Welten byte-identisch.
- [ ] Doku: `docs/adventure-schema.md` (inkl. Verweis auf Kompatibilitätsregel `:1314-1317`), `CHANGELOG.md`, `lang/de.json` vollständig; Schema-Beispiel compilt (VC-20).
- [ ] `docs/befunde-text-adventure-engine.md`: **OPEN-24** (`:218`) auf ✅ gesetzt mit Verweis auf dieses Dokument.

**Ausblick (explizit nicht Teil der Abnahme):** OPEN-24-Nachfolger für **`vsRoomOverrides`** (`src/Types/Vehicles.hs:115`) — analoge Setter-/Effektlogik für condition-flavoured Raum-Beschreibungen; sowie ggf. Tick-Globalität (Entscheidung 5) und `set_state`-Diagnose im Catch-all `src/Effects.hs:848`.

---

*Anschluss an `docs/befunde-text-adventure-engine.md` — OPEN-24 (Fahrzeug-Zustand-Setter). Dieses Dokument ist der verbindliche Umsetzungsplan; Implementierung erfolgt in den Phasen 0–7 mit den dort definierten Abbruch-/Weiterkriterien.*
