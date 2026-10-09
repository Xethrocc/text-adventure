# Changelog

## Unreleased

### FIX-07: Raumansicht nach Bewegung (Auto-Describe)

Bisher zeigte die CLI nach `go <direction>` nur `You move {dir}.` — die
Raumbeschreibung erschien erst nach einem manuellen `look`. Regeln mit `move:`
zeigten *überhaupt* keinen Zielraum (Befund `R-08`). Jetzt beschreibt die
Game-Loop nach jedem Kommando, das den Raum gewechselt hat (Ausgang, `move:`-
Effekt, Fahrzeug), den Zielraum wie `look`: Art, Beschreibung, Item-/Container-/
NPC-Liste, Fahrzeug-Zusatz — dunkle Räume melden ihre Dunkelheit. Bei Game-Over
bleibt die Ansicht aus (der Endtext hat das letzte Wort), `look`-Kommandos und
`on_look`-Hooks bleiben unverändert (der Auto-Look läuft ohne Hooks).

- `roomViewEvents` (Parser): die Raumansicht als wiederverwendbare Events,
  `look` und Auto-Look nutzen denselben Renderpfad.
- `autoDescribe` (GameLoop): hängt die Ansicht an alle vier Befehlspfade an
  (Turn/No-Turn, Veto mit/ohne Turn).

### Zork Stufe E: Dunkelheit & Zielauflösung an das Original angeglichen

Vier kleine, generische Engine-Anpassungen (jeweils mit Regressionstest):

- **Dunkelheit sieht Raum-Lichtquellen (Original `LIT?`, gparser.zil:1333):** Ein
  `dark`-Raum gilt auch dann als erhellt, wenn ein Item mit Tag `lightsource`
  **im Raum liegt** (z. B. die brennende Fackel im Fackelraum) — nicht nur, wenn
  der Spieler eine trägt.
- **Zielauflösung durch getragene Container (P-Fix):** `drink water`, `eat lunch`
  usw. finden Items in (offenen) Containern, die der Spieler trägt — das
  Original akzeptiert „den Container halten“ (`visibleItemsAt` glättet die
  Containerkette, wird jetzt auch fürs Inventar benutzt).
- **`consume` erreicht Container-Inhalte:** `consume:` eines Items in einem
  (offenen) getragenen Container wird nicht mehr mit `consume.not_reachable`
  verweigert; geschlossene Containerketten schneiden weiter ab.
- **Mengen kennen `in_container:` (B2-Sprache):** Zählfamilie (`count.…`) und
  `move_all`/`consume_all`/`reveal_all`/`set_state_all` können Container-Inhalte
  ziehen, z. B. `move_all: {what: items, in_container: flasche, tag: wasser,
  to: {in: nowhere}}` (Form: `count.items.in_container.<id>`).

### `tags_when` — zustandsabhängige Item-Tags (Stufe E Zork-Port)

Neues Item-Feld `tags_when: {<zustand>: [tags]}`: Tags, die nur gelten, solange
`itemStates.itemStatus` zum Schlüssel passt — gesteuert über das bestehende
`set_state:`. Effektive Tags = `tags` ∪ `tags_when[<status>]`; geprüft in
Dunkelheit (`lightsource`), `feelable`, Tag-Prädikaten, der Zählfamilie
(`count.items.tag.…`) und `reveal_all … tag:`. Die Autoren-Zeit-Validierung
(z. B. `DarkRoomDeadEnd`) sieht, ob ein Item den Tag in *irgendeinem* Zustand
tragen kann. Motivation: „brennende Kerze ist Lichtquelle, ausgegangene nicht“
(Dunkelheits-Semantik Original Zork I). Regressionstests:
`tags_when: a lantern lights the dark only while lit` und der JSON-Round-Trip
(`itemTagsWhen` wird bei leerem Map weggelassen — bestehende Abenteuer bleiben
byte-identisch).

### OPEN-01..09: alle offenen Engine-Punkte umgesetzt (Zork-Befunde)

Drei Blöcke (Commits `0f2f5b1`, `bc01829`, `5d7fecd`), jeweils mit Regressionstests;
Referenz ist `befunde-text-adventure-engine.md` §7.

- **`cmd.succeeded` (OPEN-01):** boolesche Variable, die nach Ausführung angibt, ob das
  Kommando gelungen ist — `false` bei give/put/open/take-Fehlern, `block:`-Vetos
  (Regeln *und* `verb_map`-Phasen), Dunkel-Verweigerungen, Mehrdeutigkeiten;
  `true` bei erfolgreichen Standard-Aktionen und regel-/handler-gesteuerten
  Custom-Verben. Compound (`take x and y`) und `take all` sind eine Konjunktion
  (alle Teile werden versucht, `true` nur wenn alle gelingen). `on: before` sieht
  das Pending-Ergebnis, `on: command` das Endergebnis. Metakommandos (`undo`,
  `help`, `save`, …) laufen ohne Trigger-Pipeline und setzen das Bit nicht.
- **Item-Status-Synchronisation (OPEN-02):** `set_state`/`set_state_all` und die
  Container-Verben schreiben jetzt `entityStates` **und** `itemStates.itemStatus`
  synchron — Zustands-Suffixe in `verb_map` (`wind,broken:`) greifen wieder, und
  `{state: X, is: Y}` liest eine definierte Schichtenfolge (NPC → Item →
  Entity-State) statt eines ODR über alle Speicher. Schließt `Z-01`.
- **`searchable: false` (OPEN-03):** Items mit `hidden: true` können vom `search`-
  Befehl ausgenommen werden (Default `true`, im `world.json` bei Default weggelassen —
  Byte-Kompatibilität). Schließt `Z-04`.
- **`location: nowhere` (OPEN-04):** Start-Ort für „existiert, aber noch nicht in der
  Welt“. Implementiert als **`Dormant`**-Ort, bewusst *nicht* als `Removed`-Tombstone
  (FIX-02: `give:` darf konsumierte Items nicht wiederbeleben). Aktivierbar über
  `give:`, `place:` und `move_all: {…, in: nowhere, …}`. Schließt `Z-05`.
- **`place:` (OPEN-05):** neuer Einzel-Item-Effekt
  `place: {item: X, in: <room>}` bzw. `{item: X, in_container: <container>}` mit
  Inventar-/Ausrüstungs-Buchhaltung, Kapazitäts- und Zyklenprüfung. Schließt `E-02`.
- **`blocked: true` (OPEN-06):** Ausgänge können direkt als blockiert deklariert
  werden (eigene Meldung per `msg:`), ohne das `when: {not: {'true': true}}`-Muster —
  die ~35 `DeadExit`-Warnungen entfallen für diese Ausgänge. Schließt `W-02`.
- **`in: {container: X, item: Y}` (OPEN-07):** Prädikat für direkte
  Container-Mitgliedschaft (auch versteckter Inhalt, geschlossene/verschlossene
  Container). Schließt `Z-03`.
- **`verb_map`-Zielaufflösung mit Präpositionen (OPEN-08):** `X with Y` / `X to Y`
  lösen den *primären* Ziel-Nomenphrase-Ausdruck (auch mehrteilige Namen wie
  `healing potion`) korrekt auf — in allen `verb_map`-Phasen, bei Items und NPCs;
  `cmd.arg1..N`, `cmd.count`, `cmd.raw_args` behalten ihre Token-Semantik.
  Schließt `P-02`.
- **`read` als Trigger (OPEN-09):** `read` ist jetzt ein kanonischer Event
  (`on: before read`, `on: command read`, `cmd.verb == "read"`), bleibt als
  Custom-Verb reserviert und aliast in `verb_map`-Schlüsseln weiter auf `examine`
  (alte `read,intact`-Einträge bleiben gültig). Schließt `R-06`.
- **Tests:** 973 grüne Einzeltests (`text-adventure-tests` 570, `worldbuilder-tests`
  299, übrige 204), `scripts/ci.sh` inkl. E2E-Playthroughs grün.

### Verbnamen vereinheitlicht (Bugfix, Zork-Port Stufe B)

- **`on: before <verb>` feuerte für `board`/`enter` nie:** `checkBeforeVeto` (`Parser.hs`)
  bezog den Verbnamen aus `extractCommandArgs`, das `EnterVehicleCmd` als `"enter"`
  beschriftete — während `commandVerbName` (Quelle von `on: command`) `"board"`
  meldete. Eine Boot-Regel `on: before board` konnte den `enter`-Veto-Pfad damit nie
  erreichen, `on: before enter` wiederum scheiterte am Compile-Fehler
  `UnknownCommandVerb` (der Name stand in keiner der beiden Listen).
- **Eine Quelle für den kanonischen Verbnamen:** `commandVerbName` delegiert jetzt auf
  `extractCommandArgs`; `cmd.verb`, `on: before` und `on: command` können sich nicht
  mehr widersprechen. `EnterVehicleCmd` heißt kanonisch `"board"` (Alias-Eingabe
  `enter boat` zählt dazu).
- **`coreCommandVerbs` war lückenhaft:** die Liste (Pflicht-Nachfolger von
  `checkCommandVerbRefs`) kannte `board`, `exit`, `drive`, `wait`, `refuel`, `repair`,
  `choose`, `give`, `put`, `open`, `close`, `lock`, `unlock`, `ask`, `tell`, `map`,
  `watch`, `play`, `hand`, `deck`, `discard`, `end_turn`, `search`, `save`, `load`,
  `saves`, `restart`, `undo`, `compound` nicht, obwohl `commandVerbName` sie ausgibt
  und `board`/`enter`/`exit`/`drive`/`wait`/`refuel`/`repair` für Autoren sogar als
  `ReservedVerbName` gesperrt sind. Diese Namen waren damit weder als Core noch als
  Custom-Verb referenzierbar.
- **Nebenbei präziser:** `Save`/`Load`/`ListSaves`/`Help`/`Quit`/`Restart`/`Undo`/
  `CompoundCommand` lieferten bisher über `commandVerbName` den Fallback `"unknown"`
  und tragen jetzt ihren echten Namen — `on: command unknown` fängt nur noch
  Parser-Fehler ab.
- **Regressionstest** `coreCommandVerbs covers every commandVerbName`: probed ein
  `Command` je Konstruktorzweig und verlangt zusätzlich
  `commandVerbName == extractCommandArgs`.

### Trigger-Korrektur (Bugfix, Zork-Port Stufe A)

- **`on: command <verb>` für eingebaute Kommando-Konstruktoren:** `commandVerbName`
  (`GameLoop.hs`) kannte `GiveCmd`, `PutInCmd`, `TakeFromCmd`, `OpenCmd`, `CloseCmd`,
  `LockCmd`, `UnlockCmd`, `AskCmd`, `TellCmd`, `RecipesCmd`, `EnterVehicleCmd`,
  `ExitVehicleCmd`, `DriveToCmd`, `WaitCmd`, `RefuelCmd`, `RepairCmd` und `ChooseCmd`
  nicht und warf sie auf den Fallback `"unknown"` — Regeln wie `on: command give`
  konnten dadurch nie feuern (betroffen u.a. `give`, `put`, `open`, `close`, `lock`,
  `unlock`, `ask`, `tell`). Alle Konstruktoren tragen jetzt ihren Spielerverb-Namen
  (`on: command give` feuert seitdem wie dokumentiert).

### Punkte-/Score-System (K17)

- **Prinzip & Design-Entscheidung:**
  - Score ist **kein neuer Engine-Block**: Punkte sammeln bleibt Autorenarbeit über normale Trigger-Effekte (`add_var` bei `on: take`, `on: learn_recipe`, `on: npc_death`, `on: enter` etc.).
  - `score` bleibt ein normales `variables:`-Item (`type: int`, optional `max:`, optional `score_rankings:`).
  - **Kein neues `SaveState`-Feld** (Regel 6 bleibt intakt): der Score-Wert lebt in der bestehenden `VarMap`, die Rangliste in den statischen Weltdaten (`VarDef.vdScoreRankings`).
- **Neues VarDef-Feld `score_rankings`:**
  - `data ScoreRanking = ScoreRanking { srAt :: Int, srTitle :: String }`.
  - Optional an der Variablendefinition (`score_rankings:` als Liste von `{at: Int, title: String}`).
- **Befehl `score`:**
  - `ScoreCmd` in `Parser.hs`, `consumesTurn = False` (Lesebefehl analog zu `inventory` und `recipes`).
  - Zeigt `Score: N/max` bzw. `Score: N` an.
  - Mit `score_rankings` wird der Titel für den höchsten Schwellenwert mit `at <= score` in einer Folgezeile (`Rank: <title>`) ausgegeben.
  - Fehlt die Variable `score`, gibt der Befehl die hilfreiche Meldung `score.no_score` aus.
- **TUI-Statuszeilen-Konvention:**
  - Existiert eine Variable namens `score`, wird ` | Score: N` an den Raumtitel des Status-Panels angehängt (`[Status] West of House | Score: N`).
  - `score` wird aus den HUD-Gauges (`numericBars`) und der Key-Value-Statustabelle (`statsTable`) herausgefiltert, um redundante Doppelanzeigen zu vermeiden.
- **Meldungskatalog & Lokalisierung:**
  - Neue Meldungen `score.show` und `score.no_score` in `src/Messages.hs`, `src/Messages/LangDe.hs`, `lang/de.json`, `docs/message-catalog.md`.
- **Compile-Validierung:**
  - `score_rankings` nur auf Variable namens `score` erlaubt (Warnung `ScoreRankingsOnNonScoreVar`).
  - Schwellenwerte (`at`) müssen eindeutig sein (Warnung `DuplicateScoreRankingThreshold`).
  - Automatisches Sortieren nach `at` aufsteigend beim Compile.
- **Byte-Vertrag:**
  - `vdScoreRankings` wird bei leerer Liste (`[]`) in `ToJSON VarDef` weggelassen. Bestehende 68 Artefakt-Paare bleiben 100 % byte-identisch.
- **Tests & Qualität:**
  - Unit-Tests: Parser, `consumesTurn = False`, Rangauswahl, `score.no_score`-Fall in `test/Tests.hs`; Sortierung und Warnungen in `worldbuilder/test/Tests.hs`; TUI-Score-Statuszeile in `text-adventure-tui/test/Tests.hs`.
  - Fixture: `examples/fixtures/score.yaml` (Zork-artig: 2 Schätze per Trigger, 1 Rezept-Lern-Punkt, Rangliste, autorengeprüfte Tests).
  - E2E: `ci/e2e/score.in` / `ci/e2e/score.expect` in `scripts/ci.sh` eingehängt.

### Rezeptwissen: Rezepte lernen, sehen, unterscheiden (K11d)

- **Das Problem & die Design-Entscheidung:**
  - `craft.no_recipe` vertrat seit K11b beide Fälle: „kein Rezept produziert X" und „ein Rezept produziert X, aber du kennst es nicht". Die Formulierung „du kennst keins" (K11b) legte die Unterscheidung an, aber das Wissen selbst existierte nicht.
  - Lösung: **Wissen in der VarMap** (`known_recipe.<id>` = 1), nach dem K9-Muster — **kein neues `SaveState`-Feld** (Regel 6 bleibt intakt).
- **Stabile Rezept-IDs:**
  - `RecipeKey` erweitert: optionales `id:` plus `requires_learning: true` (auch für Paar-Rezepte — zwei Rezepte mit gleichen Zutaten kollidieren in der Map nicht mehr).
  - Rezepte ohne `id:` verhalten sich unverändert (nie wissensgesperrt), byte-identisch zum vorherigen Verhalten.
- **Lernen und Anzeigen:**
  - Neuer Effekt `learn_recipe: <id>` (idempotent, K9-Semantik).
  - Neuer Befehl `recipes`: Header mit bekannt/gesamt, darunter die **bekannten** Rezepte als „Ergebnisname — Zutaten". Unbekannte Rezepte erscheinen nie namentlich — nur der Zähler zeigt, dass es mehr gibt. Kein bekanntes Rezept: `recipes.empty`.
  - `recipes` zählt als Lesebefehl (`consumesTurn = False`, wie `inventory`).
- **Wissenssperre (A/B-Unterscheidung, Meldungen):**
  - **B** (kein Rezept produziert X): neuer Key `craft.no_product` — „There is no recipe that produces {target}." Bewusst schmal formuliert („dieses"), damit ein Autor später Rezepte ergänzen kann, ohne dass alte Spielstände anders antworten.
  - **A** (Rezepte existieren, keines bekannt): `craft.no_recipe` bleibt — „You don't know a recipe for {target}."
  - Bekannt, Zutaten fehlen: bestehende `use.not_carried`-Meldung, **aus dem besten bekannten Kandidaten** (nie `head candidates` über alle Kandidaten — das würde Rezept-Inhalte von Rezepten zitieren, die der Spieler nie gelernt hat; Review-Befund 2).
  - `use X on Y` prüft die Sperre ebenfalls (kein Bypass): neue Meldung `use.no_known_recipe` — „You don't know a recipe with {item1} and {item2}."
- **Compile-Validierung:**
  - `start_room:` wird in `GameWorld.startRoom` sichtbar (**Byte-Vertragsänderung, bewusst, gemessen**): alle 67 gelieferten Abenteuer/Fixtures bekommen `"startRoom":"<id>"` in der `world.json` (alle hatten `start_room:` in der YAML, nutzten es aber bisher nur zur Laufzeit). **Jedes `save.json` bleibt byte-identisch** — der Start-Raum stand schon als `currentRoom` im Save. Leerer Default (`""`, z. B. via Include-Pfad) schreibt `Nothing` und fällt auf die Fallback-Kette zurück.
  - Diese Abweichung schließt die ältere „byte-identisch"-Zusage für die Artefakte formal; sie ist die einzige und vollständig gemessene (67/67 nur `startRoom`, nichts sonst).
- **Tests:** 551 Engine (+28), 289 Worldbuilder (+5). Neue Fixture `examples/fixtures/rezeptwissen.yaml` mit E2E (`ci/e2e/rezeptwissen.in/.expect`).

### Rezept-Alias: craft <Ergebnis> und result: (K11b)

- **Das Problem & die Design-Entscheidung:**
  - Bisher steckte das Rezept-Ergebnis nur in den Effekten (`give:`, `create_item:`, etc.), potentiell beliebig verschachtelt. Ein Parser-Kommando `craft <Ergebnis>` konnte kein Rezept zuverlässig finden.
  - Lösung: Ein explizites, optionales Feld `result: <itemid>` in Item-Interaktionen (`interactions.item`).
  - **Sichtbarkeitsregel:** Ohne `result:` ist ein Rezept für `craft` unsichtbar, funktioniert aber weiterhin über `use X on Y`. Kein fragiles Effekt-Scanning.
- **Engine-Verb `craft`:**
  - Neues Verb `craft <Ergebnis>` (`CraftCmd String`).
  - Keine Synonyme (`make`, `brew`) — Vokabular bleibt Autoren-Sache.
  - Das Ziel ist der Ergebnis-Name, nicht die Zutat.
- **Such- und Ausführungslogik (`craftRecipe`):**
  - Ziel auflösen gegen Item-IDs, Namen und Keywords (lose Übereinstimmung wie sonst).
  - Alle Rezepte mit passendem `result:` prüfen:
    1. Paar-Rezept: `item1`/`item2` beide erreichbar (Raum, getragen oder ausgerüstet) -> ausführen. Bindung von `{item1}` und `{item2}` in Deklarationsreihenfolge (`item1` = erstgenanntes Item, `item2` = zweitgenanntes Item).
    2. Multi-Zutaten-Rezept: `ingredients ⊆ reachable` -> ausführen (`bindIngredientVars`).
    3. Rezept bekannt, Zutaten fehlen -> bestehende Meldung `use.not_carried` ("You need to be carrying '{item}' to use it."), nicht `craft.no_recipe`.
    4. Kein passendes Rezept -> `craft.no_recipe` ("You don't know a recipe for {target}." / "Du kennst kein Rezept für {target}.").
  - `craft` führt bestehende Rezept-Outcomes aus: `consume:` greift automatisch.
- **Keine Zustandsänderung:**
  - Kein neues `SaveState`-Feld, kein neuer Effekt, kein neues Event.
- **Compile-Validierung:**
  - `result:` wird statisch gegen bekannte Items geprüft: Warnung `UnknownRecipeResult`, falls das Ziel-Item nicht deklariert ist.
- **Meldungskatalog:**
  - Neuer Schlüssel `craft.no_recipe` in `src/Messages.hs` und deutsches Sprachpaket `lang/de.json` / `src/Messages/LangDe.hs`.
- **Byte-Vertrag gemessen:**
  - 134/134 Artefakte über alle 67 Abenteuer und Fixtures 100 % byte-identisch gegen `6815668` (0 Abweichungen).
- **Tests & Qualität:**
  - 7 neue Unit-Tests in `test/Tests.hs`:
    - Erfolg Paar-Rezept (`craft mana_potion` bindet `item1`/`item2`).
    - Erfolg Multi-Zutaten (`craft storm_potion` bindet `ingredient1..N`).
    - Fehlende Zutat meldet `use.not_carried` statt `craft.no_recipe`.
    - Rezept ohne `result:` für `craft` unsichtbar, aber per `use` nutzbar.
    - Unbekanntes Ergebnis liefert `craft.no_recipe`.
    - Consume-Verhalten bei Folgeversuch (fehlende Zutat statt Erfolg/no_recipe).
    - Regressionstest für `use Mana Leaf on Kettle` verhält sich byte-identisch.
  - 1 neuer Test in `worldbuilder/test/Tests.hs` (`testCraftingRecipeResultValidation` für `UnknownRecipeResult`).
  - CI grün, 0 Compiler-Warnungen.

### Stufenwechsel-Trigger: on_standing_change (G9c)

- **Die Lücke geschlossen:**
  - Bisher mussten Autoren Stufenwechsel pollen (z. B. `on: turn` mit `standing: { at_least: 50 }`). Das verbrauchte Trigger-Budget in jedem Zug und war fehleranfällig.
  - Neu: `OnStandingChange FactionID` reagiert ereignisgesteuert direkt und genau einmal bei Schwellenüberschreitung.
- **YAML-Form:**
  - Mapping-Form: `on_standing_change: { faction: <id>, to: <stufenname> }` (wobei `to` optional ist).
  - String-Form: `on: standing_change <faction>`.
  - Kann unter `rules:` oder `triggers:` verwendet werden.
- **Semantik (echter Stufenwechsel, keine reine Wertänderung):**
  - Feuert bei **jedem echten Stufenwechsel** (Schwellenüberschreitung mit Stufenänderung).
  - Wertänderungen innerhalb derselben Stufe (z. B. 40 -> 45 innerhalb „neutral") feuern **nicht**.
  - Gleicher Wert erneut setzen (z. B. 20 -> 20) feuert **nicht** (symmetrisch zu `OnStateChange`).
  - Rückwechsel über Schwellen (z. B. 20 -> 0) feuern wie erwartet.
  - Ohne `to` feuert der Trigger bei jedem Stufenwechsel der Faktion.
  - Der Hook in `Effects.hs` fängt alle Schreibwege auf `faction.<id>` ab (`set_var`, `add_var`, `compute_var`, `standing: {add/set}` sowie Dialog-Effekte).
  - Der alte Stufenname wird dynamisch vor der Änderung berechnet (`lookupStandingName`). **Kein neues SaveState-Feld**, keine Migration, bestehende Saves bleiben 100 % kompatibel.
  - Rekursionsschutz: Triggereffekte, die dieselbe Faktion verändern, sind durch `maxOutcomeDepth = 20` gegen Endlosschleifen geschützt und erzeugen eine defensive Engine-Diagnose.
- **Ehrliche Compile-Warnungen:**
  - Da Faktionen und deren Stufennamen zur Compile-Zeit statisch bekannt sind, validiert der Compiler ehrlich:
    - Unbekannte Faktion -> Compile-Warnung `UnknownFaction`.
    - Unbekannte Stufe in `to` -> Compile-Warnung `UnknownFactionLevel`.
  - Beide Warnungen sind `ciWarning`s (die Welt läuft weiter, kein stiller Ausfall).
- **Byte-Vertrag gemessen:**
  - 134/134 Artefakte über alle 67 Abenteuer und Fixtures 100 % byte-identisch gegen `cc76b33` (0 Abweichungen).
- **Tests & Qualität:**
  - 8 neue Engine-Tests in `test/Tests.hs` (Schwellenüberschreitung 0 -> 20, Innerhalb-Stufe 40 -> 45 feuert nicht, Identischer Wert 20 -> 20 feuert nicht, Rückwechsel 20 -> 0, Ohne-to-Modus, set_var-Schreibweg, fremde Variable ignoriert, Rekursions-Tiefenschutz). Engine-Tests: 536 (vorher 528).
  - 2 neue Worldbuilder-Tests in `worldbuilder/test/Tests.hs` (YAML-Parsing und Trigger-Registrierung, statische Validierung für UnknownFaction und UnknownFactionLevel). Worldbuilder-Tests: 284 (vorher 282).
  - CI grün, 0 Compiler-Warnungen.

### Faktions-Stufen auswerten: standing_name (G9a)

- **Die Lücke geschlossen:**
  - Bisher wurden `afLevels` im Compiler validiert (`DuplicateFactionLevel`, `BadFactionLevel`), aber nie in die Runtime übertragen. Texte konnten nur Zahlen zeigen (`faction.<id>`), nicht aber Beziehungsstufen wie „neutral" oder „freundlich".
- **Runtime-Modell:**
  - `GameWorld` erhält `factions :: Map FactionID [FactionLevel]`.
  - `FactionLevel` kapselt `{ flAt :: Int, flName :: String }`.
  - Wenn keine Faktionen deklariert sind (`Map.null`), wird das Feld in `world.json` ausgelassen.
  - `SaveState` bleibt unverändert; keine neue Variable in `VarMap`.
- **ValueRef & Platzhalter: `{standing_name: <faction>}` / `{standing_name.<faction>}`:**
  - Wird wie `dice.highest` (K1) oder `set_completion` (K15) bei der Textformatierung **dynamisch berechnet**, nicht gespeichert.
  - **Auswertungsregel:** Ermittelt die **höchste** Stufe, deren `at <= aktueller Wert` (`faction.<id>`).
  - **Grenzfälle:**
    - Wert unter kleinstem `at` -> niedrigste deklarierte Stufe.
    - Keine Stufen deklariert -> `""` (leer, kein Fehler).
    - Unbekannte Faktion -> `""` (leer, kein Fehler).
    - Stufe ohne Namen -> `""`.
- **Ehrlichkeit (wie K16c):**
  - `standing_name` wird **nicht zur Compile-Zeit geprüft**. Wer `{standing_name: schreibfehler}` schreibt, erhält zur Laufzeit den leeren String `""` (kein Compile-Fehler, keine Warnung, kein Absturz).
  - *Begründung:* Der Beziehungsname hängt am dynamischen Spielstand, nicht am statischen YAML — ein Compile-Fehler wäre eine Lüge.
- **Byte-Vertrag gemessen:**
  - 131/134 Artefakte über 67 Abenteuer und Fixtures 100% byte-identisch gegen `1c88cf2`.
  - `save.json`: 67/67 (100%) byte-identisch.
  - `world.json`: 64/64 (100%) aller Abenteuer ohne Faktionen byte-identisch.
  - Genau 3 Abenteuer deklarieren Faktionen (`examples/modules/combo.yaml`, `examples/modules/factions.yaml`, `examples/modules/trade.yaml`): hier erscheint legitim das neu kompilierte `factions`-Mapping in `world.json`; alle anderen Schlüssel bleiben 100% identisch.
- **Tests & Qualität:**
  - 5 neue Engine-Tests in `test/Tests.hs` (Namensanzeige, Grenzwert-Beweis 50 vs. 49, leere Stufen, unbekannte Faktion, Negativtest auf Schreibfehler). Engine-Tests: 528 (vorher 523).
  - 1 neuer Worldbuilder-Test in `worldbuilder/test/Tests.hs` (Kompilierung von `AFactionLevel` in `GameWorld.factions` und 0 `UnknownPlaceholder`-Warnungen). Worldbuilder-Tests: 282 (vorher 281).
  - CI grün, 0 Compiler-Warnungen.

### Welt-Ziele für Fähigkeiten: use-ability <id> auf <ziel> (K16c)

- **Das Kern-Muster:** *Fähigkeit auf Ziel -> Zustandsänderung am Ziel.*
  - Verwendet die bestehende Vokabel `set_state: entity, to: state` mit dynamischer Zielauflösung `{set_state: "{cmd.target}", to: <state>}`.
  - Kein neuer Effekt (kein `set_state_dynamic`), kein neues Verb, kein neues SaveState-Feld.
- **Parser-Trennung:**
  - `use-ability <id> auf <ziel>` (Deutsch) und `use-ability <id> on <ziel>` (Englisch) trennt die Fähigkeits-ID und das Ziel sauber an `auf`/`on`.
  - Liefert `InteractWith (VCustom "use-ability") <id> <ziel>`.
  - Rückwärtskompatibilität: `use-ability <id>` ohne Ziel liefert wie bisher `Interact (VCustom "use-ability") <id>`, `cmd.target` wird zu `""` gebunden.
- **Laufzeit-Auflösung & Diagnose:**
  - `AOSetEntityState` bzw. `SetValue (VRActorProp (ActorEntity eIdRaw) PState)` löst dynamische Ziele zur Laufzeit über den bewährten K12-Mechanismus `resolveVarName` auf.
  - Compile-Prüfung: Literale Ziele werden wie bisher strikt auf Existenz geprüft (`UnknownStateTarget`). Dynamische Ziele (`'{' `elem` target`) werden bewusst **nicht** zur Compile-Zeit geprüft (gleiche Ehrlichkeit wie `{item1}` in K11c).
  - Negativfall: Ist das Ziel zur Laufzeit unbekannt, scheitert der Effekt nicht still, sondern meldet `target.not_seen` (`You don't see '<ziel>' here.`) und erzeugt einen Diagnose-Eintrag `[engine] set_state: unknown entity '<ziel>'`.
- **Stille Kante (Item-Präemption):**
  - `use-ability` (auch mit Ziel) greift vor der allgemeinen Item-Auflösung in `dispatchCommandEv`. Wenn ein Item denselben Namen wie die Fähigkeit trägt, gewinnt stets die Fähigkeit.
- **Die drei Verifikationsbeispiele:**
  1. *Fantasy (Steintafel):* `use-ability feuerschlag auf ueberwucherte_steintafel` -> Flammen verbrennen das Gestrüpp, Inschrift wird sichtbar.
  2. *Fantasy (Tor):* `use-ability feuerschlag auf zugewachsenes_tor` -> Tor wird entriegelt (`unlocked`), Weg in den nächsten Raum wird frei.
  3. *Puzzle (Text):* `use-ability alte_sprache auf antiker_text` -> Text wird lesbar (`lesbar`), Glyphen werden übersetzt.
- **Byte-Vertrag gemessen:**
  - 134/134 Artefakte über alle 67 Abenteuer und Fixtures (demo, thefog, 10 genres, 15 modules, 40 fixtures) byte-identisch gegen `a3f2321`.
- **Tests & Qualität:**
  - 6 neue Engine-Tests in `test/Tests.hs` (Parser-Trennung, 3 Welt-Ziel-Beispiele, Negativtest auf unbekanntes Ziel, Stille-Kante-Regression). Engine-Tests: 521 (vorher 515).
  - 1 neuer Worldbuilder-Test in `worldbuilder/test/Tests.hs` (dynamisches `set_state` kompiliert fehlerfrei, Literale weiterhin geprüft). Worldbuilder-Tests: 281 (vorher 280).
  - CI grün, 0 Compiler-Warnungen.

### Fähigkeiten im klassischen und narrativen Kampf (K16b)

- **Fähigkeiten in allen Kampfprofilen:**
  - Nach der Freischaltung außerhalb des Kampfes (K16a) sind Fähigkeiten (`use-ability <id>`) nun in `classic` und `narrative` vollwertig integriert.
  - Verwendet die generische Kosten-/Cooldown-/Effektlogik `tacticalAbilityIn` ohne Duplizierung.
- **Entscheidung 1 — Classic (Rundenverbrauch & Rache-Schlag):**
  - Eine Fähigkeit verbraucht konsistent eine Kampfrunde (`combat.round` inkrementiert, Meldung `combat.ability_use`: `"Round {round}: You use {ability}!"`).
  - Der Gegner kontert mit demselben regulären Rache-Schlag wie bei `CAAttack`. Nur der Spieler-Aktionsblock wird ersetzt; Ally-, Ship- und Retaliation-Logik bleiben geteilt und identisch.
  - *Begründung:* Ohne Rundenverbrauch wäre eine Fähigkeit im klassischen Kampf ein Cheat (beliebig viele Fähigkeiten vor dem Gegnerschlag).
- **Entscheidung 2 — Narrative (Vorbereitung & Wurf):**
  - Der narrative Kampf hat keine Runden (`combat.round` wird **nicht** gesetzt, Meldung `ability.use`: `"You use {ability}!"` ohne Round-Präfix).
  - Die Fähigkeit läuft als **Vorbereitung** vor dem Wurf ab (`tacticalAbilityIn` mit `inCombat=False`). Danach erfolgt der reguläre vergleichende Wurf (`effectiveAttack >= defense + difficulty`).
  - Wenn die Fähigkeit Boni auf `player.attack` / `bonus.attack` gewährt, zählt der Bonus direkt für die Sieg-/Niederlage-Entscheidung.
  - *Begründung:* Der narrative Kampf hat keine Rundenstruktur; Vorbereitung ist die einzig konsistente Semantik.
- **Gating & Fehlerbehandlung:**
  - Schlägt eine Fähigkeit fehl (unbekannte ID, Cooldown aktiv, unzureichende Ressourcen), wird die Aktion abgebrochen: keine Runde wird verbraucht, kein Rache-Schlag erfolgt und im narrativen Kampf wird kein Wurf ausgelöst.
- **Byte-Vertrag gemessen:**
  - 134/134 Artefakte über alle 67 Abenteuer und Fixtures byte-identisch gegen `cdf9636`.
- **Tests & Qualität:**
  - 2 neue Engine-Tests in `test/Tests.hs`: Classic-Rundenverbrauch mit Rache-Schlag-Vergleich gegen `CAAttack`, Narrative-Vorbereitung mit Bonus-Auswirkung und Ausschluss von `combat.round`.
  - Regressionstests: Taktischer Kampf (`combat-tactical.in`) unverändert mit `"Round 1: You use Mächtiger Hieb!"`, K16a-Weltfall unverändert.
  - Engine-Tests: 515 (vorher 513). CI grün, 0 Compiler-Warnungen.

### Fähigkeiten außerhalb des Kampfes auslösen (K16a)

- **Die Regel:** *Eine Fähigkeit braucht einen Kampf nur, wenn ihr Effekt einen Kampf braucht.* Nicht die Fähigkeit selbst.
- **Ehrliche Dokumentation — Echte Verhaltensänderung:**
  - Bisher war `use-ability <id>` in `interactNotFound` an `CombatTactical _ <- combatProfile (world state)` gekoppelt. Ein Abenteuer mit `profile: classic` oder `narrative` konnte Fähigkeiten überhaupt nicht auslösen.
  - Ab K16a ist `use-ability` ein allgemeiner Weltbefehl: Fähigkeiten können außerhalb des Kampfes sowie in allen Profilen (`tactical`, `classic`, `narrative`, `off`) ausgelöst werden.
- **Die stille Kante (Item-Präemption):**
  - `use-ability <id>` greift vor der allgemeinen Item-Auflösung in `dispatchCommandEv`. Wenn ein Item `sturm` und eine Fähigkeit `sturm` existieren, greift die Fähigkeit und das Item wird nicht fälschlich angesprochen.
- **Außerhalb des Kampfes:**
  - `tacticalStateEffects` entfällt, wenn kein Kampf läuft (`not (isCombatEngaged st)`): `combat.round`, `combat.engaged` und `combat.action` werden **nicht** gesetzt.
  - `cost_var`, `cost`, `cooldown` und `effects:` (inklusive `{consume: ...}`) laufen unverändert.
  - Im taktischen Kampf bleibt das Verhalten 100% unverändert (Regressionstest).
- **Gemessener Byte-Vertrag:**
  - `use-ability` kommt repo-weit in genau einer Datei vor (`examples/modules/combat-tactical.yaml`), die von keinem Abenteuer eingebunden wird (0 Treffer).
  - 0 Abweichungen über alle 70 Abenteuer und Fixtures gegen b7ea257.
- **Tests & Qualität:**
  - 4 neue Engine-Tests: Auslösung außerhalb des Kampfes, Item-Präemption (Stille Kante), Cooldown-Gating außerhalb des Kampfes, taktischer Kampf Regressionstest.
  - Engine-Tests: 513 (vorher 509). CI grün, 0 Warnungen.

### Multi-Zutaten-Rezepte: ingredients: in interactions.item (K11c)

- **Unabhängiger Byte-Nachweis:** Von **134** Artefakten (67 Abenteuer und
  Fixtures) weicht **kein einziges** ab — alle byte-identisch gegen `40dbfd6`,
  mit Worktree-Vergleich über `demo`, `thefog`, `genres/*`, `modules/*`,
  `fixtures/*`. Das Paar-Rezept in `fantasy.yaml` ist unverändert. Das ist der
  Beweis, dass die Umstellung der World-Datenstruktur von `(String, String)` auf
  `RecipeKey` **rückwärtskompatibel** ist: `Map.fromList [((a,b), eff)]` und
  `Map.fromList [(RecipePair a b, eff)]` erzeugen dieselbe Ausgabe.
- **Match-Regel, beide Teile getestet** (K11c.3):
  1. `R ⊆ erreichbare Items` — das Rezept feuert nur, wenn **alle** Zutaten
     erreichbar sind.
  2. **Der Befehl nennt mindestens einen Rezept-Zutaten.** Der Test
     `testIngredientsRecipeDoesNotFireWhenCommandNamesNoRecipeItem` legt alle
     drei Zutaten ins Inventar, tippt `use messer auf apfel` (keine Zutat) und
     prüft, dass das Rezept **nicht** feuert. Der zweite Teil ist nicht
     optional: ohne ihn würde jedes Rezept bei jedem `use` feuern, sobald die
     Zutaten zufällig mitgeführt werden.
- **Reihenfolge** (aus K11a und K15 übernommen): `{item1}`/`{item2}` bleiben
  unverändert, `{ingredient1..N}` kommen danach; `resolveVarName` läuft **vor**
  jeder Ortprüfung; die K15.0-Ort-Regel gilt für jede `consume:` einzeln.
- **Byte-Vertrag:** 134/134 byte-identisch. `scripts/ci.sh` grün
  (`All checks passed`), 0 Compiler-Warnungen, **509** Engine- und **280**
  Worldbuilder-Tests.

- **Optionale Listenform `ingredients:` im bestehenden `interactions.item`-Block:**
  - `AItemInteraction` erweitert um `aiiId :: Maybe String` und `aiiIngredients :: [String]`.
  - **Bewusster Verzicht auf `recipes:`-Block und `craft`-Verb:** `interactions.item` ist die autoritative Sektion für Item-auf-Item-Interaktionen. Ein separater Block wurde verworfen; die Auslösung bleibt das bewährte `use X on Y`.
  - **Konfliktverbot:** Entweder Paar (`item1`/`item2`) oder Liste (`ingredients:`). Die Angabe von beidem gleichzeitig führt zu einer harten Diagnose (`ItemInteractionConflict`), kein stilles Übernehmen.
  - `ingredients` ist in `knownKeys EntItemInteraction` registriert (0 unbekannte Schlüsselwarnungen; Tippfehler werden zuverlässig gewarnt).

- **World-Datenstruktur und Match-Regel:**
  - `itemInteractions` im `GameWorld` wurde von `Map (ItemID, ItemID) Effect` auf `Map RecipeKey Effect` generalisiert mit:
    `data RecipeKey = RecipePair String String | RecipeIngredients (Maybe String) [String]`.
  - **Begründung der Struktur:**
    1. Trennt Paar-Schlüssel sauber von Multi-Zutaten-Mengen.
    2. Die `Ord`-Instanz sortiert `RecipePair` vor `RecipeIngredients`, wodurch Paar-Rezepte deterministisch Vorrang genießen.
    3. `RecipePair` serialisiert unverändert zu `{"a": a, "b": b, "effect": ...}`, womit bestehende Abenteuer 100% byte-identisch bleiben.
  - **Die nicht verhandelbare Match-Regel:**
    Ein Rezept $R$ feuert genau dann, wenn:
    $R \subseteq \text{erreichbare Items} \land \text{Befehl nennt mindestens ein Item aus } R$.
    Ohne den zweiten Teil würde jedes Rezept bei jedem `use` feuern, sobald die Zutaten zufällig im Inventar liegen.
  - **Zumsortierung / Prüfungsreihenfolge:**
    1. `{item1}` / `{item2}` Paar-Rezepte unverändert zuerst.
    2. Multi-Zutaten-Rezepte danach, binden `{ingredient1..N}`.
    3. `resolveVarName` läuft VOR jeder Ortprüfung.
    4. Ortprüfung (`isReachableForConsume`, K15.0) greift für jedes `consume:` einzeln.

- **Dynamische Variablen `{ingredient1..N}`:**
  - Bindet `{ingredient1}`, `{ingredient2}`, ..., `{ingredientN}` positional an die deklarierte Zutatenliste.
  - Validierung: Zugriff auf nicht deklarierte Indizes (z. B. `{ingredient4}` bei 3 Zutaten), `{item1}` in Listenrezepten oder `{ingredient1}` außerhalb von Zutatenrezepten wird zur Compile-Zeit mit `UnknownItemRef` abgewiesen.

- **EHRLICHE DOKUMENTATION: Echte Verhaltensänderung:**
  - Ein Listen-Rezept benennt dem Spieler **nicht**, welches konkrete Item es ausgelöst hat. Bei $N$ Zutaten ist $X$ (oder $Y$) im Befehl nur noch der Impulsgeber; das Rezept prüft die Vollständigkeit der Menge über Raum und Inventar.
  - Kein impliziter Verbrauch: Zutaten werden nur verbraucht, wenn `consume: "{ingredientK}"` in `effects:` steht.

- **Spielverifikation (EIN Lauf, beide Zweige nebeneinander):**
  - Eigene Probe mit 3 Zutaten (`cauldron` im Raum, `fire_essence` und `water_essence`):
    - Zweig 1 (Zutat fehlt): `use fire_essence on cauldron` ohne `water_essence` -> feuert nicht (*„Nothing happens."*), beide Items bleiben erhalten.
    - Zweig 2 (alle erreichbar): Nach Aufnahme von `water_essence` -> `use fire_essence on cauldron` feuert (*„The cauldron hisses violently! Fire and water fuse into a storm potion."*).
    - `inventory` danach: `storm potion` — alle drei Zutaten (`fire_essence`, `water_essence` und `cauldron` aus dem Raum) wurden ordnungsgemäß verbraucht.

- **Byte-Vertrag (gemessen):**
  - Alle 67 Abenteuer und Fixtures (134 Artefakte: `world.json` und `save.json`) wurden mit dem Baseline-Compiler (`40dbfd6`) und dem aktuellen Stand verglichen.
  - **0 Abweichungen:** Alle 67 Abenteuer inklusive `fantasy.yaml` (dessen Rezept ein Paar ist) sind 100% byte-identisch.

- **Tests & CI:**
  - 3 neue Engine-Tests: Gesamt **509 passed** (von 506).
  - 4 neue Worldbuilder-Tests: Gesamt **280 passed** (von 276).
  - `bash scripts/ci.sh` 100% grün, 0 Compiler-Warnungen.

### Weltobjekte sind endlich: repeatable: true, Default einmal, Ort-Regel (K15)

- **K15.0 Ort-Regel für `consume:` (`isReachableForConsume`):**
  - `consumeItem` und `MoveEntity eid Removed` prüfen vor dem Entfernen den aktuellen Aufenthaltsort (`itemLocation`) des Gegenstands.
  - **Erlaubt:** `InRoom _` (im Raum), `CarriedBy ActorPlayer` (im Spielerinventar), `EquippedBy _` (ausgerüstet = am Körper getragen und damit in direktem Zugriff).
  - **Verweigert:** `InContainer _` (in einem Container/Behälter) und `CarriedBy (ActorNPC _)` (im Besitz eines NPCs).
  - Bei Verweigerung bricht die Engine nicht ab, sondern gibt die Meldung `consume.not_reachable` aus (*„Das liegt nicht bei dir."* / *„That is not within your reach."*). Das Item bleibt an seinem Ort erhalten.
  - Reihenfolge gewahrt: `resolveVarName` (K11a) wird vor der Ort-Prüfung ausgeführt, sodass dynamische Referenzen wie `"{item1}"` korrekt aufgelöst werden.
- **K15.1 Sichtbarkeitsfilter (Default `einmal`, Ausnahme `repeatable: true`):**
  - Raumitems sind standardmäßig endlich. Sobald ein Item genommen oder verbraucht wurde, wird es beim Wiederbetreten des Raumes nicht erneut materialisiert.
  - **Ausnahme `repeatable: true`:** Werkzeuge und unerschöpfliche Ressourcen tragen `repeatable: true` und bleiben an ihrem Heimatort (`itemHomeLocation`) dauerhaft erhalten.
  - **Fehlende States:** Items, die nicht im SaveState (`itemStates`) verzeichnet sind, gelten standardmäßig als sichtbar (kein Default-Blocker).
  - **Container:** Ortsfeste Container (`containers:`) und Container-Items werden gemeinsam geregelt: Default endlich, Ausnahme `repeatable: true`.
  - **Filterort:** Der Filter sitzt zentral in `getItemsInLocation` (`src/Game.hs`), der einzigen autoritativen Stelle für Raumgegenstände (von Parser, Look, Take, Completion gleichermaßen genutzt).
- **K15.2 Referenzabenteuer `examples/genres/fantasy.yaml` & Spieltest:**
  - `herb` (Waldrand) ist Default endlich; `mortar` (Einsiedlerhütte) trägt `repeatable: true`.
  - Spieltest verifiziert: Salbe herstellen -> Hütte verlassen -> zurückkehren: Kraut ist am Waldrand WEG (*„You don't see 'herb' here."*), Mörser ist in der Hütte DA (*„stone mortar"*). Ein zweiter Versuch zur Salbenherstellung scheitert.
- **K15.3 Dokumentation:**
  - `docs/adventure-schema.md` aktualisiert mit `repeatable: true`, Default einmal, Ort-Regel für `consume:` und Dokumentation der offenen Frage bzgl. aus Containern entnommener und abgelegter Items.
- **Byte-Vertrag:**
  - Gemessen mit Worktree-Vergleich gegen `1b6bf88` über alle 67 kompilierbaren Abenteuer und Fixtures.
  - **Ausschließlich** `fantasy/world.json` weicht ab (`itemRepeatable: true`, `itemHomeLocation: "hermit_hut"` auf `mortar`).
  - Alle anderen 66 Abenteuer und Fixtures sind **100% byte-identisch**.
- **Tests & CI:**
  - 9 neue Engine-Tests (4 für K15.0, 5 für K15.1): Gesamtzahl **506 passed** (von 497).
  - 2 neue Worldbuilder-Tests: Gesamtzahl **276 passed** (von 274).
  - CI (`bash scripts/ci.sh`) 100% grün, 0 Compiler-Warnungen.

### Crafting verbraucht seine Zutaten: {item1} / {item2} in interactions.item (K11a)

- **Dynamische Variablen `{item1}` / `{item2}` beim Item-auf-Item-Crafting:**
  - `tryItemOnItem` bindet im Ausführungskontext die IDs der beiden Zutaten als `item1` und `item2` (ohne `cmd.`-Präfix, symmetrisch zum Entity- und NPC-Namensraum).
  - **Rezept-Treue bei umgekehrter Reihenfolge:** Bei `use B on A` für ein deklariertes Rezept `item1: A, item2: B` bindet `{item1}` an `A` und `{item2}` an `B`. Damit zerstört `consume: "{item1}"` immer die deklarierte Zutat `item1` und schützt Werkzeuge unabhängig von der syntaktischen Eingabereihenfolge.
  - **Gültigkeitsbereich:** Die Variablen existieren ausschließlich während des Item-auf-Item-Aufrufs; sie existieren vorher nicht und kollidieren nicht mit Adventure-Variablen (gemessen: 0 Kollisionen über alle 69 Abenteuer und Fixtures).
- **Laufzeit-Auflösung in `consume:`:**
  - `consume: "{item1}"` bzw. `consume: "{item2}"` löst dynamische Referenzen zur Laufzeit über `resolveVarName` (K12-Namensauflösung) auf und entfernt das entsprechende Item aus dem Spielzustand (`MoveEntity actualEid Removed`).
  - **Ehrlicher Hinweis: Kein impliziter Verbrauch:** Zutaten werden nicht magisch verbraucht. Wer Zutaten konsumieren möchte, notiert `consume:` explizit. Werkzeuge bleiben ohne `consume:` im Inventar.
  - **Bewusste Nicht-Ausweitung auf `give:`:** `give:` erfordert ein vollständig in `itemDefs` deklariertes Item (Name, Beschreibung, Gewicht etc.); eine dynamische Namensauflösung würde nicht-deklarierte Phantom-Items erzeugen.
- **Unabhängig nachgemessen (Spiel, nicht Test):** `ci/e2e/fantasy.in` bis Zeile 16,
  dann `use herb on mortar`:
  - Ergebnis: `> You grind the moonwort in the stone mortar into a bright paste.`
  - `inventory` danach: `blue vial, rusty sword` — **beide Zutaten** (Kraut *und*
    Mörser) sind verschwunden, nicht nur eines.
  - Ein **zweiter** `use herb on mortar` scheitert mit
    `> You need to be carrying 'herb' to use it.` Das Rezept ist damit nicht mehr
    unbegrenzt wiederholbar — der eigentliche Zweck der Stufe.
  - Die Bindung im `altKey`-Zweig tauscht mit (`bindItemVars (itemId target)
    usedId`, `src/Parser.hs`), d. h. `{item1}` bleibt auch bei `use B on A` das
    **zuerst genannte** Item. Gemessen, nicht angenommen.
- **Validator-Nebenbefund:** `MoveEntity` mit dynamischer Item-Referenz wurde intern
  **zweimal** eingesammelt — `idsFromOutcomeItem` *und* `idsFromOutcomeNPC` — was
  `MissingNPC "{item1}"` meldete, obwohl kein NPC gemeint war. Beide Sammler
  filtern dynamische Platzhalter jetzt aus; die neue Diagnose `UnknownItemRef`
  (Compile) greift für ungebundene Referenzen wie `{item9}`.
- **Byte-Vertrag:** von 134 Artefakten weicht **ausschließlich**
  `examples_genres_fantasy/world.json` ab. Die 68 anderen Abenteuer und Fixtures
  sind byte-identisch (Worktree-Vergleich gegen `8457430`). `fantasy` ist ein
  Siegpfad-Genre — die `ci/e2e`-Marker bleiben grün, `scripts/ci.sh` meldet
  `All checks passed`, 0 Compiler-Warnungen.
- **Harte Compiler-Diagnose (`UnknownItemRef`):**
  - Ungebundene dynamische Referenzen wie `consume: "{item9}"` oder die Verwendung von `{item1}`/`{item2}` außerhalb von `interactions: item:` werden vom Worldbuilder als harter Compile-Fehler (`ciError` mit Code `UnknownItemRef`) und Levenshtein-Korrekturvorschlag (`formatUnknownKey`) abgewiesen.
  - Statische Validierung in `idsFromOutcomeItem` und `idsFromOutcomeNPC`: Dynamische Platzhalter (`{item1}`, `{item2}`) werden ignoriert und nicht fälschlich als fehlende statische Items oder NPCs bemängelt.
- **Referenzrezept `examples/genres/fantasy.yaml`:**
  - Rezept `herb` + `mortar` ergänzt um `consume: "{item1}"` und `consume: "{item2}"`.
  - Spieltest verifiziert: Kraut und Mörser werden zu Salbe verarbeitet; Mörser ist danach verbraucht, ein zweiter Versuch schlägt fehl.

### Aussagen mit Sprecher und Wahrheitsgehalt: statements: (K9)

- **Aussagen-Sektion (`statements:`):**
  - Eigene Top-Level-Sektion neben `facts:`, die Berichte über Sachverhalte mit Urheber (`speaker`), Inhalt (`claims`), Wahrheitswert (`truth`, Default `true`), Wortlaut (`text`), optionalem Gate (`when`) und Gruppierung (`tag`) erfasst.
  - Unterstützt sowohl Listen- als auch Map-Syntax (`parseStatementsField`).
  - Merge-Vertrag bei `include:`: eigene Sektionen zuerst, dann Includes; doppelte IDs über Dateien hinweg lösen `DuplicateStatement` aus und benennen beide Quellen.
  - Byte-Gleichheit: Leere `statementDefs` werden in `world.json` ausgelassen (`[ "statementDefs" .= statementDefs gw | not (null (statementDefs gw)) ]`).
- **Roundtrip-Bug im `knows:`-Prädikat (vorbestehend seit W1, 2026-09-29):** Beim
  seriellen/deserialisieren einer Welt schrieb `ToJSON` die Form
  `{"knows": "<actor>", "fact": "<id>"}`, während `FromJSON` an der Stelle
  `knows` als **Objekt** erwartete. Der String-Zweig griff also für jeden Actor und
  las `"player"` als Fakt-ID — jedes `knows:` mit Actor prüfte zur Laufzeit
  `known.player.player` statt `known.player.<id>`. **Kein Abenteuer war betroffen,
  weil keines je `knows:` mit Actor verwendet hat** (gemessen: 0 Treffer über alle 66
  Abenteuer und Fixtures). Erst `detective.yaml` durch K9 machte es sichtbar.
  Der Decoder akzeptiert nun alle drei Formen — `{knows: {actor, fact}}` (kanonisch),
  `{knows: <actor>, fact: <id>}` (Encoder-Form) und die Autoren-Kurzform
  `{knows: <fact>}` (unverändert, ActorPlayer). Der Kurzform-Vertrag aus
  `docs/adventure-schema.md` bleibt gewahrt.
  **Regressionsschutz:** ein Roundtrip-Test über *jeden* Predicate-Konstruktor
  (`GameWorld -> JSON -> GameWorld`), ein Test auf die erhaltene YAML-Kurzform, ein
  **Spieltest**, der eine Aussage lernt und danach die Sichtbarkeit eines
  `visible_when: {knows: …}` prüft, sowie ein Test auf Byte-Stabilität der
  Welt-Datei über einen Serialize-Deserialize-Zyklus.
  **Byte-Auswirkung:** von 134 Artefakten weicht **ausschließlich**
  `examples_genres_detective/world.json` ab — die anderen 66 Abenteuer und Fixtures
  bleiben byte-identisch, weil keines die betroffenen Prädikatformen verwendet.
- **`actorId (ActorShip v)` liefert `"ship:" ++ v`** — die Doku führt `ship:<id>`
  seit längerem als Zielform (`docs/adventure-schema.md`), `actorId` lieferte aber die
  nackte Vehicle-ID. Keine Laufzeitkollision, weil `ActorShip` in den Effekten über
  Pattern-Matching aufgelöst wird; die Änderung gleicht Encoder und Doku an.
- **Variablen-Spiegel beim `learn:` (Spielzustand):**
  - Beim Erlernen einer Aussage via `learn: <id>` spiegelt die Engine Metadaten dynamisch in die VarMap:
    - `statement.<id>.truth` (`"true"` oder `"false"` als Text-Variable)
    - `statement.<id>.speaker` (Text-Variable)
    - `statement.<id>.claims` (Text-Variable)
  - `forget: <id>` räumt `known.<actor>.<id>` und alle gespiegelten `statement.<id>.*`-Variablen rückstandsfrei ab.
  - **Architekturentscheidung zur Wahrheit:** Die Wahrheit wird dynamisch beim `learn` gesetzt (Spielzustand). Begründung: Statische Weltvariablen sind nicht dynamisch über `compare_var`/`getVariable` erreichbar, und ein Vorbefüllen von `initialVars` würde den SaveState vor dem Verhör verschmutzen sowie nach `forget` bestehen bleiben.
  - **Auswertung:** `evalPredicate (CompareVar name op n)` und `resolveValueRef (VRVariable name)` interpretieren `"true"` als 1 und `"false"` als 0, sodass `compare_var` numerisch (`op: eq`, `value: 0`/`1`) und `{ var: statement.<id>.truth, is: "false" }` per Textvergleich auf den Wahrheitsgehalt prüfen können.
- **Autorenverantwortung statt Solver-Magie:**
  - Kein automatisches `contradicts:`-Prädikat, keine Auto-Lügenerkennung, kein Solver. Widersprüche und Konfrontationen sind Autorenarbeit via `visible_when` (`all: [{knows: ...}, {knows: ...}]`) und `compare_var`/`var: ... is:`.
  - Fakten und Aussagen bleiben getrennt: Aussagen erscheinen nicht im Notizbuch (`journal: notes`).
- **Compiler-Checks & Validator (`Worldbuilder.Compile`):**
  - Harte Diagnose `DuplicateStatement` bei mehrfacher Deklaration.
  - Harte Diagnose `StatementFactClash` bei Namenskollision zwischen `facts:` und `statements:`.
  - Harte Diagnose `UnknownFact` bei `learn:`, `forget:` oder `knows:` mit unbekannter ID (vereinheitlicht für Fakten und Aussagen).
  - Schutzregel `StatementVariableClash` gegen Autorenschreiben oder Deklarieren von `statement.*`-Variablen.
  - Schema-Prüfung: `checkUnknownYamlKeys` prüft `statements` mit `EntStatement` (inkl. Tippfehler-Vorschlägen).
  - **Validator-Bugfix für `checkUnknownPlaceholders`:** Platzhalter `{var: statement.*}` und `{var: known.*}` werfen keine ungerechtfertigten `UnknownPlaceholder`-Warnungen mehr.
- **Referenzgenre `examples/genres/detective.yaml`:**
  - Sieben Aussagen deklariert: `butler_alibi` (false), `butler_ledger` (true), `butler_knew` (true), `widow_letter` (true), `doctor_greeting` (false), `doctor_confront` (false, da gezielte Schutzbehauptung zur Diskreditierung des Stallburschen), `doctor_glove` (true, formal gültiges Argument mit Tag `argument`).
  - Dialogknoten und Trigger auf `learn: <statement-id>` umgestellt.
  - **Spielbarer Widerspruch:** Im Dialog mit dem Doktor erscheint bei Vorliegen von Butler- und Doktor-Alibi (`knows: butler_alibi` und `knows: doctor_greeting`) eine neue Konfrontations-Option, die über `if: { var: statement.doctor_greeting.truth, is: "false" }` den Argwohn (`suspicion`) des Verdächtigen steigert — spielbar VOR der eigentlichen Anklage.

### Content-Migration: Detektiv-Genre auf Wissensmodell (K13)

- **Migration von `examples/genres/detective.yaml`:**
  - Reine Content-Migration ohne Engine-Code-Änderungen: Das Detektiv-Genre nutzt nun das W1-Wissensmodell (`facts:`, `learn:`, `journal: notes`).
  - **Entfernung toter Flags:** Die vier Beweis-Flags (`note_found`, `ledger_found`, `letter_found`, `glove_found`) sowie das Dialog-Flag `knows_name` und das Abschluss-Flag `solved` wurden gesetzt, aber an keiner Stelle im Spiel gelesen (toter Ballast). Alle sechs Flags wurden ersatzlos entfernt.
  - **Fakten-Sektion (`facts:`):** Vier Beweise deklariert (`beweis_alibi`, `beweis_ledger`, `beweis_letter`, `beweis_glove`) mit Text aus den bisherigen `on_take`-Nachrichten, Fundort-Quelle (`source`) aus den Ortsbeschreibungen und Tags nach Beweisart (`alibi`, `motive`, `tat`).
  - **Effekte bei Aufnahme:** In `on_take` der vier Beweisstücke ersetzen `{learn: <fact-id>}` die alten Flags und `msg`-Einträge.
  - **Notizbuch:** Durch `journal: notes` steht der Befehl `notizen` (bzw. `notes`) zur Verfügung, der gesammelte Beweise nach Tag-Gruppen formatiert auflistet.
  - **Siegpfad und Spielmechanik unverändert:** Die Vierer-`has_item`-Bedingung für `accuse` und die Quests bleiben unverändert; der bestehende E2E-Siegpfad funktioniert identisch.
- **Unabhängig nachgemessen:** Der bestehende E2E-Lauf `ci/e2e/detective.in` ergibt
  weiterhin `VICTORY`. `notizen` nach dem Fund aller vier Beweise zeigt:
  `[alibi]` (1 Eintrag), `[motive]` (2), `[tat]` (1) — je mit `source`-Angabe in
  Klammern, in Deklarationsreihenfolge. Byte-Nachweis: von 134 Artefakten weicht
  **genau eine Datei** ab (`examples_genres_detective/world.json`), die 65 anderen
  Abenteuer und Fixtures sind byte-identisch.
- **Befund — was W1 nicht kann (der Grund für diese Stufe):** W1 speichert einen
  **Fakt**, keinen **Bericht über einen Fakt mit Urheber**. Die drei Aussagen des
  Genres — der Butler belegt die Ausrede, die Witwe identifiziert „R." als Doktor,
  der Doktor führt eine Schutzbehauptung — sind genau das, was `learn:` nicht
  abbildet: „der Doktor war nicht im Zimmer" und „der Butler sagt, der Doktor war
  nicht im Zimmer" sind verschiedene Dinge. `combine:` kann Diskrepanz zwischen
  gegensätzlichen Aussagen nicht ableiten, weil es das gleichzeitige Vorhandensein
  **wahrer** Prämissen verlangt. Und weil `accuse`/`visible_when` nur über
  `has_item`/`compare_var` prüfen, bleibt die Anklage an Gegenstände gebunden —
  ein Ermittler kann einen Verdächtigen nicht mit der *Aussage eines anderen*
  konfrontieren. Das ist die Begründung für einen möglichen Kandidaten
  `statements:` — hier **nicht** gebaut, nur festgehalten.

### Variablen-Zyklen: refill_per_turn, reset_on, on_overflow (K7/K4)

- **Erweiterung von `AVariable` und `VarDef`:**
  - `AVariable` (Worldbuilder) erweitert um `avbRefillPerTurn :: Int` (Default 0), `avbResetOn :: Maybe String` (Default Nothing) und `avbOnOverflow :: [AActionOutcome]` (Default []).
  - `knownKeys EntVariable` ergänzt um `"refill_per_turn"`, `"reset_on"` und `"on_overflow"`.
  - `VarDef` (Core) erweitert um `vdOnOverflow :: [Effect]`. Handgeschriebene `ToJSON`/`FromJSON`-Instanzen für `VarDef` lassen `vdOnOverflow` weg, wenn die Liste leer ist — garantiert Byte-Gleichheit aller bestehenden Abenteuer.
- **Compiler-Sugar für Variablen-Trigger (`Worldbuilder.Compile`):**
  - Pro Variable mit `refill_per_turn > 0`: erzeugt `TriggerDef` mit `trEvent = OnTurn`, ID `var.<name>.refill`, Effekt `ModifyValue (VRVariable <name>) delta`.
  - Pro Variable mit `reset_on: turn`: erzeugt `TriggerDef` mit `trEvent = OnTurn`, ID `var.<name>.reset`, Effekt `SetValue (VRVariable <name>) (EVInt max)`.
  - Pro Variable mit `reset_on: combat_start`: erzeugt `TriggerDef` mit neuem `trEvent = OnCombatStart`, ID `var.<name>.combatreset`, Effekt `SetValue (VRVariable <name>) (EVInt max)`.
  - **Reihenfolge-Garantie:** `reset_on`-Trigger werden deterministisch VOR `refill_per_turn`-Triggern eingereiht (`varResetDefs ++ varRefillDefs`). Dadurch wird das Budget am Rundenanfang zuerst zurückgesetzt, bevor die Regeneration aufschlägt.
  - **Byte-Vertrag:** Variablen ohne diese Deklarationen erzeugen 0 Trigger.
- **Review-Befund vor dem Commit (Opus 5.5, danach behoben):** Die erste Fassung
  ließ `fireTriggersWithDepth` laufen **und** sammelte die Effekte ein zweites Mal
  über `concatMap trEffects` — die Trigger liefen also doppelt, `once:`/`cooldown:`
  wirkten nicht (der `TriggerState` stand nur im verworfenen Lauf), und `chains_to`
  verlor seine Zustandsänderungen. Der korrigierte Stand macht **einen** Lauf, nutzt
  dessen Zustand und lässt das separate `matchingTriggers` entfallen.
- **Einmaligkeit ohne Byte-Risiko:** Das Kriterium war zunächst
  `combat.engaged == 0` — untauglich, weil `resolveClassic` (`Combat.hs:365`) und
  `resolveNarrative` (`:129`) **überhaupt keine Kampfvariablen** pflegen; nur
  `resolveTactical` tut das. Ersatz: ein eigenes VarMap-Flag `combat.started`, im
  gemeinsamen Einstiegspunkt gesetzt und über `checkCombatEnd` (`:116`) zurückgesetzt —
  das erkennt das Kampfende an **Meldungsschlüsseln und Effekten**, also
  profilunabhängig, statt an einer Variablen, die nur ein Profil führt.
- **Kampfstart-Ereignis `OnCombatStart` (`Combat` & `Types.Core`):**
  - Neues `EventType`: `OnCombatStart`.
  - In `resolveCombatEv` (`src/Combat.hs`): Feuert `OnCombatStart` genau beim Eintritt in einen Kampf (`not (isCombatStarted st)`), wenn passende Trigger existieren.
  - Wendet Trigger direkt über `fireTriggersWithDepth 0 OnCombatStart st` an und übernimmt den resultierenden `GameState` (`stWithTrig`). Keine redundante `startEffs`-Sammlung, keine doppelte Ausführung, `once: true`, `cooldown:` und `chains_to`-Zustandsänderungen bleiben erhalten.
  - Kampfgrenzen-Erkennung über `combat.started` in VarMap (mit Fallback auf `isCombatEngaged`) für alle drei Profile (Classic, Narrative, Tactical). Reset bei Kampfende (`checkCombatEnd` / `combat.started = 0`) und Raumwechsel (`transitionToRoom`).
  - Manuelle `matchingTriggers`-Vorabfilterung ersatzlos entfernt — `fireTriggerList` ist die alleinige Filterlogik.
  - Bei bestehenden Kämpfen ohne `OnCombatStart`-Trigger entsteht kein Overhead und die Bytes bleiben identisch.
- **Überlauf-Erkennung `on_overflow` (`src/Effects.hs`):**
  - Variante A: `clampToVarDef` in `src/Game.hs` bleibt eine reine Funktion.
  - In `applySetValueWithDepth` und `modifyValuePropWithDepth`: Prüfung nach dem Schreiben, ob der Wert echt größer als `max` war (`rawVal > max`) und daraufhin auf `max` geklemmt wurde.
  - Nur bei echtem Überschreiten feuern die Effekte aus `vdOnOverflow`. Ein Setzen auf genau `max` löst `on_overflow` nicht aus.
  - Schutz vor Endlosrekursion durch Erhöhung von `depth + 1` beim Aufruf von `applyOutcomeWith`, wodurch `maxOutcomeDepth` (20) greift.
- **Validierung (`Worldbuilder.Compile`):**
  - `reset_on` ohne `max` wird als harter Fehler `ResetWithoutMax` abgewiesen.
  - `reset_on` mit unbekanntem Ereignis (nur `turn` und `combat_start` erlaubt) wird mit `InvalidResetOn` abgewiesen.
  - `on_overflow` ohne `max` wird als `OverflowWithoutMax` abgewiesen.
  - Negatives `refill_per_turn` wird als `NegativeRefill` abgewiesen.
- **Fixture & E2E:**
  - Neue Fixture `examples/fixtures/zyklen.yaml` mit `tests:`-Sektion (10 Marker) und E2E-Playthrough `ci/e2e/zyklen.{in,expect}`.
  - Beweist im Spiel: AP-Budget über Runden, Regeneration pro Runde, Reset+Refill-Kombination mit Beweis der Reihenfolge (Reset vor Refill erzeugt Überladung) und `on_overflow` nur bei echtem Überschreiten.
  - In `scripts/ci.sh` Stufe 4 und 4b registriert.
- **Doku & Tests:**
  - `docs/adventure-schema.md`: Dokumentation der drei Felder, der Reihenfolgeregel, der Overflow-Bedingung und des Clamping-Hinweises.
  - Engine: 489 Tests in `test/Tests.hs` (+10: `testVariableRefillPerTurn`, `testVariableResetOnTurn`, `testVariableResetBeforeRefillOrder`, `testVariableResetOnCombatStart`, `testVariableOnOverflowGenuine`, sowie 5 Review-Tests: `testCombatStartExactlyOnceThreeAttacksAllProfiles`, `testCombatStartOnceRule`, `testCombatStartChainsToState`, `testCombatStartEffectsRunExactlyOnce`, `testCombatStartRandomChoiceDeterministic`), alle grün.
  - Worldbuilder: 268 Tests in `worldbuilder/test/Tests.hs` (+3: `testVarCyclesSugarTriggers`, `testVarCyclesYamlParsingAndKnownKeys`, `testVarCyclesValidation`), alle grün.
  - 58 E2E-Playthroughs in `scripts/ci.sh` (+1: `zyklen`).

### Dynamische Variablennamen (K12)

- **Laufzeit-Auflösung von Variablennamen (`Effects` & `Game`):**
  - `resolveVarName :: String -> GameState -> String`: Löst `{cmd.*}`-Platzhalter (z. B. `{cmd.arg1}`) im Variablennamen schlank zur Laufzeit gegen `getVariable` auf. Bleibt bei unauflösbaren Platzhaltern oder Namen ohne geschweifte Klammern unverändert (Byte-Gleichheit und Performance-Garantie).
  - In `src/Effects.hs`: Die beiden Schreibstellen für `VRVariable` (`applySetValue` und `modifyValueProp`) führen den Namen über `resolveVarName` vor dem Aufruf von `setScopedVariable`. Dadurch schreiben `compute_var`, `add_var` und `set_var` in den aufgelösten Schlüssel statt in den literalen Template-String.
  - In `src/Game.hs`: `lookupVarForFormat` führt nur dann eine Auflösung durch, wenn der Name `{` enthält. Bleibt nach der Auflösung ein Platzhalter unaufgelöst, wird deterministisch `Nothing` zurückgegeben (verhindert fehlerhafte Präfix-Aufteilungen an Punkten in `{cmd.argN}`).
- **Template-Klammerung (`Messages` & `Compile`):**
  - In `src/Messages.hs` und `worldbuilder/src/Worldbuilder/Compile.hs`: `matchBrace` bereinigt; aufeinanderfolgende schließende Klammern (`}}`) dekrementieren die Klammertiefe ordnungsgemäß (notwendig für geschachtelte Platzhalter wie `{var: npc.zuneigung_{cmd.arg1}}`).
- **Worldbuilder-Validierung (`Worldbuilder.Compile`):**
  - `checkUnknownPlaceholders` erkennt dynamische `{cmd.*}`-Platzhalter und `cmd.`-Variablen als bekannt an, sodass `{var: npc.zuneigung_{cmd.arg1}}` keine `UnknownPlaceholder`-Warnung mehr erzeugt. Normale unbekannte Platzhalter werden weiterhin zuverlässig gemeldet.
- **Wichtige Grenze:**
  - `expr:` ist ein AST (`Types.Core.parseExpr`) und wird bewusst **nicht** interpoliert. Zur dynamischen Wertanpassung dient `add_var` mit aufgelöstem Namen oder separate Regeln.
- **Fixture & E2E:**
  - Neue Fixture `examples/fixtures/zuneigung.yaml` mit `tests:`-Sektion (9 Marker) und E2E-Test `ci/e2e/zuneigung.{in,expect}`.
  - Beweist im Spiel: Ein Regelpaar, drei NPCs, ein Aufruf je NPC, dynamisches Nachschlagen und Schreiben sowie Auffinden einer vorher deklarierten statischen Variable über einen dynamischen Namen.
  - Registriert in `scripts/ci.sh` Stufe 4 und 4b.
- **Doku (`docs/adventure-schema.md`):**
  - Neue Sektion `### Dynamische Variablennamen (K12)` mit Funktionsweise, Beispielen und Dokumentation der AST-Grenze (`expr:`).
- **Tests & Metriken:**
  - Engine: 479 Tests in `test/Tests.hs` (+2: `testDynamicVarNameWrite`, `testDynamicVarNameFormat`), alle grün.
  - Worldbuilder: 265 Tests in `worldbuilder/test/Tests.hs` (+2: `testDynamicVarPlaceholderNoWarning`, `testNormalUnknownPlaceholderStillWarns`), alle grün.
  - 57 E2E-Playthroughs in `scripts/ci.sh` (+1: `zuneigung`).
- **Unabhängig nachgemessen:** `add_var: {var: "npc.zuneigung_{cmd.arg1}", delta: 15}`
  auf zwei vorher deklarierte NPCs — `freundlich baer` → −20 → **−5**,
  `freundlich fuchs` → 30 → **45**, eine Regel für beide. `status baer` und
  `status fuchs` nutzen denselben Text und zeigen verschiedene Werte.
- **Nebenbefund am Code (verhaltensneutral):** In `Messages.matchBrace` sind die
  Fälle für `{{` / `}}` entfallen. Sie waren toter Code — `formatStringWith`
  fängt Doppelklammern zwei Zeilen darüber ab (`Messages.hs:880`). Gegenprobe
  mit `A={{literal}} B={gold} C={{gold}} D=\{esc\}`: Ausgabe alt wie neu
  `A={literal} B=42 C={gold} D={esc}`.

### NPC-Verhaltenszustände & KI-Compiler-Zucker (K2)

- **`set_state:`-Zielauflösung & `UnknownStateTarget` (`Worldbuilder.Compile` & `Effects`):**
  - `AOSetEntityState` löst das Ziel nun nach `ActorNPC` auf, wenn das Ziel ein bekannter NPC ist, ansonsten `ActorEntity` (für Items, Container, Devices, Vehicles, ExitLocks).
  - In `src/Effects.hs`: `applySetValue` behandelt nun `VRActorProp (ActorNPC nid) PState` über die exportierte Funktion `setNpcStatusWithEvents`. Das setzt `npcStatus` des NPCs, feuert `OnStateChange` und erzeugt bei unbekanntem NPC eine Diagnose (`addDiagnostic`).
  - Neue harte Compiler-Diagnose `UnknownStateTarget`: Wenn ein `set_state:`-Ziel weder ein NPC noch eine bekannte Entität (Item, Device, Container, Vehicle oder Exit-Lock) ist, wird die Übersetzung mit `UnknownStateTarget` als harter Fehler abgewiesen (Tippfehler-Schutz).
  - Bestehende Verwendungen auf Items (`krypta.yaml`, `ascii-state.yaml` etc.) bleiben uneingeschränkt gültig und kompilieren ohne Befund.
- **Autoren-schreibbares Präfix `state.<npc>`:**
  - `state.` gehört zu den autoreneigenen Präfixen (`authorOwnedVarPrefixes = ["state."]`) und ist bewusst **nicht** in `reservedVarWriteRules` enthalten — Autoren dürfen `state.<npc>` per `set_var` (als Text) frei lesen und schreiben.
  - `engineVars` wird **nicht** pauschal mit `state.*` geflutet, um Platzhalter-Prüfungen auf ungültige Zustände nicht zu unterdrücken. Stattdessen erkennt der Validator `state.<npc>` für NPCs mit `ai:`-Block automatisch als bekannt an.
  - Bei NPCs mit `ai:` wird `state.<npc>` mit dem Namen des ersten definierten Zustands initialisiert (sofern nicht explizit über `initial_variables:` vorgegeben).
  - Saubere Rollentrennung: `npcStatus` (aus `set_state`) repräsentiert den Lebens-/Physiszustand (z. B. `alive`, `dead`, `asleep`; vom Kampf-/Engine-System genutzt); `state.<npc>` repräsentiert den Verhaltenszustand (z. B. `patrouilliert`, `flieht`). Ein `ai:`-Zustand überschreibt `npcStatus` nicht.
- **`ai:`-Compiler-Zucker für NPCs (`Worldbuilder.Compile`):**
  - Vollständige Übersetzung von `ai: { states: { <state_name>: { on:, when:, effects:, requires:, chains_to:, go_to:, once:, cooldown:, weight: } } }` in Standard-`TriggerDef`s:
    - Jeder Zustand wird zu einem Trigger `ai.<npc>.<state>` (geschützt über `reservedTriggerPrefixes`).
    - Standard-Events (z. B. `on: turn` oder implizit) erhalten als Bedingung `var: state.<npc>, is: <state_name>`.
    - Custom-Events (`on: { custom: <evt> }` oder `on: custom`) werden zu `OnCustomEvent`.
    - Kanonisches Adressierungsschema für Zustandsübergänge: `npc_ai_<npc>_<state>`.
    - `go_to: <ziel>` übersetzt in Effekte, die `state.<npc>` auf `<ziel>` setzen UND das Ziel-Event `npc_ai_<npc>_<ziel>` per `raise:` auslösen.
    - `requires:`, `chains_to:`, `once:`, `cooldown:`, `weight:` werden 1:1 auf die entsprechenden `TriggerDef`-Felder abgebildet.
    - `knownKeys EntNPC` akzeptiert nun `ai` sowie alle Zustandsfelder.
    - Kein Zustandsautomaten-Interpreter, kein Scheduler, kein Timer — 100% reguläre Trigger-Semantik.
- **Fixture `examples/fixtures/npc-zustaende.yaml` + E2E (`ci/e2e/npc-zustaende.{in,expect}`):**
  - Demonstriert vollständigen Spielablauf: Wache patrouilliert im Startzustand (`patrouilliert`), schlägt bei Sturmglocke über `requires: [sturmglocke_gelaeutet]` in `alarmiert` um, ketten-leitet per `chains_to: [npc_ai_wache_flieht]` in den Zustand `flieht` weiter, wechselt den Raum und schließt mit `set_state: { entity: wache, to: geflohen }` ab.
  - Verifiziert: State-Wechsel, Require-Gate (Hebel ohne Glocke zündet nicht), Weiterleitung über `chains_to`, `set_state` auf NPC.
  - Registriert in `scripts/ci.sh` Stufe 4 (E2E) und Stufe 4b (`worldbuilder test`).
- **Doku (`docs/adventure-schema.md`):**
  - `set_state:`-Tabelle um NPC-Ziele, `OnStateChange` und `UnknownStateTarget` erweitert.
  - `state.<npc>` als autorenschreibbares Präfix dokumentiert.
  - Neue Sektion `### KI-Zustände (ai:, K2)` mit Feldern, Übersetzung und Trigger-Semantik.
- **Tests & Metriken:**
  - Engine: 477 Tests in `test/Tests.hs` (+2: `testSetNpcStateFiresStateChange`, `testSetNpcStateUnknownNpcDiagnostic`), alle grün.
  - Worldbuilder: 263 Tests in `worldbuilder/test/Tests.hs` (+5: `testSetStateUnknownTargetFails`, `testSetStateItemCompiles`, `testSetStateNpcCompiles`, `testStateNpcWritableNoProtectionError`, `testAiCompilesToTriggerDefs`), alle grün.
  - Alle 64 bestehenden Abenteuer und 37 bestehenden Fixtures bleiben byte-identisch.

### Variablen-Schreibschutz-Vereinheitlichung & Doku (K6-Rest)

- **Schreibschutz vereinheitlicht (`Worldbuilder.Compile`):**
  - Eine zentrale Funktion `checkReservedVarWrites` (mit Rückwärtskompatibilitäts-Alias `checkRngVarWrites`) prüft alle reservierten Präfixe (`rng.`, `dice.`, `chapter.`, `combat.`) anhand deklarativer Regeln (`ReservedVarWriteRule`).
  - Erhalt der bestehenden Fehlercodes und wörtlichen Meldungstexte (`RngVarWrite`, `ChapterVariableClash`, `CombatVariableClash`).
  - Prüftiefe pro Präfix exakt beibehalten: `rng.` und `dice.` prüfen Schreib-Outcomes (`set_var`, `set_text_var`, `add_var`, `compute_var`), `variables:`, `initial_variables:` und `procedures:`; `chapter.` und `combat.` prüfen weiterhin ausschließlich `variables:`.
  - `checkCooldownConditionReserved` bleibt bewusst getrennt (Bedingungen statt Variablen).
- **Doku (`docs/adventure-schema.md`):**
  - In der Effect-Tabelle `set_var` für Ganzzahlen und Textvariablen dokumentiert.
  - Dokumentiert, dass `set_var` mit String-Wert Textvariablen schreibt, dass es bewusst kein eigenes `set_text_var` als Autoren-Form gibt („eine Vokabel, zwei Typen“), und welche Codes die reservierten Präfixe werfen.
- **Tests & Disziplin:**
  - Neuer Test `testReservedVariablesUnified` in `worldbuilder/test/Tests.hs` pinnt alle 4 Präfixe mit ihren Originalmeldungen und die unterschiedliche Prüftiefe ab.
  - 475 Engine-Tests, 258 Worldbuilder-Tests, 0 failed; CI grün, 0 Warnungen.
  - Alle 64 Abenteuer + Fixtures kompilieren byte-identisch.

### Event-Ketten und gewichtete Ziehungsfilter (K3)

- **Drei neue Felder auf der Regel-Entität / `TriggerDef` (K3.1):**
  - `weight` (Int, Default `1`): Gewicht bei gewichteter Ziehung (Weg B). Wirkt nur bei gewichteter Ziehung; bei gezielter Auslösung per `raise:` (Weg A) dient das Feld als Dokumentation/Reserve.
  - `requires` (`[FlagID]`, Default `[]`): Alle angegebenen Flags müssen gesetzt sein (`hasFlag`), sonst überspringt die Regel die Ausführung vorab. `tsFired` bleibt dabei `False` (die Regel gilt nicht als gefeuert und feuert beim nächsten Eintreten des Events erneut). Unbekannte Flags erzeugen eine weiche Warnung (`UnsatisfiableCondition`).
  - `chains_to` (`[String]`, Default `[]`): Liste von Custom-Event-Namen, die nach den Effekten über `fireTriggersWithDepth (depth + 1)` ausgelöst werden. Reiner Compiler-Sugar für `raise:`; unterliegt automatisch der Tiefenbegrenzung (`maxOutcomeDepth = 20`, Diagnose: `trigger nesting exceeded`). Unbekannte Chains-Ziele erzeugen einen harten Compile-Fehler (`UnknownChainTarget`). Ein Veto in einer Kette stoppt nachfolgende Events.
  - Handgeschriebene `ToJSON`/`FromJSON`-Instanzen: Bei Standardwerten (`weight == 1`, leere Listen) werden die Felder im kompilierte JSON weggelassen (Byte-Vertrag).
- **Vorab-Filterung unbedienter Gates bei gewichteter Ziehung (K3.2):**
  - Bewusste Änderung der Ziehungsregel (Nutzerentscheid 2026-10-03, Weg 1, kein Bugfix): Bei gewichteten Zufallsauswahlen (`random:` / `RandomChoice` und `RandomChoiceOn`) werden Kandidaten vor dem Aufsummieren der Gewichte über `drawEligible` gefiltert. Ein bedingter Kandidat (`Conditional p then Noop`, etwa durch `when:` in Encounter-Tabellen oder ein `if:` ohne `else:`), dessen Bedingung im aktuellen Spielzustand nicht erfüllt ist, verbraucht keinen Ziehungsslot mehr.
  - Konkretes Praxisbeispiel (`examples/modules/encounters.yaml`): Der Eintrag mit `when: { has_item: torch }` und Gewicht 1 lief vor K3.2 ohne Fackel als Niete ins Leere (`Noop`); jetzt wird der Slot ohne Fackel gar nicht erst vergeben, wodurch der Wolf in einem 8-Zug-Lauf nun 3x statt 1x erscheint.
  - Kein Draw bei vollständiger Unerfüllbarkeit: Sind alle Kandidaten einer Auswahl gefiltert, findet **kein Draw** statt — weder `rngState` noch ein benannter RNG-Strom (`rng.<name>`) werden weitergeschaltet, der Salt bleibt unverändert. Eine ergebnislose Ziehung verschiebt somit keine nachfolgenden Zufallsereignisse im Spielverlauf.
  - Echte Verzweigungen bleiben erhalten: Ein Kandidat mit `Conditional p t e` (echter `else:`-Zweig) behält seinen Slot im Pool, damit der vom Autor vorgesehene Alternativzweig erreichbar bleibt. Auch explizite `Noop`-Einträge (atmosphärische Leer-Slots) bleiben unangetastet.
  - Verschachtelte Bedingungen: Bei verschachtelten Conditionals (`Conditional p (Conditional q t Noop) Noop`) wird nur die äußere Klammer für die Slot-Berechtigung ausgewertet.
- **Fixture `examples/fixtures/ketten.yaml` + E2E (`ci/e2e/ketten.{in,expect}`) (K3.3):**
  - Dreistufige Kette (`start` -> `A` -> `B`): Regel 1 löst per `raise:` Custom-Event `kette_stufe_a` aus und setzt `siegel_aktiv`; Regel 2 hört darauf, verlangt per `requires: [siegel_aktiv]` das gesetzte Flag und löst per `chains_to: [kette_stufe_b]` Stufe 3 aus, welche die Pforte entriegelt.
  - Sichtbare `requires:`-Reihenfolge im Spiel: Vorzeitiges Auslösen von `kette_stufe_a` über ein Objekt verpufft wirkungslos (Regel 2 feuert nicht, `tsFired` bleibt False); erst nach Hebelbetätigung feuert die Kette vollständig durch.
  - K3.2-Verteilungsnachweis: Ein Kelch zieht aus `stream: kelchstrom` mit einem bedingten Kandidaten (Gewicht 9, `if: {has_flag: segen_aktiv}`) und einem unbedingten Kandidaten (Gewicht 1). Solange das Segens-Gate geschlossen ist, gewinnt der unbedingte Kandidat 100% der Ziehungen (Zähler steigt verlässlich); nach Aktivierung des Opfersteins tritt der bedingte Kandidat mit 90% Wahrscheinlichkeit in den Wettbewerb.
  - Zyklus-Tiefenbegrenzung: Selbsttriggernde Regel (`on: custom zyklus_puls`, `chains_to: [zyklus_puls]`) wird bei `maxOutcomeDepth = 20` sauber abgefangen (20 Echos, kein Absturz, kein Hang). Die Diagnose `trigger nesting exceeded` wird auf stderr ausgegeben und in `ci/e2e/ketten.expect` verifiziert.
  - In CI-Stufen 4 und 4b registriert (`run_e2e` und `worldbuilder test`). Fuzzer läuft mit 0 Befunden durch.
  - **Unabhängig nachgewiesen, dass der Fixture-Test die K3.2-Semantik wirklich pinnt** (nicht nur grün ist): dieselbe Fixture gegen den Vor-K3.2-Stand `d76c7f7` laufen lassen → `FAIL ... (marker not reached: "Der Kelch tropft silberhell: Sicherer Treffer. (Sicher: 1)")`. Über 40 Kelch-Züge: **ohne K3.2 nur 2 sichere Treffer** (die erwartete 1/10-Verteilung, 38 Ziehungen gingen als `Noop` ins Leere), **mit K3.2 40 von 40**.
- **Doku:** `docs/adventure-schema.md` um Attribut-Tabelle für die Regel-Entität (`weight`, `requires`, `chains_to`) und K3.2-Ziehungssemantik ergänzt.
- **Disziplin:** Kein neues `SaveState`-Feld (Regel 6), kein Scheduler, kein eigener Zustandsspeicher. Alle bestehenden 63 Abenteuer und Fixtures kompilieren byte-identisch.
- **Tests:** `testTriggerRequiresGates`, `testTriggerChainsToFiresFollower`, `testRuleRequiresUnknownFlagWarning`, `testRuleChainsToTargetValidation`, `testRandomChoiceUnmetGateOverSalts`, `testRandomChoiceAllGatedNoDraw`, `testRandomChoiceOnAllGatedStreamUntouched`, `testRandomChoiceBranchKeepsSlot`, `testRandomChoiceNestedConditionalOuterOnly`, `testRandomChoiceOnSkipsUnmetGate`. 475 Engine-Tests, 257 Worldbuilder-Tests, 0 failed; CI grün, 0 Warnungen.

### Würfelpool `roll_dice:` (K1)

- **Neues Autoren-Outcome `roll_dice: {pool, die, stream?, keep?}`** — zieht `pool`
  Würfel mit `die` Seiten und schreibt die Ergebnisse in die VarMap:
  `dice.last_roll` (Text, die **behaltenen** Werte kommagetrennt), `dice.count`
  (Int), `dice.highest` (Int), `dice.sum` (Int). `keep` behält nur die höchsten
  `keep` Würfel und reduziert **alle** Aggregate; `stream` zieht aus einem
  benannten Strom (B8), leer = Default-Strom mit unveränderter Ziehungsfolge.
  Der Wurf ist **still** (keine Ausgabe), wie `compute_var` — die Anzeige macht
  der Autor mit `{var: dice.highest}`.
- **Kein neuer Wert-Typ, keine neue Algebra:** `dice.highest` & Co. sind ganz
  normale Variablen; der Autor vergleicht mit `compare_var` wie bei `sanity`
  oder `energy`. Ausgewertet werden kann der Wurf auch ganz ohne die Aggregate,
  über `{var: dice.last_roll}` plus `compute_var` auf `dice.sum`.
- **Kein neues `SaveState`-Feld** (Regel 6): alles über die VarMap, wie B8.
- **Harte Compile-Fehler:** `InvalidDicePool` (`pool < 1`), `InvalidDiceSides`
  (`die < 2`), `InvalidDiceKeep` (`keep < 0` oder `keep > pool`) — je mit Pfad
  und Fundstelle.
- **Reservierte Präfixe generalisiert** (B8 war auf das Literal `rng.` verdrahtet):
  jetzt `reservedPrefixes = ["rng.", "dice."]`. Die bestehende
  `RngVarWrite`-Meldung für `rng.*` bleibt wörtlich erhalten; `dice.*` bekommt
  eine generische Variante. Autoren-Schreibzugriff auf beide Namespaces ist
  unverändert ein harter Fehler.
- **`dice.*` gilt dem Validator als bekannt** (Nutzer-Entscheid 2026-10-03,
  Variante A): `{var: dice.last_roll}` im Raumtext erzeugt **keine**
  Platzhalter-Warnung, ohne dass der Autor eine `variables:`-Deklaration
  braucht. Ein Warningsystem soll nicht auf einen Wert warnen, den die Engine
  garantiert schreibt. Pinned durch `testDicePlaceholderNoWarning`.
- Tests: `testRollDicePool` (fester Startzustand, `keep`-Teilmenge, Leerfall,
  Event-Freiheit, benannter Strom lässt den Default-Strom unberührt),
  `testRollDiceValidation` (die drei harten Fehler + Autoren-Schreibschutz +
  Parse), `testDicePlaceholderNoWarning`. **255 Worldbuilder-Tests, 0 failed.**
- **Byte-Vertrag:** alle **62** gelieferten Abenteure byte-identisch — 27
  Stichproben (demo, thefog, 10 Genres, 15 `modules/`-Dateien) zu je `world.json` +
  `save.json` gegen einen Worktree auf `ecf89b0` verglichen, `diff -ru` ohne
  Befund. Die volle Zahl 62 (inkl. der 36 Fixtures) steht im
  K1-Plan-Abschnitt „Byte-Nachweis"; die CI-Stufen kompilieren alle. CI grün,
  0 Compiler-Warnungen.
- **Fixture `examples/fixtures/wuerfel.yaml` + E2E (`ci/e2e/wuerfel.{in,expect}`) (K1.2):**
  Skill-Check über `roll_dice: {pool: 2, die: 6}` und `compare_var` auf `dice.highest >= 4`
  (schaltet bei Erfolg die Kammertür frei, ohne neue Vokabel), zweites Outcome mit
  benanntem RNG-Strom (`stream: orakel`, `keep: 2`), Raumbeschreibung mit
  `{var: dice.last_roll}` ohne `variables:`-Deklaration (Beweis für Variante A),
  `tests:`-Sektion mit 8 geordneten Markern (B1-Muster). In CI-Stufen 4 und 4b registriert.
- **Engine-String-Interpolation:** `formatStringWith` toleriert nun Whitespace nach
  `var:`, damit `{var: dice.last_roll}` zur Laufzeit identisch auflöst wie im
  Worldbuilder-Validator.
- **Doku:** `docs/adventure-schema.md` um `roll_dice`-Zeile in der Effekt-Tabelle und
  ausführlichen Abschnitt mit Parametern, Aggregaten, Schreibschutz und Variante A ergänzt.
- **K4-Kleinkram (K1.3, Doku statt Code):** Neuer Abschnitt „Wertebereiche (`min` / `max`)"
  in `docs/adventure-schema.md`. `min`/`max` waren im Schema undokumentiert und konnten
  Dekoration sein — sie sind es nicht: `clampToVarDef` klemmt jeden Schreibvorgang auf
  eine `int`-Variable zentral (`src/Game.hs:1211`), unabhängig vom Effekt, und greift
  daher auch auf engine-geschriebene Variablen wie `dice.count`. Der Abschnitt sagt
  ausdrücklich, was die Klammer **nicht** leistet: ein `on_overflow`-**Ereignis** gibt
  es weiterhin nicht — das bleibt K4.
- **K1 abgeschlossen.** Alle drei Stufen umgesetzt, CI grün, 0 Compiler-Warnungen.

### Projekt-View und Quest-Diagnostik (4.6, S3)

- **`worldbuilder map <adventure.yaml> [-o map.json] [-] [--width N]`** — die
  Projekt-View als Text (ein Raster je `floor:`, Zeilen mit `y=`, Spalten mit
  `x=`, `*` vor vom Autor gesetzten Positionen) und als JSON. Das JSON folgt
  `docs/protocol-v1.md`: snake_case, `version`-Feld, `encodeSorted` (Keys auf
  jeder Tiefe sortiert) und **kein Weltzustand** — nur, was das Abenteuer
  deklariert, plus das aufgelöste Layout. Zwei Läufe sind byte-identisch, ein
  Diff zwischen zwei Commits zeigt genau die Autorenänderung.
  Inhalt: Räume (id, name, x, y, floor, `source: authored|layout`), Kanten (from,
  to, direction, `locked`, `guard`, `guard_holds`), Erreichbarkeit, Quests (id,
  name, prereqs, stages, on_complete, `startable`, `progressed`), Issues.
- **Das Raster lügt nicht:** die Zellbreite wächst auf die längste ID plus eins
  (`loc_1` und `loc_17` unterschieden sich sonst nicht mehr), Zeilen tragen ihr
  `y`, nur **echte IDs** stehen im Feld — Namen wären Prosa, keine Koordinaten.
- **Zwei neue weiche Quest-Befunde** im B4-Kanal (konservativ, `SWarning`):
  - `QuestNeverStarted` — nichts startet die Quest: kein `start_quest:` und kein
    `on_complete:` auf sie (transitiv über Ketten).
  - `QuestNeverProgressed` — startbar, aber weder `advance_quest:` noch
    `complete_quest:` zielt auf sie; der Spieler sähe Stufe 0 für immer.
  - **Bewusst *nicht* neu gebaut:** ein Prereq-Flag, das nie gesetzt wird. Das
    ist bereits der harte `UnknownQuestPrereq` aus S0 — eine zweite, weichere
    Formulierung desselben Befunds wäre nur Lärm (Plan 5.5 sagt das ausdrücklich).
  - **Erster Fund im Bestand:** `examples/demo.yaml` → `find_treasure` startet
    beim Nehmen des Schlüssels und hat zwei Stufen plus `reward:`, aber keinen
    einzigen Fortschritts-Effekt. Die Quest bleibt auf Stufe 0, „Quest
    complete!" feuert nie. **Nicht angefasst:** ein geliefertes Abenteuer zu
    ändern, bricht die Byte-Zusage; die Warnung stehen zu lassen ist der
    ehrlichere Zustand und der Beweis, dass der Kanal arbeitet.
- Neu: `Worldbuilder.QuestCheck` (nimmt die Outcomes als Parameter — `allAOutcomes`
  gehört `Worldbuilder.Compile`, das dieses Modul braucht; eine zweite
  Outcome-Sammlung hieße eine zweite Wahrheit) und `Worldbuilder.ProjectView`.
  `unreachableRoomIds` und `predicateTruth` wurden aus `Compile` herausgelöst,
  damit die View die Erreichbarkeit **wiederverwendet** statt sie zu duplizieren.
- Tests: `testQuestDiagnostics` (inkl. „Kette ist ein Startweg" und „reiner Zyklus
  ist toter Inhalt"), `testProjectView` (Struktur, Deklarationsreihenfolge,
  Byte-Stabilität, sortierte Keys), `testMapGridTellsRoomsApart`. CI-Stufe 10 prüft
  `map` und `map-set` gegen die echte Datei.
- **Dokumentierte Selbstkorrektur:** der Kommentar im Code nannte die
  Startweg-Berechnung „größter Fixpunkt". Sie ist die transitive
  Vorwärtsreichweite; ein reiner `a: b` / `b: a`-Zyklus ohne `start_quest:` ist
  damit **nicht** startbar und wird gemeldet — was stimmt, denn kein Ereignis
  erreicht ihn. Der größte Fixpunkt hätte jede Quest für startbar gehalten und
  gar nichts gemeldet.

### Kartenpositionen `map:` + Auto-Layout (4.6, S2)

- **Neues Autorenfeld `map: {x: n, y: m}` am Raum.** Reine Raster-Kosmetik für
  einen Karteneditor — kein Weg, kein Kampf und kein Befehl liest sie.
  `roomMapPos` steht im `world.json` **nur, wenn `map:` gesetzt ist**
  (Byte-Vertrag wie `roomIntro`/`roomFloor`): alle 62 gelieferten Abenteure
  bleiben byte-identisch.
- **`floor:` ist die z-Achse.** Räume mit verschiedenem `floor:` liegen auf
  getrennten Rastern; gleiche Zelle auf derselben Ebene ist der harte Fehler
  `MapOverlap` (nennt beide Räume, mit Fundstelle).
- **Auto-Layout** (`Worldbuilder.MapLayout`, reine Funktion über das Autoren-
  Adventure): Zeile = BFS-Tiefe ab `start_room`, Spalte = Reihenfolge in der
  Schicht (Richtungen kanonisch, also `north` vor `south`), gesetzte `map:`-Räume
  sind **Anker** und werden nie überschrieben, nicht erreichbare Räume kommen in
  eine eigene Zeile darunter — in **Deklarationsreihenfolge**, nie in
  `Map`/`Ord`-Reihenfolge (Projektregel 9). Das Layout lebt nur im Worldbuilder
  und wandert **nicht** in die Welt; die Welt kennt nur die gesetzten Positionen.
  Gemessen an den gelieferten Abenteueren: `fantasy.yaml` wird 14 Zeilen × 3
  Spalten, `thefog.yaml` 14 × 8 — die Einschätzung aus dem Plan trifft zu.
- **`worldbuilder map-set <datei> <raum> <x> <y>`:** der erste echte Anwendungs-
  fall des S1-Schreibers. Fehlt `map:`, wird der Schlüssel am Ende des
  Raum-Blocks in der Einrückung des Blocks eingefügt; vorhandene Koordinaten
  werden ersetzt (Flow- und Block-Form). Kommentare, Reihenfolge und Format
  bleiben byte-treu; Verweigerungen (unbekannter Raum, nicht ganzzahlig,
  Flow-Raum-Mapping) schreiben **nicht**.
- Tests: `testRoomMapPosRoundTrip`, `testMapLayout`, `testMapOverlapIsAnError`,
  `testMapSetInsertsAndReplaces`. Content: `examples/fixtures/karte.yaml`
  (gepinnter Gipfel, eigene Ebene im Verlies, 2 `tests:`-Abschnitte) +
  `ci/e2e/karte.{in,expect}` in CI-Stufen 4/4b.

### W5 Stufe 2: exakte YAML-Positionen und ein positionstreuer Schreiber (4.6, S1)

- **Neues Modul `Worldbuilder.YamlDoc`:** die Datei wird ein zweites Mal als
  `Node Pos` gelesen (`Data.YAML.decode1` hat eine `FromJSON (Node Pos)`-Instanz,
  jeder Knoten trägt `posLine`/`posColumn`/`posByteOffset`) — **ohne** neue
  Abhängigkeit und **ohne** Eingriff in den `FromJSON`-Compile-Pfad. Damit hat
  jeder dotted Issue-Pfad (`rooms.hall.exits.north`, `items.fass.keys[0]`,
  `interactions.npc[verband]`) eine exakte Fundstelle.
- **`printCompileIssues` nimmt erst die exakte Position und fällt dann auf die
  Stufe-1-Heuristik (`Worldbuilder.Locate`) zurück** — unverändert für Pfade,
  die keinen Knoten benennen (synthetische Felder wie `outcomes.set_exit.to`) und
  für Parse-Fehler. Gemessen über alle gelieferten Abenteure, 53 859 dotted
  Pfade: beide lösen für 3 634 (3 455 gleiche Zeile, **179 verschiedene**), nur
  exakt 497, nur Locate 13 109 (das sind Pfade, die es gar nicht gibt — Locate
  erfand dafür eine Zeile). In **allen** geprüften Abweichungen hat die exakte
  Position recht: Locate zeigte auf den Sektions-Header (129 Fälle) oder auf
  einen völlig anderen Treffer (50 Fälle). Beispiel `sprache.yaml`, Fehler in
  `rooms.garten`: **vorher** `line 61` (ein `garten:`-Topic beim NPC, 42 Zeilen
  daneben), **nachher** `line 19` (die `rooms.garten`-Zeile selbst).
- **Schreibende Hälfte:** `setScalarAt` ersetzt einen **einfachen** Skalar und
  lässt alle anderen Bytes stehen — Kommentare, Schlüsselreihenfolge, Format,
  Leerzeilen. Der Idempotenz-Vertrag (`set(pfad, alter Wert) == Original`) ist
  gepinnt. Verweigert wird, was nicht sicher geht, jeweils mit benanntem Grund
  und Exit 1, **ohne** die Datei anzufassen: Map/Sequenz, quotierte Skalare,
  Block-Skalare (`|`, `>`), über mehrere Zeilen gefaltete Plain-Skalare und
  Werte mit Zeilenumbruch. (Ein gefalteter Wert still zu ersetzen hieße, ihm
  seine Fortsetzungszeilen zu lassen — der Wert änderte sich ohne, dass jemand
  es merkt.)
- **Neue CLI-Kommandos:** `worldbuilder yaml-pos <datei> <pfad>` (Position
  nachschlagen, zum Vergleichen mit der Heuristik) und
  `worldbuilder yaml-set <datei> <pfad> <wert>` (einen Skalar in-place
  ersetzen). Der Map-Editor in S2 setzt darüber `map:`-Koordinaten.
- **Kein Byte-Change:** alle 61 gelieferten Abenteure kompilieren byte-identisch
  (die neue Diagnosezeile ändert nur Meldungen, keine Artefakte). 3 neue Tests
  (246 Worldbuilder-Tests), CI grün, 0 Warnungen.

### Quest-Kette `on_complete:` (4.6, S0) + harte Quest-Referenzen

- **`on_complete:` wird jetzt gelesen — vorher fiel das Feld still weg.** Der
  Schluessel stand in `docs/adventure-schema.md` in der Quest-Tabelle, aber
  `AQuest` hatte kein Feld dafuer: der Wert wurde verworfen, und `knownKeys`
  meldete ihn sogar als unbekannten YAML-Schluessel (`UnknownYamlKey`). Jetzt
  parst der Compiler ihn, `Quest.questOnComplete` traegt ihn, und das Feld
  steht im `world.json` **nur, wenn es gesetzt ist** (Hand-`ToJSON`; alle
  bisherigen Schluessel bleiben unveraendert, `questReward: null` bleibt
  wie gewohnt stehen).
- **Verdrahtet:** ist eine Quest abgeschlossen, startet `on_complete:` die
  Folge-Quest an Stufe 0 — **nach** der Belohnung der abgeschlossenen Quest
  (die Belohnung darf Flags setzen, die die Folge-Quest als Voraussetzung
  braucht). Startet die Folge-Quest nicht (Voraussetzung nicht erfuellt), ist
  das **still**: es ist ein Zustandsfall und kein Spielereignis, also kein
  neuer Katalog-Key und keine Ausgabeaenderung. Die Kette ist genau eine
  Ebene tief — eine abgeschlossene Quest kann sich nicht selbst neu starten.
- **Harte Compile-Fehler `UnknownQuestEffect` / `UnknownOnComplete`:** ein
  Tippfehler in `start_quest:`/`advance_quest:`/`complete_quest:` oder in
  `on_complete:` faellt jetzt beim Kompilieren auf, mit Pfad und Fundstelle
  (verschachtelte `if:`/`random:`-Zweige eingeschlossen). Die Welt-Validierung
  meldete dieselbe Klasse schon als `MissingQuest`, aber erst nach dem
  Kompilieren, ohne Fundstelle — und `--force` schrieb trotzdem.
- **Kein neues `SaveState`-Feld:** die Kette lebt in `activeQuests`, das es
  schon gibt. Wirkt an der einen Stelle (`Quests.completeQuestWith`).
- Tests: `testQuestOnCompleteStartsChain`, `testQuestOnCompleteJsonOmission`,
  `testQuestOnCompleteParsed`, `testQuestRefErrors`. Content:
  `examples/fixtures/quest-kette.yaml` (zwei Kettenstufen, 2 `tests:`-Abschnitte)
  + `ci/e2e/quest-kette.{in,expect}` (CI-Stufen 4/4b).

### Fehlalarm der Validierung bei `initial_flags:` (4.6, S0)

- **Befund (gemessen):** ein Abenteuer mit `initial_flags: { started: "true" }`
  und einer Regel mit `when: { has_flag: started }` wurde mit
  `MissingSetFlag "started" "checked but never set in any outcome"` **und Exit 1**
  abgelehnt; `compile` schrieb ohne `--force` nichts. Dieselbe Fehlmeldung traf
  Quest-Voraussetzungen (`UnknownQuestPrereq`), die `initial_flags` erfuellt.
- **Ursache:** beide Checks fragen „wird dieses Flag je gesetzt?", und zaehlen
  dabei nur Effekte, Bedingungstexte und Licht-Flags der **Welt**. Die
  Start-Flags liegen aber im **Save**, nicht in der Welt — sie waren unsichtbar.
- **Fix:** `validateWorldWithFlags` nimmt die Start-Flags entgegen
  (`validateWorld` bleibt als save-blinde Variante erhalten, mit leerer Menge),
  die CLI reicht die Flags aus dem kompilierten Save durch, und
  `checkQuestPrereqFlags` zaehlt die Save-Flags mit. Damit ist die Sorglos-
  Frage wieder ehrlich: wirklich nie gesetzte Flags melden weiter (der
  Engine-Test `testRuleFlagCheckedButNeverSet` pinnt das).

### Fallenlassen beim NPC-Tod (B9, Teil 3) — **B9 abgeschlossen**

- **Neues NPC-Feld `drops_on_death: true`:** die Leiche laesst ihre **getragenen
  und ausgeruesteten** Items im Raum liegen, in dem sie steht (Katalog-Zeile
  `npc.drops_items`). Getragen ist getragen — `take all from <npc>` findet die
  Beute danach nicht mehr, sie liegt ganz normal im Raum.
- **Default ist „die Leiche behaelt alles“**: byte-gefroren (die Engine hat nie
  automatisch fallen gelassen), also aendert sich kein bestehendes Abenteuer.
  Das Flag steht im `world.json` nur, wenn es gesetzt ist (NPCDef-Ausgabe
  laesst leere Felder weg), und ist in `knownKeys EntNPC` eingetragen.
- Wirkt an der **einen** Stelle (`killNPCWithMsg` → `dropOnDeath`), damit
  jeder Todesweg (Kampf, `damage_npc`, `set_state: … dead`, `unlock`) dieselbe
  Regel hat. Ein zweiter Tod ist ein No-op (kein Doppel-Drop).
- Tests: `testNpcDropsOnDeath` (Default behaelt, Flag laesst fallen, eigene
  Zeile, zweiter Tod still, leere Habe still, Ende-zu-Ende ueber den echten
  `attack`-Befehl, JSON-Auslassung + Round-Trip) + `testDropsOnDeathFlag`
  (Compile, JSON mit/ohne Flag, YAML-Front ohne `UnknownYamlKey`).
- Content: `examples/fixtures/npc-tod.yaml` (Bandit mit Flag, Kellner ohne —
  derselbe Lauf zeigt beide Seiten, 3 `tests:`-Abschnitte) +
  `ci/e2e/npc-tod.{in,expect}` (CI-Stufen 4/4b).

### NPC-Ausruestung (B9, Teil 2)

- **Neue Autorenform `give: {item: X, to: <npc-id>, equip: true}`** — eine
  Auspraegung des B7-`give`-Zuckers, **kein** neues Spieler-Kommando: der NPC
  traegt das Item danach (`EquippedBy (ActorNPC …)`). Ohne `to` zielt die Form
  wie `give:` auf den Spieler; `equip: false` bleibt `CarriedBy` (der Bool wird
  benutzt, nicht verworfen — ein `AOFoo <$ (o .: "key")` wuerde `false`
  stillschweigend akzeptieren). Unbekanntes Ziel bleibt `UnknownNpc`.
- **Zustand bleibt in der Item-Location** (kein neues `SaveState`-Feld), der
  **Slot** kommt aus dem Item (`slot:`). `MoveEntity (EquippedBy actor)`
  respektiert jetzt den Actor (`equipItemFor`): der Spielerpfad mit der
  `equipment`-Map ist byte-veraendert **nichts**, ein NPC traegt das Item als
  Location. Der Slot-Konflikt gilt **pro Actor** (`wornInSlot` als einzige
  Definition von „Slot belegt"; der zweite Waechter im gleichen Slot ist kein
  Konflikt).
- **Der Boni wirkt im Kampf:** `npcAttackWith`/`npcDefenseWith` ersetzen an
  jeder bisherigen Direktstelle das Lesen von `npcAttackBase`/`npcDefenseBase`
  (narrative, tactical, classic, Begleiter, Schiff) — mit Ausruestung addieren
  `effects: [attack+N]`, ohne Ausruestung bleibt der Bonus 0, bestehende
  Laeufe byte-identisch.
- **Sichtbarkeit:** neue Katalog-Zeile `npc.wears` („Wearing: …“) in
  `look at <npc>` neben der B7-`Carrying:`-Zeile und im Protokoll als
  `NpcSummary.equipped` (leer = Feld weggelassen). Katalog 232 → **233** Keys,
  de-Paket synchron (CI 2c `--require-complete`).
- **Doku-Fund (mitgefixt):** die Item-Tabelle nannte die Felder
  `equip_slot:`/`equip_effects:`; geparst werden `slot:`/`effects:`.
- Tests: `testNpcEquipment` (Location, Slot-Konflikt, nicht ausruestbar,
  pro-Actor-Slot, Spielerpfad byte-gleich, Kampfzahlen, `look at`, Snapshot,
  JSON-Auslassung) + `testNpcEquipmentAffectsCombat` (der gemeldete Schaden
  sinkt um genau den Ruestungsbonus) + `testGiveEquipSugar` (5 YAML-Formen,
  Bool-Sugar, Ref-Fehler).
- Content: `examples/fixtures/npc-ausruestung.yaml` (4 `tests:`-Abschnitte:
  Wearing-Zeile, Slot-Konflikt, verschiedene Slots, kein Wearing) +
  `ci/e2e/npc-ausruestung.{in,expect}` (CI-Stufen 4/4b).

### `take all from <npc>` (B9, Teil 1)

- **Neue Massen-Operation an Figuren:** `take all from <npc>` nimmt alles, was der
  NPC getragen hat — eine `npc.took_from`-Zeile pro Item, dieselbe Idiom wie
  `take all` im Raum. **Kein neuer Meldungsschluessel** (Katalog bleibt 232),
  **kein** neuer `Command`-Effekt fuer Autoren, **kein** neues `SaveState`-Feld.
- Feste Wirkung, keine Autoren-Effektliste als Schleifenkörper (B3-Linie): das
  Inventarlimit gilt pro Item (`inventory.full`, der Rest bleibt beim NPC),
  ausgeruestete Items des NPCs bleiben auf ihm (sie sind nicht „in seinen
  Haenden“), ein toter NPC bleibt ein gueltiger Traeger.
- Der gemeinsame Einzelpfad wurde in `takeItemFromNpc` gezogen — `take X from
  <npc>` (B7) liefert byte-identische Ausgabe (gleiche Guards, gleiche Args).
- Neuer Kommando-Konstruktor `TakeAllFromCmd` mit L13-Urteil (kostet einen Zug,
  wie `take all`); `extractCommandArgs` schreibt `cmd.verb = take`,
  `cmd.target = all <npc>`. Ein unbekanntes Ziel meldet wie beim Einzelgriff
  `container.not_a_container` (gleicher Aufrufzweig).
- L13-/Parser-Fallstrick: `splitPrep` verlangt Text **vor** der Praeposition —
  dafuer `afterPrep` (B9).
- Beispiel `examples/fixtures/npc-massen.yaml` (2 Items im Besitz, 1 im Raum,
  2 `tests:`-Marker) + `ci/e2e/npc-massen.{in,expect}` (CI-Stufen 4/4b).

### Item-auf-NPC-Interaktionen (B9)

- **Neue Zielart `interactions: npc:`:** `use <item> on <npc>` laesst die
  deklarierte Effektliste laufen (Heilen, Bestechen, Fesseln, Ausgeben … als
  Content — die Engine kennt weiter nur `use`). Der Eintrag greift **vor** dem
  Angriffs-Fallback: ohne Eintrag bleibt `use` auf eine lebende Figur ein
  Angriff (unveraenderter Vertrag, per Nutzer-Entscheidung so festgehalten).
  Das Item bleibt in der Hand, ausser ein Effekt bewegt es (`consume:`, `give:`).
- **Harte Compile-Fehler** fuer beide Seiten eines Eintrags: unbekanntes Item
  (`UnknownNpcInteractionItem`) und unbekannte Figur (`UnknownNpc`) — ein
  Tippfehler im Item-Namen wuerde sonst still angreifen.
- **Byte-Vertrag unveraendert:** `npcInteractions` wird in `world.json` nur
  geschrieben, wenn das Abenteuer Eintraege hat (gleiche Regel wie `procDefs`),
  und `Location`/`ActorRef` koennen NPCs schon als Ziel — **kein** neues
  `SaveState`-Feld. `entity:` und `item:` bleiben unangetastet.
- Beispiele: `examples/fixtures/npc-interaktion.yaml` (deklariertes Paar laeuft,
  verbrauchtes Item, Angriffs-Fallback beim nicht deklarierten Paar, `tests:`-
  Sektion) + `ci/e2e/npc-interaktion.{in,expect}` (CI-Stufen 4/4b); Doku in
  `docs/adventure-schema.md`.
- Offen im B9-Rest: nichts — `take all from <npc>` (Teil 1), NPC-Ausruestung
  (Teil 2) und Fallenlassen beim Tod (Teil 3) sind erledigt.

### Sprachpakete: Beispiel-Abenteuer & Genre-Demo (4.3, Teil 6)

- `examples/fixtures/sprache.yaml`: durchspielbares Mini-Abenteuer mit
  `language: de`, `messages:`-Override (`take.ok` mit `{article_acc}`),
  Grammatikfeldern an allen Items/NPCs, deutschen Eingaben (`nimm`/`schliesse`/
  `oeffne`/`gib … an …`/`nord`/`inventar`) und einer `tests:`-Sektion
  (7 deutsche Marker). E2E `ci/e2e/sprache.{in,expect}` (CI-Stufen 4/4b).
- `examples/genres/fantasy-de.yaml`: deutsche Fantasy-Variante auf Basis von
  `fantasy.yaml`, reduziert auf den Kernpfad bis zum Sieg um die Glutkrone —
  Realismus-Nachweis fuer Sprachpaket + Grammatikfelder + deutsche Eingabe in
  einem Genre-Abenteuer (P4-Entscheidung). E2E `ci/e2e/fantasy-de.{in,expect}`
  (CI-Stufe 4), additiv ohne Byte-Effekt auf die bestehenden Genres.
- `docs/message-catalog.md` zeigt die Katalog-Templates jetzt en/de
  nebeneinander (`scripts/gen-msg-catalog.py` erweitert); `lang/README.md`
  dokumentiert das Paketformat, die Werkzeuge und das Anlegen neuer Sprachen.

### Grammatikfelder `article:`/`gender:` (4.3.5, Teil 5)

- **Neue optionale Felder an Items und NPCs** (Variante A aus dem
  Sprachpakete-Plan): `article: "der"` (Kurzform = nur Nominativ) oder
  `article: {nom: "der", acc: "den", dat: "dem"}` plus `gender: m|f|n`
  (geschlossene Wertemenge, unbekannt = Compile-Fehler `InvalidGender`).
  Alles freier Text — **keine** Deklinationstabellen, keine Grammatiklogik in
  der Engine; der Autor steuert auch Sonderfaelle exakt. In `world.json`
  werden beide Felder bei leer weggelassen (Byte-Vertrag).
- **Platzhalter fuer Katalog-Templates:** `{article_nom}`/`{article_acc}`/
  `{article_dat}`/`{gender}` begleiten die Praimar-Entity einer Meldung,
  dazu pro Argument-Slot `{<slot>_article_*}`/`{<slot>_gender}` (z. B.
  `{item_article_acc}`/`{name_article_dat}` bei `container.put`). Verdrahtet
  an den Take/Give/Drop/Use/Equip/Search/Container-Call-Sites; die deutschen
  Sprachpaket-Templates nutzen sie jetzt. Fehlende Felder: der Platzhalter
  rendert als **leerer String** (gepinnt, nie `<msg:…>`/`<error:…>`).
- **Compiler-Warnung `MissingGrammar`:** in Sprachpaket-Welten (`language:`
  gesetzt) warnt der Compiler fuer Items/NPCs ohne Grammatikfelder, sobald
  die wirksamen Templates Grammatik-Platzhalter referenzieren (konservativ —
  artikellose Template-Ueberschreibungen schalten das ab).
- 5 neue Engine-Tests (457), 4 neue Worldbuilder-Tests (235).

### Sprachpakete: Alias-Paket & Eingabe (4.3, Teil 4)

- **Verhaltensaenderung:** die bisher fest im Parser verdrahteten deutschen
  Eingabewoerter (`nimm`, `lege`, `gib`, `oeffne`, `schliesse`,
  `verschliesse`, `entsperre`, `erzaehl`, `frag`, `spiele`, `passe`,
  `karten`, `ablage`, `zug beenden`, Praepositionen `an`/`auf`/`aus`/`nach`/
  `von`/`zu`) sind **nur noch mit `language: de` aktiv** — sie sind jetzt
  Alias-Tabellen im Sprachpaket. Ohne `language:` lehnt der Parser sie ab
  (gepinnt). `ci/e2e/npc-besitz.in` schreibt seine eine deutsche Zeile
  neu auf Englisch (Ausgabe byte-identisch, `.expect` unangetastet).
- **Alias-Mechanik** (`AliasEnv` in `Parser.hs`): fuehrende Alias-Phrasen
  (Verben + Befehlswörter) werden auf die kanonischen Tokens umgeschrieben
  (laengster Treffer), Richtungswoerter und Praepositionsrollen
  (`from`/`to`/`about`/`on`/`in`) werden rollenbezogen aufgeloest — keine
  globale Wort-Ersetzung (die Wuerde z. B. englisches `take an apple`
  zerstoeren). Aktiv nur bei `language:`.
- **Umfang (P2):** historische 21 Tokens plus Vervollstaendigung —
  `suche`/`benutze`/`sprich`/`greife`/`attackiere`/`untersuche` (Verben),
  `inventar`/`karte`/`status`/`rueckgaengig`/`mache rueckgaengig`/
  `speichern`/`laden`/`hilfe` (Befehle), Richtungen `nord`/`sued`/`ost`/
  `west`/`hoch`/`runter` + Diagonalen (`nordost`/`no` usw.).
- **Allgemein:** auch `play N on X` (Ziel nach Praeposition) und
  `ask X about Y` mit Rollen-Praepositionen funktionieren jetzt
  einheitlich; die deutsche Hilfe zeigt genau die funktionierenden Formen
  (Grenzen: keine Artikel-Streichung, keine trennbaren Verbprefixe, kein
  `alle`/`mit`/`geh` — bewusst nur der P2-Umfang).
- **Neu verkabelt:** `worldbuilder test` und `worldbuilder fuzz` parsen mit
  den Welt-Aliasen (Content-Tests deutscher Abenteuer koennen deutsche
  Befehle schreiben); die Fuzz-Vokabelliste behaelt ihre deutschen Woerter
  (fuer Nicht-de-Welten sind es Parser-Garbage).
- 2 neue Engine-Tests (452): Alias-Akzeptanz in `language: de`-Welten
  (20 Pins ueber Verben/Befehle/Richtungen/Prapositionen) und Ablehnung
  ohne `language:`.

### Sprachpakete: de-Paket vollstaendig (4.3, Teil 3)

- **Volluebersetzung:** `lang/de.json` deckt jetzt alle **232** Katalogschluessel
  ab (Vorher: 8) — inkl. `help.text` (deutsche Hilfe mit den Aliasen aus
  4.3.4) — plus **22 Term-Eintraege** (10 Richtungen, 7 Ausruestungsplaetze,
  5 Kartentypen). Platzhalter-Validierung beim Generieren: jede Vorlage
  behaelt exakt ihre `{arg}`-Menge.
- **Gate hart:** CI-Stufe 2c laeuft mit `--require-complete` — ein neuer
  Katalogschluessel braucht ab sofort eine Uebersetzung im selben Commit
  (kein stiller Englisch-Fallback mehr).
- **Inventar unkatalogisierte Ausgaben (4.3.3):** 33 weitere Renderstellen auf
  katalog-bewusstes Rendering umgestellt (`renderMsgFor`) — Vehicles-Fehler-
  und Statuszeilen (15), Equip-Befehle/-Liste (7), Karten-Bildschirme (10),
  Disambiguation-Optionen (1); englische Vorlage byte-identisch. Bewusst
  unuebersetzt bleiben: CLI-/TUI-Chrome ohne Schluessel (Prompts,
  "Loaded world: …"), die Welt-Ladefehler (vor der geladenen Welt) und die
  zehn derzeit unerreichten W4-`device*Msg`-Reservemeldungen in `Game.hs` —
  alle in `docs/adventure-schema.md` inventarisiert.
- **Funde:** (a) `help.text` referenziert eine `helpTemplate`-Bindung statt
  eines Literals — reine Literal-Extraktion findet 231 statt 232 Schluessel;
  (b) die `device*Msg`-Helfer in `Game.hs` haben keine Aufrufer (ihre
  Meldungen sind unerreichbar); (c) der "[Unbekannt]"-Kartenkasten ist wie
  die Kartentyp-Labels ein historischer Deutscher Hardcode.
- 2 neue Engine-Tests (450): Paket-Vollstaendigkeit (Bijektion im Haskell-
  Test) und ein deutscher Probelauf (kein `<msg:`, kein englischer
  Katalogtext, Richtungs-Term end-to-end "Du gehst nach Norden.").

### Sprachpakete: Render-Pass am Rand (4.3, Teil 2)

- **Lokalisierungs-Pass** (`Messages.localizeEvents`): keyed Meldungen werden
  am Ausgabekanal aus Key + Args neu gerendert — `messages:`-Overrides und
  Sprachpaket-Templates greifen ohne dass die ~229 `evMsg`-Call-Sites sich
  aendern. Struktur-erhaltend (Event-Anzahl/Reihenfolge, `mpKey = Nothing` =
  Autoren-Text bleibt roh), ohne `language:`/`messages:` ist der Pass die
  Identitaet.
- **Term-Uebersetzung** (`translateTerms`): die Aufzaehlungs-Argumente `dir`
  (Richtungen) und `slot` (Ausruestungsplaetze) werden ueber die
  `slot.value`-Tabelle des Sprachpakets uebersetzt; Kartentyp-Labels laufen
  ueber dieselbe Tabelle (`card_type.*`) — der Default bleibt byte-identisch
  (die historischen deutschen Labels).
- **Ausgabekanten eingebunden:** `applyLoopCommand`/`executeCommand`
  (Kommando-Events), GameLoop-Chrome (Menues, Tod/Sieg-Bildschirme,
  Speicher-Tore, `help`, `ui.press_enter` ueber `feReadPause :: String -> IO ()`)
  und die SaveLoad-Ausgaben (`formatSaveEntry`/`deleteSaveSlot`/
  `loadMetaForSlug` nehmen jetzt die Welt entgegen) rendern katalog-bewusst;
  `worldbuilder test` lokalisiert seine Marker-Texte. Protokoll: die Wire-
  Events tragen finalen Text — der Server lokalisiert vor dem Kodieren
  (`docs/protocol-v1.md` Sektion 8).
- **Nicht-Leerheits-Vertrag** dokumentiert (`Types/Output.hs` Fragment-Algebra,
  `docs/adventure-schema.md`): die Verwerf-Logik fuer leere Fragmente wertet
  den englischen Default-Text — Templates/Overrides duerfen nie leer rendern.
- 4 neue Engine-Tests (448): Pass-Verhalten (Override/Terms/Identitaet/Roh-
  Payloads), Term-Slots, `renderMsgFor`, Kartentyp-Terms.

### Sprachpakete: Datenmodell & Katalog-Aufloesung (4.3, Teil 1)

- **`language:` und `messages:`** am Abenteuer: `language: de` waehlt das
  eingebaute Sprachpaket (`lang/de.json` → generiert nach
  `src/Messages/LangDe.hs`), `messages: {key: text}` ueberschreibt einzelne
  Engine-Katalog-Meldungen. Prioritaet: `messages:` > Sprachpaket > englischer
  Standard; fehlende Paketschluessel fallen zurueck auf den englischen Text.
- **Drei Schichten als Katalog-Arithmetik** (`effectiveCatalog` in
  `src/Messages.hs`): englischer Default-Katalog, Sprachpaket-Templates und
  per-Adventure-Overrides — `renderMsgIn` rendert gegen einen beliebigen
  Katalog, `renderMsg` bleibt der englische Default.
- **Welt-Felder `worldLanguage`/`worldMessages`** in `world.json`, nur bei
  Belegung geschrieben (Byte-Vertrag wie bei `procDefs`) — ohne die neuen
  YAML-Felder bleibt alles byte-identisch.
- **Validierung:** unbekannter Sprachcode = harter Fehler (`UnknownLanguage`),
  leerer Override-Wert = harter Fehler (`EmptyMessageOverride`), unbekannter
  Katalogschluessel = Warnung (`UnknownMsgKey`). `language:`/`messages:` sind
  nur in der Hauptdatei erlaubt (kein `include:`).
- **Neues Gate `scripts/check-lang-pack.sh`** (CI-Stufe 2c): generiertes Modul
  synchron zur JSON (sha256 im generierten Header), keine unbekannten
  Katalogschluessel, Vollstaendigkeit mit `--require-complete` (greift hart,
  sobald das de-Paket vollstaendig ist). Generator:
  `python3 scripts/gen-lang-pack.py`.
- **Derzeitiger Stand des de-Pakets:** 8 uebersetzte Meldungen (Bewegung/Look)
  als Mechanik-Nachweis; Volluebersetzung, Eingabe-Aliase und Grammatikfelder
  folgen in den Stufen 4.3.3–4.3.5.

### Veto Stufe 2: `verb_map`-Phasen (4.2)

- **`before:`/`instead:` als Phasen-Präfix in `verb_map`-Schluesseln** (Items und
  NPCs): `before:take,intact:` laeuft **vor** der Standardaktion und vetot sie
  mit einem `block:` darin (wie eine `on: before`-Regel); `instead:take,intact:`
  **ersetzt** die Standardaktion vollstaendig. Schluessel ohne Praefix behalten
  exakt das historische Verhalten (auf `take` Effekte *zusammen mit* dem
  Aufheben, bei allen anderen Verben ersetzend).
- **Ein `(verb, state)`-Paar, eine Phase:** Doppelbelegung ueber Phasen hinweg
  ist ein harter Compile-Fehler (`VerbPhaseClash`). Die Lookup-Reihenfolge ist
  `instead:` → `before:` → alter Eintrag → Standardaktion.
- **Guards gelten phasenuebergreifend:** `take.already`, `take.not_portable`
  und `inventory.full` laufen vor allen Phasen — ein `instead:`-Eintrag kann
  nicht erneut feuern, sobald die Effekte das Item mit `give:` mitfuehren.
- **Zugverbrauch:** Ein `verb_map`-Veto geschieht mitten im Kommando —
  turnfoermige Kommandos verbrauchen den Zug wie jeder gescheiterte Versuch;
  das `turn:`-Feld von `block:` steuert nur Regel-Vetos.
- **Verhaltensänderung (Migration):** Die historischen `take`-Eintraege in
  `fantasy`, `space-opera` und `cyberpunk` (kronen-/artefakt-/shard-Heben)
  laufen bewusst neu als `instead:` + `give:` (echtes Ersetzen, Nutzerentscheid
  2026-10-01). Sichtbarer Unterschied: bei diesen drei Items entfaellt die
  Standard-Zeile `You take the …` — die Welten sind sonst spielidentisch.
- **Byte-Vertrag:** Alte `verb_map`-Eintraege encodieren weiterhin **ohne**
  `phase`-Feld im `world.json` (alle nicht migrierten Abenteuer bleiben
  byte-identisch, per stash-Vergleich geprueft); nur `before:`/`instead:`
  tragen das neue optionale Feld. P1-15 (Take-/Drop-Events am Zustandsdiff
  statt am Kommando) war bereits durch die Phase-0.x-Bugfixes erledigt.
- **Tests:** +7 Engine-Tests (**441**) inkl. Pins auf das Legacy-Verhalten und
  die JSON-Kodierung; +2 Worldbuilder-Tests (**228**); neues E2E-Fixture
  `phasen` (7 geordnete Marker: Veto, Ersetzen, Guards, Nachhall).

### Benannte Zufallsströme (B8)

- **`random: {stream: <name>, choices: [[gewicht, [effekte]], ...]}`** zieht aus
  einem eigenen Zufallsstrom `rng.<name>` statt aus dem gemeinsamen Default-Strom:
  Ziehungen auf einem Strom verschieben weder den Default-Strom noch andere
  Ströme — Zufall bleibt stabil gegenüber Inhaltsänderungen anderer Systeme.
  Die alte Listenform `random: [[gewicht, [effekte]], ...]` und die
  `encounter:`-Tabellen bleiben unveraendert auf dem Default-Strom
  (**byte-identisch**, neuer Effekt-Konstruktor `RandomChoiceOn`, alter
  unangetastet).
- **Speicher ohne neue SaveState-Felder** (Regel 6): Strom-Zustand in der VarMap
  unter `rng.<name>` (Hex-Text), Init beim ersten Zugriff aus Name-Hash +
  aktuellem Default-Strom **ohne Verbrauch** des Default-Stroms.
- **`rng.*` ist engine-reserviert:** `set_var`/`set_text_var`/`add_var`/
  `compute_var`, `variables:`-Deklarationen, `initial_variables:`-Eintraege und
  Prozedur-Parameter auf dem Namensraum sind harte Compile-Fehler
  (`RngVarWrite`) — schuetzt den Reproduzierbarkeitsvertrag.
- **Tests:** +3 Engine-Tests (**434**) + 1 Worldbuilder-Test (**226**); neues
  E2E-Fixture `stroeme` (geordnete Marker pinnen die exakte Ziehungsfolge).

### Content-Fuzzer (B5)

- **`worldbuilder fuzz <adventure> [--seed N] [--runs N] [--steps N] [--timeout-ms N]
  [--window N] [--replay <file>]`**: deterministische Zufalls-/Heuristiklaeufe
  gegen ein kompiliertes Abenteuer (Befehle aus dem Weltvokabular, ~20%
  Parser-Garbage). Drei Fund-Arten:
  - **Absturz**: Exception im Parse-/Anwende-/Render-Pfad eines Schritts.
  - **Haenger**: ein Schritt kehrt innerhalb des Timeouts nicht zurueck.
  - **Endlosschleife**: `window` (Default 25) Schritte ohne jeden Fortschritt
    (Zustand inkl. Zugzahl unveraendert, die `cmd.*`-Echos der Engine sind
    ausgeblendet), obwohl mindestens ein Befehl zugfaehig war — die
    Veto-Soft-Lock-Signatur.
- **Reproduzierbarkeit**: gleicher Seed = gleiche Laeufe; jeder Fund nennt Seed,
  Run, Schritt und die exakte Befehlsfolge — nachspielen per `--replay <datei>`
  (eine Eingabe pro Zeile) oder per `--seed/--runs/--steps`.
- **CI-Stufe 4c**: fester Seed 42 ueber alle gelieferten Abenteuer und Fixtures;
  Funde lassen das Gate rot werden. Kein Engine-Risiko (Tuer III): die Engine
  bleibt unangetastet, der Fuzzer treibt denselben reinen Schrittkern an wie
  die Live-Schleife (`applyLoopCommandEv`).
- **Tests**: +8 Worldbuilder-Tests (**225**) — inkl. Nachweis, dass alle drei
  Fund-Arten wirklich feuern (Exception-Pfad, Timeout-Pfad, Veto-Soft-Lock).

### Regel-Diagnostik (B4)

- **Vier Dead-Content-Diagnosen** im bestehenden Warnkanal (nicht fatal, stabile
  `ciCode`s), konservativ-dreiwertig analysiert — eine Warnung heißt „garantiert nie":
  - `UnreachableTrigger`: Regel-`on:` ohne Gegenstück (unbekannte Räume/Items,
    `custom X` ohne `raise: X`, `chapter` ohne Kapitel, `levelup` ohne `progression:`).
  - `UnsatisfiableCondition`: widersprüchliche Bedingungen (`all:` mit Negation,
    disjunkte Zahlen-Schranken, zwei Textwerte) und `has_flag` auf nie gesetzte Flags
    (`set_flag`/`initial_flags` als Setz-Seite) — Regeln, Tore und `if:`-Zweige.
  - `DeadExit`: Ausgänge mit nie zutreffendem Guard oder nie aufschließbarer
    `locked_by:`-Entity (Unlock-Pfade: NPC-Tod, `unlock`-Verb, `interactions` mit
    `state: unlocked`, `set_state … to: unlocked`).
  - `UnreachableRoom`: BFS ab `start_room` (Exits + `set_exit`/`generate_room` als
    Kanten, `move:`/Haltestellen als Ankünfte) — nie erreichbare Räume samt Ausgängen.
- **Compiler-Refactor**: `allAOutcomes` = `deepOutcomes` + `outcomeSurfaces` (jetzt
  mit Diagnostik-Pfaden); die Oberflächen-Liste bleibt der eine Vertrag für
  Referenzchecks, Asset-Sammlung und Diagnostik.
- **Funde in gelieferten Inhalten**: 3 Fixtures mit echten Positivfunden
  (krypta: `set_state … to: offen` schließt das `locked_by:`-Exit nie auf — echter
  toter Inhalt; massen/pursuit: bewusste Test-Sackgassen). Inhalte unverändert
  (Entscheidung), 26 gelieferte Abenteuer sauber.
- **Tests**: +4 Worldbuilder-Tests (**217**).

### Spiel-Export (B6)

- **`worldbuilder export <adventure> -o <dir> [--with-engine] [--zip] [--force]`**:
  Bündelt ein fertiges Spiel — `world.json`/`save.json` (**byte-identisch** zu
  `worldbuilder compile`), alle referenzierten Asset-Dateien und `play.sh`/`play.bat`
  (Layout wie `packaging/windows/play.bat`: cd ins Bundle, Engine-Suche, Audio-Helper-
  Autodetect). `--with-engine` kopiert die Engine nach `bin/`, `--zip` legt `<dir>.zip`
  an (`zip`/`7z`).
- **`assets:`**-Sektion am Adventure: explizite Extra-Dateien (README, Cover) für das
  Bundle; zusammen mit den `sfx:`/`music:`-Referenzen aus **allen** Effekt-Flächen
  (inkl. verschachtelter `if:`/`random:`-Zweige) automatisch eingesammelt.
- **Asset-Pfade sind spielwurzel-relativ** (Hauptdatei-Verzeichnis beim Export,
  Bundle-Wurzel zur Laufzeit) — die Pfadstrings in der Welt bleiben unverändert.
  Fehlende Assets und nicht-relativierbare Pfade (`..`, absolut) sind Warnungen.
- **Compiler-Härtung**: `allAOutcomes` traversiert jetzt **alle** Outcome-Flächen
  (Dialoge, Topics, on_talk, Vehicle-Bedingungen/Stations, Wetter, Drains, Beobachter,
  Patrouillen, Kampf, Abilities, Encounter) und verschachtelte Zweige rekursiv —
  `set_exit`/`give`-Referenzchecks gewinnen dieselbe Abdeckung.
- **Tests**: +4 Worldbuilder-Tests (**213**); CI-Stufe 9 (export bundle): Datei-Set,
  Byte-Identität zu `compile`, Playthrough durch das eigene `play.sh`, Zip-Smoke.
  Fixture `buendel.yaml` + Platzhalter-Assets in `examples/fixtures/assets/`.

### NPC-Besitz (B7)

- **Autorenform**: `carried_by: <npc-id>` am Item (wie `in_container:`) — das Item startet
  im Besitz eines NPCs (`CarriedBy (ActorNPC …)`). `carried_by: player` = Start im
  Spielerinventar. Unbekannte NPC-IDs → harter Fehler `UnknownNpc`; `in_container:` +
  `carried_by:` zusammen → harter Fehler `CarriedByConflict`.
- **Befehle**: `take X from <npc>` (Bestehlen/Plündern, Zielauflösung erst Container,
  dann NPC) und `give X to <npc>` / `gib X an <npc>` (Übergabe). Beide kosten einen Zug
  (L13-Urteil `GiveCmd` = `True`).
- **Sichtbarkeit**: `look at <npc>` zeigt getragene Items (`npc.carries`);
  Protokoll-Snapshot `NpcSummary.carried` (leer = Feld entfällt, byte-kompatibel).
- **Effekte**: `give: {item: X, to: <actor>}` (Objekt-Form) überreicht an NPCs — die
  String-Form `give: <item>` bleibt bei „an den Spieler“ (byte-kompatibel).
  `MoveEntity (CarriedBy <actor>)` respektiert jetzt den Ziel-Aktor (vorher fest Spieler).
- **Bugfix (4.4 nachgeholt)**: `capacity:` fehlte in `knownKeys EntItem` und erzeugte
  fälschlich `UnknownYamlKey`-Warnungen.
- **Messages**: 4 neue Schlüssel (`npc.carries`, `npc.gave_to`, `npc.no_item`,
  `npc.took_from`); Katalog jetzt **232** Keys.
- **Tests**: +3 Engine- (**431**) + 6 Worldbuilder-Tests (**209**); Fixture
  `npc-besitz.yaml` in CI-Stufe 4 (49 Playthroughs).

### Konversation (4.5)

- **Topic-Tabelle**: `topics:` am NPC — `ask <npc>` / `tell <npc>` über ein Thema führen den
  zugeordneten Effekt aus. Unbekanntes Topic → `dialog.no_topic`, unbekannter NPC →
  `dialog.no_npc`.
- **Dialog-Sugars**: `say_node: <id>` (setzt den aktiven Dialogknoten) und
  `dialogEnd: true` (beendet den Dialog) als Kurzformen für `set_var dialog_node`.
- **Barks**: `barks:` am NPC — Ambient-One-Liner, Compiler-Sugar für `on: turn`-Trigger
  mit Cooldown (Default 20). Optional mit `when`-Prädikat.
- **`on_talk:`**: Zusätzlicher Effekt bei jedem `ask`/`tell` auf den NPC,
  Compiler-Sugar für `on: talk`-Trigger (NPC-spezifisch).
- **`OnTalk` Wildcard-Matching**: Trigger mit `OnTalk "" ""` feuern bei jedem `ask`/`tell`,
  `OnTalk <npc-id> ""` feuert bei jedem Topic dieses NPCs.
- **Messages**: 2 neue Schlüssel (`dialog.no_topic`, `dialog.no_npc`).
- **Tests**: +1 Engine- (**427**) + 3 Worldbuilder-Tests (**203**); Fixture
  `gespraeche.yaml` (Topics, Barks, on_talk, Dialogbaum mit say_node/dialogEnd) in
  CI-Stufe 4.

### Container (4.4)

- **Tragbare Container**: Items mit `capacity: N` (Truhe, Tasche) — Verschachtelung
  beliebig tief über `in_container:` (der Orts-Baum über die Item-Locations).
- **Ortsfeste Container**: `containers:`-Sektion (wie `devices:`) mit `capacity:`, `open:`,
  `locked:` — der Startzustand landet in den Entity-States.
- **Gebaute Kern-Verben** (immer verfügbar): `open`/`close`/`lock`/`unlock` (Zustandswechsel
  `open`/`closed`/`locked`), `take X from Y`, `put X in Y` (einzeln, Kapazität geprüft).
  `take X` findet X auch in offenen Containern; `look` zeigt den Inhalt offener Container.
- **Scope-Vertrag:** geschlossene Container schneiden den Zweig ab (ihre Inhalte sind weder
  sichtbar noch erreichbar); die Verschachtelung ist beliebig tief.
- **Zählbare Limits:** `player: {inventory_limit: N}` (VarMap `inventory.limit`) und der
  Laufzeit-Effekt `set_inventory_limit: N` — beim Überschreiten verweigern `take`/
  `take from` (`inventory.full`). Kapazitäten zählen Items (kein Gewicht/Volumen).
- **Key-Bindung ist Autoren-Vokabel:** `lock`/`unlock` wechseln nur den Zustand; wer einen
  Schlüssel verlangt, sperrt per Regel (block).
- **Zero New `SaveState` Fields:** Zustand = `entityStates`, Inhalt = `itemStates`
  (`InContainer`), Limit = VarMap. `ContainerState` ist jetzt der Definitionsteil der
  `containers:`-Einträge.
- **Messages**: 18 neue Schlüssel (container.*, inventory.full).
- **Tests**: 2 Engine- (**426**) + 2 Worldbuilder-Tests (**200**); Fixture `behaelter.yaml`
  (Inhalt/Nehmen, Schließen/Sperren, verschlossene Kiste + Kapazität) in CI-Stufe 4b.

### Geschlossene Mengen-Operationen (B3)

- **Fünf geschlossene Effekte** ersetzen die 20-Zweig-Kaskaden: `damage_all`, `move_all`,
  `reveal_all`, `consume_all`, `set_state_all`. „Geschlossen" ist der Vertrag — der Autor
  liefert **nie** eine Effektliste als Schleifenkörper (keine Turing-Vollständigkeit, kein
  Autoren-Kontrollfluss).
- **Zielmengen in B2-Sprache**: `what` (items/npcs/alive_npcs) × `in: <raum>` / `by: <actor>`
  × optionaler `tag:` — Abfrage und Wirkung sprechen eine Sprache. `move_all` hat zusätzlich
  `to: {in: …}` / `to: {by: …}`.
- **Wirkung pro Zielart**: `damage_all` → NPC-Health (wie `damage:`, keine Klemmung);
  `move_all` → Item-Location / NPC-Position; `reveal_all` → `itemDiscovered`;
  `consume_all` → `Removed`; `set_state_all` → `itemStatus` / `npcStatus`.
- **Die Menge ist jedes Item am Ort — auch versteckte** (sonst fände `reveal_all` seine
  Ziele nie); die B2-Zählung zählt damit auch versteckte Items (Autoren-Abfrage, keine
  Sichtbarkeits-Abfrage).
- **Tests**: 1 Engine- (**424**) + 1 Worldbuilder-Test (**198**); Fixture `massen.yaml`
  (alle fünf Operationen als Regeln mit Markern) in CI-Stufe 4b. Zwei alte Test-Warnings
  (unused binds) und ein Test-Defekt (`a` floss nicht ins Ergebnis ein) mit gefixt.

### Mengen- und Tag-Abfragen: Prädikate und die Zähl-Familie (B2)

- **Tag-Prädikate** `HasTaggedItem ActorRef String` (`actor_has_tag: {actor, tag}`) und
  `RoomHasTaggedItem RoomID String` (`room: X, has_item_tag: Y`) — generalisieren
  `playerHasTaggedItem` auf jeden Akteur und auf Räume („trägt etwas mit Tag X",
  „liegt hier eine Lichtquelle?").
- **Die Zähl-Familie** `VRCount CountSpec` als Int-Wert in `compare_var`/`compare`/
  `compute_var`: **was** (`items`/`npcs`/`alive_npcs`) × **wo** (`in: <raum>` /
  `by: <actor>`) × optionaler **Tag-Filter**. String-Form `count.<was>.<in|by>.<id>[.<tag>]`,
  Objekt-Form `{count: {what: …, in|by: …, tag: …}}`. „Anzahl Items ≥ n" und „alle NPCs
  tot" (`alive_npcs == 0`) sind damit Zeilen, keine 20-Zweig-Kaskaden.
- **`compare_var` liest jetzt auch `distance.`- und `count.`-Werte** (wie zuvor
  `condition_turns.`) — die Prüfregel bleibt: genre-neutrale Primitive, Inhalt macht das Genre.
- **Anmerkung:** getragene NPCs zählen für `by:` als 0; fehlende Räume/IDs zählen als 0
  (wie bei den bestehenden Prädikaten) — eine Referenzprüfung für Prädikat-Räume ist
  weiterhin offene Härterung.
- **Tests**: 2 Engine-Tests (**423**) + Fixture `mengen.yaml` (alle vier Katalog-Beispiele
  als Regeln mit Markern) in CI-Stufe 4b. Die Vokabel liegt in der Engine (keine
  Worldbuilder-Schicht nötig).

### Bibliotheken: `include:` für wiederverwendbare Sektionen (5.3)

- **`include: [lib/a.yaml, lib/b.yaml]`** (transitiv erlaubt): ein Abenteuer kann
  Bibliotheksdateien einbinden (Prozeduren, Verben, Regeln, Inhalte — „Pakete"), Pfade
  relativ zur jeweiligen Datei. JSON und YAML wie überall.
- **Merge-Vertrag (fest, weil YAML-Objekte keine Reihenfolge haben und die
  Trigger-Reihenfolge semantisch wirksam ist):** eigene Sektionen **vor** den Includes
  (die Hauptdatei hat Vorrang — ihr erster `block:` stoppt die Bibliothek); Includes in
  Listenreihenfolge, Tiefensuche (eine Datei vor ihren eigenen Includes).
- **Zyklus = harter Fehler** (mit Kette); eine über zwei Wege erreichbare Datei (Diamond)
  wird **einmal** geladen, an der Position ihres ersten Vorkommens.
- **Single-Value-Felder sind der Hauptdatei vorbehalten** (`name`, `start_room`,
  `player`, `combat`, `game`, `journal`, `progression`, `interactions`, `deck`,
  `title_art`, …) — eine Bibliothek, die eines setzt, ist ein harter Fehler mit Pfad.
- **Doppelte IDs über den Merge hinweg sind ein harter Fehler**, der **beide Dateien**
  nennt (alle ID-tragenden Sektionen: rooms, items, npcs, quests, vehicles, verbs,
  variables, rules, cards, sandbox_zones, procedures, chapters, facts, devices, clips,
  abilities, encounter_tables, factions, pursuit, initial_flags/variables, end_art;
  `tests:`-Namen sind keine referenzierbaren IDs und `combine:` hat keine ID).
- **Byte-Identität:** derselbe Inhalt kompiliert zur identischen Welt, egal ob monolithisch
  oder in Bibliotheken aufgeteilt (Unit-Test). Ohne `include:` bleibt alles beim Alten
  (Fast-Path).
- **Tests**: 7 Worldbuilder-Tests (**197**; Merge, Forbidden, Duplikate, transitiv,
  Zyklus, Diamond, Byte-Identität) + Fixture `include_demo.yaml`/`include_lib.yaml`
  (dogfoodet `worldbuilder test` mit Bibliothek) in CI-Stufe 4b.

### Verfolgung & Pfadsuche: `pursuit:` und die Distanz-Vokabel (Tür IV)

- **Kern `src/Pursuit.hs`**: stateless, reine BFS-Suche über den Laufzeit-Graphen
  (`effectiveConnections`) — `bfsDistances` (Hops, `-1` = unerreichbar) und
  `stepToward`/`stepAway` (je **eine** Kante, nie der ganze Pfad).
- **Wortschatz**: `distance.<sucher>.<ziel>` als Int-Wert in `compare_var`/`compute_var`
  (`-1`-Sentinel); Effekte `step_toward`/`step_away_from` in der Form
  `[sucher, ziel, msg?]` (Sucher: NPC oder Schiff; Ziel: Akteur oder `room:<id>`).
  Ohne `msg` gilt der Katalog (`pursuit.step`/`pursuit.flee` mit `{name}`/`{room}`/`{dir}`),
  ohne Schritt `pursuit.no_path`.
- **`pursuit:`-Sektion** (`npc`, `target?`, `ignores?`, `msg?`) erzeugt pro Verfolger einen
  `on: turn`-Trigger (ein Schritt pro Zug) — **deterministisch in NPC-ID-Reihenfolge**
  emittiert (Trigger-Reihenfolge ist semantisch wirksam).
- **Tie-Break-Vertrag (fest, sichtbar):** bei Gleichstand gewinnt die **kleinste
  Ziel-Raum-ID**, dann `Pursuit.directionPriority` („Gameplay-Vertrag, nicht
  umsortieren") — bewusst unabhängig von `Direction`s abgeleitetem `Ord`.
- **Fairness als Vertrag:** Default „wie der Spieler" (offene Ausgänge, aufgeschlossene
  Türen, erfüllte Wachen-Prädikate); `ignores: [locked|guarded]` ist die explizite
  Ausnahme („Wolf durch die verriegelte Tür"). `step_away_from` ist Flucht: streng
  größerer Abstand, wobei „unerreichbar" als unendlich weit zählt.
- **Reproduzierbarkeit:** stateless (kein neues Save-Feld), kein `rngState`-Verbrauch,
  nur geordnete Container; Save/Load mitten in der Verfolgung rechnet identisch weiter
  (Unit-Test). `distance.`-Werte sind live berechnete virtuelle Werte wie `condition_turns.`.
- **Compiler-Checks**: `MissingNPC` (unbekannter Verfolger), `UnknownPursuitIgnore`
  (falscher `ignores:`-Wert).
- **Meldungskatalog**: 3 neue Schlüssel (`pursuit.step`, `pursuit.flee`, `pursuit.no_path`).
- **Tests**: 5 Engine-Tests (**421**) + 2 Worldbuilder-Tests (**190**); Golden-Fixture
  `examples/fixtures/pursuit.yaml` (Tie-Break-Pfad „ostweg" + Fairness an der verriegelten
  Tür) in CI-Stufe 4b.

### Fortschritt & Stufen: `progression:` für Erfahrungspunkte (XP) und Level (W2)

- **`progression:`-Sektion** (`AProgressionDef` → `ProgressionDef`): Datengestützte Stufentabelle mit Schwellen (`xp:`), Titeln (`name:`), optionaler Aufstiegsnachricht (`msg:` / `level_msg:`) und Stufeneffekten (`effects:`). Leere Tabellen werden im `world.json` weggelassen (M2-Invariante, Byte-Identität für bestehende Welten).
- **Effekt `gain_xp: <int>`**:
  - Modifiziert `xp.current`. Negative Delta-Werte sind erlaubt, werden aber nach unten auf 0 geclamped (`xp.clamped`).
  - **Stufen-Garantie (Anti-De-Level):** XP-Verlust führt niemals zu einem Herabstufen (`level.current` wächst strikt monoton).
  - Mehrfachaufstiege in einem Zug laufen geordnet in aufsteigender Stufenfolge ab.
- **Event `OnLevelUp <n>`** (Trigger `on: levelup <n>` / `on: level_up <n>`): Feuert beim Erreichen der Stufe `n`.
- **Zero New `SaveState` Fields**: Alle Zustände liegen in `variables`:
  - `xp.current` (Int, Default 0)
  - `level.current` (Int, Default 1)
  - Additive Kampfboni: `bonus.attack` in `effectiveAttack`, `bonus.defense` in `effectiveDefense`, `bonus.hp` in `effectiveMaxHealth`.
  - Reservierte Namensräume `xp.`, `level.`, `bonus.`.
- **Präsentation**: `stats` zeigt bei vorhandenem `progressionDef` die Fortschrittszeile (`Level n — Name (xp/next XP)` bzw. `Level n — Name (xp XP)` auf Max-Level). Ohne `progression:` bleibt die Ausgabe byte-identisch.
- **Compiler-Checks**: `EmptyLevels`, `BadLevelXp`, `NonMonotonicXp`, `ProgressionVariableClash`, `GainXpWithoutProgression` (Warnung).
- **Meldungskatalog**: 4 neue Schlüssel (`stats.progression`, `stats.progression_max`, `levelup.default`, `xp.clamped`), Katalog bei **206 Keys** (100% referenziert).
- **Tests**: 6 Engine-Tests (**416**) + 2 Worldbuilder-Tests (**188**); Golden Fixture `examples/fixtures/quest_rpg.yaml` mit 11 Assertions in CI-Stufe 4b.

### Vorrichtungen & Halterungen: `devices:` für Hebel und Halterungen (W4)

- **`devices:`-Sektion** (`ADeviceDef` → `DeviceDef`): ortsfeste Vorrichtungen im Raum mit Zuständen (`flip_states`), Fit-Regeln (`fits_tag` / `fits`), geschlossenen Effektlisten (`on_insert`, `on_remove`, `on_flip_*`) und optionaler Aufnahme für genau ein Item. Hebel und Halterung sind zwei Ausprägungen desselben Konzepts.
- **Primitives (Tür I)**:
  - Prädikat `ActorHas ActorRef ItemID` (verallgemeinert `PlayerHas`, deckt `ActorEntity <devId>` und `ActorNPC <npcId>` ab).
  - Effekte `Mount ItemID ActorRef` (platziert Item bei Entität: `CarriedBy (ActorEntity devId)`) und `Unmount ItemID` (legt Item zurück in den Raum der Vorrichtung).
- **Zero New `SaveState` Fields**: Item-Orte werden im bestehenden `itemStates`-Feld gespeichert (`CarriedBy (ActorEntity devId)`), Hebelzustände über das bestehende `actorProperties`-Zustandsmodell (`PState`).
- **Fit-Kontrakt & Veto-Muster (2.2)**:
  - Generierte `OnBefore`-Trigger weisen unpassende Items (`reject`), Mehrfachbelegung (`occupied`) und nicht getragene Items (`not_carried`) vor dem Zugverbrauch ab (`Block (Just msg) False`, 0 Züge).
  - Erfolgreicher Einbau (`stecke <item> in <halterung>`) und Ausbau (`ziehe <item> aus <halterung>`) verbraucht einen regulären Spielzug und feuert `on_insert` bzw. `on_remove`.
  - Lever-Flip toggelt atomar über `Conditional` zwischen den deklarierten Zuständen und führt richtungsabhängige Effekte (`on_flip_<state>`) aus.
- **Sichtbarkeit**: `examine <device>` zeigt die Vorrichtungsbeschreibung und bei montiertem Item automatisch `Mounted: <Item-Name>.` an.
- **Explizites Beleuchtungsmuster**: Ersetzt aufwändige Physik/Propagation durch explizite Flag-Kopplung (`on_insert: set_flag: lit, "true"`, `on_remove: set_flag: lit, "false"`).
- **Compiler-Checks**: `DuplicateDevice`, `UnknownDeviceLocation`, `UnknownDeviceItem`, `DeviceFlipStateCount`, `UnknownDeviceTag` (Warnung), `DeviceWithoutEffects` (Warnung), `DeviceVerbClash`.
- **Meldungskatalog**: 11 neue Schlüssel (`device.*`), Katalog bei **202 Keys**.
- **Tests**: 3 Engine-Tests (**410**) + 2 Worldbuilder-Tests (**186**); Fixture `examples/fixtures/krypta.yaml` in CI-Stufe 4b.

### Kapitel: `chapters:` mit Auto-Gate und `next_chapter`/`goto_chapter` (W3)

- **`chapters:`-Sektion** (`AChapterDef` → `ChapterDef`): benannte Kapitel in narrativer
  Reihenfolge mit optionaler `intro:`-Meldung und optionaler `when:`-Auto-Gate-Bedingung
  (Prädikate, wie bei Ausgängen). Kompiliert als **Liste** — die Deklarationsreihenfolge
  ist Gameplay-Vertrag (nicht umsortieren); leere Liste wird im `world.json` weggelassen.
- **Effekte `next_chapter`/`goto_chapter: <id>`** und **Event `OnChapter <id>`**
  (Trigger `on: chapter <id>`) — der Szenenwechsel (Ortswechsel, Zustände, Meldungen)
  bleibt Autorenregel am Event.
- **Auto-Gate:** höchstens **ein** Wechsel pro Zug, geprüft nach dem Turn-Trigger-Fold
  (in `GameLoop`, beiden zug-konsumierenden Pfaden); Kandidat = erstes Kapitel in
  Deklarationsreihenfolge mit erfülltem `when:`, weder besucht noch aktuell.
- **Keine Rückblenden — maschinell erzwungen:** `goto_chapter` auf ein besuchtes Kapitel
  wird verweigert („This chapter is behind you."); statisch erkennbare Rückwärts-Sprünge
  (`goto_chapter` in einer `on: chapter`-Regel auf ein früheres Kapitel) sind
  **Compile-Fehler**; unbekannte Ziele (`UnknownChapter`), doppelte IDs
  (`DuplicateChapter`), unerreichbare Kapitel (`UnreachableChapter`, Warnung) und
  `chapter.`-Variablen-Kollisionen (`ChapterVariableClash`) sind Compiler-Checks.
- **State als VarMap** (`chapter.current`, `chapter.visited.<id>`) — kein neues
  `SaveState`-Feld, Save-Bytes unangetastet; reserviertes Präfix (auch für
  Prozedur-Parameter).
- **Tests**: 3 Engine-Tests (**407**) + 3 Worldbuilder-Tests (**184**); Fixture
  `examples/fixtures/kapitel.yaml` (Auto-Gate, Sprünge, Rücksprung-Verweigerung) in
  CI-Stufe 4b.

### Wissensmodell: `facts:`, `combine:`, `learn`/`forget`/`knows` und Notizbuch (W1)

- **`facts:`-Sektion** (`AFactDef` → `FactDef`): Fakten mit `keys` (Wörter zum Ansprechen),
  Notizbuch-`text`, optionalem `source`, `tag` (Gruppierung), `learn_msg` (Meldung beim
  Lernen) und `silent` (pro Fakt). Kompiliert als **Liste** in Deklarationsreihenfolge —
  die Notizbuch-Reihenfolge ist ein Gameplay-Vertrag („nicht umsortieren“); leere Listen
  werden im `world.json` weggelassen.
- **Prädikat `Knows`** (`knows: <fact>` bzw. `{knows: <actor>, fact: <fact>}`): liest
  `known.<actor>.<fact>` (VarMap); Akteure haben getrennte Wissensstände (NPC-Schatten).
- **Effekte `learn`/`forget`**: Lernen ist **idempotent** (Set-Semantik); pro neu gelerntem
  Fakt feuert **`OnLearn`** einmal, in Lernreihenfolge. `forget` entfernt nur den genannten
  Fakt — es gibt kein automatisches Vergessen und keine Rückwärts-Kaskade.
- **`combine:`-Kaskade** als geschlossene Operation in der `Learn`-Anwendung (FIFO-Queue
  über die deklarierte Tabelle, jeder Fakt höchstens einmal, Prämissen gegen den
  fortschreibenden State) — **kein** Verlass auf `maxOutcomeDepth`, keine Trigger-Rekursion.
- **Meldungen**: nur der Spieler sieht Notizen (NPC-Lernen still); `silent: true`
  unterdrückt; Autor-`learn_msg`/`combine`-`msg` schlagen den Katalog-Default
  (`learn.default`, „Noted.“) — Katalog jetzt 187 Keys.
- **`kombiniere`-Befehl** (W1.4, compiler-generiert): pro `combine:`-Eintrag werden
  Trigger generiert (beide Argumentformen `X Y`/`X mit Y`, beide Reihenfolgen, Matching
  über `cmd.arg*` gegen `keys` per `VarIs`), **gated auf `Knows` beider Prämissen**
  (detektivische Fairness); ein generierter Fallback antwortet bei Nicht-Match
  („You cannot combine these like that.“). Das Verb wird in die Registry injiziert
  (Default `kombiniere`, Alias `combine`, per `combine_verb:` überschreibbar).
- **Notizbuch** (W1.5): `journal: notes` generiert den `notizen`-Befehl (Alias `notes`) →
  Effekt `ShowNotes`: gelernte, nicht-silente Spieler-Fakten in **Deklarationsreihenfolge**
  (ungetaggte zuerst, dann Tags in Erstauftretens-Reihenfolge), mit `(source)`. NPC-Wissen
  ist nie sichtbar (das ist die Detektivarbeit).
- **Checks** (`worldbuilder`): `UnknownFact` (learn/forget/knows/combine-Referenzen),
  `DuplicateFact`, `YieldsWithoutPremises`, `KnownVariableClash` (`known.` gehört der Engine;
  auch in den Proc-Param-Präfixen).
- **Tests**: 4 Engine-Tests (**404**) + 3 Worldbuilder-Tests (**181**); Fixture
  `examples/fixtures/wissen.yaml` (Lernen → Kombinieren → Notizbuch, plus Fehlschlags-Pfad)
  in CI-Stufe 4b.

### Content-Tests als Daten: `tests:`-Sektion und `worldbuilder test` (B1)

- **`tests:`-Sektion** (`worldbuilder/src/Worldbuilder/Types.hs`, `AContentTest`): der Autor
  schreibt eigene Regressionstests als Daten — `name` (optional, Default `unnamed`),
  `input: [befehle]` (Eingabefolge, Zeilen wie am Prompt) und `expect: [marker]`
  (**geordnete** Marker: jeder Marker muss in der gerenderten Ausgabe erscheinen, in
  deklarierter Reihenfolge — Teilfolgen-Semantik).
- **Runner `worldbuilder test <adventure.yaml> [name]`** (neues Modul
  `worldbuilder/src/Worldbuilder/Test.hs`): kompiliert das Adventure, spielt jeden Test in
  einem **frischen Zustand** durch (Loop-Kern `applyLoopCommandEv`, Start-Look eingeschlossen,
  Custom-Verben via `parseCommandWith`) und prüft die Marker. Reiner Lauf — `save`/`load`
  erzeugen ihre Meldungen, aber keine Dateien; nach Game Over werden keine Befehle mehr
  gefüttert. Optionaler Namensfilter (Argument ohne `-`).
- **Exit-Code:** ein fehlgeschlagener Test bricht mit Exit 1 ab — CI-tauglich.
- **CI-Stufe 4b** (`scripts/ci.sh`): `worldbuilder test` für Abenteuer mit `tests:`-Sektion
  (erweiterte Liste); das `procedures`-Fixture dogfoodet den eigenen Mechanismus.
- **Tests**: 2 Worldbuilder-Tests (**178** gesamt): Marker-Semantik/-Parsing und Runner-
  Round-Trip (bestehend + fehlgeschlagen) über eine temporäre Adventure-Datei.

### Procedures: `procedures:`-Block, `call:` und Parameter-Scope (Phase 2.5, D2)

- **`procedures:`-Block** (`worldbuilder/src/Worldbuilder/Types.hs`, `AProcDef`): benannte, parametrisierte Effekt-Bündel (`id`, `params: [..]`, `effects: [..]`), kompiliert nach `ProcDef` in `GameWorld.procDefs`. Eine leere Map wird im `world.json` **weggelassen** (ToJSON-Omission) — Byte-Identität und Checksumme bestehender Welten bleiben unverändert.
- **Effekt `CallProc`** (`src/Types/Core.hs` + `src/Effects.hs`): führt den Prozedurkörper mit literalen Argumenten aus. Parameter und Locals liegen in einem frischen Scope (`GameState.procScopes`, innermost-first, **runtime-only** — `GameState` hat keine JSON-Instanz, nichts wird gespeichert), der beim Verlassen verworfen wird. Namen ohne Scope-Binding schreiben in die VarMap wie bisher.
- **Scope-lesende Variablenauflösung** (`src/Game.hs`): `getVariable`, `resolveValueRef (VRVariable …)` und `lookupVarForFormat` (`{template}`) prüfen den Prozedur-Scope zuerst — Parameter und Locals schatten Adventure-Variablen (lesend wie schreibend).
- **`call:` im YAML**: `call: <name>` (ohne Argumente) oder `call: {proc: <name>, args: [literal, ...]}` — Argumente sind int/bool/text-**Literale** (kein dynamischer Aufruf), damit Aufrufstellen statisch prüfbar bleiben (dieselbe Eigenschaft wie `raise: name`).
- **Compiler-Checks** (`worldbuilder/src/Worldbuilder/Compile.hs`): `UnknownProc`, `ProcArity`, `DuplicateProc`, `ProcParamReserved` (Parameter dürfen nicht in engine-eigenen Namensräumen liegen: `cmd.`, `combat.`, `env.`, `faction.`, `party.`, `patrol.`, `ship.`, `stealth.`) und **`ProcRecursion`**: Zyklen im Call-Graph (auch indirekt) sind Compile-Fehler — `maxOutcomeDepth` bleibt Laufzeitschutz für verschachtelte Effekte und wird bewusst **nicht** für die Terminierung von Prozeduraufrufen herangezogen.
- **Veto-Semantik wie `Sequence`**: `block` im Prozedurkörper stoppt die restlichen Körpereffekte **und** die restlichen Effekte des Aufrufers (2.2-Semantik).
- **Meldungen**: `proc.unknown`, `proc.arity` (Message-Katalog jetzt 185 Keys).
- **Tests**: 6 Engine-Tests (**400** gesamt) + 5 Worldbuilder-Tests (**176** gesamt); E2E-Fixture `procedures` inkl. verschachteltem Aufruf (`examples/fixtures/procedures.yaml`, `ci/e2e/procedures.in/.expect`, CI-Stufe 4).

### Text-Erweiterung: Inline-Bedingungen, Ausdrücke via Expr-Parser und Props (Phase 2.4)

- **Inline-Bedingungen `{if <flag/var-cond>|a|b}`** (`src/Messages.hs`):
  - Syntax für bedingte Textverzweigungen in allen Templates (`msg`, `text`, Raum- und Item-Beschreibungen, Dialoge): `{if <cond>|then_branch|else_branch}` (optional ohne Else-Zweig: `{if <cond>|then_branch}` mit Default `""`).
  - Unterstützt Flag-Prüfungen (`{if has_torch|...|...}`), Negation (`{if !has_torch|...|...}`), numerische Vergleiche (`==`, `!=`, `/=`, `>=`, `<=`, `>`, `<`, `=`) und Text-Variablen-Vergleiche (`{if weather == rain|...|...}`).
  - Verschachtelte Klammern und Format-Platzhalter in den Zweigen werden rekursiv aufgelöst; Pipes können mit `\|` maskiert werden.
- **Ausdrücke `{= <expr>}` via vorhandenem `Expr`-Parser** (`src/Messages.hs`; nutzt den vorhandenen `Expr`-Parser aus `src/Types/Core.hs` unverändert):
  - Mathematische Berechnungen direkt in Text-Templates: `{= gold * 2}`, `{= (gold + bonus) * 2}`, `{= item.torch.fuel + 5}`.
  - Vollständige Wiederverwendung von `parseExpr` und `Expr` (Grundrechenarten, Klammern, `min`, `max`, `clamp`).
  - Format-Modifikatoren integrierbar: `{= gold * 2:+}` (Vorzeichen erzwingen), `{= gold * 2:6}` (Breiten-Padding). Division durch 0 ist zero-safe (`0`).
- **Entity-Props `{item.<id>.<prop>}` und `{npc.<id>.<prop>}`** (`src/Game.hs`; liest die vorhandenen `ItemState.itemProps`/`NPCState.npcProps` aus `src/Types/Core.hs` unverändert):
  - Zugriff auf ganzzahlige Props von Items (`ItemState.itemProps`) und NPCs (`NPCState.npcProps`, `npcHealth`) direkt in Templates, Bedingungen und Ausdrücken (z. B. `{item.torch.fuel}`, `{npc.guard.mood}`, `{npc.guard.hp}`).
  - `VRVariable` in `resolveValueRef` erweitert, sodass Props auch in `ValueRef` und Predicates (`compare_var`) transparent aufgelöst werden.
- **Fehlerbehandlung (kein stiller Durchfall des rohen Templates)**:
  - Unbekanntes Item/Prop: `<error: unknown item '<id>'>` bzw. `<error: unknown prop '<prop>' on item '<id>'>`.
  - Unbekannter NPC/Prop: `<error: unknown npc '<id>'>` bzw. `<error: unknown prop '<prop>' on npc '<id>'>`.
  - Unbekannte Variable im Ausdruck: `<error: unknown variable '<name>'>`.
  - Syntaxfehler/unbekannter Befehl im Ausdruck: `<error: expr: <reason>>`.
  - Kaputte If-Syntax: `<error: invalid if syntax: expected {if <cond>|a|b}>` bzw. `<error: invalid if condition: <cond>>`.
  - Bestehende unqualifizierte Platzhalter `{unknown}` bleiben für Abwärtskompatibilität unverändert.
- **Compiler & Worldbuilder** (`worldbuilder/src/Worldbuilder/Compile.hs`):
  - `isKnownPlaceholder` erkennt `item.*`, `npc.*`, `flag.*`, `flag:*` und `condition_turns.*`.
  - `extractPlaceholders` filtert `{if ...}` und `{= ...}` vorab, sodass keine Fehlwarnungen (`UnknownPlaceholder`) erzeugt werden.
- **Tests & Qualität**:
  - 4 neue Unit-Tests mit 50 Assertions in `test/Tests.hs` (**394 Engine-Tests** gemessen, vorher 390).
  - **171 Worldbuilder-Tests** gemessen (unverändert grün).
  - **20 TUI-Tests** gemessen (unverändert grün).
  - **46 Loop-Einträge** in ci.sh-Stufe 4/5 (gemessen, unverändert) + 1 Worldgen-Eingabe; `scripts/ci.sh` grün.
  - Byte-Identität: bestehende E2E-Läufe und generierte Welten/Saves (`thefog`, etc.) byte-identisch.
  - Abwärtskompatibilität: mit dem Vorher-Stand (`38584d6`) via `git worktree add` kompilierte Welt und Save laden mit dem neuen Stand ohne Versionswarnung, `worldChecksum` stabil.
  - Message-Katalog **183 Keys** unverändert (`scripts/check-msg-catalog.sh` grün).
  - 0 Compiler-Warnungen im kalten Build (`cabal clean && cabal build all --enable-tests`).

### Disambiguation: EvDisambiguate, numerierte Rückfrage, Antwort ohne Zugverbrauch (Phase 2.3)

- **Event `EvDisambiguate [String]`** (`src/Types/Output.hs`, `docs/protocol-v1.md`, `test/golden/protocol/server_events.json`):
  - Neuer Konstruktor in `OutputEvent` mit den Kandidaten-IDs in Reihenfolge der Rückfrage; das Event ist textfrei (`evTextOf` = `""`), die CLI-Ausgabe bleibt damit byte-identisch.
  - Protokoll-Typ `"disambiguate"` mit Feld `candidates`; Event-Katalog der Spezifikation auf 13 Events erweitert, Golden-Datei um den Event ergänzt (restliche Bytes unverändert).
- **Numerierte Rückfrage** (`src/Parser.hs`, `src/Messages.hs`):
  - `interactAmbiguous` liefert jetzt `EvDisambiguate` plus die Rückfrage „Which do you mean: [1] …, [2] …?“ — der bestehende Katalog-Key `disambiguate.prompt` bekommt die numerierte Liste, der neue Key `disambiguate.option` (`[{n}] {name}`) hält die Nummerierung im Katalog (kein hartkodierter Spielertext).
- **Antwort per Zahl oder unterscheidendem Wort** (`src/GameLoop.hs`, `src/Parser.hs`, `src/Types/Core.hs`):
  - Neuer Laufzeitzustand `LoopState.lsPendingDisambiguation :: Maybe PendingDisambiguation` (Kandidaten-IDs + auslösendes Kommando); `LoopState` wird nicht serialisiert, Saves und Welten bleiben unberührt.
  - `resolveDisambiguationAnswer` nimmt eine 1-basierte Zahl (der Parser macht aus `1` ein `ChooseCmd`) oder ein Wort, das genau einen Kandidaten beschreibt (ID, Anzeigename oder Keywords); sonst gilt die Eingabe nicht als Antwort.
  - `GameState.chosenTarget :: Maybe String` (reiner Laufzeitzustand wie `lastVeto`) lässt die Wiederholung exakt auf der gewählten Entity-ID auflösen — nötig, weil die ID eines Kandidaten zugleich Keyword eines anderen sein kann; `resolveTarget` gewährt ihr Vorrang und der Wert wird nach dem Wiederholen wieder gelöscht.
  - Nicht als Antwort interpretierbare Eingaben laufen als normales Kommando und schließen die Rückfrage.
- **Kein Zugverbrauch** (`src/GameLoop.hs`): die Antwort läuft über `runCommandNoTurn` — kein `turnCount`-Increment, kein Condition-Tick, kein `on: turn` (`fireCommandTriggersSkipping` schließt den Turn-Event aus). Die mehrdeutige Eingabe selbst kostet wie bisher ihren normalen Zug.
- **E2E-Fixtures**:
  - `examples/fixtures/disambiguation.yaml` mit zwei mehrdeutigen Gruppen (zwei Edelsteine, zwei Schlüssel) und einem Zähler-Report.
  - `ci/e2e/disambiguation.{in,expect}` (Stufe 4): Antwort per Zahl und per unterscheidendem Wort; der Marker `ZUGZAEHLER-4` belegt, dass die Antworten die Uhr nicht weiterstellen.
  - `ci/e2e/disambiguation-fallback.{in,expect}` (Stufe 5): eine nicht unterscheidende Eingabe läuft als normales Kommando und schließt die Rückfrage.
- **Tests & Qualität**:
  - 5 neue Unit-Tests in `test/Tests.hs` (**390 Engine-Tests** gemessen, vorher 385).
  - **46 Loop-Einträge** in ci.sh-Stufe 4/5 (gemessen, vorher 44) + 1 Worldgen-Eingabe; `scripts/ci.sh` grün („All checks passed.“).
  - Byte-Identität: alle **45** beiden Revisionen gemeinsamen E2E-Eingaben (44 Playthroughs + Worldgen) liefern identische `world.json`, `save.json` und stdout+stderr (Vollvergleich, gleiche absolute Pfade).
  - Abwärtskompatibilität: mit dem Vorher-Stand kompilierte Welt und Save laden mit dem neuen Stand ohne Versionswarnung, `worldChecksum` stabil (`2442544942641501920`).
  - Message-Katalog **183 Keys** (vorher 182, neuer Key `disambiguate.option`); `scripts/check-msg-catalog.sh` grün.
  - 0 Compiler-Warnungen (`cabal clean && cabal build all --enable-tests`).

### Veto Stufe 1 (D3): OnBefore, Block, cmd.target und Guarded Exit (Phase 2.2)

- **Event `OnBefore verb` und `Block`-Effekt** (`src/Types/Core.hs`, `src/Effects.hs`, `src/Parser.hs`, `src/GameLoop.hs`):
  - Neuer Event-Typ `OnBefore String` in `EventType`, der vor der Ausführung eines Befehls getriggert wird.
  - Neuer Effekt `Block (Maybe String) Bool` im `Effect`-Typ mit opt-in `consumesTurn`-Flag (`block: "msg"`, `block: true`, `block: false`, oder Objektform `block: { msg: "...", turn: true }`).
  - Veto-Zustand `lastVeto :: Maybe Bool` im `GameState` (reiner Laufzeitzustand, nicht in Saves persistiert).
  - Veto-Semantik: Alle passenden Before-Regeln laufen in Definitionsreihenfolge. Der erste `Block`-Effekt setzt das Veto; nachfolgende Effekte der blockierenden Regel und alle verbleibenden Before-Regeln werden sofort gestoppt.
  - Zugverbrauch bei Veto: Standardmäßig verbraucht ein geblocktes Kommando keinen Zug (`consumesTurn = False`), d. h. der Rundenzähler bleibt unverändert, Conditions ticken nicht und NPCs bewegen sich nicht. Mit `turn: true` verbraucht die geblockte Aktion einen Zug und lässt die Ticks voranschreiten.
- **Variablen `cmd.target` und `cmd.target_kind`** (`src/Parser.hs`):
  - Vor der Regelauswertung und vor der Befehlsausführung bindet `bindCommandVars` (über `resolveCmdTarget`) die Ziel-Informationen des Spielers:
    - `cmd.target`: aufgelöste Entity-ID (z. B. `"relic"` oder Zielraum bei Bewegung) bzw. rohe Eingabe.
    - `cmd.target_kind`: Art des Ziels (`"item"`, `"npc"`, `"vehicle"`, `"room"`, `"choice"`, `"all"`, `"ambiguous"`, `"none"`).
- **Guarded Exit mit `when:`-Prädikat und Fehltext** (`src/Types/Core.hs`, `src/Parser.hs`, `src/Validate.hs`, `src/Types/Protocol.hs`, `worldbuilder/src/Worldbuilder/Compile.hs`, `worldbuilder/src/Worldbuilder/Types.hs`, `text-adventure-tui/src/TextAdventure/Tui/Hud.hs`):
  - Neuer Ausgangs-Konstruktor `Guarded String Predicate (Maybe String)` in `Exit`.
  - Bewegungsauswertung in `Parser.dispatchCommandEv (Go dir)`: Bei erfülltem Prädikat gelingt der Raumwechsel; andernfalls wird die Bewegung blockiert und der konfigurierte Fehltext `msg:` (bzw. `message:`, oder der Katalog-Default `move.blocked`) ausgegeben.
  - Aeson-Serialisierung und Validierung voll integriert; Snapshots und TUI melden `Guarded` als verriegelt (`locked: true`).
- **Worldbuilder & Schema**:
  - `AExitRef` um `aeWhen :: Maybe Predicate` und `aeMsg :: Maybe String` erweitert.
  - `AOBlock (Maybe String) Bool` in `AActionOutcome` mit String-, Bool- und Objekt-Syntax.
  - `knownKeys EntExitRef` um `"when"`, `"msg"`, `"message"` ergänzt.
  - Autoren-Dokumentation in `docs/adventure-schema.md` auf Englisch gepflegt.
- **E2E-Fixtures Wächter und Traglast**:
  - `examples/fixtures/waechter.yaml`, `ci/e2e/waechter.in`, `ci/e2e/waechter.expect`: Testet Guarded Exit (`when: has_flag pass_granted`, `msg: ...`) und `on: before take` mit `block:`.
  - `examples/fixtures/traglast.yaml`, `ci/e2e/traglast.in`, `ci/e2e/traglast.expect`: Testet Traglast-Limitierung über `on: before take` mit `cmd.target`/`cmd.target_kind`, Kapazitätscheck mit Veto, sowie `block: { turn: true }`.
  - Beide Fixtures in `scripts/ci.sh` (Stufe 4 Playthroughs) mit eindeutigen, ausgangszustands-fremden Markern registriert.
- **Tests & Qualität**:
  - 5 neue Unit-Tests in `test/Tests.hs` (**385 Engine-Tests** gemessen, vorher 380).
  - 2 neue Unit-Tests in `worldbuilder/test/Tests.hs` (**171 Worldbuilder-Tests** gemessen, vorher 169).
  - Abwärtskompatibilität verifiziert: Mit dem Vorher-Stand kompilierte Welt (`thefog`) und Save (`save.json`) laden mit dem neuen Stand byte-identisch ohne Checksum-Warnung (`BYTE-IDENTICAL`).
  - `scripts/ci.sh` grün (alle 44 E2E-Läufe erfolgreich und bisherige 42 Eingaben unverändert).
  - Message-Katalog unverändert (182 Keys, `check-msg-catalog.sh` PASS).
  - 0 Compiler-Warnungen (`cabal clean && cabal build all --enable-tests`).

### Timer abfragbar: Predicate has_condition, ValueRef condition_turns, hidden Condition (Phase 2.1)

- **Predicate `has_condition: <name>`** (`src/Types/Core.hs`, `src/Game.hs`):
  - Neues Prädikat `HasCondition String` im Core-AST, JSON-Repräsentation `{"has_condition": "<name>"}`.
  - Autoren-Schnittstelle in `when:`, `if:`, `visible_when:` (z. B. `{ has_condition: bomb }`).
  - Evaluierung prüft, ob die angegebene Condition im aktuellen `conditions (save st)` aktiv ist.
- **ValueRef `condition_turns: <name>`** (`src/Types/Core.hs`, `src/Game.hs`):
  - Neuer Konstruktor `VRConditionTurns String` im `ValueRef`-Typ, JSON-Repräsentation `{"condition_turns": "<name>"}` und String-Punktsyntax `"condition_turns.<name>"`.
  - Auflösung in `resolveValueRef` liefert die verbleibenden Züge einer aktiven Condition, oder `0` wenn die Condition inaktiv oder abgelaufen ist.
  - Verwendbar in `compare:`-Prädikaten (z. B. `compare: { lhs: { condition_turns: bomb }, op: "<=", rhs: 2 }`) sowie `condition_turns:`-Shorthand.
  - `CompareVar` und `VRVariable` unterstützen `"condition_turns.<name>"` als Fallback.
  - `Comparator` unterstützt neben String-Symbolen (`<=`, `>=`, `==` etc.) auch die Konstruktornamen (`CLte`, `CGt` etc.) für vollständige JSON-Round-Trips.
- **Feld `hidden: true` für Conditions** (`src/Types/Core.hs`, `worldbuilder/src/Worldbuilder/Types.hs`, `src/Parser.hs`, `src/Types/Protocol.hs`, `text-adventure-tui/src/TextAdventure/Tui/Hud.hs`):
  - `Condition` um `condHidden :: Bool` erweitert.
  - `ApplyCondition` im `Effect`-Typ um `Bool` erweitert mit abwärtskompatiblem 4-Feld-Fallback beim JSON-Dekodieren.
  - `hidden: true` unterdrückt die Condition in der Spieler-Statusanzeige (`stats`-Befehl), im `PlayerSnapshot` des Protokolls (`psConditions`) und in der TUI-HUD-Anzeige.
  - Abwärtskompatible Aeson-Serialisierung: `condHidden: false` wird beim Kodieren weggelassen (Projektprinzip: kein Diff auf bestehenden Saves und Welten, World-Checksum bleibt stabil). Ältere Saves ohne `condHidden` dekodieren mit Default `False`.
- **E2E-Fixture Bombe (`examples/fixtures/bomb.yaml`)**:
  - Bildet ein Entschärfungsszenario ab: versteckte Bombe (`hidden: true`, 4 Züge), Entschärfer prüft `has_condition: bomb` vor `clear_condition: bomb`, und Warnregel triggert auf `condition_turns <= 2` erst nach Scharfschaltung.
  - In `scripts/ci.sh` (Stufe 4 Playthroughs) registriert (`ci/e2e/bomb.in`, `ci/e2e/bomb.expect`).
- **Tests & Qualität**:
  - 4 neue Unit-Tests in `test/Tests.hs` (**380 Tests** gesamt, vorher 376).
  - Vorher-Stand-Save und -Welt verifiziert: Save lädt ohne Checksum-Warnung, World-Checksumme identisch.
  - `scripts/ci.sh` grün (alle Stufen PASS, 42 bestehende E2E-Läufe byte-identisch).
  - 0 Compiler-Warnungen (`cabal clean && ./scripts/ci.sh`).

### Protokoll v1: Typen, Codec, Golden-Tests und Spezifikation (Phase 1.4)

- **Protokoll v1 als Typen und Codec ohne Transport** (`src/Types/Protocol.hs`):
  - Versioniertes Wire-Protokoll mit `currentProtocolVersion = 1`.
  - `ClientMsg` mit Nachrichtenarten:
    - `command`: regulaeres Textkommando (`command`)
    - `choose`: Dialog-/Menue-Auswahl per Index (`index`)
    - `continue`: Weiterblaettern bei Narrativ-/Pausen-Zustaenden
    - `load_world`: Weltdatei neu laden (`path`)
    - `save`: Spielstand in Slot speichern (`slot`)
    - `load`: Spielstand aus Slot laden (`slot`)
  - `ServerMsg` mit Nachrichtenarten:
    - `events`: Liste strukturierter Ausgabeevents (`events: [OutputEvent]`)
    - `snapshot`: Kompakter Zustands-Snapshot fuer UI-HUD, Map, Quests und Dialoge
    - `error`: Protokollfehler (`code`, `message` — ein `details`-Feld gibt es nicht) mit vier Fehlercodes als snake_case-Werte: `version_mismatch`, `unknown_type`, `malformed_payload`, `session_error`. Ein unbekannter `type`-Diskriminator wird als `unknown_type` gemeldet (in beiden Richtungen getestet), ein bekannter Typ mit kaputter Nutzlast bleibt `malformed_payload`; `session_error` ist reserviert und hat noch keinen Produzenten.
  - Strukturierte Snapshots fuer Frontends: `Snapshot` mit `turn`, `player`, `room`, `quests`, `dialogue`, `combat`, `game_over` und `visited_rooms`; darunter `PlayerSnapshot` (health/max_health/attack/defense, optional gold, conditions, equipment, inventory), `RoomSnapshot` (id, name, description, exits samt Lock-Status, items, npcs, vehicle), `ConditionSnapshot` (name, remaining turns), `QuestSnapshot` (aktive und abgeschlossene Quests), `DialogueSnapshot` (NPC, Knoten, Text, Auswahl-Optionen), `CombatSnapshot` (`engaged`) und `GameOverSnapshot` (`reason`, `menu`).
  - Extraktionsfunktion `makeSnapshot :: GameState -> Snapshot` fuer den Praesentations-Tier.
  - Striktes Parsing mit Versionsvalidierung (`decodeClientMsg`, `decodeServerMsg`): abweichende oder fehlende Versionen werden mit `version_mismatch` zurueckgewiesen, unbekannte Nachrichtentypen mit `unknown_type`, strukturell falsche Nutzlast mit `malformed_payload`.
- **Deterministische Golden-Tests:**
  - `encodeSorted` garantiert stabile Bytes auf allen Verschachtelungsebenen via `aeson-pretty` (`confCompare = compare`).
  - 9 Golden-JSON-Referenzdateien in `test/golden/protocol/` (`client_command.json`, `client_choose.json`, `client_continue.json`, `client_load_world.json`, `client_save.json`, `client_load.json`, `server_events.json`, `server_snapshot.json`, `server_error.json`).
  - Byte-Stabilitaet belegt der Test durch den Vergleich der kodierten Bytes gegen die 9 Golden-Dateien auf der Platte (Determinismus ueber Prozessgrenzen hinweg). Der zusaetzliche `run1 == run2`-Vergleich im Test kodiert dagegen **nicht** zweimal — beide Bindungen teilen denselben Thunk — und traegt nichts bei.
- **Aeson-Serialisierung fuer alle 12 Ausgabeevents** (`src/Types/Output.hs`):
  - `ToJSON` und `FromJSON` Instanzen fuer `OutputEvent` (`message`, `text`, `art`, `anim`, `sfx`, `music_start`, `music_stop`, `room_changed`, `quest_update`, `dialogue`, `combat`, `game_over`).
  - Serialisierung fuer `Style`, `Span`, `StyledText`, `Color`, `MsgPayload`, `ArtHotspot` und `ArtPayload`.
- **Session-Uebergangs-Bruecke (Constraint 6):**
  - Session-Uebergaenge aus Phase 1.3 (`transitionSave`, `transitionLoad`, `transitionRestart`, `transitionGameOver`, `advanceNarrative`) liefern intern `[String]`.
  - An der Protokollgrenze werden diese Zeilen durch `sessionLinesToEvents :: [String] -> [OutputEvent]` als `EvText (styledText line)` eingekleidet, sodass das Wire-Format einheitlich `[OutputEvent]` bleibt (dokumentiert in `docs/protocol-v1.md`).
- **Protokoll-Spezifikation** (`docs/protocol-v1.md`):
  - Vollstaendiges englisches Referenzdokument fuer Frontend- und Backend-Autoren (Nachrichten, vollstaendiger 12-Event-Katalog, Snapshot-Schema, Fehlercodes und Versionierungsrichtlinien).
- **Test- und Build-Nachweise:**
  - 6 neue Unit- und Golden-Tests in `test/Tests.hs` (**376 Tests** gesamt, vorher 370).
  - Nachtrag (2026-09-28): unbekannte `type`-Diskriminatoren werden als `unknown_type` klassifiziert statt als generische kaputte Nutzlast — drei neue Zusicherungen im Fehlertest (Client, Server, Gegenprobe „bekannter Typ mit kaputter Nutzlast"), per Fehlerinjektion als wirksam belegt. Der vakuume `run1 == run2`-Vergleich im Golden-Test ist entfernt: er kodierte nur einmal (geteilter Thunk).
  - `cabal clean && cabal build all --enable-tests` mit **0 Warnungen**.
  - `scripts/ci.sh` vollstaendig gruen (alle 8 Stufen PASS).

### CI: save/load-Rundlauf als Stufe 8

- **Befund aus der 1.3-Prüfung:** kein einziges E2E-Fixture nutzt `save` oder
  `load`. Die Persistenz-Verdrahtung (Save-/Load-Zweig in `loopGame`, Lade-Zweig
  des Todesmenüs) hatte damit keine End-to-End-Abdeckung — die Byte-Vergleiche
  über `ci/e2e/*.in` berühren sie nicht.
- Stufe 8 fährt je Fall **zwei** Läufe gegen ein isoliertes `TA_SAVES_DIR`:
  Lauf 1 schreibt den Slot, Lauf 2 lädt ihn. Fälle: `thefog` (normaler
  Ladebefehl) und `patrol` (Tod → `l` im Todesmenü → laden).
- Die Marker sind absichtlich zustandsabhängig: `Game loaded from` **plus**
  `Inventory: map` (gespeichert nach `take map`). Ein Raumname hätte auch im
  frisch gestarteten Spiel gematcht — belegt durch Fehlerinjektion: mit einem
  nicht ladenden `ReqLoad` fallen beide Marker und die Stufe bricht ab.

### Purer Session-Automat: Save/Load/Restart/GameOver/Death/Victory (Phase 1.3)

- Session-Logik aus den interaktiven Loops (`loopGame`, `deathLoop`, `victoryLoop`, `runRestart`)
  in pure Zustandsübergänge herausgezogen:
  - Neuer Request-Typ `SessionRequest`: `ReqSave String`, `ReqLoad String`,
    `ReqPersistMeta`, `ReqPause`, `ReqDeleteSave String`, `ReqListSaves`
    (Alias `type IoRequest = SessionRequest`). Ein parameterloser ADT ohne
    Continuations: der pure Übergang gibt den Wunsch zurück, der Interpreter
    führt ihn aus.
  - Neuer Session-Zustand `SessionState`: `SessionPlaying LoopState`,
    `SessionDeath LoopState`, `SessionDeathPromptLoad LoopState`,
    `SessionVictory LoopState GameOverReason`, `SessionEnded`.
  - Pure Übergänge in `src/GameLoop.hs`: `transitionSave` (Policy-Check, Save-Erzeugung), `transitionLoad` (Policy-Check), `transitionLoadSuccess` (Meta-Merge, RNG-Reseed), `transitionListSaves`, `transitionRestart` (Run-Index bump, Seed-Reseed), `transitionGameOver` (Verzweigung Death/Victory), `transitionDeathUndo`, `transitionDeathCanLoad`, `transitionDeathLoadSlot`, `transitionDeathInput`, `transitionVictoryInput` und `advanceNarrative` (erzeugt `[ReqPause]` für Zwischenschritte).
- `loopGame`, `deathLoop`, `victoryLoop` und `runRestart` sind dünne Interpreter,
  die IO-Wünsche über `executeRequest` ausführen und die puren Übergänge weiterschalten.
  Die Übergänge liefern ein Tupel aus neuem Zustand, IO-Wünschen und fertig
  gerenderten Ausgabezeilen — meist `(Zustand, [SessionRequest], [String])`,
  bei den Menü-Helfern schmaler (`transitionLoadSuccess :: … -> (LoopState,
  [String])`, `transitionDeathCanLoad :: … -> (Bool, [String])`,
  `transitionDeathLoadSlot :: … -> (String, SessionRequest)`). Die Session-Schicht
  nutzt bewusst noch nicht den `[OutputEvent]`-Strom aus 1.2.
- **`ReqLoad` wird jetzt wirklich ausgeführt.** `executeRequest` liefert ein
  Ergebnis (`data RequestOutcome = Loaded GameState (Map String VariableValue)`,
  exportiert), sodass der Interpreter den Slot selbst lädt (`loadGame` plus
  `loadMeta` des geladenen World). Die Loops reichen die Antwort an
  `transitionLoadSuccess` weiter — der zuvor inline duplizierte Ladeblock in
  `loopGame` und `deathLoop` ist entfallen. Alle sechs Wünsche laufen damit über
  denselben Ausführer, und die IO-Antwort ist Teil der testbaren Schnittstelle.
- Spiel-Logik (Save/Load-Regeln, Death/Victory-Menü-Auswahl, Pause-Sequenzen bei Narrativen) ist vollständig ohne IO unit-testbar.
- **7 neue Unit-Tests** in `test/Tests.hs` für alle puren Übergänge; **370 Engine-Tests** (vorher 363, alle PASS).
- **Akzeptanzkriterium erfüllt:** alle **42 E2E-Fixtures** (`ci/e2e/*.in`) vor und nach dem Umbau gegen isolierte `TA_SAVES_DIR` durchgespielt; vollständige stdout sowie persistierte `world.json`/`save.json` sind **byte-identisch** (`diff -r` leer).
- Build mit **0 Warnungen** (geprüft mit `cabal clean && ./scripts/ci.sh`).

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
- **Build wieder warnungsfrei:** Phase 1.1/1.2 hinterließ **18 Warnungen** (17 davon in der Linux-Sicht) —
  14 im Engine-Build (12 tote Import-Einträge bzw. `Types.Output`-Importe doppelt
  neben der `Types`-Fassade, dazu zwei durch den Umbau verwaiste Bindungen
  `Parser.withAscii` und `GameLoop.combineMessages`) und 3 im Test-Build (zwei
  redundante Importe, ein ungenutztes `ls1`). Sie blieben unentdeckt, weil der
  letzte `ci.sh`-Lauf des Batches auf dem Doku-Commit saß: `cabal` kompilierte
  nichts neu, also meldete der Warnungs-Gate nichts. **`touch` und
  `-fforce-recomp` erzwingen keinen Neubau** — cabal entscheidet über Inhalts-
  Hashes der Dateien; verlässlich ist nur `cabal clean` vor dem Build (oder eine
  echte Inhaltsänderung).
- **Nachtrag (ein Lauf später):** der erste Push war trotzdem rot — auf Windows
  meldete `src/Types/Core.hs:196` den ungenutzten Typ `CommandResultEv`
  (`-Wunused-top-binds`). Er stand nicht in der Exportliste von `Types.Core` und
  wurde nirgends verwendet, nur in Kommentaren erwähnt; auf Linux blieb er
  unentdeckt, weil das Modul nie neu kompiliert wurde, während der
  Windows-Runner kalt baut. Behoben, indem der Alias seine dokumentierte Rolle
  bekommt: `CommandResultEv` ist jetzt exportiert und
  `executeCommandEv :: Command -> GameState -> CommandResultEv`. Danach
  `cabal clean` + voller Build: 0 Warnungen auf beiden Plattformen.
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
