# Bekannte Befunde — text-adventure-Engine

**Stand:** 2026-10-11 · gesammelt beim Zork-I-Port (`text-adventure/examples/zork1.yaml`),
Stufen A–F · **Vollständige Fassung** zu `plan-zork-port.md` §5 ·
**Update 2026-10-08:** `OPEN-01`..`OPEN-09` sind umgesetzt (Commits in §7) ·
**Update 2026-10-11:** `OPEN-14`..`OPEN-23` aus `plan-zork-mechanik-entscheidungen.md` eingetragen ·
**Update 2026-10-11 (spät):** `OPEN-15` (`steal:`) und `OPEN-20` (`swap_inventory:`) umgesetzt
**Update 2026-10-12:** `OPEN-14` (`wander:`) umgesetzt — inkl. Run-Seed (`save.runSeed`),
Patrol-Modi `mode: pingpong|random` und Dieb-Umbau im Zork-Port (`thief-wanders` → `wander:`)
**Update 2026-10-12 (spät):** `OPEN-19` (Multi-Raum-Objekte, `locations:`) umgesetzt —
`grating`/`grating-bottom` im Zork-Port wieder zu einem Gitter verschmolzen
**Update 2026-10-13:** `OPEN-22` (`cmd.raw`) umgesetzt — der Loud Room hört mit
(echo/bug/Mitlesen unbekannter Wörter), die Wasserkopplung (XNOR) ist originalgetreu
**Update 2026-10-13 (spät):** `OPEN-23` zu generalisierten Gefahrenlagen (`hazards:`) — Stufe 1 (AP A–F) nach `docs/plan-hazards.md` vollständig umgesetzt

**Update 2026-10-14:** `OPEN-24` (Fahrzeug-Zustandsänderung) vollständig umgesetzt —
`apply_vehicle_condition`/`clear_vehicle_condition`/`vehicle_has_condition` nach
`docs/detailplan_vehicle_condition.md`; `OPEN-23a` (Zork-Dam-Umbau) wartet weiter.

**Zweck:** Alles, was beim Bauen von Abenteuern und Regeln stolpert oder gestolpert hat —
Laufzeit-Semantik, Workarounds, behobene Engine-Bugs und offene Engine-Punkte. Kein
Ersatz für `text-adventure/docs/adventure-schema.md`, sondern die „Fallstricke, die dort
nicht (oder zu gut versteckt) stehen".

**Aufbau:** Befunde tragen IDs, damit Pläne und Tickets darauf verweisen können.

| Präfix | Bedeutung |
|---|---|
| `R-##` | Regeln & Trigger |
| `Z-##` | Zustand, Items, Container |
| `E-##` | Effekte & Zielmengen |
| `P-##` | Parser & Verben |
| `W-##` | Werkzeuge, Validierung, Testen |
| `FIX-##` | **behobene** Engine-Bugs (mit Commit) |
| `OPEN-##` | **offene** Engine-Punkte / Wünsche |

Status in der Tabelle je Abschnitt: ✅ behoben · ⚙️ Workaround im Port · 🔧 offener Punkt.

---

## 0. Die fünf teuersten Klebestellen (kurz)

1. **`R-01`** — Regeln feuern nacheinander gegen den **schon geänderten** Zustand.
   Zwei `when:`-Guard-Regeln sind **nicht** disjunkt. → eine Regel mit `if/else`. *(offen)*
2. **`Z-06`** — **`verb_map` hat keinen Fallback über den Zustand:** ein Eintrag `wind:`
   matcht nur `itemStatus == "intact"`. Seit `OPEN-02` schreibt `set_state` `itemStatus` mit
   — **pro Zustand ein eigener Eintrag** (`wind,intact:` / `wind,broken:`), sonst stirbt die
   Reaktion nach dem Statuswechsel still. *(2026-10-09 zwei Regressionen verursacht)*
3. **`Z-12`** — **`portable: false` verschluckt `instead:take`** (Take-Guards laufen vor
   jeder `verb_map`-Phase). Eigene Take-Reaktionen brauchen ein tragbares Item.
   *(2026-10-09: der Skelett-Fluch lief nie)*
4. **`R-05`** — `block:` stoppt die **restlichen Effekte der eigenen Regel**. `move:` vorher. *(offen)*
5. **`R-03`/`R-02`** — `on: command` sieht den *End*zustand und feuert auch bei
   Fehlversuchen; `on: before` den Vorzustand. Seit `OPEN-01` an **`cmd.succeeded`**
   erkennbar. *(offen, aber abgesichert)*

Früher teuer, jetzt behoben: **`P-04`** (Container-Kommandos dispatchten nicht über
`verb_map` — das Gitter-Rätsel war tot, `FIX-08`) und **`R-08`** (Bewegungen zeigten
keinen Zielraum, `FIX-07`).

---

## 1. Regeln & Trigger (`R-##`)

| ID | Status | Befund |
|---|---|---|
| `R-01` | ⚙️ | **Regeln feuern in Definitionsreihenfolge gegen den bereits geänderten Zustand.** Zwei Regeln mit `when:`-Guard sind damit nicht automatisch disjunkt: feuert Regel A und ändert dabei einen von Regel Bs Guard geprüchten Zustand, feuert B trotzdem. **Lösung:** *eine* Regel mit `if/`else` statt mehrerer Regeln. |
| `R-02` | ⚙️ | **`on: command` feuert auch bei fehlgeschlagenen Befehlen** (Semantik „versucht", wie dokumentiert). Regeln brauchen eigene Zustands-Guards (`actor_has`, `has_item`, Flags). |
| `R-03` | ⚙️ | **`on: command` feuert gegen den *End*zustand des Kommandos.** `on: command put` sieht das abgelegte Item nicht mehr (`has_item` ist dann falsch). Wer den **Vor**zustand prüfen will, nimmt `on: before <verb>` — ohne `block:` läuft die Standardaktion danach weiter. |
| `R-04` | ⚙️ | **`on: before <verb>` + `block:` ist der saubere Weg für *eingebaute* Kommandos** (`give`, `board`, `open`, `take` …): die Effekte **vor** dem `block:` laufen, seine `msg:` ist die **einzige** Ausgabe. Kein „You don't see 'x' here." + Regeltext mehr. |
| `R-05` | ⚙️ | **`block:` stoppt die restlichen Effekte der eigenen Regel** („Trailing effects … do not run"). `move:`, `set_state:` o. ä. muss **vor** dem `block:` stehen. |
| `R-06` | ✅ | **`read` ist kein `on: command`-Trigger** — `read` ist ein eingebautes Item-Verb und als `ReservedVerbName` gesperrt. Buch/Leaflet laufen über `verb_map: read:` am Item (→ `P-04`). |
| `R-07` | ✅ | **`commandVerbName` warf 17 eingebaute Kommandos auf `"unknown"`** — `on: command give`/`put`/`open`/… konnten nie feuern. Behoben `FIX-01`. |
| `R-08` | ✅ | **`move:` gab keine Raumbeschreibung aus** — *keine* Bewegung tat es: nach `go <dir>` zeigte die CLI nur „You move {dir}.", der Zielraum erst nach manuellem `look`. Behoben `FIX-07` (Auto-Describe nach jedem Raumwechsel); eigene `msg:`-Texte davor sind jetzt nur noch für die *Handlung*, nicht mehr für die Orientierung nötig. |
| `R-09` | ⚙️ | **`cmd.*`-Variablen sind pro Kommando gebunden und danach verbraucht** — `cmd.arg1..N` sind nur in Regeln des *aktuellen* Kommandos aussagekräftig. |
| `R-10` | ⚙️ | **`once: true`** feuert genau einmal pro Spiel (Anti-Inflation für Punkte-Regeln). Punkte-Effekte in `if/`else`-Zweigen legt man am besten zusätzlich in die Bedingung, nicht nur in den Effekt. |

**Regel-Fallstrick in Reinform** (aus dem Port):

```yaml
# FALSCH: nach `land-river-1` ist der Spieler in dam-base — dann trifft die
# Fallback-Regel zu und meldet zusätzlich "You can't land here."
- id: land-river-1
  on: command land
  when: {at: player, room: river-1}
  effects: [ {move: dam-base} ]
- id: land-nowhere
  on: command land
  when: {not: {any: [{at: player, room: river-1}, …]}}
  effects: [ {msg: "You can't land here."} ]

# RICHTIG: eine Regel, if/else — `R-01`
- id: land
  on: command land
  effects:
    - if: {at: player, room: river-1}
      then: [ {msg: "…"}, {move: dam-base} ]
      else:
        - if: {at: player, room: river-2}
          then: [ {msg: "There is no safe landing spot here."} ]
          else: [ … ]
```

---

## 2. Zustand, Items, Container (`Z-##`)

| ID | Status | Befund |
|---|---|---|
| `Z-01` | ✅ | **`set_state` und der `verb_map`-Zustands-Lookup sprechen verschiedene Speicher.** `set_state: X, to: Y` schreibt `entityStates` (bei NPCs `npcStates`) — `runVerbMapEntry` (`Parser.hs:1712`) liest aber `itemStatus` (`itemStates`). Zustands-Suffixe wie `wind,broken:` landen deshalb auf `intact`/`unknown` und greifen nach `set_state` **nie**. **Workaround:** keine Zustands-Suffixe, sondern **ein** Eintrag mit `if: {state: X, is: Y}` (→ `Z-02`). |
| `Z-02` | ⚙️ | **`{state: X, is: Y}` liest alle Zustands-Layer** (`Game.hs:1512`): `entityStates`, `npcStates`, `itemStates.itemStatus`, Karten-Stapel und Textvariablen. Deshalb ist `if: {state: X, is: Y}` der robuste Ersatz für `Z-01`. |
| `Z-03` | ✅ | **`actor_has` prüft nur `CarriedBy`** (`Game.hs:539`), **nicht** `InContainer`. „Die Kohle ist in der Maschine" ist damit **kein** Prädikat. Workaround: das `put X in Y` per `on: before put` in ein Flag merken (`coal-in-machine`). |
| `Z-04` | ✅ | **`search` findet *alle* versteckten Items im Raum** (`searchRoom`, `Parser.hs:2071`: `itemLocation == InRoom rId` + `itemHidden` + `not itemDiscovered`). „Erst X findet Y" ist mit `hidden: true` also nur ehrlich, wenn kein `search` dazwischenfunkt. Workaround: `on: before search` + `block:` bis die Mechanik gelöst ist. |
| `Z-05` | ✅ | **`location:` verlangt einen echten Raum** (`InvalidItemLocation`). „Item startet außerhalb der Welt" ist nicht ausdrückbar — Erscheinen/Verschwinden muss über `give:`/`move_all`/`consume:` gelöst werden, nicht über einen Start-Ort. |
| `Z-06` | ⚙️ | **`verb_map` hat keinen Fallback über den Zustand.** Ein Eintrag `wind:` ist `wind,intact` und matcht also **nur** bei `itemStatus == "intact"`; für andere Werte findet `runVerbMapEntry` nichts und die Standardaktion läuft („You can't do that … right now."). ⚠️ **Seit `OPEN-02` wird `itemStatus` von `set_state` mitgeschrieben** — jede Statusmaschine braucht deshalb **pro Zustand einen eigenen Eintrag** (`wind,intact:` / `wind,broken:`), sonst verschwindet die Reaktion nach dem Statuswechsel still. Genau das hat 2026-10-09 zwei Regressionen im Zork-Port verursacht (`canary` `wind:`, `egg` `throw:`). |
| `Z-07` | ⚙️ | **Die Standard-Guards `take.already`, `take.not_portable`, `inventory.full` laufen vor *allen* `verb_map`-Phasen** (`before:`/`instead:`/historisch). Ein `before:take`-Veto greift also nie bei schon getragenen Items („You already have the …"). |
| `Z-08` | ⚙️ | **`portable: false` + `take_failure: <Text>`** ist die Standard-Antwort für untragbares. Wer es *später* aufhebbar machen will, braucht `before:take` + `block:` statt `portable: false`. |
| `Z-09` | ⚙️ | **Dunkelheit prüft vor Aktionen** — in einem `dark`-Raum sind `look`/`examine`/`take` ohne Lichtquelle blockiert (`attack` u. a. dagegen nicht). Falsche Dark-Tags wirken also selektiv seltsam. |
| `Z-10` | ⚙️ | **Items sind über `InContainer`-Indirektion verknüpft:** bewegt man den Container, folgt der Inhalt automatisch (`itemLocation == InContainer <id>`). Praktisch für „bricht auf"/„fällt heraus". |
| `Z-11` | ✅ | **`give:` an einen Removed-Ort ressurrektierte Items** (kein Tombstone). Behoben `FIX-02`. |
| `Z-12` | ⚙️ | **`portable: false` verschluckt `instead:take`-Einträge.** Der Take-Pfad prüft *vor* jeder `verb_map`-Phase `itemPortable` und antwortet `take_failure`/`take.not_portable` — ein `instead:take` (z. B. der Geister-Fluch der Knochen) feuert dann **nie**. **Muster:** wer mit `instead:take` eine *eigene* Take-Reaktion will, lässt das Item `portable` (der Eintrag ersetzt den Griff, es wird also nichts aufgehoben) und belegt alle drei Phasen-Wege im Kopf: `instead:` = die Aktion, `before:` = Veto möglich, `take_failure` = nur für echtes Untragbares. |
| `Z-13` | ⚙️ | **Semantik-Tags und Container-Startzustand (Stand 2026-10-09):** `scenery` = NDESCBIT (ansprechbar, aber nicht in der „You see:"-Liste, `take all` überspringt es); `transparent` = TRANSBIT (Inhalt auch geschlossen sichtbar/erreichbar, `open` gibt „Opened." statt Reveal); Item-Container starten **offen**, `state: open\|closed\|locked` setzt dagegen den Startzustand in die `entityStates`. `visibleItemsAt` (Zielauflösung, `take all`) und `playerHasTaggedItem` (Licht!) sehen durch see-through Container — „Lampe in der Tasche leuchtet, zugeknöpft wird es dunkel" (Original `LIT?`). |

---

## 3. Effekte & Zielmengen (`E-##`)

| ID | Status | Befund |
|---|---|---|
| `E-01` | ⚙️ | **`reveal_all`, `move_all`, `consume_all`, `set_state_all`** sind die einzigen Massen-Operationen und sprechen dieselbe Zielmengen-Sprache wie die B2-Abfragen: `what` × `in:`/`by:` × optional `tag:`. `move_all` bewegt **alles** an einem Ort — auch versteckte. Quellen und Ziele dürfen NPCs sein (`by: thief`), was „Beute zurückgeben" (`to: {by: player}`) und „stehlen" (`to: {by: thief}`) einzeilig ausdrückt. |
| `E-02` | ✅ | ~~**Kein Effekt verschiebt ein *einzelnes* Item in einen Raum.**~~ **Seit `OPEN-05` gibt es `place: {item: X, in: <raum>}` bzw. `{in_container: <cid>}`** — auch aus `location: nowhere`, mit Inventar-/Kapazitäts-/Zyklus-Checks. Damit sind „Glocke/Kerzen fallen zu Boden" (Original: `MOVE BELL/CANDLES HERE`) und „Diamant kommt aus der Maschine" originalgetreu. ⚠️ `place:` hat **kein** Ziel „aktueller Raum" — wer etwas vor die Füße fallen lassen will, braucht pro Raum eine Variante oder `give:`. |
| `E-03` | ✅ | ~~**`consume` erreicht Container-Inhalt nicht**~~ **Seit 2026-10-09 (Stufe E) behoben:** `consume:` erreicht Items in (offenen) Containern, deren Kette zum Spieler/Raum führt — das Original akzeptiert „den Container halten" (V-EAT). Zusätzlich kennen die B2-Mengen jetzt **`in_container:`** (`count.items.in_container.<id>`, `move_all: {what: items, in_container: X, tag: t, to: {in: nowhere}}`) — damit ist „Flasche leertrinken" (→ dormant, `fill` bringt es zurück) sauber ausdrückbar. Geschlossene Containerketten schneiden weiter ab. |
| `E-04` | ⚙️ | **Ketten richtig herum:** `give:` → `consume:` in dieser Reihenfolge nutzen (der Consume-Guard greift sonst bei NPC-Items); `consume:` → `give:` geht seit `FIX-02` bewusst **nicht** (kein Zurückholen aus `Removed`). |
| `E-05` | ⚙️ | **`condition:` (Timer) mit `end:`** ist der Ersatz für Original-Interrupts (`QUEUE`/`DISABLE`). `clear_condition` beendet vorzeitig; der `end:`-Zweig prüft besser noch einmal selbst, ob er noch zählt (sonst doppelte Meldung, siehe `R-01`). |
| `E-06` | ⚙️ | **`random:` in Effekten** ist der Ersatz für `PICK-ONE`; die Engine-RNG ist deterministisch (→ `W-01`). Funktionsreichweite: beliebige Effekte, inkl. `move_npc` — damit ist „NPC zieht in einen zufälligen Raum" (Dieb-Wanderung) als **eine** Regel mit `random:`-Ästen + `move_npc` ausdrückbar, ohne `patrol:` (das zusätzlich warnen/greifen würde). **`random: {stream: <name>, …}` (B8) isoliert Systeme:** Ziehungen auf einem Strom verschieben weder den Default-Strom noch andere Ströme. Praktisch bewiesen am Dieb (`stream: thief`): ein fremder Default-Strom-Zug (Damm-Auswurf im Loud Room) ändert sein Stehl-Verhalten bei gleicher Zugzahl **nicht**. Jedes System, das pro Zug würfelt (Dieb, Wetter, Loot), gehört deshalb auf **eigenen** Strom. |
| `E-07` | ⚙️ | **`set_flag: x, val: 'false'`** ist das Ausschalten (Flags sind faktisch boolesch, `has_flag` prüft auf `"true"`). Vergleiche gegen andere Strings brauchen Textvariablen. |

---

## 4. Parser & Verben (`P-##`)

| ID | Status | Befund |
|---|---|---|
| `P-01` | ✅ | **Ein** kanonischer Verbnamen für `cmd.verb`, `on: before` und `on: command` (vorher gesplittet: `board` vs `enter`). Aliase zählen dazu: `enter boat` → `board`, `get`/`grab` → `take`, `look at` → `examine`. Behoben `FIX-03`. |
| `P-02` | ✅ | **`verb_map` löst Zielstrings mit Präpositionen nicht auf.** `turn switch with screwdriver`, `rub mirror with knife`, `open egg with knife` kommen als **ein** Zielstring an („switch with screwdriver") → „You don't see … here.", das `verb_map` feuert nie. **Abhilfe:** `on: before <verb>` + `block:` — dort sind `cmd.arg1..3` sauber gesplittet (`turn`,`switch`,`with`,`screwdriver`). |
| `P-03` | ✅ | **Zwei verschiedene „bekannte Verben"-Listen:** `reservedVerbWords` (`Compile.hs:1248`, darf *nicht* als Custom-Verb deklariert werden) und `coreCommandVerbs` (`Verbs.hs`, darf in `on: command`/`on: before` stehen). Nicht identisch — `open`/`close`/`read` sind reserviert, stehen aber nicht in `coreCommandVerbs`. |
| `P-04` | ✅ | **Wo greift was?** `take`/`drop`/`use`/`talk`/`attack`/`examine`/`search`/`read`/`look` laufen über `verb_map` am Item (`coreVerbDefs`; `read` und `look` sind Aliase von `examine`). ⚠️ Früher hatte nur `open` einen halben `instead:`-Lookup — `close`/`lock`/`unlock` und alle Nicht-Instead-Phasen liefen in die Standard-Aktion (das Gitter-`unlock:`/`open:` des Zork-Ports war damit **still tot**, `FIX-08`). Seit `FIX-08` dispatchen **alle vier Container-Kommandos** über `runVerbMapEntry` (alle Phasen). ⚠️ Für *andere* Verben gilt weiter `Z-12`. |
| `P-05` | ⚙️ | **Custom-Verben mit einem Wort** (`wind canary`) kommen als `Interact` an und lösen das Ziel über `verb_map` auf. **Mit mehreren Wörtern** (`turn switch with screwdriver`) als `ActionWithArgs` — Zielauflösung dann am Gesamtstring (→ `P-02`). |
| `P-06` | ⚙️ | **`cmd.count`/`cmd.arg1..N`** sind die einzige zuverlässige Basis für „mit welchem Werkzeug". `open egg` → `cmd.count == 1`; `open egg with knife` → `arg1=egg, arg2=with, arg3=knife`. `with`/`to` sind **keine** Stopwörter (nur `the`, `a`, `an`, `some`, `that`, `this`). |
| `P-07` | ⚙️ | **Stopwörter werden nur im Zielteil gestrippt** (`safeStripStopWords`), nicht in `cmd.raw_args`. |
| `P-08` | 🔧 | **`verb_map` bei *NPCs* wird für `Interact` nicht ausgewertet.** `examine thief`, `take thief`, `talk thief` laufen am NPC-`verb_map` vorbei — die Effekte kommen nicht, es erscheint die Standard-Meldung („You can't do that to thief."). Dokumentiert ist `npcVerbMap` nur im `search`-Pfad. Abhilfe: `on: before <verb>` + `block:` mit dem Text (wie bei Items mit Präpositions-Zielen, `P-02`). Item-`verb_map` funktioniert dagegen normal. |
| `P-09` | ✅ | **`move <ding>` war nicht ausdrückbar:** der Parser las jedes Zwei-Wort-`move` als Bewegung (Nicht-Richtung → „I couldn't understand that sentence.“) und `move` stand auf `ReservedVerbName` — `move rug` (Zork) ließ sich weder parsen noch als Custom-Verb deklarieren. Jetzt claimt der Parser `move <x>` nur bei echter Richtung, `move` ist deklarierbar (`go`/`walk` bleiben reserviert). Siehe CHANGELOG, Plan C2. |

---

## 5. Werkzeuge, Validierung, Testen (`W-##`)

| ID | Status | Befund |
|---|---|---|
| `W-01` | ⚙️ | **Die Engine-RNG ist deterministisch *und* der Seed ist fest verdrahtet:** `rngState` steht als Konstante im initialen Save (zork1: `11400714819323198485`), nicht als Uhrzeit-Zufall. Gleiche Befehlsfolge = **byte-identische** Ausgabe (per `md5sum` verifiziert). ⚠️ Nicht zubverbrauchende Befehle (`look`, `inventory`, `stats`) ziehen **keinen** Slot — nur Zug-Befehle. Zufalls-Events (Troll-Würfel, Dieb-Steal) testet man also über die **Zugzahl/-folge**, nicht über „Lauf"-Wiederholungen. Sehr nützlich: `rngState` im Save überschreiben (`seed.py rng=42`), um eine ganze Zugfolge festzuzurren. ⚠️ **Ergänzt 2026-10-12 (OPEN-14):** der *Wander*-Zufall hängt nicht an `rngState`, sondern am neuen `save.runSeed` — der bei einem Spielstart **ohne** explizites `--save` von der Uhr gezogen wird (und bei Roguelike-Restarts neu ist). Byte-identische Läufe brauchen deshalb wie gehabt `--save` (Seed im Save) oder die neue `--seed N`-Flagge; beides ist in CI/E2E so gesetzt. |
| `W-02` | ✅ | **~35 `DeadExit`-Warnungen** von `worldbuilder validate` kommen vom Muster `when: {not: {'true': true}}` für absichtlich blockierte Ausgänge — funktional korrekt, aber Rauschen. Schön wäre eine eigene „blocked"-Syntax (`Wunsch: OPEN-06`). |
| `W-03` | ⚙️ | **Punkte zählen im Port bei Erstnahme**, nicht über die Vitrine (Original: `OTVAL-FROB`). Dokumentierte Abweichung, `plan-k17-score.md` §7.4. |
| `W-04` | 🔧 | **Engine-Nummerierung uneinheitlich:** Score-System heißt Plan/Code „K17", der Commit „K11e" — vereinheitlichen. |
| `W-05` | ⚙️ | **`scripts/ci.sh` bricht bei *jedem* Build-Warning ab** (nicht nur bei `-Werror`-Klassen). Warnungen im Build sind damit harte Fehler. |
| `W-06` | ⚙️ | **`worldbuilder validate` unterscheidet harte Fehler** (Exit 1, u. a. `UnknownCommandVerb`, `UnknownVerb`, `VerbPhaseClash`, `ReservedVerbName`, `DuplicateTriggerId`) **von Warnungen** (Exit 0, u. a. `DeadExit`, `MissingSetFlag`, `UnsatisfiableCondition`, `InvalidItemLocation`, `KeywordCollision`). |
| `W-07` | ⚙️ | **Save-JSON seeden ist der schnellste Testweg:** Felder `currentRoom`, `inventory` (`itemStates[*].itemLocation = {tag: CarriedBy, contents: player}`), `flags`, `entityStates` (wie `set_state`), `itemStates[*].itemStatus`, **Lebenspunkte** (`player.playerHealth`/`playerMaxHealth`, `npcStates[*].npcHealth` — nicht `hp`!), **RNG** (`rngState`, um Zufallsfolgen festzuzurren). NPCs per `npcStates[*].npcStatus`. Damit sind beliebige Spielstände in Sekunden herstellbar. |
| `W-08` | ⚙️ | **Backups der Port-Datei** liegen in `~/workspace/zork1.before-<stufe>.yaml` (die `/tmp`-Backups aus Schritt 2/Scoring sind flüchtig). |
| `W-10` | ⚙️ | **Win-Path-Harness** (`~/workspace/win.py` + `~/workspace/route.py`): die Zork-Lösung als Wegpunkte + Aktionen, `route.py` expandiert Bewegungen per **gewichtetem BFS** über `zork1.yaml` (bewachte Ausgänge kosten 10 — sonst laufen Routen querfeldein durch Guard-Ausgänge wie das Tempelloch). Ausgabe = Eingabestrom; `worldbuilder compile` + `printf 'quit'\n \| cat win.in - \| cli` reicht zum Nachlaufen, die Engine-RNG (`W-01`) macht Wiederholungen byte-identisch. |
| `W-09` | ⚙️ | **Der CLI schreibt `--save <datei>` bei `quit` nicht.** Die Datei ist die Quelle zum **Laden**; geschrieben wird nur über den `save`-Befehl (benannte Slots). Wer den Zustand nach einer Sitzung prüfen will, muss **im Spiel bleiben** (`look`, Wächter-Regeln, Exit-Guards) oder zuvor `save` benutzen. ⚠️ **Falle:** Ich habe daraus fälschlich „`set_flag`/`move_npc` wirken nicht" geschlossen (Save-JSON blieb leer) und war schon beim Engine-Quellcode — beides war korrekt. Verdachtsfälle immer erst *in der Sitzung* verifizieren. |

---

## 6. Behobene Engine-Bugs (`FIX-##`)

| ID | Commit | Was war kaputt |
|---|---|---|
| `FIX-01` | `25e8248` | **`commandVerbName` kannte 17 eingebaute Kommandos nicht** (`GiveCmd`, `PutInCmd`, `TakeFromCmd`, `OpenCmd`, `CloseCmd`, `LockCmd`, `UnlockCmd`, `AskCmd`, `TellCmd`, `RecipesCmd`, `EnterVehicleCmd`, `ExitVehicleCmd`, `DriveToCmd`, `WaitCmd`, `RefuelCmd`, `RepairCmd`, `ChooseCmd`) und warf sie auf `"unknown"` — `on: command give`-Regeln konnten **nie** feuern. |
| `FIX-02` | `b692c6b` | **`give:` ressurrektierte Removed-Items** (`relocateItem` ohne Tombstone) — `consume:` → `give:` holte konsumierte Items zurück. Jetzt: `relocateItem` lässt `Removed`-Items `Removed`. |
| `FIX-03` | `c043490` | **Verbnamen gesplittet:** `checkBeforeVeto` bezog den Namen aus `extractCommandArgs` (`EnterVehicleCmd` = `"enter"`), `on: command` aus `commandVerbName` (`"board"`) — `on: before board` erreichte den Veto-Pfad nie, `on: before enter` scheiterte an `UnknownCommandVerb`. Zusätzlich war `coreCommandVerbs` lückenhaft (kannte `board`, `give`, `put`, `open`, `close`, `lock`, `unlock`, `ask`, `tell`, `map`, `watch`, `save`, `restart`, `undo`, `compound` u. a. nicht). Jetzt delegiert `commandVerbName` auf `extractCommandArgs`; Regressionstest probed ein `Command` je Konstruktorzweig. |
| `FIX-04` | `c043490` | **`docs/adventure-schema.md:954` referenzierte falsche Trigger** (`on: npc_death`, `on: visit`, `on: put` existieren nicht). Korrigiert, plus Liste der Kern-Befehlsnamen unter „Command Veto". |
| `FIX-05` | — (Port) | **`coffin` stand auf `portable: false`** im Zork-Port: `take coffin` scheiterte immer, damit waren 25 Punkte und der volle 350-Weg unerreichbar (Höchststand 325). Original ist `TAKEBIT` (`1dungeon.zil:466`). |
| `FIX-06` | — (Port) | **`mirror-room-2` war als `dark` getaggt**, ist im Original aber `ONBIT` (hell) — blockierte `examine`/`take` am Spiegel. |
| `FIX-07` | `5ce2a17` | **Bewegungen zeigten nie eine Raumbeschreibung** (`R-08`): nur `look` renderte Räume, `go <dir>` ließ den Spieler im Blindflug (`sideEvents`/`EvRoomChanged` war textlos by design). Jetzt beschreibt die Game-Loop nach jedem Kommando mit Raumwechsel den Zielraum wie `look` (`roomViewEvents`/`autoDescribe`), inkl. `move:`-Effekten — bei Game-Over bleibt die Ansicht aus, `on_look`-Hooks laufen nur beim expliziten `look`. |
| `FIX-08` | `d08d5c4` | **Container-Kommandos dispatchten nicht über `verb_map`** (`P-04`): `open` prüfte nur `PhaseInstead`, `close`/`lock`/`unlock` gar nicht — das Zork-Gitter (`unlock:`/`open:` am Item) war **unspielbar** („It is already open." statt des Schlossmechanismus). Seitdem laufen alle vier über `runVerbMapEntry`; dazu Open-Reveal wie `V-OPEN` (`container.opened_reveals`), `examine` → Look-Inside (`container.empty` zurück im Katalog) und „It is now pitch black." nach `close` ins Dunkel (`container.dark_now`). |
| `FIX-09` | `eefa17b` | **`directionWord` kannte die Einzelbuchstaben `n`/`s`/`e`/`w`/`u`/`d` nicht** (nur ausgeschriebene Richtungen + `ne`/`nw`/`se`/`sw`) — klassische IF-Eingabe wurde zu „I couldn't understand that sentence.“. |
| `FIX-10` | `137bf4b` | **Container-Zielauflösung granular — `put` verschluckte Items** (Playtest-Befunde): (1) `put X in Y` legte Items in sichtbare Nicht-Container (Schwert als „Tasche“) — `findContainerRef` matched jedes Item, `relocateItem` legte hinein, die Anzeige hängt aber an `isContainer` (`capacity`), das Item war praktisch weg. (2) Fehlendes put-Item meldete `container.no_item` („You find no skull in trophy case.“ — klang wie Durchsuchen der Vitrine) statt `target.not_carried`. (3) Ein bekannter, aber nicht sichtbarer Ziel-Container meldete `container.not_a_container` („trophy case is not a container.“ aus dem Keller) statt `target.not_seen` — Scope ≠ Existenz. (4) `take all from <container>` lehnte sichtbare Container ab („is not a container“). Neu: `targetExistsAnywhere` + `containerRefError` (granular: bekannt/unsichtbar → „You can't see any such thing.“, unbekannt → historische Formulierung), `PutInCmd`/`TakeFromCmd` verlangen echte Container, `take all from <container>` als B9-Massen-Entnahme. |
| `FIX-11` | `e22dbf7` | **`message:`-Alias an `step_toward:`/`step_away_from:` war toter Code** — der Parser las `so .:? "msg" <\|> so .:? "message"`; weil `.:?` bei fehlendem Key erfolgreich `Nothing` liefert (statt zu feitern), wurde `message:` **nie** gelesen. Jetzt liest die Maybe-Kette den Alias wirklich; `msg:` bleibt die dokumentierte Form. |
| `FIX-12` | `b4a8ca9` | **Alias-Fallbacks überall tot (die FIX-11-Klasse, 7 weitere Stellen):** Exit-`message:` (`AExitRef`), `block:` mit `message:`/`text:`, Biome-`ascii:` (Alias von `ascii_art:`), TacticalCombat-Legacy `tcInitiative`/`tcFleeAllowed`/`tcMaxRounds`/`tcSpeedAttribute` und `ChoiceSnapshot`-`nextNode`. Alle Alias-Felder lesen jetzt per Maybe-Kette ihren Fallback (bei doppeltem Key gewinnt die Hauptform) — alte JSON-Dateien mit Alias-Namen fallen nicht mehr still in die Defaultwerte. Regressionstests in beiden Suites. ⚠️ Merke für neuen Code: **Alias-Keys nie als `.:? "a" <\|> .:? "b"`** — nur als Maybe-Kette (`>>= maybe … (pure . Just)`). |

---

## 7. Offene Engine-Punkte (`OPEN-##`)

| ID | Wunsch | Warum | Status |
|---|---|---|---|
| `OPEN-01` | **`cmd.succeeded`**-Variable (oder `on: command_success`) | `on: command` feuert auch bei Fehlversuchen (`R-02`); heute braucht jede Regel eigene Guards. | ✅ `5d7fecd` — boolesche `cmd.succeeded`-Variable, Konjunktion für Compound/`all` |
| `OPEN-02` | **`set_state` setzt bei Items auch `itemStatus`** | Schließt `Z-01`; die Doku verspricht „Status von Items/Toren", der `verb_map`-Lookup liest aber einen anderen Speicher. | ✅ `5d7fecd` — `entityStates` + `itemStatus` synchron, `{state:…, is:…}` mit Schichtenfolge |
| `OPEN-03` | **`hidden: discover_only` / `searchable: false`** | `search` deckt heute *alle* versteckten Items im Raum auf (`Z-04`); „erst Rätsel, dann Fund" braucht ein `hidden`, das `search` nicht aushebelt. | ✅ `bc01829` — `searchable: false` (Default `true`, JSON-kompatibel) |
| `OPEN-04` | **Start-Ort `Removed`** für Items (`location: nowhere`) | „Item existiert, ist aber noch nicht in der Welt" ist heute nicht ausdrückbar (`Z-05`); Erscheinen müsste sonst über `move_all` aus einem Platzhalter-Raum gehen. | ✅ `bc01829` — `location: nowhere` → eigener **`Dormant`**-Ort (bewusst *nicht* `Removed`: FIX-02 bleibt intakt) |
| `OPEN-05` | **`place:`-Effekt** für ein einzelnes Item (`place: {item: X, in: Y}` / `in_container:`) | `E-02`: Glocke/Kerzen „fallen zu Boden", Diamant „kommt in die Maschine" sind ohne Massen-Operation nicht sauber ausdrückbar. | ✅ `bc01829` — `place: {item, in/in_container}` inkl. Kapazitäts-/Zyklus-Checks |
| `OPEN-06` | **`blocked: true`-Syntax für Ausgänge** | Ersetzt `when: {not: {'true': true}}` + Meldung und verschluckt die ~35 `DeadExit`-Warnungen (`W-02`). | ✅ `bc01829` — `blocked: true` (+ `msg:`); alte Muster warnen weiter |
| `OPEN-07` | **Prädikat für Container-Inhalt** (`in: {container: X, item: Y}`) | `Z-03`: `actor_has` sieht `InContainer` nicht. | ✅ `bc01829` — `in: {container: X, item: Y}` (direkte Mitgliedschaft) |
| `OPEN-08` | **`verb_map`-Zielaufflösung mit Präpositionen** (oder: `verb_map` feuert anhand `cmd.arg1` statt am Zielstring) | Schließt `P-02`; `X with Y`/`X to Y` sind die Normalform bei Werkzeug-Verben. | ✅ `0f2f5b1` — längster Nomenphrase-Präfix vor `with`/`to`, alle Phasen, Items + NPCs |
| `OPEN-09` | **`read` als `on: command`-Trigger** (oder Klarstellung) | `R-06`: `read` ist reserviert *und* kein Core-Command — man kann es weder deklarieren noch abhören, nur über `verb_map` am Item. | ✅ `0f2f5b1` — `on: before/command read`; `verb_map`-Alias auf `examine` bleibt |
| `OPEN-10` | **Item gezielt aus einem Container entfernen** | `consume:` erreicht Container-Inhalt nicht (`E-03`, „That is not within your reach."); `move_all`/`consume_all` sehen mit `in: <raum>` nur `InRoom` — **kein** `InContainer`; `place: {item: X, in: nowhere}` ist als Ziel bewusst verboten. Damit ist „die Maschine verbraucht die Kohle *im Inneren*" nicht einzeilig ausdrückbar. | ✅ 2026-10-09 — **`in_container:` als Mengen-Selektor** (B2-Sprache, `move_all`/`consume_all`/`reveal_all`/`set_state_all` + Zählfamilie) **und** `consume`-Reichweite über offene getragene Container. `place: … in: nowhere` bleibt bewusst verboten (FIX-02); der Weg nach draußen ist `move_all … to: {in: nowhere}`. |
| `OPEN-11` | **Ziele „aktueller Raum" / „Raum des NPCs"** | `place:`, `move_npc:`, `move_all … to: {in: X}` verlangen ein **literales** Ziel. Damit ist nicht ausdrückbar: „Item fällt vor die Füße" (Glocke/Kerzen, Dieb-Beute), „Dieb lässt Kram in *seinem* Raum liegen" (`DROP-JUNK`), „NPC zieht zum Spieler". Workaround im Port: `give:` (Landet in den Händen) bzw. der Zielraum ist durch die Regel selbst bestimmt. | ✅ 2026-10-09 — Dynamische Ziele `{here}`, `{room_of: <npc>}` in `worldbuilder` & Engine umgesetzt. |
| `OPEN-12` | **Zustandsabhängige Item-Tags** | „Die Kerze ist nur, solange sie brennt, `lightsource`" — Tags waren statisch, die Dunkelheitsprüfung beleuchtete Räume auch mit ausgegangener Lampe. | ✅ 2026-10-09 (`tags_when`, Stufe E) — `tags_when: {<zustand>: [tags]}`, effektive Tags = `tags` ∪ `tags_when[status]`; alle Tag-Prüfungen (Dunkelheit, `feelable`, B2, `reveal_all`) und die Validierung (`itemTagsAnyState`) ziehen mit. JSON nur bei Befüllung (Byte-Vertrag). |
| `OPEN-13` | **Dunkelheit & Zielauflösung an `LIT?`/Parser angeglichen** | Drei Kleinteile aus Stufe E: (a) ein `dark`-Raum blieb dunkel, obwohl eine brennende Fackel *darin lag* (Original `LIT?` sieht Raum-Objekte); (b) `drink water`/`eat lunch` fanden Items nicht, die in einem *getragenen* Container liegen; (c) `consume` dort mit `consume.not_reachable` scheiterte. | ✅ 2026-10-09 — `isDark` sieht Raum-Lichtquellen, `resolveTarget` glättet getragene Container (`visibleItemsAt`), `isReachableForConsume` folgt der Containerkette. Regressionstests in den jeweiligen Suiten. |
| `OPEN-14` | **Patrouillen & Wandern (mit Pursuit-Kopplung)** | NPCs bewegen sich auf Patrouillenrouten oder wandern zufällig durch Nachbarräume; wechselt bei Sicht-/Hörkontakt dynamisch auf `StepToward` / `pursuit` (A1/A2). | ✅ 2026-10-12 — `wander:`-Sektion + `WanderStep`-Effekt: zielloser Schritt als Hash aus (Run-Seed, Zug, NPC-ID) über die stabil sortierten Kanten (stateless, kein `rngState`-Verbrauch), `stay_in`/`avoid`/`ignores`/`pace`; `notice:`/`on_notice: pursue|flee|hold`/`lose_after` koppeln an `StepToward`/`StepAwayFrom` (Variablen `wander.<npc>.mode`/`.lost`, reserviert). Run-Seed als neues `save.runSeed` (nur bei ≠ Default im JSON — Byte-Vertrag): explizites `--save` behält ihn (Tests reproduzierbar), `--seed N` setzt ihn, menschlicher Start ohne `--save` zieht einen Uhr-Seed. Patrol zusätzlich `mode: loop|pingpong|random` (`BadPatrolMode`). |
| `OPEN-15` | **`steal_item`-Aktion/Effekt** | Zielgerichteter Diebstahl von Items für NPCs und Spieler (z. B. `steal: {from: <actor>, item: <item>}`), statt pauschalem `move_all` (A3). | ✅ 2026-10-11 — `steal: {item, from, to?, msg?}` (`StealItem`): nimmt genau dieses Item aus den Beständen von `from:` (getragen, ausgerüstet **oder** im mitgeführten Container — A3 „Taschen-Gegenstände“, auch geschlossen) nach `to:` (Default `player`). Ohne Fund No-op mit `steal.no_item`/`npc.no_item`; `msg:` ersetzt die Erfolgs-Meldung. Rollen-Meldungen wie im ganzen Katalog: Spieler als Dieb → `npc.took_from`, als Opfer → `steal.from_you`, NPC ↔ NPC → `steal.taken`. `from: == to:` stiller No-op. |
| `OPEN-16` | **Parser Default-Ziel bei eineindeutigem Scope** | Bei transitiven Verben (`attack`, `open`, etc.) ohne angegebenes Objekt automatisch das einzige passende Ziel im Raum anvisieren (A5). | ✅ 2026-10-11 — `bareVerbCandidates` + `interactEntityById` + `containerCandidates`; bare `open`/`close`/`lock`/`unlock`/`use`/`look at` werden erkannt; Custom-Verben mit `on: command`-Trigger behalten altes Verhalten. |
| `OPEN-17` | **Generische Item-Timer (`fuel` / `burn_time`)** | Zähler/Verbrauch für Gegenstände (Fackeln, Batterien, Kerzen, abkühlende Glocken) analog zum Fuel-System von Fahrzeugen (B2/D6). | ✅ 2026-10-11 — `fuel: {start, ticks_when, warn, on_empty}` am Item (`FuelDef`): Zähler in `itemProps["fuel"]` (persistiert, lesbar als `item.<id>.fuel`), Tick pro verbrauchtem Zug (`tickItemFuels` neben `tickConditions`), `ticks_when` pausiert („extinguish“ wie `DISABLE`), Meilensteine feuern bei Tick-Dekrementen auf ihren Wert (Sprünge überspringen sie), `on_empty` beim Werden-0. Effekte `set_fuel: {item, to}` / `add_fuel: {item, delta}` (klammert bei 0; Item ohne `fuel:` → No-op + Diagnose). Removed/Dormant-Items frieren ein. |
| `OPEN-18` | **Zufallsauswahl in `messages:`-Katalog** | Erlaubt String-Listen in `messages:` (z. B. `take.not_portable: ["A valiant attempt.", ...]`) für abwechslungsreiche Ablehnungstexte wie Zorks `PICK-ONE` (C3). | ✅ 2026-10-11 — `messages:`-Werte dürfen Einzeltext **oder Pool-Liste** sein; die Auswahl rotiert deterministisch durch Salz (Zugzahl) + Vorkommen im Batch am Output-Edge (`localizeEventsWith`/`localizeEventsForSalted`): gleiche Zugfolge = gleiche Ausgabe (W-01), wiederholte Versuche zeigen die nächste Variante. Pool-Einträge sind volle Templates; Einzelwerte kodieren weiter als plain String in `world.json` (Byte-Vertrag), Pools als Array. |
| `OPEN-19` | **Multi-Raum-Objekte (`locations: [...]`)** | Ein Item/Scenery-Objekt ist in mehreren Räumen gleichzeitig sichtbar und manipulierbar (Gitter oben/unten, Fenster, Sci-Fi-Terminals) (D1). | ✅ 2026-10-12 — `locations: [raum, …]` am Item → `InRooms` (neues `Location`-Konstrukt, JSON nur bei Benutzung). Alle Orts-Abfragen laufen über `locationMatches` (Sichtbarkeit je Raum, Zählfamilie, Search, Lichtquellen, Entity-at). **Ein** Objekt mit **einem** geteilten Zustand; `take` nimmt es aus allen Räumen, `drop` legt es als Einzel-Raum-Item ab, `portable: false` hält Szenerie fest. Effekt-Ziele bleiben Single-Raum; Konflikte (`in_container:`/`carried_by:`/`nowhere`) = `LocationsConflict`. Einschränkung: `repeatable:`-Home = erster Raum. Port: `grating` + `grating-bottom` verschmolzen (eine `verb_map`, Seiten-Texte via `at: player, room:`). |
| `OPEN-20` | **`swap_inventory`-Effekt** | Atomarer Tausch zweier Inventare (`swap_inventory: {source, target}`) für Charakterwechsel, Drohnensteuerung und Parallelwelten (F1). | ✅ 2026-10-11 — `swap_inventory: {source, target}` (`SwapInventory`): geschlossener Tausch von getragen + ausgerüstet, getragene Ausrüstung bleibt getragen. Darstellung je Ziel (B9): Spieler = `equipment`-Slot-Map + `CarriedBy` (Map wird konsistent neu aufgebaut), NPC = `EquippedBy`. Still wie `give:` (Inszenierung als eigenes `msg:`), Container-Inhalte ziehen mit (Z-10), `source: == target:` No-op. |
| `OPEN-21` | **Mehrwort-Verben & Präpositionalphrasen** | Parser-Unterstützung für Verben wie `fill <x> with <y>`, `put out` (C1). | ✅ 2026-10-13 — `verbs:` mit Mehrwort-`name`/`aliases` und Full-Line-`patterns:` (`"put <x> on <y>"`, Slots Ziel/Werkzeug); längste Phrase schlägt Einzelwörter, Templates vor der Core-Matrix. Generischer `InteractWith`-Dispatch (X zuerst, dann Werkzeug-Y — `PUT X ON Y` hängt am Y), `cmd.tool`/`cmd.tool_kind`, Werkzeug-Keys `verb[,state][,tool:<id>]` in `verb_map` (Tool schlägt Plain, wie `Z-06`), Masse `put out all` mit Konjunktion, Compound-Teile feuern `on: command` pro Teil. Worldbuilder: `pattern:`/`patterns:`, Warnungen `ReservedVerbPhrase` (D3)/`DuplicateVerbPattern`/`UnknownVerbTool`, Fehler `BadVerbPattern`. Ticket: `plan-open21-mehrwort-verben.md`. |
| `OPEN-22` | **Rohes Spielerkommando in Bedingungen (`cmd.raw`)** | Zugriff auf den Original-Eingabestring für wörtliche Echo-Rätsel wie den Loud Room (C4). | ✅ 2026-10-13 — der Eingabe-Loop reicht die getippte Zeile (normalisiert: Kleinbuchstaben, Whitespace kollabiert) über `LoopState.lsRawInput` an `applyLoopCommandCore`, das sie **vor `checkBeforeVeto`** bindet: alle Phasen sehen denselben Wert, `bindCommandVars` löscht sie nicht. Meta-Befehle lassen den Wert unverändert, der Disambiguierungs-Replay (`runCommandNoTurn`) behält die ursprüngliche Zeile. Unerkannte Eingabe feuert die Trigger-Verben `unknown` (`on: before`/`on: command unknown`). Port: Loud Room mit `echo`/`bug`/Mitlesen (§8), globale `echo`/`bug`-Verben (`V-ECHO`/`V-BUG`). |
| `OPEN-23` | **Gefahrenlagen (`hazards:`) — allgemeines Verlauf/Footprint/Hook-System** | Statt reinem Wasserstandsmodul (E2): Wasser/Flut, Lava, Feuer, Sturm, Erdrutsch/Einsturz mit `track` (Pegel/Ziel/Rate, reversibel), `footprint` (`elevation`/`list`/`tag`, später `spread`) und freien Hooks. | ✅ 2026-10-13 (Stufe 1 umgesetzt) — Plan: `docs/plan-hazards.md` (**gültiges Dokument zu OPEN-23**; Entscheidungen E1–E7). Reines Worldbuilder-Sugar ohne Kern-Eingriff: AST & Desugaring (`AHazard`, `ATrack`, `AFootprint`, `hazard_level`, `in_hazard`), Compiler (`compileHazards`, `hazard.*`-Variablen & -Trigger `tick`/`stage`/`zone`/`telegraph`), Autor-Effekte (`hazard_target`, `hazard_jump`, `hazard_pause`, `hazard_resume`), Validierung (`UnknownHazard`, `HazardLevelOutOfRange`, `HazardUnknownRoom`, `HazardFootprintEmpty`, `HazardVariableClash`, `DuplicateHazard`, `HazardIdEmpty`), Byte-Vertrag gewahrt, Fixture `examples/modules/hazards.yaml` (Wasser + Sturm + Einsturz), Fuzz-, Golden- & CI-Tests grün. **`OPEN-23a`**: Zork-Dam-Umbau (wartet laut E5). |
| `OPEN-24` | **Fahrzeug-Zustandsänderung (`apply_vehicle_condition` / `clear_vehicle_condition`)** | Fahrzeug-`conditions:` hatten zuvor keinen Setter (`Set.insert` auf `vsActiveConditions` fehlte; clear-only via `repair`). | ✅ 2026-10-14 — `apply_vehicle_condition` und `clear_vehicle_condition` als Core-Effekte (`ApplyVehicleCondition`/`ClearVehicleCondition`) und Worldbuilder-Sugar mit dynamischem Token `"current"`. Setter mit `Set.insert` auf `vsActiveConditions`, Event-Emissionen `vehicle.condition_set`/`cleared`. Prädikat `vehicle_has_condition` (`VehicleHasCondition`) mit Integration in `evalPredicate` und `isChoiceVisible`. Refchecks `UnknownVehicleConditionTarget` (Error) und `UnknownVehicleCondition` (Warning). Testplan VC-01..VC-20 grün, Byte-Vertrag gewahrt. Detailplan: `docs/detailplan_vehicle_condition.md`. |

Commits: `0f2f5b1` (OPEN-08/09) · `bc01829` (OPEN-03..07) · `5d7fecd` (OPEN-01/02) —
jeweils mit Regressionstests; Details in `CHANGELOG.md`.

---

## 8. Anhang: Abweichungen des Zork-Ports vom Original

Nur als Kurzform; die Begründung steht je Stufe in `plan-zork-port.md` §4.

**Stufe A — Troll:** generische Geschenke (`troll-give-other`) feuern auch bei
fehlgeschlagenem `give` und werden nicht verbraucht (keine dynamische `consume`-Referenz
in Regeln).

**Stufe B — Flags & Boot:**
- `Vehicles.hs` bewusst **nicht** benutzt (Stopp-/Routenmodell, bricht Einweg-Fluss und
  `land`-Wege) → Zustandsmaschine über `in-boat`/`boat-inflated`/`boat-punctured`.
- `land` fasst Original-Richtung `LAND` **und** `V-DISEMBARK` zusammen.
- Glocke/Kerzen fallen seit `place:` (OPEN-05) originalgetreu zu Boden; `I-XBH`
  (Glocke kühlt ab) entfällt weiter, `ROB` (Beute-Spillover) ist seit Stufe F am
  Skelett-Fluch wieder da (Knochen → Halle der Toten).
- `CYCLOWRATH`/`I-CYCLOPS`: nur der Durst-Timer nach `give lunch` ist umgesetzt, nicht das
  Hochzählen ab Raumbetretung (`CYCLOMAD`).
- Kein `I-RIVER`-treibendes Boot (Fluss-Räume werden manuell per `down` durchfahren).
- Kerzen: `light candles` mit brennendem `match`; Brenndauern siehe Stufe E.
- `pray` im Südtempel → `forest-1` ist der Original-`V-PRAY`, **kein** „Vampir-Bann"
  (die alte Planzeile zu `coffin_cure` war falsch; Knoblauch dient der Fledermaus).
- `coffin_cure` ist kein Flag mehr, sondern direkt `not: {has_item: coffin}` am Ausgang.

**Stufe C — Item-Rätsel:**
- **Ei-Entscheidung (bewusst, vom Auftraggeber bestätigt):** originalgetreu als **Falle** —
  `open egg with <Waffe/Werkzeug>` und `throw egg` ruinieren Ei **und** Kanarienvogel
  dauerhaft. Der intakte Vogel (und damit `score-canary` 10 + `score-bauble` 2) kommt nur
  über den Dieb (Stufe D). Der alte Plan-Text „`open egg with knife` → `canary`" war falsch.
  **Folge: der 350-Punkte-Weg ist bis Stufe D bei 338 gedeckelt.**
- `broken-egg`/`broken-canary` sind keine eigenen Objekte, sondern **Zustände** von
  `egg`/`canary` (`state: intact|open|broken`) mit CondText-Beschreibungen.
- `bauble` landet direkt im Inventar (`move_all … to: {by: player}`) statt im Gras.
- Kohle: `put coal in machine` **oder** bloßes Mitführen genügt; die Kohle wird über
  `place: {item: coal, in: machine-room}` + `consume: coal` verbraucht (E-03-Workaround,
  weiter gültig — seit 2026-10-09 ginge auch `consume:` direkt aus dem Container).
- Loud Room: **seit `OPEN-22` (2026-10-13) hört der Raum mit** — bare `echo` stellt still,
  `bug` antwortet mit der Original-Zeile, unbekannte Wörter werden echo't (`{cmd.raw}`;
  Näherung: V-ECHO wiederholt im Original das *letzte* Wort, der Port die ganze Zeile).
  Bekannte Verben laufen normal (kein per-Verb-Veto — die eigentliche Eingabe-Schleifen-
  Übernahme bleibt außen vor), der Rauswurf läuft weiter über `on: turn` statt `M-END`,
  jetzt mit originalgetreuer Wasserkopplung (Schleife nur bei `GATES-OPEN == LOW-TIDE`).
- Ein-Wort-Verben (C1): **seit `OPEN-21` (2026-10-13) originalgetreu** — `put out`/`blow out`
  sind Aliase von `extinguish` (Original `PUT OUT`/`BLOW OUT` = `V-LAMP-OFF`),
  `put putty on leak` läuft über den Werkzeug-Key `put on,tool:putty` am `leak`
  (Fallback „There's no good surface on the leak." = Original V-PUT-ON),
  `fill <x> with <y>` antwortet mit V-FILLs „You may know how to do that, but I don't."
  (nur die blanke `fill <x>`-Form tut etwas — die bestehende `fill-bottle`-Regel mit der
  originalen 19-Raum-Wasserliste bleibt unverändert). Abweichung dokumentiert:
  gemischte Compounde (`take lamp and put out lamp`) sind eine Port-Neuerung.
- Spiegel: nur der Spieler wechselt den Raum, das Vertauschen des Inventars beider Räume
  (`MIRROR-MIRROR`) entfällt; `LUCKY` gibt es nicht, nur `mirror-mung`.
- Spiegel-Mechanik ist **`rub`**, nicht `push` (die alte Planzeile war hier ebenfalls falsch).
- Gas: geprüft werden `state: candles|match is: lit` und der mitgeführte `torch`
  (Original: `HELD?` + `ONBIT` für candles/torch/match — **die Lampe ist sicher**).
- Leck: `use putty on leak` statt `put putty`/`plug leak with putty`; `squeeze tube` entfällt.
- Maschine: **Dumbwaiter-Kette (Kohlegrube ↔ Schatzkammer) fehlt noch.**

**Stufe D — Dieb:**
- **Dieb-Wanderung (seit OPEN-14, 2026-10-12):** `thief-wanders` (die alte
  7/10-Dauerverfolgung über `step_toward`/`step_away`) ist durch einen
  `wander:`-Eintrag ersetzt. Recherchenbefund zur Wahrheit: das Original
  wandert **nicht zufällig** — `I-THIEF` (1actions.zil:3890) schiebt den Dieb
  jede Runde eine Position in der Raumliste weiter (`NEXT?` über den
  `ROOMS`-Baum) und überspringt dabei alles mit `SACREDBIT` sowie alles
  Nicht-Land (`RLANDBIT`). Die Ausschlüsse sind als `avoid:` originalgetreu
  übernommen (37 `SACREDBIT`-Räume + `reservoir`/`in-stream`); die geordnete
  Rundreise ist bewusst durch zielloseres Streifen ersetzt (Auftraggeber-
  Entscheidung „Hinterhalter"). Kontakt bei Distanz ≤ 2 → Verfolgung
  (`notice: 2, on_notice: pursue, lose_after: 2`), still (`msg: ''`), Flavour
  liegt in `thief-footsteps`/`thief-steals`. Bewusste Abweichung: die
  Ausschluss-Räume gelten nur fürs Streifen, die Verfolgung ignoriert sie.
  ⚠️ `win.py`-Check: die Früh-Kill-Strategie (30 × `attack thief` in der
  living-room) beruhte auf der alten Dauerverfolgung und ist gestrichen —
  der Dieb kommt nicht mehr von allein, Diebstahl wird entsprechend seltener.

**Stufe E — Licht & Alltag:**
- **Brenndauern exakt nach den Original-Tabellen:** Lampe 385 Züge ab Entzünden
  (LAMP-TABLE 200/100/70/15, Dimmer-Meldungen bei 185/85/15 Rest), Kerzen 75 ab
  Entzünden (CANDLE-TABLE 40/20/10/5, Meldungen bei 35/15/5 Rest), Streichholz 2.
  (Der Plan-Text „Lampe ~350, Kerzen ~20" war eine grobe Leseart der Tabellen —
  korrigiert.) Timer laufen **nur im brennenden Zustand**: `extinguish` pausiert wie
  das Original (`DISABLE <INT …>`), statt die Batterie zu löschen/neu zu laden.
- **Licht folgt dem Zustand** (`tags_when: {lit: [lightsource]}`), inkl. Raum-Licht
  (ein brennendes Licht am Boden erhellt seinen Raum, Original `LIT?`). Die drei alten
  Flags `candles_lit`/`match-lit`/`torch-lit` sind ersatzlos durch Item-Zustände
  ersetzt. Die **Fackel** brennt von Anfang an (`state: lit`, Original ONBIT) und ist
  nicht löschbar (`extinguish torch` → „You nearly burn your hand…“, Regel
  `torch-out`); `torch-light` entfällt (Plan B1, 2026-10-11).
- **Löschen** heißt `extinguish`/`douse` (Original EXTINGUISH/DOUSE) plus
  `turn off <x>` als Regel — `put out`/`blow out` fehlen (Parser kann nur
  ein-Wort-Verben). `burn` ist ein **Alias von `light`** (Original: BURN zündet
  Streichholz/Kerzen) — `burn <irgendwas>` zündet daher nichts an und bringt einen
  auch nicht um (Original V-BURN: „catches fire … JIGS-UP").
- **Streichholz-Matchbook:** `light match` verbraucht eines von 6 (`match_count`);
  in LOWER-SHAFT/TIMBER-ROOM pfeift es sofort aus (Original MATCH-FUNCTION).
- **Wasser:** `drink water` (auch `eat water`) leert die Flasche ins Dormant-Lager,
  `fill bottle` füllt sie nur an den 19 GLOBAL-WATER-Räumen des Originals nach.
  `drink`/`fill` nur für die Flasche — `fill X with Y` entfällt („You may know how
  to do that, but I don't.").
- **Essen:** `eat lunch`/`eat garlic` (Original V-EAT: „Thank you very much. It really
  hit the spot."), `drink <Essen>` → „How can you drink that?". `eat lunch` vertilgt
  den Lunch auch für den Zyklop (wie im Original).
- **Mailbox startet zu** (Original MAILBOX-F: „It is securely anchored." als
  `take_failure`); `open mailbox` öffnet, der Leaflet liegt darin.
- **Scenery:** chimney/slide/river/cliffs/board/forest/tree/rainbow sind je **ein**
  Item im charakteristischen Raum (Original: LOCAL-GLOBALS über viele Räume). Die
  Engine hat kein GLOBAL-OBJECT-Konzept — `examine river` klappt also nur dort, wo
  das Item liegt. **Muster für zwei-Zimmer-Objekte (Gitter):** zwei Items mit
  gleichem `name:`/`keys:`, gleicher `verb_map:` und gemeinsamen Flags — `grating`
  (oben) + `grating-bottom` (unten) teilen `grate-unlocked`/`grate-open`; die
  Effekte prüfen `at: player, room: …` für die seitenspezifischen Texte.
- **Scenery-Tag:** die 40 NDESCBIT-Objekte tragen `scenery` (aus den Listen weg),
  Examine-Texte sind die V-EXAMINE-Antworten des Originals.

**Stufe F — Texte & Feinschliff:** Raum-Kurznamen/Texte und Item-Examine-Texte
sind originalgetreu (LDESC/M-LOOK, V-EXAMINE-Hierarchie), `messages:`-Override für
die Grundverben. Verbleibende Abweichungen (bewusst/dokumentiert):
- `open <Container>` mit **einem** Inhalt ohne FDESC zeigt die Reveal-Form
  („Opening the X reveals a Y.") statt „The X opens." + FDESC-Zeile (der
  FDESC-Sonderfall des Originals braucht ein eigenes Item-Feld; nur der Sarg
  betraf es; bewusst akzeptiert, Plan D2).
- Die YUKS-Take-Verweigerungen sind **statisch** („A valiant attempt.") statt
  `PICK-ONE`-Zufall — Meldungs-Overrides sind Einzelwerte (→ `OPEN-18`).
- `raise`/`lower`/`kick`/`kiss`/`move` sind seit 2026-10-11 Verben im Port und
  lösen wie `take`/`attack`/`push`/`rub` den Skelett-Fluch aus; `move`/`raise rug`
  am Teppich (Plan C2). `move` war vorher `ReservedVerbName` und der Parser las
  `move <x>` immer als Richtung — beides korrigiert (`P-09`).
- V-LOOK-INSIDE zeigt auf `examine` die Inhalte unter der Beschreibung, nicht
  als Ersatz (die Beschreibung deckt die Original-„X is closed/open."-Zeile ab).

---

## 9. Wie man hier etwas ergänzt

1. Befund im passenden Abschnitt als Zeile mit **stabiler ID** eintragen (`R-11` usw.).
   IDs nie wiederverwenden, auch nicht nach einer Behebung — dann `FIX-##` anlegen.
2. Bei Workarounds das **Muster** zeigen (YAML-Schnipsel wie in `R-01`), nicht nur beschreiben.
3. Engine-Behebungen zusätzlich unter `FIX-##` mit **Commit** eintragen und die
   Status-Spalte des ursprünglichen Befunds auf ✅ stellen (Text stehen lassen).
4. Neuer Wunsch für die Engine → `OPEN-##` mit kurzer Begründung („Warum").
