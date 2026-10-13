# Plan — Zork-I-Mechaniken: Workarounds + Engine-Change-Entscheidungen

**Stand:** 2026-10-09 · **Zweck:** Liste aller Zork-Mechaniken, die im Port über Workarounds
abgebildet sind, mit Antwort-Feld „Engine-Change nötig?". Der Nutzer geht die Liste durch und
entscheidet pro Zeile: **Ja** (eigene Engine-Funktion bauen) / **Nein** (Workaround bleibt) /
**Später** (Workaround bleibt, Ticket offen) / **Gestrichen** (kein Engine-Change nötig, reiner
YAML-Task in `zork1.yaml` — siehe Abschnitt „Offene YAML-Tasks“) / ✅ (bereits erledigt).

**Verwandte Dokumente:**
- `befunde-text-adventure-engine.md` (gleiches Verzeichnis) — Befunde (E-##, P-##, OPEN-##), §8 Abweichungen
- `../examples/zork1.yaml` — der Port selbst (Zeilenangaben unten: Stand 2026-10-11, können driften — Regel-/Item-ID ist maßgeblich)
- `~/workspace/plan-zork-port.md` — Port-Gesamtplan (Stufen A–F) *(außerhalb dieses Repos; hier nicht vorhanden)*
- `~/workspace/win.py` — E2E-Durchspiel (Thief-Vorweg-Kill nötig, Torch-Fix) *(außerhalb dieses Repos; hier nicht vorhanden)*

**Status-Spalten:** `DEC` = Entscheidung (auszufüllen), `ADR` = ggf. Mini-Notiz (Warum, wo).

---

## A. NPCs & Bewegung

| # | Mechanik | Original (Zork I) | Port-Workaround | Grenze des Workarounds | DEC | ADR / Notiz |
|---|---|---|---|---|---|---|
| A1 | NPC-Wandering (Thief/Troll) | Recherchenbefund (1actions.zil:3890): **kein Zufall** — `I-THIEF` schiebt den Dieb jede Runde eine Position in der Raumliste weiter und überspringt `SACREDBIT`/Nicht-Land | alt: `on: turn` mit `random:`/`move_npc` (oder 7/10-`step_toward`-Dauerverfolgung) | (alt) literales `move_npc`-Ziel, ~35 Zweige; kein „wander in any direction“ | Ja → ✅ umgesetzt 2026-10-12 | `OPEN-14`: `wander:`-Sektion — zielloser Schritt als Run-Seed-Hash über die erlaubten Kanten (`stay_in`/`avoid`/`ignores`/`pace`); im Zork-Port als Dieb-`avoid:`-Liste mit den originalgetreuen Ausschlüssen (37 `SACREDBIT`-Räume + `reservoir`/`in-stream`) |
| A2 | NPC-Follow-Player (Thief betritt den Raum des Spielers) | Thief „wandered through the room“ genau dort, wo der Spieler ist | alt: Workaround-Konstrukt `random:` + `move_npc: thief` + Meldung | (alt) Pfadlogik „betritt den Raum, in dem gerade der Spieler ist“ nicht ausdrückbar | Ja → ✅ umgesetzt 2026-10-12 | `OPEN-14`: `notice: R` / `on_notice: pursue|flee|hold` / `lose_after` — Kontakt (`distance.<npc>.player ≤ R`) schaltet auf `StepToward`/`StepAwayFrom` um. Port: Hinterhalter `notice: 2` (Auftraggeber-Entscheidung), still + `thief-footsteps`; ⚠️ `win.py`-Früh-Kill gestrichen (beruhte auf der alten Dauerverfolgung) |
| A3 | NPC-Diebstahl + Rückgabe | Thief stiehlt Taschen-Gegenstände (bringt Werte + Gauntlet), bei Tod Rückgabe | `move_all` (Tag `schatz`) → Thief; bei Tod `move_all` zurück | „Abstrakte" Meldung nur als Text; Thief kann auf dem Weg nicht unterbrochen werden | Ja → ✅ umgesetzt 2026-10-11 | `OPEN-15`: `steal: {item, from, to?, msg?}` — gezielter Diebstahl inkl. Container-Inhalt des Opfers („Taschen-Gegenstände"), Meldungen rollenbasiert (`steal.*`, `npc.*`) plus `msg:`-Override |
| A4 | NPC-Kill → Zustand `dead` | NPC stirbt, Meldung + Gegenstand fällt | **Engine-Fix 2026-10-09:** `src/Combat.hs` setzt `SetValue (VRActorProp … PState) (EVString "dead")` bei Todesstoß — abgeschlossen | — (erledigt) | ✅ | Bereits erledigt |
| A5 | `attack` ohne Objekt | Original: `ATTACK` greift das NPC im Raum an („You can't see any such thing" nur ohne NPC) | Port: bare `attack`/`kill`/`hit` → **stille Nicht-Reaktion** (keine Meldung, Befehl zählt als gescheitert, der Zug läuft trotzdem weiter; geprüft 2026-10-11); nur `attack troll` funktioniert | Kein Kontext-Objekt-Fallback für Verben ohne Objekt | Ja → ✅ umgesetzt 2026-10-11 | `OPEN-16`: bare Verben lösen das einzige passende Ziel automatisch auf; bei mehreren Zielen Disambiguation-Frage, bei keinem Ziel verb-spezifische Meldung. |
| A6 | NPC-Kampfrunden | NPC greift selbst an, Spielzug-Kopplung („The troll fends you off…") | Kampf-Engine (Runden + Gegenzug) funktioniert; Barrier-Texte („blocks all passages out") via Trigger | Passt — nur dokumentiert | Nein | Bestehendes Verhalten passt; Items `troll` (zork1.yaml ~L3686), Regeln `troll-defeated`, `troll-give-*` |

---

## B. Licht & Brenndauer

| # | Mechanik | Original | Port-Workaround | Grenze | DEC | ADR / Notiz |
|---|---|---|---|---|---|---|
| B1 | Fackel (Torch) leuchtet | `ONBIT` an der Fackel: erhellt Raum + tragbar | Item `torch` **ohne** `tags_when: {lit: [lightsource]}` und ohne `state:` → `take torch` geht, aber die Fackel leuchtet **nicht**. Regeln `torch-light`/`torch-already-lit` (Zork L4964+) setzen nur einen Zustand | Brennende Fackel am Boden erhellt **nicht** den Raum; in finsteren Räumen also dunkel | Gestrichen | Reiner YAML-Fix, ✅ **umgesetzt 2026-10-11**: Item `torch` mit `state: lit` + `tags_when: {lit: [lightsource]}`; `torch-light` entfernt, `torch-out` neu. Engine konnte `roomHoldsLightsource` + `effectiveItemTags` bereits (`OPEN-12`/`OPEN-13`). |
| B2 | Batterie-/Brenn-Timer | LAMP-TABLE 385 (Dimm-Meldungen bei 185/85/15), CANDLE-TABLE 75, Match 2 — Timer nur im brennenden Zustand | Variablen + `on: turn`-Regeln (`lamp-dim-1/2/3`, `candles-dim-*`, …); `extinguish` pausiert | Funktioniert exakt, aber ~6 Regel-Paare pro Quelle; `win.py` hat kein Auto-Relight (Lampe stirbt bei Zug ~385) | Ja → ✅ umgesetzt 2026-10-11 | `OPEN-17`: `fuel: {start, ticks_when, warn, on_empty}` am Item + `set_fuel`/`add_fuel` — die Regel-Paare können raus (Details: Befunde `OPEN-17`) |
| B3 | Brennende Fackel zerstört Kerzen | `V-CANARY`-bzw. Torch-Hitze: Kerzen vergasen | Regel `torch-and-candles` (Zork L4188–4198) | Passt | Nein | Regel `candles-light` (~L4174, Torch-Zweig im if/else, `consume: candles`) |
| B4 | Matchbook mit 6 Hölzern | `MATCH-FUNCTION`: 6 Streichhölzer, verbrauchen sich; in LOWER-SHAFT/TIMBER-ROOM pfeift es sofort aus | Variable `match_count` + Regeln; Meldung via CondText | Passt | Nein | Variable `match_count` (~L60), Regeln `match-light`, `match-burn`, `match-burnout` |

---

## C. Parser & Verben

| # | Mechanik | Original | Port-Workaround | Grenze | DEC | ADR / Notiz |
|---|---|---|---|---|---|---|
| C1 | Ein-Wort-Verben | Parser hat Mehrwort-Verben (`PUT OUT`, `FILL X WITH Y`, `PUT PUTTY ON LEAK`) | `extinguish`/`douse`/`turn off` statt `put out`/`blow out`; `use putty on leak` statt `put putty`; `fill X with Y` fehlt („You may know how to do that, but I don't.") | Original-Grammatik wird nicht voll abgedeckt; Spieler kennt die Original-Syntax | Später → 📋 geplant 2026-10-13 | `OPEN-21`: Ticket `plan-open21-mehrwort-verben.md` (Ist-Zustand verifiziert: `put out candle` fällt als `drop` durch, `blow out` → `Unknown`; Arbeitspakete A–E, **D1–D4 entschieden**: Pattern-Syntax, `cmd.tool` + Werkzeug-Match, Phrase-Verben mit Warnung, Massenformen mit Konjunktion) |
| C2 | Verben `raise`/`lower`/`kick`/`kiss`/`move` | eigene V-Routinen | nur `take`, `attack`, `push`, `rub` lösen den Skelett-Fluch aus; die übrigen Verben fehlen komplett | 5 Original-Verben nicht portiert | Gestrichen → ✅ umgesetzt 2026-10-11 | YAML-Task, aber `move` brauchte einen Mini-Engine-Fix (Parser + Reserved-Liste, siehe CHANGELOG). Verben `move`/`raise`/`lower`/`kick`/`kiss` deklariert, an `bones` gehängt; `move`/`raise rug` am Item `rug`. |
| C3 | Zufalls-Antworten (YUKS, PICK-ONE) | `PICK-ONE` wählt bei jedem Take-Fail zufällig aus ~7 Spott-Meldungen | Meldungs-Override ist Einzelwert — statisches „A valiant attempt." | Keine Mehrfach-Meldungen pro Verb | Ja → ✅ umgesetzt 2026-10-11 | `OPEN-18`: `messages:`-Werte dürfen Pool-Listen sein, Auswahl rotiert deterministisch pro Zug (Details: Befunde `OPEN-18`) |
| C4 | Loud Room hört Kommandos mit | `P-CONT`-Flag: das Kommando selbst zählt als Echo | `on: turn` statt M-END; seit 2026-10-13 `cmd.raw`: bare `echo` stellt still, `bug` antwortet, unbekannte Wörter werden echo't (`on: before unknown` + `block: '{cmd.raw} {cmd.raw} ...'`) | Original übernimmt die **Eingabeschleife** — bekannte Verben laufen im Port normal weiter (kein per-Verb-Veto); V-ECHO wiederholt das *letzte* Wort, der Port die ganze Zeile | Ja → ✅ umgesetzt 2026-10-13 | `OPEN-22`: `cmd.raw` (Eingabe-Loop bindet die normalisierte Zeile, alle Phasen sehen sie); Wasserkopplung (XNOR `gates-open`/`low_tide`) ist originalgetreu |

---

## D. Items & Container

| # | Mechanik | Original | Port-Workaround | Grenze | DEC | ADR / Notiz |
|---|---|---|---|---|---|---|
| D1 | Zwei-Raum-Scenery (Gitter, Einbuchtungen) | `LOCAL-GLOBALS`: ein Objekt in vielen Räumen sichtbar | Muster: zwei Items (`grating` + `grating-bottom`) mit gleichem Namen/Keys/Flags; Effekte prüfen `at: player, room: …` | Bei jedem weiteren Zwei-Raum-Objekt (Einbuchtung im Reservoir-Duel) zu duplizieren; kein „Item in mehreren Räumen" | Ja → ✅ umgesetzt 2026-10-12 | `OPEN-19`: `locations: [raum, …]` — ein Objekt, ein geteilter Zustand, `take`/`drop` wohlfdefiniert. Das Gitter-Duplikat ist verschmolzen (`grating` mit `locations: [grating-clearing, grating-room]`, eine `verb_map`); die Reservoir-Einbuchtungen können künftig direkt `locations:` nutzen (Details: Befunde `OPEN-19`) |
| D2 | FDESC-Sonderfall beim Container-Open | „The X opens." + FDESC-Zeile | Engine zeigt bei einem Inhalt ohne FDESC die Reveal-Form („Opening the X reveals a Y.") | Nur der Sarg betroffen; dokumentiert, akzeptiert | Nein | Engine-Verhalten `container.opened_reveals` (FIX-08); Item `coffin` |
| D3 | Ei-Falle | `open egg with Waffe` / `throw egg` ruinieren Ei + Vogel | Zustandsmodell `intact|open|broken` auf `egg`/`canary` (bewusste Entscheidung) | Passt | Nein | Items `egg` (~L3331), `canary` (~L3378), Regel `egg-open` (~L4707) |
| D4 | Dumbwaiter-Kette (Kohlegrube ↔ Schatzkammer) | Kohle wird von der Maschine weiter transportiert | **Fehlt noch** — `put coal in machine`/Mitführen genügt, aber die Kette fehlt | Original-Kette fehlt | Gestrichen | Reiner YAML-Task (über Trigger/Move-Regeln ohne Engine-Änderung lösbar) |
| D5 | Bauble ins Inventar | Vogel lässt Bauble ins Gras fallen | `give: bauble` im `verb_map` des `canary` (Direkt-Inventar) | Passt | Nein | Item `canary` (~L3423–3426), Regel `score-bauble` |
| D6 | Glocke kühlt ab (I-XBH) | Bell wird nach kurzer Zeit unbrauchbar | entfallen | Kein Kühl-Timer | Ja → ✅ über OPEN-17 (2026-10-11) | `fuel:` + `ticks_when` (oder ohne: tickt immer), `on_empty` als Abkühl-Reaktion |
| D7 | Beute-Spillover (ROB) | Skeleton-Fluch: Knochen → Halle der Toten | seit Stufe F umgesetzt | Passt | Nein | Item `bones` (~L2767, Skelett-Fluch in `verb_map`) |

---

## E. Fahrzeuge & Fluss

| # | Mechanik | Original | Port-Workaround | Grenze | DEC | ADR / Notiz |
|---|---|---|---|---|---|---|
| E1 | Boot-State-Maschine | Deflated/Inflated/Punctured, `LAUNCH`, `LAND`, Drift | Flags `in-boat`/`boat-inflated`/`boat-punctured`; `land` fasst `LAND` + `V-DISEMBARK`; Vehicles.hs bewusst nicht benutzt | Kein Drift (I-RIVER); Fluss-Räume werden manuell per `down` durchfahren | Gestrichen | Kein Echtzeit-Drift; schrittweiser Drift pro Zug/User-Input ist per YAML (`on: turn` + `move: <room>`) voll abbildbar |
| E2 | Tide / Reservoir-Wasserstand | Tide-Maschine (FLOOR-MODE): Wasserstand steigt/fällt, `low_tide`-Zyklus | Flag `low_tide` + Raum-Varianten/Ausgangs-`when`; Schalter am Dam toggelt | Kein echter Wasserstand; Kein „Objekt schwimmt mit" | Später | Ticket für dynamischen Wasserstand |
| E3 | Pumpe/Dam-Wasser | `release`/`press yellow` → Wasserstand | über Flag-Toggle | Passt | Nein | Item `bolt` (~L2332, `verb_map: turn`), `yellow-button` (~L2480), Flags `gate-flag`/`gates-open`/`low_tide` |

---

## F. Welt-Logik & Sonstiges

| # | Mechanik | Original | Port-Workaround | Grenze | DEC | ADR / Notiz |
|---|---|---|---|---|---|---|
| F1 | Spiegel-Inventar-Vertauschung (MIRROR-MIRROR) | Spieler wechselt Raum, Inventar beider Seiten tauscht | nur der Spieler wechselt; `LUCKY` fehlt, nur `mirror-mung` | Original-Spiegel-Vertauschung entfällt | Ja → ✅ umgesetzt 2026-10-11 | `OPEN-20`: `swap_inventory: {source, target}` — atomar, getragen + ausgerüstet, getragen bleibt getragen (Details: Befunde `OPEN-20`) |
| F2 | Zyklop-Aufregung ab Raumbetretung (CYCLOMAD) | Zyklop zählt Treffer ab Betretung | nur der Durst-Timer nach `give lunch` umgesetzt | „Hochzählen ab Betretung" fehlt | Gestrichen | Reiner YAML-Task (per `on: turn` + Counter-Variable lösbar) |
| F3 | `pray` im Südtempel | `V-PRAY`: „A mysterious voice…" | Portiert (→ forest-1), **kein** Vampir-Bann | Passt | Nein | Regel `south-temple-pray` (~L4315) |
| F4 | Mailbox startet zu | `MAILBOX-F`: „securely anchored" | `take_failure` + `open mailbox` öffnet | Passt | Nein | Item `mailbox` (~L2905) |
| F5 | Verb-Sonderfälle (drop-verb: `egg`, `nest`, `rope`) | eigene V-Routinen für drop | Stufe F umgesetzt (zork1.yaml) | Passt | ✅ | Bereits erledigt |
| F6 | Score-Regeln (25 Regeln) | Museum of Natural History: Scoring-Tabelle | 25 Score-Regeln + Stufe A–F | Passt | ✅ | Bereits erledigt |

---

## Wie wir weitermachen

1. Für jede Zeile mit `DEC: ☐` entscheiden: **Ja** / **Nein** / **Später** — ✅ **Erledigt (2026-10-11)**.
2. Bei **Ja**: Mini-Ticket im `befunde-text-adventure-engine.md` (§7 OPEN) eintragen und Engine-Patch planen — ✅ **Erledigt (`OPEN-14` bis `OPEN-23` eingetragen)**.
3. Bei **Nein**: Verweis auf den konkreten Workaround (Datei + Regel-ID) im Plan behalten. — ✅ **Erledigt (2026-10-11)**, siehe Spalte „ADR / Notiz“.
   Bei **Gestrichen**: Eintrag in „Offene YAML-Tasks“ (unten) — ✅ **Erledigt (2026-10-11)**.
4. Nach dem Durchgang: `win.py` auf den finalen Stand anpassen — Thief-Vorweg-Kill ✅ **erledigt (2026-10-12)**: Block gestrichen, der Hinterhalter-Dieb kommt nicht mehr von allein. Offen: Torch-Relight, `low_tide`-Hack entfernen (falls B1/E2 per Engine gelöst werden).

---

## Offene YAML-Tasks (Entscheidung „Gestrichen“ — kein Engine-Change)

Stand 2026-10-11, gegen `examples/zork1.yaml` geprüft: **B1 und C2 sind umgesetzt, die übrigen drei (D4, E1, F2) sind offen.**

| # | Task | Ansatzpunkt in `zork1.yaml` | Status |
|---|---|---|---|
| B1 | Fackel leuchtet: `state: lit` (brennt von Anfang an, Original ONBIT) + `tags_when: {lit: [lightsource]}` am Item `torch`; `torch-light` entfällt, `torch-already-lit` bleibt, neu: `torch-out` + Zweig in `turn-off-alias` („You nearly burn your hand…“) | Item `torch`, Regeln `torch-already-lit`, `torch-out`, `turn-off-alias` | ✅ umgesetzt + getestet (2026-10-11): `worldbuilder validate` sauber; Spieltest: `light`/`extinguish`/`turn off torch` liefern die Original-Meldungen, ein dunkler Raum (Keller) ist mit Fackel (getragen oder am Boden) hell, ohne dunkel |
| C2 | Verben `raise`/`lower`/`kick`/`kiss`/`move` per `verbs:`/`verb_map` (`VCustom`) anlegen und an den Skelett-Fluch hängen; dazu `move`/`raise rug` | `verbs:`, `bones`, `rug` | ✅ umgesetzt + getestet (2026-10-11). **Nicht rein YAML:** `move` war `ReservedVerbName` und der Parser las `move <x>` immer als Richtung → Mini-Fix in `Parser.hs` + `Compile.hs`, Tests ergänzt (CHANGELOG). |
| D4 | Dumbwaiter-Kette Kohlegrube ↔ Schatzkammer über Trigger/Move-Regeln | `coal`/`machine`-Regeln (E-03-Workaround, Befunde §8 Stufe C) | ☐ offen |
| E1 | Fluss-Drift: `on: turn` + `move: <room>` solange `in-boat` gesetzt ist (ein Raum pro Zug Richtung Wasserfall) | Flags `in-boat`/`boat-inflated`, Fluss-Räume (~L1679–1814) | ☐ offen |
| F2 | Zyklop-Aufregung ab Raumbetretung: Counter-Variable + `on: turn` (`CYCLOMAD`) | bisher nur `cyclops_thirst` (~L4048–4106) | ☐ offen |

---

## Anhang: Kurvengerüst — NPC-Wandering vs. Pursuit (aus der Pursuit-Entscheidung 2026-09-30)

**Befund:** die Engine hat seit `2a9e61c` bereits eine Pfadsuche (`StepToward`/`StepAwayFrom`,
`pursuit:`-Sektion im YAML → `on: turn`-Trigger pro NPC). Das Fixture ist
`examples/fixtures/pursuit.yaml` (Jaeger + Sperrer, je 1 Schritt pro Zug Richtung Spieler).
Damit wäre der Thief **kein** `random:`-Konstrukt, sondern ein Pursuit-Eintrag — aber:

| Aspekt | NPC-Wandering (heute) | Pursuit (`pursuit:` + `StepToward`) |
|---|---|---|
| Pfadfindung | **keine** — literales `move_npc`-Ziel | BFS über `effectiveConnections` (1 Hop = 1 Zug, Distanz-Tie-Break) |
| Authoring-Aufwand | ~35 `random:`-Zweige pro NPC | 3 Zeilen YAML (`pursuit: [{npc: thief, target: player}]`) |
| Ausgangs-Wahl | nur ins Ziel | configurable: `ignores: [locked, guarded]` |
| Verhalten | kommt per Zufall (oder gar nicht) | kommt **immer** (bis Distanz 0), Fairness-Distanz 1 Zug |
| Meldung | eigener Text | `pursuit.step` / `pursuit.flee` (oder eigener `msg`) |
| „Er folgt mir NICHT" | nicht ausdrückbar | `StepAwayFrom` (Flucht) |

**Fazit:** Für den Thief ist Pursuit semantisch **richtiger** (er jagt aktiv den Spieler),
für den Troll aber nicht (der bleibt in seinem Raum und blockiert nur). Ein Mischmodus
(„wander + nur zu Spieler-Reise-Pfaden") wäre ein eigener Engine-Punkt, der Pursuit mit
Wandern kombiniert. — **Umgesetzt 2026-10-12 (OPEN-14):** der Mischmodus existiert als
`wander:`-Sektion mit `notice:`-Kopplung (streifen bis Kontakt, dann Verfolgung). Die
„Spieler-Reise-Pfade"-Einschränkung ist dabei nicht Teil des Features — die Verfolgung
folgt dem Spieler überallhin.
