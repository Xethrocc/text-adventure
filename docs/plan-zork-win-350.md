# Plan: win.py-Vollendung — Zork-Port auf 350 Punkte / Barrow-Sieg

**Stand:** 2026-10-14 · **Status:** 🅿️ Geparkt — nicht in Arbeit · **Vorlauf:** `60a04c8`
**Zweck:** `~/workspace/win.py` spielt den Port bis zum Sieg durch
(`score >= 350` setzt `won`, Betreten von `stone-barrow` feuert `game_end: victory`).
**Warum geparkt:** eigener Nachmittag Aufwand; die Chalice hängt an der
Balancing-Entscheidung `OPEN-25`.

---

## 1. Ist-Stand (verifiziert 2026-10-14)

Was schon läuft (voller Durchlauf ohne Diebstahl, 0 Drowns):

- **Mechaniken:** Damm (`hazards: reservoir:`) ✅, Machine (`turn switch with screwdriver`) ✅,
  Exorzismus am Hades-Eingang ✅, Sack-Strategie (Diebstahl-fensterfrei dank
  `put`-Scope-Fix `60a04c8`) ✅, Barrow-Abschluss in der Route ✅.
- **Eingesammelt (9/19):** painting 10, coins 15, torch 20, diamond 20, skull 20,
  sceptre 10, coffin 25, trident 15 + Leaflet/Map (ohne Punkte).
- **Raumfunde (4/5):** kitchen 10, cellar 25, ew-passage 5, lower-shaft 13 —
  es fehlt **treasure-room 25**.
- **Punkte bisher:** ~221 von 350.

## 2. Was noch fehlt

### 2.1 Schätze (Punkte aus `examples/zork1.yaml`-Score-Regeln)

| Schatz | Pkt. | Fundort (gemessen) | Besonderheit |
|---|---|---|---|
| `trunk` | 20 | `reservoir` | liegt am Grund — erst mit `low_tide` erreichbar (Reihenfolge beachten: Bolzen öffnen, Pegel 0, dann `take trunk`) |
| `jade` | 10 | `bat-room` | Vampirfledermaus! (Garlic mitführen? Port-Regel prüfen) |
| `bar` (Goldbarren) | 15 | `loud-room` | Loud-Room-Regeln (XNOR-Gate, siehe OPEN-22) beachten |
| `bracelet` | 10 | `gas-room` | **Gas-Falle:** brennende Quellen entzünden das Gas (Port-Regel `gas-room` prüfen — Lampe löschen vor Eintritt!) |
| `pot-of-gold` | 20 | `end-of-rainbow` | Regenbogen-Zyklus (Port-Regel prüfen: wann ist das Regenbogen-Ende betretbar?) |
| `scarab` | 10 | `nowhere` (Start) | wird per `search` enthüllt (sandy-cave? Fundstelle im Port prüfen) |
| `bauble` | 2 | `nowhere` (Start) | per `search`/Trigger auffindbar (Port-Regel prüfen) |
| `egg` | 10 | (in `nest`, `up-a-tree`) | `open egg` **vorsichtig** — unsanftes Öffnen beschädigt den Kanarienvogel (Port-`verb_map` beachten) |
| `canary` | 10 | (in `egg`) | nur bei `state: intact` (Score-Regel `score-canary` ist bedingt) |
| `chalice` | 15 | `treasure-room` | **an `OPEN-25` hängend** (siehe unten) |

Dazu: **treasure-room** (25 Pkt., `on: enter`) — Betreten löscht den Dieb dorthin
(`thief-fighting`); Chalice-Guard `chalice-guard` sperrt das Nehmen, solange der
Dieb im Raum steht.

### 2.2 Harte Voraussetzung: `OPEN-25` (Duellbalance Dieb)

Ohne die Chalice sind maximal **335/350** erreichbar → kein Sieg. Der Kampf ist
im aktuellen Balancing mathematisch verloren (Spieler 30 HP / Treffer 6 vs. Dieb
35 HP / Treffer 6, deterministisch). **Vor der Route-Erweiterung entscheiden:**

- **A. Schadensvarianz im Kampf** (Zufallskomponente ±N, Kandidat für den Kern) —
  Duell wird zu einer Wette, Seed-Suche findet gewinnbare Läufe.
- **B. Spieler-Puffer:** Schwert als `slot: weapon, effects: [attack+4]`
  (wie der Stiletto) — Duell 6 Runden à 10 vs. bleibt knapp, aber machbar
  (Port-Datenänderung, klein).
- **C. Chalice-Snatch ohne Kampf:** Guard nur bei `at: thief` im Raum — wenn der
  Dieb wegwandert, kurz rein und mitnehmen (Timing-Zufall, Seed-Suche).

**✅ Entschieden (2026-10-14, Patrick): Variante B** — das Schwert bekommt
`slot: weapon, effects: [attack+4]` (analog Stiletto). Umsetzung in AP A;
Kampf-Nachweis (Sieg über den Dieb) ist Teil der Abnahme.

## 3. Arbeitspakete (wenn freigegeben)

| AP | Inhalt | Aufwand |
|---|---|---|
| A | `OPEN-25`-Entscheidung A/B/C umsetzen (bei B: `zork1.yaml`-Schwert-Slot + Kampf-Nachweis) | 0,5–2 d je nach Variante |
| B | Route-Erweiterung `win.py`: 10 Schätze + treasure-room in die STEPS (Reihenfolge beachten: trunk erst nach dem Damm-Ablaufen; Lampe vor `gas-room` löschen; Loud-Room-Passage beachten) | 1 d |
| C | Score-`take`s fenstertauglich: jeder `take X` + `put X in brown sack` = 1 Diebstahl-Fenster (~19 gesamt) → **Seed-Suche** (Muster: `/tmp/seedsearch2.sh`, `--seed N`, Ziel „mastered the first part") | 0,5 d |
| D | Gewinner-Seed in `win.py`-Kopf dokumentieren + `plan-zork-mechanik-entscheidungen.md` Punkt 4 abhaken | 0,25 d |

**Abnahme:** `python3 win.py | text-adventure-cli --world zork1 --seed <N>` endet
mit „You have mastered the first part of the ZORK trilogy." — verifizierbar per
grep auf die Siegzeile.

## 4. Offene Detailfragen (beim Umsetzen prüfen)

- Fundorte von `emerald`, `egg`/`nest`, `scarab`, `bauble` (Standortangaben im
  YAML teils `in_container`/`nowhere`) — einmal `grep` gegen den finalen Stand.
- Regenbogen-Timing (`end-of-rainbow`) und Gas-Falle (`gas-room`) sind
  Port-Regeln mit eigenen Bedingungen — vor der Routenplanung lesen.
- Die Score-`take`s der 19 Schätze summieren 272 + Räume 78 = 350 (§7.5-Budget);
  bei Änderungen Summe neu prüfen.

*Anschluss: `docs/plan-zork-mechanik-entscheidungen.md` (Punkt 4 der „Wie wir
weitermachen"-Liste: win.py an den finalen Stand anpassen) und `OPEN-25` in
`docs/befunde-text-adventure-engine.md`.*
