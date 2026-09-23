# TheFog — kompletter Playthrough (verifiziert)

Dieser Durchlauf wurde **tatsächlich durchgespielt** (Engine v0.8, `worldbuilder compile` → `text-adventure --world … --save …`) und endet mit dem **Fog-/Game-Over-Screen** — dem einzigen Ende des Spiels (Original-Getreue, siehe unten).

## Kurzfassung (Befehlsfolge)

```
south
look               <- Basement: du siehst die Truhe (der Kristall liegt drin)
search chest       <- gibt dir den Kristall
take shield
equip shield       <- Verteidigung 3 → 7: der Wolf kontert nur noch für 1
north
north
north
north
attack wolf        (der Wolf fällt im 4. Schlag; jede Runde kontert er für 1)
north north north east north north west south southeast
                               <- Waldroute: Patrouillenwölfe warnen und beißen
use earth shrine               <- Schattenwolf (Wächter) steht hier und schlägt für 2 zu
south north east south south west south south south south east east south southeast
use water shrine
north north west west west west west west west west southeast
use fire shrine
south east east east east south south southeast southeast south
use air shrine      <- vierter Schrein: das Spiel endet hier
-> THE FOG DESCENDS / GAME OVER
```

Gemessener Durchlauf dieses Skripts: **7×** „WARNING: A wolf is nearby!", **5** Patrouillenbisse (1 Schaden), **2** Wächterschläge (2 Schaden), **3** Wolfskonter (1 Schaden) — der Spieler verliert 12 seiner 30 KP und **erreicht das Fog-Ende lebend**.

## Ablauf mit Etappen

| # | Ziel | Weg ab vorherigem Punkt | Aktion |
|---|------|--------------------------|--------|
| 1 | Basement (`loc_4`) | `south` | `search chest` (→ Kristall), `take shield`, `equip shield`, zurück `north` |
| 2 | Wolf (`loc_3`) | `north north north` | `attack wolf` → „You hit for 8, it hits you for 1." ×3, im 4. Schlag fällt der Wolf; Nordausgang entriegelt |
| 3 | Waldroute (`loc_12`…`loc_21`) | `north north north east north north west south southeast` | drei Patrouillenwölfe auf ihren Rundkursen; je Kontakt Warnung + Biss |
| 4 | Earth Shrine (`loc_28`) | (siehe 3) | `use earth shrine` — der **Schattenwolf** steht hier unbeweglich und schlägt für 2 zu |
| 5 | Water Shrine (`loc_38`) | `south north east south south west south south south south east east south southeast` | `use water shrine` |
| 6 | Fire Shrine (`loc_45`) | `north north west west west west west west west west southeast` | `use fire shrine` |
| 7 | Air Shrine (`loc_52`) | `south east east east east south south southeast southeast south` | `use air shrine` → **FOG / GAME OVER** |

## Das Ende

- **Aktivieren des vierten Schreins beendet das Spiel.** Rule `fog_descends` (`on: turn`, alle vier `*_shrine_active`-Flags):
  „The fog descends over the land... / Wolves grow stronger, guards fall, the world becomes hostile..." + `game_end: fog` → eigener End-Banner (`end_art.fog` → „GAME OVER").
- **Kein Victory.** Der frühere Thronsaal-Sieg (`enter loc_37` bei vier Schrein-Flags) war eine Erfindung des Ports und wurde entfernt. Der **Thronsaal (`loc_37`) ist reine Kulisse**, wie im Haskell-Original.
- Der Schrein-Flag-Mechanismus entspricht dem Original: `checkEnding = e && w && f && a` → dort ebenfalls sofortiges Ende („Game Over.").
- **Niederlage ist möglich:** `end_art.death` → „YOU HAVE DIED / The forest keeps what it takes." Der Tod ist eine **Abweichung vom Original** (dort fügt nichts dem Spieler Schaden zu) und bewusste Entscheidung: die Wölfe sollen tödlich sein.

## Wölfe

Zwei Figuren aus dem Original, die der Port zuerst weggelassen hatte — jetzt über das `patrol:`-Segment (Modul 7i) abgebildet:

| Wolf | Ort | Verhalten |
|---|---|---|
| `wolf` (Straßenwolf, als NPC) | `loc_3` | liegt quer über dem Nordausgang (`locked_by: wolf`), `attack wolf` im klassischen Kampf |
| `wolf_guardian` — „Wolf (Guardian)" | `loc_28` (Earth Shrine) | **steht still** (`guardian: true`), warnt und schlägt für 2 zu |
| `wolf_forest1/2/3` — „Patrolling Wolf" | Wandernd | Rundkurse `[12…21]`, `[17…13]`, `[20…13]` mit den Original-Indizes 1/0/2, warnen und beißen für 1 |

- **Takt:** Der Rundkurs läuft bei jedem **zugverbrauchenden** Befehl einen Raum weiter. `look`, `watch`, `help`, `inventory` kosten laut `consumesTurn` keinen Zug — die Wölfe ziehen also nur, wenn man wirklich handelt. (Das ist auch der Grund, warum die Waldroute im Durchlauf kontrollierbar bleibt.)
- **Warnung:** Der Original-Wortlaut „WARNING: A wolf is nearby!" feuert, sobald ein Wolf im selben Raum steht — genau wie das rote Banner in `gameLoop` beim Original. Zusätzlich listet `look` anwesende Wölfe als „Also here: …".
- **Im Original unblutig:** Dort waren die vier Wölfe reine Kulisse (`checkEncounter` warnte nur, `wolfHp`/`wolfAttack` wurden nie gelesen) und der Kampfschirm fügte dem Spieler **nie** Schaden zu. Dass Wölfe hier beißen und töten können, ist die bewusste Erweiterung.

## Kampfbildschirm (`combat.screen`)

Der Straßenwolf wird jetzt im Original-Format dargestellt. `thefog.yaml` setzt
dafür einen `combat.screen`-Block mit der **Original-Kampfkunst von `loc 54`**
(Wolfsgesicht + Baum, aus `TheFog/Ascii.hs`) — sonst nichts, es gelten also die
Vorgaben (Szenenzeile `>>You are in a Fight!<<`, Original-Flucht-Hinweis,
Balkenbreite 10).

Gemessen im Durchlauf (erste der vier Runden bis zum tödlichen Schlag):

```
________________________________________________________________________________
________________________________________________________________________________
     .---.
    |  _  |
    | (o o)|
   .\   /
    \_/|
 |
   |____|

            .  .
         .  |  .
      .  |  |  .
     |   |  |   |
     |   |  |   |
      \  |  /
        \|/
         |
                    You are fighting a wolf
                    >>You are in a Fight!<<
Your Atk: 10
Your Def: 7
Your HP:  30 [██████████]
Your Steps:   9
HP of the wolf: 30 [██████████]
You can 'attack' or try to 'flee'..
 What will u do?
You hit for 8, it hits you for 1.
```

- Gezeichnet wird **vor** der Auflösung der Runde, aus dem Zustand davor — wie im
  Original, das den Schirm ausgab und *dann* zuschlug. Die Folgerunde zeigt
  deshalb die neuen Balken (Spieler 29, Wolf 22).
- `Your Def: 7` ist die Verteidigung **mit ausgerüstetem Schild** (3 + 4); der
  Gegenschlag bleibt damit genau 1 (siehe Werte-Tabelle unten).
- `Your Steps` ist der Zugzähler der Engine (`turnCount`), nicht das `charSteps`
  des Originals — dort stand die Zeile immer auf 0, weil `addSteps` nie lief.
- **Nur der Straßenwolf** bekommt die Original-Kampfkunst: `screen.art` ist ein
  `CondText` mit einer Variante auf `at: player, room: loc_3` (der Straßenwolf
  steht fest in `loc_3`) — die Guardian-/Patrouillen-Kämpfe zeigen den Schirm
  ohne Kunst. Begründung: `combat.screen` ist ein Profil pro Adventure; der
  Schirm ist global, aber die Wolfsgesicht-Kunst gehört nur zum klassischen
  Wolf-Fight des Originals (`loc 54`).
- Alle fünf Wölfe tragen eine normale **Zustandskunst** (`npc.ascii`, gezeichnet
  pro Runde über `combatArtMsg`) — beim Straßenwolf erscheint sie zusätzlich zur
  Original-Kampfkunst (bewusste Entscheidung: die Kunst darf doppelt stehen).
  Die Wolfskunst ist eine Port-Neuzeichnung; das Original kannte nur den
  Fight-Schirm (`Ascii.hs` kennt nur `ascii 0/1/54`).

## Spieler-Werte und warum sie nicht die Original-Zahlen sind

`player: { max_hp: 30, attack: 10, defense: 3 }`.

Das Original nennt in `giveName` `attack 1 / defense 1 / life 10` — diese Werte sind dort aber **Kosmetik**: `fightLoop` kämpft gegen fest `enemyHP = 10` mit `getAtk = 1` (also genau 10 Schläge) und fügt dem Spieler überhaupt keinen Schaden zu; Schwert und Schild sind wirkungslos (`addAng` gibt das Argument unverändert zurück und wird nie aufgerufen). Ein Testlauf mit den Original-Zahlen machte das Spiel **ungewinnbar**: 1 Schaden pro Schlag gegen einen 30-KP-Wolf, während der Wolf (Angriff 8) für 3 konterte und den Spieler mit 10 KP im 4. Schlagwechsel tötete. Der `thefog`-E2E-Pfad fiel damit durch.

Die hier gesetzten Werte stellen die Original-**Mechanik** her statt die Original-**Ziffern**:

- `attack 10` → `max 1 (10 − 2) = 8` Schaden pro Schlag ⇒ der 30-KP-Wolf fällt im 4. Schlag.
- `defense 3` (+ Schild 4) = 7 ⇒ Wolfskonter `8 − 7 = 1`. **Mindestens 1 Schaden bleibt immer** — auch mit Schild wird der Wolf nicht harmlos; ohne Schild sind es 5. (Eigener Schaden hat eine Untergrenze von 1, Gegnerschaden von 0 — deshalb Verteidigung 3 statt 5, sonst wäre der Konter genau 0.)
- `max_hp 30` deckt die Summe aus Wolfskonter (3), Patrouillenbissen (5) und Wächterschlägen (4) ab.

## Schlüsselmechaniken

- **Kristall** liegt im Basement (`loc_4`) in der Truhe (`in_container: chest`); erst `search chest` gibt ihn frei. Ohne Kristall sagen alle vier Schreine „… is dormant. You need the crystal to awaken it." (`if: { has_item: crystal }`).
- **Schwert** (`loc_30`, in der Grabkammer): **wirkungslos und nicht ausrüstbar** — wie im Original. Die Prophezeiung verheißt, dass ein Held mit dem Schwert den Nebel bezwingt; der Protagonist ist dieser Held nicht, also kann er es nicht führen. (Im Port gab es `weapon`-Slot + `attack+9`, das widersprach der Erzählabsicht.)
- **Schild** ebenfalls im Basement (im Original wirkungslos, im Port `defense+4`).
- **Wolf blockiert** den Nordausgang von `loc_3` nach `loc_6` (`locked_by: wolf`). Erst sein Tod entriegelt den Weg. (Erfindung des Ports — im Original blockiert der Wolf nichts.)
- **Hinter dem Wolf** liegt die Earth Shrine (Waldroute) und Schwert/Grabhöhle — ohne Wolf-Tod unerreichbar.
- **Water/Fire/Air Shrines** sind auch ohne Wolf-Tod erreichbar (West-/Ost-Route ab `loc_2`), aber erst mit Kristall aktivierbar.
- **Prinzessin** (`use princess` an `loc_33`): ⅓ Ertrinken, ⅔ Rettung — via `random`-Effect mit Gewichten 1:2 (Original: `stepCounter mod 3 == 0` ⇒ ertrunken). Beim Ertrinken bleibt sie tot an der Fundstelle (`damage_npc`), ein weiteres `use princess` sagt „already dead. You failed to save her." Rettung ist **optional** und hat keinen Einfluss aufs Ende (im Original ebenfalls nicht).
- **Quests** starten über Regeln (`on: enter loc_<schrein/wolf/prinzessin>`), `once: true`.
