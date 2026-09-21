# TheFog — kompletter Playthrough (verifiziert)

Dieser Durchlauf wurde **tatsächlich durchgespielt** (Engine v0.8, `worldbuilder compile` → `text-adventure --world … --save …`) und endet mit dem **Fog-/Game-Over-Screen** — dem einzigen Ende des Spiels (Original-Getreue, siehe unten).

## Kurzfassung (Befehlsfolge)

```
south
look               <- Basement: du siehst die Truhe (der Kristall liegt drin)
search chest       <- gibt dir den Kristall
take shield
equip shield
north
north
north
north
attack wolf        (4 mal nötig, bis "You attack the wolf and kill it!")
north north north east north north west south southeast
use earth shrine
south north east south south west south south south south east east south southeast
use water shrine
north north west west west west west west west west southeast
use fire shrine
south east east east east south south southeast southeast south
use air shrine      <- vierter Schrein: das Spiel endet hier
-> THE FOG DESCENDS / GAME OVER
```

## Ablauf mit Etappen

| # | Ziel | Weg ab vorherigem Punkt | Aktion |
|---|------|--------------------------|--------|
| 1 | Basement (`loc_4`) | `south` | `search chest` (→ Kristall), `take shield`, `equip shield`, zurück `north` |
| 2 | Wolf (`loc_3`) | `north north north` | `attack wolf` ×4 → Wolf tot, Nordausgang entriegelt |
| 3 | Earth Shrine (`loc_28`) | `north north north east north north west south southeast` | `use earth shrine` |
| 4 | Water Shrine (`loc_38`) | `south north east south south west south south south south east east south southeast` | `use water shrine` |
| 5 | Fire Shrine (`loc_45`) | `north north west west west west west west west west southeast` | `use fire shrine` |
| 6 | Air Shrine (`loc_52`) | `south east east east east south south southeast southeast south` | `use air shrine` → **FOG / GAME OVER** |

## Das Ende

- **Aktivieren des vierten Schreins beendet das Spiel.** Rule `fog_descends` (`on: turn`, alle vier `*_shrine_active`-Flags):
  „The fog descends over the land... / Wolves grow stronger, guards fall, the world becomes hostile..." + `game_end: fog` → eigener End-Banner (`end_art.fog` → „GAME OVER").
- **Kein Victory.** Der frühere Thronsaal-Sieg (`enter loc_37` bei vier Schrein-Flags) war eine Erfindung des Ports und wurde entfernt. Der **Thronsaal (`loc_37`) ist reine Kulisse**, wie im Haskell-Original.
- Der Schrein-Flag-Mechanismus entspricht dem Original: `checkEnding = e && w && f && a` → dort ebenfalls sofortiges Ende („Game Over.").

## Schlüsselmechaniken

- **Kristall** liegt im Basement (`loc_4`) in der Truhe (`in_container: chest`); erst `search chest` gibt ihn frei. Ohne Kristall sagen alle vier Schreine „… is dormant. You need the crystal to awaken it." (`if: { has_item: crystal }`).
- **Schild** ebenfalls im Basement (im Original wirkungslos, im Port `defense+4`).
- **Wolf blockiert** den Nordausgang von `loc_3` nach `loc_6` (`locked_by: wolf`). Erst sein Tod entriegelt den Weg. (Erfindung des Ports — im Original blockiert der Wolf nichts.)
- **Hinter dem Wolf** liegt die Earth Shrine (Waldroute) und Schwert/Grabhöhle — ohne Wolf-Tod unerreichbar.
- **Water/Fire/Air Shrines** sind auch ohne Wolf-Tod erreichbar (West-/Ost-Route ab `loc_2`), aber erst mit Kristall aktivierbar.
- **Prinzessin** (`use princess` an `loc_33`) ist **optional** und hat keinen Einfluss aufs Ende (im Original ebenfalls nicht).
- **Quests** starten über Regeln (`on: enter loc_<schrein/wolf/prinzessin>`), `once: true`.