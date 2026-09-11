# TheFog — kompletter Playthrough (verifiziert)

Dieser Durchlauf wurde **tatsächlich durchgespielt** (Engine v0.8, `worldbuilder compile` → `text-adventure --world … --save …`) und endet mit dem Victory-Screen.

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
attack wolf      (3–4 mal, bis "You attack the wolf and kill it!")
north north north east north north west south southeast
use earth shrine
south north east south south west south south south south east east south southeast
use water shrine
north north west west west west west west west west southeast
use fire shrine
south east east east east south south southeast southeast south
use air shrine
north north north north north east east east east south
use princess
north east east east east
-> VICTORY
```

## Ablauf mit Etappen

| # | Ziel | Weg ab vorherigem Punkt | Aktion |
|---|------|--------------------------|--------|
| 1 | Basement (`loc_4`) | `south` | `search chest` (→ Kristall), `take shield`, `equip shield`, zurück `north` |
| 2 | Wolf (`loc_3`) | `north north north` | `attack wolf` ×3–4 → Wolf tot, Nordausgang entriegelt |
| 3 | Earth Shrine (`loc_28`) | `north north north east north north west south southeast` | `use earth shrine` |
| 4 | Water Shrine (`loc_38`) | `south north east south south west south south south south east east south southeast` | `use water shrine` |
| 5 | Fire Shrine (`loc_45`) | `north north west west west west west west west west southeast` | `use fire shrine` |
| 6 | Air Shrine (`loc_52`) | `south east east east east south south southeast southeast south` | `use air shrine` |
| 7 | Prinzessin / River (`loc_33`) | `north north north north north east east east east south` | `use princess` (→ sie wird nach Home gebracht) |
| 8 | Throne Room (`loc_37`) | `north east east east east` | betreten → **VICTORY** |

## Schlüsselmechaniken

- **Kristall** liegt im Basement (`loc_4`), **Schild** ebenfalls. Mit Schild macht der Wolf 0 Schaden.
- **Wolf blockiert** den Nordausgang von `loc_3` nach `loc_6` (`locked_by: wolf`). Erst sein Tod entriegelt den Weg (Engine setzt `entityStates[wolf] = "unlocked"`).
- **Hinter dem Wolf** liegen Earth Shrine (Waldroute) und Schwert/Grabhöhle — ohne Wolf-Tod unerreichbar.
- **Water/Fire/Air Shrines** sind auch ohne Wolf-Tod erreichbar (West-/Ost-Route ab `loc_2`) — aber erst mit Kristall aktivierbar.
- **Thron-Ende**: Betreten von `loc_37` mit allen vier Schrein-Flags (`*_shrine_active`) → Victory; sonst passiert nichts.
- **Quests** starten über Regeln (`on: enter loc_<schrein/wolf/prinzessin>`), `once: true`.

## Pflicht-Mechaniken

- **Kristall ist Pflicht**: Ohne ihn sagen alle vier Schreine „… is dormant. You need the crystal to awaken it." (`if: { has_item: crystal }`).
- **Truhe ist Pflicht**: Der Kristall liegt im Basement **in der Truhe** (`in_container: chest`); erst `search chest` gibt ihn frei. Ohne Suche ist er nicht sichtbar und nicht nehmbar.
- Wolf-Tod entriegelt den Nordausgang; 4 Schrein-Flags entriegeln das Victory.
