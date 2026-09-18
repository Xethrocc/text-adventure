# Genre-Fixtures

Sechs Mini-Adventures, die den Engine-Kern gegen unterschiedliche
Genre-Anforderungen prüfen. Jede ist ein vollständiges, spielbares Adventure
(13–18 Räume) und läuft **ohne Genre-spezifischen Engine-Code**.

Pfad: `examples/genres/`

| Fixture | Räume | Prüft |
|---|---|---|
| `pure-if.yaml` | 15 | Exploration, `search` + versteckte Items, Container (`in_container`), Dialog mit `visible_when`, Custom-Verb (`align`), Predicate-Ketten, Trigger-Puzzle. Kein Kampf. |
| `fantasy.yaml` | 18 | Custom-Verb `cast`, Variable `mana`, **Crafting** (item-on-item), Ressourcen-Gate (`compare_var`), Loot, Kampf, Quest mit Stages, Victory. |
| `cyberpunk.yaml` | 15 | Custom-Verb `hack`, Variable `heat` mit Eskalationsschwelle (`on: turn` + `compare_var`), Fraktions-Flag via Dialog, Infiltration, Exfil-Victory. |
| `space-opera.yaml` | 13 + Shuttle | Custom-Verb `dock`, Variable `oxygen` mit Drain + Tod bei 0, **Vehicle** (Stops, `start_stop`), Crew-NPC, Loot, Victory bei Rückkehr. |
| `detective.yaml` | 16 | Predicate-Ketten über 4 Beweise, Dialog-Optionen via `visible_when`, Custom-Verb `accuse`: richtige Anklage + Beweise = Victory, falsche Anklage = Failure-End. |
| `horror.yaml` | 17 | Variable `sanity` mit Drain, **Scheduler** (`on: turn` + `cooldown`), dunkle Räume mit `light_flag`, **vier Enden** (Madness/Caught/Escape/Ashes). |

## Nutzung

```bash
# kompilieren
cabal run worldbuilder -- compile examples/genres/<genre>.yaml -o /tmp/out

# validieren
cabal run worldbuilder -- validate examples/genres/<genre>.yaml

# spielen
cabal run text-adventure -- --world /tmp/out/world.json --save /tmp/out/save.json
```

## Regressionsschutz

Der Worldbuilder-Test `all 6 genre fixtures compile + validate clean` lädt alle
sechs Dateien, kompiliert sie und prüft `validateWorld` + `validateGameState`.
Damit fällt auf, wenn eine Schema-Änderung ein Genre bricht.

## Feststellung

Alle sechs Genres kamen ohne neuen Interpreter oder State-Silo aus. Die vier
Lücken, die beim Bauen auftraten, wurden generisch geschlossen (siehe
`docs/adventure-schema.md` → Outcomes/Predicates):

- `visible_when` bei Dialog-Optionen
- Bare Custom-Verbs (`align`, `accuse`) und deren `OnCommand`-Name
- `OnUse/OnTake/OnDrop`-Events trugen den Rohtext statt der Item-ID
- `compare_var` (Variable vs. Literal), `set_var`/`add_var`, `in_container`,
  `move_npc`, `game_end`, autorisierbare Entity-/Item-Interactions (Crafting)
