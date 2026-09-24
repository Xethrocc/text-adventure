# Genre-Fixtures

Neun Mini-Adventures, die den Engine-Kern gegen unterschiedliche
Genre-Anforderungen prüfen. Jede ist ein vollständiges, spielbares Adventure
(1 bis 18 Räume) und läuft **ohne Genre-spezifischen Engine-Code**.

Pfad: `examples/genres/`

| Fixture | Räume | Prüft |
|---|---|---|
| `pure-if.yaml` | 15 | Exploration, `search` + versteckte Items, Container (`in_container`), Dialog mit `visible_when`, Custom-Verb (`align`), Predicate-Ketten, Trigger-Puzzle. Kein Kampf. |
| `fantasy.yaml` | 18 | Custom-Verb `cast`, Variable `mana`, **Crafting** (item-on-item), Ressourcen-Gate (`compare_var`), Loot, Kampf, Quest mit Stages, Victory. |
| `cyberpunk.yaml` | 15 | Custom-Verb `hack`, Variable `heat` mit Eskalationsschwelle (`on: turn` + `compare_var`), Fraktions-Flag via Dialog, Infiltration, Exfil-Victory. |
| `space-opera.yaml` | 13 + Shuttle | Custom-Verb `dock`, Variable `oxygen` mit Drain + Tod bei 0, **Vehicle** (Stops, `start_stop`), Crew-NPC, Loot, Victory bei Rückkehr. |
| `detective.yaml` | 16 | Predicate-Ketten über 4 Beweise, Dialog-Optionen via `visible_when`, Custom-Verb `accuse`: richtige Anklage + Beweise = Victory, falsche Anklage = Failure-End. |
| `horror.yaml` | 17 | Variable `sanity` mit Drain, **Scheduler** (`on: turn` + `cooldown`), dunkle Räume mit `light_flag`, **vier Enden** (Madness/Caught/Escape/Ashes). |
| `economy_hamurabi.yaml` | 4 | Rundenbasierte Wirtschaft: `compute_var` mit `cmd.argN`, **`compare_var` Variable gegen Variable** (beide Zweige: „genug Gold" / „zu wenig"), `clamp`, `formatWithVars` mit Feldbreite (`{gold:6}`), Dashboard per Custom-Verb `status`, NPC-Dialog. Kein Ende — Läufe sind offen. |
| `deckbuilder_spire.yaml` | 4 | Kartenspiel: `cards`-Block (Kosten `energy`, `type`, `target`), deterministisches Deck, `play <n> [auf <ziel>]` / `spiele`, `cards`/`deck`/`end turn`, Kartenbox-HUD, Kampf gegen zwei Gegner. |
| `sandbox_wilderness.yaml` | 1 + generiert | Runtime-Worldgen: `sandbox_zones` mit Biome-Gewichten und Zell-Seed, Betreten materialisiert Räume (Tags `forest`/`clearing`), `generate_room` gräbt Stollen mit dynamischem Auf-/Ab-Exit, Ernte- und Energie-Formeln (`max`/`min`). |

## Nutzung

```bash
# kompilieren
cabal run worldbuilder -- compile examples/genres/<genre>.yaml -o /tmp/out

# validieren
cabal run worldbuilder -- validate examples/genres/<genre>.yaml

# spielen
cabal run text-adventure-cli -- --world /tmp/out/world.json --save /tmp/out/save.json
```

## Regressionsschutz

Der Worldbuilder-Test `the 6 original genre fixtures compile + validate clean`
lädt die sechs klassischen Dateien, kompiliert sie und prüft `validateWorld` +
`validateGameState`. Damit fällt auf, wenn eine Schema-Änderung ein Genre
bricht. Die drei neueren Genres (Wirtschaft, Kartenspiel, Sandbox) haben
daneben eigene Tests (`economy genre fixture …`, `deckbuilder genre fixture …`,
`testSandboxWildernessFixtureCompiles`). Die CI-Stufe 3 validiert zusätzlich
**jede** Datei unter `examples/genres/` über den Glob.

## Feststellung

Alle neun Genres kamen ohne neuen Interpreter oder State-Silo aus. Die vier
Lücken, die beim Bauen auftraten, wurden generisch geschlossen (siehe
`docs/adventure-schema.md` → Outcomes/Predicates):

- `visible_when` bei Dialog-Optionen
- Bare Custom-Verbs (`align`, `accuse`) und deren `OnCommand`-Name
- `OnUse/OnTake/OnDrop`-Events trugen den Rohtext statt der Item-ID
- `compare_var` (Variable gegen Literal **oder** gegen eine zweite Variable
  über `var:`/`other_var:`), `set_var`/`add_var`, `in_container`,
  `move_npc`, `game_end`, autorisierbare Entity-/Item-Interactions (Crafting)
