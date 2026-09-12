# Gameplay-Module (Phase 7)

Optionale Module sind **YAML-Segmente + Compiler-Pass** über dem bestehenden
Rule-Core. Sie erzeugen keine eigenen Interpreter, keine State-Silos und keine
eigenen Packages — Modulzustand lebt im bestehenden `SaveState` (bevorzugt als
`VarMap`-Eintrag), und alle Effekte laufen durch denselben Outcome-Interpreter
(`applyOutcomeWith`).

**Default-Invariante:** Ohne das Modul-YAML-Segment ändert sich nichts. Alle
bestehenden Fixtures und E2E-Erwartungen bleiben unverändert grün.

---

## 7a — Fraktionen und Reputation

**Kernänderung: keine.** Standing ist ein `VarMap`-Eintrag pro Fraktion,
kein neues Feld in `SaveState`.

```yaml
factions:
  - id: smugglers
    name: Schmuggler-Gilde
    initial: 0
    levels:
      - { at: 20,  name: ally }
      - { at: -20, name: hunted }
```

- Every faction seeds the variable `faction.<id>` (int, `initial`, default 0).
- **Effect:** `standing: { faction: smugglers, add: 20 }` → `ModifyValue
  (VRVariable "faction.smugglers") 20`; `set:` analog auf `SetValue`.
- **Predicate:** `standing: { faction: smugglers, at_least: 20 }` (auch
  `at_most`, `equals`) → `CompareVar` (Parse-Alias im Kern-FromJSON, kein
  neuer Konstruktor; gespeichert wird immer die kanonische `compare_var`-Form).
- **Compile-Fehler:** `DuplicateFaction` (doppelte IDs), `FactionVariableClash`
  (Autor deklariert `faction.X` als Variable), `UnknownFaction` (jede
  `faction.<id>`-Referenz in Effekten/Predicates, die nicht deklariert ist —
  nur geprüft, wenn das `factions:`-Segment vorhanden ist).

Fixture: `examples/modules/factions.yaml` — Zwei Fraktionen (Wache, Gilde).
Der Auftrag für die Wache senkt das Standing der Gilde; das Gleiche gilt
umgekehrt. Der Gilde-Gefallen hebt `faction.smugglers` auf 20, worauf eine
Rule (`on: turn` + `when: standing … at_least: 20`) per `set_state` das
`locked_by`-Tor zur Höhle öffnet; der Höhleneintrag ist das Victory-Ende.

---

## 7b — Handel und Ökonomie

**Kernänderung: keine.** Währung und Lagerbestand sind `VarMap`-Einträge;
`buy`/`sell` sind Custom-Verben, die Transaktionslogik läuft als
Outcome-Effekte über die Item-`verb_map` (Conditional + CompareVar).

```yaml
variables:
  - { name: credits, type: int, initial: 50 }
initial_variables:
  shop.merchant.rope: 2        # Lager — ein VarMap-Eintrag pro Artikel

verbs:
  - { name: buy, aliases: [purchase] }
  - { name: sell, aliases: [pawn] }
```

- **Preis + Bestand als Daten:** `{ if: {compare_var: {name: credits, op: gte,
  value: 12}}, then: [{add_var: credits, delta: -12}, {add_var:
  shop.merchant.rope, delta: -1}, {give: rope}], else: [{msg: …}] }` — der
  Erfolgszweig ändert Währung + Bestand, der Ablehnungszweig nur den Text.
- **Kauf ohne Deckung verändert nachweislich nichts** (nur `msg` im `else`).
- **Rabatt über 7a:** ein `standing`-Prädikat öffnet einen Dialogoption
  (Member-Preis), ein gesetztes Flag gated den günstigeren `if`-Zweig.
- **`on: command buy`-Rule** als generische Reaktion (z. B. Standing-Gewinn).

Fixture: `examples/modules/trade.yaml` — 13 Räume, Krämer mit drei Artikeln,
Member-Preis über die Händlergilde (7a), Sieg über das Werfttor (braucht das
Gildensiegel).

---

## 7c — Encounter-Tabellen

**Kernänderung: keine.** Ein `encounter_tables:`-Block kompiliert zu
gewöhnlichen Triggern, deren Effekt ein einziger `RandomChoice` über die
gewichteten Einträge ist; `on`, `when` und `cooldown` sind die normale
Trigger-Mechanik, ein optionales `when` pro Eintrag wird zu einem
`Conditional`-Gate. Die Engine kennt nur Trigger, RandomChoice und
Conditional — das Modul ist reiner Schema-Sugar.

```yaml
encounter_tables:
  - id: wilds
    on: turn
    cooldown: 2
    when: { not: { has_flag: camp_cleared } }
    entries:
      - { weight: 3, effects: [ { msg: "Ein Wolf!" }, { damage: 1 } ] }
      - { weight: 2, effects: [ { msg: "Nur Wind." } ] }
      - weight: 1
        when: { has_item: torch }
        effects: [ { msg: "Glühwürmchen." } ]
```

- Jede Tabelle → ein Trigger `encounter.<id>` mit `trEffects = [RandomChoice
  [(weight, effect) | Einträge]]`; ungültige Tabelle (`DuplicateEncounterTable`,
  `EmptyEncounterTable`, `BadEncounterWeight`) ist ein Compile-Fehler.
- **Determinismus:** gezogen wird aus dem `rngState` des Saves — gleicher
  Seed → identische Zugfolge (Engine-Test `testRandomChoiceDeterministic`).
- **Cooldown:** nach dem Feuern bleibt die Tabelle für `cooldown` weitere
  Feuerversuche stumm (Engine-Test `testTriggerCooldownGatesTurns`).

Fixture: `examples/modules/encounters.yaml` — Der Spieler durchquert die
Wildnis zum Schrein; Wölfe, Wind und (mit Fackel) Glühwürmchen wechseln sich
ab. Das Lagerfeuer setzt `camp_cleared`, wodurch die aggressive Tabelle
`wilds` verstummt und die ruhige `calm` übernimmt — das `when`-Gate auf
Tabellen- und Eintragsebene ist damit in einem Spiel belegt.