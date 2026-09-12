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