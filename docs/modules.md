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

---

## 7d — Survival, Wetter, Umweltgefahren

**Kernänderung: keine.** Das `environment:`-Segment ist Schema-Sugar auf
`on: turn`-Trigger: Wetter ist die Int-Variable `env.weather` (Index in
`states`), jede Transition wird ein OnTurn-Trigger mit `SetValue`, jeder
Drain ein OnTurn-Trigger mit `ModifyValue` + `Conditional` (at_zero bei ≤ 0).

```yaml
environment:
  weather:
    states: [clear, storm]
    initial: clear
    transitions:
      - { when: {compare_var: {name: day, op: gte, value: 3}}, to: storm,
          effects: [ { msg: "Ein Sturm zieht auf." } ] }
  drains:
    - { var: hunger, per_turn: -1, when: { not: { has_flag: fed } },
        at_zero: [ { game_end: death, msg: "Du verhungerst." } ] }
```

- **Wetter als Variable:** `env.weather` wird automatisch deklariert
  (`env.*`-Namespace ist reserviert, `EnvironmentVariableClash` bei
  Autor-Deklaration). CondText-Varianten im Raum prüfen den Index, z. B.
  `when: {compare_var: {name: env.weather, op: eq, value: 1}}` für „storm".
  Der Wetterzustand liegt damit im Save und überlebt Save/Load.
- **Retro-Notiz:** CondText in Räumen heißt im YAML `description:` mit
  `default`/`variants` — das alte String-Feld heißt `desc:`.
- **Mehrere Transitionen:** jede wird ein eigener `on: turn`-Trigger; gilt
  mehr als eine gleichzeitig, gewinnt die später deklarierte (Doku: Guard
  mit Flags bauen, wenn Einmaligkeit gewünscht ist).
- **Drain-Validierung:** `UnknownWeatherState` (initial/`to` nicht in
  `states`), `UnknownDrainVariable` (Var nicht unter `variables:`
  deklariert).
- **Engine-Tests:** „drain stops when flag fed" — Drain stoppt bei Flag,
  Tod bei 0; „trigger cooldown gates turns" (7c) gilt auch hier.

Fixture: `examples/modules/survival.yaml` — Der Eispass. Hunger-Drain
(essen → `fed` stoppt ihn), Tag-Zähler, Sturm ab Tag 4, Raumtext am Kamm
wechselt per CondText mit dem Wetter, Schutzhütte = Victory.

---

## 7e — Stealth und Lärm

**Kernänderung: keine — und das ist der Punkt.** Der Plan erlaubte maximal
*eine* generische Predicate-Erweiterung (`perceived_by`), aber die Fixture
brauchte sie nicht: Lärm, Schwelle und Wahrnehmung sind vollständig als
Daten ausdrückbar. `stealth:`-Segment kompiliert zu gewöhnlichen Triggern
und einer Variablen.

```yaml
stealth:
  noise:
    var: noise        # Variablenname (Default "noise")
    on_move: 2        # Lärm-Zuwachs bei Raumwechsel (eigener on:enter-Trigger je Raum)
    decay_per_turn: -1
    max: 10           # Obergrenze; der Compiler hängt Conditional-Clamps an
  observers:
    - npc: guard
      hears_at: 5
      cooldown: 3     # optional: N Turns bis die Wache wieder hört
      on_hear: [ { set_flag: alarmed, val: "true" }, ... ]
```

- **Reihenfolge:** Beobachter feuern VOR dem Decay-Trigger, damit die Wache
  den vollen Lärm desselben Zuges hört, bevor er abklingt.
- **Schleichen ist nicht magisch:** ein Custom-Verb (`sneak`) + eine
  `on: command sneak`-Regel, die `noise` senkt — dieselbe Datenmechanik wie
  `buy` in 7b.
- **Dunkle Räume / light_flag:** `isDark` existierte schon im Kern. Die
  Fixture nutzt es: Im dunklen Wachposten hört die Wache erst ab `hears_at`,
  mit Laterne (Item mit `tags: [lightsource]` oder `light_flag` gesetzt)
  greift eine eigene Regel mit niedrigerer Schwelle — Wahrnehmung als Daten,
  nicht als neues Predicate.
- **Validierung:** `UnknownObserverNPC` (Observer-NPC existiert nicht),
  `StealthVariableClash` (Autor deklariert die noise-Variable separat).
- **Engine-Tests:** „noise observer hears at threshold then cooldown" —
  Lärm steigt beim Bewegen, Wache hört ab Schwelle einmal, Cooldown remmt
  das Wiederhören, Decay läuft danach.

Fixture: `examples/modules/stealth.yaml` — Die Nachtschicht. Fünf
sneak-Bewegungen halten den Lärm unter der Hör-Schwelle der Wache; wer
rennt, wird gehört (Tod), wer die Laterne nimmt, wird im dunklen Posten
gesehen. Tresorraum = Victory.

---

## 7f — Kampfprofile

**Kernänderung: ja — der einzige echte Umbau der Phase 7.** Kampf verlässt
`Parser.executeAttack` und wird eine über Daten gewählte Policy im
Regelkern. Neue Datei: `src/Combat.hs`.

```yaml
combat:
  profile: classic            # off | narrative | classic
  # off:        attack wird abgelehnt (attack_refused), kein HP-Verbrauch
  # narrative:  vergleichender Wurf (Spieler-Angriff vs. Verteidigung +
  #             difficulty) -> on_win / on_lose-Effects, keine HP-Attrition
  # classic:    exakt das Verhalten vor 7f — Default ohne combat:-Block
```

**Architektur-Regel (umgesetzt):** `resolveCombat :: CombatProfile ->
[CombatActor] -> CombatTarget -> GameState -> ([Effect], [String])` — eine
reine Funktion, die Effects erzeugt, die anschließend durch
`applyOutcomeWith` laufen. Kein zweiter Interpreter, kein zweiter
`applyLoopCommand`-Pfad. `Parser.executeAttack` ist ein dünner Wrapper
(Profil + `[PlayerActor]` + `TargetNPC` verdrahten, Effekte anwenden,
Meldungen durchreichen). `CombatActor` ist von Anfang an eine Liste —
Begleiter (7g) erweitern sie, ohne die Signatur zu ändern.

- **classic bleibt bit-identisch:** keine `combat:`-Block-Variante ist ein
  eigener Test (Engine: `testCombatDamageUsesDefense` u. a.), und die
  Fixture `combat-classic.yaml` belegt das Verhalten mit explizitem Block.
  Schaden = `attack − defense` (min 1 / min 0), Gegner kontert im selben
  Befehl, NPC-Tod via `killNPC`, Spieler-Tod via `endGame`.
- **narrative:** `effectiveAttack >= npcDefenseBase + difficulty` gewinnt.
  Die `on_win`-/`on_lose`-Effects entscheiden alles; die klassische
  HP-Attrition entfällt komplett (Fixture: Kampf öffnet ein Tor, statt den
  gegner zu zerhauen).
- **off:** `attack` wird mit `attack_refused`-Text abgelehnt; niemand
  verliert HP (Fixture: „Der Verhandlungsweg").
- **7f-3 (`tactical`)** bleibt Teilstopp: im Worldbuilder mit
  `CombatProfileNotSupported` abgelehnt, bis ein Referenzspiel es verlangt
  (Entscheidung im Plan ✅). `CombatProfile`-JSON kennt den Konstruktor noch
  nicht — er kommt mit 7f-3.
- **Validierung:** `UnknownCombatProfile` (unbekannter Name),
  `CombatProfileNotSupported` (tactical bis 7f-3).

Fixtures: `examples/modules/combat-off.yaml` („Der Verhandlungsweg" —
Wächterin, Umhang-Quest, Sieg ohne einen Schlag), `combat-narrative.yaml`
(„Der Duellplatz" — ein Wurf entscheidet, Tor öffnet sich),
`combat-classic.yaml` („Der Kerkerkopf" — expliziter Classic-Block,
6 Treffer, Tod per `killNPC` öffnet den Schatzraum via `locked_by`).

---

## 7g — Party und Begleiter

**Kernänderung: ja — klein und an genau drei Stellen.** (1) `followParty` in
`Game.hs`, (2) Begleiter als zusätzliche Aktoren im 7f-Resolver, (3) der
Order-Verb-Toggle als Compiler-Pass. Kein neues Save-Feld, keine neue Datei
unter `src/`.

```yaml
npcs:
  - id: alwin
    name: Knappe Alwin
    max_hp: 20
    attack: 4
    defense: 2
    party:
      can_join: true         # false: Block inert
      order_verb: folgen     # Custom-Verb; toggelt Beitritt/Verlassen
      hp_tracked: true       # verlangt max_hp
      follow_msg: "…"
      stay_msg: "…"
```

- **Roster ohne State-Silo:** Mitgliedschaft ist der `VarMap`-Eintrag
  `party.<npcId>` (1 = folgt). Der Compiler deklariert ihn aus dem Block;
  Autoren dürfen ihn nicht zusätzlich unter `variables:` anlegen
  (`PartyVariableClash`) — die Referenz, dass ein Modul ohne neues Feld
  auskommt (vgl. `faction.<id>` in 7a, `env.*` in 7d).
- **Anwerben/Entlassen:** ein `npcVerbMap`-Eintrag
  `(VCustom order_verb, npc-state)` → `Conditional (party.<id> gte 1)`
  entlässt, sonst wirbt an. Derselbe Befehl toggelt. Autoren-Effekte auf
  demselben Schlüssel laufen zuerst, der Toggle danach. Core-Verben als
  `order_verb` werden abgelehnt (sie würden das Built-in für diesen NPC
  verdecken).
- **Folgen (Kern):** `followParty :: RoomID -> GameState -> GameState`
  verschiebt lebende Begleiter in den neuen Raum und wird überall dort
  aufgerufen, wo sich der Spielerraum ändert: `transitionToRoom` (Gehen **und**
  Teleport), `enterVehicle`, `exitVehicle`, `moveVehicleToStop`. Bewusste
  Alternative zu einem Trigger pro (Raum × NPC), das wären O(Räume × NPCs)
  Regeln für eine Kernmechanik.
- **Kampf (Kern):** `Parser.executeAttack` baut
  `PlayerActor : [CompanionActor nid | Mitglieder im Raum]`. Im `classic`-Profil
  schlägt jeder Begleiter mit, der dort steht, wo das Ziel steht
  (`max 1 (attack − defense)`); beendet der Spieler das Ziel im selben Schlag,
  schlagen Begleiter nicht mehr zu. Der Konter trifft weiter den Spieler
  (kein Balance-Umbau), `narrative`/`off` ignorieren Begleiter. Ohne Begleiter
  ist die Ausgabe bit-identisch (Engine-Test `testPartyNoCompanionUnchanged`).
- **Tod feuert ein Event:** `killNPC` feuert `OnStateChange <npc>`; ein toter
  Begleiter folgt nicht mehr und schlägt nicht mehr zu. Neu ist außerdem, dass
  die Meldungen dieser Tod-Event-Rules **durchgereicht** werden:
  `killNPCWithMsg`/`modifyNPCHealth`/`modifyValueProp` geben sie als String
  zurück, statt sie im Interpreter mit `fst` zu verwerfen (Engine-Test
  `testNPCDeathEventMessageShown`).
- **`damage_npc: { npc: id, amount: N }`** — Zucker auf den bestehenden
  `ModifyValue (VRProperty id "hp")`-Kern-Effekt, damit das YAML NPC-Schaden
  ausdrücken kann (Begleiter-Tod per Falle). Compile-Check `UnknownDamageNPC`.
- **Validierung:** `PartyOrderVerbUnknown`, `PartyHealthMissing`,
  `PartyVariableClash`, `UnknownDamageNPC`.
- **Referenz-Bausteine:** `collectFactionRefs` und `checkDamageNpcRefs` teilen
  sich jetzt `allWorldEffects`/`allWorldPredicates` — der nächste Modul-Check
  muss die Welt nicht erneut durchlaufen.

Fixture: `examples/modules/party.yaml` („Der Knappenzug", 14 Räume) — Knappe
per `folgen alwin` anwerben, Wolf mit zwei Aktoren besiegen, Steinschlag tötet
den Knappen, seine `on: state`-Rule öffnet das Runentor (`set_state`), Sieg in
der Bergkammer. E2E: `ci/e2e/party.in/.expect`.

---

## 7h — Erweiterte Fahrzeuge und Raumschiffe

**Kernänderung: ja — drei kleine Stellen.** (1) `Location "player" <room>`
prüft jetzt auch den Spielerraum (vorher nur NPCs/Items), (2) `ShipActor` im
7f-Resolver, (3) der Compiler-Pass für `systems:`/`stations:`. Kein neues
Save-Feld, keine neue Datei unter `src/`.

```yaml
vehicles:
  - id: kestrel
    type: player
    entry_room: ks_bridge      # = cockpit
    interior: [ ks_bridge, ks_engine, ks_guns, ks_cargo ]
    stops: { "Dock 7": ks_dock, "Asteroidengürtel": ks_asteroid }
    systems:
      power:   { initial: 6, max: 6 }     # je Salve 1
      shields: { initial: 8, max: 8 }     # fängt den Konter ab
      hull:    { initial: 12, max: 12 }   # nimmt den Rest
      weapons: { initial: 2, max: 5 }     # Schaden des Schiffes
    stations:
      - { room: ks_engine, verb: umleiten, effects: [ { add_var: ship.kestrel.power, delta: 3 } ] }
      - { room: ks_guns,   verb: aufladen, effects: [ { add_var: ship.kestrel.weapons, delta: 1 } ] }
```

- **Systeme = VarMap:** je System eine Variable `ship.<vehicleId>.<name>` mit
  `min 0`/`max` aus dem Block; Autoren dürfen sie nicht selbst deklarieren
  (`ShipVariableClash`) — der dritte Beleg der „kein State-Silo"-Regel nach
  `faction.<id>` (7a) und `party.<id>` (7g). Obergrenzen sind **Deko**: ein
  Clamp gehört als `if: { compare_var: … gt N } → set_var` in die Effekte.
- **Stationen = Interior-Raum + Verb:** der Compiler erzeugt je Station einen
  Trigger `ship.<id>.station.<room>` (`on: command <verb>`) mit dem Gate
  `{ at: player, room: <room> }` plus optionalem `when:`. Die einzige
  Kernänderung dafür ist generisch: `evalPredicate (Location "player" r)` war
  implementiert, aber blind für den Spieler (nur NPC/Item-Locations).
  Validierung: `UnknownStationRoom` (kein Interior-Raum),
  `UnknownStationVerb` (nicht deklariert — Core-Verben werden bewusst
  abgelehnt, sie würden das Built-in im Raum verdecken).
- **Schiffs-Kampf = Vehicle als Aktor:** `CombatActor` hat jetzt `ShipActor
  VehicleID`; `Parser.executeAttack` hängt das Schiff an, wenn der Spieler an
  Bord ist. Ein Fahrzeug „hat Systeme", sobald irgendeine `ship.<id>.*`-Variable
  existiert — ohne `systems:` bleibt alles bit-identisch (Engine-Test
  `testOrdinaryVehicleUnchanged`). Im `classic`-Profil:
  - das Schiff feuert `weapons` und verbraucht 1 `power`; ohne Energie bleibt
    es stumm (Meldung, kein Schaden);
  - der Konter trifft **das Schiff**: `shields` fangen ab, der Rest geht auf
    `hull`; beide werden als geclampte `SetValue`-Effekte emittiert, weil
    `VTInt`-Grenzen im Kern nicht durchgesetzt werden;
  - ohne Schilde **und** Hülle nimmt weiter der Spieler den Schaden (7f).
  - `hull <= 0` ist kein Kern-Sonderpfad: die Fixture beendet das Spiel mit
    einer `on: turn`-Rule.
- **Grenze der Ausbaustufe:** Ein *gegnerisches* Schiff ist in dieser Stufe
  ein NPC mit `max_hp`/`attack` (die Türme treffen dein Schiff). Duell
  Schiff-gegen-Schiff als zwei Fahrzeuge wäre ein eigener Schritt (7h-2) —
  `CombatTarget` kennt nur NPC-Ziele.
- **Fixtures:** `examples/modules/starship.yaml` („Der Kestrel-Lauf",
  14 Räume, E2E `ci/e2e/starship.in/.expect`): Stationen nutzen (Energie
  umleiten, Läufe aufladen), fliegen, Batterie anstöpseln (Clamp sichtbar),
  Korsar beschießen — Schilde fangen ab, brechen, die Hülle nimmt Schaden.