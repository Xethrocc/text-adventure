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
  profile: tactical           # off | narrative | classic | tactical
  initiative: by_speed        # player_first | enemy_first | by_speed (nur tactical)
  flee_allowed: true          # Flucht per 'flee' erlaubt (nur tactical)
  max_rounds: 10              # optionales Rundenlimit (nur tactical)
  speed_attribute: agility    # Attribut/Skill für Initiative-Vergleich (nur tactical)

abilities:
  - id: power_strike
    name: Power-Schlag
    cost_var: stamina
    cost: 15
    cooldown: 2
    effect:
      - { damage_npc: { npc: gladiator, amount: 20 } }
      - { msg: "Dein wuchtiger Hieb erschüttert den Champion!" }
```

**Architektur-Regel (umgesetzt):** `resolveCombat :: CombatProfile ->
[CombatActor] -> CombatTarget -> CombatAction -> GameState -> ([Effect], [String])` — eine
reine Funktion, die Effects erzeugt, die anschließend durch
`applyOutcomeWith` laufen. Kein zweiter Interpreter, kein zweiter
`applyLoopCommand`-Pfad. `Parser.executeAttack` ist ein dünner Wrapper
(Profil + `[PlayerActor]` + `TargetNPC` / `TargetShip` verdrahten, Effekte anwenden,
Meldungen durchreichen). `CombatActor` ist von Anfang an eine Liste —
Begleiter (7g) und Schiffe (7h) erweitern sie, ohne die Signatur zu ändern.

- **classic bleibt bit-identisch:** keine `combat:`-Block-Variante ist ein
  eigener Test (Engine: `testCombatDamageUsesDefense` u. a.), und die
  Fixture `combat-classic.yaml` belegt das Verhalten mit explizitem Block.
  Schaden = `attack − defense` (min 1 / min 0), Gegner kontert im selben
  Befehl, NPC-Tod via `killNPC`, Spieler-Tod via `endGame`.
- **narrative:** `effectiveAttack >= npcDefenseBase + difficulty` gewinnt.
  Die `on_win`-/`on_lose`-Effects entscheiden alles; die klassische
  HP-Attrition entfällt komplett (Fixture: Kampf öffnet ein Tor, statt den
  Gegner zu zerhauen).
- **off:** `attack` wird mit `attack_refused`-Text abgelehnt; niemand
  verliert HP (Fixture: „Der Verhandlungsweg").
- **tactical (7f-3):** Rundenbasierter Taktikkampf mit Runden-Treiber:
  - Aktionen: `attack` / `hit`, `defend` (Verteidigungsbonus für eine Runde),
    `flee` (Fluchtversuch, falls `flee_allowed: true`), `use-ability <id>` /
    `ability <id>` (Spieler-Fähigkeiten mit Ressourcenkosten und Cooldown).
  - Initiative: `player_first`, `enemy_first` oder `by_speed` (dynamischer
    Wurf basierend auf `speed_attribute` vs. Gegner-Geschwindigkeit).
  - Rundenzustand in der VarMap: `combat.round`, `combat.engaged`,
    `combat.initiative.<actorId>`. Geschützt gegen Autorenkollisionen über
    `CombatVariableClash`.
  - Gegnerreaktion: gewöhnliche `on: turn`-Regel mit
    `when: { compare_var: { name: combat.engaged, op: gte, value: 1 } }`.
- **Validierung:** `UnknownCombatProfile` (unbekannter Profilname),
  `UnknownInitiativeRule` (ungültige Initiativ-Regel).

Fixtures:
- `examples/modules/combat-off.yaml` („Der Verhandlungsweg" — Wächterin, Umhang-Quest, Sieg ohne einen Schlag).
- `examples/modules/combat-narrative.yaml` („Der Duellplatz" — ein Wurf entscheidet, Tor öffnet sich).
- `examples/modules/combat-classic.yaml` („Der Kerkerkopf" — expliziter Classic-Block, 6 Treffer, Tod per `killNPC` öffnet den Schatzraum via `locked_by`).
- `examples/modules/combat-tactical.yaml` („Die Gladiatoren-Arena" — Taktikprofil, Fähigkeiten, Gegnerreaktion; E2E Happy Path `combat-tactical` und Flucht-Pfad `combat-tactical-fail`).

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
  `faction.<id>` (7a) und `party.<id>` (7g). Die Grenzen sind seit P1-8 wirksam:
  `SetValue`/`ModifyValue` auf eine `VTInt`-Variable clampen automatisch auf
  `min`/`max`, Autoren-Clamps sind nicht mehr nötig.
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
    `hull`; beide werden als `SetValue`-Effekte emittiert (die `VTInt`-Grenzen
    werden seit P1-8 beim Setzen durchgesetzt);
  - ohne Schilde **und** Hülle nimmt weiter der Spieler den Schaden (7f).
  - `hull <= 0` ist kein Kern-Sonderpfad: die Fixture beendet das Spiel mit
    einer `on: turn`-Rule.
- **Schiff-gegen-Schiff-Duelle (7h-2):**
  - `CombatTarget = TargetNPC String String | TargetShip VehicleID String` in `Combat.hs`.
  - Zielauflösung: `attack <ship>` / `fire <ship>` im Parser zielt auf Schiffe am selben Halt (`outsideStop`), wenn der Spieler sich nicht selbst im Zielschiff befindet.
  - Gewöhnliche Fahrzeuge ohne Systeme werden abgewiesen (`"You can't attack the <name>."`).
  - Schadensberechnung via `shipAbsorb`: Schüsse treffen zuerst Schilde, der Durchschlag geht auf die Hülle.
  - Zerstörung bei Hülle 0: setzt `ship.<id>.hull` auf 0, gibt eine Zerstörungsmeldung aus und unterdrückt Gegenfeuer.
  - Überlebende Schiffe feuern zurück (kostet 1 Energie), Schaden trifft Spielerschilde und -hülle.
  - Validierung: `ActorShip vId` in Regeln wird gegen `vehicleDefs` geprüft (`MissingVehicle`).
- **Fixtures:**
  - `examples/modules/starship.yaml` („Der Kestrel-Lauf", 14 Räume, E2E `ci/e2e/starship.in/.expect`): Stationen nutzen, fliegen, Batterie anstöpseln, NPC-Korsar beschießen.
  - `examples/modules/starship-loss.yaml`: Hüllenbruch-Verlustpfad (`ci/e2e/starship-loss-fail.*`).
  - `examples/modules/ship-duel.yaml` („Das Asteroiden-Duell", 14 Räume): echtes Duell Kestrel vs. Korsaren-Fregatte (`TargetShip`). E2E Happy Path `ci/e2e/ship-duel.*` (`VICTORY`) und Failure Path `ci/e2e/ship-duel-fail.*` (`hull_failure` Zerstörung).

---

## Kompositionsbeweis — `examples/modules/combo.yaml`

Abnahmepunkt 2 der Phase: **ein** Referenzspiel nutzt **fünf** Module
gleichzeitig (7a, 7b, 7d, 7g, 7h) — und kein Modul weiß vom anderen. Alle
Verknüpfungen sind Autoren-Regeln über bestehende Konzepte (Flags, `standing`,
`has_item`, `compare_var`, `{ at: player, room: … }`-Gates):

| Modul | Beitrag im Spiel |
|---|---|
| 7a Fraktionen | Gildenauftrag hebt `gilde` auf 12; die `standing`-Rule öffnet das Siegel (`set_state`); der Freibeuter-Deal ist der Gegenläufer (gilde −10 / freibeuter +10) |
| 7b Handel | `buy` auf Kanister (12) und Zelle (25) gegen `credits`; Bestand in `shop.haendler.*` |
| 7d Survival | `oxygen` −1 je Zug, `at_zero` = Tod; der zweite Drain macht den Strahlungssturm (`env.weather >= 1`) teurer |
| 7g Party | `folgen vela` holt die Schrauberin in die Gruppe; sie folgt und schlägt im Kampf mit |
| 7h Raumschiff | Tarsis VII mit `umleiten`/`aufladen` an den Stationen; Kampf gegen den Kaperer über Schilde und Hülle |

Die Verkettung: Kanister kaufen (7b) → Atemluft sichern (7d) → Vela anwerben
(7g) → Gildenauftrag annehmen (7a) → Stationen bedienen und zum Gürtel fliegen
(7h) → Kaperer besiegen (7h + 7g) → Schwarzbox bergen → beim Gildenbüro abgeben
(7a) → das Siegel öffnet den Tresorraum → Sieg. E2E:
`ci/e2e/combo.in/.expect`, registriert in `scripts/ci.sh` (28
Playthroughs: 20 Happy Paths + 8 Failure Paths).

---

## Neues Modul anlegen (Entwicklerleitfaden)

Die Modul-Architektur folgt einem strikten Schema: **YAML-Segment + Compiler-Pass auf bestehende Core-Konzepte**. Ein neues Modul erzeugt keinen eigenen Interpreter, kein eigenes `SaveState`-Feld und kein zweites State-Silo. Ohne das YAML-Segment bleibt jede bestehende Welt bit-identisch.

### 1. Compiler-Signatur

Jede Modul-Kompilierung in `worldbuilder/src/Worldbuilder/Compile.hs` folgt dem Schema:

```haskell
compileMyModule :: Adventure -> ([CompileIssue], [TriggerDef], Map String VarDef, Map String VariableValue)
```

Sie liefert vier Dinge:
1. `[CompileIssue]`: Diagnosefehler und Warnungen bei fehlerhaftem Schema oder Referenzen.
2. `[TriggerDef]`: generierte Trigger (z. B. `OnCommand`, `OnTurn`, `OnStateChange`).
3. `Map String VarDef`: Variablendefinitionen für die Welt (`varDefs`), inklusive Typ und Clamping-Grenzen (`VTInt (Just min) (Just max)`).
4. `Map String VariableValue`: Anfangswerte im `SaveState` (`variables`).

### 2. Die fünf Eintragungsstellen im Worldbuilder (`Compile.hs`)

Wenn ein neues YAML-Segment `mymodule:` eingeführt wird:

1. **AST-Erweiterung (`Worldbuilder/Types.hs`)**:
   - Feld `advMyModule :: Maybe AMyModule` im `Adventure`-Record ergänzen.
   - YAML-Dekodierer (`FromJSON Adventure`) um `.:? "mymodule"` erweitern.
2. **Aufruf in `compileAdventure` (`Compile.hs`)**:
   - `let (myIssues, myTriggers, myVarDefs, myInitVars) = compileMyModule adv`
3. **Namespace-Kollisionsprüfung (`mergeMyModuleVars`)**:
   - Autoren-Variablen dürfen den reservierten Modul-Präfix (z. B. `mymodule.`) nicht deklarieren:
   ```haskell
   [ ciError ("variables." ++ name) "MyModuleVariableClash"
       ("variable '" ++ name ++ "' uses reserved module prefix 'mymodule.'")
   | (name, _) <- Map.toList (advVariables adv), "mymodule." `isPrefixOf` name ]
   ```
4. **Zusammenführung**:
   - `allTriggerDefs = ... ++ myTriggers`
   - `allVarDefs = ... ` (Map.union mit `myVarDefs`)
   - `allInitialVars = ... ` (Map.union mit `myInitVars`)
   - `allErrors = ... ++ myIssues`
5. **Validierung (`Compile.hs` und `src/Validate.hs`)**:
   - Referenzprüfungen gegen unbekannte IDs (Räume, NPCs, Items, Verben) durchführen.
   - Falls Trigger generiert werden: sicherstellen, dass deren IDs den reservierten Präfix tragen und keine Kollisionen erzeugen.

### 3. CI- und Test-Integration

Zu jedem Modul gehört:
1. **Unit-Tests (`worldbuilder/test/Tests.hs` & `test/Tests.hs`)**:
   - Test auf erfolgreiche Kompilierung und Validierung.
   - Test auf gemeldete Schemafehler / Clashes (`Duplicate...`, `Unknown...`, `...VariableClash`).
2. **Modul-Fixture (`examples/modules/<modul>.yaml`)**:
   - Vollständiges, spielbares Mini-Adventure (13–18 Räume).
3. **CI-Pipeline (`scripts/ci.sh`)**:
   - **Stufe 3 (Validierung)**: `examples/modules/<modul>.yaml` in die Liste aufnehmen.
   - **Stufe 4 (Happy Path E2E)**: `ci/e2e/<modul>.in` und `ci/e2e/<modul>.expect`.
   - **Stufe 5 (Failure Path E2E)**: `ci/e2e/<modul>-fail.in` und `ci/e2e/<modul>-fail.expect`.