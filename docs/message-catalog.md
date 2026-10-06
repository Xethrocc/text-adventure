# Engine-Message-Katalog (Phase 1.1)

**Stand:** 2026-09-28 · **Kataloggröße: 237 Keys** (`src/Messages.hs`, `catalogEntries`)

## Was diese Stufe leistet

Alle player-facing Meldungen der Engine sind aus den Modulen (`Parser`, `GameLoop`,
`Cards`, `Combat`, `Vehicles`, `Effects`, `Quests`, `Game`, `SaveLoad`, `World`,
`Frontend`) in einen zentralen Katalog mit stabilen, dotted Keys (`area.name`)
gezogen worden. Rendering läuft über `Messages.renderMsg` und damit über dieselbe
`{var}`-Interpolation (`formatStringWith`, aus `Game.hs` hierher verlagert), die
auch YAML-Texte nutzen. Sprache ist weiterhin Englisch als Default-Katalog;
Sprachpakete (`language: de`, `messages:`-Overrides) sind Phase 4.3 und legen
sich dann als Overlay auf genau diese Keys.

**Nachweis Byte-Identität:** Alle E2E-Eingaben aus `scripts/ci.sh` (41 Loop-Einträge
+ Worldgen) wurden vor dem Refactor als Golden-Capture gesichert und nach jedem
Teil-Commit erneut gespielt; `world.json`, `save.json` und die vollständige
Ausgabe sind über alle **42** Eingaben byte-identisch (`diff -r` leer).
Zusätzlich 363 Engine-Tests grün (0 Warnungen).

**Nachtrag (2026-09-28):** Vier Keys wurden nachträglich entfernt, weil sie keinen
Aufruf hatten — `map.dark`/`watch.dark` (im Dunkeln rendert `map`/`watch` die
autorenkonfigurierbare `darkRoomEv`-Meldung) sowie `map.legend_header`/`card.play.on`
(Dubletten bewusst geteilter Inline-Fragmente: `nl2 ++ evRaw "Legend:\n"` bzw.
`evRaw (" on " ++ target)`). `scripts/check-msg-catalog.sh` prüft das jetzt als
CI-Stufe in beide Richtungen: kein Katalog-Key ohne Aufrufstelle, keine
Aufrufstelle mit unbekanntem Key.

## Inventar-Methodik

1. Grep über alle Engine-Module nach String-Literalen mit großem Anfangsbuchstaben
   (das von der Review verwendete Rough-Count-Verfahren, ergab dort 102–130).
2. Manueller Call-Site-Sweep je Modul: *jede* Zeichenkette, die den Spieler
   erreicht — inklusive kleingeschriebener Fragmente (`"Undone."`, `"  (Empty)"`,
   `"compatible"`, Header-Zeilen), die der Rough-Grep nicht zählt.
3. Klassifikation (siehe unten) — nur Klasse **M/H** wurde katalogisiert.

### Ergebnis der Klassifikation

| Klasse | Bedeutung | Behandlung | Umfang (Appr.) |
|---|---|---|---|
| **M** | narrative Meldung an den Spieler | Katalog-Key | 182 Keys (inkl. H) |
| **H** | Header-/Label-/Menü-Zeile (Inventar:, === Journal ===, Menüs, Help) | Katalog-Key | in den 182 enthalten |
| **S** | Screen-/Art-Content (Kampfbildschirm, Kartenboxen, HP-Balken, `combatScreenDefaults`, deutsche HUD-Labels) | **bewusst nicht** — zieht mit 1.2 (EvArt/EvScreen, Styling-Modell) in strukturierte Payloads | ~20 Literale |
| **W** | Worldgen-Content-Templates (`Wildnis [{x}, {y}]`, Biome-Beschreibungen in `Game.hs`) | nicht Engine-UI, sondern generierter Weltinhalt | 2 Literale |
| **X** | interne Assertions (`error "Bad verb: "…`, JSON-Tags wie `"MetaFile"`, Env-Namen `TA_SAVES_DIR`, Typ-/Konstruktor-Namen in Show-Instanzen) | Programmierfehler-Indikatoren, keine Spieler-UI | ~80 Literale, überwiegend Types/*.hs |
| — | `Sample.hs` | in-code Adventure-Content (äquivalent zu YAML, nicht Engine-Text) | 54 Literale |

Die 182 Keys entstehen aus ~158 Rough-Grep-Literalen + ~30 kleingeschriebenen
Fragmenten; zusammengesetzte Meldungen wurden in Teil-Templates zerlegt
(z. B. `card.play` / `card.play.exhausted`), damit Sprachpakete
Satzstellung frei wählen können.

## Konventionen

- **Key-Format:** `bereich.name`, Kleinbuchstaben + `_`, keine Leerzeichen.
  Stabil = Key ändert sich nie; der *Text* kann sich ändern (auch per Override).
- **Args:** `[(String, String)]`; Template-Platzhalter `{arg}` mit denselben
  Modifikatoren wie bei YAML-Texten (`{n:+}`, `{n:6}`). Unbekannte Platzhalter
  bleiben als `{name}` stehen (einheitliches Verhalten mit `formatStringWith`).
- **Fehlender Key:** `renderMsg` rendert laut als `<msg:key>` — nie still
  ersetzten; die 363 Tests und die E2E-Goldens decken alle Pfade ab, in denen
  das auftauchen könnte.
- **Kein Zugriff auf Spielvariablen in Engine-Templates** (bewusst): `renderMsg`
  substituiert nur Args. Damit kann eine YAML-Variable namens `item` keine
  Engine-Meldung umlenken. Ob Engine-Texte später Variablen interpolieren dürfen,
  entscheidet 4.3 (Sprachpakete) — dann mit klarem Namensraum.

## Katalog (generiert aus `catalogEntries`, sortiert)

Sehr lange Templates (z. B. `help.text`) sind in der Tabelle bei 120 Zeichen
abgeschnitten mit `…`; maßgeblich ist `src/Messages.hs`.

<!-- BEGIN GENERATED (aus src/Messages.hs via scripts/gen-msg-catalog.py — nicht von Hand editieren) -->
| # | MsgId | Template (en) | Template (de) |
|---|---|---|---|
| 1 | `ability.use` | `You use {ability}!` | `Du benutzt {ability}!` |
| 2 | `attack.cant_here` | `You can't attack the {target} here.` | `Hier kannst du {target} nicht angreifen.` |
| 3 | `attack.cant_target` | `You can't attack the {target}.` | `Du kannst {target} nicht angreifen.` |
| 4 | `attack.none_here` | `There's nothing to fight here.` | `Hier gibt es nichts zu bekämpfen.` |
| 5 | `card.cost_insufficient` | `Not enough {res} (need {need}, have {have}).` | `Nicht genug {res} (braucht {need}, hat {have}).` |
| 6 | `card.count_suffix` | ` (x{count})` | ` (x{count})` |
| 7 | `card.discard_pile_header` | `=== Discard Pile ({n} cards) ===` | `=== Ablagestapel ({n} Karten) ===` |
| 8 | `card.draw_pile_header` | `=== Draw Pile ({n}/{total} cards) ===` | `=== Nachziehstapel ({n}/{total} Karten) ===` |
| 9 | `card.hand_empty` | `Your hand is empty.` | `Deine Hand ist leer.` |
| 10 | `card.invalid_number` | `Invalid card number {idx}. You have {n} card(s) in hand.` | `Ungültige Kartennummer {idx}. Du hast {n} Karte(n) auf der Hand.` |
| 11 | `card.no_deck` | `You don't have a deck.` | `Du hast kein Deck.` |
| 12 | `card.no_deck_endturn` | `You don't have a deck to end your turn.` | `Du hast kein Deck, mit dem du deinen Zug beenden könntest.` |
| 13 | `card.no_deck_play` | `You don't have a deck to play cards from.` | `Du hast kein Deck, aus dem du Karten spielen könntest.` |
| 14 | `card.no_enemies` | `There are no living enemies here to target.` | `Hier gibt es keine lebenden Gegner zum Anvisieren.` |
| 15 | `card.no_match` | `No living enemy matches '{target}'.` | `Kein lebender Gegner passt zu '{target}'.` |
| 16 | `card.pile_empty` | `  (Empty)` | `  (Leer)` |
| 17 | `card.pile_line` | `  - {name}` | `  - {name}` |
| 18 | `card.piles_footer` | `Hand: {hand} \| Discard: {discard} \| Exhaust: {exhaust}` | `Hand: {hand} \| Ablage: {discard} \| Erschöpft: {exhaust}` |
| 19 | `card.play` | `You play {card}` | `Du spielst {card}` |
| 20 | `card.play.exhausted` | ` (Exhausted).` | ` (Erschöpft).` |
| 21 | `card.specify_target` | `Please specify a target (e.g. 'play <n> <target>'). Available: {names}` | `Bitte gib ein Ziel an (z. B. 'play <n> <ziel>'). Verfügbar: {names}` |
| 22 | `card.turn_ended` | `Turn ended. Energy restored to {max}. Drew {n} cards.` | `Zug beendet. Energie auf {max} aufgefüllt. {n} Karten gezogen.` |
| 23 | `card.unknown` | `Unknown card: '{id}'.` | `Unbekannte Karte: '{id}'.` |
| 24 | `chapter.no_next` | `There is no next chapter.` | `Es gibt kein nächstes Kapitel.` |
| 25 | `chapter.refuse_back` | `This chapter is behind you.` | `Dieses Kapitel liegt hinter dir.` |
| 26 | `chapter.unknown` | `There is no such chapter.` | `Dieses Kapitel gibt es nicht.` |
| 27 | `choice.invalid` | `Invalid choice. Please select a number from 1 to {max}.` | `Ungültige Auswahl. Bitte gib eine Zahl von 1 bis {max} ein.` |
| 28 | `combat.ability_cooldown` | `Ability is on cooldown.` | `Die Fähigkeit lädt noch auf.` |
| 29 | `combat.ability_unknown` | `Unknown ability '{id}'.` | `Unbekannte Fähigkeit '{id}'.` |
| 30 | `combat.ability_use` | `Round {round}: You use {ability}!` | `Runde {round}: Du benutzt {ability}!` |
| 31 | `combat.ally_strike` | `{ally} strikes for {dmg}.` | `{ally} schlägt zu und verursacht {dmg}.` |
| 32 | `combat.attack_hit` | `Round {round}: You hit the {target} for {dmg}.` | `Runde {round}: Du triffst {target} für {dmg}.` |
| 33 | `combat.attack_kill` | `Round {round}: You attack the {target} and kill it!` | `Runde {round}: Du greifst {target} an und tötest es!` |
| 34 | `combat.attack_ship` | `Round {round}: You attack the {target}.` | `Runde {round}: Du greifst {target} an.` |
| 35 | `combat.attack_ship_destroy` | `Round {round}: You attack the {target} and destroy it!` | `Runde {round}: Du greifst {target} an und zerstörst es!` |
| 36 | `combat.classic_attack` | `You attack the {target}.` | `Du greifst {target} an.` |
| 37 | `combat.classic_destroy` | `You attack the {target} and destroy it!` | `Du greifst {target} an und zerstörst es!` |
| 38 | `combat.classic_exchange` | `You hit for {dmg}, it hits you for {npc_dmg}.` | `Du triffst für {dmg}, es trifft dich für {npc_dmg}.` |
| 39 | `combat.classic_kill` | `You attack the {target} and kill it!` | `Du greifst {target} an und tötest es!` |
| 40 | `combat.defend` | `Round {round}: You brace yourself against the {target}.` | `Runde {round}: Du stellst dich {target} zur Wehr.` |
| 41 | `combat.flee` | `Round {round}: You flee from the {target}!` | `Runde {round}: Du fliehst vor {target}!` |
| 42 | `combat.flee_denied` | `You can't flee from the {target}!` | `Du kannst nicht vor {target} fliehen!` |
| 43 | `combat.lose` | `You lose the fight against the {target}. Your attack is turned aside.` | `Du verlierst den Kampf gegen {target}. Dein Angriff wird abgewehrt.` |
| 44 | `combat.not_engaged` | `You are not in combat.` | `Du befindest dich in keinem Kampf.` |
| 45 | `combat.not_enough_resources` | `Not enough resources.` | `Nicht genug Ressourcen.` |
| 46 | `combat.not_yet` | `You can't do that in combat yet.` | `Das kannst du im Kampf noch nicht tun.` |
| 47 | `combat.ship_destroyed` | `The {target} is already destroyed.` | `{target} ist bereits zerstört.` |
| 48 | `combat.strikes_back_kill` | `The {target} strikes back and kills you!` | `{target} schlägt zurück und tötet dich!` |
| 49 | `combat.win` | `You win the fight against the {target}! Your attack lands cleanly.` | `Du gewinnst den Kampf gegen {target}! Dein Treffer sitzt sauber.` |
| 50 | `consume.not_reachable` | `That is not within your reach.` | `Das liegt nicht bei dir.` |
| 51 | `container.already_closed` | `{name} is already closed.` | `Bereits geschlossen: {article_nom} {name}.` |
| 52 | `container.already_open` | `{name} is already open.` | `Bereits offen: {article_nom} {name}.` |
| 53 | `container.closed` | `{name} is closed now.` | `Jetzt ist {article_nom} {name} geschlossen.` |
| 54 | `container.contains` | `In {name}: {items}.` | `In {article_dat} {name}: {items}.` |
| 55 | `container.empty` | `{name} is empty.` | `Leer: {article_nom} {name}.` |
| 56 | `container.full` | `There is no room in {name}.` | `In {article_dat} {name} ist kein Platz mehr.` |
| 57 | `container.is_closed` | `{name} is closed.` | `Geschlossen: {article_nom} {name}.` |
| 58 | `container.is_locked` | `{name} is locked.` | `Verschlossen: {article_nom} {name}.` |
| 59 | `container.locked` | `{name} is locked now.` | `Jetzt ist {article_nom} {name} verschlossen.` |
| 60 | `container.move_refused` | `You can't move an item into a container that way.` | `So kannst du keinen Gegenstand in einen Behälter legen.` |
| 61 | `container.no_item` | `You find no {item} in {name}.` | `Du findest in {name_article_dat} {name} kein {item}.` |
| 62 | `container.not_a_container` | `{target} is not a container.` | `{target} ist kein Behälter.` |
| 63 | `container.not_locked` | `{name} is not locked.` | `Ist nicht verschlossen: {article_nom} {name}.` |
| 64 | `container.opened` | `{name} is open now.` | `Jetzt ist {article_nom} {name} offen.` |
| 65 | `container.put` | `You put {item} in {name}.` | `Du legst {item_article_acc} {item} in {name_article_acc} {name}.` |
| 66 | `container.took_from` | `You take {item} from {name}.` | `Du nimmst {item_article_acc} {item} aus {name_article_dat} {name}.` |
| 67 | `container.unlocked` | `{name} is unlocked now.` | `Jetzt ist {article_nom} {name} entriegelt.` |
| 68 | `craft.no_recipe` | `You don't know a recipe for {target}.` | `Du kennst kein Rezept für {target}.` |
| 69 | `dark.default` | `It's pitch black. You can't see anything.` | `Es ist pechschwarz. Du kannst nichts sehen.` |
| 70 | `death.title` | `  YOU HAVE DIED` | `  DU BIST GESTORBEN` |
| 71 | `device.cant_flip` | `You cannot flip that.` | `Das kannst du nicht umstellen.` |
| 72 | `device.cant_insert` | `You cannot insert that.` | `Das kannst du nicht hineinstecken.` |
| 73 | `device.cant_remove` | `You cannot remove that.` | `Das kannst du nicht herausnehmen.` |
| 74 | `device.examine_mounted` | `\nMounted: {item}.` | `
Eingesteckt: {item}.` |
| 75 | `device.flip` | `You flip the {device} to {state}.` | `Du stellst {device} auf {state}.` |
| 76 | `device.insert` | `You insert the {item} into the {device}.` | `Du steckst {item} in {device}.` |
| 77 | `device.not_carried` | `You are not carrying {item}.` | `Du hast {item} nicht dabei.` |
| 78 | `device.not_in_device` | `There is no {item} in the {device}.` | `In {device} steckt kein {item}.` |
| 79 | `device.occupied` | `There is already something in the {device}.` | `In {device} steckt bereits etwas.` |
| 80 | `device.reject` | `The {item} does not fit into the {device}.` | `{item} passt nicht in {device}.` |
| 81 | `device.remove` | `You remove the {item} from the {device}.` | `Du nimmst {item} aus {device}.` |
| 82 | `dialog.no_npc` | `There is no one by that name.` | `Hier ist niemand mit diesem Namen.` |
| 83 | `dialog.no_topic` | `{npc} has nothing to say about {topic}.` | `Über {topic} hat {npc} nichts zu sagen.` |
| 84 | `dialogue.choice_line` | `  [{i}] {text}` | `  [{i}] {text}` |
| 85 | `dialogue.ended` | `Dialogue ended.` | `Gespräch beendet.` |
| 86 | `dialogue.line` | `{name}: \"{text}\"` | `{name}: "{text}"` |
| 87 | `dialogue.none_active` | `You are not in a conversation right now.` | `Du führst gerade kein Gespräch.` |
| 88 | `dialogue.nothing_more` | `{name} has nothing more to say.` | `{name} hat nichts mehr zu sagen.` |
| 89 | `dialogue.nothing_to_say` | `{name} has nothing to say.` | `{name} hat nichts zu sagen.` |
| 90 | `dialogue.partner_gone` | `The person you were talking to is gone.` | `Die Person, mit der du gesprochen hast, ist weg.` |
| 91 | `disambiguate.option` | `[{n}] {name}` | `[{n}] {name}` |
| 92 | `disambiguate.prompt` | `Which do you mean: {names}?` | `Was meinst du: {names}?` |
| 93 | `drop.nothing` | `You're not carrying anything to drop.` | `Du hast nichts abzulegen.` |
| 94 | `drop.ok` | `You drop the {item}.` | `Du legst {article_acc} {item} ab.` |
| 95 | `end.rule_line` | `=========================================` | `=========================================` |
| 96 | `enter.not_seen` | `You don't see '{target}' here to enter.` | `Du siehst hier kein '{target}', in das du einsteigen könntest.` |
| 97 | `equip.header` | `Equipment:\n` | `Ausrüstung:
` |
| 98 | `equip.line` | `  {slot}: {name}` | `  {slot}: {name}` |
| 99 | `equip.need_carried` | `You need to be carrying the {item}.` | `Du musst {item} bei dir haben.` |
| 100 | `equip.not_equippable` | `You cannot equip the {item}.` | `Du kannst {item} nicht ausrüsten.` |
| 101 | `equip.nothing` | `You have nothing equipped.` | `Du hast nichts ausgerüstet.` |
| 102 | `equip.ok` | `You equip the {item}.` | `Du rüstest {article_acc} {item} aus.` |
| 103 | `equip.slot_occupied` | `You already have the {item} equipped there. Unequip it first.` | `Du hast dort bereits {item} ausgerüstet. Lege es zuerst ab.` |
| 104 | `fuel.status` | `{vehicle} fuel ({item}): {f}/{max}` | `Treibstoff von {vehicle} ({item}): {f}/{max}` |
| 105 | `fuel.status_zero` | `{vehicle} fuel ({item}): 0/{max}` | `Treibstoff von {vehicle} ({item}): 0/{max}` |
| 106 | `game.restart_start` | `Starting a new game...\n` | `Ein neues Spiel beginnt...
` |
| 107 | `gameover.custom` | `Game Over: {msg}` | `Spiel vorbei: {msg}` |
| 108 | `help.text` | `=== Available Commands ===\n\nMovement:\n  go/move/walk <direction>   - Move north/south/east/west/up/down\n  <directio…` | `=== Verfügbare Befehle ===

Bewegung:
  go/move/walk <richtung>    - In eine Richtung gehen
  nord/sued/ost/west/hoch/r…` |
| 109 | `inv.empty` | `You're not carrying anything.` | `Du trägst nichts bei dir.` |
| 110 | `inv.header` | `Inventory: {items}` | `Inventar: {items}` |
| 111 | `inventory.full` | `You are carrying too much.` | `Du trägst zu viel bei dir.` |
| 112 | `item.cant_do` | `You can't do that to the {item} right now.` | `Das kannst du mit {article_dat} {item} gerade nicht tun.` |
| 113 | `item.no_id` | `There is no item '{id}'.` | `Es gibt keinen Gegenstand '{id}'.` |
| 114 | `learn.default` | `Noted.` | `Notiert.` |
| 115 | `levelup.default` | `You reached level {level}: {name}!` | `Du erreichst Stufe {level}: {name}!` |
| 116 | `load.ironman_blocked` | `Loading is disabled in ironman mode.` | `Laden ist im Ironman-Modus deaktiviert.` |
| 117 | `load.permadeath` | `No load after death (permadeath).` | `Nach dem Tod kein Laden (Permadeath).` |
| 118 | `load.prompt` | `Enter save name to load (or press Enter for 'savegame'):` | `Speicherstand zum Laden eingeben (Enter für 'savegame'):` |
| 119 | `look.corpse_many` | `\nBodies lie here: {names}.` | `
Hier liegen Leichen: {names}.` |
| 120 | `look.corpse_one` | `\nThe body of {name} lies here.` | `
Der Körper von {name} liegt hier.` |
| 121 | `look.fuel` | `\nFuel ({item}: {f}/{max})` | `
Treibstoff ({item}: {f}/{max})` |
| 122 | `look.fuel_out` | `\nOut of {item} (0/{max})` | `
{item} aufgebraucht (0/{max})` |
| 123 | `look.items` | `\nYou see: {names}.` | `
Du siehst: {names}.` |
| 124 | `look.npcs` | `\nAlso here: {names}.` | `
Außerdem sind hier: {names}.` |
| 125 | `look.see_nothing` | `\nYou see nothing of interest.` | `
Du siehst nichts Interessantes.` |
| 126 | `look.void` | `You're in a void. There's nothing here.` | `Du befindest dich im Nichts. Hier gibt es nichts zu sehen.` |
| 127 | `map.legend_line` | `  {i}: {label}` | `  {i}: {label}` |
| 128 | `map.no_marks` | `There is nothing marked on the map.` | `Auf der Karte ist nichts markiert.` |
| 129 | `map.void` | `You're in a void. There's nothing to map.` | `Du befindest dich im Nichts. Hier gibt es nichts zu kartieren.` |
| 130 | `menu.death_full` | `  [U]ndo  \|  [L]oad last save  \|  [R]estart  \|  [Q]uit` | `  [U] Rückgängig  \|  [L] Laden  \|  [R] Neustart  \|  [Q] Ende` |
| 131 | `menu.restart_quit` | `  [R]estart  \|  [Q]uit` | `  [R] Neustart  \|  [Q] Ende` |
| 132 | `meta.corrupted` | `Warning: meta file '{path}' is corrupted — starting fresh.` | `Warnung: Meta-Datei '{path}' ist beschädigt - es wird neu begonnen.` |
| 133 | `meta.unreadable` | `Warning: meta file '{path}' is unreadable — starting fresh.` | `Warnung: Meta-Datei '{path}' ist unlesbar - es wird neu begonnen.` |
| 134 | `move.blocked` | `You can't go that way.` | `Dort kannst du nicht hindurch.` |
| 135 | `move.door_locked` | `The door is locked.` | `Die Tür ist verschlossen.` |
| 136 | `move.no_exit` | `There's nothing in that direction.` | `In diese Richtung führt kein Weg.` |
| 137 | `move.ok` | `You move {dir}.` | `Du gehst nach {dir}.` |
| 138 | `notes.empty` | `Your notes are empty.` | `Deine Notizen sind leer.` |
| 139 | `notes.header` | `Your notes:` | `Deine Notizen:` |
| 140 | `npc.already_dead` | `The {npc} is already dead.` | `{npc} ist bereits tot.` |
| 141 | `npc.cant_do` | `You can't do that to {npc}.` | `Das kannst du mit {npc} nicht tun.` |
| 142 | `npc.carries` | `\nCarrying: {items}.` | `
Dabei: {items}.` |
| 143 | `npc.dead_silent` | `The {npc} is dead and says nothing.` | `{npc} ist tot und sagt nichts.` |
| 144 | `npc.drops_items` | `\n{npc} drops: {items}.` | `
{npc} laesst fallen: {items}.` |
| 145 | `npc.gave_to` | `You give the {item} to {npc}.` | `Du gibst {item_article_acc} {item} an {npc_article_acc} {npc}.` |
| 146 | `npc.no_item` | `You find no {item} on {npc}.` | `Du findest {item} bei {npc_article_dat} {npc} nicht.` |
| 147 | `npc.took_from` | `You take the {item} from {npc}.` | `Du nimmst {item_article_acc} {item} von {npc_article_dat} {npc}.` |
| 148 | `npc.wears` | `\nWearing: {items}.` | `
Getragen: {items}.` |
| 149 | `parse.unknown` | `I don't understand '{input}'. Type 'help' for available commands.` | `Ich verstehe '{input}' nicht. Tippe 'help' für eine Befehlsliste.` |
| 150 | `proc.arity` | `That procedure cannot be called this way.` | `Diese Prozedur kann so nicht aufgerufen werden.` |
| 151 | `proc.unknown` | `That procedure does not exist.` | `Diese Prozedur existiert nicht.` |
| 152 | `pursuit.flee` | `{name} flees to {room}.` | `{name} flieht nach {room}.` |
| 153 | `pursuit.no_path` | `{name} can find no way.` | `{name} findet keinen Weg.` |
| 154 | `pursuit.step` | `{name} moves to {room}.` | `{name} bewegt sich nach {room}.` |
| 155 | `quest.cannot_start` | `You cannot start that quest right now.` | `Diese Quest kannst du gerade nicht beginnen.` |
| 156 | `quest.not_active` | `That quest is not active.` | `Diese Quest ist nicht aktiv.` |
| 157 | `quests.active_header` | `Active:` | `Aktiv:` |
| 158 | `quests.completed_header` | `Completed:` | `Abgeschlossen:` |
| 159 | `quests.journal_empty` | `Your journal is empty.` | `Dein Journal ist leer.` |
| 160 | `quests.journal_header` | `=== Journal ===\n` | `=== Journal ===
` |
| 161 | `quit.bye` | `Goodbye!` | `Auf Wiedersehen!` |
| 162 | `quit.thanks` | `Thanks for playing!` | `Danke fürs Spielen!` |
| 163 | `refuel.no_vehicle` | `There is no vehicle to refuel.` | `Hier gibt es kein Fahrzeug zum Betanken.` |
| 164 | `refuel.not_needed` | `The {vehicle} doesn't need fuel.` | `{vehicle} braucht keinen Treibstoff.` |
| 165 | `repair.nothing_broken` | `There is nothing broken about the {vehicle} that matches '{target}'.` | `An {vehicle} ist nichts kaputt, das zu '{target}' passt.` |
| 166 | `repair.ok` | `You repair the {vehicle} ({problem}).` | `Du reparierst {vehicle} ({problem}).` |
| 167 | `repair.problems` | ` Problems: {list}.` | ` Probleme: {list}.` |
| 168 | `save.compatible` | `compatible` | `kompatibel` |
| 169 | `save.corrupted` | `Error: Save file is corrupted or incompatible.` | `Fehler: Speicherstand ist beschädigt oder inkompatibel.` |
| 170 | `save.entry` | `  {name} — {timestamp} ({compat})` | `  {name} — {timestamp} ({compat})` |
| 171 | `save.list_header` | `=== Saved Games ===` | `=== Spielstände ===` |
| 172 | `save.loaded` | `Game loaded from {path} (saved: {timestamp}).` | `Spielstand geladen von {path} (gespeichert: {timestamp}).` |
| 173 | `save.loaded_legacy` | `Game loaded (legacy format).` | `Spielstand geladen (altes Format).` |
| 174 | `save.loaded_legacy2` | `Game loaded from legacy format.` | `Spielstand geladen aus altem Format.` |
| 175 | `save.none_found` | `No saved games found.` | `Keine Spielstände gefunden.` |
| 176 | `save.not_found` | `Error: Save file '{path}' not found.` | `Fehler: Speicherstand '{path}' nicht gefunden.` |
| 177 | `save.saved` | `Game saved to {path} ({timestamp}).` | `Spielstand gespeichert unter {path} ({timestamp}).` |
| 178 | `save.savezone_only` | `You can only rest at a savezone.` | `Speichern ist nur in einer Sicherheitszone möglich.` |
| 179 | `save.slot_delete_failed` | `Warning: could not delete save slot {path}.` | `Warnung: Speicherstand {path} konnte nicht gelöscht werden.` |
| 180 | `save.slot_deleted` | `Save slot deleted: {path}` | `Speicherstand gelöscht: {path}` |
| 181 | `save.unreadable` | `Error: Could not read file '{path}'.` | `Fehler: Datei '{path}' konnte nicht gelesen werden.` |
| 182 | `save.version_warning` | `Warning: This save was made with a different world version. Results may be unpredictable.` | `Warnung: Dieser Spielstand wurde mit einer anderen Weltversion erstellt. Die Ergebnisse können unvorhersehbar sein.` |
| 183 | `save.world_mismatch` | `world mismatch!` | `Welt abweichend!` |
| 184 | `search.nothing` | `You find nothing of interest.` | `Du findest nichts Interessantes.` |
| 185 | `search.nothing_item` | `You find nothing special about the {item}.` | `An {article_dat} {item} findest du nichts Besonderes.` |
| 186 | `search.nothing_npc` | `You find nothing on {npc}.` | `An {npc_article_dat} {npc} findest du nichts.` |
| 187 | `search.reveal` | `You find the {item}.` | `Du findest {article_acc} {item}.` |
| 188 | `search.void` | `You're in a void. There's nothing to search.` | `Du befindest dich im Nichts. Hier gibt es nichts zu durchsuchen.` |
| 189 | `stats.attack` | `Attack:  {atk} (base {base})` | `Angriff:  {atk} (Basis {base})` |
| 190 | `stats.conditions` | `Conditions: {conds}\n` | `Zustände: {conds}
` |
| 191 | `stats.defense` | `Defense: {def} (base {base})` | `Verteidigung: {def} (Basis {base})` |
| 192 | `stats.health` | `Health:  {hp} / {max}` | `Gesundheit:  {hp} / {max}` |
| 193 | `stats.progression` | `Level {level} — {name} ({xp}/{next} XP)` | `Stufe {level} — {name} ({xp}/{next} XP)` |
| 194 | `stats.progression_max` | `Level {level} — {name} ({xp} XP)` | `Stufe {level} — {name} ({xp} XP)` |
| 195 | `stats.skills` | `Skills: {skills}\n` | `Fähigkeiten: {skills}
` |
| 196 | `take.already` | `You already have the {item}.` | `Du hast {article_acc} {item} bereits.` |
| 197 | `take.none_here` | `There's nothing here to take.` | `Hier gibt es nichts zu nehmen.` |
| 198 | `take.not_portable` | `You can't take the {item}.` | `Du kannst {article_acc} {item} nicht mitnehmen.` |
| 199 | `take.ok` | `You take the {item}.` | `Du nimmst {article_acc} {item}.` |
| 200 | `target.not_carried` | `You don't have '{target}'.` | `Du hast '{target}' nicht.` |
| 201 | `target.not_seen` | `You don't see '{target}' here.` | `Du siehst '{target}' hier nicht.` |
| 202 | `ui.press_enter` | `  [Press Enter to continue]` | `  [Enter drücken, um fortzufahren]` |
| 203 | `undo.disabled` | `Undo is disabled in this adventure.` | `Rückgängig ist in diesem Abenteuer deaktiviert.` |
| 204 | `undo.done` | `Undone.` | `Rückgängig gemacht.` |
| 205 | `undo.ironman` | `No undo in ironman mode.` | `Kein Rückgängig im Ironman-Modus.` |
| 206 | `undo.nothing` | `Nothing to undo.` | `Nichts rückgängig zu machen.` |
| 207 | `undo.permadeath` | `No undo after death (permadeath).` | `Nach dem Tod kein Rückgängig (Permadeath).` |
| 208 | `unequip.all` | `You remove all equipment.` | `Du legst die gesamte Ausrüstung ab.` |
| 209 | `unequip.not_equipped` | `The {item} is not equipped.` | `Du hast {article_acc} {item} nicht ausgerüstet.` |
| 210 | `unequip.ok` | `You unequip the {item}.` | `Du legst {article_acc} {item} ab.` |
| 211 | `use.not_carried` | `You need to be carrying '{item}' to use it.` | `Dazu musst du '{item}' bei dir haben.` |
| 212 | `use.nothing` | `Nothing happens.` | `Es passiert nichts.` |
| 213 | `use.ok` | `You use the {item}. {msg}` | `Du benutzt {article_acc} {item}. {msg}` |
| 214 | `use.unreachable` | `You can't reach '{entity}' from here.` | `Du kommst von hier nicht an '{entity}' heran.` |
| 215 | `vehicle.board` | `You board the {vehicle}.` | `Du steigst in {vehicle}.` |
| 216 | `vehicle.cant_drive` | `You can't drive there from here. Stations: {stations}` | `Von hier aus kannst du nicht dorthin fahren. Stationen: {stations}` |
| 217 | `vehicle.cant_steer` | `You can't steer the {vehicle}; it follows its own route.` | `Du kannst {vehicle} nicht steuern; es folgt seiner eigenen Route.` |
| 218 | `vehicle.disembark` | `You disembark from the {vehicle}.` | `Du steigst aus {vehicle} aus.` |
| 219 | `vehicle.drive_to` | `You drive to {stop}.` | `Du fährst nach {stop}.` |
| 220 | `vehicle.fuelled` | `The {vehicle} is fuelled ({f}/{max}).` | `{vehicle} ist betankt ({f}/{max}).` |
| 221 | `vehicle.manual_only` | `This vehicle only moves when you drive it.` | `Dieses Fahrzeug bewegt sich nur, wenn du fährst.` |
| 222 | `vehicle.need_controls` | `You need to be at the controls to drive.` | `Du musst an den Kontrollen sein, um zu fahren.` |
| 223 | `vehicle.no_here_enter` | `There is no '{id}' here to enter.` | `Hier gibt es kein '{id}', in das du einsteigen könntest.` |
| 224 | `vehicle.no_id` | `There is no '{id}'.` | `Es gibt kein '{id}'.` |
| 225 | `vehicle.not_here` | `The {vehicle} is not here.` | `{vehicle} ist nicht hier.` |
| 226 | `vehicle.not_in` | `You are not in a vehicle.` | `Du befindest dich in keinem Fahrzeug.` |
| 227 | `vehicle.not_on` | `You are not on a vehicle.` | `Du befindest dich in keinem Fahrzeug.` |
| 228 | `vehicle.route_end` | `The route has no further stops.` | `Die Route hat keine weiteren Haltepunkte.` |
| 229 | `vehicle.travel_on` | `You travel on to {stop}.` | `Du fährst weiter nach {stop}.` |
| 230 | `vehicle.warning` | `Warning: {list}!` | `Warnung: {list}!` |
| 231 | `victory.title` | `  VICTORY!` | `  SIEG!` |
| 232 | `watch.nothing` | `There is nothing to watch about {label}.` | `Über {label} gibt es nichts zu beobachten.` |
| 233 | `watch.start` | `Watching {label}...` | `Du beobachtest {label}...` |
| 234 | `watch.void` | `You're in a void. There's nothing to watch.` | `Du befindest dich im Nichts. Hier gibt es nichts zu beobachten.` |
| 235 | `world.file_unreadable` | `Could not read world file '{path}': {err}` | `Weltdatei '{path}' konnte nicht gelesen werden: {err}` |
| 236 | `world.save_unreadable` | `Could not read save file '{path}': {err}` | `Speicherdatei '{path}' konnte nicht gelesen werden: {err}` |
| 237 | `xp.clamped` | `XP cannot drop below 0.` | `XP können nicht unter 0 fallen.` |

<!-- END GENERATED -->