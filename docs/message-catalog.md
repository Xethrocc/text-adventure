# Engine-Message-Katalog (Phase 1.1)

**Stand:** 2026-09-28 · **Kataloggröße: 247 Keys** (`src/Messages.hs`, `catalogEntries`)

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
| 55 | `container.dark_now` | `It is now pitch black.` | `Es ist jetzt stockdunkel.` |
| 56 | `container.empty` | `{name} is empty.` | `Leer: {article_nom} {name}.` |
| 57 | `container.full` | `There is no room in {name}.` | `In {article_dat} {name} ist kein Platz mehr.` |
| 58 | `container.is_closed` | `{name} is closed.` | `Geschlossen: {article_nom} {name}.` |
| 59 | `container.is_locked` | `{name} is locked.` | `Verschlossen: {article_nom} {name}.` |
| 60 | `container.locked` | `{name} is locked now.` | `Jetzt ist {article_nom} {name} verschlossen.` |
| 61 | `container.move_refused` | `You can't move an item into a container that way.` | `So kannst du keinen Gegenstand in einen Behälter legen.` |
| 62 | `container.no_item` | `You find no {item} in {name}.` | `Du findest in {name_article_dat} {name} kein {item}.` |
| 63 | `container.not_a_container` | `{target} is not a container.` | `{target} ist kein Behälter.` |
| 64 | `container.not_locked` | `{name} is not locked.` | `Ist nicht verschlossen: {article_nom} {name}.` |
| 65 | `container.opened` | `{name} is open now.` | `Jetzt ist {article_nom} {name} offen.` |
| 66 | `container.opened_reveals` | `Opening the {name} reveals {items}.` | `Öffnen des {name} zeigt {items}.` |
| 67 | `container.put` | `You put {item} in {name}.` | `Du legst {item_article_acc} {item} in {name_article_acc} {name}.` |
| 68 | `container.took_from` | `You take {item} from {name}.` | `Du nimmst {item_article_acc} {item} aus {name_article_dat} {name}.` |
| 69 | `container.unlocked` | `{name} is unlocked now.` | `Jetzt ist {article_nom} {name} entriegelt.` |
| 70 | `craft.no_product` | `There is no recipe that produces {target}.` | `Es gibt kein Rezept, das {target} herstellt.` |
| 71 | `craft.no_recipe` | `You don't know a recipe for {target}.` | `Du kennst kein Rezept für {target}.` |
| 72 | `dark.default` | `It's pitch black. You can't see anything.` | `Es ist pechschwarz. Du kannst nichts sehen.` |
| 73 | `death.title` | `  YOU HAVE DIED` | `  DU BIST GESTORBEN` |
| 74 | `device.cant_flip` | `You cannot flip that.` | `Das kannst du nicht umstellen.` |
| 75 | `device.cant_insert` | `You cannot insert that.` | `Das kannst du nicht hineinstecken.` |
| 76 | `device.cant_remove` | `You cannot remove that.` | `Das kannst du nicht herausnehmen.` |
| 77 | `device.examine_mounted` | `\nMounted: {item}.` | `
Eingesteckt: {item}.` |
| 78 | `device.flip` | `You flip the {device} to {state}.` | `Du stellst {device} auf {state}.` |
| 79 | `device.insert` | `You insert the {item} into the {device}.` | `Du steckst {item} in {device}.` |
| 80 | `device.not_carried` | `You are not carrying {item}.` | `Du hast {item} nicht dabei.` |
| 81 | `device.not_in_device` | `There is no {item} in the {device}.` | `In {device} steckt kein {item}.` |
| 82 | `device.occupied` | `There is already something in the {device}.` | `In {device} steckt bereits etwas.` |
| 83 | `device.reject` | `The {item} does not fit into the {device}.` | `{item} passt nicht in {device}.` |
| 84 | `device.remove` | `You remove the {item} from the {device}.` | `Du nimmst {item} aus {device}.` |
| 85 | `dialog.no_npc` | `There is no one by that name.` | `Hier ist niemand mit diesem Namen.` |
| 86 | `dialog.no_topic` | `{npc} has nothing to say about {topic}.` | `Über {topic} hat {npc} nichts zu sagen.` |
| 87 | `dialogue.choice_line` | `  [{i}] {text}` | `  [{i}] {text}` |
| 88 | `dialogue.ended` | `Dialogue ended.` | `Gespräch beendet.` |
| 89 | `dialogue.line` | `{name}: \"{text}\"` | `{name}: "{text}"` |
| 90 | `dialogue.none_active` | `You are not in a conversation right now.` | `Du führst gerade kein Gespräch.` |
| 91 | `dialogue.nothing_more` | `{name} has nothing more to say.` | `{name} hat nichts mehr zu sagen.` |
| 92 | `dialogue.nothing_to_say` | `{name} has nothing to say.` | `{name} hat nichts zu sagen.` |
| 93 | `dialogue.partner_gone` | `The person you were talking to is gone.` | `Die Person, mit der du gesprochen hast, ist weg.` |
| 94 | `disambiguate.option` | `[{n}] {name}` | `[{n}] {name}` |
| 95 | `disambiguate.prompt` | `Which do you mean: {names}?` | `Was meinst du: {names}?` |
| 96 | `drop.nothing` | `You're not carrying anything to drop.` | `Du hast nichts abzulegen.` |
| 97 | `drop.ok` | `You drop the {item}.` | `Du legst {article_acc} {item} ab.` |
| 98 | `end.rule_line` | `=========================================` | `=========================================` |
| 99 | `enter.not_seen` | `You don't see '{target}' here to enter.` | `Du siehst hier kein '{target}', in das du einsteigen könntest.` |
| 100 | `equip.header` | `Equipment:\n` | `Ausrüstung:
` |
| 101 | `equip.line` | `  {slot}: {name}` | `  {slot}: {name}` |
| 102 | `equip.need_carried` | `You need to be carrying the {item}.` | `Du musst {item} bei dir haben.` |
| 103 | `equip.not_equippable` | `You cannot equip the {item}.` | `Du kannst {item} nicht ausrüsten.` |
| 104 | `equip.nothing` | `You have nothing equipped.` | `Du hast nichts ausgerüstet.` |
| 105 | `equip.ok` | `You equip the {item}.` | `Du rüstest {article_acc} {item} aus.` |
| 106 | `equip.slot_occupied` | `You already have the {item} equipped there. Unequip it first.` | `Du hast dort bereits {item} ausgerüstet. Lege es zuerst ab.` |
| 107 | `fuel.status` | `{vehicle} fuel ({item}): {f}/{max}` | `Treibstoff von {vehicle} ({item}): {f}/{max}` |
| 108 | `fuel.status_zero` | `{vehicle} fuel ({item}): 0/{max}` | `Treibstoff von {vehicle} ({item}): 0/{max}` |
| 109 | `game.restart_start` | `Starting a new game...\n` | `Ein neues Spiel beginnt...
` |
| 110 | `gameover.custom` | `Game Over: {msg}` | `Spiel vorbei: {msg}` |
| 111 | `help.text` | `=== Available Commands ===\n\nMovement:\n  go/move/walk <direction>   - Move north/south/east/west/up/down\n  <directio…` | `=== Verfügbare Befehle ===

Bewegung:
  go/move/walk <richtung>    - In eine Richtung gehen
  nord/sued/ost/west/hoch/r…` |
| 112 | `inv.empty` | `You're not carrying anything.` | `Du trägst nichts bei dir.` |
| 113 | `inv.header` | `Inventory: {items}` | `Inventar: {items}` |
| 114 | `inventory.full` | `You are carrying too much.` | `Du trägst zu viel bei dir.` |
| 115 | `item.cant_do` | `You can't do that to the {item} right now.` | `Das kannst du mit {article_dat} {item} gerade nicht tun.` |
| 116 | `item.no_id` | `There is no item '{id}'.` | `Es gibt keinen Gegenstand '{id}'.` |
| 117 | `learn.default` | `Noted.` | `Notiert.` |
| 118 | `levelup.default` | `You reached level {level}: {name}!` | `Du erreichst Stufe {level}: {name}!` |
| 119 | `load.ironman_blocked` | `Loading is disabled in ironman mode.` | `Laden ist im Ironman-Modus deaktiviert.` |
| 120 | `load.permadeath` | `No load after death (permadeath).` | `Nach dem Tod kein Laden (Permadeath).` |
| 121 | `load.prompt` | `Enter save name to load (or press Enter for 'savegame'):` | `Speicherstand zum Laden eingeben (Enter für 'savegame'):` |
| 122 | `look.corpse_many` | `\nBodies lie here: {names}.` | `
Hier liegen Leichen: {names}.` |
| 123 | `look.corpse_one` | `\nThe body of {name} lies here.` | `
Der Körper von {name} liegt hier.` |
| 124 | `look.fuel` | `\nFuel ({item}: {f}/{max})` | `
Treibstoff ({item}: {f}/{max})` |
| 125 | `look.fuel_out` | `\nOut of {item} (0/{max})` | `
{item} aufgebraucht (0/{max})` |
| 126 | `look.items` | `\nYou see: {names}.` | `
Du siehst: {names}.` |
| 127 | `look.npcs` | `\nAlso here: {names}.` | `
Außerdem sind hier: {names}.` |
| 128 | `look.see_nothing` | `\nYou see nothing of interest.` | `
Du siehst nichts Interessantes.` |
| 129 | `look.void` | `You're in a void. There's nothing here.` | `Du befindest dich im Nichts. Hier gibt es nichts zu sehen.` |
| 130 | `map.legend_line` | `  {i}: {label}` | `  {i}: {label}` |
| 131 | `map.no_marks` | `There is nothing marked on the map.` | `Auf der Karte ist nichts markiert.` |
| 132 | `map.void` | `You're in a void. There's nothing to map.` | `Du befindest dich im Nichts. Hier gibt es nichts zu kartieren.` |
| 133 | `menu.death_full` | `  [U]ndo  \|  [L]oad last save  \|  [R]estart  \|  [Q]uit` | `  [U] Rückgängig  \|  [L] Laden  \|  [R] Neustart  \|  [Q] Ende` |
| 134 | `menu.restart_quit` | `  [R]estart  \|  [Q]uit` | `  [R] Neustart  \|  [Q] Ende` |
| 135 | `meta.corrupted` | `Warning: meta file '{path}' is corrupted — starting fresh.` | `Warnung: Meta-Datei '{path}' ist beschädigt - es wird neu begonnen.` |
| 136 | `meta.unreadable` | `Warning: meta file '{path}' is unreadable — starting fresh.` | `Warnung: Meta-Datei '{path}' ist unlesbar - es wird neu begonnen.` |
| 137 | `move.blocked` | `You can't go that way.` | `Dort kannst du nicht hindurch.` |
| 138 | `move.door_locked` | `The door is locked.` | `Die Tür ist verschlossen.` |
| 139 | `move.no_exit` | `There's nothing in that direction.` | `In diese Richtung führt kein Weg.` |
| 140 | `move.ok` | `You move {dir}.` | `Du gehst nach {dir}.` |
| 141 | `notes.empty` | `Your notes are empty.` | `Deine Notizen sind leer.` |
| 142 | `notes.header` | `Your notes:` | `Deine Notizen:` |
| 143 | `npc.already_dead` | `The {npc} is already dead.` | `{npc} ist bereits tot.` |
| 144 | `npc.cant_do` | `You can't do that to {npc}.` | `Das kannst du mit {npc} nicht tun.` |
| 145 | `npc.carries` | `\nCarrying: {items}.` | `
Dabei: {items}.` |
| 146 | `npc.dead_silent` | `The {npc} is dead and says nothing.` | `{npc} ist tot und sagt nichts.` |
| 147 | `npc.drops_items` | `\n{npc} drops: {items}.` | `
{npc} laesst fallen: {items}.` |
| 148 | `npc.gave_to` | `You give the {item} to {npc}.` | `Du gibst {item_article_acc} {item} an {npc_article_acc} {npc}.` |
| 149 | `npc.no_item` | `You find no {item} on {npc}.` | `Du findest {item} bei {npc_article_dat} {npc} nicht.` |
| 150 | `npc.took_from` | `You take the {item} from {npc}.` | `Du nimmst {item_article_acc} {item} von {npc_article_dat} {npc}.` |
| 151 | `npc.wears` | `\nWearing: {items}.` | `
Getragen: {items}.` |
| 152 | `parse.unknown` | `I don't understand '{input}'. Type 'help' for available commands.` | `Ich verstehe '{input}' nicht. Tippe 'help' für eine Befehlsliste.` |
| 153 | `proc.arity` | `That procedure cannot be called this way.` | `Diese Prozedur kann so nicht aufgerufen werden.` |
| 154 | `proc.unknown` | `That procedure does not exist.` | `Diese Prozedur existiert nicht.` |
| 155 | `pursuit.flee` | `{name} flees to {room}.` | `{name} flieht nach {room}.` |
| 156 | `pursuit.no_path` | `{name} can find no way.` | `{name} findet keinen Weg.` |
| 157 | `pursuit.step` | `{name} moves to {room}.` | `{name} bewegt sich nach {room}.` |
| 158 | `quest.cannot_start` | `You cannot start that quest right now.` | `Diese Quest kannst du gerade nicht beginnen.` |
| 159 | `quest.not_active` | `That quest is not active.` | `Diese Quest ist nicht aktiv.` |
| 160 | `quests.active_header` | `Active:` | `Aktiv:` |
| 161 | `quests.completed_header` | `Completed:` | `Abgeschlossen:` |
| 162 | `quests.journal_empty` | `Your journal is empty.` | `Dein Journal ist leer.` |
| 163 | `quests.journal_header` | `=== Journal ===\n` | `=== Journal ===
` |
| 164 | `quit.bye` | `Goodbye!` | `Auf Wiedersehen!` |
| 165 | `quit.thanks` | `Thanks for playing!` | `Danke fürs Spielen!` |
| 166 | `recipes.empty` | `You know no recipes.` | `Du kennst keine Rezepte.` |
| 167 | `recipes.entry` | `{name} — {ingredients}` | `{name} — {ingredients}` |
| 168 | `recipes.header` | `Recipes: {known} / {total}` | `Rezepte: {known} / {total}` |
| 169 | `recipes.learn.default` | `You learn a recipe: {recipe}.` | `Du lernst ein Rezept: {recipe}.` |
| 170 | `refuel.no_vehicle` | `There is no vehicle to refuel.` | `Hier gibt es kein Fahrzeug zum Betanken.` |
| 171 | `refuel.not_needed` | `The {vehicle} doesn't need fuel.` | `{vehicle} braucht keinen Treibstoff.` |
| 172 | `repair.nothing_broken` | `There is nothing broken about the {vehicle} that matches '{target}'.` | `An {vehicle} ist nichts kaputt, das zu '{target}' passt.` |
| 173 | `repair.ok` | `You repair the {vehicle} ({problem}).` | `Du reparierst {vehicle} ({problem}).` |
| 174 | `repair.problems` | ` Problems: {list}.` | ` Probleme: {list}.` |
| 175 | `save.compatible` | `compatible` | `kompatibel` |
| 176 | `save.corrupted` | `Error: Save file is corrupted or incompatible.` | `Fehler: Speicherstand ist beschädigt oder inkompatibel.` |
| 177 | `save.entry` | `  {name} — {timestamp} ({compat})` | `  {name} — {timestamp} ({compat})` |
| 178 | `save.list_header` | `=== Saved Games ===` | `=== Spielstände ===` |
| 179 | `save.loaded` | `Game loaded from {path} (saved: {timestamp}).` | `Spielstand geladen von {path} (gespeichert: {timestamp}).` |
| 180 | `save.loaded_legacy` | `Game loaded (legacy format).` | `Spielstand geladen (altes Format).` |
| 181 | `save.loaded_legacy2` | `Game loaded from legacy format.` | `Spielstand geladen aus altem Format.` |
| 182 | `save.none_found` | `No saved games found.` | `Keine Spielstände gefunden.` |
| 183 | `save.not_found` | `Error: Save file '{path}' not found.` | `Fehler: Speicherstand '{path}' nicht gefunden.` |
| 184 | `save.saved` | `Game saved to {path} ({timestamp}).` | `Spielstand gespeichert unter {path} ({timestamp}).` |
| 185 | `save.savezone_only` | `You can only rest at a savezone.` | `Speichern ist nur in einer Sicherheitszone möglich.` |
| 186 | `save.slot_delete_failed` | `Warning: could not delete save slot {path}.` | `Warnung: Speicherstand {path} konnte nicht gelöscht werden.` |
| 187 | `save.slot_deleted` | `Save slot deleted: {path}` | `Speicherstand gelöscht: {path}` |
| 188 | `save.unreadable` | `Error: Could not read file '{path}'.` | `Fehler: Datei '{path}' konnte nicht gelesen werden.` |
| 189 | `save.version_warning` | `Warning: This save was made with a different world version. Results may be unpredictable.` | `Warnung: Dieser Spielstand wurde mit einer anderen Weltversion erstellt. Die Ergebnisse können unvorhersehbar sein.` |
| 190 | `save.world_mismatch` | `world mismatch!` | `Welt abweichend!` |
| 191 | `score.no_score` | `This story does not use a score. Authors: declare a `score` variable (and optionally `score_rankings`) and award points…` | `Dieses Abenteuer verwendet kein Punktesystem. Autoren: Deklariere eine 'score'-Variable (und optional 'score_rankings')…` |
| 192 | `score.show` | `Score: {score}{if rank\|\nRank: {rank}}` | `Punktestand: {score}{if rank\|
Rang: {rank}}` |
| 193 | `search.nothing` | `You find nothing of interest.` | `Du findest nichts Interessantes.` |
| 194 | `search.nothing_item` | `You find nothing special about the {item}.` | `An {article_dat} {item} findest du nichts Besonderes.` |
| 195 | `search.nothing_npc` | `You find nothing on {npc}.` | `An {npc_article_dat} {npc} findest du nichts.` |
| 196 | `search.reveal` | `You find the {item}.` | `Du findest {article_acc} {item}.` |
| 197 | `search.void` | `You're in a void. There's nothing to search.` | `Du befindest dich im Nichts. Hier gibt es nichts zu durchsuchen.` |
| 198 | `stats.attack` | `Attack:  {atk} (base {base})` | `Angriff:  {atk} (Basis {base})` |
| 199 | `stats.conditions` | `Conditions: {conds}\n` | `Zustände: {conds}
` |
| 200 | `stats.defense` | `Defense: {def} (base {base})` | `Verteidigung: {def} (Basis {base})` |
| 201 | `stats.health` | `Health:  {hp} / {max}` | `Gesundheit:  {hp} / {max}` |
| 202 | `stats.progression` | `Level {level} — {name} ({xp}/{next} XP)` | `Stufe {level} — {name} ({xp}/{next} XP)` |
| 203 | `stats.progression_max` | `Level {level} — {name} ({xp} XP)` | `Stufe {level} — {name} ({xp} XP)` |
| 204 | `stats.skills` | `Skills: {skills}\n` | `Fähigkeiten: {skills}
` |
| 205 | `take.already` | `You already have the {item}.` | `Du hast {article_acc} {item} bereits.` |
| 206 | `take.none_here` | `There's nothing here to take.` | `Hier gibt es nichts zu nehmen.` |
| 207 | `take.not_portable` | `You can't take the {item}.` | `Du kannst {article_acc} {item} nicht mitnehmen.` |
| 208 | `take.ok` | `You take the {item}.` | `Du nimmst {article_acc} {item}.` |
| 209 | `target.not_carried` | `You don't have '{target}'.` | `Du hast '{target}' nicht.` |
| 210 | `target.not_seen` | `You don't see '{target}' here.` | `Du siehst '{target}' hier nicht.` |
| 211 | `ui.press_enter` | `  [Press Enter to continue]` | `  [Enter drücken, um fortzufahren]` |
| 212 | `undo.disabled` | `Undo is disabled in this adventure.` | `Rückgängig ist in diesem Abenteuer deaktiviert.` |
| 213 | `undo.done` | `Undone.` | `Rückgängig gemacht.` |
| 214 | `undo.ironman` | `No undo in ironman mode.` | `Kein Rückgängig im Ironman-Modus.` |
| 215 | `undo.nothing` | `Nothing to undo.` | `Nichts rückgängig zu machen.` |
| 216 | `undo.permadeath` | `No undo after death (permadeath).` | `Nach dem Tod kein Rückgängig (Permadeath).` |
| 217 | `unequip.all` | `You remove all equipment.` | `Du legst die gesamte Ausrüstung ab.` |
| 218 | `unequip.not_equipped` | `The {item} is not equipped.` | `Du hast {article_acc} {item} nicht ausgerüstet.` |
| 219 | `unequip.ok` | `You unequip the {item}.` | `Du legst {article_acc} {item} ab.` |
| 220 | `use.no_known_recipe` | `You don't know a recipe with {item1} and {item2}.` | `Du kennst kein Rezept mit {item1} und {item2}.` |
| 221 | `use.not_carried` | `You need to be carrying '{item}' to use it.` | `Dazu musst du '{item}' bei dir haben.` |
| 222 | `use.nothing` | `Nothing happens.` | `Es passiert nichts.` |
| 223 | `use.ok` | `You use the {item}. {msg}` | `Du benutzt {article_acc} {item}. {msg}` |
| 224 | `use.unreachable` | `You can't reach '{entity}' from here.` | `Du kommst von hier nicht an '{entity}' heran.` |
| 225 | `vehicle.board` | `You board the {vehicle}.` | `Du steigst in {vehicle}.` |
| 226 | `vehicle.cant_drive` | `You can't drive there from here. Stations: {stations}` | `Von hier aus kannst du nicht dorthin fahren. Stationen: {stations}` |
| 227 | `vehicle.cant_steer` | `You can't steer the {vehicle}; it follows its own route.` | `Du kannst {vehicle} nicht steuern; es folgt seiner eigenen Route.` |
| 228 | `vehicle.disembark` | `You disembark from the {vehicle}.` | `Du steigst aus {vehicle} aus.` |
| 229 | `vehicle.drive_to` | `You drive to {stop}.` | `Du fährst nach {stop}.` |
| 230 | `vehicle.fuelled` | `The {vehicle} is fuelled ({f}/{max}).` | `{vehicle} ist betankt ({f}/{max}).` |
| 231 | `vehicle.manual_only` | `This vehicle only moves when you drive it.` | `Dieses Fahrzeug bewegt sich nur, wenn du fährst.` |
| 232 | `vehicle.need_controls` | `You need to be at the controls to drive.` | `Du musst an den Kontrollen sein, um zu fahren.` |
| 233 | `vehicle.no_here_enter` | `There is no '{id}' here to enter.` | `Hier gibt es kein '{id}', in das du einsteigen könntest.` |
| 234 | `vehicle.no_id` | `There is no '{id}'.` | `Es gibt kein '{id}'.` |
| 235 | `vehicle.not_here` | `The {vehicle} is not here.` | `{vehicle} ist nicht hier.` |
| 236 | `vehicle.not_in` | `You are not in a vehicle.` | `Du befindest dich in keinem Fahrzeug.` |
| 237 | `vehicle.not_on` | `You are not on a vehicle.` | `Du befindest dich in keinem Fahrzeug.` |
| 238 | `vehicle.route_end` | `The route has no further stops.` | `Die Route hat keine weiteren Haltepunkte.` |
| 239 | `vehicle.travel_on` | `You travel on to {stop}.` | `Du fährst weiter nach {stop}.` |
| 240 | `vehicle.warning` | `Warning: {list}!` | `Warnung: {list}!` |
| 241 | `victory.title` | `  VICTORY!` | `  SIEG!` |
| 242 | `watch.nothing` | `There is nothing to watch about {label}.` | `Über {label} gibt es nichts zu beobachten.` |
| 243 | `watch.start` | `Watching {label}...` | `Du beobachtest {label}...` |
| 244 | `watch.void` | `You're in a void. There's nothing to watch.` | `Du befindest dich im Nichts. Hier gibt es nichts zu beobachten.` |
| 245 | `world.file_unreadable` | `Could not read world file '{path}': {err}` | `Weltdatei '{path}' konnte nicht gelesen werden: {err}` |
| 246 | `world.save_unreadable` | `Could not read save file '{path}': {err}` | `Speicherdatei '{path}' konnte nicht gelesen werden: {err}` |
| 247 | `xp.clamped` | `XP cannot drop below 0.` | `XP können nicht unter 0 fallen.` |

<!-- END GENERATED -->