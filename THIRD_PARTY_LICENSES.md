# Third-Party Licenses

Die Engine steht unter der **MIT-Lizenz** (siehe [LICENSE](LICENSE)). Diese
Datei dokumentiert, welche Fremdwerke als Quelle, Vorlage oder Referenz für
Beispiele und Ports (`examples/`) dienen und unter welchen Bedingungen.

> Stand der Prüfung: 2026-10-10, per Websuche. Vor jeder tatsächlichen
> Übernahme von Code, Text oder Daten den Lizenztext im Original-Repo prüfen
> und die Spalte „Geprüft am" aktualisieren.

## Aufnahmeregeln

| Lizenz | Ins Repo übernehmen? | Auflage |
|---|---|---|
| Public Domain / gemeinfrei | Ja | Quelle nennen. Bei Project-Gutenberg-Texten Gutenberg-Lizenzhinweise entfernen oder beachten. |
| MIT / BSD | Ja | Copyright-Hinweis und Lizenztext der Quelle erhalten (Eintrag unten + Header in der Datei). |
| MPL-2.0 | Nur nach bewusster Entscheidung | Übernommene/geänderte Dateien bleiben unter MPL-2.0. Bevorzugt nur als Inspiration nutzen. |
| GPL / NGPL / AGPL | **Nein** | Kein Code, Text oder Daten. Nur Ideen und Mechaniken. |
| Freeware / closed source / ohne Lizenz | **Nein** | Kein Code, Text oder Assets. Nur Ideen und Mechaniken. |

Ideen, Regeln und Spielmechaniken sind nicht urheberrechtlich geschützt;
konkreter Code, Text, Rätselformulierungen und Assets schon. Namen und Marken
(z. B. „Zork", „Infocom", „Rogue") sind unabhängig von der Code-Lizenz
geschützt — für Beispiele im Repo neutrale Namen bevorzugen.

## Als Quelle im Repo erlaubt

| Werk | Lizenz | Verwendung | Quelle | Geprüft am |
|---|---|---|---|---|
| Zork I–III | MIT (seit Nov. 2025, Microsoft/Activision) | Nur Code. Handbücher, Cover und Assets sind ausgenommen, übrige Infocom-Titel bleiben proprietär. Verwendung von Story-Text/Rätseln bewusst entscheiden. | GitHub `historicalsource` | 2026-10-10 |
| Open Adventure (Colossal Cave, 430 Punkte) | BSD-2-Clause | Code/Datenstruktur nutzbar, Lizenztext beilegen. Das Original von 1976 ist rechtlich nur „quasi PD" — Open-Adventure-Fassung verwenden. | gitlab.com/esr/open-adventure | 2026-10-10 |
| Rogue 5.4 | BSD (Toy, Wichman, Arnold) | Lizenztext beilegen. | Original-Quellcode-Release | 2026-10-10 |
| Gemeinfreie Literatur (Verne, Poe, Doyle u. a.) | Public Domain | Story-Basis für eigene Adaptionen. | Project Gutenberg | 2026-10-10 |
| Traditionelle Brettspiele (Backgammon, Dame, Schach, Mühle, Nim, Mancala) | Regeln gemeinfrei | Eigene Implementierung. BASIC-Listings (z. B. „101 BASIC Computer Games") **nicht** übernehmen, Lizenz unklar. | – | 2026-10-10 |

## Nur Inspiration (nichts übernehmen)

| Werk | Lizenz / Status | Grund |
|---|---|---|
| A Dark Room | MPL-2.0 | Dateiweises Copyleft; Name/Marken nicht mitlizenziert. |
| Umoria (Moria) | GPL (seit 2008) | Copyleft. |
| NetHack | NGPL | Copyleft. |
| dnd (Walter Bright) | keine freie Lizenz | Urheberrechtlich geschützt. |
| Cragne Manor | Freeware | 80+ Autoren, Einzelgenehmigungen. |
| Cloak of Darkness | Freeware | Port-Spezifikation, keine Lizenz für fremde Implementierungen. |
| Dwarf Fortress, Caves of Qud | closed source | – |
| Trinity, A Mind Forever Voyaging | proprietär | – |
| Cookie Clicker, Adventure Capitalist, Egg Factory, Slay the Spire, MTG, UNO | proprietär | – |
| Everlasting Summer, Choice of Games, Doki Doki Literature Club | proprietär | – |

## Nicht belegt

- `1879` (angeblich D. A. Wheeler) und `Hamurabi`: Eine Public-Domain-Angabe
  konnte nicht bestätigt werden. Nicht verwenden, bis die Lizenz geklärt ist.

## Einträge für übernommene Werke

Pro tatsächlich übernommenem Fremdwerk hier einen Eintrag anlegen:

```
### <Werk>
- Datei(en): examples/...
- Quelle: <URL, Commit/Version>
- Lizenz: <SPDX-ID>
- Copyright: <Copyright-Zeile aus der Quelle>
- Geprüft am: <Datum>
- Änderungen: <kurz>
```

(Noch keine endgültigen Einträge.)

### Zork I (vorläufig)
- Datei(en): `examples/zork1.yaml` (Lizenztext: `examples/zork1.LICENSE`)
- Status: **vorläufig, nur zum Testen** — 1:1-Port auf die Engine. Bei
  Lizenzproblemen wird die Datei aus dem Repo entfernt.
- Quelle: https://github.com/historicalsource/zork1 @
  `97b7b3d68c075dd9af7da499c3e9690ada3471fd` (2025-11-21)
- Lizenz: MIT
- Copyright: Copyright (c) 2025 Microsoft
- Geprüft am: 2026-10-10 (LICENSE-Datei im lokalen Klon gelesen)
- Herkunftsprüfung (2026-10-10): Stichprobe von 171 längeren Textzeilen aus
  `zork1.yaml` gegen die lokalen Klone abgeglichen: 149 stammen nachweislich
  aus `zork1/*.zil`. Die 22 übrigen sind Zeilenumbruch-Fragmente von
  Raumbeschreibungen (kein Treffer wegen Umbruch), keine davon tritt
  ausschließlich in `zork-mdl`, `MITDDC/zork`, `zork2` oder `zork3` auf. Die
  Quellverweise im YAML nennen durchgehend `1actions.zil`, `1dungeon.zil` und
  `gverbs.zil` aus `zork1`. Kein Hinweis auf Fremdmaterial außerhalb der
  MIT-lizenzierten `zork1`-Quellen.
- Offen: Rolle von Name/Marke „Zork" und von Story-Texten klären. Der Port gibt
  die Texte 1:1 wieder, die Freigabe betrifft laut Berichten den Quellcode.
- Änderungen: Umsetzung der ZIL-Logik in das Engine-YAML-Format
