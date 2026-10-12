# Spielideen für Beispiel-Adventures

Sammlung von Ideen für weitere Beispiel-Spiele in `examples/`, um die Engine zu
testen und vorzuführen. Bereits vorhanden (und daher hier ausgelassen): die
Genre-Fixtures in `examples/genres/` (Fantasy, Horror, Cyberpunk, Detektiv,
Space Opera, Pure IF, Visual Novel, Roguelike, Survival, Wirtschaft, Idle,
Deckbuilder, Nim, Pendel, Alchemie, Sandbox), die Modul-Fixtures in
`examples/modules/` sowie der Zork-I-Port.

**Status-Legende:** `[ ]` offen · `[~]` in Arbeit · `[x]` umgesetzt · `[-]` verworfen

---

## 1. Showcase-Kombinationen (mehrere Features in einem Spiel)

### [ ] Oregon Trail – „Der Treck nach Westen“
Ein Wagenzug zieht über Etappen nach Westen: Proviant kaufen, Flüsse
durchqueren, Krankheiten überstehen.
- **Zeigt:** Fahrzeug mit automatischer Route und Stopps, Survival und Wetter,
  Handel, Zufallsbegegnungen, Zustände (Ruhr …) mit Wirkung pro Runde
- **Warum:** Fast jeder kennt das Vorbild, und es zeigt viele Module in einem
  kompakten Spiel.

### [ ] FTL-Lite – „Sprung ins Ungewisse“
Mehrere Sektoren, Schiffsduelle, eine Crew, Permadeath.
- **Zeigt:** Raumschiff-Systeme und -Duelle, Party, neu erzeugte Welt pro
  Durchlauf, `meta.*`-Freischaltungen über mehrere Runs

### [ ] Heist – „Der Tresor von Monte Carlo“
Zuerst die Planung (Crew anwerben, Ausrüstung besorgen), dann der Einbruch.
- **Zeigt:** Kapitel/Phasen, Party, Stealth, Patrouillen, dynamische Ausgänge
  (Lüftungsschacht aufschrauben → `set_exit`)

### [ ] U-Boot – „Tiefe 400“
Ein beschädigtes U-Boot, der Sauerstoff wird knapp, Abteile laufen voll.
- **Zeigt:** Schiffssysteme als U-Boot, Sauerstoff-Zustand, dunkle Räume,
  `remove_exit`, wenn Schotten schließen

---

## 2. Neue Genres

### [ ] Zeitschleife – à la *Outer Wilds* / *Täglich grüßt das Murmeltier*
Jede Schleife dauert N Züge. Wissen bleibt über `meta.*` erhalten, die Welt
wird zurückgesetzt.
- **Zeigt:** Die Rogue-Plattform (Permadeath + Meta-Progression) als
  Erzählmittel, außerdem Runden-Trigger und Wissen
- **Warum:** Eine der originellsten Ideen für diese Engine. ⭐

### [ ] Whodunit mit zufälligem Täter
Beim Start wählt `RandomChoice` den Mörder, die Indizien passen sich an.
- **Zeigt:** Der deterministische Zufallsgenerator im Spielstand macht jeden
  Fall reproduzierbar und trotzdem wiederspielbar (Unterschied zum festen
  `detective.yaml`).

### [ ] Gerichtsdrama – à la *Ace Attorney*
Zeugen befragen und mit `use beweis on zeuge` Widersprüche aufdecken.
- **Zeigt:** Dialogbäume, Wissen, bedingte Antwortoptionen

### [ ] Grenzposten – à la *Papers, Please*
Jeden Tag kommen zufällige Reisende mit Dokumenten. Du gleichst die
Tagesregeln ab und stempelst.
- **Zeigt:** Zyklen, Zufallsauswahl, Prüfungen mit `compare_var`,
  Fraktions-Ansehen

### [ ] Bauernhof – Stardew-Lite
Tage und Jahreszeiten, Saat und Ernte, Markt, Zuneigung zu Dorfbewohnern.
- **Zeigt:** Zyklen, Mengen, Handel, Zuneigung

### [ ] Monster sammeln – Pokémon-Lite
Kreaturen per Custom-Verb `fangen` ins Team holen und taktisch kämpfen lassen.
- **Zeigt:** Party, Kampffähigkeiten, Begegnungstabellen

### [ ] Spielbuch-Port – à la *Fighting Fantasy*
Abschnitte als Räume, Würfelkämpfe und Glücksproben.
- **Zeigt:** Würfelpools, klassischer Kampf; eine Hommage wie der Zork-Port

---

## 3. Mechanik-Experimente (Fixture-Größe, TUI/ASCII-Tests)

### [ ] Mastermind / Safeknacker
Farben als eigene Verben (`setze rot`), Auswertung über Variablen.
- **Testet:** Wie weit man trotz fehlender parametrisierter Eingabe kommt

### [ ] Escape Room nur über ASCII-Hotspots
Das ganze Rätsel läuft über `look at <n>` und zustandsabhängige Bilder, also
ein Point-and-Click im Terminal.
- **Testet:** Hotspots, `CondText`

### [ ] Tamagotchi
Ein animiertes ASCII-Haustier mit Hunger- und Laune-Werten, die pro Runde
sinken.
- **Testet:** `frames`/`watch`, Runden-Trigger, Zustände

### [ ] Leuchtturmwärter
Atmosphärisch, mit animiertem Leuchtfeuer, Wetterzyklen und Schiffen, die auf
dich angewiesen sind (ausführlicher in Abschnitt 4).

---

## 4. ASCII-Showcases (die Grafik selbst als Spielmechanik)

Verfügbare Werkzeuge: zustandsabhängige Bilder (`CondText`), Bildwechsel pro
Zug (`frames`/`every`), Ambient-Loops mit eigener Bildrate (`ambient.fps`),
blockierende Zwischensequenzen (`clip`/`watch`), Hotspots, 24-Bit-Farbe,
Minimap mit `map:`-Positionen und mehreren Ebenen, `img2ascii`, `text2ascii`,
`video2ascii`, `title_art`/`end_art`.

### Die Grafik ist das Rätsel

#### [ ] „Das Observatorium“: Sternbilder finden
- Ein Teleskop-Raum zeigt einen Sternhimmel, der per `ambient` langsam funkelt.
- Mit Hotspots auf Sternen (`look at 3`) verbindest du Sternbilder.
- Jede Sichtung ändert über `CondText` das Bild, die gefundenen Linien
  erscheinen farbig.
- Tag und Nacht kommen über `frames`/`every`: Manche Sterne sind nur in
  bestimmten Zügen sichtbar.

#### [ ] „Der Fälscher“: Bilder vergleichen
- Zwei Gemälde, aus echten Bildern mit `img2ascii` erzeugt, mit kleinen
  Unterschieden.
- Die Unterschiede sind Hotspots; wer alle findet, entlarvt die Fälschung.
- **Zeigt:** `img2ascii`, 24-Bit-Farbe und Hotspots in einem Spiel

#### [ ] „Die Spieluhr“: ein mechanisches Rätsel
- Ein großes ASCII-Uhrwerk mit Zahnrädern als Hotspots.
- Jede Interaktion dreht Teile und ändert das Bild über `CondText`.
- Wenn alles stimmt, läuft eine Zwischensequenz: Das Werk setzt sich in
  Bewegung.
- Größerer Bruder von `clock_pendulum.yaml`.

### Atmosphäre und Bewegung

#### [ ] „Nachtzug nach Odessa“: Landschaft vor dem Fenster
- Ein fahrender Zug (Fahrzeug mit automatischer Route). Im Abteil zieht
  draußen per `ambient` die Landschaft vorbei: Wald, Brücke, Meer.
- Mit `video2ascii` aus echtem Zugfenster-Material erzeugt, wirkt das richtig
  filmisch.
- Ein Krimi im Abteil, und der Blick aus dem Fenster liefert Hinweise („Wann
  fuhren wir über die Brücke?“).

#### [ ] „Leuchtturmwärter“
- Der Lichtkegel läuft als Ambient-Loop, das Meer wechselt mit dem Wetter über
  `CondText`: ruhig, Sturm, Nebel.
- Schiffe tauchen als Hotspots am Horizont auf. Wer sie übersieht, sieht im
  Ende-Bild (`end_art`) ein Wrack.

#### [ ] Aquarium oder Terrarium
- Ein Ambient-Loop mit Fischen und Blasen, Fütterung über Hotspots.
- Die Fische verändern sich über Variablen (wachsen, Farbe wechselt).
- Eher ein ruhiger Bildschirmschoner zum Spielen, aber ein starker
  TUI-Showcase.

### Minimap und HUD als Spielfeld

#### [ ] „Minenfeld“ / Minesweeper
- Jede Feldzelle ist ein Raum mit Kartenposition (`map:`), die Minimap ist
  also das Spielfeld.
- Nebel des Krieges = unaufgedeckte Felder, `search` zeigt Nachbarminen über
  eine Variable.
- **Testet:** Wie weit sich Minimap und HUD als Spielbrett nutzen lassen

#### [ ] „Der Turm“: Aufstieg über mehrere Ebenen
- Ein mehrstöckiger Turm, Ebenen wechselst du mit `F2`.
- Jede Etage hat eigenen ASCII-Stil und eigene Farbpalette, vom Kerker in Grau
  bis zur Spitze mit Sonnenaufgang in Farbe.
- Titel-Banner aus `text2ascii`, das Ende-Bild ist ein Panorama.

### ⭐ Favorit: „Das Theater der Schatten“

Ein ganzes Spiel als **Puppentheater**. Jeder Raum ist eine Bühne, das
Art-Panel ist der Bühnenausschnitt.

| Element | Umsetzung |
|---|---|
| Vorhang auf/zu | Zwischensequenz (`clip`) beim Betreten eines Raums |
| Kulissen | wechseln über `CondText`, je nachdem, was der Spieler verändert hat |
| Puppen | NPCs als Hotspots auf der Bühne (`look at 2`, `talk to 2`) |
| Akte / Szenen | Ambient-Loops im Hintergrund (flackernde Kerze, fallender Schnee) |
| Titel und Ende | Theaterplakate aus `text2ascii` (`title_art` / `end_art`) |

Damit wird praktisch **jedes** ASCII-Feature in einem stimmigen Rahmen
gezeigt, und die Einschränkung „alles ist Text“ wird zum Stilmittel.

---

## Empfohlene Reihenfolge

1. **Das Theater der Schatten**: bester ASCII-/TUI-Showcase
2. **Zeitschleife**: zeigt die Rogue-Plattform als Erzählmittel
3. **Oregon Trail**: bekanntes Vorbild, viele Module in einem Spiel
4. Mechanik-Experimente als kleine Fixtures nebenbei (Tamagotchi, Escape Room,
   Mastermind, Minesweeper)
