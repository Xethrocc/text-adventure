# Plan: Survival Wilderness Mystery / Expeditions-Tagebuch

**Titel des Abenteuers:** *Verschollen im ewigen Eis* (`examples/genres/survival_expedition.yaml`)  
**Genre:** Survival Wilderness Mystery / Arktische Expeditions-Simulation  
**Sprache:** Deutsch (`language: de`) mit vollständigen grammatikalischen Attributen (`gender:`, `article:`)  
**Engine-Highlights:**
- `OPEN-17` generische Item-Timer (`fuel:`):
  - `sturmlaterne` mit Petroleum-Verbrauch (`ticks_when: lit`, Meilenstein-Warnung bei Restwärme/Licht, Erlöschen bei 0, Nachfüllen via `add_fuel` / `set_fuel`).
  - `lagerfeuer` in der Schutzhütte / Basislager (brennt ab, kann mit Treibholz nachgelegt werden).
- **Kälte- & Unterkühlungsmechanik (`waerme`):**
  - Körperwärme (`waerme`, Start: 20, Max: 20, Min: 0).
  - In eisigen Außenbereichen (Schneesturm / Eisspalten) sinkt die Wärme pro Zug um 1 (im tosenden Schneesturm um 2).
  - Bei `waerme == 0` stirbt der Expeditionsleiter an Erfrierung / Hypothermie (`game_end: death`).
  - In geheizten Schutzräumen (Zelt, Basishütte, beheizt durch Kamin oder Sturmlaterne) regeneriert Wärme.
- **Tagebuch- & Dokumenten-Archäologie:**
  - Aufdecken der tragischen Vorgeschichte von Dr. Archibald Thorne (Expedition *HMS Resolute*).
  - 4 zerrissene Tagebuchseiten (`tagebuch_seite_1` bis `tagebuch_seite_4`) mit `read`-Reaktionen und Lore-Enthüllungen.
  - Das vollständige Logbuch enthüllt die Koordinaten des verschütteten Eis-Mausoleums und den Code des Notfall-Senders.
- **Survival-Interaktionen & Werkzeugkette:**
  - `eispickel` zum Aufschlagen von Gletschereis und Überqueren der Steilwand.
  - `kletterseil` zur Sicherung über der Eisschlucht.
  - `petroleumkanister` zum Auftanken der Sturmlaterne (`add_fuel: {item: sturmlaterne, delta: 10}`).
  - `notstromaggregat` mit Handkurbel und Zündkerze zum Einschalten der Funkstation.
  - `signalpistole` zum Abfeuern des Rettungssignals auf dem Gipfelplateau bei laufendem Funkleitstrahl.

---

## 1. Szenario & Handlung

### Ausgangslage
Wir schreiben das Jahr 1912. Du bist Kapitän Robert Vance, Anführer einer Rettungsexpedition in die arktische Eiswüste von King William Island. Vor zwei Jahren verschwand die wissenschaftliche Arktisexpedition von Dr. Archibald Thorne spurlos auf der Suche nach einem prähistorischen Meteoriten ("Der Himmelsspalt"), der tief im Permafrost begraben liegt.

Ein plötzlicher arktischer Schneesturm hat dein Expeditionsschiff von der Packeiskante abgetrieben. Du bist allein im verlassenen Außenposten "Camp Aurora" gestrandet, nur ausgerüstet mit deiner Sturmlaterne, Notproviant und deinem festen Willen zu überleben.

### Ziel
1. **Überleben:** Schütze dich vor der tödlichen Kälte (Windchill) und halte die Sturmlaterne am Brennen.
2. **Rekonstruktion:** Finde die 4 verstreuten Logbuchseiten von Dr. Thorne, um das Schicksal der *HMS Resolute* und das Geheimnis des Himmelsspalts zu lüften.
3. **Funkstation & Rettung:** Repariere das Notstromaggregat in der wetterfesten Forschungsstation, aktiviere die arktische Funkboje und feuere auf dem Grat die rote Signalrakete ab, um das Rettungsflugschiff herbeizurufen.

---

## 2. Spielwelt & Raumtopologie

Die Spielwelt umfasst 8 stimmungsvolle Schauplätze:

```
                  [gipfel_plateau] (Signalfeuer / Gipfel)
                         |
                   [steilwand] (Eisklettern mit Eispickel)
                         |
[gletscherhoehle] --- [eisspalte] (Kletterseil über den Abgrund)
                         |
                  [schneefeld]
                   /        \
   [forschungsstation]     [camp_aurora] (Start)
            |                    |
     [funkzentrale]        [proviantlager]
```

1. **`camp_aurora`** (Startraum): Verlassenes Zeltdorf am Rand der Eisbank. Windgeschützt, aber kalt. Beinhaltet ein erloschenes Lagerfeuer, Kisten und eine Petroleumlampe.
2. **`proviantlager`**: Eingestürzter Unterstand hinter dem Camp. Beinhaltet eine Kiste mit Trockenfleisch, Kletterseil und die 1. Logbuchseite.
3. **`schneefeld`**: Weite, vom Blizzard zerfurchte Eiswüste. Extreme Kälte (-2 Wärme/Zug). Weggabelung nach Westen zur Station und nach Norden zur Gletscherspalte.
4. **`forschungsstation`**: Massives Holzblockhaus von Thornes Expedition. Trocken und windgeschützt. Enthält ein defektes Dieselaggregat, Werkzeugkasten und die 2. Logbuchseite.
5. **`funkzentrale`**: Rückraum der Forschungsstation mit Morse-Funkgeräten und Antennenkabeln. Benötigt Strom vom Aggregat. Enthält die 3. Logbuchseite und eine Signalpistole mit roter Leuchtkugel.
6. **`eisspalte`**: Schwindelerregender Abgrund im bläulichen Packeis. Der Übergang ist eingestürzt. Erfordert das Spannen des Kletterseils, um nach Norden zur Steilwand oder nach Westen in die Höhle zu gelangen.
7. **`gletscherhoehle`**: Schutzhöhle im Eis. Tropfende Eiszapfen, skelettierte Überreste von Thornes Zeltlager. Enthält den Petroleumkanister und die finale 4. Logbuchseite.
8. **`gipfel_plateau`**: Höchster Punkt des Eisrückens. Peitschende Sturmböen. Hier befindet sich der Antennenmast. Wenn der Sender läuft und die Signalpistole abgefeuert wird, landet das Rettungsflugzeug -> **Sieg!**

---

## 3. Variablen, Ressourcen & Mechaniken

### Kälte & Körperwärme (`waerme`)
- Variable `waerme`: Start 15, Max 20, Min 0.
- Im Freien (`schneefeld`, `eisspalte`, `steilwand`, `gipfel_plateau`):
  - Kälte-Drain pro Zug: -1 Wärme (bei brennender Laterne) bzw. -2 Wärme (wenn Laterne unlit).
- In Unterständen (`camp_aurora`, `forschungsstation`, `funkzentrale`, `gletscherhoehle`):
  - Wärme bleibt stabil oder regeneriert (+2 Wärme pro Zug, wenn Lagerfeuer brennt oder Station geheizt ist).
- Notfall: Essen von Trockenfleisch (`eat fleisch`) spendet sofort +5 Wärme und sättigt.
- Erfrierung: Wenn `waerme <= 0`, bricht der Spieler zusammen: `game_end: death` ("Du sinkst in den gefrorenen Schnee. Die Kälte betäubt deine Sinne...").

### Sturmlaterne & Brennstoff (`fuel:`)
- Item `sturmlaterne`:
  - `state: unlit`
  - `fuel:`
    - `start: 12`
    - `ticks_when: lit`
    - `warn:`
      - `at: 4` -> Nachricht: *"Das Glas der Sturmlaterne trübt sich, die Flamme flackert schwach. Das Petroleum neigt sich dem Ende zu!"*
    - `on_empty:`
      - Nachricht: *"Mit einem letzten Zischen erlischt die Flamme der Sturmlaterne. Eisige Dunkelheit umfängt dich!"*
      - `set_state: { target: sturmlaterne, to: unlit }`
- Zünden/Löschen:
  - `light sturmlaterne` (Alias: `anzuenden`, `entzuenden`) mit Zündhölzern (`streichhoelzer`).
  - `extinguish sturmlaterne` (Alias: `loeschen`, `ausmachen`) stoppt den Fuel-Verbrauch!
- Nachfüllen:
  - `use petroleum on sturmlaterne` oder `fill sturmlaterne`:
    - Ruft `add_fuel: { item: sturmlaterne, delta: 12 }` auf!

### Das Lagerfeuer (`lagerfeuer`)
- Befindet sich in `camp_aurora`.
- Kann mit Treibholz (`feuerholz`) und Streichhölzern entzündet werden.
- Besitzt ebenfalls `fuel: { start: 10, ticks_when: burning, on_empty: [...] }`.
- Schenkt solange es brennt Behaglichkeit und wärmt den Spieler auf 20 Wärme auf.

### Das Expeditions-Tagebuch & Lore
- 4 Fundstücke von Dr. Thornes Tagebuch:
  1. *Seite 1 (Camp Aurora / Proviantlager):* Berichtet von der Ankunft und dem Fund seltsamer Meteorsplitter im Gletschereis.
  2. *Seite 2 (Forschungsstation):* Berichtet vom Ausfall des Funkers und dem Bau der Notantenne auf dem Gipfelplateau.
  3. *Seite 3 (Funkzentrale):* Schildert die Verzweiflung und gibt die Frequenz für den Notsender an ("Notrufkanal 121.5 MHz").
  4. *Seite 4 (Gletscherhöhle):* Die tragischen letzten Worte von Dr. Thorne – Warnung vor der Kälte und Bestätigung, dass die Signalrakete vom Gipfel aus gesehen werden kann.
- Jeder Fund verleiht +25 Expeditionspunkte (Score).

---

## 4. Gegenstände & Grammatik

Alle Items werden mit korrekter deutscher Grammatik spezifiziert:
- `sturmlaterne` (f, `die/die/der`), `fuel:`, tragbar, `lightsource` im Zustand `lit` via `tags_when: { lit: [lightsource] }`.
- `streichhoelzer` (m-pl, `die/die/den Streichhölzern`), tragbar.
- `eispickel` (m, `der/den/dem`), tragbar, schlägt Eis frei und ermöglicht Klettern an der Steilwand.
- `kletterseil` (n, `das/das/dem`), tragbar, überbrückt die Gletscherspalte.
- `petroleumkanister` (m, `der/den/dem`), tragbar, enthält Brennstoff.
- `trockenfleisch` (n, `das/das/dem`), tragbar, essbar via `eat`.
- `feuerholz` (n, `das/das/dem`), tragbar, nährt das Lagerfeuer.
- `kurbel` (f, `die/die/der`), tragbar, wirft das Notstromaggregat an.
- `signalpistole` (f, `die/die/der`), tragbar, feuert die Rettungsrakete ab.
- `tagebuch_seite_1..4` (f, `die/die/der`), lesbar via `read`/`examine`.
- `aggregat` (n, `das/das/dem`), stationär in Forschungsstation.

---

## 5. Walkthrough & Testpfade

### A. Happy-Path (Sieg / Evakuierung)
1. `take streichhoelzer`
2. `take sturmlaterne`
3. `light sturmlaterne` (Laterne brennt, Wärme geschützt)
4. `south` (ins Proviantlager)
5. `take fleisch`
6. `take seil`
7. `take seite1`
8. `read seite1`
9. `north`
10. `north` (auf das Schneefeld)
11. `west` (zur Forschungsstation)
12. `take kurbel`
13. `take seite2`
14. `read seite2`
15. `use kurbel on aggregat` (Aggregat springt an, Strom fließt!)
16. `north` (in die Funkzentrale)
17. `take signalpistole`
18. `take seite3`
19. `read seite3`
20. `use funkgeraet` (SOS-Signal auf 121.5 MHz wird abgestrahlt!)
21. `south`
22. `east` (zurück aufs Schneefeld)
23. `north` (an die Eisspalte)
24. `use seil` (Seilbrücke über die Spalte errichtet!)
25. `west` (in die Gletscherhöhle)
26. `take eispickel`
27. `take petroleum`
28. `take seite4`
29. `read seite4`
30. `use petroleum on sturmlaterne` (Laterne nachgefüllt via `add_fuel`!)
31. `east`
32. `north` (an die Steilwand)
33. `use eispickel` (Klettert die Steilwand hinauf!)
34. `north` (auf das Gipfelplateau)
35. `fire signalpistole` (Rote Leuchtkugel schießt in den Polarhimmel! Rettungsflieger sichtet das Signal -> SIEG!)

### B. Fail-Path (Kältetod / Hypothermie)
1. Spieler startet ohne Laterne oder lässt sie ausgehen.
2. Spieler rennt ungeschützt wiederholt durch das Schneefeld und verbraucht alle Wärme-Punkte.
3. `waerme` fällt auf 0.
4. Tödlicher Zusammenbruch: `game_end: death`.

---

## 6. Validierung & CI-Schritte
1. Datei `examples/genres/survival_expedition.yaml` anlegen.
2. `worldbuilder validate` -> 0 Errors, 0 Warnings.
3. `worldbuilder test` -> Alle integrierten Story-Tests grün.
4. `worldbuilder fuzz` -> 10 Runs x 120 Steps, 0 Findings.
5. CI Fixtures:
   - `ci/e2e/survival_expedition.in` & `.expect`
   - `ci/e2e/survival_expedition-frost-fail.in` & `.expect`
6. `scripts/ci.sh` erweitern (Stages 4, 4b, 5).
7. Gesamten CI-Lauf `./scripts/ci.sh` ausführen (alle 10 Stages grün).
8. Git-Commit & Push auf `origin/master`.
