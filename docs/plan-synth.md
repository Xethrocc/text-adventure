# Plan: Software-Synth für Autoren (Phase 5.2)

Stand: 2026-10-17, Code-Stand `8895aed` (Planbasis `cfbf380`). **Status: Design abgenommen (5.2a `cfbf380`-Basis, 5.2b `456a743`, 5.2c `f6f53a2`, 5.2d `b3cf83a`, 5.2e `8895aed` erledigt), nächste Stufe 5.2f (hängt an Phase 3).** Ersetzt die Zeile 5.2 („Synth-Tab") in
`implementierungsplan_engine_review_2026-09.md` und beantwortet dort den offenen Punkt
„5.2: Datenmodell und Render-Ort des Synths".

**Ziel:** Autoren beschreiben Soundeffekte (und in Stufe 2 kurze Musik-Loops) als **Daten
im Adventure** statt als WAV/OGG-Dateien. Die Engine erzeugt daraus deterministisch
Klang, der auf CLI, TUI, Export und Browser identisch ist.

---

## 1. Ausgangslage im Code

Die Audio-Pipeline existiert vollständig, aber nur für Dateien:

| Schicht | Ort | Typ / Verhalten |
|---|---|---|
| Effekte (purer Kern) | `src/Types/Core.hs:1165` | `PlaySfx FilePath \| PlayMusic FilePath \| StopMusic` |
| Anwendung | `src/Effects.hs:465` | hängt an `pendingSfx` an / setzt `pendingMusic` |
| Queue | `GameState.pendingSfx :: [FilePath]`, `pendingMusic :: Maybe MusicCommand` (`Types/Core.hs:3000`) | **runtime-only**, nicht in `save.json` |
| Protokoll-Events | `src/GameLoop.hs:360` → `EvSfx`/`EvMusicStart`/`EvMusicStop` (`src/Types/Output.hs:307`) | `{"type":"sfx","path":…}` (`docs/protocol-v1.md:202`) |
| Dispatch (IO-Rand) | `src/GameLoop.hs` `dispatchAudioAndCutscene` | ruft `fePlaySfx`/`feStartMusic`/`feStopMusic` |
| Frontend-Seam | `src/Frontend.hs:50` | `FilePath -> IO ()` |
| Ausgabe | `src/Audio.hs` | externe Helper (`mpv`, `ffplay`, `openmpt123`, …), Discovery CLI → Env → `bin/` → PATH |
| Verkabelung | `text-adventure-cli/app/Main.hs:206/244` | nur CLI; **TUI ist No-op** (`Tui.hs:213`), WASM-Stub `unsupported` |
| Autorenform | `worldbuilder/src/Worldbuilder/Types.hs:1742/1966` | `sfx:`/`music:`/`stop_music:` |
| Compile | `worldbuilder/src/Worldbuilder/Compile.hs:3822` | `AOPlaySfx` → `E.PlaySfx` |
| Export (B6) | `worldbuilder/src/Worldbuilder/Export.hs:51` | sammelt Asset-Pfade fürs Bundle |

Architektur-Randbedingungen, die der Synth einhalten muss:

- **A1:** Das Engine-Binary linkt keine Audio-Bibliothek; Ausgabe über externe Helper.
- **D14:** Kunst und Clips sind zur Compile-Zeit eingebettet — Laufzeit braucht keine Dateien.
- **Byte-Vertrag:** Abenteuer ohne neue Felder kompilieren zu byte-identischem `world.json`
  (neue Maps werden bei Leere weggelassen, vgl. `procDefs`, `npcInteractions`).
- **WASM (5.1):** Der Kern bleibt frei von `process`/`directory`.
- **Fuzzer (B5):** Jeder Schritt muss in begrenzter Zeit zurückkehren.

---

## 2. Entscheidungen

| Nr. | Frage | Entscheidung | Begründung |
|---|---|---|---|
| S1 | Sounds als Daten oder Dateien? | **Daten im Adventure**, eingebettet in `world.json` (D14-Muster) | Kein Asset-Management, kleine Bundles, versionierbar, WebUI-Vorschau ohne Dateizugriff |
| S2 | Wo wird gerendert? | **Ein purer Renderer im Kern** (`src/Synth/`), Ausgabeformat PCM → WAV | Ein Klang überall, golden-testbar, WASM-fähig, A1 bleibt unberührt |
| S3 | Wo entsteht die Datei? | **Am IO-Rand** (`dispatchAudioAndCutscene`), lazy, gecacht per Patch-Hash | Kern bleibt pur; Frontends sehen weiter nur `FilePath` |
| S4 | Frontend-Seam ändern? | **Nein.** `fePlaySfx :: FilePath -> IO ()` bleibt | CLI, TUI, WASM-Stub und `Audio.hs` brauchen keine Änderung |
| S5 | Effektform | **Objektform** `sfx: {synth: id}` / `music: {synth: id}`; Stringform bleibt Datei | Kein String-Dualismus („ist das ein Pfad oder eine ID?"); Tippfehler = harter Fehler |
| S6 | Neue Effekt-Konstruktoren | `PlaySfxSynth String`, `PlayMusicSynth String`; alte unangetastet | Muster `RandomChoiceOn`: alte Welten byte-identisch |
| S7 | Grenzen | Fest im Compiler: SFX ≤ 10 s, Track ≤ 120 s, 22050 Hz mono 16 bit | Begrenzte Render-Zeit (B5), begrenzte Temp-Dateien (≈ 5 MB/Track) |

### Verworfene Alternativen

- **MIDI + `fluidsynth`/`timidity`** (stehen schon in der PATH-Suche, `Audio.hs:83`):
  Klang hängt vom Soundfont des Rechners ab → nicht deterministisch, keine Browser-Parität,
  keine Golden-Tests.
- **Baking nur zur Export-Zeit** (Worldbuilder schreibt WAVs ins Bundle): Der Kern hätte
  keinen Synth, die WebUI-Vorschau bräuchte eine zweite Implementierung, `world.json` müsste
  generierte Pfade tragen. Widerspricht D14.
- **Rendern nur im Browser (Web Audio):** CLI/TUI blieben stumm; zwei Klangquellen, sobald
  doch ein Haskell-Renderer dazukommt.
- **Audio-Bibliothek linken (SDL_mixer, OpenAL, portaudio):** bricht A1 und das
  Windows-Packaging, blockiert WASM.

---

## 3. Datenmodell

### 3.1 SFX-Patch (Stufe 1)

```yaml
sounds:
  alarm:
    wave: square            # sine | square | saw | triangle | noise
    freq: 880               # Hz, Startfrequenz (20–8000)
    sweep: -400             # Hz/s, linearer Pitch-Sweep (optional, Default 0)
    duty: 0.5               # nur square: Pulsbreite 0.05–0.95 (optional)
    dur: 0.35               # Sekunden, 0.01–10
    attack: 0.005           # ADSR in Sekunden (optional, Default 0.005/0/1.0/0.05)
    decay: 0.05
    sustain: 0.7            # Pegel 0–1
    release: 0.2
    volume: 0.8             # 0–1 (optional, Default 0.8)
  tuer:
    layers:                 # mehrere Stimmen gemischt (max. 4)
      - { wave: noise, dur: 0.15, release: 0.1, volume: 0.5 }
      - { wave: sine,  freq: 110, sweep: -200, dur: 0.3 }
```

Kurzform: ein Patch ist entweder ein einzelnes Stimmen-Objekt oder `{layers: [...]}`.

### 3.2 Track (Stufe 2)

```yaml
tracks:
  nebel:
    bpm: 90
    steps: 16                   # Steps pro Pattern (Sechzehntel)
    loop: true
    instruments:                # Instrumente = Patches ohne freq/dur
      lead: { wave: triangle, release: 0.15 }
      bass: { wave: square, duty: 0.25, release: 0.05 }
    channels:                   # max. 4 Kanäle
      - instrument: lead
        pattern: "A4 . C5 . E5 . . . G4 . . . E4 . . ."
      - instrument: bass
        pattern: "A2 . . . A2 . . . F2 . . . G2 . . ."
    order: [0, 0, 1, 0]         # optional: Pattern-Reihenfolge, wenn channels mehrere Patterns tragen
```

Notation: Notenname + Oktave (`C4`, `F#3`, `Bb2`), `.` = Pause/Halten, `-` = Note-off.
Die Raster-Syntax ist entschieden (O3).

**Festlegung Mehrfach-Patterns & `order:` (5.2d):**
- Ein Kanal deklariert entweder `pattern: "<steps>"` (Einzel-Pattern, äquivalent zu `patterns: ["<steps>"]`) **oder** `patterns: ["<pat0>", "<pat1>", ...]` (Liste von Pattern-Strings, jeweils mit exakt `steps:` Sechzehntel-Tokens; gleichzeitige Angabe von `pattern:` und `patterns:` ist ein `SynthBadPatch`-Fehler).
- `order: [Int]` auf Track-Ebene ist optional und enthält 0-basierte Indizes in die `patterns:`-Liste jedes Kanals (z. B. `order: [0, 0, 1, 0]`).
- Fehlt `order:` (oder ist leer), ist die effektive Reihenfolge `[0 .. k - 1]`, wobei `k` die Anzahl der Patterns des ersten Kanals ist (bei Einzel-Pattern `pattern:` also `[0]`).
- Jeder Index `i` in der effektiven Reihenfolge muss für jeden Kanal gültig sein (`0 <= i < length patterns`), andernfalls meldet der Compiler `SynthParamRange`.
- Die Gesamtdauer des Tracks beträgt `length effectiveOrder * steps * (15.0 / bpm)` Sekunden und darf **120 s** (S7) nicht überschreiten (`SynthTooLong`).
- In `ToJSON Channel` wird ein einzelnes Pattern kompakt als `"pattern": "..."` serialisiert, mehrere Patterns als `"patterns": [...]`; `trOrder` wird in `ToJSON Track` nur geschrieben, wenn `not (null trOrder)`.

### 3.3 Effekte

```yaml
effects:
  - sfx: audio/klick.wav        # unverändert: Datei
  - sfx: {synth: alarm}         # neu
  - music: {synth: nebel}       # neu (Stufe 2)
  - stop_music: true            # unverändert, stoppt beides
```

### 3.4 Kern-Typen (Skizze)

```haskell
-- src/Synth/Patch.hs
data Wave  = Sine | Square | Saw | Triangle | Noise
data Voice = Voice { vWave :: Wave, vFreq :: Double, vSweep :: Double, vDuty :: Double
                   , vDur :: Double, vEnv :: Adsr, vVolume :: Double }
newtype SynthSound = SynthSound [Voice]          -- 1–4 Layer
data Channel = Channel { chInstrument :: String, chPatterns :: [String] }
data Track = Track { trBpm :: Int, trSteps :: Int, trLoop :: Bool
                   , trInstruments :: Map String Voice, trChannels :: [Channel]
                   , trOrder :: [Int] }

-- src/Types/Core.hs
data SoundRef = SoundFile FilePath | SoundSynth String   -- runtime-only Queue-Element
-- GameWorld:
, worldSounds :: Map.Map String SynthSound   -- leer = weggelassen
, worldTracks :: Map.Map String Track        -- leer = weggelassen (Stufe 2)
-- Effect:
| PlaySfxSynth String
| PlayMusicSynth String
-- MusicCommand:
data MusicCommand = MusicStart FilePath | MusicStartSynth String | MusicStop
```

`pendingSfx` wird `[SoundRef]`. Weil das Feld runtime-only ist, berührt das weder
`save.json` noch den Byte-Vertrag.

**Noise-Determinismus:** Der Rauschgenerator ist ein fester LCG/xorshift mit Seed aus dem
Sound-ID-Hash — nie aus `rngState`. Ein SFX darf die Spiel-Zufallsströme nicht verschieben
(vgl. B8).

---

## 4. Architektur

```
YAML sounds:/tracks:  ──Worldbuilder──▶  world.json (worldSounds/worldTracks)
                                                │
Effekt sfx:{synth}  ──Effects (pur)──▶  pendingSfx = [SoundSynth "alarm"]
                                                │
                       ┌────────────────────────┴───────────────────────┐
                       ▼                                                ▼
     GameLoop:360 Protokoll-Events                     dispatchAudioAndCutscene (IO)
     EvSfxSynth "alarm" (+ optional WAV)               Synth.Cache.resolve → /tmp/…/<hash>.wav
                       │                                                │
                       ▼                                                ▼
              WebUI / Web Audio                        fePlaySfx path  (unverändert)
                                                                        │
                                                                        ▼
                                                       Audio.playSfx → mpv/ffplay
```

### 4.1 Neue Module

| Modul | Inhalt | IO? |
|---|---|---|
| `src/Synth/Patch.hs` | Typen, JSON-Instanzen, Defaults, Limits als Konstanten | nein |
| `src/Synth/Render.hs` | `renderSound :: SynthSound -> [Int16]`, `renderTrack :: Track -> [Int16]`; Oszillatoren, ADSR, Sweep, Mixer mit Clipping | nein |
| `src/Synth/Wav.hs` | `encodeWav :: Int -> [Int16] -> BL.ByteString` (RIFF/PCM, 44-Byte-Header) | nein |
| `src/Synth/Cache.hs` | `resolveSound :: GameWorld -> SoundRef -> IO (Maybe FilePath)` — rendert bei Bedarf in `$XDG_CACHE_HOME/text-adventure/synth/<hash>.wav` (Fallback `getTemporaryDirectory`), Dateiname = Hash über Patch-JSON + Renderer-Version | ja |

`Synth.Cache` gehört **nicht** in die WASM-Kernbibliothek; die WASM-Seite nutzt nur
`Render` + `Wav`.

### 4.2 Änderungen an bestehendem Code

| Datei | Änderung |
|---|---|
| `src/Types/Core.hs` | `SoundRef`, `worldSounds`/`worldTracks` (mit Weglass-Regel in `ToJSON GameWorld`), Effekt-Konstruktoren, `MusicStartSynth` |
| `src/Effects.hs:465` | zwei neue Fälle; `PlaySfx path` queued `SoundFile path` |
| `src/GameLoop.hs` `dispatchAudioAndCutscene` | `SoundRef` → `resolveSound` → `fePlaySfx`/`feStartMusic`; Fehler (Render/Schreiben) gehen in `feDiagnostics`, nie ins Spiel |
| `src/GameLoop.hs:360` | `SoundFile p` → `EvSfx p` (byte-gleich), `SoundSynth id` → neues `EvSfxSynth id` |
| `src/Types/Output.hs:307` | `EvSfxSynth String`, `EvMusicStartSynth String`; JSON `{"type":"sfx","synth":"alarm"}` — `path`-Events unverändert |
| `src/Game.hs:272`, `src/Sample.hs:516` | Initialwerte (Typ ändert sich, Wert bleibt `[]`) |
| `docs/protocol-v1.md` | neue Event-Felder |
| `worldbuilder/…/Types.hs:1966` | Objektform `sfx: {synth: …}` / `music: {synth: …}`, Sektionen `sounds:`/`tracks:` in `knownKeys` |
| `worldbuilder/…/Compile.hs:3822` | `AOPlaySfxSynth` → `E.PlaySfxSynth` usw.; `worldSounds` füllen |
| `worldbuilder/…/Export.hs:51` | Synth-Referenzen erzeugen **keine** Asset-Einträge |
| Worldbuilder-Validierung | neue Codes, siehe 4.3 |

**Nicht** geändert: `src/Frontend.hs`, `src/Audio.hs`, `text-adventure-cli/app/Main.hs`,
TUI, `wasm-spike/stubs/Frontend.hs`.

`MusicHandle` dedupliziert nach Pfad (`Audio.hs` `startMusic`); da der Cache-Pfad aus dem
Patch-Hash entsteht, funktioniert „gleicher Track läuft schon → No-op" ohne Änderung.

### 4.3 Validierung (Worldbuilder)

Harte Fehler (stabile Codes):

| Code | Auslöser |
|---|---|
| `UnknownSynthSound` | `sfx: {synth: x}` ohne `sounds.x` |
| `UnknownSynthTrack` | `music: {synth: x}` ohne `tracks.x` |
| `SynthParamRange` | Parameter außerhalb des Bereichs (freq, dur, ADSR, volume, duty, bpm) |
| `SynthTooLong` | SFX > 10 s, gerenderte Track-Länge > 120 s |
| `SynthTooManyLayers` | > 4 Layer bzw. > 4 Kanäle |
| `SynthBadNote` | unlesbare Note im Pattern (mit Spalte) |
| `SynthUnknownInstrument` | Kanal nennt nicht deklariertes Instrument |
| `SynthIdClash` | gleiche ID in `sounds:` und `tracks:` |

Warnungen: `UnusedSynthSound`, `UnusedSynthTrack` (deklariert, nie referenziert) — im bestehenden SWarning-Kanal.

### 4.4 Autorenwerkzeug

```bash
worldbuilder synth list    mein-spiel.yaml               # IDs, Dauer, Layer
worldbuilder synth preview mein-spiel.yaml alarm         # rendert + spielt über Audio-Helper
worldbuilder synth render  mein-spiel.yaml alarm -o alarm.wav
```

`preview` nutzt dieselbe Discovery wie die Engine (`Audio.discoverAudio`) — kein zweiter
Abspielpfad. `render` ist der Pfad, über den Golden-Tests laufen.

---

## 5. Stufenplan

Jede Stufe endet mit grünem `scripts/ci.sh`, CHANGELOG-Eintrag und aktuellen README-Zahlen.

### 5.2a — Design fixieren *(S)* — **Erledigt (2026-10-13)**

- Dieses Dokument abnehmen, offene Punkte O1–O4 entscheiden (→ Abschnitt 6, alle Vorschläge angenommen).
- Abnahme: Entscheidungen in Abschnitt 2 als „entschieden" markiert.

### 5.2b — Renderer im Kern *(M)* — **Erledigt (2026-10-16)**

- `Synth.Patch`, `Synth.Render`, `Synth.Wav`; keine Anbindung an Spiel oder Worldbuilder.
- Tests:
  - Golden: SHA-256 der Samples für je einen Patch pro Wellenform + Layer-Patch.
  - Eigenschaften: Länge = `round (dur * 22050)`; kein Sample außerhalb `Int16`; ADSR
    beginnt und endet bei 0 (kein Knacken); Noise ist pro ID reproduzierbar.
  - WAV-Header: Größenfelder stimmen, Datei ist mit `ffprobe` lesbar (CI, falls verfügbar).
  - Laufzeit: 10-s-SFX rendert in < 200 ms (Grenze für den Dispatch).
- WASM-Spike baut `Synth.Render` mit (Nachweis, dass der Kern WASM-tauglich bleibt).

### 5.2c — SFX im Spiel + Autorenform *(M)* — **Erledigt (2026-10-17)**

- `SoundRef`, neue Effekte, `worldSounds`, `Synth.Cache`, Dispatch, Protokoll-Events.
- Worldbuilder: `sounds:`, Objektform `sfx: {synth: …}`, Validierungscodes, `synth list/render/preview`.
- Doku: Abschnitt „Synth-Sounds" in `adventure-schema.md`, Effekt-Tabelle Zeile 616ff,
  `protocol-v1.md`.
- Fixture `examples/fixtures/synth.yaml` mit `tests:`-Sektion + E2E (CI 4/4b); Protokoll-Golden
  um ein `sfx`-Synth-Event erweitern.
- **Byte-Nachweis:** alle gelieferten Abenteuer mit `cfbf380` und mit dem neuen Stand
  kompilieren → `world.json`/`save.json` identisch; dazu kommt nur das neue Fixture.

### 5.2d — Musik-Tracks *(M–L)* — **Erledigt (2026-10-17)**

- Pattern-Syntax (O3), `Track`, `Channel` (`pattern:` + `patterns:` mit `order:`), `renderTrack`, `music: {synth: …}`, `PlayMusicSynth`, `MusicStartSynth`, `EvMusicStartSynth`.
- Loop-Naht: letzter Step klingt in den ersten aus (Release wird bei `loop: true` über die Loop-Grenze in Step 0 umgebrochen), damit der Helper-Loop nicht knackt; Test auf Sample-Kontinuität an der Naht.
- Validierung `UnknownSynthTrack`, `SynthBadNote` (mit Spalte), `SynthUnknownInstrument`, `SynthTooLong` (> 120 s), `SynthIdClash`, `UnusedSynthTrack`.
- Fixture `examples/fixtures/synth.yaml` + E2E `ci/e2e/synth.*` + Protokoll-Golden `server_events.json` erweitert; Byte-Vertrag für bestehende Abenteuer nachgewiesen.

### 5.2e — TUI-Audio *(S, unabhängig)* — **Erledigt (2026-10-17)**

- `Tui.hs:213` mit `Audio.playSfx`/`startMusic`/`stopMusic` verkabeln, inkl. `--no-audio`/
  `--audio-helper` wie in der CLI. Ohne diese Stufe hört man Synth (und Dateien) nur in der CLI.
- Kann jederzeit vorgezogen werden.

### 5.2f — WebUI-Synth-Tab *(L, hängt an Phase 3)*

- Abspielen in der Spielvorschau (3.2) gemäß O2.
- Editor: Patch-Formular mit Live-Vorschau, Pattern-Raster für Tracks, schreibt über den
  positionstreuen YAML-Schreiber (4.6).

---

## 6. Entschiedene Punkte (ehemals offen)

**Entschieden 2026-10-13 per `/questionnaire`** — alle vier Vorschläge angenommen, Stufenreihenfolge
5.2b → 5.2c → 5.2d wie geplant, 5.2e (TUI-Audio) unabhängig. Damit ist **5.2a abgeschlossen**.

| Nr. | Frage | Entscheidung |
|---|---|---|
| O1 | Cache-Ort und Aufräumen | `$XDG_CACHE_HOME/text-adventure/synth/`, Windows `%LOCALAPPDATA%`; kein Aufräumen nötig (inhaltsadressiert, klein); Renderer-Version im Hash invalidiert alte Dateien |
| O2 | WebUI: WAV-Bytes oder Patch-Daten übers Protokoll? | **WAV-Bytes von der Engine** (Base64 bzw. eigener Endpoint `/synth/<id>.wav`) — ein Renderer, identischer Klang, Browser braucht nur `decodeAudioData`. Patch-Daten nur, falls Live-Editing im Browser ohne Server-Roundtrip gewünscht ist |
| O3 | Pattern-Syntax für Tracks | **String-Raster** wie in 3.2 (lesbar in YAML, diff-freundlich); Listen-Form `[[step, note, len]]` verworfen |
| O4 | Lautstärke-Normierung zwischen Synth und Datei-Sounds | Synth-Ausgabe auf −3 dBFS Spitze begrenzen; globaler Regler erst mit WebUI |

---

## 7. Risiken

- **Latenz:** Jeder SFX startet einen Helper-Prozess (~50–150 ms). Für Erzähl-Sounds okay,
  für UI-Blips hörbar. Gegenmittel später (nicht in diesem Plan): Helper mit Pipe-Steuerung
  oder vorgewärmter Prozess.
- **Erstes Abspielen:** Render + Schreiben im Dispatch blockiert die Schleife kurz; durch S7
  begrenzt (Tracks bis ~120 s ≈ einige 100 ms). Falls störend: Vorrendern aller
  `worldSounds` beim Spielstart in einem Hintergrund-Thread.
- **Gleichzeitige SFX:** Mehrere Helper-Prozesse laufen parallel — Mixing macht das
  Betriebssystem. Kein Engine-Mixer in diesem Plan.
- **Fuzzer (B5):** Rendern liegt außerhalb des reinen Schrittkerns (`applyLoopCommandEv`
  queued nur `SoundRef`), der Fuzzer rendert also nie — die Limits schützen trotzdem das
  echte Spiel.

## 8. Nicht-Ziele

- Kein Echtzeit-Synth, kein Audio-Thread, kein Mixer in der Engine.
- Keine Audio-Bibliothek im Engine-Binary (A1).
- Keine Effekte wie Hall/Filter/LFO in Stufe 1–2 (Filter frühestens als Erweiterung nach 5.2d).
- Kein MIDI-Import/-Export.
- Kein rhythmusgesteuertes Gameplay — die Schleife bleibt zugbasiert (Nachtrag 2026-09-29:
  keine Echtzeit, kein Scheduler-Ticker).
