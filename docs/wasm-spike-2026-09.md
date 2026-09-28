# WASM-Machbarkeits-Spike (Plan-Stufe 1.0)

**Datum:** 2026-09-28 · **Ergebnis: ✅ MACHBAR — besser als geplant**

## Kernfrage der Plan-Stufe

> Kern-Module (`Types`, `Game`, `Parser`, `Effects`, … ohne `haskeline`/`process`/`directory`)
> mit dem GHC-WASM-Backend bauen, ein `applyLoopCommand` im Browser ausführen.

## Ergebnis in einem Satz

Die **unveränderte Produktions-Library** (alle 20 Module inkl. `Frontend` mit Haskeline,
`SaveLoad`, `Audio`, `World`) **kompiliert und läuft** mit GHC 9.14.1 (wasm32-wasi,
GMP-Flavour) unter wasmtime **und** Node.js — die Ausgaben sind **byte-identisch** zu einem
nativ (GHC 9.6.7) gebauten Referenzlauf. Eine Stub- oder IO-Trennung ist für den Spike
**nicht nötig**; für 5.1 bleibt sie trotzdem empfohlen (siehe Blocker).

## Aufbau des Spike (`wasm-spike/`)

| Datei | Zweck |
|---|---|
| `spike.cabal` | Library (_HS-Quelle: `../src`, also der echte Code) + `spike-main` |
| `Spike.hs` | Fahrprogramm: Sample-Spiel (`initSampleGame`) + kompiliertes `dark-feelable`-Adventure über `applyLoopCommand` |
| `copy.sh`, `stubs/`, `src-gen/` | **Erster Versuch** (Fallback dokumentiert): Kopie der Kern-Module + IO-Stubs für `Frontend`/`SaveLoad`/`World` — funktionierte, ist aber obsolet, seit die echte Library baut |
| `run.sh` | Fahrer: copy → build → wasmtime mit `--dir .` |
| `data/` | `world.json`/`save.json` (via `worldbuilder compile examples/fixtures/dark-feelable.yaml -o wasm-spike/data`), nicht eingecheckt |

## Durchgeführte Versuche und Messwerte

1. **Toolchain:** `ghc-wasm-meta` (`setup.sh`, ~2 GB unter `~/.ghc-wasm`): GHC
   9.14.1.20260731 (wasm32-wasi, GMP), wasi-sdk 29, wasmtime, cabal 3.14.2, plus
   Node-Runner. Installation ~10 min, skriptgesteuert, keine Manuellen Eingriffe.
2. **GHC-Pakete der Bindist:** `containers`, `text`, `time`, **`directory`,
   `filepath`, `process`, `haskeline`** (WASI-gepatcht) sind bereits dabei; `aeson`
   und `aeson-pretty` wurden von Hackage gebaut (einmalig ~3 min).
3. **Stub-Variante (erster Versuch):** 17 Module kopiert, `Frontend`/`SaveLoad`/`World`
   gestubbt → baut und läuft. bestätigt die Plan-Annahme für den reinen Kern.
4. **Echte Library:** `hs-source-dirs: ../src`, alle Produktions-Dependencies
   (`haskeline`, `directory`, `process`, `filepath`, `time`, `aeson`, `aeson-pretty`) →
   baut **ohne eine einzige Quellcode-Änderung**.
5. **Fahrprogramm** (identischer Quelltext nativ und WASM):
   - Sample-Spiel: `look`, `take torch`, `inventory`, `go north`, `help`
   - `dark-feelable`-Adventure (aeson-Deserialisierung von `world.json`/`save.json`):
     `look` (dunkel → blockiert), `take relic` (nicht ertastbar → blockiert),
     `take torch` (`feelable` → OK), `inventory`, `use lever` (Raum wird sichtbar), `look`
6. **Ergebnis:** wasmtime **und** Node.js (`node:wasi`, Preview 1) liefern Ausgaben
   **byte-identisch** zum nativen Lauf (nach Abfiltern der Node-Experimental-Warnung).
   Runtime-Overhead: wasmtime ~70 ms für das ganze Fahrprogramm.
7. **Zusätzlicher Typecheck:** `Audio`, `Frontend`, `SaveLoad`, `World` (die echten
   IO-Module) typechecken auf wasm32-wasi (`-fno-code`) ebenfalls.

## Erkenntnisse für 1.3/1.4 (Protokoll & Session-Automat)

- **Kein Blocker für das Protokoll:** `applyLoopCommand` ist rein und läuft in WASI;
  ein späteres `ClientMsg`/`ServerMsg`-JSON (1.4) nutzt dasselbe aeson, das im Spike
  bereits World-Dateien deserialisiert hat.
- **Styling:** Die Spike-Ausgaben enthalten die ANSI-Codes der Engine unverändert —
  die Stufe-1.2-Entscheidung (Style-/Span-Info statt ANSI in Events) bleibt notwendig,
  sonst muss die WebUI ANSI parsen. Der Spike ändert daran nichts, bestätigt aber, dass
  die rohen Strings 1:1 ankommen.
- **Derzeitiger Zustand als „Impedance“ fürs Browser-Target:** Der RIO-Teil
  (`loopGame`) braucht ein Event-Driven-Frontend statt blockierendem `feReadInput`;
  das ist genau die 1.3-Arbeit (IO-Wünsche statt direkter IO). Der Spike liefert dafür
  die Bestätigung, dass darunter ein vollwertiger, rein rechennder Kern steht.
- **Encoding:** `hSetEncoding stdout utf8` funktioniert unter WASI; für den
  Browser-Export (5.1) sollte die Ausgabe später über das JSON-Protokoll laufen
  (Strings, keine Bytes), dann entfällt das Thema ganz.

## Blocker / Restrisiken für 5.1 (Web-Export)

| # | Blocker | Schwere | Anmerkung |
|---|---|---|---|
| 1 | **`haskeline` läuft unter WASI nur als Stub-Loop** (stdin-Handle ohne TTY); für den Browser ohnehin irrelevant — dort stellt das JS-Frontend die Eingabe | niedrig | Für 1.3/1.4 unkritisch: das reine `applyLoopCommand` braucht kein haskeline |
| 2 | **Save/Load über WASI-Preopens** (`--dir .`) funktioniert nur für Dateisystem-Runtimes; **Browser hat kein WASI-FS** | mittel | Save/Load muss in 5.1 über JS-Imports (`JSFFI`) oder das Server-Protokoll laufen — betrifft `SaveLoad`/`World`-IO, nicht den Kern |
| 3 | **`process`-Aufrufe in `Audio.hs`** (ffplay/ffmpeg) existieren im Browser nicht | mittel | Audio geht über Web Audio (Phase 5.2, ohnehin geplant); `Audio.hs` einfach nicht in den WASM-Build linken |
| 4 | **GHC-WASM ist 9.14-Flavour** (Produktion: 9.6.7) — keine Sprach-Importe, aber zwei GHC-Versionen pflegen | niedrig | CI könnte optional einen wasm-Build als Smoke-Test aufnehmen |
| 5 | **WASM-Modulgröße 6,0 MB** (unkomprimiert, -O1, Debug-Info) | niedrig | Für den Web-Export: `wasm-opt`/gzip/br und ggf. Splitting; keine Struktur-Blocker |
| 6 | Node 26 markiert `node:wasi` als experimentell; wasmtime/Chrome/Firefox laufen stabil | niedrig | Für die WebUI (Haskell-Server, Phase 3) irrelevant; für 5.1 Browser-Target Standard-WASM |

## Aufwandsschätzung für 5.1 (Plan fordert „Aufwand für 5.1“)

- **JS-Glue + FFI für Input/Output über das Protokoll (1.4):** S
- **Save/Load über JS-Storage oder Server:** M (Ersatz für die WASI-FS-Teile von
  `SaveLoad`; der pure Teil bleibt)
- **Audio-Entkopplung (`Audio.hs` nicht linken, Web Audio im Frontend):** S
- **Build-Integration ( Cabal-Flags, CI-Smoke, `wasm-opt`, Packaging):** M
- **Insgesamt 5.1: eher M als XL** — der Risiko-Teil („kann der Kern überhaupt nach
  WASM?“) ist durch den Spike entfallen; was bleibt, ist I/O-Glue.

## Entscheidungsempfehlung (Haltepunkt „nach 1.0“)

- **5.1 (Web-Export) bleibt im Plan** — der Spike ist klar positiv.
- **Phase 3 (WebUI) kann wie geplant auf den Haskell-Server setzen**; der WASM-Pfad
  ist eine spätere, optionale Distribution-Form desselben Protokolls.
- Für **1.3** (purer Session-Automat) gilt der Befund als Bestätigung, nicht als
  neue Arbeit: Der Kern ist schon rein; es fehlt nur die Loop-IO als „Wünsche“.
