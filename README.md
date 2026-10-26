# Counterpointer

A trainer for species counterpoint. Enter a cantus firmus (or load one of Fux's), write a counterpoint against it, and the app reports which rules each note breaks, without blocking anything. Static re-frame SPA, works offline (PWA), everything stays in the browser (IndexedDB).

## What it does

- **Two-staff score**: upper voice on the treble staff, lower on the bass. Click to place notes; the counterpoint can be above or below the cantus. Interval numbers and the motion type (contrary / oblique / similar / parallel) are shown between the staves.
- **Input**: click, computer keyboard (`A`–`G` enters a note and moves on, arrows move it, `#` `-` `n` accidentals, `Tab` switches voice), typed text (`D F E D G F A G F E D`, `D4 C#5`), or a MIDI keyboard.
- **Rule check**: errors (core rules) and warnings (style points), with a penalty total (10 per error, 3 per warning). Tap a violation to highlight it on the staff, or mute that rule.
- **Rule sets**: Strict (Fux) and Relaxed (modern textbook), or any custom selection.
- **Shape stats**: share of contrary motion, imperfect consonances and stepwise motion. Descriptive, not scored.
- **Hints**: a panel under the score with advice for the selected bar (opening, cadence with the exact notes, the cantus direction, leaps to recover, parallel chains, consonance balance), and on request the notes that break no rules there, tried one by one against the rule check. Each violation shows a "Try:" fix when tapped. The Hints tab has a step-by-step way to work, per-species tips, common problems and fixes, practice habits and further reading.
- **Cheat sheet**: intervals and their consonance class, motion types with examples, the cadence, terms, and every rule explained.
- **Library**: Fux's six cantus firmi (one per mode), an example solution, and your saved exercises.
- Modes: Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Ionian, on any final with up to six sharps or flats.

First species (1:1) and second species (2:1: half notes, passing tones on the upbeat, an opening half rest, downbeat-to-downbeat checks). Switching species keeps the counterpoint's downbeats.

## Develop

```sh
npm install
npx shadow-cljs watch app     # http://localhost:8281
npx shadow-cljs compile test  # theory and rule tests (node)
```

## Build & deploy

```sh
npm run dist
rsync -a --delete dist/ server:/var/www/counterpointer/
```

Plain static files; the nginx config from chordtrainer's README applies as is (no-cache for `sw.js`, `index.html`, `/js/`; immutable for `/samples/`).

Piano samples: FluidR3_GM acoustic grand (via [gleitz/midi-js-soundfonts](https://github.com/gleitz/midi-js-soundfonts), CC BY 3.0).

## License

MIT, see [LICENSE](LICENSE). The piano samples are CC BY 3.0 (see above).
