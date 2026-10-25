# Counterpointer

Static re-frame/shadow-cljs SPA for species counterpoint, no backend. Sibling of `../chordtrainer` (same stack; `audio`, `midi`, `storage` were copied from it). See README for features and deploy.

## Commands

- `npx shadow-cljs watch app`: dev server on :8281
- `npx shadow-cljs compile test`: runs `test/` on node (theory + rule tests)
- `npm run dist`: release build into `dist/`

## Conventions

- `dist/` is committed on purpose (deploy by `git pull`). Run `npm run dist` before committing changes to `src/` or `resources/public/`.
- `theory.cljs`, `rules.cljs` and `hints.cljs` are pure; keep them that way. `hints/candidates` tries each pitch in the selected slot through `rules/analyze` and keeps violations touching that slot; at the penultimate slot it assumes the final note if it's missing. `hints/fixes` holds the "Try:" text per rule id: add one for every new rule.
- Notes are `{:s step :a alt}`: `:s` is diatonic (letter + 7 × octave, C4 = 28), `:a` the alteration. Intervals are named from letter distance, so spelling matters (A4 vs d5). Never store bare MIDI numbers; use `t/spell` for MIDI input.
- Lines are vectors with `nil` for empty bars, trimmed of trailing `nil`s (`t/put`, `t/trim`). Max 32 cantus notes.
- The counterpoint is a flat vector of *slots*, `rules/per-bar` per cantus note (1 in 1st species, 2 in 2nd). Slot `k` sounds against cantus note `(quot k per)`. Selection `:col` and violation `:cols` are slots for the counterpoint and two-voice rules, but bars for the cantus (`:voice :cf`). In 2nd species a `nil` first slot is the opening half rest and the final note is drawn as a whole note.
- Rules never block input; they only report. Every rule is metadata in `rules/rules` plus a `check` (two-voice) or `check-line` (per voice) method. `:presets` decides membership of Strict/Relaxed; `:kind` decides where the score marks it.
- The test `fux-cantus-firmi-are-clean` checks all six Fux cantus firmi and `example-solutions-are-clean` checks the library examples (one per species) under both presets. Run tests after changing a rule; a rule that flags Fux himself is probably wrong.
- Persistence: IndexedDB via idb-keyval, keys `settings`, `exercise`, `saved`, as EDN strings, written by the global `persist` interceptor once storage has loaded. New setting keys need a default in `events/default-settings`. When the preset isn't `:custom`, the rule set is recomputed from the preset on load, so new rules appear automatically.
- Playback ticks carry a `:token`; any new delayed event must check it.
- `resources/public/sw.js` lists the app shell and samples; bump `VERSION` when the cached set changes.

## Roadmap

`TODO.md`. Species 3 (4 slots per bar) should fit the slot model; species 4 needs ties between slots, 5 needs mixed durations.
