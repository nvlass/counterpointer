# Counterpointer

Static re-frame/shadow-cljs SPA for species counterpoint, no backend. Sibling of `../chordtrainer` (same stack; `audio`, `midi`, `storage` were copied from it). See README for features and deploy.

## Commands

- `npx shadow-cljs watch app`: dev server on :8281
- `npx shadow-cljs compile test`: runs `test/` on node (theory + rule tests)
- `npm run dist`: release build into `dist/`

## Conventions

- `dist/` is committed on purpose (deploy by `git pull`). Run `npm run dist` before committing changes to `src/` or `resources/public/`.
- `theory.cljs` and `rules.cljs` are pure; keep them that way.
- Notes are `{:s step :a alt}`: `:s` is diatonic (letter + 7 × octave, C4 = 28), `:a` the alteration. Intervals are named from letter distance, so spelling matters (A4 vs d5). Never store bare MIDI numbers; use `t/spell` for MIDI input.
- Lines are vectors with `nil` for empty bars, trimmed of trailing `nil`s (`t/put`, `t/trim`). Max 32 notes.
- Rules never block input; they only report. Every rule is metadata in `rules/rules` plus a `check` (two-voice) or `check-line` (per voice) method. `:presets` decides membership of Strict/Relaxed; `:kind` decides where the score marks it.
- The test `fux-cantus-firmi-are-clean` checks all six Fux cantus firmi and `example-solution-is-clean` checks a known-good counterpoint under Strict. Run tests after changing a rule; a rule that flags Fux himself is probably wrong.
- Persistence: IndexedDB via idb-keyval, keys `settings`, `exercise`, `saved`, as EDN strings, written by the global `persist` interceptor once storage has loaded. New setting keys need a default in `events/default-settings`. When the preset isn't `:custom`, the rule set is recomputed from the preset on load, so new rules appear automatically.
- Playback ticks carry a `:token`; any new delayed event must check it.
- `resources/public/sw.js` lists the app shell and samples; bump `VERSION` when the cached set changes.

## Roadmap

`TODO.md`. Species 2–5 are next; the data model will need durations per bar for the counterpoint line.
