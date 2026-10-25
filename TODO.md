# TODO

## Species

- [x] 2nd species (2:1): half notes; passing dissonances on the weak beat; downbeat-to-downbeat parallels.
- [ ] 3rd species (4:1): quarters; passing and neighbour tones, cambiata, double neighbour.
- [ ] 4th species: ties and suspensions (7–6, 4–3 above; 2–3 below), breaking species.
- [ ] 5th species (florid): mixed values.
- [ ] Data model: slots cover species 2–3; species 4 needs ties between slots, 5 mixed durations. Score needs quarter heads, beams or flags, ties.

## Ideas

- [x] Hint mode: for the selected bar, show which notes would break no rules (still not enforced).
- [ ] Hints that look ahead: rank candidates by whether the next bars stay solvable (search a few bars deep).
- [ ] Three-voice counterpoint.
- [ ] Alto/tenor clefs as an option.
- [ ] Share an exercise as a link or text.
- [ ] Export a MIDI file.

## Translations

- [ ] i18n with `tongue` (tonsky/tongue), Greek first, English stays the default.
  - Dictionary ns with `:en` / `:el`; a `tr` fn from the current locale (setting, default from `navigator.language`).
  - Strings to move: views, cheat sheet, Hints tab, rule titles/descriptions and `hints/fixes`, violation messages built in `rules.cljs`, tips in `hints.cljs`. Messages that interpolate (note names, intervals, bar numbers) become tongue templates with arguments, which keeps `rules`/`hints` pure (return keys + args, translate in the view).
  - Music terms in Greek: settle a glossary first (e.g. αντίστιξη, cantus firmus, ευθεία/αντίθετη/πλάγια/παράλληλη κίνηση, τέλεια/ατελής σύμφωνία, διαφωνία, τρίτονο, προσαγωγέας). Note names: keep C D E… or Ντο Ρε Μι…? Decide (probably a setting).
  - Interval ordinals and plurals (3rd/3ης, "two octaves") need per-language formatting fns, not just strings.
