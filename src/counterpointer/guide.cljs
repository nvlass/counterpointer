(ns counterpointer.guide
  (:require [counterpointer.rules :as rules]
            [counterpointer.hints :as hints]))

(defn view []
  [:div.cheat
   [:div.card
    [:h3 "A way to work"]
    [:ol.steps
     [:li [:b "Get to know the cantus."] " Play it, sing it. Find its high point and how it reaches the final (from above or below)."]
     [:li [:b "Write the cadence first."] " The last two bars are nearly fixed: both voices step into the final from opposite sides, one by a half step (6→8 or 3→1). The Write tab's hints name the exact notes."]
     [:li [:b "Then the opening."] " Unison, 5th or octave above; unison or octave below."]
     [:li [:b "Sketch the middle in 3rds and 6ths, moving against the cantus."] " When it goes up, go down. That one habit avoids most parallels and direct 5ths."]
     [:li [:b "Shape your own line."] " Put your climax somewhere other than the cantus's. Move mostly by step; after a big leap, turn back."]
     [:li [:b "Check, then listen."] " Fix the errors first (red). Warnings (amber) are style; if it sounds good, a warning can stay."]
     [:li [:b "Do it again."] " The same cantus with the counterpoint below, then in another mode. Each one is a different puzzle."]]]

   [:div.card
    [:h3 "First species (1:1)"]
    [:ul
     [:li "Think in " [:b "3rds and 6ths"] ". Most good solutions are 3rds and 6ths with a few 5ths and octaves, often where the voices cross the middle of their ranges."]
     [:li [:b "Contrary motion first"] ", oblique next (when the cantus repeats), similar and parallel only into 3rds and 6ths."]
     [:li "Two or three parallel 3rds or 6ths are fine and sound sweet; then break the chain."]
     [:li "Get into a 5th or octave by " [:b "contrary motion"] ", e.g. from a 6th to an octave or a 3rd to a 5th with the voices moving apart."]
     [:li "Swapping between a 3rd and a 6th (or 10th) is an easy way to keep moving: the counterpoint leaps a 4th or 5th while the cantus steps the other way."]
     [:li "Stuck in one bar? Try the note a step above and a step below the obvious one before changing the bars around it."]]]

   [:div.card
    [:h3 "Second species (2:1)"]
    [:ul
     [:li [:b "Plan the downbeats like first species"] " (consonant, good motion between them), then fill in the upbeats."]
     [:li "The upbeat gives you three tools: a " [:b "passing tone"] " (step through, even as a dissonance), a " [:b "consonant leap"] " (to change register or set up the next downbeat), and a " [:b "consonant step"] "."]
     [:li "If two downbeats are a 3rd apart melodically, a passing tone between them almost always works."]
     [:li "A leap on the upbeat is a good way to escape a parallel: it changes the approach to the next downbeat."]
     [:li "Check the motion " [:b "from the upbeat to the next downbeat"] " for parallels and direct 5ths, and downbeat to downbeat for 5–5 or 8–8."]
     [:li "Start with a half rest and enter on the upbeat; end with 5–6–8 above (or 5–3–1 below)."]
     [:li "Avoid repeated notes; and don't let the line zig-zag up and down a 3rd for long."]]]

   [:div.card
    [:h3 "Common problems and fixes"]
    [:p.hint "The same advice appears under a violation when you tap it in the Write tab."]
    [:dl.fixes
     (for [id [:parallel-perfect :direct-perfect :consonance :weak-dissonance-passing :leading-tone :cadence-step
               :parallel-run :imperfect-majority :leap-recovery :climax :tritone-outline :range]
           :let [r (rules/rule-by-id id)]]
       ^{:key id}
       [:<> [:dt (:title r)] [:dd (hints/fixes id)]])]]

   [:div.card
    [:h3 "Practice habits"]
    [:ul
     [:li [:b "Sing."] " Sing the cantus while playing the counterpoint, then swap. If a line is awkward to sing, it's probably awkward."]
     [:li [:b "Listen to each voice alone"] " (the Cantus / Counterpoint buttons), then together."]
     [:li [:b "Keep several attempts"] " at the same cantus (save each) and compare their Shape panels."]
     [:li [:b "Use Relaxed while learning,"] " Strict once the core rules are second nature."]
     [:li [:b "\"Show notes that work here\""] " (on the Write tab) is a crutch: use it when stuck, then ask yourself why the others fail."]]]

   [:div.card
    [:h3 "Further reading"]
    [:ul
     [:li "Johann Joseph Fux, " [:i "The Study of Counterpoint"] " (Gradus ad Parnassum, tr. Alfred Mann): the source, written as a dialogue between teacher and student. Short and readable."]
     [:li "Knud Jeppesen, " [:i "Counterpoint: The Polyphonic Vocal Style of the Sixteenth Century"] ": species counterpoint grounded in Palestrina's actual practice."]
     [:li "Felix Salzer & Carl Schachter, " [:i "Counterpoint in Composition"] ": species as a way into tonal music, with many examples from the repertoire."]
     [:li "Peter Schubert, " [:i "Modal Counterpoint, Renaissance Style"] ": a modern, practical textbook."]]]])
