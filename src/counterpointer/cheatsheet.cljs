(ns counterpointer.cheatsheet
  (:require [counterpointer.theory :as t]
            [counterpointer.rules :as rules]
            [counterpointer.library :as lib]
            [counterpointer.score :as score]))

(def c-major {:mode :ionian :final {:l 0 :a 0}})
(def d-dorian {:mode :dorian :final {:l 1 :a 0}})

(defn example [key upper lower caption]
  (let [e {:cf (lib/line key lower) :cp (lib/line key upper) :key key :position :above}
        {:keys [verts motions]} (rules/context e)]
    [:figure.example
     [score/score (assoc e :n-cols (count (:cp e)) :verts verts :motions motions :labels true :compact true)]
     [:figcaption caption]]))

(def intervals
  [["P1" "unison" 0 "C–C" :perfect]
   ["m2" "minor 2nd" 1 "E–F" :dissonant]
   ["M2" "major 2nd" 2 "C–D" :dissonant]
   ["m3" "minor 3rd" 3 "D–F" :imperfect]
   ["M3" "major 3rd" 4 "C–E" :imperfect]
   ["P4" "perfect 4th" 5 "C–F" :dissonant]
   ["A4 / d5" "tritone" 6 "F–B / B–F" :dissonant]
   ["P5" "perfect 5th" 7 "C–G" :perfect]
   ["m6" "minor 6th" 8 "E–C" :imperfect]
   ["M6" "major 6th" 9 "C–A" :imperfect]
   ["m7" "minor 7th" 10 "D–C" :dissonant]
   ["M7" "major 7th" 11 "C–B" :dissonant]
   ["P8" "octave" 12 "C–C" :perfect]])

(def class-label {:perfect "perfect consonance" :imperfect "imperfect consonance" :dissonant "dissonance"})

(defn view []
  [:div.cheat
   [:div.card
    [:h3 "The idea"]
    [:p "A " [:b "cantus firmus"] " (CF, \"fixed song\") is a plain melody in whole notes. You write a second line, the "
     [:b "counterpoint"] " (CP), against it. The goal is two lines that are each good melodies and stay independent of each other."]
    [:p "Fux (" [:i "Gradus ad Parnassum"] ", 1725) teaches it in five " [:b "species"] ", each adding rhythm:"]
    [:ol
     [:li [:b "1st, note against note (1:1)"] ": only consonances. This is the one implemented now."]
     [:li [:b "2nd (2:1)"] ": two half notes per CF note; the second may be a passing dissonance."]
     [:li [:b "3rd (4:1)"] ": four quarters; passing and neighbour tones, the cambiata."]
     [:li [:b "4th, syncopation"] ": tied notes, suspensions that resolve down by step."]
     [:li [:b "5th, florid"] ": all of the above mixed."]]]

   [:div.card
    [:h3 "Intervals"]
    [:p "Count letters, both ends included: C up to E is a 3rd (C, D, E). The quality (perfect, major, minor, augmented, diminished) comes from the semitones. Beyond the octave they are " [:b "compound"] ": a 10th is a 3rd plus an octave and is treated like a 3rd."]
    [:table.intervals
     [:thead [:tr [:th "Name"] [:th ""] [:th.num "Semitones"] [:th "e.g."] [:th "In two voices"]]]
     [:tbody
      (for [[n long semis eg cls] intervals]
        ^{:key n}
        [:tr [:td [:b n]] [:td long] [:td.num semis] [:td eg] [:td [:span.cls {:class (name cls)} (class-label cls)]]])]]]

   [:div.card
    [:h3 "Consonance and dissonance"]
    [:dl
     [:dt [:span.cls.perfect "Perfect consonances"] " unison, 5th, octave (and 12th, 15th…)"]
     [:dd "Stable and hollow. They frame the exercise (start and end) but too many in a row sound empty, and two of the same kind in parallel are forbidden."]
     [:dt [:span.cls.imperfect "Imperfect consonances"] " 3rds and 6ths, major or minor (and 10ths…)"]
     [:dd "Sweet and mobile. They should be the majority, and you can move between them freely, even in parallel (up to three in a row)."]
     [:dt [:span.cls.dissonant "Dissonances"] " 2nds, 7ths, the 4th, the tritone, any augmented or diminished interval"]
     [:dd "Not allowed in first species. Later species let them in only in controlled ways (passing tones, suspensions). The 4th is consonant between upper voices in fuller textures, but dissonant against the bass, and with two voices one of them is the bass."]]]

   [:div.card
    [:h3 "Motion between two voices"]
    [:p "How the voices move from one interval to the next. The numbers between the staves are the intervals."]
    [:div.examples
     [example c-major "E4 G4" "C4 B3" [:span [:b "Contrary"] ": opposite directions. The best for independence."]]
     [example c-major "E4 A4" "C4 C4" [:span [:b "Oblique"] ": one voice holds, the other moves."]]
     [example c-major "G4 B4" "C4 D4" [:span [:b "Similar"] ": same direction, different intervals."]]
     [example c-major "E4 F4" "C4 D4" [:span [:b "Parallel"] ": same direction, same interval. Fine for 3rds and 6ths…"]]
     [example c-major "G4 A4" "C4 D4" [:span "…but " [:b "parallel 5ths"] " (and octaves) are forbidden: the voices fuse into one."]]
     [example c-major "E4 A4" "C4 D4" [:span [:b "Direct (hidden) 5th"] ": " [:i "similar"] " motion into a 5th or octave (here 3→5, both rising). Strict style avoids it; modern texts allow it when the upper voice moves by step."]]]
    [:p "Rule of thumb: approach perfect consonances by contrary or oblique motion; use similar and parallel motion only into imperfect ones."]]

   [:div.card
    [:h3 "The cadence"]
    [:div.examples
     [example d-dorian "C#4 D4" "E3 D3" [:span "Counterpoint above: 7→1 against 2→1, a major 6th opening to an octave. The C♯ is the raised leading tone."]]
     [example d-dorian "E4 D4" "C#4 D4" [:span "Counterpoint below: a minor 3rd closing to a unison."]]]
    [:p "Both voices step into the final from opposite sides, and one of them by a half step. In modes without a half step below the final (Dorian, Mixolydian, Aeolian) the 7th is raised: this is " [:b "musica ficta"] ", an accidental singers added by convention. Phrygian already has the half step above (F→E), so its 7th stays."]]

   [:div.card
    [:h3 "Other terms"]
    [:dl
     [:dt "Mode, final"] [:dd "The scale. The " [:b "final"] " is its home note: D for Dorian, E Phrygian, F Lydian, G Mixolydian, A Aeolian, C Ionian on the white keys. The cantus starts and ends on it. Ionian and Aeolian are today's major and natural minor."]
     [:dt "Step, skip, leap"] [:dd "A 2nd is a step; a 3rd is a skip; a 4th or more is a leap. Lines should move mostly by step."]
     [:dt "Leap recovery"] [:dd "After a large leap (more than a 4th), turn around, ideally by step, to fill in the gap."]
     [:dt "Climax"] [:dd "The highest note of a line. Ideally it occurs once, so the melody has one clear peak."]
     [:dt "Voice crossing / overlap"] [:dd "Crossing: the upper voice goes below the lower one. Overlap: a voice moves past the note the other one just left. Both blur the two lines."]
     [:dt "Antiparallels"] [:dd "5th to 12th or octave to 15th by contrary motion. The same perfect interval twice, just disguised."]
     [:dt "Tritone"] [:dd "Three whole steps (F–B), the \"devil in music\". Avoided as a leap, and as the outline of a phrase."]]]

   [:div.card
    [:h3 "Rules checked"]
    [:p.hint "Errors are the core rules every textbook agrees on. Warnings are style points that differ between books; " [:i "strict only"] " ones are Fux's stricter preferences."]
    (for [[g label] rules/groups]
      ^{:key g}
      [:div.rule-group
       [:div.group-label label]
       [:ul.rule-list
        (for [{:keys [id title desc level presets]} (filter #(= g (:group %)) rules/rules)]
          ^{:key id}
          [:li [:span.tag {:class (name level)} (name level)] " " [:b title]
           (when-not (contains? presets :relaxed) [:span.tag.preset "strict only"])
           (when-not (contains? presets :strict) [:span.tag.preset "relaxed only"])
           [:div desc]])]])]

   [:div.card
    [:h3 "About the score"]
    [:p "There is no objective measure of musical quality. What exists, going back to computer counterpoint programs like Schottstaedt's (1984), is a " [:b "penalty"] ": each broken rule adds points (" (rules/penalty :error) " per error, " (rules/penalty :warning) " per warning here). Lower is closer to the style, not necessarily more beautiful."]
    [:p "The Shape panel shows proportions teachers look at (contrary motion, imperfect consonances, steps). Compare them across your attempts at the same cantus, and play both lines: your ear is the final judge."]]])
