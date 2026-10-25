(ns counterpointer.i18n.en
  (:require [clojure.string :as str]
            [counterpointer.theory :as t]))

;; ---------------------------------------------------------------- formatting

(defn- note [loc n] (t/note-name n (:names loc)))
(defn- pcn [loc n] (t/pc-name n (:names loc)))
(defn- fin [loc f] (t/final-name f (:names loc)))

(def ^:private modes
  {:dorian "Dorian" :phrygian "Phrygian" :lydian "Lydian" :mixolydian "Mixolydian"
   :aeolian "Aeolian (minor)" :ionian "Ionian (major)"})

(defn- key-name [loc {:keys [mode final]}] (str (fin loc final) " " (modes mode)))

(def ^:private iv-words
  {1 "unison" 2 "2nd" 3 "3rd" 4 "4th" 5 "5th" 6 "6th" 7 "7th" 8 "octave"
   9 "9th" 10 "10th" 11 "11th" 12 "12th" 13 "13th" 14 "14th" 15 "two octaves"})

(def ^:private q-words
  {:P "perfect" :M "major" :m "minor" :A "augmented" :d "diminished"
   :AA "doubly augmented" :dd "doubly diminished"})

(defn- iv-long [{:keys [num q] :as iv}]
  (cond
    (t/tritone? iv) (str "tritone (" (q-words q) " " (iv-words num) ")")
    (= num 15) (str (when-not (= q :P) (str (q-words q) " ")) "two octaves")
    :else (str (q-words q) " " (iv-words num (str num "th")))))

(defn- a [s]
  (cond (re-find #"^two" s) s
        (re-find #"^[aeiou]" s) (str "an " s)
        :else (str "a " s)))

(defn- perfects [v] (cond (zero? (:g v)) "unisons" (zero? (:g7 v)) "octaves" :else "5ths"))

(defn- pair [v w] (str "(" (:num v) " → " (:num w) ")"))

(defn- imperfects [g7] (if (= 2 g7) "3rds" "6ths"))

(def ^:private motions
  {:contrary "contrary" :oblique "oblique" :similar "similar" :parallel "parallel" :static "static"})

;; ---------------------------------------------------------------- dictionary

(def dict
  {:fmt
   {:note note
    :pc pcn
    :final fin
    :key key-name
    :mode (fn [_ id] (modes id))
    :on-final (fn [loc f] (str "on " (fin loc f)))
    :signature (fn [_ s] (cond (zero? s) "no sharps or flats" (pos? s) (str s " ♯") :else (str (- s) " ♭")))
    :iv-name (fn [_ v] (t/interval-name v))
    :iv-long (fn [_ v] (iv-long v))
    :iv (fn [_ v] (str (t/interval-name v) ": " (iv-long v)))
    :motion (fn [_ k] (motions k))
    :motion-abbr (fn [_ k] ({:contrary "c" :oblique "o" :similar "s" :parallel "p" :static "–"} k))}

   :bars
   {:one (fn [_ n] (str "bar " n))
    :range (fn [_ x y] (str "bars " x "–" y))
    :list (fn [_ cs] (str "bars " (str/join ", " cs)))
    :beat (fn [_ n down?] (str "bar " n (if down? ", downbeat" ", upbeat")))}

   :parse
   {:too-many (fn [_ n] (str "At most " n " notes"))
    :bad-token (fn [_ tok] (str "Can't read “" tok "”"))}

   :group
   {:harmony "Intervals between the voices"
    :motion "Motion between the voices"
    :cadence "Cadence"
    :melody "Each melodic line"
    :cf "Cantus firmus"}

   :preset {:strict "Strict (Fux)" :relaxed "Relaxed (modern)" :custom "Custom"}

   :level {:error "error" :warning "warning" :info "info"}

   :rule
   {:consonance "Consonant downbeats"
    :weak-dissonance-passing "Dissonant upbeats only as passing tones"
    :weak-dissonance "Dissonant upbeats as passing or neighbour tones"
    :start "Open on a perfect consonance"
    :end "Close on a unison or octave"
    :imperfect-majority "Mostly imperfect consonances"
    :unison "No unison in the middle"
    :spacing "Voices within a 10th"
    :crossing "No voice crossing"
    :overlap "No voice overlap"
    :parallel-perfect "No parallel 5ths or octaves"
    :downbeat-parallels "No 5ths or octaves on successive downbeats"
    :contrary-perfect "No 5ths/octaves by contrary motion"
    :direct-perfect "No direct (hidden) 5ths or octaves"
    :direct-perfect-leap "Direct 5ths/octaves only with a step on top"
    :parallel-run "At most 3 parallel 3rds or 6ths in a row"
    :leap-together "Don't leap together"
    :static "Some voice should move"
    :cadence-step "Approach the final by step, in contrary motion"
    :leading-tone "Half step into the final"
    :final-whole "End on a whole note"
    :melodic-dissonance "No dissonant leaps"
    :leap-size "No leaps beyond an octave"
    :sixth "Only the rising minor 6th"
    :leap-recovery "Recover large leaps"
    :leap-chain "Careful with successive leaps"
    :repeat "Avoid repeated notes"
    :tritone-outline "Don't outline a tritone"
    :range "Range within a 10th"
    :climax "A single climax"
    :leaps-many "Mostly stepwise"
    :mode "Stay in the mode"
    :cf-final "Cantus starts and ends on the final"
    :cf-ending "Cantus ends by step"
    :cf-length "Cantus length 8–16"}

   :rule.desc
   {:consonance "Every vertical interval on a downbeat must be a consonance: unison, 3rd, 5th, 6th, octave (or compounds like 10th, 12th). The 4th counts as a dissonance in two voices. In first species every note is a downbeat."
    :weak-dissonance-passing "A dissonance on the second half note must be a passing tone: approached and left by step in the same direction, filling in a 3rd between two consonant downbeats (D–E–F over a held D–F–A)."
    :weak-dissonance "The modern version: a dissonant upbeat may also be a neighbour tone, stepping away and back to the same note. Fux allows only passing tones."
    :start "Start on a unison, 5th or octave when the counterpoint is above; unison or octave when it is below (a 5th below the cantus would suggest a different final)."
    :end "The last interval must be a unison or an octave, both voices on the final."
    :imperfect-majority "In the middle, 3rds and 6ths should outnumber 5ths and octaves. Perfect consonances sound hollow and stop the motion; imperfect ones keep it going."
    :unison "Fux allows the unison only on the first and last note: in the middle the two voices merge into one."
    :spacing "Keep the voices no more than a 10th apart. A 12th is tolerated now and then; wider and they stop sounding like a pair."
    :crossing "The upper voice should stay above the lower one. Crossing hides which voice is which."
    :overlap "A voice should not move past the note the other voice just left (e.g. the lower voice rising above the upper voice's previous note)."
    :parallel-perfect "Two perfect consonances of the same kind in a row, in the same direction (5→5, 8→8, 1→8, 5→12): the voices lose their independence. The central rule of counterpoint."
    :downbeat-parallels "5th to 5th or octave to octave from one downbeat to the next is heard as parallel motion; a single upbeat in between doesn't hide it. Some texts accept it when the upbeat leaps a 4th or more."
    :contrary-perfect "5→12 or 8→15 in contrary motion ('antiparallels') still moves between the same perfect consonance; treated as a disguised parallel."
    :direct-perfect "Approaching a 5th or octave by similar motion (both voices moving the same way). Strict style avoids it altogether."
    :direct-perfect-leap "The modern version of the previous rule: similar motion into a 5th or octave is fine if the upper voice moves by step, but not when it leaps."
    :parallel-run "Parallel imperfect consonances are allowed, but a long chain of them makes the counterpoint a mere shadow of the cantus."
    :leap-together "Both voices leaping (a 4th or more) in the same direction is heavy and undermines independence."
    :static "Both voices repeating their notes means nothing happens."
    :cadence-step "The cadence: one voice goes 2→1, the other 7→1, so the penultimate interval is a 6th resolving to an octave, or a 3rd resolving to a unison."
    :leading-tone "One of the two voices must arrive at the final by a half step. In Dorian, Mixolydian and Aeolian raise the 7th (musica ficta: C♯ in D Dorian). In Phrygian the half step is already F→E from above, so the 7th stays a whole step."
    :final-whole "In second species the counterpoint's last note is a whole note, like the cantus."
    :melodic-dissonance "No augmented or diminished melodic intervals (the tritone, augmented 2nd, a chromatic step like F→F♯) and no 7ths."
    :leap-size "The octave is the largest leap allowed."
    :sixth "Fux allows the minor 6th upwards only; major 6ths and falling 6ths are avoided. Modern texts usually allow both."
    :leap-recovery "After a leap larger than a 4th, turn back in the opposite direction, preferably by step, to fill the gap."
    :leap-chain "Two leaps in the same direction should outline a consonant chord (e.g. D–F–A) and stay within an octave; three in a row are too many."
    :repeat "Repeating a note stalls the line. Fux tolerates it rarely; strict style avoids it."
    :tritone-outline "Even by step, a line whose turning points are a tritone apart (e.g. F up to B, then turning) has the tritone's harshness."
    :range "Each line should stay within a 10th, a comfortable singing range."
    :climax "The highest note should appear once, so the line has a clear peak. Returning to it weakens the shape."
    :leaps-many "A good line moves mostly by step. Flagged when more than a third of the moves are leaps of a 4th or more, or more than two thirds are 3rds or larger."
    :mode "Accidentals outside the mode, except the raised leading tone at the cadence. (Fux also writes B♭ in F Lydian to avoid the tritone F–B.)"
    :cf-final "The cantus firmus defines the mode: it begins and ends on the final (D in D Dorian)."
    :cf-ending "The cantus reaches its final by step, usually 2→1."
    :cf-length "Typical cantus firmi have 8 to 16 notes."}

   :fix
   {:consonance "Move the note a step up or down; one of its neighbours is usually a 3rd or 6th."
    :weak-dissonance-passing "Make the dissonance a passing tone: step into it and keep stepping the same way. Or use a consonance on the upbeat instead."
    :weak-dissonance "Step into the dissonance and step out of it (same direction for a passing tone, back for a neighbour), or make the upbeat consonant."
    :start "Open on the final or its 5th (above), or on the final (below)."
    :end "Make the last note the final, a unison or octave with the cantus."
    :imperfect-majority "Replace some 5ths and octaves in the middle with 3rds or 6ths, usually by moving the counterpoint a step."
    :unison "Move the note a step so the voices separate; a 3rd is usually one step away."
    :spacing "Bring the counterpoint closer by an octave, or change direction earlier so it doesn't drift away."
    :crossing "Keep the counterpoint on its side: move it an octave, or turn the line earlier."
    :overlap "Don't jump past the note the other voice just left; approach from your own side."
    :parallel-perfect "Change the direction of one voice: contrary motion into the 5th or octave is fine, or pick a 3rd/6th instead."
    :downbeat-parallels "Change one of the two downbeats to a 3rd or 6th; the upbeat can't hide the parallel."
    :contrary-perfect "Use a 3rd or 6th on one of the two notes instead of the 5th/octave."
    :direct-perfect "Approach the 5th or octave by contrary or oblique motion, or choose an imperfect consonance."
    :direct-perfect-leap "Approach the 5th or octave by step in the upper voice, or by contrary motion."
    :parallel-run "Break the chain: move in contrary motion once, or switch between 3rds and 6ths."
    :leap-together "Let one voice step while the other leaps, or make the leap in contrary motion."
    :static "Move the counterpoint while the cantus holds."
    :cadence-step "End with both voices stepping into the final from opposite sides: 7→1 against 2→1."
    :leading-tone "Raise the 7th in the voice that rises to the final (C♯ in D Dorian, F♯ in G Mixolydian, G♯ in A Aeolian)."
    :final-whole "Leave the last bar with a single note."
    :melodic-dissonance "Choose a consonant melodic interval (2nd, 3rd, 4th, 5th, rising minor 6th, octave), or get there in two moves."
    :leap-size "Split the leap into two smaller moves, or move to the other octave."
    :sixth "Use a smaller leap, or the rising minor 6th."
    :leap-recovery "After the big leap, step back in the opposite direction."
    :leap-chain "Make the two leaps outline a triad (3rd + 3rd, or 3rd + 4th), or put a step between them."
    :repeat "Move by step instead of repeating."
    :tritone-outline "Change a turning point by a step so the outline becomes a 4th or 5th."
    :range "Narrow the line: leap less, or turn back sooner."
    :climax "Lower one of the high points, or make one clearly higher."
    :leaps-many "Replace some leaps with steps; save leaps for where they help (after which you turn back)."
    :mode "Use the mode's own note; accidentals are only for the leading tone at the cadence."
    :cf-final "Start and end the cantus on the final."
    :cf-ending "Approach the final by step, usually from above (2→1)."
    :cf-length "Most cantus firmi are 8 to 16 notes long."}

   :v
   {:consonance (fn [_ v] (str (t/interval-name v) " (" (iv-long v) ") is a dissonance"
                               (when (= 3 (:g7 v)) "; the 4th is dissonant in two voices")))
    :start (fn [_ v above?] (str "Opens on " (t/interval-name v) "; use "
                                 (if above? "a unison, 5th or octave" "a unison or octave (counterpoint below)")))
    :end (fn [_ v] (str "Ends on " (t/interval-name v) "; end on a unison or octave"))
    :unison (fn [_ second?] (if second? "Unison on a downbeat in the middle" "Unison in the middle of the exercise"))
    :imperfect-majority (fn [_ p i] (str p " perfect vs. " i " imperfect consonances on the downbeats in the middle"))
    :spacing (fn [_ v] (str "The voices are " (a (iv-long v)) " apart"))
    :crossing "The upper voice is below the lower voice"
    :overlap (fn [_ up?] (if up?
                           "The lower voice moves above the upper voice's previous note"
                           "The upper voice moves below the lower voice's previous note"))
    :weak-dissonance (fn [_ v neighbor?]
                       (str (t/interval-name v) " on the upbeat "
                            (if neighbor?
                              "is a neighbour tone; strict style allows only passing tones"
                              "is not a passing tone (approach and leave it by step, same direction)")))
    :downbeat-parallels (fn [_ v w] (str (str/capitalize (perfects w)) " on successive downbeats " (pair v w)))
    :final-whole "The last bar should be a single whole note"
    :parallel-perfect (fn [_ v w] (str "Parallel " (perfects w) " " (pair v w)))
    :contrary-perfect (fn [_ v w] (str (str/capitalize (perfects w)) " by contrary motion " (pair v w)))
    :direct-perfect (fn [_ v w] (str "Direct " (perfects w) ": similar motion into a perfect consonance " (pair v w)))
    :direct-perfect-leap (fn [_ v w] (str "Direct " (perfects w) " with a leap in the upper voice " (pair v w)))
    :parallel-run (fn [_ n g7] (str n " parallel " (imperfects g7) " in a row"))
    :leap-together "Both voices leap in the same direction"
    :static "Neither voice moves"
    :cadence-step (fn [_ kind leap?]
                    (str "The final is reached by " (motions kind) " motion" (when leap? " with a leap")
                         "; both voices should step into it from opposite sides"))
    :leading-tone "No half step into the final; raise the 7th in the voice that rises to the final"
    :melodic-dissonance (fn [_ m] (str (if (zero? (:g m)) "Chromatic step" "Dissonant leap") ": " (iv-long m)))
    :leap-size (fn [_ m] (str "Leap of " (a (iv-long m))))
    :sixth (fn [_ m] (str (if (pos? (:dir m)) "Rising " "Falling ") (iv-long m)))
    :leap-recovery (fn [_ m] (str "Leap of " (a (iv-long m)) " not followed by a turn the other way"))
    :leap-chain (fn [_ iv] (str "Two leaps the same way outline " (a (iv-long iv))))
    :leap-chain-three "Three leaps in the same direction"
    :repeat "Repeated note"
    :tritone-outline (fn [loc x y] (str "The line outlines a tritone: " (note loc x) " to " (note loc y)))
    :range (fn [_ iv] (str "Range of " (a (iv-long iv))))
    :climax (fn [loc n k] (str "The highest note, " (note loc n) ", appears " k " times"))
    :leaps-many (fn [_ k n] (str k " of " n " moves are leaps"))
    :mode (fn [loc n k] (str (note loc n) " is outside " (key-name loc k)))
    :cf-final (fn [loc f] (str "The cantus should begin and end on " (fin loc f)))
    :cf-ending (fn [_ m] (str "The cantus reaches the final by " (a (iv-long m))))
    :cf-length (fn [_ n] (str "The cantus has " n " notes; 8–16 is typical"))}

   :tip
   {:cadence (fn [loc {:keys [from-above? target final where sixth? downbeat?]}]
               (str "Cadence: the cantus steps into the final from " (if from-above? "above" "below")
                    ", so the counterpoint steps in from the other side: " (pcn loc target) " → " (fin loc final)
                    " on " (case where :bar "this bar" :upbeat "this upbeat" "this bar's upbeat") ". "
                    (if sixth?
                      "That's a 6th opening to the octave."
                      "That's a 3rd closing to the unison (or a 10th to the octave).")
                    (when downbeat?
                      (str " On the downbeat a " (if sixth? "5th" "consonance a step away")
                           " works well (5–6–8 above, 5–3–1 below)."))))
    :opening (fn [loc above? f fifth second?]
               (str "Opening: "
                    (if above?
                      (str "a perfect consonance above the cantus: unison, 5th or octave (" (fin loc f) " or " (pcn loc fifth) ").")
                      (str "the final " (fin loc f) " in unison or an octave below; a 5th below would imply another mode."))
                    (when second? " In second species it's common to leave the downbeat empty (a half rest, → skips it) and enter on the upbeat.")))
    :motion (fn [loc d before now]
              (str "The cantus " (case d 1 "rises" -1 "falls" "repeats") " into this bar ("
                   (pcn loc before) " → " (pcn loc now) "). "
                   (case d
                     1 "Moving down here gives contrary motion, the safest way to avoid parallels."
                     -1 "Moving up here gives contrary motion, the safest way to avoid parallels."
                     "Whatever you do is oblique motion, which is always safe.")))
    :leap (fn [_ m] (str "You just leapt " (a (iv-long m)) (if (pos? (:dir m)) " up" " down")
                         "; turn back " (if (pos? (:dir m)) "down" "up") ", ideally by step."))
    :chain (fn [_ n g7] (str n " parallel " (imperfects g7) " in a row already: break the chain here with contrary or oblique motion."))
    :climax (fn [loc top bar] (str "You're near your high point, " (note loc top) " (bar " bar "). Keep the climax unique: go clearly above it or stay below."))
    :balance (fn [_ p i] (str "So far " p " perfect vs. " i " imperfect consonances in the middle; a 3rd or 6th here would help."))
    :no-cantus "Start with a cantus firmus: pick one of Fux's at the bottom of the page, or write your own on the lower staff (Tab switches voice)."
    :no-selection "Select a note or an empty bar to get hints for it."
    :cantus "The cantus: begin and end on the final, move mostly by step, one clear high point, and reach the final by step (usually 2→1)."
    :select-cp "Select a counterpoint bar for hints about the counterpoint."
    :past-end "This bar is past the end of the cantus."
    :last (fn [loc f above? second?]
            (str "Last note: the final, " (fin loc f) ", in unison or an octave " (if above? "above" "below") " the cantus."
                 (when second? " A whole note: leave the upbeat empty.")))
    :upbeat "Upbeat: a consonance (step or leap), or a dissonance that passes by step between two downbeats."
    :downbeat "Downbeat: it must be consonant, and check the upbeat before it: 5th→5th or 8ve→8ve is still a parallel."}

   :nav {:write "Write" :hints "Hints" :cheat "Cheat sheet" :rules "Rules"}

   :ui
   {:loading-sounds "loading sounds…"
    :language "Language"
    :mode "Mode"
    :counterpoint "Counterpoint"
    :cantus "Cantus"
    :above "above the cantus"
    :below "below"
    :species "Species"
    :species-1 "1st · 1:1" :species-2 "2nd · 2:1" :species-3 "3rd · 4:1" :species-4 "4th" :species-5 "florid"
    :click-staff "Click the staff to place a note"
    :tool-up "Up a step (↑, ⇧↑ octave)"
    :tool-down "Down a step (↓, ⇧↓ octave)"
    :tool-sharp "Sharpen (#)"
    :tool-flat "Flatten (-)"
    :tool-natural "Natural (n)"
    :tool-delete "Delete (⌫)"
    :tool-voice "Switch voice (Tab)"
    :tool-undo "Undo (⌘Z)"
    :tool-redo "Redo (⇧⌘Z)"
    :stop "■ Stop"
    :play "▶ Play"
    :tempo "Tempo"
    :legend [:<> "Between the staves: interval numbers, and the motion into each bar: "
             [:b "c"] "ontrary, " [:b "o"] "blique, " [:b "s"] "imilar, " [:b "p"] "arallel."]
    :type-notes "Type or paste notes"
    :cantus-upper "Cantus (upper)" :cantus-lower "Cantus (lower)"
    :cp-upper "Counterpoint (upper)" :cp-lower "Counterpoint (lower)"
    :typing-second [:<> "Second species: two half notes per bar; start with " [:code "-"] " for a half rest, and end on a single note (the whole note). " [:code "|"] " between bars is optional."]
    :typing-notes [:<> "Letters with optional ♯/♭ (# or b) and octave: " [:code "D F E D"] " or " [:code "D4 C#5 Bb3"]
                   ". Without an octave a note goes to the nearest one. " [:code "-"] " leaves a bar empty; "
                   [:code "n"] " forces a natural (" [:code "Bn4"] ")."]
    :typing-keys [:<> "Keys: " [:kbd "A"] "–" [:kbd "G"] " enter a note and move on, " [:kbd "↑"] [:kbd "↓"]
                  " move it (⇧ by octave), " [:kbd "#"] " " [:kbd "-"] " " [:kbd "n"] " accidentals, "
                  [:kbd "←"] [:kbd "→"] " move, " [:kbd "Tab"] " other voice, " [:kbd "⌫"] " delete, "
                  [:kbd "Space"] " play, " [:kbd "⌘Z"] " undo."]
    :cp-tag "CP" :cf-tag "CF"
    :try "Try: "
    :mute "mute"
    :mute-title "Turn this rule off"
    :rule-check "Rule check"
    :start-with-cf "Start with a cantus firmus (bottom of the page has Fux's)."
    :progress (fn [_ k n] (str "Counterpoint: " k " of " n " notes"))
    :complete (fn [_ n] (str "Complete · " n " bars"))
    :penalty "penalty"
    :no-violations "No rule violations."
    :results-hint (fn [_ e w] (str "Errors cost " e ", warnings " w ". Nothing is blocked; tap a row to highlight it on the staff."))
    :no-rule-broken "No rule broken here"
    :hints "Hints"
    :hints-cp "counterpoint · "
    :hints-cf "cantus · "
    :clean-notes "Notes that break no rules here"
    :hide "hide"
    :no-clean "None: every note here breaks something, often because of a neighbouring note. Try changing the notes around it."
    :warned-notes "Only style warnings (hover for why)"
    :cands-hint "Numbers are the interval with the other voice. Judged against the notes already written on both sides."
    :show-cands "Show notes that work here"
    :shape "Shape"
    :shape-sub "descriptive, not scored"
    :motion "Motion"
    :intervals "Intervals"
    :imperfect "imperfect" :perfect "perfect" :dissonant "dissonant"
    :cp-melody "Counterpoint melody"
    :steps "steps" :thirds "3rds" :leaps "leaps" :repeats "repeats"
    :shape-hint "Rough targets: contrary motion is the most common, imperfect consonances outnumber perfect ones, and the line moves mostly by step."
    :cantus-firmi "Cantus firmi"
    :example (fn [loc species k] (str "Example: " (case species 1 "1st" 2 "2nd" (str species "th")) " species · " (key-name loc k)))
    :library-hint "Loading a cantus replaces the exercise (undo brings it back) and clears the counterpoint, keeping the species and position."
    :saved "Saved"
    :delete "delete"
    :name-exercise "Name this exercise"
    :save "Save"
    :rule-set "Rule set"
    :rule-set-hint "Strict follows Fux closely. Relaxed keeps the core rules and drops some style points. Turning single rules on or off makes it Custom."
    :check-cf "Check the cantus firmus too"
    :check-cf-hint "Melodic rules apply to the cantus as well, useful when you write your own."
    :strict-only "strict only"
    :relaxed-only "relaxed only"
    :second-species "2nd species"
    :settings "Settings"
    :hear "When placing a note, play"
    :hear-both "both notes" :hear-note "the note" :hear-off "nothing"
    :note-names "Note names"
    :labels-toggle "Interval numbers and motion between the staves"
    :hints-toggle "Hints panel on the Write tab"
    :hints-toggle-hint "Advice for the selected bar, and on request the notes that break no rules there."
    :midi-toggle "MIDI keyboard input"
    :midi-connected (fn [_ inputs] (str "Connected: " (str/join ", " inputs) ". Notes go to the selected bar."))
    :midi-none "No MIDI devices found."
    :midi-unsupported "This browser has no Web MIDI."
    :midi-denied "MIDI access was denied."
    :midi-off "Played notes enter the selected bar and move on."}

   :cheat
   {:idea-title "The idea"
    :idea [:<>
           [:p "A " [:b "cantus firmus"] " (CF, “fixed song”) is a plain melody in whole notes. You write a second line, the "
            [:b "counterpoint"] " (CP), against it. The goal is two lines that are each good melodies and stay independent of each other."]
           [:p "Fux (" [:i "Gradus ad Parnassum"] ", 1725) teaches it in five " [:b "species"] ", each adding rhythm (1st and 2nd are implemented):"]
           [:ol
            [:li [:b "1st, note against note (1:1)"] ": only consonances."]
            [:li [:b "2nd (2:1)"] ": two half notes per CF note; the second may be a passing dissonance."]
            [:li [:b "3rd (4:1)"] ": four quarters; passing and neighbour tones, the cambiata."]
            [:li [:b "4th, syncopation"] ": tied notes, suspensions that resolve down by step."]
            [:li [:b "5th, florid"] ": all of the above mixed."]]]
    :intervals-title "Intervals"
    :intervals [:p "Count letters, both ends included: C up to E is a 3rd (C, D, E). The quality (perfect, major, minor, augmented, diminished) comes from the semitones. Beyond the octave they are " [:b "compound"] ": a 10th is a 3rd plus an octave and is treated like a 3rd."]
    :th-name "Name" :th-semitones "Semitones" :th-eg "e.g." :th-class "In two voices"
    :tritone "tritone"
    :class-perfect "perfect consonance" :class-imperfect "imperfect consonance" :class-dissonant "dissonance"
    :consonance-title "Consonance and dissonance"
    :consonance [:dl
                 [:dt [:span.cls.perfect "Perfect consonances"] " unison, 5th, octave (and 12th, 15th…)"]
                 [:dd "Stable and hollow. They frame the exercise (start and end) but too many in a row sound empty, and two of the same kind in parallel are forbidden."]
                 [:dt [:span.cls.imperfect "Imperfect consonances"] " 3rds and 6ths, major or minor (and 10ths…)"]
                 [:dd "Sweet and mobile. They should be the majority, and you can move between them freely, even in parallel (up to three in a row)."]
                 [:dt [:span.cls.dissonant "Dissonances"] " 2nds, 7ths, the 4th, the tritone, any augmented or diminished interval"]
                 [:dd "Not allowed in first species. Later species let them in only in controlled ways (passing tones, suspensions). The 4th is consonant between upper voices in fuller textures, but dissonant against the bass, and with two voices one of them is the bass."]]
    :motion-title "Motion between two voices"
    :motion-intro "How the voices move from one interval to the next. The numbers between the staves are the intervals."
    :motion-contrary [:span [:b "Contrary"] ": opposite directions. The best for independence."]
    :motion-oblique [:span [:b "Oblique"] ": one voice holds, the other moves."]
    :motion-similar [:span [:b "Similar"] ": same direction, different intervals."]
    :motion-parallel [:span [:b "Parallel"] ": same direction, same interval. Fine for 3rds and 6ths…"]
    :motion-parallel-5 [:span "…but " [:b "parallel 5ths"] " (and octaves) are forbidden: the voices fuse into one."]
    :motion-direct [:span [:b "Direct (hidden) 5th"] ": " [:i "similar"] " motion into a 5th or octave (here 3→5, both rising). Strict style avoids it; modern texts allow it when the upper voice moves by step."]
    :motion-rule "Rule of thumb: approach perfect consonances by contrary or oblique motion; use similar and parallel motion only into imperfect ones."
    :cadence-title "The cadence"
    :cadence-above [:span "Counterpoint above: 7→1 against 2→1, a major 6th opening to an octave. The C♯ is the raised leading tone."]
    :cadence-below [:span "Counterpoint below: a minor 3rd closing to a unison."]
    :cadence [:p "Both voices step into the final from opposite sides, and one of them by a half step. In modes without a half step below the final (Dorian, Mixolydian, Aeolian) the 7th is raised: this is " [:b "musica ficta"] ", an accidental singers added by convention. Phrygian already has the half step above (F→E), so its 7th stays."]
    :second-title "Second species (2:1)"
    :second [:<>
             [:p "Two half notes against each cantus note. The first half is the " [:b "downbeat"] " (strong), the second the " [:b "upbeat"] " (weak). Downbeats follow the first-species rules: consonant, and no parallel 5ths or octaves into them."]
             [:p "The upbeat is where the new freedom is. A consonant upbeat may move by step or leap; a " [:b "dissonant"] " upbeat must be a " [:b "passing tone"] ": it fills in a 3rd by step, continuing in the same direction."]]
    :second-passing [:span [:b "Passing tone"] ": E–F–G over C, the 4th on the upbeat is fine because it passes by step between two consonances."]
    :second-neighbour [:span [:b "Neighbour tone"] ": step away and back. Fux doesn't allow it in second species; the Relaxed preset does."]
    :second-leap [:span [:b "Consonant leap"] ": a 3rd down on the upbeat, from one consonance to another. Leaps are only possible between consonances."]
    :second-parallels [:span [:b "5ths on successive downbeats"] ": 5 – 3 – 5 still sounds like parallel 5ths; the upbeat is too short to hide them."]
    :second-habits [:p "Other habits: start with a half rest (the counterpoint enters on the upbeat, with a perfect consonance), avoid repeating a note, and end on a whole note. A unison is allowed on an upbeat but not on a downbeat in the middle. The cadence is usually 5–6–8 above (B–C♯–D over E–D in D Dorian), or 5–3–1 below."]
    :second-cadence [:span "The second-species cadence above: 5th, 6th (raised leading tone), octave."]
    :terms-title "Other terms"
    :terms [:dl
            [:dt "Mode, final"] [:dd "The scale. The " [:b "final"] " is its home note: D for Dorian, E Phrygian, F Lydian, G Mixolydian, A Aeolian, C Ionian on the white keys. The cantus starts and ends on it. Ionian and Aeolian are today's major and natural minor."]
            [:dt "Step, skip, leap"] [:dd "A 2nd is a step; a 3rd is a skip; a 4th or more is a leap. Lines should move mostly by step."]
            [:dt "Leap recovery"] [:dd "After a large leap (more than a 4th), turn around, ideally by step, to fill in the gap."]
            [:dt "Climax"] [:dd "The highest note of a line. Ideally it occurs once, so the melody has one clear peak."]
            [:dt "Voice crossing / overlap"] [:dd "Crossing: the upper voice goes below the lower one. Overlap: a voice moves past the note the other one just left. Both blur the two lines."]
            [:dt "Antiparallels"] [:dd "5th to 12th or octave to 15th by contrary motion. The same perfect interval twice, just disguised."]
            [:dt "Tritone"] [:dd "Three whole steps (F–B), the “devil in music”. Avoided as a leap, and as the outline of a phrase."]]
    :rules-title "Rules checked"
    :rules-hint [:p.hint "Errors are the core rules every textbook agrees on. Warnings are style points that differ between books; " [:i "strict only"] " ones are Fux's stricter preferences."]
    :about-title "About the score"
    :about (fn [_ e w]
             [:<>
              [:p "There is no objective measure of musical quality. What exists, going back to computer counterpoint programs like Schottstaedt's (1984), is a " [:b "penalty"] ": each broken rule adds points (" e " per error, " w " per warning here). Lower is closer to the style, not necessarily more beautiful."]
              [:p "The Shape panel shows proportions teachers look at (contrary motion, imperfect consonances, steps). Compare them across your attempts at the same cantus, and play both lines: your ear is the final judge."]])}

   :guide
   {:work-title "A way to work"
    :work [:ol.steps
           [:li [:b "Get to know the cantus."] " Play it, sing it. Find its high point and how it reaches the final (from above or below)."]
           [:li [:b "Write the cadence first."] " The last two bars are nearly fixed: both voices step into the final from opposite sides, one by a half step (6→8 or 3→1). The Write tab's hints name the exact notes."]
           [:li [:b "Then the opening."] " Unison, 5th or octave above; unison or octave below."]
           [:li [:b "Sketch the middle in 3rds and 6ths, moving against the cantus."] " When it goes up, go down. That one habit avoids most parallels and direct 5ths."]
           [:li [:b "Shape your own line."] " Put your climax somewhere other than the cantus's. Move mostly by step; after a big leap, turn back."]
           [:li [:b "Check, then listen."] " Fix the errors first (red). Warnings (amber) are style; if it sounds good, a warning can stay."]
           [:li [:b "Do it again."] " The same cantus with the counterpoint below, then in another mode. Each one is a different puzzle."]]
    :first-title "First species (1:1)"
    :first [:ul
            [:li "Think in " [:b "3rds and 6ths"] ". Most good solutions are 3rds and 6ths with a few 5ths and octaves, often where the voices cross the middle of their ranges."]
            [:li [:b "Contrary motion first"] ", oblique next (when the cantus repeats), similar and parallel only into 3rds and 6ths."]
            [:li "Two or three parallel 3rds or 6ths are fine and sound sweet; then break the chain."]
            [:li "Get into a 5th or octave by " [:b "contrary motion"] ", e.g. from a 6th to an octave or a 3rd to a 5th with the voices moving apart."]
            [:li "Swapping between a 3rd and a 6th (or 10th) is an easy way to keep moving: the counterpoint leaps a 4th or 5th while the cantus steps the other way."]
            [:li "Stuck in one bar? Try the note a step above and a step below the obvious one before changing the bars around it."]]
    :second-title "Second species (2:1)"
    :second [:ul
             [:li [:b "Plan the downbeats like first species"] " (consonant, good motion between them), then fill in the upbeats."]
             [:li "The upbeat gives you three tools: a " [:b "passing tone"] " (step through, even as a dissonance), a " [:b "consonant leap"] " (to change register or set up the next downbeat), and a " [:b "consonant step"] "."]
             [:li "If two downbeats are a 3rd apart melodically, a passing tone between them almost always works."]
             [:li "A leap on the upbeat is a good way to escape a parallel: it changes the approach to the next downbeat."]
             [:li "Check the motion " [:b "from the upbeat to the next downbeat"] " for parallels and direct 5ths, and downbeat to downbeat for 5–5 or 8–8."]
             [:li "Start with a half rest and enter on the upbeat; end with 5–6–8 above (or 5–3–1 below)."]
             [:li "Avoid repeated notes; and don't let the line zig-zag up and down a 3rd for long."]]
    :fixes-title "Common problems and fixes"
    :fixes-hint "The same advice appears under a violation when you tap it in the Write tab."
    :habits-title "Practice habits"
    :habits [:ul
             [:li [:b "Sing."] " Sing the cantus while playing the counterpoint, then swap. If a line is awkward to sing, it's probably awkward."]
             [:li [:b "Listen to each voice alone"] " (the Cantus / Counterpoint buttons), then together."]
             [:li [:b "Keep several attempts"] " at the same cantus (save each) and compare their Shape panels."]
             [:li [:b "Use Relaxed while learning,"] " Strict once the core rules are second nature."]
             [:li [:b "“Show notes that work here”"] " (on the Write tab) is a crutch: use it when stuck, then ask yourself why the others fail."]]
    :reading-title "Further reading"
    :reading [:ul
              [:li "Johann Joseph Fux, " [:i "The Study of Counterpoint"] " (Gradus ad Parnassum, tr. Alfred Mann): the source, written as a dialogue between teacher and student. Short and readable."]
              [:li "Knud Jeppesen, " [:i "Counterpoint: The Polyphonic Vocal Style of the Sixteenth Century"] ": species counterpoint grounded in Palestrina's actual practice."]
              [:li "Felix Salzer & Carl Schachter, " [:i "Counterpoint in Composition"] ": species as a way into tonal music, with many examples from the repertoire."]
              [:li "Peter Schubert, " [:i "Modal Counterpoint, Renaissance Style"] ": a modern, practical textbook."]]}})
