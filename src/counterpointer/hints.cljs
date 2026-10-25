(ns counterpointer.hints
  (:require [counterpointer.theory :as t]
            [counterpointer.rules :as r]))

;; Dynamic help for the selected note: positional tips, and which candidate notes break no rules.

(def fixes
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
   :cf-ending "Approach the final by step, usually from above (2→1)."})

;; ---------------------------------------------------------------- helpers

(defn- above? [e] (not= (:position e) :below))

(defn- per [e] (r/per-bar (:species e)))

(defn- leading-tone [key]
  (let [l (mod (dec (get-in key [:final :l])) 7)
        d (mod (- (dec (t/final-pc key)) (t/natural-pc l)) 12)]
    {:s l :a (if (> d 6) (- d 12) d)}))

(defn- pc-name [{:keys [s a]}] (str (t/letters (mod s 7)) (t/acc-sym a)))

(defn- degree-note [key deg]
  (let [l (mod (+ (get-in key [:final :l]) deg) 7)]
    {:s l :a (t/default-alt key l)}))

(defn- prev-notes
  "Present notes before `col` in `line`, latest first, as [i note]."
  [line col]
  (filter second (map (fn [i] [i (get line i)]) (range (dec col) -1 -1))))

(defn- direction-word [d] (case d 1 "rises" -1 "falls" "repeats"))

;; ---------------------------------------------------------------- tips

(defn- cadence-tip [{:keys [cf key] :as e} beat p]
  (let [n (count cf)
        pen (get cf (- n 2))
        fin (get cf (dec n))]
    (when (and pen fin (= 1 (:g (t/interval pen fin))))
      (let [cf-from-above? (> (t/midi pen) (t/midi fin))
            target (if cf-from-above?
                     (if (= :phrygian (:mode key)) (degree-note key 6) (leading-tone key))
                     (degree-note key 1))
            sixth? (= (above? e) cf-from-above?)
            where (if (> p 1) (if (zero? beat) "this bar's upbeat" "this upbeat") "this bar")]
        (str "Cadence: the cantus steps into the final from " (if cf-from-above? "above" "below")
             ", so the counterpoint steps in from the other side: " (pc-name target) " → " (t/final-name (:final key))
             " on " where ". "
             (if sixth?
               "That's a 6th opening to the octave."
               "That's a 3rd closing to the unison (or a 10th to the octave).")
             (when (and (> p 1) (zero? beat))
               (str " On the downbeat a "
                    (if sixth? "5th" "consonance a step away")
                    " works well (5–6–8 above, 5–3–1 below).")))))))

(defn- opening-tip [e p]
  (let [key (:key e)
        f (t/final-name (:final key))
        fifth (pc-name (degree-note key 4))]
    (str "Opening: "
         (if (above? e)
           (str "a perfect consonance above the cantus: unison, 5th or octave (" f " or " fifth ").")
           (str "the final " f " in unison or an octave below; a 5th below would imply another mode."))
         (when (> p 1) " In second species it's common to leave the downbeat empty (a half rest, → skips it) and enter on the upbeat."))))

(defn- motion-tip [{:keys [cf]} b]
  (when-let [[now before] (and (pos? b) [(get cf b) (get cf (dec b))])]
    (when (and now before)
      (let [d (compare (t/midi now) (t/midi before))]
        (str "The cantus " (direction-word d) " into this bar (" (pc-name before) " → " (pc-name now) "). "
             (case d
               1 "Moving down here gives contrary motion, the safest way to avoid parallels."
               -1 "Moving up here gives contrary motion, the safest way to avoid parallels."
               "Whatever you do is oblique motion, which is always safe."))))))

(defn- leap-tip [cp col]
  (let [[[_ y] [_ x]] (prev-notes cp col)]
    (when (and x y)
      (let [m (t/move x y)]
        (when (>= (:g m) 4)
          (str "You just leapt " (t/with-article (t/interval-long m)) (if (pos? (:dir m)) " up" " down")
               "; turn back " (if (pos? (:dir m)) "down" "up") ", ideally by step."))))))

(defn- chain-tip [e col]
  (let [{:keys [motions]} (r/context e)
        before (reverse (filter #(< (:i %) col) motions))
        run (take-while #(and (= :parallel (:kind %)) (= :imperfect (:cons (:a %)))
                              (= (:g7 (:a %)) (:g7 (:a (first before)))))
                        before)]
    (when (and (>= (count run) 2) (= (dec col) (inc (:i (first before)))))
      (str (inc (count run)) " parallel " (if (= 2 (:g7 (:a (first run)))) "3rds" "6ths")
           " in a row already: break the chain here with contrary or oblique motion."))))

(defn- climax-tip [cp col]
  (let [ns (keep-indexed #(when (and %2 (not= %1 col)) [%1 %2]) cp)
        [_ prev] (first (prev-notes cp col))]
    (when (and prev (>= (count ns) 3))
      (let [[i top] (apply max-key #(t/midi (second %)) ns)]
        (when (and (<= (- (:s top) (:s prev)) 1) (not= top prev))
          (str "You're near your high point, " (t/note-name top) " (bar " (inc i) "). Keep the climax unique: go clearly above it or stay below."))))))

(defn- balance-tip [e col]
  (let [{:keys [verts n]} (r/context e)
        vs (filter #(and % (zero? (:beat %)) (< 0 (:bar %) (dec n)) (< (:i %) col)) verts)
        perf (count (filter #(= :perfect (:cons %)) vs))
        imp (count (filter #(= :imperfect (:cons %)) vs))]
    (when (and (>= (+ perf imp) 3) (>= perf imp))
      (str "So far " perf " perfect vs. " imp " imperfect consonances in the middle; a 3rd or 6th here would help."))))

(defn tips
  "Short advice for the selected bar, most specific first."
  [{:keys [cf cp] :as e} {:keys [voice col] :as sel}]
  (let [n (count cf)
        p (per e)]
    (cond
      (zero? n)
      ["Start with a cantus firmus: pick one of Fux's at the bottom of the page, or write your own on the lower staff (Tab switches voice)."]

      (nil? sel) ["Select a note or an empty bar to get hints for it."]

      (= voice :cf)
      ["The cantus: begin and end on the final, move mostly by step, one clear high point, and reach the final by step (usually 2→1)."
       "Select a counterpoint bar for hints about the counterpoint."]

      :else
      (let [b (quot col p)
            beat (mod col p)
            final-slot (* p (dec n))]
        (->> [(when (>= b n) "This bar is past the end of the cantus.")
              (when (and (zero? b) (< col final-slot)) (opening-tip e p))
              (when (= col final-slot)
                (str "Last note: the final, " (t/final-name (:final (:key e))) ", in unison or an octave "
                     (if (above? e) "above" "below") " the cantus."
                     (when (> p 1) " A whole note: leave the upbeat empty.")))
              (when (and (= b (- n 2)) (> n 2)) (cadence-tip e beat p))
              (when (and (pos? beat) (< b (dec n)) (not= b (- n 2)))
                "Upbeat: a consonance (step or leap), or a dissonance that passes by step between two downbeats.")
              (when (and (zero? beat) (< 0 b (- n 2))) (motion-tip e b))
              (when (and (> p 1) (zero? beat) (< 0 b (dec n)))
                "Downbeat: it must be consonant, and check the upbeat before it: 5th→5th or 8ve→8ve is still a parallel.")
              (leap-tip cp col)
              (chain-tip e col)
              (climax-tip cp col)
              (when (zero? beat) (balance-tip e col))]
             (remove nil?)
             vec)))))

;; ---------------------------------------------------------------- candidates

(defn- touching
  "Violations that involve `col` of `voice`."
  [vs voice col p]
  (filter (fn [v]
            (and (not= :info (:level v))
                 (if (= voice :cf)
                   (if (= :cf (:voice v))
                     (some #{col} (:cols v))
                     (and (nil? (:voice v)) (some #(= col (quot % p)) (:cols v))))
                   (and (not= :cf (:voice v)) (some #{col} (:cols v))))))
          vs))

(defn- candidate-notes [{:keys [cf cp key] :as e} voice col]
  (let [p (per e)
        n (count cf)
        partner (if (= voice :cp) (get cf (quot col p)) (get cp (* col p)))
        line (get e voice)
        [_ prev] (first (prev-notes line col))
        upper? (= (= voice :cp) (above? e))
        steps (cond
                partner (if upper?
                          (range (:s partner) (+ (:s partner) 12))
                          (range (- (:s partner) 11) (inc (:s partner))))
                prev (range (- (:s prev) 6) (+ (:s prev) 7))
                :else (range (if upper? 27 20) (if upper? 36 29)))
        lt-col (if (= voice :cp) (dec (* p (dec n))) (- n 2))]
    (for [s steps
          :let [base {:s s :a (t/default-alt key (mod s 7))}
                raised (update base :a inc)]
          note (if (and (= col lt-col) (not (t/leading-tone? key base)) (t/leading-tone? key raised))
                 [base raised]
                 [base])]
      note)))

(defn- with-final
  "Before the last note is written, judge the penultimate one as if the counterpoint ended on the nearest final."
  [{:keys [cf key] :as e} voice col note]
  (let [final-slot (* (per e) (dec (count cf)))]
    (if (and (= voice :cp) (= col (dec final-slot)) (nil? (get-in e [:cp final-slot])))
      (update e :cp t/put final-slot (t/nearest key (get-in key [:final :l]) (:s note)))
      e)))

(defn candidates
  "Every note tried in the selected bar, with the violations it would cause there; cleanest and closest first."
  [e {:keys [voice col]} enabled check-cf?]
  (let [p (per e)
        line (get e voice)
        [_ prev] (first (prev-notes line col))]
    (->> (candidate-notes e voice col)
         (map (fn [note]
                (let [e' (with-final (update e voice t/put col note) voice col note)
                      vs (touching (r/analyze e' enabled check-cf?) voice col p)]
                  {:note note
                   :errors (count (filter #(= :error (:level %)) vs))
                   :warnings (count (filter #(= :warning (:level %)) vs))
                   :msgs (mapv :msg vs)
                   :distance (if prev (Math/abs (- (:s note) (:s prev))) 0)})))
         (sort-by (juxt :errors :warnings :distance)))))

(defn interval-to-partner [{:keys [cf cp] :as e} voice col note]
  (let [p (per e)
        partner (if (= voice :cp) (get cf (quot col p)) (get cp (* col p)))]
    (when partner (:num (t/interval note partner)))))
