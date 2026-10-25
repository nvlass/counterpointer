(ns counterpointer.rules
  (:require [clojure.string :as str]
            [counterpointer.theory :as t]))

;; Every rule is checked; presets only decide which ones are shown and counted.
;; :kind decides where the score marks it: :vertical (interval labels), :motion (between columns), :melodic (notes).

(def rules
  [{:id :consonance :group :harmony :level :error :kind :vertical :presets #{:strict :relaxed}
    :title "Only consonances"
    :desc "Every vertical interval must be a consonance: unison, 3rd, 5th, 6th, octave (or compounds like 10th, 12th). The 4th counts as a dissonance in two voices."}
   {:id :start :group :harmony :level :error :kind :vertical :presets #{:strict :relaxed}
    :title "Open on a perfect consonance"
    :desc "Start on a unison, 5th or octave when the counterpoint is above; unison or octave when it is below (a 5th below the cantus would suggest a different final)."}
   {:id :end :group :harmony :level :error :kind :vertical :presets #{:strict :relaxed}
    :title "Close on a unison or octave"
    :desc "The last interval must be a unison or an octave, both voices on the final."}
   {:id :imperfect-majority :group :harmony :level :warning :kind :vertical :presets #{:strict :relaxed}
    :title "Mostly imperfect consonances"
    :desc "In the middle, 3rds and 6ths should outnumber 5ths and octaves. Perfect consonances sound hollow and stop the motion; imperfect ones keep it going."}
   {:id :unison :group :harmony :level :warning :kind :vertical :presets #{:strict}
    :title "No unison in the middle"
    :desc "Fux allows the unison only on the first and last note: in the middle the two voices merge into one."}
   {:id :spacing :group :harmony :level :warning :kind :vertical :presets #{:strict :relaxed}
    :title "Voices within a 10th"
    :desc "Keep the voices no more than a 10th apart. A 12th is tolerated now and then; wider and they stop sounding like a pair."}
   {:id :crossing :group :harmony :level :warning :kind :vertical :presets #{:strict :relaxed}
    :title "No voice crossing"
    :desc "The upper voice should stay above the lower one. Crossing hides which voice is which."}
   {:id :overlap :group :motion :level :warning :kind :motion :presets #{:strict}
    :title "No voice overlap"
    :desc "A voice should not move past the note the other voice just left (e.g. the lower voice rising above the upper voice's previous note)."}

   {:id :parallel-perfect :group :motion :level :error :kind :motion :presets #{:strict :relaxed}
    :title "No parallel 5ths or octaves"
    :desc "Two perfect consonances of the same kind in a row, in the same direction (5→5, 8→8, 1→8, 5→12): the voices lose their independence. The central rule of counterpoint."}
   {:id :contrary-perfect :group :motion :level :warning :kind :motion :presets #{:strict :relaxed}
    :title "No 5ths/octaves by contrary motion"
    :desc "5→12 or 8→15 in contrary motion ('antiparallels') still moves between the same perfect consonance; treated as a disguised parallel."}
   {:id :direct-perfect :group :motion :level :warning :kind :motion :presets #{:strict}
    :title "No direct (hidden) 5ths or octaves"
    :desc "Approaching a 5th or octave by similar motion (both voices moving the same way). Strict style avoids it altogether."}
   {:id :direct-perfect-leap :group :motion :level :warning :kind :motion :presets #{:relaxed}
    :title "Direct 5ths/octaves only with a step on top"
    :desc "The modern version of the previous rule: similar motion into a 5th or octave is fine if the upper voice moves by step, but not when it leaps."}
   {:id :parallel-run :group :motion :level :warning :kind :motion :presets #{:strict :relaxed}
    :title "At most 3 parallel 3rds or 6ths in a row"
    :desc "Parallel imperfect consonances are allowed, but a long chain of them makes the counterpoint a mere shadow of the cantus."}
   {:id :leap-together :group :motion :level :warning :kind :motion :presets #{:strict}
    :title "Don't leap together"
    :desc "Both voices leaping (a 4th or more) in the same direction is heavy and undermines independence."}
   {:id :static :group :motion :level :warning :kind :motion :presets #{:strict :relaxed}
    :title "Some voice should move"
    :desc "Both voices repeating their notes means nothing happens."}

   {:id :cadence-step :group :cadence :level :error :kind :motion :presets #{:strict :relaxed}
    :title "Approach the final by step, in contrary motion"
    :desc "The cadence: one voice goes 2→1, the other 7→1, so the penultimate interval is a 6th resolving to an octave, or a 3rd resolving to a unison."}
   {:id :leading-tone :group :cadence :level :error :kind :motion :presets #{:strict :relaxed}
    :title "Half step into the final"
    :desc "One of the two voices must arrive at the final by a half step. In Dorian, Mixolydian and Aeolian raise the 7th (musica ficta: C♯ in D Dorian). In Phrygian the half step is already F→E from above, so the 7th stays a whole step."}

   {:id :melodic-dissonance :group :melody :level :error :kind :melodic :presets #{:strict :relaxed}
    :title "No dissonant leaps"
    :desc "No augmented or diminished melodic intervals (the tritone, augmented 2nd, a chromatic step like F→F♯) and no 7ths."}
   {:id :leap-size :group :melody :level :error :kind :melodic :presets #{:strict :relaxed}
    :title "No leaps beyond an octave"
    :desc "The octave is the largest leap allowed."}
   {:id :sixth :group :melody :level :warning :kind :melodic :presets #{:strict}
    :title "Only the rising minor 6th"
    :desc "Fux allows the minor 6th upwards only; major 6ths and falling 6ths are avoided. Modern texts usually allow both."}
   {:id :leap-recovery :group :melody :level :warning :kind :melodic :presets #{:strict :relaxed}
    :title "Recover large leaps"
    :desc "After a leap larger than a 4th, turn back in the opposite direction, preferably by step, to fill the gap."}
   {:id :leap-chain :group :melody :level :warning :kind :melodic :presets #{:strict :relaxed}
    :title "Careful with successive leaps"
    :desc "Two leaps in the same direction should outline a consonant chord (e.g. D–F–A) and stay within an octave; three in a row are too many."}
   {:id :repeat :group :melody :level :warning :kind :melodic :presets #{:strict}
    :title "Avoid repeated notes"
    :desc "Repeating a note stalls the line. Fux tolerates it rarely; strict style avoids it."}
   {:id :tritone-outline :group :melody :level :warning :kind :melodic :presets #{:strict}
    :title "Don't outline a tritone"
    :desc "Even by step, a line whose turning points are a tritone apart (e.g. F up to B, then turning) has the tritone's harshness."}
   {:id :range :group :melody :level :warning :kind :melodic :presets #{:strict :relaxed}
    :title "Range within a 10th"
    :desc "Each line should stay within a 10th, a comfortable singing range."}
   {:id :climax :group :melody :level :warning :kind :melodic :presets #{:strict :relaxed}
    :title "A single climax"
    :desc "The highest note should appear once, so the line has a clear peak. Returning to it weakens the shape."}
   {:id :leaps-many :group :melody :level :warning :kind :melodic :presets #{:strict}
    :title "Mostly stepwise"
    :desc "A good line moves mostly by step. Flagged when more than a third of the moves are leaps of a 4th or more, or more than two thirds are 3rds or larger."}
   {:id :mode :group :melody :level :warning :kind :melodic :presets #{:strict :relaxed}
    :title "Stay in the mode"
    :desc "Accidentals outside the mode, except the raised leading tone at the cadence. (Fux also writes B♭ in F Lydian to avoid the tritone F–B.)"}

   {:id :cf-final :group :cf :level :error :kind :melodic :presets #{:strict :relaxed}
    :title "Cantus starts and ends on the final"
    :desc "The cantus firmus defines the mode: it begins and ends on the final (D in D Dorian)."}
   {:id :cf-ending :group :cf :level :error :kind :melodic :presets #{:strict :relaxed}
    :title "Cantus ends by step"
    :desc "The cantus reaches its final by step, usually 2→1."}
   {:id :cf-length :group :cf :level :info :kind :melodic :presets #{:strict :relaxed}
    :title "Cantus length 8–16"
    :desc "Typical cantus firmi have 8 to 16 notes."}])

(def rule-by-id (into {} (map (juxt :id identity) rules)))

(def groups
  [[:harmony "Intervals between the voices"]
   [:motion "Motion between the voices"]
   [:cadence "Cadence"]
   [:melody "Each melodic line"]
   [:cf "Cantus firmus"]])

(def presets {:strict "Strict (Fux)" :relaxed "Relaxed (modern)"})

(defn preset-rules [p] (set (keep #(when (contains? (:presets %) p) (:id %)) rules)))

(def penalty {:error 10 :warning 3 :info 0})

;; ---------------------------------------------------------------- context

(defn- verticals [cf cp]
  (vec (for [i (range (max (count cf) (count cp)))
             :let [a (get cf i) b (get cp i)]]
         (when (and a b)
           (let [iv (t/interval a b)]
             (assoc iv :i i :cons (t/consonance iv)))))))

(defn- motion-kind [mcf mcp a b]
  (let [dc (:dir mcf) dp (:dir mcp)]
    (cond (and (zero? dc) (zero? dp)) :static
          (or (zero? dc) (zero? dp)) :oblique
          (not= dc dp) :contrary
          (= (:g7 a) (:g7 b)) :parallel
          :else :similar)))

(defn- motions [cf cp verts]
  (vec (for [i (range (dec (count verts)))
             :let [a (get verts i) b (get verts (inc i))]
             :when (and a b)
             :let [mcf (t/move (cf i) (cf (inc i)))
                   mcp (t/move (cp i) (cp (inc i)))]]
         {:i i :a a :b b :cf mcf :cp mcp :kind (motion-kind mcf mcp a b)})))

(defn- line-moves [line]
  (vec (for [i (range (dec (count line)))
             :let [x (get line i) y (get line (inc i))]
             :when (and x y)]
         (assoc (t/move x y) :i i))))

(defn context [{:keys [cf cp key position]}]
  (let [cf (t/trim cf) cp (t/trim cp)
        verts (verticals cf cp)
        above? (not= position :below)]
    {:cf cf :cp cp :key key :above? above?
     :n (count cf)
     :upper (if above? cp cf) :lower (if above? cf cp)
     :verts verts
     :motions (motions cf cp verts)}))

(defn- span [i j] (vec (range i (inc j))))

(defn- pair-label [m] (str (:num (:a m)) " → " (:num (:b m))))

;; ---------------------------------------------------------------- vertical rules

(def perfect-word {0 "octaves" 4 "5ths"})

(defn- perfect-kind [v] (if (zero? (:g v)) "unisons" (perfect-word (:g7 v))))

(defmulti check (fn [id _] id))

(defmethod check :default [_ _] nil)

(defmethod check :consonance [_ {:keys [verts]}]
  (for [v verts :when (and v (= :dissonant (:cons v)))]
    {:cols [(:i v)]
     :msg (str (t/interval-name v) " (" (t/interval-long v) ") is a dissonance"
               (when (= 3 (:g7 v)) "; the 4th is dissonant in two voices"))}))

(defmethod check :start [_ {:keys [verts above?]}]
  (when-let [v (first verts)]
    (when-not (and (= :perfect (:cons v)) (or above? (zero? (:g7 v))))
      [{:cols [0]
        :msg (str "Opens on " (t/interval-name v) "; use "
                  (if above? "a unison, 5th or octave" "a unison or octave (counterpoint below)"))}])))

(defmethod check :end [_ {:keys [verts n]}]
  (when-let [v (and (> n 1) (get verts (dec n)))]
    (when-not (and (= :perfect (:cons v)) (zero? (:g7 v)))
      [{:cols [(dec n)] :msg (str "Ends on " (t/interval-name v) "; end on a unison or octave")}])))

(defn- middle [{:keys [verts n]}]
  (keep #(when (and % (< 0 (:i %) (dec n))) %) verts))

(defmethod check :unison [_ ctx]
  (for [v (middle ctx) :when (zero? (:g v))]
    {:cols [(:i v)] :msg "Unison in the middle of the exercise"}))

(defmethod check :imperfect-majority [_ ctx]
  (let [vs (middle ctx)
        perf (filter #(= :perfect (:cons %)) vs)
        imp (filter #(= :imperfect (:cons %)) vs)]
    (when (and (>= (+ (count perf) (count imp)) 4) (> (count perf) (count imp)))
      [{:cols (mapv :i perf)
        :msg (str (count perf) " perfect vs. " (count imp) " imperfect consonances in the middle")}])))

(defmethod check :spacing [_ {:keys [verts]}]
  (for [v verts :when (and v (> (:g v) 9))]
    {:cols [(:i v)] :msg (str "The voices are " (t/with-article (t/interval-long v)) " apart")}))

(defmethod check :crossing [_ {:keys [upper lower verts]}]
  (for [v verts :when (and v (< (t/midi (upper (:i v))) (t/midi (lower (:i v)))))]
    {:cols [(:i v)] :msg "The upper voice is below the lower voice"}))

(defmethod check :overlap [_ {:keys [upper lower motions]}]
  (for [{:keys [i]} motions
        :let [j (inc i)
              up (> (t/midi (lower j)) (t/midi (upper i)))
              down (< (t/midi (upper j)) (t/midi (lower i)))]
        :when (or up down)]
    {:cols [i j]
     :msg (if up
            "The lower voice moves above the upper voice's previous note"
            "The upper voice moves below the lower voice's previous note")}))

;; ---------------------------------------------------------------- motion rules

(defn- same-perfect? [{:keys [a b]}]
  (and (= :perfect (:cons a)) (= :perfect (:cons b)) (= (:g7 a) (:g7 b))))

(defmethod check :parallel-perfect [_ {:keys [motions]}]
  (for [m motions :when (and (= :parallel (:kind m)) (same-perfect? m))]
    {:cols [(:i m) (inc (:i m))]
     :msg (str "Parallel " (perfect-kind (:b m)) " (" (pair-label m) ")")}))

(defmethod check :contrary-perfect [_ {:keys [motions]}]
  (for [m motions :when (and (= :contrary (:kind m)) (same-perfect? m))]
    {:cols [(:i m) (inc (:i m))]
     :msg (str (str/capitalize (perfect-kind (:b m))) " by contrary motion (" (pair-label m) ")")}))

(defn- upper-move [above? m] (if above? (:cp m) (:cf m)))

(defmethod check :direct-perfect [_ {:keys [motions]}]
  (for [m motions :when (and (= :similar (:kind m)) (= :perfect (:cons (:b m))))]
    {:cols [(:i m) (inc (:i m))]
     :msg (str "Direct " (perfect-kind (:b m)) ": similar motion into a perfect consonance (" (pair-label m) ")")}))

(defmethod check :direct-perfect-leap [_ {:keys [motions above?]}]
  (for [m motions
        :when (and (= :similar (:kind m)) (= :perfect (:cons (:b m)))
                   (> (:g (upper-move above? m)) 1))]
    {:cols [(:i m) (inc (:i m))]
     :msg (str "Direct " (perfect-kind (:b m)) " with a leap in the upper voice (" (pair-label m) ")")}))

(defmethod check :parallel-run [_ {:keys [motions]}]
  (let [par? #(and (= :parallel (:kind %)) (= :imperfect (:cons (:a %))))
        runs (->> motions
                  (partition-by (fn [m] [(par? m) (:g7 (:a m))]))
                  (mapcat (fn [ms]
                            ;; split where motions aren't adjacent columns
                            (reduce (fn [acc m]
                                      (if (and (seq acc) (= (:i m) (inc (:i (peek (peek acc))))))
                                        (conj (pop acc) (conj (peek acc) m))
                                        (conj acc [m])))
                                    [] ms)))
                  (filter #(par? (first %))))]
    (for [run runs :when (>= (count run) 3)]
      {:cols (span (:i (first run)) (inc (:i (peek run))))
       :msg (str (inc (count run)) " parallel " (if (= 2 (:g7 (:a (first run)))) "3rds" "6ths") " in a row")})))

(defmethod check :leap-together [_ {:keys [motions]}]
  (for [m motions
        :when (and (#{:similar :parallel} (:kind m)) (>= (:g (:cf m)) 3) (>= (:g (:cp m)) 3))]
    {:cols [(:i m) (inc (:i m))] :msg "Both voices leap in the same direction"}))

(defmethod check :static [_ {:keys [motions]}]
  (for [m motions :when (= :static (:kind m))]
    {:cols [(:i m) (inc (:i m))] :msg "Neither voice moves"}))

;; ---------------------------------------------------------------- cadence

(defn- cadence-motion [{:keys [motions n]}]
  (when (> n 1) (first (filter #(= (- n 2) (:i %)) motions))))

(defmethod check :cadence-step [_ ctx]
  (when-let [m (cadence-motion ctx)]
    (when-not (and (= :contrary (:kind m)) (= 1 (:g (:cf m))) (= 1 (:g (:cp m))))
      [{:cols [(:i m) (inc (:i m))]
        :msg (str "The final is reached by " (name (:kind m)) " motion"
                  (when-not (and (= 1 (:g (:cf m))) (= 1 (:g (:cp m)))) " with a leap")
                  "; both voices should step into it from opposite sides")}])))

(defmethod check :leading-tone [_ ctx]
  (when-let [m (cadence-motion ctx)]
    (when-not (or (= 1 (Math/abs (:semis (:cf m)))) (= 1 (Math/abs (:semis (:cp m)))))
      [{:cols [(:i m) (inc (:i m))]
        :msg "No half step into the final; raise the 7th in the voice that rises to the final"}])))

;; ---------------------------------------------------------------- melodic rules

(defn- adjacent-pairs [moves]
  (for [[a b] (partition 2 1 moves) :when (= (:i b) (inc (:i a)))] [a b]))

(defn- runs
  "Contiguous stretches of present notes, as vectors of [i note]."
  [line]
  (->> (map-indexed vector line)
       (partition-by (comp nil? second))
       (remove (comp nil? second first))
       (map vec)))

(defmulti check-line (fn [id _] id))

(defmethod check-line :default [_ _] nil)

(defmethod check-line :melodic-dissonance [_ {:keys [moves]}]
  (for [m moves
        :when (and (<= (:g m) 7) (or (#{:A :d :AA :dd} (:q m)) (= 6 (:g7 m))))]
    {:cols [(:i m) (inc (:i m))]
     :msg (str (if (zero? (:g m)) "Chromatic step" "Dissonant leap") ": " (t/interval-long m))}))

(defmethod check-line :leap-size [_ {:keys [moves]}]
  (for [m moves :when (> (:g m) 7)]
    {:cols [(:i m) (inc (:i m))] :msg (str "Leap of " (t/with-article (t/interval-long m)))}))

(defmethod check-line :sixth [_ {:keys [moves]}]
  (for [m moves :when (and (= 5 (:g m)) (or (= :M (:q m)) (and (= :m (:q m)) (neg? (:dir m)))))]
    {:cols [(:i m) (inc (:i m))]
     :msg (str (if (pos? (:dir m)) "Rising " "Falling ") (t/interval-long m))}))

(defmethod check-line :leap-recovery [_ {:keys [moves]}]
  (for [[a b] (adjacent-pairs moves)
        :when (and (>= (:g a) 4) (not (and (= (:dir b) (- (:dir a))) (<= (:g b) 2))))]
    {:cols [(:i a) (inc (:i a)) (inc (:i b))]
     :msg (str "Leap of " (t/with-article (t/interval-long a)) " not followed by a turn the other way")}))

(defmethod check-line :leap-chain [_ {:keys [line moves]}]
  (let [skip? #(and (>= (:g %) 2) (not (zero? (:dir %))))
        pairs (adjacent-pairs moves)]
    (concat
     (for [[a b] pairs
           :when (and (skip? a) (skip? b) (= (:dir a) (:dir b)))
           :let [total (t/interval (line (:i a)) (line (inc (:i b))))]
           :when (or (> (:g total) 7) (= :dissonant (t/consonance total)))]
       {:cols [(:i a) (inc (:i a)) (inc (:i b))]
        :msg (str "Two leaps the same way outline " (t/with-article (t/interval-long total)))})
     (for [[[a b] [_ c]] (partition 2 1 pairs)
           :when (and (= (:i c) (inc (:i b)))
                      (every? skip? [a b c]) (apply = (map :dir [a b c])))]
       {:cols (span (:i a) (inc (:i c))) :msg "Three leaps in the same direction"}))))

(defmethod check-line :repeat [_ {:keys [moves]}]
  (for [m moves :when (and (zero? (:g m)) (zero? (:semis m)))]
    {:cols [(:i m) (inc (:i m))] :msg "Repeated note"}))

(defn- turning-points [run]
  (let [dirs (map (fn [[_ x] [_ y]] (compare (t/midi y) (t/midi x))) run (rest run))]
    (loop [pts [(first run)] k 0 prev 0 ds dirs]
      (if (empty? ds)
        (conj pts (peek run))
        (let [d (first ds)]
          (if (and (not (zero? d)) (not (zero? prev)) (not= d prev))
            (recur (conj pts (nth run k)) (inc k) d (rest ds))
            (recur pts (inc k) (if (zero? d) prev d) (rest ds))))))))

(defmethod check-line :tritone-outline [_ {:keys [line]}]
  (for [run (runs line)
        [[i x] [j y]] (partition 2 1 (turning-points run))
        :when (> (- j i) 1)
        :let [iv (t/interval x y)]
        :when (and (#{:A :d} (:q iv)) (#{3 4} (:g7 iv)))]
    {:cols (span i j) :msg (str "The line outlines a tritone: " (t/note-name x) " to " (t/note-name y))}))

(defmethod check-line :range [_ {:keys [line]}]
  (let [ns (keep-indexed #(when %2 [%1 %2]) line)]
    (when (seq ns)
      (let [[li lo] (apply min-key #(:s (second %)) ns)
            [hi-i hi] (apply max-key #(:s (second %)) ns)
            iv (t/interval lo hi)]
        (when (> (:g iv) 9)
          [{:cols (vec (sort [li hi-i])) :msg (str "Range of " (t/with-article (t/interval-long iv)))}])))))

(defmethod check-line :climax [_ {:keys [line]}]
  (let [ns (keep-indexed #(when %2 [%1 (t/midi %2)]) line)]
    (when (>= (count ns) 3)
      (let [top (apply max (map second ns))
            at (mapv first (filter #(= top (second %)) ns))]
        (when (> (count at) 1)
          [{:cols at :msg (str "The highest note, " (t/note-name (line (first at))) ", appears " (count at) " times")}])))))

(defmethod check-line :leaps-many [_ {:keys [moves]}]
  (let [skips (filter #(>= (:g %) 2) moves)
        big (filter #(>= (:g %) 3) moves)
        leaps (if (> (* 3 (count big)) (count moves)) big skips)]
    (when (and (>= (count moves) 5)
               (or (> (* 3 (count big)) (count moves)) (> (* 3 (count skips)) (* 2 (count moves)))))
      [{:cols (vec (distinct (mapcat #(vector (:i %) (inc (:i %))) leaps)))
        :msg (str (count leaps) " of " (count moves) " moves are leaps")}])))

(defmethod check-line :mode [_ {:keys [line key n]}]
  (for [[i note] (map-indexed vector line)
        :when (and note (not (t/in-mode? key note))
                   (not (and (= i (- n 2)) (t/leading-tone? key note))))]
    {:cols [i] :msg (str (t/note-name note) " is outside " (t/key-name key))}))

(defmethod check-line :cf-final [_ {:keys [line key n]}]
  (let [fpc (t/final-pc key)
        bad (cond-> []
              (and (first line) (not= fpc (t/pc (first line)))) (conj 0)
              (and (> n 1) (get line (dec n)) (not= fpc (t/pc (line (dec n))))) (conj (dec n)))]
    (when (seq bad)
      [{:cols bad :msg (str "The cantus should begin and end on " (t/final-name (:final key)))}])))

(defmethod check-line :cf-ending [_ {:keys [moves n]}]
  (when-let [m (and (> n 1) (first (filter #(= (- n 2) (:i %)) moves)))]
    (when (not= 1 (:g m))
      [{:cols [(- n 2) (dec n)] :msg (str "The cantus reaches the final by " (t/with-article (t/interval-long m)))}])))

(defmethod check-line :cf-length [_ {:keys [n]}]
  (when (and (> n 1) (not (<= 8 n 16)))
    [{:cols [] :msg (str "The cantus has " n " notes; 8–16 is typical")}]))

;; ---------------------------------------------------------------- analysis

(defn- tag [rule voice vs]
  (for [v vs] (assoc v :rule (:id rule) :level (:level rule) :kind (:kind rule) :voice voice)))

(defn analyze
  "All violations of `enabled` rules, ordered by position. Melodic rules run on the counterpoint,
  and on the cantus too when `check-cf?`; :cf rules only on the cantus."
  [exercise enabled check-cf?]
  (let [{:keys [cf cp n key] :as ctx} (context exercise)
        line-ctx (fn [line] {:line line :moves (line-moves line) :key key :n n})
        cp-ctx (line-ctx cp)
        cf-ctx (line-ctx cf)
        vs (for [r rules
                 :when (contains? enabled (:id r))
                 v (case (:group r)
                     :melody (concat (tag r :cp (check-line (:id r) cp-ctx))
                                     (when check-cf? (tag r :cf (check-line (:id r) cf-ctx))))
                     :cf (when check-cf? (tag r :cf (check-line (:id r) cf-ctx)))
                     (tag r nil (check (:id r) ctx)))]
             v)]
    (vec (sort-by (fn [v] [(or (first (:cols v)) 999) ({:error 0 :warning 1 :info 2} (:level v))]) vs))))

(defn score [violations]
  (let [by (frequencies (map :level violations))]
    {:errors (by :error 0) :warnings (by :warning 0) :infos (by :info 0)
     :penalty (reduce + (map (comp penalty :level) violations))}))

(defn stats [exercise]
  (let [{:keys [verts motions cp]} (context exercise)
        moves (line-moves cp)
        verts (remove nil? verts)]
    {:motion (frequencies (map :kind motions))
     :motions (count motions)
     :cons (frequencies (map :cons verts))
     :verts (count verts)
     :melody (frequencies (map #(let [g (:g %)] (cond (zero? g) :repeat (= 1 g) :step (= 2 g) :skip :else :leap)) moves))
     :moves (count moves)}))
