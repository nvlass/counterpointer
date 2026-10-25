(ns counterpointer.theory
  (:require [clojure.string :as str]))

;; A note is {:s diatonic step, :a alteration}: :s is letter index + 7 × octave, so C4 = 28.
;; Spelling matters: intervals are named from letter distance, not semitones alone.

(def letters ["C" "D" "E" "F" "G" "A" "B"])
(def natural-pc [0 2 4 5 7 9 11])
(def letter-fifths [0 2 4 -1 1 3 5])

(def max-notes 32)

(defn letter [{:keys [s]}] (mod s 7))
(defn octave [{:keys [s]}] (quot s 7))

(defn midi [{:keys [s a]}]
  (+ 12 (* 12 (quot s 7)) (natural-pc (mod s 7)) a))

(defn pc [n] (mod (midi n) 12))

(def acc-sym {-2 "𝄫" -1 "♭" 0 "" 1 "♯" 2 "𝄪"})
(def acc-ascii {-2 "bb" -1 "b" 0 "" 1 "#" 2 "##"})

(defn note-name
  ([n] (note-name n letters))
  ([n names] (str (names (letter n)) (acc-sym (:a n)) (octave n))))

(defn pc-name
  "Letter and accidental, no octave."
  [n names]
  (str (names (letter n)) (acc-sym (:a n))))

;; ---------------------------------------------------------------- modes

(def modes
  [{:id :dorian :steps [0 2 3 5 7 9 10] :k 2}
   {:id :phrygian :steps [0 1 3 5 7 8 10] :k 4}
   {:id :lydian :steps [0 2 4 6 7 9 11] :k -1}
   {:id :mixolydian :steps [0 2 4 5 7 9 10] :k 1}
   {:id :aeolian :steps [0 2 3 5 7 8 10] :k 3}
   {:id :ionian :steps [0 2 4 5 7 9 11] :k 0}])

(def mode-by-id (into {} (map (juxt :id identity) modes)))

(defn final-name
  ([f] (final-name f letters))
  ([{:keys [l a]} names] (str (names l) (acc-sym a))))

(defn final-pc [{:keys [final]}] (mod (+ (natural-pc (:l final)) (:a final)) 12))

(defn signature
  "Sharps (positive) or flats (negative) in the key signature."
  [{:keys [mode final]}]
  (- (+ (letter-fifths (:l final)) (* 7 (:a final))) (:k (mode-by-id mode))))

(defn finals [mode]
  (for [a [0 -1 1] l (range 7)
        :let [f {:l l :a a}]
        :when (<= (Math/abs (signature {:mode mode :final f})) 6)]
    f))

(defn default-alt [{:keys [mode final] :as key} l]
  (let [deg (mod (- l (:l final)) 7)
        target (+ (final-pc key) ((:steps (mode-by-id mode)) deg))
        d (mod (- target (natural-pc l)) 12)]
    (if (> d 6) (- d 12) d)))

(defn in-mode? [key n] (= (:a n) (default-alt key (letter n))))

(defn degree [{:keys [final]} n] (mod (- (letter n) (:l final)) 7))

(defn leading-tone?
  "A raised 7th: the note a semitone below the final, spelled as degree 7."
  [key n]
  (and (= 6 (degree key n)) (= (pc n) (mod (dec (final-pc key)) 12))))

(defn nearest
  "The note with letter `l` closest to step `ref`, altered as the key wants."
  [key l ref]
  (let [s (first (filter #(= l (mod % 7)) (range (- ref 3) (+ ref 4))))]
    {:s s :a (default-alt key l)}))

(defn spell
  "Spells a MIDI pitch, preferring the key's own notes, then B♭ (the usual ficta), then sharps or flats to match the signature."
  [key m]
  (let [est (quot (* 7 (- m 12)) 12)
        flat-key? (neg? (signature key))
        cost (fn [{:keys [s a]}]
               (cond (= a (default-alt key (mod s 7))) 0
                     (and (= -1 a) (= 6 (mod s 7))) 0.5
                     (if flat-key? (pos? a) (neg? a)) 2
                     :else 1))]
    (->> (for [s (range (- est 2) (+ est 3))
               :let [a (- m (midi {:s s :a 0}))]
               :when (<= -1 a 1)]
           {:s s :a a})
         (sort-by cost)
         first)))

;; ---------------------------------------------------------------- intervals

(def major-ref [0 2 4 5 7 9 11])

(defn interval
  "Interval between two notes regardless of order: :g letter distance (0 = unison), :g7 reduced, :q quality."
  [x y]
  (let [[lo hi] (if (or (< (:s y) (:s x)) (and (= (:s x) (:s y)) (< (midi y) (midi x)))) [y x] [x y])
        g (- (:s hi) (:s lo))
        g7 (mod g 7)
        semis (- (midi hi) (midi lo))
        d (- semis (major-ref g7) (* 12 (quot g 7)))
        q (if (#{0 3 4} g7)
            (case d 0 :P 1 :A -1 :d (if (pos? d) :AA :dd))
            (case d 0 :M -1 :m 1 :A -2 :d (if (pos? d) :AA :dd)))]
    {:g g :g7 g7 :num (inc g) :semis semis :q q}))

(def q-label {:P "P" :M "M" :m "m" :A "A" :d "d" :AA "AA" :dd "dd"})

(defn interval-name [{:keys [num q]}] (str (q-label q) num))

(defn tritone? [{:keys [num q]}] (or (and (= q :A) (= num 4)) (and (= q :d) (= num 5))))

(defn consonance
  "Harmonic class in two voices: the 4th counts as a dissonance."
  [{:keys [g7 q]}]
  (cond (and (= q :P) (#{0 4} g7)) :perfect
        (and (#{2 5} g7) (#{:M :m} q)) :imperfect
        :else :dissonant))

(defn move
  "Melodic motion from x to y: the interval plus :dir -1/0/1."
  [x y]
  (assoc (interval x y) :dir (compare (midi y) (midi x))))

;; ---------------------------------------------------------------- lines

(defn trim [line] (vec (reverse (drop-while nil? (reverse line)))))

(defn put [line col note]
  (let [line (into (vec line) (repeat (max 0 (- (inc col) (count line))) nil))]
    (trim (assoc line col note))))

(defn shift [line steps] (mapv #(when % (update % :s + steps)) line))

(defn recenter-steps
  "The octave shift (in steps) that puts the line's average step nearest `center`."
  [line center]
  (let [ss (keep :s line)]
    (if (empty? ss)
      0
      (* 7 (Math/round (/ (- center (/ (reduce + ss) (count ss))) 7))))))

(defn recenter [line center] (shift line (recenter-steps line center)))

(defn note->text [key n]
  (if-not n
    "-"
    (str (letters (letter n))
         (if (and (zero? (:a n)) (not (zero? (default-alt key (letter n)))))
           "n"
           (acc-ascii (:a n)))
         (octave n))))

(defn line->text [key line] (str/join " " (map #(note->text key %) line)))

(def token-re #"^([A-Ga-g])(##|bb|#|b|n|♯|♭|♮)?(-?\d)?$")

(defn parse-line
  "Parses `D F E D` or `D4 F#4 Bb3`; `-` leaves a gap. Missing octaves go to the nearest note."
  [key text center]
  (loop [toks (remove str/blank? (str/split (str/trim text) #"[\s,|]+"))
         ref center
         out []]
    (cond
      (empty? toks) {:line (trim out)}
      (>= (count out) max-notes) {:error [:parse/too-many max-notes]}
      :else
      (let [tok (first toks)]
        (if (#{"-" "_" "."} tok)
          (recur (rest toks) ref (conj out nil))
          (if-let [[_ l acc oct] (re-matches token-re tok)]
            (let [li (.indexOf letters (str/upper-case l))
                  s (if oct (+ li (* 7 (js/parseInt oct))) (:s (nearest key li ref)))
                  a (case acc
                      ("#" "♯") 1 ("b" "♭") -1 "##" 2 "bb" -2 ("n" "♮") 0
                      (default-alt key li))
                  n {:s s :a a}]
              (recur (rest toks) s (conj out n)))
            {:error [:parse/bad-token tok]}))))))
