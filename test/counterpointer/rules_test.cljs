(ns counterpointer.rules-test
  (:require [cljs.test :refer [deftest is testing]]
            [counterpointer.theory :as t]
            [counterpointer.rules :as r]
            [counterpointer.library :as lib]))

(def dorian {:mode :dorian :final {:l 1 :a 0}})

(defn n [s] (first (lib/line dorian s)))

(defn ex [cf cp & [position]]
  {:cf (lib/line dorian cf) :cp (lib/line dorian cp) :key dorian :position (or position :above)})

(defn rule-ids [exercise preset]
  (set (map :rule (r/analyze exercise (r/preset-rules preset) true))))

(deftest intervals
  (is (= "P5" (t/interval-name (t/interval (n "D4") (n "A4")))))
  (is (= "M6" (t/interval-name (t/interval (n "E4") (n "C#5")))))
  (is (= "m10" (t/interval-name (t/interval (n "D4") (n "F5")))))
  (is (= "A4" (t/interval-name (t/interval (n "F4") (n "B4")))))
  (is (= "d5" (t/interval-name (t/interval (n "B4") (n "F5")))))
  (is (= "A1" (t/interval-name (t/interval (n "F4") (n "F#4")))))
  (is (= :dissonant (t/consonance (t/interval (n "D4") (n "G4")))))
  (is (= :perfect (t/consonance (t/interval (n "D3") (n "A4")))))
  (is (= :imperfect (t/consonance (t/interval (n "D4") (n "F5"))))))

(deftest keys-and-spelling
  (is (zero? (t/signature dorian)))
  (is (= -1 (t/signature {:mode :dorian :final {:l 4 :a 0}})))
  (is (= -1 (t/default-alt {:mode :dorian :final {:l 4 :a 0}} 6)) "G Dorian has B♭")
  (is (= {:s 31 :a 1} (t/spell {:mode :mixolydian :final {:l 1 :a 0}} 66)) "F♯ in D Mixolydian")
  (is (= {:s 34 :a -1} (t/spell {:mode :lydian :final {:l 3 :a 0}} 70)) "B♭, not A♯, near F")
  (is (= 29 (:s (t/nearest dorian 1 31))) "D nearest to F4 is D4")
  (testing "parse picks the nearest octave and round-trips"
    (let [line (lib/line dorian "D F E D G F A G F E D")]
      (is (= "D4 F4 E4 D4 G4 F4 A4 G4 F4 E4 D4" (t/line->text dorian line))))
    (let [g-dorian {:mode :dorian :final {:l 4 :a 0}}
          line (:line (t/parse-line g-dorian "G4 B4 Bn4 - C5" 28))]
      (is (= [-1 0] (map :a (subvec line 1 3))))
      (is (nil? (line 3)))
      (is (= "G4 Bb4 Bn4 - C5" (t/line->text g-dorian line))))))

(deftest fux-cantus-firmi-are-clean
  (doseq [{:keys [key cf name]} lib/fux
          :let [e {:cf (lib/line key cf) :cp [] :key key :position :above}
                vs (remove #(= :info (:level %)) (r/analyze e (r/preset-rules :strict) true))]]
    (is (empty? vs) [name (map :msg vs)])))

(deftest example-solution-is-clean
  (let [{:keys [cf cp]} lib/example]
    (is (empty? (rule-ids (ex cf cp) :strict)))))

(deftest detects
  (testing "parallel 5ths and 8ves"
    (is (contains? (rule-ids (ex "D4 E4" "A4 B4") :relaxed) :parallel-perfect))
    (is (contains? (rule-ids (ex "D4 E4" "D5 E5") :relaxed) :parallel-perfect))
    (is (contains? (rule-ids (ex "D4 E4" "D4 E5") :relaxed) :parallel-perfect) "unison to octave"))
  (testing "antiparallel 5ths"
    (is (contains? (rule-ids (ex "D4 G3" "A4 D5") :relaxed) :contrary-perfect)))
  (testing "direct octaves: strict always, relaxed only with a leap on top"
    (is (contains? (rule-ids (ex "F4 G4" "D5 G5") :strict) :direct-perfect))
    (is (contains? (rule-ids (ex "F4 G4" "D5 G5") :relaxed) :direct-perfect-leap))
    (is (not (contains? (rule-ids (ex "E4 D4" "A4 A4") :strict) :direct-perfect)) "oblique")
    (is (not (contains? (rule-ids (ex "C4 D4" "B4 A4") :relaxed) :direct-perfect-leap))))
  (testing "dissonances, including the 4th"
    (is (contains? (rule-ids (ex "D4" "G4") :relaxed) :consonance)))
  (testing "four parallel 3rds"
    (let [ids (rule-ids (ex "D4 E4 F4 G4" "F4 G4 A4 B4") :relaxed)]
      (is (contains? ids :parallel-run)))
    (is (not (contains? (rule-ids (ex "D4 E4 F4" "F4 G4 A4") :relaxed) :parallel-run))))
  (testing "cadence needs contrary steps and a half step"
    (is (contains? (rule-ids (ex "D4 E4 D4" "A4 C5 D5") :relaxed) :leading-tone))
    (is (not (contains? (rule-ids (ex "D4 E4 D4" "A4 C#5 D5") :relaxed) :leading-tone)))
    (is (contains? (rule-ids (ex "D4 A4 D4" "A4 F5 D5") :relaxed) :cadence-step)))
  (testing "ficta at the cadence isn't out of mode, elsewhere it is"
    (is (not (contains? (rule-ids (ex "D4 E4 D4" "A4 C#5 D5") :relaxed) :mode)))
    (is (contains? (rule-ids (ex "D4 E4 D4 E4 D4" "A4 C#5 D5 C#5 D5") :relaxed) :mode)))
  (testing "counterpoint below may not open on a 5th"
    (is (contains? (rule-ids (ex "A4" "D4" :below) :relaxed) :start))
    (is (not (contains? (rule-ids (ex "A4" "D4" :above) :relaxed) :start))))
  (testing "melodic"
    (is (contains? (rule-ids (ex "" "F4 B4") :relaxed) :melodic-dissonance))
    (is (contains? (rule-ids (ex "" "D4 C5") :relaxed) :melodic-dissonance) "7th")
    (is (contains? (rule-ids (ex "" "D4 A4 D5") :relaxed) :leap-recovery))
    (is (contains? (rule-ids (ex "" "D4 G4 C5") :relaxed) :leap-chain) "two 4ths outline a 7th")
    (is (not (contains? (rule-ids (ex "" "D4 F4 A4") :relaxed) :leap-chain)))
    (is (contains? (rule-ids (ex "" "F4 G4 A4 B4 A4") :strict) :tritone-outline))
    (is (contains? (rule-ids (ex "" "D4 A4 G4 A4 F4") :relaxed) :climax))
    (is (contains? (rule-ids (ex "" "D4 G5") :relaxed) :leap-size))))
