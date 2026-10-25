(ns counterpointer.hints-test
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as str]
            [counterpointer.theory :as t]
            [counterpointer.rules :as r]
            [counterpointer.hints :as h]
            [counterpointer.library :as lib]))

(def dorian {:mode :dorian :final {:l 1 :a 0}})

(defn ex [cp & [species]]
  {:cf (lib/line dorian "D3 F3 E3 D3 G3 F3 A3 G3 F3 E3 D3") :cp (lib/line dorian cp)
   :key dorian :position :above :species (or species 1)})

(def solution "D4 C4 G3 A3 B3 D4 C4 E4 D4 C#4 D4")

(defn clean-names [e sel]
  (->> (h/candidates e sel (r/preset-rules :strict) true)
       (filter #(and (zero? (:errors %)) (zero? (:warnings %))))
       (map (comp t/note-name :note))
       set))

(deftest candidates-include-the-known-answer
  (let [full (lib/line dorian solution)]
    (doseq [col (range 11)
            :let [e (assoc (ex solution) :cp (assoc full col nil))]]
      (is (contains? (clean-names e {:voice :cp :col col}) (t/note-name (full col))) col))))

(deftest candidates-offer-the-raised-leading-tone
  (let [e (ex "D4 C4 G3 A3 B3 D4 C4 E4 D4 - D4")]
    (is (contains? (clean-names e {:voice :cp :col 9}) "C♯4"))
    (is (not (contains? (clean-names e {:voice :cp :col 9}) "C4"))))
  (testing "also before the final note is written"
    (let [e (ex "D4 C4 G3 A3 B3 D4 C4 E4 D4")]
      (is (= #{"C♯4"} (clean-names e {:voice :cp :col 9}))))))

(deftest tips
  (testing "cadence names the notes"
    (is (some #(str/includes? % "C♯ → D") (h/tips (ex "D4 C4 G3 A3 B3 D4 C4 E4 D4") {:voice :cp :col 9}))))
  (testing "the cantus direction"
    (is (some #(str/includes? % "rises") (h/tips (ex "D4") {:voice :cp :col 1}))))
  (testing "leap recovery"
    (is (some #(str/includes? % "turn back down") (h/tips (ex "D4 C4 A4") {:voice :cp :col 3}))))
  (testing "phrygian keeps the 7th"
    (let [phr {:mode :phrygian :final {:l 2 :a 0}}
          e {:cf (lib/line phr "E3 C3 D3 C3 A2 A3 G3 E3 F3 E3") :cp [] :key phr :position :above :species 1}]
      (is (some #(str/includes? % "D → E") (h/tips e {:voice :cp :col 8}))))))
