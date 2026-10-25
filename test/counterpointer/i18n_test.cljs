(ns counterpointer.i18n-test
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as str]
            [counterpointer.i18n :as i18n]
            [counterpointer.rules :as r]
            [counterpointer.hints :as h]
            [counterpointer.library :as lib]
            [counterpointer.events :as events]))

(defn- key-set [dict]
  (set (for [[ns m] dict k (keys m)] (keyword (name ns) (name k)))))

(deftest dictionaries-have-the-same-keys
  (let [en (key-set (i18n/dicts :en)) el (key-set (i18n/dicts :el))]
    (is (empty? (remove el en)) "missing in :el")
    (is (empty? (remove en el)) "missing in :en")))

(deftest every-rule-has-texts
  (doseq [lang i18n/langs
          {:keys [id]} r/rules
          ns ["rule" "rule.desc" "fix"]
          :let [k (keyword ns (name id))]]
    (is (contains? (key-set (i18n/dicts lang)) k) [lang k])))

(defn- rng [seed]
  (let [s (atom seed)]
    (fn [n] (swap! s #(mod (+ (* % 1103515245) 12345) 2147483648)) (mod (quot @s 65536) n))))

(def all-rules (into (r/preset-rules :strict) (r/preset-rules :relaxed)))

(defn- exercises []
  (let [rand (rng 7)]
    (for [entry lib/fux
          species [1 2]
          position [:above :below]
          _ (range 6)
          :let [e (events/from-library entry position species)
                per (r/per-bar species)
                len (inc (* per (dec (count (:cf e)))))
                cp (vec (for [_ (range len)]
                          (when (pos? (rand 8))
                            {:s (+ (:s (first (:cf e))) (if (= position :above) 2 -9) (rand 11))
                             :a (dec (rand 3))})))]]
      (assoc e :cp cp))))

(def rare
  "Messages the generated exercises don't reach (the Fux cantus firmi are clean)."
  (let [m4 {:g 3 :g7 3 :num 4 :q :P :semis 5 :dir 1}]
    [[:tip/no-cantus] [:tip/no-selection] [:tip/past-end] [:v/final-whole] [:v/parallel-run 4 2]
     [:v/cf-ending m4] [:v/cf-final {:l 1 :a 0}] [:v/cf-length 5]]))

(deftest messages-render-in-every-language
  (let [es (exercises)
        msgs (concat rare
                     (mapcat #(map :msg (r/analyze % all-rules true)) es)
                     (for [e (take 40 es) col (range (count (:cp e))) tip (h/tips e {:voice :cp :col col})] tip)
                     (for [e (take 5 es) col (range 3) tip (h/tips e {:voice :cf :col col})] tip))
        used (set (map first msgs))]
    (testing "every message key is exercised"
      (is (empty? (remove used (filter #(#{"v" "tip"} (namespace %)) (key-set (i18n/dicts :en)))))))
    (doseq [lang i18n/langs names [:letters :solfege]
            :let [loc (i18n/locale lang names)]
            m (distinct msgs)
            :let [s (i18n/msg loc m)]]
      (is (and (string? s) (not (str/includes? s "Missing")) (not (str/includes? s "{"))) [lang m s]))))
