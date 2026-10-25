(ns counterpointer.hints
  (:require [counterpointer.theory :as t]
            [counterpointer.rules :as r]))

;; Dynamic help for the selected note: positional tips, and which candidate notes break no rules.
;; Tips are message vectors [:tip/<key> & args], translated in the view; "Try:" fixes are :fix/<rule-id>.

;; ---------------------------------------------------------------- helpers

(defn- above? [e] (not= (:position e) :below))

(defn- per [e] (r/per-bar (:species e)))

(defn- leading-tone [key]
  (let [l (mod (dec (get-in key [:final :l])) 7)
        d (mod (- (dec (t/final-pc key)) (t/natural-pc l)) 12)]
    {:s l :a (if (> d 6) (- d 12) d)}))

(defn- degree-note [key deg]
  (let [l (mod (+ (get-in key [:final :l]) deg) 7)]
    {:s l :a (t/default-alt key l)}))

(defn- prev-notes
  "Present notes before `col` in `line`, latest first, as [i note]."
  [line col]
  (filter second (map (fn [i] [i (get line i)]) (range (dec col) -1 -1))))

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
            sixth? (= (above? e) cf-from-above?)]
        [:tip/cadence {:from-above? cf-from-above? :target target :final (:final key)
                       :where (if (> p 1) (if (zero? beat) :bar-upbeat :upbeat) :bar)
                       :sixth? sixth? :downbeat? (and (> p 1) (zero? beat))}]))))

(defn- opening-tip [e p]
  (let [key (:key e)]
    [:tip/opening (above? e) (:final key) (degree-note key 4) (> p 1)]))

(defn- motion-tip [{:keys [cf]} b]
  (when-let [[now before] (and (pos? b) [(get cf b) (get cf (dec b))])]
    (when (and now before)
      [:tip/motion (compare (t/midi now) (t/midi before)) before now])))

(defn- leap-tip [cp col]
  (let [[[_ y] [_ x]] (prev-notes cp col)]
    (when (and x y)
      (let [m (t/move x y)]
        (when (>= (:g m) 4)
          [:tip/leap m])))))

(defn- chain-tip [e col]
  (let [{:keys [motions]} (r/context e)
        before (reverse (filter #(< (:i %) col) motions))
        run (take-while #(and (= :parallel (:kind %)) (= :imperfect (:cons (:a %)))
                              (= (:g7 (:a %)) (:g7 (:a (first before)))))
                        before)]
    (when (and (>= (count run) 2) (= (dec col) (inc (:i (first before)))))
      [:tip/chain (inc (count run)) (:g7 (:a (first run)))])))

(defn- climax-tip [cp col]
  (let [ns (keep-indexed #(when (and %2 (not= %1 col)) [%1 %2]) cp)
        [_ prev] (first (prev-notes cp col))]
    (when (and prev (>= (count ns) 3))
      (let [[i top] (apply max-key #(t/midi (second %)) ns)]
        (when (and (<= (- (:s top) (:s prev)) 1) (not= top prev))
          [:tip/climax top (inc i)])))))

(defn- balance-tip [e col]
  (let [{:keys [verts n]} (r/context e)
        vs (filter #(and % (zero? (:beat %)) (< 0 (:bar %) (dec n)) (< (:i %) col)) verts)
        perf (count (filter #(= :perfect (:cons %)) vs))
        imp (count (filter #(= :imperfect (:cons %)) vs))]
    (when (and (>= (+ perf imp) 3) (>= perf imp))
      [:tip/balance perf imp])))

(defn tips
  "Short advice for the selected bar, most specific first."
  [{:keys [cf cp] :as e} {:keys [voice col] :as sel}]
  (let [n (count cf)
        p (per e)]
    (cond
      (zero? n)
      [[:tip/no-cantus]]

      (nil? sel) [[:tip/no-selection]]

      (= voice :cf)
      [[:tip/cantus] [:tip/select-cp]]

      :else
      (let [b (quot col p)
            beat (mod col p)
            final-slot (* p (dec n))]
        (->> [(when (>= b n) [:tip/past-end])
              (when (and (zero? b) (< col final-slot)) (opening-tip e p))
              (when (= col final-slot)
                [:tip/last (:final (:key e)) (above? e) (> p 1)])
              (when (and (= b (- n 2)) (> n 2)) (cadence-tip e beat p))
              (when (and (pos? beat) (< b (dec n)) (not= b (- n 2)))
                [:tip/upbeat])
              (when (and (zero? beat) (< 0 b (- n 2))) (motion-tip e b))
              (when (and (> p 1) (zero? beat) (< 0 b (dec n)))
                [:tip/downbeat])
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
