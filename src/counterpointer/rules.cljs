(ns counterpointer.rules
  (:require [counterpointer.theory :as t]))

;; Every rule is checked; presets only decide which ones are shown and counted.
;; :kind decides where the score marks it: :vertical (interval labels), :motion (between columns), :melodic (notes).
;; Titles, descriptions and fixes live in the i18n dictionaries (:rule/<id>, :rule.desc/<id>, :fix/<id>);
;; violation :msg is a message vector [:v/<key> & args], translated in the view.

(def rules
  [{:id :consonance :group :harmony :level :error :kind :vertical :presets #{:strict :relaxed}}
   {:id :weak-dissonance-passing :group :harmony :level :error :kind :vertical :presets #{:strict} :species #{2}}
   {:id :weak-dissonance :group :harmony :level :error :kind :vertical :presets #{:relaxed} :species #{2}}
   {:id :start :group :harmony :level :error :kind :vertical :presets #{:strict :relaxed}}
   {:id :end :group :harmony :level :error :kind :vertical :presets #{:strict :relaxed}}
   {:id :imperfect-majority :group :harmony :level :warning :kind :vertical :presets #{:strict :relaxed}}
   {:id :unison :group :harmony :level :warning :kind :vertical :presets #{:strict}}
   {:id :spacing :group :harmony :level :warning :kind :vertical :presets #{:strict :relaxed}}
   {:id :crossing :group :harmony :level :warning :kind :vertical :presets #{:strict :relaxed}}
   {:id :overlap :group :motion :level :warning :kind :motion :presets #{:strict}}

   {:id :parallel-perfect :group :motion :level :error :kind :motion :presets #{:strict :relaxed}}
   {:id :downbeat-parallels :group :motion :level :warning :kind :vertical :presets #{:strict :relaxed} :species #{2}}
   {:id :contrary-perfect :group :motion :level :warning :kind :motion :presets #{:strict :relaxed}}
   {:id :direct-perfect :group :motion :level :warning :kind :motion :presets #{:strict}}
   {:id :direct-perfect-leap :group :motion :level :warning :kind :motion :presets #{:relaxed}}
   {:id :parallel-run :group :motion :level :warning :kind :motion :presets #{:strict :relaxed}}
   {:id :leap-together :group :motion :level :warning :kind :motion :presets #{:strict}}
   {:id :static :group :motion :level :warning :kind :motion :presets #{:strict :relaxed}}

   {:id :cadence-step :group :cadence :level :error :kind :motion :presets #{:strict :relaxed}}
   {:id :leading-tone :group :cadence :level :error :kind :motion :presets #{:strict :relaxed}}

   {:id :final-whole :group :cadence :level :warning :kind :melodic :presets #{:strict :relaxed} :species #{2}}

   {:id :melodic-dissonance :group :melody :level :error :kind :melodic :presets #{:strict :relaxed}}
   {:id :leap-size :group :melody :level :error :kind :melodic :presets #{:strict :relaxed}}
   {:id :sixth :group :melody :level :warning :kind :melodic :presets #{:strict}}
   {:id :leap-recovery :group :melody :level :warning :kind :melodic :presets #{:strict :relaxed}}
   {:id :leap-chain :group :melody :level :warning :kind :melodic :presets #{:strict :relaxed}}
   {:id :repeat :group :melody :level :warning :kind :melodic :presets #{:strict}}
   {:id :tritone-outline :group :melody :level :warning :kind :melodic :presets #{:strict}}
   {:id :range :group :melody :level :warning :kind :melodic :presets #{:strict :relaxed}}
   {:id :climax :group :melody :level :warning :kind :melodic :presets #{:strict :relaxed}}
   {:id :leaps-many :group :melody :level :warning :kind :melodic :presets #{:strict}}
   {:id :mode :group :melody :level :warning :kind :melodic :presets #{:strict :relaxed}}

   {:id :cf-final :group :cf :level :error :kind :melodic :presets #{:strict :relaxed}}
   {:id :cf-ending :group :cf :level :error :kind :melodic :presets #{:strict :relaxed}}
   {:id :cf-length :group :cf :level :info :kind :melodic :presets #{:strict :relaxed}}])

(def rule-by-id (into {} (map (juxt :id identity) rules)))

(def groups [:harmony :motion :cadence :melody :cf])

(def presets [:strict :relaxed])

(defn preset-rules [p] (set (keep #(when (contains? (:presets %) p) (:id %)) rules)))

(def penalty {:error 10 :warning 3 :info 0})

;; ---------------------------------------------------------------- context

(defn per-bar
  "Counterpoint slots per cantus note: the counterpoint is a flat vector of slots, `nil` for a rest or gap."
  [species]
  (case species 2 2 1))

(defn- verticals [cf cp per]
  (vec (for [i (range (max (count cp) (* per (count cf))))
             :let [a (get cf (quot i per)) b (get cp i)]]
         (when (and a b)
           (let [iv (t/interval a b)]
             (assoc iv :i i :bar (quot i per) :beat (mod i per) :cons (t/consonance iv)))))))

(defn- motion-kind [mcf mcp a b]
  (let [dc (:dir mcf) dp (:dir mcp)]
    (cond (and (zero? dc) (zero? dp)) :static
          (or (zero? dc) (zero? dp)) :oblique
          (not= dc dp) :contrary
          (= (:g7 a) (:g7 b)) :parallel
          :else :similar)))

(defn- motions [cf cp verts per]
  (vec (for [i (range (dec (count verts)))
             :let [a (get verts i) b (get verts (inc i))]
             :when (and a b)
             :let [mcf (t/move (cf (quot i per)) (cf (quot (inc i) per)))
                   mcp (t/move (cp i) (cp (inc i)))]]
         {:i i :a a :b b :cf mcf :cp mcp :kind (motion-kind mcf mcp a b)})))

(defn- line-moves [line]
  (vec (for [i (range (dec (count line)))
             :let [x (get line i) y (get line (inc i))]
             :when (and x y)]
         (assoc (t/move x y) :i i))))

(defn context [{:keys [cf cp key position species]}]
  (let [per (per-bar species)
        cf (t/trim cf) cp (t/trim cp)
        verts (verticals cf cp per)
        above? (not= position :below)
        cf-at (fn [i] (get cf (quot i per)))
        cp-at (fn [i] (get cp i))]
    {:cf cf :cp cp :key key :above? above? :per per
     :n (count cf)
     :final-slot (* per (dec (count cf)))
     :upper (if above? cp-at cf-at) :lower (if above? cf-at cp-at)
     :verts verts
     :motions (motions cf cp verts per)}))

(defn- span [i j] (vec (range i (inc j))))

;; ---------------------------------------------------------------- vertical rules

(defmulti check (fn [id _] id))

(defmethod check :default [_ _] nil)

(defmethod check :consonance [_ {:keys [verts]}]
  (for [v verts :when (and v (zero? (:beat v)) (= :dissonant (:cons v)))]
    {:cols [(:i v)]
     :msg [:v/consonance v]}))

(defmethod check :start [_ {:keys [verts above? per]}]
  (when-let [v (first (filter some? (take per verts)))]
    (when-not (and (= :perfect (:cons v)) (or above? (zero? (:g7 v))))
      [{:cols [(:i v)]
        :msg [:v/start v above?]}])))

(defmethod check :end [_ {:keys [verts n final-slot]}]
  (when-let [v (and (> n 1) (get verts final-slot))]
    (when-not (and (= :perfect (:cons v)) (zero? (:g7 v)))
      [{:cols [final-slot] :msg [:v/end v]}])))

(defn- middle
  "Downbeat verticals between the first and last bar."
  [{:keys [verts n]}]
  (keep #(when (and % (zero? (:beat %)) (< 0 (:bar %) (dec n))) %) verts))

(defmethod check :unison [_ {:keys [per] :as ctx}]
  (for [v (middle ctx) :when (zero? (:g v))]
    {:cols [(:i v)] :msg [:v/unison (> per 1)]}))

(defmethod check :imperfect-majority [_ ctx]
  (let [vs (middle ctx)
        perf (filter #(= :perfect (:cons %)) vs)
        imp (filter #(= :imperfect (:cons %)) vs)]
    (when (and (>= (+ (count perf) (count imp)) 4) (> (count perf) (count imp)))
      [{:cols (mapv :i perf)
        :msg [:v/imperfect-majority (count perf) (count imp)]}])))

(defmethod check :spacing [_ {:keys [verts]}]
  (for [v verts :when (and v (> (:g v) 9))]
    {:cols [(:i v)] :msg [:v/spacing v]}))

(defmethod check :crossing [_ {:keys [upper lower verts]}]
  (for [v verts :when (and v (< (t/midi (upper (:i v))) (t/midi (lower (:i v)))))]
    {:cols [(:i v)] :msg [:v/crossing]}))

(defmethod check :overlap [_ {:keys [upper lower motions]}]
  (for [{:keys [i]} motions
        :let [j (inc i)
              up (> (t/midi (lower j)) (t/midi (upper i)))
              down (< (t/midi (upper j)) (t/midi (lower i)))]
        :when (or up down)]
    {:cols [i j]
     :msg [:v/overlap up]}))

;; ---------------------------------------------------------------- weak beats (2nd species)

(defn- weak-dissonances [{:keys [verts cp]} neighbor-ok?]
  (for [v verts
        :when (and v (pos? (:beat v)) (= :dissonant (:cons v)))
        :let [i (:i v) prev (get cp (dec i)) nxt (get cp (inc i))]
        :when (and prev nxt)
        :let [a (t/move prev (cp i))
              b (t/move (cp i) nxt)
              steps? (and (= 1 (:g a)) (= 1 (:g b)) (not (zero? (:dir a))))
              passing? (and steps? (= (:dir a) (:dir b)))
              neighbor? (and steps? (= (:dir a) (- (:dir b))))]
        :when (not (or passing? (and neighbor-ok? neighbor?)))]
    {:cols [(dec i) i (inc i)]
     :msg [:v/weak-dissonance v neighbor?]}))

(defmethod check :weak-dissonance-passing [_ ctx] (weak-dissonances ctx false))

(defmethod check :weak-dissonance [_ ctx] (weak-dissonances ctx true))

(defmethod check :downbeat-parallels [_ {:keys [verts per cf]}]
  (when (> per 1)
    (for [v verts
          :when (and v (zero? (:beat v)))
          :let [w (get verts (+ (:i v) per))]
          :when (and w (= :perfect (:cons v)) (= :perfect (:cons w)) (= (:g7 v) (:g7 w))
                     (not= (t/midi (cf (:bar v))) (t/midi (cf (:bar w)))))]
      {:cols (span (:i v) (:i w))
       :msg [:v/downbeat-parallels v w]})))

(defmethod check :final-whole [_ {:keys [per n cp final-slot]}]
  (when (and (> per 1) (> n 1) (get cp (inc final-slot)))
    [{:cols [(inc final-slot)] :msg [:v/final-whole]}]))

;; ---------------------------------------------------------------- motion rules

(defn- same-perfect? [{:keys [a b]}]
  (and (= :perfect (:cons a)) (= :perfect (:cons b)) (= (:g7 a) (:g7 b))))

(defmethod check :parallel-perfect [_ {:keys [motions]}]
  (for [m motions :when (and (= :parallel (:kind m)) (same-perfect? m))]
    {:cols [(:i m) (inc (:i m))]
     :msg [:v/parallel-perfect (:a m) (:b m)]}))

(defmethod check :contrary-perfect [_ {:keys [motions]}]
  (for [m motions :when (and (= :contrary (:kind m)) (same-perfect? m))]
    {:cols [(:i m) (inc (:i m))]
     :msg [:v/contrary-perfect (:a m) (:b m)]}))

(defn- upper-move [above? m] (if above? (:cp m) (:cf m)))

(defmethod check :direct-perfect [_ {:keys [motions]}]
  (for [m motions :when (and (= :similar (:kind m)) (= :perfect (:cons (:b m))))]
    {:cols [(:i m) (inc (:i m))]
     :msg [:v/direct-perfect (:a m) (:b m)]}))

(defmethod check :direct-perfect-leap [_ {:keys [motions above?]}]
  (for [m motions
        :when (and (= :similar (:kind m)) (= :perfect (:cons (:b m)))
                   (> (:g (upper-move above? m)) 1))]
    {:cols [(:i m) (inc (:i m))]
     :msg [:v/direct-perfect-leap (:a m) (:b m)]}))

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
       :msg [:v/parallel-run (inc (count run)) (:g7 (:a (first run)))]})))

(defmethod check :leap-together [_ {:keys [motions]}]
  (for [m motions
        :when (and (#{:similar :parallel} (:kind m)) (>= (:g (:cf m)) 3) (>= (:g (:cp m)) 3))]
    {:cols [(:i m) (inc (:i m))] :msg [:v/leap-together]}))

(defmethod check :static [_ {:keys [motions]}]
  (for [m motions :when (= :static (:kind m))]
    {:cols [(:i m) (inc (:i m))] :msg [:v/static]}))

;; ---------------------------------------------------------------- cadence

(defn- cadence-motion [{:keys [motions n final-slot]}]
  (when (> n 1) (first (filter #(= final-slot (inc (:i %))) motions))))

(defmethod check :cadence-step [_ ctx]
  (when-let [m (cadence-motion ctx)]
    (when-not (and (= :contrary (:kind m)) (= 1 (:g (:cf m))) (= 1 (:g (:cp m))))
      [{:cols [(:i m) (inc (:i m))]
        :msg [:v/cadence-step (:kind m) (not (and (= 1 (:g (:cf m))) (= 1 (:g (:cp m)))))]}])))

(defmethod check :leading-tone [_ ctx]
  (when-let [m (cadence-motion ctx)]
    (when-not (or (= 1 (Math/abs (:semis (:cf m)))) (= 1 (Math/abs (:semis (:cp m)))))
      [{:cols [(:i m) (inc (:i m))]
        :msg [:v/leading-tone]}])))

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
     :msg [:v/melodic-dissonance m]}))

(defmethod check-line :leap-size [_ {:keys [moves]}]
  (for [m moves :when (> (:g m) 7)]
    {:cols [(:i m) (inc (:i m))] :msg [:v/leap-size m]}))

(defmethod check-line :sixth [_ {:keys [moves]}]
  (for [m moves :when (and (= 5 (:g m)) (or (= :M (:q m)) (and (= :m (:q m)) (neg? (:dir m)))))]
    {:cols [(:i m) (inc (:i m))]
     :msg [:v/sixth m]}))

(defmethod check-line :leap-recovery [_ {:keys [moves]}]
  (for [[a b] (adjacent-pairs moves)
        :when (and (>= (:g a) 4) (not (and (= (:dir b) (- (:dir a))) (<= (:g b) 2))))]
    {:cols [(:i a) (inc (:i a)) (inc (:i b))]
     :msg [:v/leap-recovery a]}))

(defmethod check-line :leap-chain [_ {:keys [line moves]}]
  (let [skip? #(and (>= (:g %) 2) (not (zero? (:dir %))))
        pairs (adjacent-pairs moves)]
    (concat
     (for [[a b] pairs
           :when (and (skip? a) (skip? b) (= (:dir a) (:dir b)))
           :let [total (t/interval (line (:i a)) (line (inc (:i b))))]
           :when (or (> (:g total) 7) (= :dissonant (t/consonance total)))]
       {:cols [(:i a) (inc (:i a)) (inc (:i b))]
        :msg [:v/leap-chain total]})
     (for [[[a b] [_ c]] (partition 2 1 pairs)
           :when (and (= (:i c) (inc (:i b)))
                      (every? skip? [a b c]) (apply = (map :dir [a b c])))]
       {:cols (span (:i a) (inc (:i c))) :msg [:v/leap-chain-three]}))))

(defmethod check-line :repeat [_ {:keys [moves]}]
  (for [m moves :when (and (zero? (:g m)) (zero? (:semis m)))]
    {:cols [(:i m) (inc (:i m))] :msg [:v/repeat]}))

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
    {:cols (span i j) :msg [:v/tritone-outline x y]}))

(defmethod check-line :range [_ {:keys [line]}]
  (let [ns (keep-indexed #(when %2 [%1 %2]) line)]
    (when (seq ns)
      (let [[li lo] (apply min-key #(:s (second %)) ns)
            [hi-i hi] (apply max-key #(:s (second %)) ns)
            iv (t/interval lo hi)]
        (when (> (:g iv) 9)
          [{:cols (vec (sort [li hi-i])) :msg [:v/range iv]}])))))

(defmethod check-line :climax [_ {:keys [line]}]
  (let [ns (keep-indexed #(when %2 [%1 (t/midi %2)]) line)]
    (when (>= (count ns) 3)
      (let [top (apply max (map second ns))
            at (mapv first (filter #(= top (second %)) ns))]
        (when (> (count at) 1)
          [{:cols at :msg [:v/climax (line (first at)) (count at)]}])))))

(defmethod check-line :leaps-many [_ {:keys [moves]}]
  (let [skips (filter #(>= (:g %) 2) moves)
        big (filter #(>= (:g %) 3) moves)
        leaps (if (> (* 3 (count big)) (count moves)) big skips)]
    (when (and (>= (count moves) 5)
               (or (> (* 3 (count big)) (count moves)) (> (* 3 (count skips)) (* 2 (count moves)))))
      [{:cols (vec (distinct (mapcat #(vector (:i %) (inc (:i %))) leaps)))
        :msg [:v/leaps-many (count leaps) (count moves)]}])))

(defmethod check-line :mode [_ {:keys [line key lt-index]}]
  (for [[i note] (map-indexed vector line)
        :when (and note (not (t/in-mode? key note))
                   (not (and (= i lt-index) (t/leading-tone? key note))))]
    {:cols [i] :msg [:v/mode note key]}))

(defmethod check-line :cf-final [_ {:keys [line key n]}]
  (let [fpc (t/final-pc key)
        bad (cond-> []
              (and (first line) (not= fpc (t/pc (first line)))) (conj 0)
              (and (> n 1) (get line (dec n)) (not= fpc (t/pc (line (dec n))))) (conj (dec n)))]
    (when (seq bad)
      [{:cols bad :msg [:v/cf-final (:final key)]}])))

(defmethod check-line :cf-ending [_ {:keys [moves n]}]
  (when-let [m (and (> n 1) (first (filter #(= (- n 2) (:i %)) moves)))]
    (when (not= 1 (:g m))
      [{:cols [(- n 2) (dec n)] :msg [:v/cf-ending m]}])))

(defmethod check-line :cf-length [_ {:keys [n]}]
  (when (and (> n 1) (not (<= 8 n 16)))
    [{:cols [] :msg [:v/cf-length n]}]))

;; ---------------------------------------------------------------- analysis

(defn- tag [rule voice vs]
  (for [v vs] (assoc v :rule (:id rule) :level (:level rule) :kind (:kind rule) :voice voice)))

(defn analyze
  "All violations of `enabled` rules, ordered by position. Melodic rules run on the counterpoint,
  and on the cantus too when `check-cf?`; :cf rules only on the cantus."
  [exercise enabled check-cf?]
  (let [{:keys [cf cp n key final-slot] :as ctx} (context exercise)
        line-ctx (fn [line lt] {:line line :moves (line-moves line) :key key :n n :lt-index lt :final-slot final-slot})
        cp-ctx (line-ctx cp (dec final-slot))
        cf-ctx (line-ctx cf (- n 2))
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
