(ns counterpointer.events
  (:require [re-frame.core :as rf]
            [counterpointer.theory :as t]
            [counterpointer.rules :as rules]
            [counterpointer.library :as lib]
            [counterpointer.audio :as audio]
            [counterpointer.storage :as storage]
            [counterpointer.midi :as midi]))

(def default-settings
  {:preset :strict
   :rules (rules/preset-rules :strict)
   :check-cf true
   :labels true
   :hear :vertical
   :tempo 60
   :midi false})

(def centers {:treble 31 :bass 24})

(defn- center [position voice]
  (centers (if (= (= voice :cp) (not= position :below)) :treble :bass)))

(defn from-library [{:keys [key cf cp]} position]
  {:key key
   :position position
   :species 1
   :cf (t/recenter (lib/line key cf) (center position :cf))
   :cp (if cp (t/recenter (lib/line key cp) (center position :cp)) [])})

(def initial-db
  {:view :write
   :settings default-settings
   :exercise (from-library (first lib/fux) :above)
   :history []
   :future []
   :sel {:voice :cp :col 0}
   :focus nil
   :saved []
   :loaded? false
   :samples nil
   :text-error {}
   :play {:playing false :token 0 :col nil}
   :midi {:state :off :inputs []}})

;; ---------------------------------------------------------------- effects

(rf/reg-fx :audio/play (fn [events] (audio/stop-all!) (audio/play-events! events)))
(rf/reg-fx :audio/stop (fn [_] (audio/stop-all!)))
(rf/reg-fx :later
  (fn [xs] (doseq [{:keys [ms event]} xs] (js/setTimeout #(rf/dispatch event) ms))))
(rf/reg-fx :midi/connect (fn [on?] (if on? (midi/connect!) (midi/disconnect!))))

(def persist
  "Saves the persisted parts of the db whenever they change, once storage has loaded."
  (rf/->interceptor
   :id :persist
   :after (fn [ctx]
            (let [before (get-in ctx [:coeffects :db])
                  after (get-in ctx [:effects :db])]
              (when (and after (:loaded? before))
                (doseq [k [:settings :exercise :saved]
                        :when (not= (k before) (k after))]
                  (storage/save! k (k after)))))
            ctx)))

(rf/reg-global-interceptor persist)

;; ---------------------------------------------------------------- lifecycle

(rf/reg-event-db :init (fn [_ _] initial-db))

(rf/reg-event-fx :storage/loaded
  (fn [{:keys [db]} [_ {:keys [settings exercise saved]}]]
    (let [s (merge default-settings settings)
          s (if (= :custom (:preset s)) s (assoc s :rules (rules/preset-rules (:preset s))))]
      (cond-> {:db (cond-> (assoc db :settings s :saved (vec saved) :loaded? true)
                     exercise (assoc :exercise exercise))}
        (:midi s) (assoc :midi/connect true)))))

(rf/reg-event-db :audio/loaded (fn [db [_ n]] (assoc db :samples n)))

(rf/reg-event-db :set-view (fn [db [_ v]] (assoc db :view v)))

;; ---------------------------------------------------------------- settings

(rf/reg-event-fx :settings/set
  (fn [{:keys [db]} [_ k v]]
    (cond-> {:db (assoc-in db [:settings k] v)}
      (= k :midi) (assoc :midi/connect v))))

(rf/reg-event-db :settings/preset
  (fn [db [_ p]]
    (update db :settings assoc :preset p :rules (rules/preset-rules p))))

(rf/reg-event-db :settings/toggle-rule
  (fn [db [_ id]]
    (update db :settings
            (fn [s]
              (let [rs ((if (contains? (:rules s) id) disj conj) (:rules s) id)
                    p (some (fn [[p _]] (when (= rs (rules/preset-rules p)) p)) rules/presets)]
                (assoc s :rules rs :preset (or p :custom)))))))

;; ---------------------------------------------------------------- editing

(defn- edit
  "Applies `f` to the exercise, keeping undo history."
  [db f]
  (let [before (:exercise db)
        after (f before)]
    (if (= before after)
      db
      (-> db
          (assoc :exercise after :future [] :focus nil)
          (update :history #(vec (take-last 100 (conj % before))))))))

(defn n-cols [{:keys [cf cp]}]
  (min t/max-notes (max 8 (inc (max (count cf) (count cp))))))

(defn- note-at [db {:keys [voice col]}] (get-in db [:exercise voice col]))

(defn- audition [db {:keys [voice col]}]
  (let [{:keys [cf cp]} (:exercise db)
        dur 1.4]
    (case (get-in db [:settings :hear])
      :off nil
      :note (when-let [n (get-in db [:exercise voice col])] [[(t/midi n) 0 dur]])
      (seq (for [n [(get cf col) (get cp col)] :when n] [(t/midi n) 0 dur])))))

(defn- with-audition [db sel]
  (cond-> {:db db}
    (audition db sel) (assoc :audio/play (audition db sel))))

(defn- put-note [db {:keys [voice col]} note]
  (edit db #(update % voice t/put col note)))

(defn- ref-step
  "Where a newly typed note should go: near the previous note in the voice, else near the staff's middle."
  [db {:keys [voice col]}]
  (let [{:keys [position] :as e} (:exercise db)
        line (voice e)
        other (get-in e [({:cp :cf :cf :cp} voice) col])
        upper? (= (= voice :cp) (not= position :below))]
    (or (some :s (reverse (take col line)))
        (:s (get line col))
        (some-> other :s (+ (if upper? 4 -4)))
        (center position voice))))

(defn- clamp-col [db col] (max 0 (min (dec (n-cols (:exercise db))) col)))

(rf/reg-event-fx :pick
  (fn [{:keys [db]} [_ voice col step]]
    (let [sel {:voice voice :col col}
          key (get-in db [:exercise :key])
          old (note-at db sel)
          db (cond-> (assoc db :sel sel :last-entry nil)
               (not= step (:s old)) (put-note sel {:s step :a (t/default-alt key (mod step 7))}))]
      (with-audition db sel))))

(rf/reg-event-db :select (fn [db [_ sel]] (assoc db :sel sel :last-entry nil)))

(defn- step-note [db sel delta]
  (let [key (get-in db [:exercise :key])
        n (or (note-at db sel) {:s (ref-step db sel) :a 0})
        s (+ (:s n) delta)
        a (if (zero? (mod delta 7)) (:a n) (t/default-alt key (mod s 7)))]
    (put-note db sel {:s s :a a})))

(defn- alter-note [db sel f]
  (if-let [n (note-at db sel)]
    (put-note db sel (update n :a #(max -1 (min 1 (f %)))))
    db))

(defn- type-letter [db sel l]
  (let [key (get-in db [:exercise :key])
        db (put-note db sel (t/nearest key l (ref-step db sel)))
        next {:voice (:voice sel) :col (clamp-col db (inc (:col sel)))}
        db (assoc db :last-entry sel)]
    (assoc (with-audition db sel) :db (assoc db :sel next))))

(defn- move-sel [db delta]
  (-> db
      (update-in [:sel :col] #(clamp-col db (+ % delta)))
      (assoc :last-entry nil)))

(defn- edit-target
  "The selected bar, or the note just entered when the cursor has moved on to an empty bar."
  [db op]
  (let [sel (:sel db) last (:last-entry db)]
    (if (and (#{:up :raise :lower :natural} op) sel (not (note-at db sel))
             (= last (update sel :col dec)) (note-at db last))
      last
      sel)))

(rf/reg-event-fx :edit
  (fn [{:keys [db]} [_ op & args]]
    (if-let [sel (edit-target db op)]
      (let [after (case op
                    :up (step-note db sel (first args))
                    :raise (alter-note db sel inc)
                    :lower (alter-note db sel dec)
                    :natural (alter-note db sel (constantly 0))
                    :delete (put-note db sel nil)
                    :backspace (if (note-at db sel)
                                 (put-note db sel nil)
                                 (let [db (move-sel db -1)] (put-note db (:sel db) nil)))
                    db)]
        (if (#{:up :raise :lower :natural} op)
          (with-audition after sel)
          {:db after}))
      {:db db})))

(rf/reg-event-fx :type-letter
  (fn [{:keys [db]} [_ l]]
    (if-let [sel (:sel db)] (type-letter db sel l) {:db db})))

(rf/reg-event-db :move-sel
  (fn [db [_ delta]] (if (:sel db) (move-sel db delta) db)))

(rf/reg-event-db :switch-voice
  (fn [db _]
    (update db :sel #(if % (update % :voice {:cp :cf :cf :cp}) {:voice :cp :col 0}))))

(rf/reg-event-db :undo
  (fn [db _]
    (if-let [prev (peek (:history db))]
      (-> db
          (assoc :exercise prev :focus nil)
          (update :history pop)
          (update :future conj (:exercise db)))
      db)))

(rf/reg-event-db :redo
  (fn [db _]
    (if-let [nxt (peek (:future db))]
      (-> db
          (assoc :exercise nxt :focus nil)
          (update :future pop)
          (update :history conj (:exercise db)))
      db)))

(rf/reg-event-db :clear
  (fn [db [_ voice]]
    (-> (edit db #(assoc % voice []))
        (assoc :sel {:voice voice :col 0}))))

(rf/reg-event-db :apply-text
  (fn [db [_ voice text]]
    (let [{:keys [key position]} (:exercise db)
          {:keys [line error]} (t/parse-line key text (center position voice))]
      (if error
        (assoc-in db [:text-error voice] error)
        (-> (edit db #(assoc % voice line))
            (update :text-error dissoc voice))))))

(rf/reg-event-db :set-key
  (fn [db [_ key]] (edit db #(assoc % :key key))))

(rf/reg-event-db :set-position
  (fn [db [_ position]]
    (edit db (fn [e]
               (-> e
                   (assoc :position position)
                   (update :cf t/recenter (center position :cf))
                   (update :cp t/recenter (center position :cp)))))))

(rf/reg-event-db :load
  (fn [db [_ entry]]
    (let [position (get-in db [:exercise :position])]
      (-> (edit db (constantly (if (:exercise entry)
                                 (:exercise entry)
                                 (from-library entry position))))
          (assoc :sel {:voice :cp :col 0} :text-error {})))))

(rf/reg-event-db :save-as
  (fn [db [_ name]]
    (let [entry {:name name :exercise (:exercise db) :t (js/Date.now)}]
      (update db :saved #(conj (vec (remove (fn [e] (= name (:name e))) %)) entry)))))

(rf/reg-event-db :delete-saved
  (fn [db [_ name]] (update db :saved #(vec (remove (fn [e] (= name (:name e))) %)))))

(rf/reg-event-db :focus
  (fn [db [_ i]] (assoc db :focus (when-not (= i (:focus db)) i))))

;; ---------------------------------------------------------------- playback

(rf/reg-event-fx :play
  (fn [{:keys [db]} [_ which]]
    (let [{:keys [cf cp]} (:exercise db)
          len (max (count cf) (count cp))
          dur (/ 60 (get-in db [:settings :tempo]))
          token (inc (get-in db [:play :token]))
          voices (case which :cf [cf] :cp [cp] [cf cp])]
      (if (zero? len)
        {:db db}
        {:db (assoc db :play {:playing which :token token :col 0})
         :audio/play (for [line voices [i n] (map-indexed vector line) :when n]
                       [(t/midi n) (* i dur) (* 0.96 dur)])
         :later (for [i (range 1 (inc len))]
                  {:ms (* 1000 dur i) :event [:play-tick token (when (< i len) i)]})}))))

(rf/reg-event-db :play-tick
  (fn [db [_ token col]]
    (if (= token (get-in db [:play :token]))
      (assoc db :play {:playing (when col (get-in db [:play :playing])) :token token :col col})
      db)))

(rf/reg-event-fx :stop
  (fn [{:keys [db]} _]
    {:db (update db :play #(assoc % :playing false :col nil :token (inc (:token %))))
     :audio/stop true}))

(rf/reg-event-fx :toggle-play
  (fn [{:keys [db]} _]
    {:dispatch (if (get-in db [:play :playing]) [:stop] [:play :both])}))

;; ---------------------------------------------------------------- midi

(rf/reg-event-db :midi/status (fn [db [_ s]] (assoc db :midi s)))

(rf/reg-event-db :midi/note-on
  (fn [db [_ m]]
    (if-let [sel (:sel db)]
      (let [db (assoc (put-note db sel (t/spell (get-in db [:exercise :key]) m)) :last-entry sel)]
        (assoc db :sel (update sel :col #(clamp-col db (inc %)))))
      db)))

(rf/reg-event-db :midi/note-off (fn [db _] db))

;; ---------------------------------------------------------------- keyboard

(def letter-keys (zipmap ["c" "d" "e" "f" "g" "a" "b"] (range)))

(rf/reg-event-fx :key
  (fn [{:keys [db]} [_ k {:keys [shift mod]}]]
    (cond
      (and mod (= k "z")) {:dispatch (if shift [:redo] [:undo])}
      (and mod (= k "y")) {:dispatch [:redo]}
      mod {}
      (not= :write (:view db)) {}
      :else
      (if-let [ev (case k
                    "ArrowUp" [:edit :up (if shift 7 1)]
                    "ArrowDown" [:edit :up (if shift -7 -1)]
                    "ArrowLeft" [:move-sel -1]
                    "ArrowRight" [:move-sel 1]
                    "Tab" [:switch-voice]
                    ("#" "+") [:edit :raise]
                    "-" [:edit :lower]
                    "n" [:edit :natural]
                    "Backspace" [:edit :backspace]
                    "Delete" [:edit :delete]
                    " " [:toggle-play]
                    "Escape" [:select nil]
                    (when-let [l (letter-keys (.toLowerCase k))] [:type-letter l]))]
        {:dispatch ev}
        {}))))
