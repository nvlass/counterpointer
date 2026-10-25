(ns counterpointer.views
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [clojure.string :as str]
            [counterpointer.theory :as t]
            [counterpointer.rules :as rules]
            [counterpointer.library :as lib]
            [counterpointer.score :as score]
            [counterpointer.cheatsheet :as cheat]
            [counterpointer.guide :as guide]
            [counterpointer.hints :as hints]
            [counterpointer.i18n :as i18n :refer [tr tr-msg]]))

(defn <sub [q] @(rf/subscribe q))
(defn >evt [e] (rf/dispatch e))

(defn seg [options value on-change]
  [:div.seg
   (for [[v label & [disabled]] options]
     ^{:key (str v)}
     [:button {:class (when (= v value) "on") :disabled disabled :on-click #(on-change v)} label])])

(defn toggle [label value on-change & [hint]]
  [:label.toggle
   [:input {:type "checkbox" :checked (boolean value) :on-change #(on-change (not value))}]
   [:span label (when hint [:small hint])]])

(defn slider [label value lo hi step unit on-change]
  [:label.slider
   [:span label [:b value unit]]
   [:input {:type "range" :min lo :max hi :step step :value value
            :on-change #(on-change (js/parseFloat (.. % -target -value)))}]])

(defn- bar-span [cols]
  (let [cs (map inc (sort cols))]
    (cond (empty? cs) nil
          (= 1 (count cs)) (tr :bars/one (first cs))
          (= (count cs) (inc (- (last cs) (first cs)))) (tr :bars/range (first cs) (last cs))
          :else (tr :bars/list cs))))

(defn bars
  "Labels violation columns: bars for the cantus, slots (shown as bars and beats) otherwise."
  [cols voice per]
  (if (and (> per 1) (not= voice :cf))
    (if (= 1 (count cols))
      (let [c (first cols)] (tr :bars/beat (inc (quot c per)) (zero? (mod c per))))
      (bar-span (distinct (map #(quot % per) cols))))
    (bar-span cols)))

;; ---------------------------------------------------------------- setup

(defn key-picker [{:keys [mode final] :as key}]
  [:div.key-row
   [:select {:value (name mode)
             :on-change #(let [m (keyword (.. % -target -value))
                               f (if (some #{final} (t/finals m)) final (first (t/finals m)))]
                           (>evt [:set-key {:mode m :final f}]))}
    (for [m t/modes] ^{:key (:id m)} [:option {:value (name (:id m))} (tr :fmt/mode (:id m))])]
   [:select {:value (str (:l final) "/" (:a final))
             :on-change #(let [[l a] (map js/parseInt (str/split (.. % -target -value) "/"))]
                           (>evt [:set-key {:mode mode :final {:l l :a a}}]))}
    (for [f (sort-by (juxt :l :a) (t/finals mode))]
      ^{:key (str (:l f) (:a f))} [:option {:value (str (:l f) "/" (:a f))} (tr :fmt/on-final f)])]
   [:span.hint (tr :fmt/signature (t/signature key))]])

(defn setup-card []
  (let [{:keys [key position species]} (<sub [:exercise])]
    [:div.card.setup
     [:div.field [:span (tr :ui/mode)] [key-picker key]]
     [:div.row2
      [:div.field [:span (tr :ui/counterpoint)]
       [seg [[:above (tr :ui/above)] [:below (tr :ui/below)]] position #(>evt [:set-position %])]]
      [:div.field [:span (tr :ui/species)]
       [seg [[1 (tr :ui/species-1)] [2 (tr :ui/species-2)] [3 (tr :ui/species-3) true] [4 (tr :ui/species-4) true] [5 (tr :ui/species-5) true]]
        (or species 1) #(>evt [:set-species %])]]]]))

;; ---------------------------------------------------------------- score

(defn text-entry [voice label]
  (let [{:keys [key] :as e} (<sub [:exercise])
        per (<sub [:per])
        text (if (and (= voice :cp) (> per 1))
               (str/join " | " (map #(t/line->text key %) (partition-all per (:cp e))))
               (t/line->text key (voice e)))
        err (get (<sub [:text-error]) voice)
        apply! #(>evt [:apply-text voice (.. % -target -value)])]
    [:label.text-entry
     [:span label]
     ^{:key text}
     [:input {:default-value text :spellCheck false :autoCapitalize "characters"
              :placeholder "D F E D G F A G F E D"
              :on-blur apply!
              :on-key-down #(when (= "Enter" (.-key %)) (apply! %) (.blur (.-target %)))}]
     (when err [:small.err-text (tr-msg err)])]))

(defn edit-toolbar []
  (let [sel (<sub [:sel])
        e (<sub [:exercise])
        note (when sel (get-in e [(:voice sel) (:col sel)]))]
    [:div.toolbar
     [:span.sel-info
      (if sel
        [:<> [:b (tr (if (= :cp (:voice sel)) :ui/counterpoint :ui/cantus))] " · "
         (bars [(:col sel)] (:voice sel) (<sub [:per]))
         (when note [:<> " · " (tr :fmt/note note)])]
        (tr :ui/click-staff))]
     [:div.tools
      [:button {:title (tr :ui/tool-up) :on-click #(>evt [:edit :up 1])} "↑"]
      [:button {:title (tr :ui/tool-down) :on-click #(>evt [:edit :up -1])} "↓"]
      [:button {:title (tr :ui/tool-sharp) :disabled (not note) :on-click #(>evt [:edit :raise])} "♯"]
      [:button {:title (tr :ui/tool-flat) :disabled (not note) :on-click #(>evt [:edit :lower])} "♭"]
      [:button {:title (tr :ui/tool-natural) :disabled (not note) :on-click #(>evt [:edit :natural])} "♮"]
      [:button {:title (tr :ui/tool-delete) :on-click #(>evt [:edit :backspace])} "⌫"]
      [:button {:title (tr :ui/tool-voice) :on-click #(>evt [:switch-voice])} "⇅"]
      [:button {:title (tr :ui/tool-undo) :disabled (empty? (<sub [:history])) :on-click #(>evt [:undo])} "↶"]
      [:button {:title (tr :ui/tool-redo) :disabled (empty? (<sub [:future])) :on-click #(>evt [:redo])} "↷"]]]))

(defn play-bar []
  (let [{:keys [playing]} (<sub [:play])
        tempo (:tempo (<sub [:settings]))]
    [:div.play-bar
     (if playing
       [:button.primary {:on-click #(>evt [:stop])} (tr :ui/stop)]
       [:button.primary {:on-click #(>evt [:play :both])} (tr :ui/play)])
     [:button {:on-click #(>evt [:play :cf])} (tr :ui/cantus)]
     [:button {:on-click #(>evt [:play :cp])} (tr :ui/counterpoint)]
     [:div.tempo [slider (tr :ui/tempo) tempo 30 120 2 " bpm" #(>evt [:settings/set :tempo %])]]]))

(defn score-card []
  (let [{:keys [cf cp key position]} (<sub [:exercise])
        {:keys [verts motions]} (<sub [:context])
        vs (<sub [:violations])
        focus (<sub [:focus])
        per (<sub [:per])
        settings (<sub [:settings])
        focused (when focus (get vs focus))]
    [:div.card.score-card
     [edit-toolbar]
     [:div.score-wrap
      [score/score {:cf cf :cp cp :key key :position position
                    :n-cols (<sub [:n-cols])
                    :verts verts :motions motions
                    :marks (<sub [:marks])
                    :sel (<sub [:sel])
                    :per per
                    :highlight (if (= :cf (:voice focused))
                                 (mapcat #(range (* per %) (* per (inc %))) (:cols focused))
                                 (:cols focused))
                    :playing (:col (<sub [:play]))
                    :labels (:labels settings)
                    :on-pick (fn [voice col step] (>evt [:pick voice col step]))}]]
     (when (:labels settings)
       [:div.legend (tr :ui/legend)])
     [play-bar]
     [:details.typing
      [:summary (tr :ui/type-notes)]
      [text-entry (if (= position :below) :cf :cp) (tr (if (= position :below) :ui/cantus-upper :ui/cp-upper))]
      [text-entry (if (= position :below) :cp :cf) (tr (if (= position :below) :ui/cp-lower :ui/cantus-lower))]
      (when (> per 1)
        [:p.hint (tr :ui/typing-second)])
      [:p.hint (tr :ui/typing-notes)]
      [:p.hint (tr :ui/typing-keys)]]]))

;; ---------------------------------------------------------------- results

(defn violation-row [i {:keys [rule level voice cols msg]} focus per]
  [:li {:class (str (name level) (when (= i focus) " focus"))
        :on-click #(>evt [:focus i])}
   [:span.dot]
   [:div.v-body
    [:div.v-msg
     (when voice [:span.voice (tr (if (= voice :cp) :ui/cp-tag :ui/cf-tag))])
     (tr-msg msg)]
    [:div.v-meta (bars cols voice per) (when (seq cols) " · ") (tr (keyword "rule" (name rule)))]
    (when (= i focus)
      [:div.v-fix (tr :ui/try) (tr (keyword "fix" (name rule)))])]
   [:button.link {:title (tr :ui/mute-title)
                  :on-click (fn [e] (.stopPropagation e) (>evt [:settings/toggle-rule rule]))} (tr :ui/mute)]])

(defn results-card []
  (let [vs (<sub [:violations])
        {:keys [errors warnings penalty]} (<sub [:score])
        {:keys [cf cp]} (<sub [:exercise])
        per (<sub [:per])
        focus (<sub [:focus])
        n (count cf)
        expected (if (pos? n) (inc (* per (dec n))) 0)
        filled (count (filter some? (take expected cp)))
        expected (if (and (> per 1) (nil? (first cp))) (dec expected) expected)]
    [:div.card.results
     [:div.summary
      [:div
       [:h3 (tr :ui/rule-check)]
       [:div.hint (cond (zero? n) (tr :ui/start-with-cf)
                        (< filled expected) (tr :ui/progress filled expected)
                        :else (tr :ui/complete n))]]
      [:div.penalty {:class (cond (pos? errors) "bad" (pos? warnings) "warn" :else "good")}
       [:b penalty] [:small (tr :ui/penalty)]]]
     (if (empty? vs)
       [:p.clean (tr :ui/no-violations)]
       [:ul.violations
        (for [[i v] (map-indexed vector vs)]
          ^{:key i} [violation-row i v focus (<sub [:per])])])
     [:p.hint (tr :ui/results-hint (rules/penalty :error) (rules/penalty :warning))]]))

(defn candidate-chip [e voice col {:keys [note errors warnings msgs]}]
  (let [iv (hints/interval-to-partner e voice col note)]
    [:button.chip.cand {:class (cond (pos? errors) "bad" (pos? warnings) "warn")
                        :title (if (seq msgs) (str/join "\n" (map tr-msg msgs)) (tr :ui/no-rule-broken))
                        :on-click #(>evt [:place voice col note])}
     [:b (tr :fmt/note note)] (when iv [:small iv])]))

(defn hints-card []
  (let [{:keys [hints candidates]} (<sub [:settings])]
    (when hints
      (let [tips (<sub [:tips])
            sel (<sub [:sel])
            e (<sub [:exercise])
            cands (<sub [:candidates])
            clean (filter #(and (zero? (:errors %)) (zero? (:warnings %))) cands)
            warned (filter #(and (zero? (:errors %)) (pos? (:warnings %))) cands)]
        [:div.card.hints-now
         [:h3 (tr :ui/hints) (when sel [:small (tr (if (= :cp (:voice sel)) :ui/hints-cp :ui/hints-cf))
                                         (bars [(:col sel)] (:voice sel) (<sub [:per]))])]
         [:ul.tips (for [[i tip] (map-indexed vector tips)] ^{:key i} [:li (tr-msg tip)])]
         (when (and sel (seq (:cf e)))
           (if candidates
             [:div.cands
              [:div.group-label (tr :ui/clean-notes)
               [:button.link {:on-click #(>evt [:settings/set :candidates false])} (tr :ui/hide)]]
              (if (seq clean)
                [:div.chips (for [c (take 10 clean)] ^{:key (t/note-name (:note c))} [candidate-chip e (:voice sel) (:col sel) c])]
                [:p.hint (tr :ui/no-clean)])
              (when (seq warned)
                [:<>
                 [:div.group-label (tr :ui/warned-notes)]
                 [:div.chips (for [c (take 8 warned)] ^{:key (t/note-name (:note c))} [candidate-chip e (:voice sel) (:col sel) c])]])
              [:p.hint (tr :ui/cands-hint)]]
             [:button.ghost {:on-click #(>evt [:settings/set :candidates true])} (tr :ui/show-cands)]))]))))

(defn pct [k m total] (if (pos? total) (Math/round (* 100 (/ (get m k 0) total))) 0))

(defn meter-row [label v hint]
  [:div.meter-row
   [:span label]
   [:span.meter [:span {:style {:width (str v "%")}}]]
   [:b v "%"]
   (when hint [:small hint])])

(defn stats-card []
  (let [{:keys [motion motions cons verts melody moves]} (<sub [:stats])]
    (when (pos? verts)
      [:div.card.stats
       [:h3 (tr :ui/shape) " " [:small (tr :ui/shape-sub)]]
       [:div.stat-cols
        [:div
         [:div.group-label (tr :ui/motion)]
         (for [k [:contrary :oblique :similar :parallel]]
           ^{:key k} [meter-row (tr :fmt/motion k) (pct k motion motions)])]
        [:div
         [:div.group-label (tr :ui/intervals)]
         (for [k [:imperfect :perfect :dissonant]]
           ^{:key k} [meter-row (tr (keyword "ui" (name k))) (pct k cons verts)])]
        [:div
         [:div.group-label (tr :ui/cp-melody)]
         (for [[k label] [[:step :ui/steps] [:skip :ui/thirds] [:leap :ui/leaps] [:repeat :ui/repeats]]]
           ^{:key k} [meter-row (tr label) (pct k melody moves)])]]
       [:p.hint (tr :ui/shape-hint)]])))

;; ---------------------------------------------------------------- library

(defn library-card []
  (let [name (r/atom "")]
    (fn []
      (let [saved (<sub [:saved])]
        [:div.card.library
         [:h3 (tr :ui/cantus-firmi)]
         [:div.chips
          (for [entry lib/fux]
            ^{:key (:name entry)}
            [:button.chip {:on-click #(>evt [:load entry])} (tr :fmt/key (:key entry))])
          (for [entry lib/examples]
            ^{:key (:name entry)}
            [:button.chip {:on-click #(>evt [:load entry])} (tr :ui/example (:species entry) (:key entry))])]
         [:p.hint (tr :ui/library-hint)]
         [:h3 (tr :ui/saved)]
         (when (seq saved)
           [:ul.saved
            (for [{n :name :as entry} (sort-by :name saved)]
              ^{:key n}
              [:li [:button.link {:on-click #(>evt [:load entry])} n]
               [:span.hint (tr :fmt/key (get-in entry [:exercise :key]))]
               [:button.link.del {:on-click #(>evt [:delete-saved n])} (tr :ui/delete)]])])
         [:div.file-row
          [:input.name {:value @name :placeholder (tr :ui/name-exercise)
                        :on-change #(reset! name (.. % -target -value))}]
          [:button {:disabled (str/blank? @name)
                    :on-click #(do (>evt [:save-as (str/trim @name)]) (reset! name ""))} (tr :ui/save)]]]))))

(defn write-view []
  [:<>
   [setup-card]
   [score-card]
   [hints-card]
   [results-card]
   [stats-card]
   [library-card]])

;; ---------------------------------------------------------------- rules & settings

(defn rules-view []
  (let [{:keys [preset check-cf labels hear midi note-names] :as settings} (<sub [:settings])
        enabled (:rules settings)
        midi-state (<sub [:midi])
        names (i18n/solfege (:lang settings))]
    [:<>
     [:div.card
      [:h3 (tr :ui/rule-set)]
      [seg (conj (mapv (fn [p] [p (tr (keyword "preset" (name p)))]) rules/presets)
                 [:custom (tr :preset/custom) (not= preset :custom)])
       preset #(>evt [:settings/preset %])]
      [:p.hint (tr :ui/rule-set-hint)]
      [toggle (tr :ui/check-cf) check-cf #(>evt [:settings/set :check-cf %])
       (tr :ui/check-cf-hint)]]
     (for [g rules/groups]
       ^{:key g}
       [:div.card
        [:h3 (tr (keyword "group" (name g)))]
        (for [{:keys [id] :as r} (filter #(= g (:group %)) rules/rules)]
          ^{:key id}
          [:label.toggle.rule
           [:input {:type "checkbox" :checked (contains? enabled id)
                    :on-change #(>evt [:settings/toggle-rule id])}]
           [:span [cheat/rule-tags r] " " (tr (keyword "rule" (name id)))
            [:small (tr (keyword "rule.desc" (name id)))]]])])
     [:div.card
      [:h3 (tr :ui/settings)]
      [:div.field [:span (tr :ui/hear)]
       [seg [[:vertical (tr :ui/hear-both)] [:note (tr :ui/hear-note)] [:off (tr :ui/hear-off)]] hear #(>evt [:settings/set :hear %])]]
      [:div.field [:span (tr :ui/note-names)]
       [seg [[:letters "C D E"] [:solfege (str/join " " (take 3 names))]] note-names #(>evt [:settings/set :note-names %])]]
      [toggle (tr :ui/labels-toggle) labels #(>evt [:settings/set :labels %])]
      [toggle (tr :ui/hints-toggle) (:hints settings) #(>evt [:settings/set :hints %])
       (tr :ui/hints-toggle-hint)]
      [toggle (tr :ui/midi-toggle) midi #(>evt [:settings/set :midi %])
       (case (:state midi-state)
         :ready (if (seq (:inputs midi-state))
                  (tr :ui/midi-connected (:inputs midi-state))
                  (tr :ui/midi-none))
         :unsupported (tr :ui/midi-unsupported)
         :denied (tr :ui/midi-denied)
         (tr :ui/midi-off))]]]))

;; ---------------------------------------------------------------- app

(defn app []
  (let [view (<sub [:view])
        lang (:lang (<sub [:settings]))]
    [:div.app
     [:header
      [:h1 "Counterpointer"]
      [:div.header-end
       (when-not (<sub [:samples]) [:span.badge (tr :ui/loading-sounds)])
       [:div.lang {:title (tr :ui/language)}
        [seg [[:en "EN"] [:el "ΕΛ"]] lang #(>evt [:settings/set :lang %])]]]]
     [:nav
      (for [v [:write :hints :cheat :rules]]
        ^{:key v}
        [:button {:class (when (= v view) "on") :on-click #(>evt [:set-view v])} (tr (keyword "nav" (name v)))])]
     (case view
       :cheat [cheat/view]
       :hints [guide/view]
       :rules [rules-view]
       [write-view])]))
