(ns counterpointer.views
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [clojure.string :as str]
            [counterpointer.theory :as t]
            [counterpointer.rules :as rules]
            [counterpointer.library :as lib]
            [counterpointer.score :as score]
            [counterpointer.cheatsheet :as cheat]))

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
          (= 1 (count cs)) (str "bar " (first cs))
          (= (count cs) (inc (- (last cs) (first cs)))) (str "bars " (first cs) "–" (last cs))
          :else (str "bars " (str/join ", " cs)))))

(defn bars
  "Labels violation columns: bars for the cantus, slots (shown as bars and beats) otherwise."
  [cols voice per]
  (if (and (> per 1) (not= voice :cf))
    (if (= 1 (count cols))
      (let [c (first cols)] (str "bar " (inc (quot c per)) (if (zero? (mod c per)) ", downbeat" ", upbeat")))
      (bar-span (distinct (map #(quot % per) cols))))
    (bar-span cols)))

;; ---------------------------------------------------------------- setup

(defn key-picker [{:keys [mode final] :as key}]
  [:div.key-row
   [:select {:value (name mode)
             :on-change #(let [m (keyword (.. % -target -value))
                               f (if (some #{final} (t/finals m)) final (first (t/finals m)))]
                           (>evt [:set-key {:mode m :final f}]))}
    (for [m t/modes] ^{:key (:id m)} [:option {:value (name (:id m))} (:name m)])]
   [:select {:value (str (:l final) "/" (:a final))
             :on-change #(let [[l a] (map js/parseInt (str/split (.. % -target -value) "/"))]
                           (>evt [:set-key {:mode mode :final {:l l :a a}}]))}
    (for [f (sort-by (juxt :l :a) (t/finals mode))]
      ^{:key (str (:l f) (:a f))} [:option {:value (str (:l f) "/" (:a f))} (str "on " (t/final-name f))])]
   [:span.hint (let [s (t/signature key)]
                 (cond (zero? s) "no sharps or flats"
                       (pos? s) (str s " ♯")
                       :else (str (- s) " ♭")))]])

(defn setup-card []
  (let [{:keys [key position species]} (<sub [:exercise])]
    [:div.card.setup
     [:div.field [:span "Mode"] [key-picker key]]
     [:div.row2
      [:div.field [:span "Counterpoint"]
       [seg [[:above "above the cantus"] [:below "below"]] position #(>evt [:set-position %])]]
      [:div.field [:span "Species"]
       [seg [[1 "1st · 1:1"] [2 "2nd · 2:1"] [3 "3rd · 4:1" true] [4 "4th" true] [5 "florid" true]]
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
     (when err [:small.err-text err])]))

(defn edit-toolbar []
  (let [sel (<sub [:sel])
        e (<sub [:exercise])
        note (when sel (get-in e [(:voice sel) (:col sel)]))]
    [:div.toolbar
     [:span.sel-info
      (if sel
        [:<> [:b (if (= :cp (:voice sel)) "Counterpoint" "Cantus")] " · "
         (bars [(:col sel)] (:voice sel) (<sub [:per]))
         (when note [:<> " · " (t/note-name note)])]
        "Click the staff to place a note")]
     [:div.tools
      [:button {:title "Up a step (↑, ⇧↑ octave)" :on-click #(>evt [:edit :up 1])} "↑"]
      [:button {:title "Down a step (↓, ⇧↓ octave)" :on-click #(>evt [:edit :up -1])} "↓"]
      [:button {:title "Sharpen (#)" :disabled (not note) :on-click #(>evt [:edit :raise])} "♯"]
      [:button {:title "Flatten (-)" :disabled (not note) :on-click #(>evt [:edit :lower])} "♭"]
      [:button {:title "Natural (n)" :disabled (not note) :on-click #(>evt [:edit :natural])} "♮"]
      [:button {:title "Delete (⌫)" :on-click #(>evt [:edit :backspace])} "⌫"]
      [:button {:title "Switch voice (Tab)" :on-click #(>evt [:switch-voice])} "⇅"]
      [:button {:title "Undo (⌘Z)" :disabled (empty? (<sub [:history])) :on-click #(>evt [:undo])} "↶"]
      [:button {:title "Redo (⇧⌘Z)" :disabled (empty? (<sub [:future])) :on-click #(>evt [:redo])} "↷"]]]))

(defn play-bar []
  (let [{:keys [playing]} (<sub [:play])
        tempo (:tempo (<sub [:settings]))]
    [:div.play-bar
     (if playing
       [:button.primary {:on-click #(>evt [:stop])} "■ Stop"]
       [:button.primary {:on-click #(>evt [:play :both])} "▶ Play"])
     [:button {:on-click #(>evt [:play :cf])} "Cantus"]
     [:button {:on-click #(>evt [:play :cp])} "Counterpoint"]
     [:div.tempo [slider "Tempo" tempo 30 120 2 " bpm" #(>evt [:settings/set :tempo %])]]]))

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
       [:div.legend "Between the staves: interval numbers, and the motion into each bar: "
        [:b "c"] "ontrary, " [:b "o"] "blique, " [:b "s"] "imilar, " [:b "p"] "arallel."])
     [play-bar]
     [:details.typing
      [:summary "Type or paste notes"]
      [text-entry (if (= position :below) :cf :cp) (if (= position :below) "Cantus (upper)" "Counterpoint (upper)")]
      [text-entry (if (= position :below) :cp :cf) (if (= position :below) "Counterpoint (lower)" "Cantus (lower)")]
      (when (> per 1)
        [:p.hint "Second species: two half notes per bar; start with " [:code "-"] " for a half rest, and end on a single note (the whole note). " [:code "|"] " between bars is optional."])
      [:p.hint "Letters with optional ♯/♭ (# or b) and octave: " [:code "D F E D"] " or " [:code "D4 C#5 Bb3"]
       ". Without an octave a note goes to the nearest one. " [:code "-"] " leaves a bar empty; "
       [:code "n"] " forces a natural (" [:code "Bn4"] ")."]
      [:p.hint "Keys: " [:kbd "A"] "–" [:kbd "G"] " enter a note and move on, " [:kbd "↑"] [:kbd "↓"]
       " move it (⇧ by octave), " [:kbd "#"] " " [:kbd "-"] " " [:kbd "n"] " accidentals, "
       [:kbd "←"] [:kbd "→"] " move, " [:kbd "Tab"] " other voice, " [:kbd "⌫"] " delete, "
       [:kbd "Space"] " play, " [:kbd "⌘Z"] " undo."]]]))

;; ---------------------------------------------------------------- results

(defn violation-row [i {:keys [rule level voice cols msg]} focus per]
  (let [r (rules/rule-by-id rule)]
    [:li {:class (str (name level) (when (= i focus) " focus"))
          :on-click #(>evt [:focus i])}
     [:span.dot]
     [:div.v-body
      [:div.v-msg
       (when voice [:span.voice (if (= voice :cp) "CP" "CF")])
       msg]
      [:div.v-meta (bars cols voice per) (when (seq cols) " · ") (:title r)]]
     [:button.link {:title "Turn this rule off"
                    :on-click (fn [e] (.stopPropagation e) (>evt [:settings/toggle-rule rule]))} "mute"]]))

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
       [:h3 "Rule check"]
       [:div.hint (cond (zero? n) "Start with a cantus firmus (bottom of the page has Fux's)."
                        (< filled expected) (str "Counterpoint: " filled " of " expected " notes")
                        :else (str "Complete · " n " bars"))]]
      [:div.penalty {:class (cond (pos? errors) "bad" (pos? warnings) "warn" :else "good")}
       [:b penalty] [:small "penalty"]]]
     (if (empty? vs)
       [:p.clean "No rule violations."]
       [:ul.violations
        (for [[i v] (map-indexed vector vs)]
          ^{:key i} [violation-row i v focus (<sub [:per])])])
     [:p.hint "Errors cost " (rules/penalty :error) ", warnings " (rules/penalty :warning)
      ". Nothing is blocked; tap a row to highlight it on the staff."]]))

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
       [:h3 "Shape " [:small "descriptive, not scored"]]
       [:div.stat-cols
        [:div
         [:div.group-label "Motion"]
         [meter-row "contrary" (pct :contrary motion motions)]
         [meter-row "oblique" (pct :oblique motion motions)]
         [meter-row "similar" (pct :similar motion motions)]
         [meter-row "parallel" (pct :parallel motion motions)]]
        [:div
         [:div.group-label "Intervals"]
         [meter-row "imperfect" (pct :imperfect cons verts)]
         [meter-row "perfect" (pct :perfect cons verts)]
         [meter-row "dissonant" (pct :dissonant cons verts)]]
        [:div
         [:div.group-label "Counterpoint melody"]
         [meter-row "steps" (pct :step melody moves)]
         [meter-row "3rds" (pct :skip melody moves)]
         [meter-row "leaps" (pct :leap melody moves)]
         [meter-row "repeats" (pct :repeat melody moves)]]]
       [:p.hint "Rough targets: contrary motion is the most common, imperfect consonances outnumber perfect ones, and the line moves mostly by step."]])))

;; ---------------------------------------------------------------- library

(defn library-card []
  (let [name (r/atom "")]
    (fn []
      (let [saved (<sub [:saved])]
        [:div.card.library
         [:h3 "Cantus firmi"]
         [:div.chips
          (for [entry lib/fux]
            ^{:key (:name entry)}
            [:button.chip {:on-click #(>evt [:load entry])} (subs (:name entry) 6)])
          (for [entry lib/examples]
            ^{:key (:name entry)}
            [:button.chip {:on-click #(>evt [:load entry])} (str "Example: " (:name entry))])]
         [:p.hint "Loading a cantus replaces the exercise (undo brings it back) and clears the counterpoint, keeping the species and position."]
         [:h3 "Saved"]
         (when (seq saved)
           [:ul.saved
            (for [{n :name :as entry} (sort-by :name saved)]
              ^{:key n}
              [:li [:button.link {:on-click #(>evt [:load entry])} n]
               [:span.hint (t/key-name (get-in entry [:exercise :key]))]
               [:button.link.del {:on-click #(>evt [:delete-saved n])} "delete"]])])
         [:div.file-row
          [:input.name {:value @name :placeholder "Name this exercise"
                        :on-change #(reset! name (.. % -target -value))}]
          [:button {:disabled (str/blank? @name)
                    :on-click #(do (>evt [:save-as (str/trim @name)]) (reset! name ""))} "Save"]]]))))

(defn write-view []
  [:<>
   [setup-card]
   [score-card]
   [results-card]
   [stats-card]
   [library-card]])

;; ---------------------------------------------------------------- rules & settings

(defn rules-view []
  (let [{:keys [preset check-cf labels hear midi] :as settings} (<sub [:settings])
        enabled (:rules settings)
        midi-state (<sub [:midi])]
    [:<>
     [:div.card
      [:h3 "Rule set"]
      [seg (conj (vec rules/presets) [:custom "Custom" (not= preset :custom)]) preset #(>evt [:settings/preset %])]
      [:p.hint "Strict follows Fux closely. Relaxed keeps the core rules and drops some style points. Turning single rules on or off makes it Custom."]
      [toggle "Check the cantus firmus too" check-cf #(>evt [:settings/set :check-cf %])
       "Melodic rules apply to the cantus as well, useful when you write your own."]]
     (for [[g label] rules/groups]
       ^{:key g}
       [:div.card
        [:h3 label]
        (for [{:keys [id title desc level presets species]} (filter #(= g (:group %)) rules/rules)]
          ^{:key id}
          [:label.toggle.rule
           [:input {:type "checkbox" :checked (contains? enabled id)
                    :on-change #(>evt [:settings/toggle-rule id])}]
           [:span [:span.tag {:class (name level)} (name level)] " " title
            (when-not (contains? presets :relaxed) [:span.tag.preset "strict only"])
            (when-not (contains? presets :strict) [:span.tag.preset "relaxed only"])
            (when species [:span.tag.preset "2nd species"])
            [:small desc]]])])
     [:div.card
      [:h3 "Settings"]
      [:div.field [:span "When placing a note, play"]
       [seg [[:vertical "both notes"] [:note "the note"] [:off "nothing"]] hear #(>evt [:settings/set :hear %])]]
      [toggle "Interval numbers and motion between the staves" labels #(>evt [:settings/set :labels %])]
      [toggle "MIDI keyboard input" midi #(>evt [:settings/set :midi %])
       (case (:state midi-state)
         :ready (if (seq (:inputs midi-state))
                  (str "Connected: " (str/join ", " (:inputs midi-state)) ". Notes go to the selected bar.")
                  "No MIDI devices found.")
         :unsupported "This browser has no Web MIDI."
         :denied "MIDI access was denied."
         "Played notes enter the selected bar and move on.")]]]))

;; ---------------------------------------------------------------- app

(defn app []
  (let [view (<sub [:view])]
    [:div.app
     [:header
      [:h1 "Counterpointer"]
      (when-not (<sub [:samples]) [:span.badge "loading sounds…"])]
     [:nav
      (for [[v label] [[:write "Write"] [:cheat "Cheat sheet"] [:rules "Rules & settings"]]]
        ^{:key v}
        [:button {:class (when (= v view) "on") :on-click #(>evt [:set-view v])} label])]
     (case view
       :cheat [cheat/view]
       :rules [rules-view]
       [write-view])]))
