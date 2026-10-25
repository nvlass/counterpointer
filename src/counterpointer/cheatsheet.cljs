(ns counterpointer.cheatsheet
  (:require [counterpointer.rules :as rules]
            [counterpointer.library :as lib]
            [counterpointer.score :as score]
            [counterpointer.i18n :refer [tr]]))

(def c-major {:mode :ionian :final {:l 0 :a 0}})
(def d-dorian {:mode :dorian :final {:l 1 :a 0}})

(defn example [key upper lower caption & [species]]
  (let [species (or species 1)
        per (rules/per-bar species)
        e {:cf (lib/line key lower) :cp (lib/line key upper) :key key :position :above :species species}
        {:keys [verts motions]} (rules/context e)]
    [:figure.example
     [score/score (assoc e :n-cols (* per (count (:cf e))) :per per
                         :verts verts :motions motions :labels true :compact true)]
     [:figcaption caption]]))

(def intervals
  [[{:num 1 :q :P} 0 [0 0] :perfect]
   [{:num 2 :q :m} 1 [2 3] :dissonant]
   [{:num 2 :q :M} 2 [0 1] :dissonant]
   [{:num 3 :q :m} 3 [1 3] :imperfect]
   [{:num 3 :q :M} 4 [0 2] :imperfect]
   [{:num 4 :q :P} 5 [0 3] :dissonant]
   [:tritone 6 [3 6] :dissonant]
   [{:num 5 :q :P} 7 [0 4] :perfect]
   [{:num 6 :q :m} 8 [2 0] :imperfect]
   [{:num 6 :q :M} 9 [0 5] :imperfect]
   [{:num 7 :q :m} 10 [1 0] :dissonant]
   [{:num 7 :q :M} 11 [0 6] :dissonant]
   [{:num 8 :q :P} 12 [0 0] :perfect]])

(defn- pcs [[x y]] (str (tr :fmt/pc {:s x :a 0}) "–" (tr :fmt/pc {:s y :a 0})))

(defn interval-row [[iv semis eg cls]]
  [:tr
   [:td [:b (if (= iv :tritone)
              (str (tr :fmt/iv-name {:num 4 :q :A}) " / " (tr :fmt/iv-name {:num 5 :q :d}))
              (tr :fmt/iv-name iv))]]
   [:td (if (= iv :tritone) (tr :cheat/tritone) (tr :fmt/iv-long iv))]
   [:td.num semis]
   [:td (if (= iv :tritone) (str (pcs eg) " / " (pcs (reverse eg))) (pcs eg))]
   [:td [:span.cls {:class (name cls)} (tr (keyword "cheat" (str "class-" (name cls))))]]])

(defn rule-tags [{:keys [level presets species]}]
  [:<>
   [:span.tag {:class (name level)} (tr (keyword "level" (name level)))]
   (when-not (contains? presets :relaxed) [:span.tag.preset (tr :ui/strict-only)])
   (when-not (contains? presets :strict) [:span.tag.preset (tr :ui/relaxed-only)])
   (when species [:span.tag.preset (tr :ui/second-species)])])

(defn view []
  [:div.cheat
   [:div.card
    [:h3 (tr :cheat/idea-title)]
    (tr :cheat/idea)]

   [:div.card
    [:h3 (tr :cheat/intervals-title)]
    (tr :cheat/intervals)
    [:table.intervals
     [:thead [:tr [:th (tr :cheat/th-name)] [:th ""] [:th.num (tr :cheat/th-semitones)] [:th (tr :cheat/th-eg)] [:th (tr :cheat/th-class)]]]
     [:tbody
      (for [[i row] (map-indexed vector intervals)]
        ^{:key i} [interval-row row])]]]

   [:div.card
    [:h3 (tr :cheat/consonance-title)]
    (tr :cheat/consonance)]

   [:div.card
    [:h3 (tr :cheat/motion-title)]
    [:p (tr :cheat/motion-intro)]
    [:div.examples
     [example c-major "E4 G4" "C4 B3" (tr :cheat/motion-contrary)]
     [example c-major "E4 A4" "C4 C4" (tr :cheat/motion-oblique)]
     [example c-major "G4 B4" "C4 D4" (tr :cheat/motion-similar)]
     [example c-major "E4 F4" "C4 D4" (tr :cheat/motion-parallel)]
     [example c-major "G4 A4" "C4 D4" (tr :cheat/motion-parallel-5)]
     [example c-major "E4 A4" "C4 D4" (tr :cheat/motion-direct)]]
    [:p (tr :cheat/motion-rule)]]

   [:div.card
    [:h3 (tr :cheat/cadence-title)]
    [:div.examples
     [example d-dorian "C#4 D4" "E3 D3" (tr :cheat/cadence-above)]
     [example d-dorian "E4 D4" "C#4 D4" (tr :cheat/cadence-below)]]
    (tr :cheat/cadence)]

   [:div.card
    [:h3 (tr :cheat/second-title)]
    (tr :cheat/second)
    [:div.examples
     [example c-major "E4 F4 G4" "C4 B3" (tr :cheat/second-passing) 2]
     [example c-major "E4 F4 E4" "C4 A3" (tr :cheat/second-neighbour) 2]
     [example c-major "G4 E4 F4" "C4 D4" (tr :cheat/second-leap) 2]
     [example c-major "G4 E4 A4" "C4 D4" (tr :cheat/second-parallels) 2]]
    (tr :cheat/second-habits)
    [:div.examples
     [example d-dorian "B3 C#4 D4" "E3 D3" (tr :cheat/second-cadence) 2]]]

   [:div.card
    [:h3 (tr :cheat/terms-title)]
    (tr :cheat/terms)]

   [:div.card
    [:h3 (tr :cheat/rules-title)]
    (tr :cheat/rules-hint)
    (for [g rules/groups]
      ^{:key g}
      [:div.rule-group
       [:div.group-label (tr (keyword "group" (name g)))]
       [:ul.rule-list
        (for [{:keys [id] :as r} (filter #(= g (:group %)) rules/rules)]
          ^{:key id}
          [:li [rule-tags r] " " [:b (tr (keyword "rule" (name id)))]
           [:div (tr (keyword "rule.desc" (name id)))]])]])]

   [:div.card
    [:h3 (tr :cheat/about-title)]
    (tr :cheat/about (rules/penalty :error) (rules/penalty :warning))]])
