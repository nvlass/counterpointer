(ns counterpointer.guide
  (:require [counterpointer.i18n :refer [tr]]))

(defn view []
  [:div.cheat
   [:div.card
    [:h3 (tr :guide/work-title)]
    (tr :guide/work)]

   [:div.card
    [:h3 (tr :guide/first-title)]
    (tr :guide/first)]

   [:div.card
    [:h3 (tr :guide/second-title)]
    (tr :guide/second)]

   [:div.card
    [:h3 (tr :guide/fixes-title)]
    [:p.hint (tr :guide/fixes-hint)]
    [:dl.fixes
     (for [id [:parallel-perfect :direct-perfect :consonance :weak-dissonance-passing :leading-tone :cadence-step
               :parallel-run :imperfect-majority :leap-recovery :climax :tritone-outline :range]]
       ^{:key id}
       [:<> [:dt (tr (keyword "rule" (name id)))] [:dd (tr (keyword "fix" (name id)))]])]]

   [:div.card
    [:h3 (tr :guide/habits-title)]
    (tr :guide/habits)]

   [:div.card
    [:h3 (tr :guide/reading-title)]
    (tr :guide/reading)]])
