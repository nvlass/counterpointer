(ns counterpointer.subs
  (:require [re-frame.core :as rf]
            [counterpointer.rules :as rules]
            [counterpointer.events :as events]
            [counterpointer.hints :as hints]
            [counterpointer.i18n :as i18n]))

(doseq [k [:view :settings :exercise :sel :focus :saved :samples :play :midi :text-error :history :future]]
  (rf/reg-sub k (fn [db _] (k db))))

(rf/reg-sub :loc :<- [:settings] (fn [s _] (i18n/locale (:lang s) (:note-names s))))

(rf/reg-sub :context :<- [:exercise] (fn [e _] (rules/context e)))

(rf/reg-sub :violations
  :<- [:exercise] :<- [:settings]
  (fn [[e s] _] (rules/analyze e (:rules s) (:check-cf s))))

(rf/reg-sub :score :<- [:violations] (fn [vs _] (rules/score vs)))

(rf/reg-sub :stats :<- [:exercise] (fn [e _] (rules/stats e)))

(rf/reg-sub :n-cols :<- [:exercise] (fn [e _] (events/n-cols e)))

(def rank {:error 2 :warning 1 :info 0})

(defn- worst [m k level] (update m k #(if (and % (>= (rank %) (rank level))) % level)))

(rf/reg-sub :per :<- [:exercise] (fn [e _] (events/per e)))

(rf/reg-sub :marks
  :<- [:violations] :<- [:per]
  (fn [[vs per] _]
    (reduce
     (fn [acc {:keys [cols level kind voice]}]
       (let [notes (case voice
                     :cf (for [c cols] [:cf c])
                     :cp (for [c cols] [:cp c])
                     (mapcat (fn [c] [[:cp c] [:cf (quot c per)]]) cols))
             acc (reduce #(update-in %1 [:notes] worst %2 level) acc notes)]
         (case (when-not voice kind)
           :vertical (reduce #(update %1 :verts worst %2 level) acc cols)
           :motion (reduce #(update %1 :moves worst %2 level) acc (butlast cols))
           acc)))
     {:notes {} :verts {} :moves {}}
     (remove #(= :info (:level %)) vs))))

(rf/reg-sub :tips
  :<- [:exercise] :<- [:sel]
  (fn [[e sel] _] (hints/tips e sel)))

(rf/reg-sub :candidates
  :<- [:exercise] :<- [:sel] :<- [:settings]
  (fn [[e sel s] _]
    (when (and sel (:candidates s) (seq (:cf e)))
      (hints/candidates e sel (:rules s) (:check-cf s)))))
