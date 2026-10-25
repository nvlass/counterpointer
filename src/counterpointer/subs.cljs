(ns counterpointer.subs
  (:require [re-frame.core :as rf]
            [counterpointer.rules :as rules]
            [counterpointer.events :as events]))

(doseq [k [:view :settings :exercise :sel :focus :saved :samples :play :midi :text-error :history :future]]
  (rf/reg-sub k (fn [db _] (k db))))

(rf/reg-sub :context :<- [:exercise] (fn [e _] (rules/context e)))

(rf/reg-sub :violations
  :<- [:exercise] :<- [:settings]
  (fn [[e s] _] (rules/analyze e (:rules s) (:check-cf s))))

(rf/reg-sub :score :<- [:violations] (fn [vs _] (rules/score vs)))

(rf/reg-sub :stats :<- [:exercise] (fn [e _] (rules/stats e)))

(rf/reg-sub :n-cols :<- [:exercise] (fn [e _] (events/n-cols e)))

(def rank {:error 2 :warning 1 :info 0})

(defn- worst [m k level] (update m k #(if (and % (>= (rank %) (rank level))) % level)))

(rf/reg-sub :marks
  :<- [:violations]
  (fn [vs _]
    (reduce
     (fn [acc {:keys [cols level kind voice]}]
       (let [voices (if voice [voice] [:cf :cp])
             acc (reduce (fn [a [v c]] (update a :notes worst [v c] level))
                         acc (for [v voices c cols] [v c]))]
         (case kind
           :vertical (reduce #(update %1 :verts worst %2 level) acc cols)
           :motion (reduce #(update %1 :moves worst %2 level) acc (butlast cols))
           acc)))
     {:notes {} :verts {} :moves {}}
     (remove #(= :info (:level %)) vs))))
