(ns counterpointer.score
  (:require [reagent.core :as r]
            [counterpointer.theory :as t]))

;; Two staves: treble for the upper voice, bass for the lower one. One column per counterpoint
;; slot (`per` slots in a bar); the cantus note of a bar sits on its first slot.
;; Vertical unit: 5px per diatonic step, so staff lines are 10px apart.

(def sp 5)

(def staves
  {:treble {:top 38 :bottom 30 :lo 21 :hi 46}
   :bass {:top 26 :bottom 18 :lo 9 :hi 34}})

(def sharp-steps {:treble [38 35 39 36 33 37 34] :bass [24 21 25 22 19 23 20]})
(def flat-steps {:treble [34 37 33 36 32 35 31] :bass [20 23 19 22 18 21 17]})

(defn layout [{:keys [n-cols key compact per]}]
  (let [col-w (if (> per 1) (if compact 34 40) (if compact 46 54))
        top (if compact 34 46)
        gap (if compact 96 112)
        bottom (if compact 34 46)
        sig (Math/abs (t/signature key))
        x0 (+ 40 (* 9 sig) 12)
        treble-y top
        bass-y (+ top 40 gap)]
    {:col-w col-w :x0 x0 :treble-y treble-y :bass-y bass-y
     :mid (+ top 40 (/ gap 2))
     :w (+ x0 (* col-w n-cols) 8)
     :h (+ bass-y 40 bottom)}))

(defn- staff-y [lay staff] (if (= staff :treble) (:treble-y lay) (:bass-y lay)))

(defn- y-of [lay staff step]
  (+ (staff-y lay staff) (* sp (- (get-in staves [staff :top]) step))))

(defn- cx [lay i] (+ (:x0 lay) (* (:col-w lay) (+ i 0.5))))

(defn- ledger-steps [staff step]
  (let [{:keys [top bottom]} (staves staff)]
    (concat (when (>= step (+ top 2)) (range (+ top 2) (inc step) 2))
            (when (<= step (- bottom 2)) (range (- bottom 2) (dec step) -2)))))

(defn- ellipse-path [x y rx ry phi]
  (let [c (Math/cos (* phi (/ Math/PI 180))) s (Math/sin (* phi (/ Math/PI 180)))]
    (str "M" (- x (* rx c)) "," (- y (* rx s))
         " A" rx "," ry " " phi " 1,0 " (+ x (* rx c)) "," (+ y (* rx s))
         " A" rx "," ry " " phi " 1,0 " (- x (* rx c)) "," (- y (* rx s)) " Z ")))

(defn- head-path [x y dur]
  (if (= dur :half)
    (str (ellipse-path x y 6.4 4.5 -22) (ellipse-path x y 5.4 2.1 -22))
    (str (ellipse-path x y 7.2 5 0) (ellipse-path x y 3.4 2.6 -35))))

(def middle-step {:treble 34 :bass 22})

(defn- note-el [lay staff i note key cls & [dur]]
  (let [x (cx lay i)
        y (y-of lay staff (:s note))
        acc (when-not (t/in-mode? key note) (if (zero? (:a note)) "♮" (t/acc-sym (:a note))))
        up? (< (:s note) (middle-step staff))]
    [:g {:class cls}
     (for [st (ledger-steps staff (:s note))]
       ^{:key st} [:line.ledger {:x1 (- x 11) :x2 (+ x 11) :y1 (y-of lay staff st) :y2 (y-of lay staff st)}])
     (when acc [:text.acc {:x (- x 12) :y (+ y 5) :text-anchor "end"} acc])
     [:path.head {:d (head-path x y dur) :fill-rule "evenodd"}]
     (when (= dur :half)
       (let [sx (if up? (+ x 5.7) (- x 5.7))]
         [:line.stem {:x1 sx :x2 sx :y1 (if up? (- y 1.5) (+ y 1.5)) :y2 (if up? (- y 33) (+ y 33))}]))]))

(defn- clef
  "Clef glyphs differ a lot between fonts, so measure the glyph and scale it into the given box."
  [_ _ _ _]
  (let [el (atom nil)
        bbox (r/atom nil)]
    (r/create-class
     {:component-did-mount
      (fn [] (when-let [n @el]
               (let [b (.getBBox n)]
                 (when (pos? (.-height b)) (reset! bbox [(.-x b) (.-y b) (.-height b)])))))
      :reagent-render
      (fn [glyph x y-top h]
        (let [[bx by bh] @bbox]
          [:text.clef {:ref #(reset! el %) :x 0 :y 0 :font-size 40
                       :visibility (if bh "visible" "hidden")
                       :transform (when bh (str "translate(" x "," y-top ") scale(" (/ h bh) ") translate(" (- bx) "," (- by) ")"))}
           glyph]))})))

(defn- staff-el [lay staff key]
  (let [y0 (staff-y lay staff)
        sig (t/signature key)
        steps (take (Math/abs sig) ((if (neg? sig) flat-steps sharp-steps) staff))]
    [:g.staff
     (for [k (range 5)]
       ^{:key k} [:line {:x1 4 :x2 (- (:w lay) 8) :y1 (+ y0 (* 10 k)) :y2 (+ y0 (* 10 k))}])
     [:line {:x1 4 :x2 4 :y1 y0 :y2 (+ y0 40)}]
     [:line {:x1 (- (:w lay) 8) :x2 (- (:w lay) 8) :y1 y0 :y2 (+ y0 40)}]
     (if (= staff :treble)
       [clef "𝄞" 8 (- y0 13) 68]
       [clef "𝄢" 9 (- y0 1) 32])
     (for [[k st] (map-indexed vector steps)]
       ^{:key k}
       [:text.acc {:x (+ 40 (* 9 k)) :y (+ (y-of lay staff st) 5)} (if (neg? sig) "♭" "♯")])]))

(def motion-abbr {:contrary "c" :oblique "o" :similar "s" :parallel "p" :static "–"})

(defn- pick-at [lay {:keys [n-cols]} x y]
  (let [col (Math/floor (/ (- x (:x0 lay)) (:col-w lay)))]
    (when (< -1 col n-cols)
      (let [staff (if (< y (:mid lay)) :treble :bass)
            {:keys [top lo hi]} (staves staff)
            step (+ top (Math/round (/ (- (staff-y lay staff) y) sp)))]
        {:staff staff :col col :step (max lo (min hi step))}))))

(defn- event-point [e]
  (let [svg (.-currentTarget e)
        rect (.getBoundingClientRect svg)
        vb (.. svg -viewBox -baseVal)
        k (/ (.-width vb) (.-width rect))]
    [(* k (- (.-clientX e) (.-left rect))) (* k (- (.-clientY e) (.-top rect)))]))

(defn score
  "`marks` is {:notes {[voice col] level} :verts {col level} :moves {col level}}, from violations."
  [_]
  (let [hover (r/atom nil)]
    (fn [{:keys [cf cp key position n-cols verts motions marks sel highlight playing on-pick labels compact per]
          :or {per 1} :as props}]
      (let [props (assoc props :per per)
            lay (layout props)
            final-slot (* per (dec (count cf)))
            cp-dur (fn [i] (if (or (= per 1) (and (= i final-slot) (nil? (get cp (inc i))))) :whole :half))
            slot-of {:cf #(* per %) :cp identity}
            above? (not= position :below)
            staff-of {:cp (if above? :treble :bass) :cf (if above? :bass :treble)}
            voice-of {(staff-of :cp) :cp (staff-of :cf) :cf}
            lines {:cf cf :cp cp}
            {:keys [col-w x0 mid w h]} lay
            top-y (- (:treble-y lay) 30)
            bot-y (+ (:bass-y lay) 70)]
        [:svg.score {:viewBox (str "0 0 " w " " h) :width w :height h
                     :class (when on-pick "interactive")
                     :on-pointer-move (when on-pick
                                        #(let [[x y] (event-point %)] (reset! hover (pick-at lay props x y))))
                     :on-pointer-leave #(reset! hover nil)
                     :on-click (when on-pick
                                 #(let [[x y] (event-point %)]
                                    (when-let [{:keys [staff col step]} (pick-at lay props x y)]
                                      (let [v (voice-of staff)]
                                        (on-pick v (if (= v :cf) (quot col per) col) step)))))}
         (for [i highlight]
           ^{:key (str "h" i)}
           [:rect.hl {:x (+ x0 (* col-w i)) :y top-y :width col-w :height (- bot-y top-y) :rx 6}])
         (when playing
           [:rect.playing {:x (+ x0 (* col-w playing)) :y top-y :width col-w :height (- bot-y top-y) :rx 6}])
         (when-let [{:keys [voice col]} sel]
           (let [y0 (staff-y lay (staff-of voice))
                 span (if (= voice :cf) per 1)]
             [:rect.cursor {:x (+ x0 (* col-w ((slot-of voice) col)) 3) :y (- y0 22)
                            :width (- (* col-w span) 6) :height 84 :rx 6}]))
         [staff-el lay :treble key]
         [staff-el lay :bass key]
         (for [i (range per n-cols per) staff [:treble :bass] :let [x (+ x0 (* col-w i)) y0 (staff-y lay staff)]]
           ^{:key (str "b" i (name staff))}
           [:line.bar {:x1 x :x2 x :y1 y0 :y2 (+ y0 40)}])
         (for [voice [:cf :cp]
               [i note] (map-indexed vector (lines voice))
               :when note]
           ^{:key (str (name voice) i)}
           [note-el lay (staff-of voice) ((slot-of voice) i) note key
            (str "note " (name voice) " " (some-> (get-in marks [:notes [voice i]]) name)
                 (when (= sel {:voice voice :col i}) " sel"))
            (if (= voice :cf) :whole (cp-dur i))])
         (when (and (> per 1) (nil? (first cp)) (second cp))
           (let [y (+ (staff-y lay (staff-of :cp)) 20)]
             [:rect.rest {:x (- (cx lay 0) 6) :y (- y 5) :width 12 :height 5}]))
         (when-let [{:keys [staff col step]} (and on-pick @hover)]
           (let [v (voice-of staff)
                 c (if (= v :cf) (quot col per) col)
                 note (get-in lines [v c])]
             (when-not (= step (:s note))
               [note-el lay staff ((slot-of v) c) {:s step :a (t/default-alt key (mod step 7))} key "note ghost"
                (if (= v :cf) :whole (cp-dur c))])))
         (when labels
           [:g.labels
            (for [v verts :when v :let [i (:i v)]]
              ^{:key (str "v" i)}
              [:text {:x (cx lay i) :y (+ mid 4) :text-anchor "middle"
                      :class (let [mark (get-in marks [:verts i])
                                   ok-weak? (and (pos? (:beat v)) (= :dissonant (:cons v)) (not mark))]
                               (str "iv " (if ok-weak? "passing" (name (:cons v))) " " (some-> mark name)))}
               [:title (str (t/interval-name v) ": " (t/interval-long v))]
               (:num v)])
            (for [m motions :let [i (:i m)]]
              ^{:key (str "m" i)}
              [:text {:x (+ x0 (* col-w (inc i))) :y (+ mid 4) :text-anchor "middle"
                      :class (str "mv " (name (:kind m)) " " (some-> (get-in marks [:moves i]) name))}
               [:title (name (:kind m))]
               (motion-abbr (:kind m))])])]))))
