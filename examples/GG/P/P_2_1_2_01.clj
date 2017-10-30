(ns examples.GG.P.P-2-1-2-01
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]))

(def ^:const ^int scl (/ 600 20))

(defn draw
  "Draw circles"
  [canvas window _ _]
  (let [rng (r/make-randomizer :default (get-state window))
        mx (mouse-x window)
        my (max 0.01 (mouse-y window))]
    (-> canvas
        (set-background :white)
        (set-color 0 0 0 180)
        (set-stroke (/ my 60))
        (translate (* 0.5 scl) (* 0.5 scl)))
    (doseq [grid-y (range 20)
            grid-x (range 20)]
      (let [px (* scl grid-x)
            py (* scl grid-y) 
            shift-x (/ (r/drandom rng (- mx) mx) 20)
            shift-y (/ (r/drandom rng (- mx) mx) 20)]
        (ellipse canvas (+ px shift-x) (+ py shift-y) (/ my 15) (/ my 15) true)))))

(def window (show-window {:canvas (make-canvas 600 600)
                          :window-name "P_2_1_2_01"
                          :draw-fn draw
                          :state 0}))

(defmethod mouse-event [(:window-name window) :mouse-pressed] [_ _] (r/irand))
