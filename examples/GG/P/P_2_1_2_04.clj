(ns examples.GG.P.P-2-1-2-04
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.vector :as v]
            [clojure2d.math.random :as r]
            [clojure2d.math :as m]
            [clojure2d.color :as c]))

(def ^:const ^int tile-count 20)
(def ^:const ^double rect-size 30.0)

(defn draw 
  "Draw rects"
  [canvas window _ _]
  (let [rng (r/make-randomizer :default (get-state window))
        mx (/ (max 2 (mouse-x window)) 20)
        my (/ (max 2 (mouse-y window)) 20)]
    (-> canvas
        (set-background :white)
        (set-color (c/from-HSB (c/make-color 136 255 164 154))))
    (doseq [grid-x (range tile-count)
            grid-y (range tile-count)]
      (let [px (* grid-x (/ (width window) tile-count))
            py (* grid-y (/ (height window) tile-count))
            p [(v/vec2 (+ px (r/drandom rng (- mx) mx))
                       (+ py (r/drandom rng (- my) my)))
               (v/vec2 (+ px rect-size (r/drandom rng (- mx) mx))
                       (+ py (r/drandom rng (- my) my)))
               (v/vec2 (+ px rect-size (r/drandom rng (- mx) mx))
                       (+ py rect-size (r/drandom rng (- my) my)))
               (v/vec2 (+ px (r/drandom rng (- mx) mx))
                       (+ py rect-size (r/drandom rng (- my) my)))]]
        (path canvas p true false)))))

(def window (show-window {:canvas (make-canvas 600 600)
                          :draw-fn draw
                          :window-name "P_2_1_2_04"
                          :state 0}))

(defmethod mouse-event [(:window-name window) :mouse-pressed] [_ _] (r/irand))
