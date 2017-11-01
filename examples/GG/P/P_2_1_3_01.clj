(ns examples.GG.P.P-2-1-3-01
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^int w 800)
(def ^:const ^int h 800)

(def ^:const ^int tile-count-x 10)
(def ^:const ^int tile-count-y 10)

(def ^:const ^int tile-width (/ w tile-count-x))
(def ^:const ^int tile-height (/ h tile-count-y))

(defn draw 
  "Draw rects"
  [canvas window _ _]
  (let [rng (r/make-randomizer :default (get-state window))
        circle-count (inc (/ ^int (mouse-x window) 30))
        ^double end-size (m/norm (max 2 ^int (mouse-x window)) 0 w (* 0.5 tile-width) 0)
        ^double end-offset (m/norm (max 2 ^int (mouse-y window)) 0 h 0 (* 0.5 (- tile-width end-size)))]
    (-> canvas
        (set-background :white)
        (set-color 0 0 0 128)
        (translate (* 0.5 (/ w tile-count-x)) (* 0.5 (/ h tile-count-y))))
    (doseq [grid-y (range tile-count-y)
            grid-x (range tile-count-x)]
      (let [angle (case (int (r/irandom rng 4))
                    0 (- m/HALF_PI)
                    2 m/HALF_PI
                    3 m/PI
                    0.0)]
        (-> canvas
            (push-matrix)
            (translate (* ^long grid-x tile-width) (* ^long grid-y tile-height))
            (scale 1.0 (/ tile-height tile-width))
            (rotate angle))
        (dotimes [i circle-count]
          (let [diameter (m/norm i 0 (dec circle-count) tile-width end-size)
                offset (m/norm i 0 (dec circle-count) 0 end-offset)]
            (ellipse canvas offset 0 diameter diameter true)))
        (pop-matrix canvas)))))


(def window (show-window {:canvas (make-canvas w h)
                          :draw-fn draw
                          :window-name "P_2_1_3_01"
                          :state 0}))

(defmethod mouse-event [(:window-name window) :mouse-pressed] [_ _] (r/irand))
