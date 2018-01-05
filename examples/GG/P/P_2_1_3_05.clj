(ns examples.GG.P.P-2-1-3-05
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^int w 600)
(def ^:const ^int h 600)

(def ^:const ^int tile-count-x 10)
(def ^:const ^int tile-count-y 10)

(def ^:const ^int tile-width (/ w tile-count-x))
(def ^:const ^int tile-height (/ h tile-count-y))

(def ^:const ^float color-step (/ 6.0 360.0))

(defn draw 
  "Draw rects"
  [canvas window _ _]
  (let [rng (r/make-randomizer :default (get-state window))
        step-size (/ (int (m/abs ^int (mouse-x window))) 10)
        end-size (/ (int (m/abs ^int (mouse-y window))) 10)]
    (set-background canvas :white)
    (translate canvas (* 0.5 tile-width) (* 0.5 tile-height))
    (doseq [grid-y (range tile-count-y)
            grid-x (range tile-count-x)]
      (let [pos-x (* ^long grid-x tile-width)
            pos-y (* ^long grid-y tile-height)
            cs (int (r/irandom rng 4))] 
        (dotimes [i step-size]
          (let [clr (* 255.0 (- 1.0 (* i color-step)))
                ^double diameter-x (m/norm i 0 step-size tile-width end-size)
                ^double diameter-y (m/norm i 0 step-size tile-height end-size)]
            (set-color canvas clr clr clr)
            (case cs
              0 (ellipse canvas (+ i pos-x) pos-y diameter-x diameter-x)
              1 (ellipse canvas pos-x (+ i pos-y) diameter-y diameter-y)
              2 (ellipse canvas (- pos-x i) pos-y diameter-x diameter-x)
              3 (ellipse canvas pos-x (- pos-y i) diameter-y diameter-y))))))))

(def window (show-window {:canvas (make-canvas w h)
                          :draw-fn draw
                          :window-name "P_2_1_3_05"
                          :state 0}))

(defmethod mouse-event [(:window-name window) :mouse-pressed] [_ _] (r/irand))
