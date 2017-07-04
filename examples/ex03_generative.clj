(ns examples.ex03-generative
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def foreground (c/make-awt-color 240 240 240 100))
(def background (c/make-awt-color 10 20 40 100))

(defn draw-lines
  ""
  [canvas ^long framecount]
  (translate canvas 300 0)
  (rotate canvas (* 0.25 (m/qsin (/ framecount 50.0))))
  (let [off (/ framecount 150.0)
        ^double yratio (m/norm (m/qsin (* 0.1 off m/TWO_PI)) -1 1 20 500)]
    (loop [y (double 100.1)]
      (let [^double n (m/norm (r/noise (+ (/ 1.0 (- 520.0 yratio)) (/ y yratio)) off ) 0.0 1.0 -200.0 200.0)]
        (line canvas 0.0 y n y))
      (when (< y 500) (recur (+ y 5.0)))))
  canvas)

(defn draw
  ""
  [canvas framecount & res]
  (set-awt-color canvas foreground)
  (set-stroke canvas 1.85)
  (set-awt-background canvas background)
  (draw-lines canvas framecount))

(defn example-03
  []
  (show-window (create-canvas 600 600) "clojure-canvas" 600 600 60 #(draw %1 %2 %3)))

(example-03)
