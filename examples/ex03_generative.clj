(ns examples.ex03-generative
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m])
  (:import  [java.awt Color]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def foreground (Color. 240 240 240 100))
(def background (Color. 10 20 40 100))

(defn draw-lines
  ""
  [canvas framecount]
  (let [off (/ framecount 150.0)
        yratio (m/norm (m/qsin (* 0.1 off m/TWO_PI)) -1 1 20 500)]
    (loop [y 100.1]
      (let [n (m/norm (m/noise (+ (/ 1.0 (- 520 yratio)) (/ y yratio)) off ) 0.0 1.0 -200.0 200.0)]
        (line canvas 300 y (+ 300 n) y))
      (when (< y 500) (recur (+ y 5.0)))))
  canvas)

(defn draw
  ""
  [canvas framecount & res]
  (with-canvas canvas
    (set-awt-color foreground)
    (set-stroke 1.85)
    (set-awt-background background)
    (draw-lines framecount)))

(defn example-03
  []
  (show-window (create-canvas 600 600) "clojure-canvas" 600 600 25 draw))

(example-03)
