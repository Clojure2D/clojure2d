(ns examples.NOC.ch08.recursion-8-1
  (:require [clojure2d.core :refer :all]))

(def canvas (make-canvas 640 360))
(def window (show-window canvas "Recursion 8_1"))

(defn draw-circle
  "Draw recursively smaller circles."
  [canvas x y r]
  (ellipse canvas x y r r true)
  (when (> r 2.0)
    (recur canvas x y (* 0.75 r))))

(with-canvas canvas
  (set-background :white)
  (set-color :black)
  (draw-circle (/ (width canvas) 2)
               (/ (height canvas) 2)
               (width canvas)))
