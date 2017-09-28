(ns examples.NOC.ch08.recursion-8-3
  (:require [clojure2d.core :refer :all]))

(def canvas (make-canvas 640 360))
(def window (show-window canvas "Recursion 8_3"))

(defn draw-circle
  ""
  [canvas x y r]
  (ellipse canvas x y r r true)
  (when (> r 8)
    (let [r2 (/ r 2)]
      (draw-circle canvas (+ x r2) y r2)
      (draw-circle canvas (- x r2) y r2)
      (draw-circle canvas x (+ y r2) r2)
      (draw-circle canvas x (- y r2) r2))))

(with-canvas canvas
  (set-background :white)
  (set-color :black)
  (draw-circle (/ (width canvas) 2)
               (/ (height canvas) 2)
               400))
