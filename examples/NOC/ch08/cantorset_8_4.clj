(ns examples.NOC.ch08.recursion-8-4
  (:require [clojure2d.core :refer :all]))

(def canvas (make-canvas 800 200))
(def window (show-window canvas "Cantor set 8_4"))

(defn cantor
  "Draw cantor set recursively."
  [canvas x y len]
  (when (>= len 1.0)
    (rect canvas x y len 10)
    (cantor canvas x (+ y 30) (/ len 3.0))
    (cantor canvas (+ x (/ (* 2.0 len) 3.0)) (+ y 30) (/ len 3.0))))

(with-canvas-> canvas
  (set-background :white)
  (set-color :black)
  (cantor 35 0 730))
