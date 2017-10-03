(ns examples.ex38-old-school-xor
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def canvas (make-canvas 600 600))

(defn draw-rings
  "Draw rings"
  [canvas posx posy]
  (dotimes [x 17]
    (let [size (* (inc x) 80)]
      (ellipse canvas posx posy size size true)))
  canvas)

(defn draw
  "Frames"
  [canvas _ ^long fps _]
  (let [t (/ fps 60.0) 
        sa (m/norm (m/qsin (* 0.5 t)) -1.0 1.0 100 500)
        ca (m/norm (m/qcos t) -1.0 1.0 100 500)
        sb (m/norm (m/qsin (inc t)) -1.0 1.0 100 500)
        cb (m/norm (m/qcos (* 2.0 (dec t))) -1.0 1.0 200 500)]
    (-> canvas
        (set-background :black)
        (set-color :white)
        (set-xor-mode :black)
        (set-stroke 20)
        (draw-rings sb cb)
        (draw-rings sa ca))))

(def window (show-window canvas "Oldschool XOR" #(draw %1 %2 %3 %4)))
