(ns examples.ex24-stripes
  "Draw stripes" 
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m])
  (:import [java.awt Color]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def canvas (create-canvas 600 600))

(def window (show-window canvas "stripes" 600 600 25))

(defn draw
  ""
  [canvas]
  (let [step (/ m/TWO_PI ^int (m/irand 3 60))
        step2 (/ step 2.0)
        vs (reduce #(let [p1x (+ 300 (* 200 (m/cos %2)))
                          p1y (+ 300 (* 200 (m/sin %2)))
                          angle (+ ^double %2 step2)
                          p2x (+ 300 (* 100 (m/cos angle)))
                          p2y (+ 300 (* 100 (m/sin angle)))]
                      (conj %1 [p1x p1y] [p2x p2y]))
                   []
                   (range 0 (+ step2 m/TWO_PI) step))]
    (set-background canvas Color/black)
    (set-color canvas 200 200 190)
    (triangle-strip canvas vs)))

(with-canvas canvas
  (draw))
