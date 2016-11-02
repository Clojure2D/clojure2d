(ns examples.ex02-draw
  "Draw synced with given refresh rate" 
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.pixels :as p])
  (:import [java.awt.event MouseEvent]
           [java.awt Color]))


(defn draw
  ""
  [canvas framecount & res]
  (let [fc (/ framecount 100.0)
        n (->> fc
               (m/tan)
               (m/sin)
               (m/abs)
               (+ 0.1))
        cn (int (m/constrain (m/norm n -1.0 1.0 -20 20) -20 20))
        ew (* n 80)
        eh (* (- 1.0 n) 80)]
    (with-canvas canvas
      (set-background (Color. 45 45 41 20)))
    
    (set-canvas-pixels canvas (p/filter-channels p/gaussian-blur-2 (get-canvas-pixels canvas)))

    (with-canvas canvas
      (set-color (Color. (- 146 cn) (- 199 cn) (- 163 cn)))
      (ellipse 50 50 ew eh))))

(defn example-02
  ""
  []
  (show-window (create-canvas 100 100) "ellipse" 300 300 25 draw))

(example-02)
