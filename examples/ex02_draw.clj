(ns examples.ex02-draw
  "Draw synced with given refresh rate" 
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.pixels :as p])
  (:import [java.awt.event MouseEvent]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn draw
  ""
  [canvas framecount & res]
  (let [fc (/ ^long framecount 100.0)
        n (->> fc
               (m/tan)
               (m/sin)
               (m/abs)
               (+ 0.1))
        ^double cn (m/cnorm n -1.0 1.0 -20 20)
        ew (* n 160.0)
        eh (* (- 1.0 n) 160.0)]

    (with-canvas canvas
      (set-background 45 45 41 20))
    
    (p/set-canvas-pixels canvas (->> canvas
                                     p/get-canvas-pixels
                                     (p/filter-channels p/gaussian-blur-2 nil)))

    (with-canvas canvas
      (set-color (- 146.0 ew) (- 199.0 cn) (- 163.0 eh))
      (ellipse 100 100 ew eh))))

(defn example-02
  ""
  []
  (show-window (make-canvas 200 200) "ellipse" 300 300 25 draw))

(def window (example-02))
