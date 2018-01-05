(ns examples.ex02-draw
  "Draw synced with given refresh rate" 
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.pixels :as p]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)
(m/use-primitive-operators)

(defn draw
  ""
  [canvas window framecount & res]
  (let [fc (/ ^long framecount 100.0)
        n (->> fc
               (m/tan)
               (m/sin)
               (m/abs)
               (+ 0.1))
        ^double cn (m/cnorm n -1.0 1.0 -20 20)
        ew (* n 160.0)
        eh (* (- 1.0 n) 160.0)]

    (-> canvas
        (set-color 45 45 41 20)
        (rect 0 0 (width canvas) (height canvas))
        (p/set-canvas-pixels! (->> canvas
                                   p/get-canvas-pixels
                                   (p/filter-channels p/gaussian-blur-2 false)))

        (set-color (- 146.0 ew) (- 199.0 cn) (- 163.0 eh))
        (ellipse 100 100 ew eh))))

(show-window {:canvas (make-canvas 200 200 :mid)
              :window-name "ellipse"
              :w 400
              :h 400
              :fps 30
              :hint :mid
              :draw-fn draw})
