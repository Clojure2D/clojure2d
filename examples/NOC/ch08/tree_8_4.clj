(ns examples.NOC.ch08.tree-8-4
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]))

(def canvas (make-canvas 300 200))

(defn branch
  "Generate branches."
  [canvas len theta]
  (let [sw (m/norm len 2 120 1 10)
        nlen (* 0.66 len)]

    (-> canvas
        (set-stroke sw)
        (line 0 0 0 (- len))
        (translate 0 (- len)))

    (if (> nlen 2.0)
      (-> canvas
          (push-matrix)
          (rotate theta)
          (branch nlen theta)
          (pop-matrix)
          (push-matrix)
          (rotate (- theta))
          (branch nlen theta)
          (pop-matrix))
      canvas)))

(defn draw
  ""
  [canvas window _ _]
  (let [theta (m/cnorm (mouse-x window) 0 (width window) 0 m/HALF_PI)]
    (-> canvas
        (set-background :white)
        (set-color :black)
        (translate (/ (width canvas) 2) (height canvas))
        (branch 60.0 theta))))

(def window (show-window canvas "Tree 8_4" draw))

