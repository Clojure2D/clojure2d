(ns examples.NOC.ch08.treestochastic-angleonly-8-7
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]))

(def canvas (make-canvas 800 200))
(def window (show-window canvas "Tree Stochastic - angle only 8_7"))

(defn branch
  ""
  [canvas ^double h]
  (let [sw (m/norm h 2.0 120.0 1.0 5.0)
        newh (* 0.66 h)
        theta (r/drand m/THIRD_PI)]
    (-> canvas
        (set-stroke sw)
        (line 0 0 0 (- h))
        (translate 0 (- h)))

    (when (> newh 2.0)
      (-> canvas
          (push-matrix)
          (rotate theta)
          (branch newh)
          (pop-matrix)
          (push-matrix)
          (rotate (- theta))
          (branch newh)
          (pop-matrix))))
  canvas)

(defn new-tree
  "Create new tree"
  []
  (with-canvas canvas
    (set-background :white)
    (set-color :black)
    (text "Click mouse to generate a new tree" 10 (- (height canvas) 10))
    (translate (/ (width canvas) 2.0) (height canvas))
    (branch 60.0)))

(defmethod mouse-event [(:window-name window) :mouse-pressed] [_ _]
  (new-tree))

(new-tree)
