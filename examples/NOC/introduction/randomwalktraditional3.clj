(ns examples.NOC.introduction.randomwalktraditional3
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn draw
  ""
  [canvas _ _ state]
  (let [[^double x ^double y] (or state [(* 0.5 ^int (width canvas))
                                         (* 0.5 ^int (height canvas))])
        stepx (r/drand -1.0 1.0)
        stepy (r/drand -1.0 1.0)
        nx (m/constrain (+ x stepx) 0 ^int (width canvas))
        ny (m/constrain (+ y stepy) 0 ^int (height canvas))]

    (-> canvas
        (set-color :white)
        (point x y))

    [nx ny]))

(show-window (make-canvas 200 200) "Random Walk - Traditional 3" draw)
