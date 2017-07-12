(ns examples.NOC.introduction.randomwalktraditional2
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]
            [clojure2d.math :as m]
            [clojure2d.color :as c]
            [clojure2d.math.vector :as v])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn draw
  ""
  [canvas _ _ state]
  (let [[^double x ^double y] (or state [(* 0.5 ^int (width canvas))
                                         (* 0.5 ^int (height canvas))])
        stepx (dec (int (r/drand 3)))
        stepy (dec (int (r/drand 3)))
        nx (m/constrain (+ x stepx) 0 (width canvas))
        ny (m/constrain (+ y stepy) 0 (height canvas))]

    (-> canvas
        (set-color c/:white)
        (point nx ny))

    [nx ny]))

(show-window (make-canvas 200 200) "Random Walk - Traditional 2" draw)
