(ns examples.NOC.introduction.randomwalklevy
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]
            [clojure2d.math :as m]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn montecarlo-fn
  ""
  ^double []
  (let [^double r1 (r/drand)
        probability (m/pow (- 1.0 r1) 8.0)]
    (if (< ^double (r/drand) probability)
      r1
      -1.0)))

(defn draw
  ""
  [canvas _ _ state]
  (let [[^double x ^double y] (or state [(* 0.5 ^int (width canvas))
                                         (* 0.5 ^int (height canvas))])
        stepsize (* 50.0 ^double (first (filter pos? (repeatedly montecarlo-fn))))
        stepsize- (- stepsize)
        ^double stepx (r/drand stepsize- stepsize)
        ^double stepy (r/drand stepsize- stepsize) 
        nx (m/constrain (+ x stepx) 0 (width canvas))
        ny (m/constrain (+ y stepy) 0 (height canvas))]

    (-> canvas
        (set-color c/:white)
        (line x y nx ny))

    [nx ny]))

(show-window (make-canvas 640 480) "Random Walk - Levy" draw)

