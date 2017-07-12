(ns examples.NOC.introduction.simpleprobability
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v]
            [clojure2d.math :as m]
            [clojure2d.color :as c])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; move mouse around window

(defn draw
  ""
  [canvas window _ state]
  (let [^Vec2 v (or state (Vec2. 0.0 0.0))
        prob (/ ^double (m/constrain (mouse-x window) 5 (width window)) ^double (width window))
        nx (m/wrap 0 (width canvas) (+ 10.0 (.x v)))
        ny (if (zero? nx)
             (m/wrap 0 (height canvas) (+ 10.0 (.y v)))
             (.y v))]

    (when (< ^double (r/drand) prob)
      (-> canvas
          (set-background 0 0 0 1)
          (set-color c/:white)
          (ellipse nx ny 10 10)))

    (Vec2. nx ny)))

(show-window (make-canvas 200 200) "Simple probability" draw)
