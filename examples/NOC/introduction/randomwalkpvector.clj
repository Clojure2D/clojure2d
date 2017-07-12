(ns examples.NOC.introduction.randomwalkpvector
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
  (let [pos (or state (Vec2. (* 0.5 ^int (width canvas))
                             (* 0.5 ^int (height canvas))))
        vel (v/generate-vec2 (partial r/drand -2.0 2.0))
        ^Vec2 npos (v/add pos vel)
        nx (m/constrain (.x npos) 0 (width canvas))
        ny (m/constrain (.y npos) 0 (height canvas))]

    (-> canvas
        (set-background c/:white)
        (set-color 175 175 175)
        (crect nx ny 40 40)
        (set-color 0 0 0)
        (crect nx ny 40 40 true))

    (Vec2. nx ny)))

(show-window (make-canvas 400 400) "Random Walk - Vec2" 30 draw)
