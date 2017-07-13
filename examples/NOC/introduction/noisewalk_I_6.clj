(ns examples.NOC.introduction.noisewalk-I-6
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v])
  (:import clojure2d.math.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def step (Vec2. 0.005 0.005))

(defn draw
  ""
  [canvas _ _ state]
  (let [^Vec2 noff (or state (Vec2. (r/drand 1000) (r/drand 1000)))

        x (m/norm (r/noise (.x noff)) 0.0 1.0 0.0 (width canvas))
        y (m/norm (r/noise (.y noff)) 0.0 1.0 0.0 (height canvas))]

    (-> canvas
        (set-background :white)
        (set-color 127 127 127)
        (ellipse x y 48 48)
        (set-color :black)
        (ellipse x y 48 48 true))

    (v/add noff step)))

(show-window (make-canvas 800 200) "Noise Walk I_6" 30 draw)
