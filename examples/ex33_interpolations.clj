(ns ex33-interpolations
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.color :as c]
            [clojure2d.math.vector :as v]
            [clojure2d.extra.glitch :as g])
  (:import  [clojure2d.math.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def canvas (create-canvas 600 500 :high))

(def palette (first (filter #(> (c/get-luma (first %)) 150) (repeatedly #(:palette (g/color-reducer-machine))))))

(def c1 (first palette))
(def c2 (second palette))
(def c3 (nth palette 2))
(def c4 (nth palette 3))

(defn draw
  "Draw interpolations"
  [c ^long fc dir]
  (let [t (m/frac (/ fc 120.0))
        d (if (zero? t) (not dir) dir)
        t' (if d t (- 1.0 t))]

    (set-color c 0 0 0 100)
    (rect c 0 0 600 500)

    (set-color c c1)
    (ellipse c (m/lerp 100 500 t') 100 70 70)
    
    (set-color c c2)
    (ellipse c (m/cos-interpolation 100 500 t') 200 70 70)

    (set-color c c3)
    (ellipse c (m/smooth-interpolation 100 500 t') 300 70 70)

    (set-color c c4)
    (ellipse c (m/quad-interpolation 100 500 t') 400 70 70)

    d))

(def window (show-window canvas "Interpolations" 600 500 25 #(draw %1 %2 %3)))
