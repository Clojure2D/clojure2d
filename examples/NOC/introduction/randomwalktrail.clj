(ns examples.NOC.introduction.randomwalktrail
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v]
            [clojure2d.math :as m])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; no deftype approach

(defn draw
  ""
  [canvas window ^long framecount state]
  (let [[position history] (or state [(Vec2. (* 0.5 ^int (width canvas)) (* 0.5 ^int (height canvas))) ;; position
                                      clojure.lang.PersistentQueue/EMPTY]) ;; history
        velocity (v/generate-vec2 (partial r/drand -2.0 2.0))
        ^Vec2 nposition (v/add position velocity)
        ^Vec2 nposition (Vec2. (m/constrain (.x nposition) 8 (- ^int (width window) 9))
                               (m/constrain (.y nposition) 8 (- ^int (height window) 9)))
        nhistory (conj history nposition)
        nhistory (if (== (count nhistory) 1001) (pop nhistory) nhistory)]

    (-> canvas
        (set-background :white)
        (set-color 175 175 175)
        (crect (.x nposition) (.y nposition) 16 16)
        (set-color 0 0 0)
        (crect (.x nposition) (.y nposition) 16 16 true)
        (path nhistory))

    [nposition nhistory]))

(def window (show-window (make-canvas 400 400) "Random walk - trail" 30 draw))
