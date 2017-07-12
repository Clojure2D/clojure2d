(ns examples.NOC.introduction.noisewalk-velocity
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v]
            [clojure2d.math :as m])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; no deftype approach

(def nstep (Vec2. 0.005 0.005))

(defn draw
  ""
  [canvas window ^long framecount state]
  (let [[position noff history] (or state [(Vec2. (* 0.5 ^int (width canvas)) (* 0.5 ^int (height canvas))) ;; position
                                           (v/generate-vec2 (partial r/drand 1000)) ;; noise field position
                                           clojure.lang.PersistentQueue/EMPTY]) ;; history
        ^Vec2 nnoff (v/add noff nstep)
        velocity (-> (Vec2. (m/norm (r/noise (.x nnoff)) 0 1 -1 1)
                            (m/norm (r/noise (.y nnoff)) 0 1 -1 1))
                     (v/mult 5))
        ^Vec2 nposition (v/add position velocity)
        ^Vec2 nposition (Vec2. (m/constrain (.x nposition) 8 (- ^int (width window) 9))
                               (m/constrain (.y nposition) 8 (- ^int (height window) 9)))
        nhistory (conj history nposition)
        nhistory (if (== (count nhistory) 1001) (pop nhistory) nhistory)]

    (-> canvas
        (set-background 255 255 255)
        (set-color 175 175 175)
        (crect (.x nposition) (.y nposition) 16 16)
        (set-color 0 0 0)
        (crect (.x nposition) (.y nposition) 16 16 true)
        (path nhistory))

    [nposition nnoff nhistory]))

(def window (show-window (make-canvas 400 400) "Noise walk - velocity" 30 draw))
