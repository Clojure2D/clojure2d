(ns examples.NOC.introduction.noisewalk-acceleration
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v]
            [clojure2d.math :as m]
            [clojure2d.color :as c])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; no deftype approach

(def nstep (Vec2. 0.01 0.01))

(defn draw
  ""
  [canvas window ^long framecount state]
  (let [[position velocity noff history] (or state [(Vec2. (* 0.5 ^int (width canvas)) (* 0.5 ^int (height canvas))) ;; position
                                                    (Vec2. 0 0) ;; velocity
                                                    (v/generate-vec2 (partial r/drand 1000)) ;; noise field position
                                                    clojure.lang.PersistentQueue/EMPTY]) ;; history
        ^Vec2 nnoff (v/add noff nstep)
        nvelocity (-> (Vec2. (m/norm (r/noise (.x nnoff)) 0 1 -1 1)
                             (m/norm (r/noise (.y nnoff)) 0 1 -1 1))
                      (v/mult 0.1)
                      (v/add velocity)
                      (v/limit 1.0))
        ^Vec2 nposition (v/add position nvelocity)
        ^Vec2 nposition (Vec2. (m/constrain (.x nposition) 8 (- ^int (width window) 9))
                               (m/constrain (.y nposition) 8 (- ^int (height window) 9)))
        nhistory (conj history nposition)
        nhistory (if (== (count nhistory) 1001) (pop nhistory) nhistory)]

    (-> canvas
        (set-background c/:white)
        (set-color 175 175 175)
        (crect (.x nposition) (.y nposition) 16 16)
        (set-color 0 0 0)
        (crect (.x nposition) (.y nposition) 16 16 true)
        (path nhistory))

    [nposition nvelocity nnoff nhistory]))

(def window (show-window (make-canvas 640 360) "Noise walk - acceleration" draw))
