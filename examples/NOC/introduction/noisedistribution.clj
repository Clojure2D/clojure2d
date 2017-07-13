(ns examples.NOC.introduction.noisedistribution
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]
            [clojure2d.math.joise :as j]
            [clojure2d.math.vector :as v]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn draw
  ""
  [canvas _ ^long framecount state]
  (let [vals (or state (repeatedly (width canvas) #(double 0)))
        xoff (* 0.003 framecount)
        ^double n (r/noise xoff)
        index (int (* n ^int (width canvas)))
        nvals (vec (map-indexed #(if (== ^int %1 index) (inc ^double %2) %2) vals)) ;; increase hit index
        mx (double (v/mx nvals))
        normalized (if (> mx ^int (height canvas))
                     (vec (map #(* ^int (height canvas) (/ ^double % mx)) nvals))
                     nvals)]

    (set-background canvas 100 100 100)
    (set-color canvas :white)

    ;; draw lines
    (dotimes [x (width canvas)]
      (line canvas x (height canvas) x (- ^int (height canvas) ^double (normalized x))))

    nvals))

(def window (show-window (make-canvas 300 200) "Noise distribution" 200 draw))

