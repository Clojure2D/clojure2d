(ns NOC.ch03.simple-harmonic-motion-3-6
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 640)
(def ^:const ^int h 360)

(def ^:const ^double aVelocity 0.03)
(def ^:const ^double amplitude 300)

(defn draw
  ""
  [canvas window _ state]
  (let [^double angle (or state 0.0)
        x (* amplitude (m/sin angle))]
    (-> canvas
        (set-background :white)
        (translate (/ w 2) (/ h 2))
        (set-color :black)
        (line 0 0 x 0)
        (set-color 175 175 175)
        (ellipse x 0 20 20)
        (set-color :black)
        (ellipse x 0 20 20 true))

    (+ angle aVelocity)))

(def window (show-window (make-canvas w h) "Simple harmonic motion 3_6" draw))
