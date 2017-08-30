(ns NOC.ch03.simple-harmonic-motion-3-5
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 640)
(def ^:const ^int h 360)

(def ^:const ^double period 120)
(def ^:const ^double amplitude 300)

(defn draw
  ""
  [canvas window ^long framecount _]
  (let [x (* amplitude (m/sin (/ (* m/TWO_PI framecount) period)))]
    (-> canvas
        (set-background :white)
        (set-stroke 2.0)
        (translate (/ w 2) (/ h 2))
        (set-color :black)
        (line 0 0 x 0)
        (set-color 127 127 127)
        (ellipse x 0 48 48)
        (set-color :black)
        (ellipse x 0 48 48 true))))

(def window (show-window (make-canvas w h) "Simple harmonic motion 3_5" draw))
