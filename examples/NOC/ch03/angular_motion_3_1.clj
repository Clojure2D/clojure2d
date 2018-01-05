(ns NOC.ch03.angular-motion-3-1
  (:require [clojure2d.core :refer :all]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 800)
(def ^:const ^int h 200)

(defn draw
  ""
  [canvas window _ state]
  (let [[^double angle ^double velocity] (or state [0.0 0.0])]
    (-> canvas
        (set-background :white)
        (set-stroke 2.0)
        (translate (/ w 2) (/ h 2))
        (rotate angle)
        (set-color 127 127 127)
        (ellipse 60 0 16 16)
        (ellipse -60 0 16 16)
        (set-color :black)
        (ellipse 60 0 16 16 true)
        (ellipse -60 0 16 16 true)
        (line -60 0 60 0))
    [(+ angle velocity) (+ velocity 0.0001)]))

(def window (show-window (make-canvas w h) "Angular motion 3_1" draw))
