(ns NOC.ch03.polartocartesian-3-4
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 640)
(def ^:const ^int h 360)

(def ^:const ^double r (* h 0.4))

(defn draw
  ""
  [canvas _ _ state]
  (let [^double theta (or state 0.0)
        x (* r (m/cos theta))
        y (* r (m/sin theta))]

    (-> canvas
        (set-background :white)
        (translate (/ w 2) (/ h 2))
        (set-color :black)
        (set-stroke 2.0)
        (line 0 0 x y)
        (ellipse x y 48 48 true)
        (set-color 127 127 127)
        (ellipse x y 47 47))

    (+ theta 0.02)))

(def window (show-window (make-canvas w h) "Polar to cartesan 3_4" draw))
