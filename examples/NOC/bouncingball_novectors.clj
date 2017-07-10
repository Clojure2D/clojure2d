(ns examples.NOC.bouncingball-novectors
  (:require [clojure2d.core :refer :all]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn boundary-check
  "Returns -1.0 if out of borders, 1.0 otherwise"
  ^double [^double mx ^double v]
  (if (< -1.0 v mx) 1.0 -1.0))

(defn draw
  "Bounce ball"
  [canvas _ state]
  (let [[^double x ^double y ^double xspeed ^double yspeed] (or state [100 100 2.5 2.0])
        nx (+ x xspeed)
        ny (+ y yspeed)]

    (-> canvas
        (set-background 255 255 255)
        (set-stroke 2.0)
        (set-color 127 127 127)
        (ellipse nx ny 48 48)
        (set-color 0 0 0)
        (ellipse nx ny 48 48 true))
    
    [nx ny
     (* xspeed (boundary-check (width canvas) nx))
     (* yspeed (boundary-check (height canvas) ny))]))

(def window (show-window (make-canvas 800 200) "Example 1-1: Bouncing Ball, no vectors" draw))
