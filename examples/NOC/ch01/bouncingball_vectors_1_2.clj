(ns examples.NOC.ch01.bouncingball-vectors-1-2
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.vector :as v])
  (:import clojure2d.math.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn boundary-check
  "Return -1.0 if out of borders, 1.0 otherwise"
  [^double mx1 ^double mx2 ^Vec2 v]
  (Vec2. (if (< -1.0 (.x v) mx1) 1.0 -1.0)
         (if (< -1.0 (.y v) mx2) 1.0 -1.0)))

(defn draw
  "Bounce ball"
  [canvas _ _ state]
  (let [[position velocity] (or state [(Vec2. 100 100)
                                       (Vec2. 2.5 5.0)])
        ^Vec2 nposition (v/add position velocity)]

    (-> canvas
        (set-background 255 255 255 10)
        (set-color 175 175 175)
        (ellipse (.x nposition) (.y nposition) 16 16)
        (set-color 0 0 0)
        (ellipse (.x nposition) (.y nposition) 16 16 true))
    
    [nposition
     (v/emult velocity (boundary-check (width canvas) (height canvas) nposition))]))

(def window (show-window (make-canvas 200 200) "Example 1-2: Bouncing Ball, with Vec2!" draw))
