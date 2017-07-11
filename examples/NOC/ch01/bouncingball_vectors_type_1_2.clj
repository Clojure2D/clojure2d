(ns examples.NOC.ch01.bouncingball-vectors-type-1.2
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.vector :as v])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def canvas (make-canvas 200 200))

(defprotocol BallProto 
  (update-ball [b])
  (display-ball [b canvas]))

(deftype Ball [^Vec2 position velocity]
  BallProto
  (update-ball [_]
    (let [^Vec2 nposition (v/add position velocity)
          nvelocity (v/emult velocity (Vec2. (if (< -1.0 (.x nposition) (width canvas)) 1.0 -1.0)
                                             (if (< -1.0 (.y nposition) (height canvas)) 1.0 -1.0)))]
      (Ball. nposition nvelocity)))
  (display-ball [b canvas]
    (-> canvas
        (set-color 175 175 175)
        (ellipse (.x position) (.y position) 16 16)
        (set-color 0 0 0)
        (ellipse (.x position) (.y position) 16 16 true))
    b))

(defn draw
  "update and draw ball"
  [canvas _ _ state]
  (let [b (or state (Ball. (Vec2. 100.0 100.0)
                           (Vec2. 2.5 5.0)))]
    
    (set-background canvas 255 255 255)
    (-> b
        update-ball
        (display-ball canvas))))

(def window (show-window canvas "Example 1-2: Bouncing Ball, with Vec2 and type!" draw))
