(ns NOC.ch03.wave-abc-3-9
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 250)
(def ^:const ^int h 200)

(def ^:const ^double angleVel (rand-nth [0.05 0.2 0.4]))

(defn draw
  ""
  [canvas _ ^long fc _]
  (let [startangle (* fc 0.015)]

    (set-background canvas :white)
    (set-stroke canvas 2.0)
    (loop [x (int 0)
           angle startangle]
      (when (<= x w)
        (let [y (m/norm (m/sin angle) -1.0 1.0 0 h)]
          (set-color canvas :black 50)          
          (ellipse canvas x y 48 48)
          (set-color canvas :black)          
          (ellipse canvas x y 48 48 true))
        (recur (+ x 24) (+ angle angleVel))))))

(def window (show-window (make-canvas w h) "Wave abc 3_9" draw))
