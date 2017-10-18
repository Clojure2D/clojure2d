(ns examples.NOC.ch01.motion101-acceleration-1-10
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.vector :as v])
  (:import clojure2d.math.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^double topspeed 5.0)

(defprotocol MoverProto
  (update-mover [t window])
  (display-mover [t canvas]))

(deftype Mover [^Vec2 position
                velocity]
  MoverProto
  (update-mover [_ window]
    (let [acceleration (-> (mouse-pos window)
                           (v/sub position)
                           (v/set-mag 0.2))
          nvelocity (-> velocity
                        (v/add acceleration)
                        (v/limit topspeed))
          ^Vec2 nposition (v/add position velocity)]
      (Mover. nposition
              nvelocity)))
  
  (display-mover [m canvas]
    (-> canvas
        (set-background 255 255 255)
        (set-stroke 2.0)
        (set-color 127 127 127)
        (ellipse (.x position) (.y position) 48 48)
        (set-color 0 0 0)
        (ellipse (.x position) (.y position) 48 48 true))
    m))

;; run few times
(show-window (make-canvas 640 360) "Motion 101 Acceleration (1.10)"
             (fn [canvas window _ mover]
               (let [m (or mover (Mover. (Vec2. (* 0.5 ^int (width window)) (* 0.5 ^int (height window)))
                                         (Vec2. 0 0)))] 
                 (display-mover (update-mover m window) canvas))))
