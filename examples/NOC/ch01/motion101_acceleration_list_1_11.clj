(ns examples.NOC.ch01.motion101-acceleration-list-1-11
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v])
  (:import clojure2d.math.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^double topspeed 5.0)
(def ^:const ^int movers-no 20)

(defprotocol MoverProto
  (update-mover [t window])
  (display-mover [t canvas]))

(deftype Mover [^Vec2 position
                velocity]
  MoverProto
  (update-mover [_ window]
    (let [acceleration (-> (mouse-pos window)
                           (v/sub position)
                           (v/normalize)
                           (v/mult 0.2))
          nvelocity (-> velocity
                        (v/add acceleration)
                        (v/limit topspeed))
          ^Vec2 nposition (v/add position velocity)]
      (Mover. nposition
              nvelocity)))
  
  (display-mover [m canvas]
    (-> canvas 
        (set-stroke 2.0)
        (set-color 127 127 127 200)
        (ellipse (.x position) (.y position) 48 48)
        (set-color 0 0 0)
        (ellipse (.x position) (.y position) 48 48 true))
    m))

(defn make-mover
  "Create random Mover"
  [w h]
  (Mover. (Vec2. (r/drand w) (r/drand h))
          (Vec2. 0 0)))

(defn draw
  "Process and display Movers"
  [canvas window _ movers]
  (let [m (or movers (repeatedly movers-no #(make-mover (width window) (height window))))]
    (set-background canvas 255 255 255)
    (mapv #(display-mover (update-mover % window) canvas) m)))

(def window (show-window (make-canvas 640 360) "Motion 101 Acceleration List (1.11)" draw))

;; it's slower than original version, probably due to overhead in mapv and immutable way of updating
