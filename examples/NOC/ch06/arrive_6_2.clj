(ns NOC.ch06.arrive-6-2
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v])
  (:import clojure2d.math.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^double maxspeed 4.0)
(def ^:const ^double maxforce 0.1)
(def ^:const ^double r 6.0)
(def ^:const ^double r2 (+ r r))

(defn seek
  ""
  [target [position velocity]]
  (let [desired (v/sub target position)
        ^double d (v/mag desired)
        acceleration (-> desired 
                         (v/scale (if (< d 100.0)
                                    (m/norm d 0 100 0 maxspeed)
                                    maxspeed))
                         (v/sub velocity)
                         (v/limit maxforce))
        nvelocity (-> velocity
                      (v/add acceleration)
                      (v/limit maxspeed))]
    [(v/add position velocity) nvelocity]))

(defn draw
  ""
  [canvas window _ state]
  (let [vehicle (or state [(Vec2. 300.0 200.0) (Vec2. 0.0 -2.0)])
        [^Vec2 position velocity :as nvehicle] (seek (mouse-pos window) vehicle)
        theta (+ m/HALF_PI ^double (v/heading velocity))]

    (-> canvas
        (set-background :white)
        (set-color 200 200 200)
        (ellipse (mouse-x window) (mouse-y window) 48 48)
        (set-color :black)
        (set-stroke 2.0)
        (ellipse (mouse-x window) (mouse-y window) 48 48 true)
        (set-stroke 1.0)
        (set-color 127 0 0)
        (push-matrix)
        (translate (.x position) (.y position))
        (rotate theta)
        (triangle 0 (- r2) (- r) r2 r r2)
        (pop-matrix))

    nvehicle))

(def window (show-window (make-canvas 640 360) "Arrive 6_2" draw))
