(ns NOC.ch03.pointing-velocity-3-3
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.vector :as v]
            [clojure2d.math :as m])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 640)
(def ^:const ^int h 360)

(def ^:const ^double topspeed 4.0)

(deftype Mover [position velocity])

(defn update-mover
  "Update mover position and speed"
  [^Mover m window]
  (let [mouse (mouse-pos window)
        dir (-> mouse
                (v/sub (.position m))
                v/normalize
                (v/mult 0.5))
        nvelocity (v/limit (v/add (.velocity m) dir) topspeed)
        nposition (v/add (.position m) nvelocity)]
    (Mover. nposition nvelocity)))

(defn check-edges
  "Wrap screen"
  [^Mover m]
  (let [^Vec2 p (.position m)]
    (Mover. (Vec2. (m/wrap 0 w (.x p))
                   (m/wrap 0 h (.y p)))
            (.velocity m))))

(defn draw
  "Update and draw mover"
  [canvas window _ state]
  (let [^Mover m (or state (Mover. (Vec2. (/ w 2) (/ h 2))
                                   (Vec2. 0 0)))
        theta (v/heading (.velocity m))]

    (-> canvas
        (set-background :white)
        (set-stroke 2.0)
        (translate (.position m))
        (rotate theta) 
        (set-color 127 127 127)
        (crect 0 0 30 10)
        (set-color :black)
        (crect 0 0 30 10 true))

    (-> m
        (update-mover window)
        (check-edges))))

(def window (show-window (make-canvas w h) "Pointing velocity 3_3" draw))
