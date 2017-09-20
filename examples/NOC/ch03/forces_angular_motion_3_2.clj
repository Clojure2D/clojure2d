(ns NOC.ch03.forces-angular-motion-3-2
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.vector :as v]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 640)
(def ^:const ^int h 360)

(def ^:const ^double attractor-mass 20.0)
(def ^:const ^Vec2 attractor-pos (Vec2. (/ w 2) (/ h 2)))

(def ^:const ^int movers-no 20)

(deftype Mover [^Vec2 position ^Vec2 velocity ^double mass
                ^double angle ^double avelocity])

(defn attract
  ""
  [^Mover m]
  (let [force (v/sub attractor-pos (.position m))
        distance (m/constrain ^double (v/mag force) 5.0 25.0)
        strength (/ (* 0.4 attractor-mass (.mass m)) (m/sq distance))]
    (-> force
        (v/normalize)
        (v/mult strength))))

(defn make-mover
  ""
  [x y m]
  (Mover. (Vec2. x y) (Vec2. (r/drand -1 1) (r/drand -1 1)) m 0.0 0.0))

(defn update-mover
  ""
  [canvas ^Mover m]
  (let [^Vec2 acceleration (attract m)
        nvelocity (v/add (.velocity m) acceleration)
        ^Vec2 nposition (v/add (.position m) nvelocity)

        aacceleration (/ (.x acceleration) 10.0)
        navelocity (+ aacceleration (.avelocity m))
        nangle (+ (.angle m) navelocity)
        size (* (.mass m) 16.0)]

    (-> canvas
        (push-matrix)
        (translate (.x nposition) (.y nposition))
        (rotate nangle)
        (set-color 175 175 175 200)
        (crect 0 0 size size)
        (set-color :black)
        (crect 0 0 size size true)
        (pop-matrix))
    
    (Mover. nposition nvelocity (.mass m) nangle navelocity)))

(defn draw
  ""
  [canvas window _ state]
  (let [movers (or state (repeatedly movers-no #(make-mover (r/irand w) (r/irand h) (r/drand 0.1 2.0))))]

    (-> canvas
        (set-background :white)
        (set-color 127 127 127)
        (set-stroke 2.0)
        (ellipse (.x attractor-pos) (.y attractor-pos) 48 48)
        (set-color :black)
        (ellipse (.x attractor-pos) (.y attractor-pos) 48 48 true))
    
    (mapv (partial update-mover canvas) movers)))

(def window (show-window (make-canvas w h) "Forces angular motion 3_2" draw))
