(ns examples.NOC.ch06.staywithinwalls-trail-6-3
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v])
  (:import clojure2d.math.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^double maxspeed 3.0)
(def ^:const ^double maxforce 0.15)
(def ^:const ^double r 6.0)
(def ^:const ^double r2 (+ r r))

(def ^:const ^double d 25.0)

(def ^:const ^int w 640)
(def ^:const ^int h 360)

(defn boundaries-and-run
  ""
  [^Vec2 position ^Vec2 velocity]
  (let [desired (cond
                  (< (.x position) d) (Vec2. maxspeed (.y velocity))
                  (> (.x position) (- w d)) (Vec2. (- maxspeed) (.y velocity))
                  (< (.y position) d) (Vec2. (.x velocity) maxspeed)
                  (> (.y position) (- h d)) (Vec2. (.x velocity) (- maxspeed))
                  :else nil)
        acceleration (if-not (nil? desired)
                       (-> desired
                           (v/normalize)
                           (v/mult maxspeed)
                           (v/sub velocity)
                           (v/limit maxforce))
                       (Vec2. 0.0 0.0))
        nvelocity (-> velocity
                      (v/add acceleration)
                      (v/limit maxspeed))]
    [(v/add position nvelocity) nvelocity]))

(defn draw
  ""
  [canvas window _ state]
  (let [[position velocity history] (or state [(Vec2. (/ w 2) (/ h 2)) (v/mult (Vec2. 3.0 -2.0) 5.0)])
        [^Vec2 nposition nvelocity] (boundaries-and-run position velocity)
        theta (+ m/HALF_PI ^double (v/heading velocity))]

    (-> canvas
        (set-background :white)
        (set-color :black)
        (set-color 175 175 175)
        (crect (/ w 2) (/ h 2) (- w (+ d d)) (- h (+ d d)) true)
        (set-color 127 0 0)
        (push-matrix)
        (translate (.x nposition) (.y nposition))
        (rotate theta)
        (triangle 0 (- r2) (- r) r2 r r2)
        (pop-matrix))

    [nposition nvelocity]))

(def window (show-window (make-canvas w h) "Stay Within Walls - 6_3" draw))
