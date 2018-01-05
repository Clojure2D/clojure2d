(ns examples.NOC.ch06.staywithincircle
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v])
  (:import clojure2d.math.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^double maxspeed 3.0)
(def ^:const ^double maxforce 0.15)
(def ^:const ^double r 3.0)
(def ^:const ^double r2 (+ r r))

(def ^:const ^int w 640)
(def ^:const ^int h 360)

(def ^Vec2 circleposition (Vec2. (/ w 2) (/ h 2)))
(def ^:const ^double circle-radius (- (* h 0.5) 25.0))

(defn boundaries-and-draw
  ""
  [canvas [position velocity]]
  (let [predict (-> velocity
                    (v/mult 25.0))
        ^Vec2 futureposition (v/add position predict)
        distance (double (v/dist futureposition circleposition))
        acceleration (if (> distance circle-radius)
                       (-> circleposition
                           (v/sub position)
                           (v/normalize)
                           (v/mult (v/mag velocity))
                           (v/add velocity)
                           (v/normalize)
                           (v/mult maxspeed)
                           (v/sub velocity)
                           (v/limit maxforce))
                       (Vec2. 0 0))
        nvelocity (-> velocity
                      (v/add acceleration)
                      (v/limit maxspeed))
        ^Vec2 nposition (v/add position nvelocity)
        theta (+ m/HALF_PI ^double (v/heading nvelocity))]

    (-> canvas
        (set-background :white)
        (set-color 175 175 175)
        (ellipse (.x circleposition) (.y circleposition) (* 2 circle-radius) (* 2 circle-radius) true)
        (set-color :red)
        (ellipse (.x futureposition) (.y futureposition) 4.0 4.0)
        (push-matrix)
        (translate (.x nposition) (.y nposition))
        (rotate theta)
        (set-color 175 175 175) 
        (triangle 0 (- r2) (- r) r2 r r2)
        (set-color :black) 
        (triangle 0 (- r2) (- r) r2 r r2 true)
        (pop-matrix))

    [nposition nvelocity]))

(defn draw
  ""
  [canvas window _ state]
  (boundaries-and-draw canvas (or state [(Vec2. (/ w 2) (/ h 4)) (v/mult (Vec2. 1.0 0.0) 5.0)])))

(def window (show-window (make-canvas w h) "Stay Within Circle" draw))
