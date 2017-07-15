(ns NOC.ch06.flocking-6-9
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v])
  (:import clojure2d.math.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 640)
(def ^:const ^int h 360)

(defprotocol BoidProto
  (separate [b bs])
  (cohesion-align [b bs]))

(deftype Boid [position
               velocity
               maxspeed maxforce]
  Object
  (toString [_]
    (str position ", " velocity))
  BoidProto
  (cohesion-align [_ bs]
    (let [bs-filtered (filter #(< 0.0 (v/dist position (.position ^Boid %)) 50.0) bs)]
      (if (pos? (count bs-filtered))
        (v/add (-> (reduce #(v/add %1 (.position ^Boid %2)) (Vec2. 0.0 0.0) bs-filtered)
                   (v/div (count bs-filtered))
                   (v/sub position)
                   (v/normalize)
                   (v/mult maxspeed)
                   (v/sub velocity)
                   (v/limit maxforce))
               (-> (reduce #(v/add %1 (.velocity ^Boid %2)) (Vec2. 0.0 0.0) bs-filtered)
                   (v/normalize)
                   (v/mult maxspeed)
                   (v/sub velocity)
                   (v/limit maxforce)))
        (Vec2. 0.0 0.0))))
  (separate [_ bs]
    (let [steer (reduce #(let [d (v/dist position (.position ^Boid %2))]
                           (if (< 0 d 25.0)
                             (-> position
                                 (v/sub (.position ^Boid %2))
                                 (v/normalize)
                                 (v/div d)
                                 (v/add %1))
                             %1)) (Vec2. 0.0 0.0) bs)]

      (if (pos? ^double (v/mag steer))
        (-> steer
            (v/normalize)
            (v/mult maxspeed)
            (v/sub velocity)
            (v/limit maxforce))
        (Vec2. 0.0 0.0)))))

(defn make-boid
  ""
  [^double x ^double y]
  (Boid. (Vec2. (+ x ^double (r/grand)) (+ y ^double (r/grand)))
         (Vec2. (r/grand) (r/grand))
         (r/drand 2.0 4.0)
         (r/drand 0.01 0.1)
         ;; 3.0 0.05
         ))

(defn update-boid-and-draw
  ""
  [canvas bs ^Boid b]
  (let [^double r (.maxspeed b)
        r2 (+ r r)
        acceleration (-> (v/mult (separate b bs) 1.5)
                         (v/add (cohesion-align b bs)))
        velocity (-> (.velocity b)
                     (v/add acceleration)
                     (v/limit r))
        ^Vec2 position (v/add (.position b) velocity)
        ^Vec2 position (Vec2. (m/wrap (- r) (+ r w) (.x position))
                              (m/wrap (- r) (+ r h) (.y position)))
        theta (+ (m/radians 90) ^double (v/heading velocity))]

    (-> canvas
        (push-matrix)
        (translate (.x position) (.y position))
        (rotate theta) 
        (triangle 0 (- r2) (- r) r2 r r2)
        (pop-matrix))
    
    (Boid. position velocity r (.maxforce b))))

(def boids (atom (repeatedly 200 #(make-boid (/ w 2) (/ h 2)))))

(defn draw
  ""
  [canvas _ _ _]
  (let [bs @boids]
    (-> canvas
        (set-background :linen)
        (set-color 175 0 0 200))
    (swap! boids (constantly (mapv (partial update-boid-and-draw canvas bs) bs)))))

(def window (show-window (make-canvas w h) "Flocking 6_9" draw))

(defmethod mouse-event ["Flocking 6_9" :mouse-dragged] [e]
  (swap! boids conj (make-boid (mouse-x e) (mouse-y e))))



