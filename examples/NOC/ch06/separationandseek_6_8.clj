(ns NOC.ch06.separationandseek-6-8
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v])
  (:import clojure2d.math.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 640)
(def ^:const ^int h 360)

(defprotocol VehicleProto
  (apply-behaviors-and-draw [t canvas window vs]))

(deftype Vehicle [position
                  velocity
                  ^double r
                  maxspeed
                  maxforce]
  VehicleProto
  (apply-behaviors-and-draw [_ canvas window vs]
    (let [desired-separation (+ r r)
          separate-sum (reduce #(let [d (v/dist position (.position ^Vehicle %2))]
                                  (if (< 0 d desired-separation)
                                    (-> position
                                        (v/sub (.position ^Vehicle %2))
                                        (v/normalize)
                                        (v/div d)
                                        (v/add %1))
                                    %1)) (Vec2. 0.0 0.0) vs)
          separate-force (v/mult (if (pos? ^double (v/magsq separate-sum))
                                   (-> separate-sum
                                       (v/normalize)
                                       (v/mult maxspeed)
                                       (v/sub velocity)
                                       (v/limit maxforce))
                                   separate-sum) 2.0)
          seek-force (-> (mouse-pos window)
                         (v/sub position)
                         (v/normalize)
                         (v/mult maxspeed)
                         (v/sub velocity)
                         (v/limit maxforce))
          acceleration (v/add separate-force seek-force)
          nvelocity (-> velocity
                        (v/add acceleration)
                        (v/limit maxspeed))
          ^Vec2 nposition (v/add position nvelocity)]

      (-> canvas
          (push-matrix)
          (translate (.x nposition) (.y nposition))
          (set-color 175 175 175)
          (ellipse 0 0 r r)
          (set-color :black)
          (ellipse 0 0 r r true)
          (pop-matrix))
      
      (Vehicle. nposition nvelocity r maxspeed maxforce))))

(defn make-vehicle
  ""
  [x y]
  (Vehicle. (Vec2. x y)
            (Vec2. 0.0 0.0)
            12.0 6.0 0.2))

(def vehicles (atom (repeatedly 100 #(make-vehicle (r/drand w) (r/drand h)))))

(defn draw
  ""
  [canvas window _ _]
  (let [b @vehicles]

    (set-background canvas :white)
    
    (swap! vehicles (constantly (mapv #(apply-behaviors-and-draw % canvas window b) b)))))

(def window (show-window (make-canvas w h) "Separation and Seek 6_8" draw))

(defmethod mouse-event ["Separation and Seek 6_8" :mouse-dragged] [e _]
  (swap! vehicles conj (make-vehicle (mouse-x e) (mouse-y e))))
