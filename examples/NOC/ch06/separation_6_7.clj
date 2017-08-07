(ns NOC.ch06.separation-6-7
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v])
  (:import clojure2d.math.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 640)
(def ^:const ^int h 360)

(defprotocol VehicleProto
  (apply-behaviors-and-draw [t canvas vs]))

(deftype Vehicle [position
                  velocity
                  ^double r
                  maxspeed
                  maxforce]
  VehicleProto
  (apply-behaviors-and-draw [_ canvas vs]
    (let [desired-separation (+ r r)
          separate-sum (reduce #(let [d (v/dist position (.position ^Vehicle %2))]
                                  (if (< 0 d desired-separation)
                                    (-> position
                                        (v/sub (.position ^Vehicle %2))
                                        (v/normalize)
                                        (v/div d)
                                        (v/add %1))
                                    %1)) (Vec2. 0.0 0.0) vs)
          acceleration (if (pos? ^double (v/magsq separate-sum))
                         (-> separate-sum
                             (v/normalize)
                             (v/mult maxspeed)
                             (v/sub velocity)
                             (v/limit maxforce))
                         separate-sum)
          nvelocity (-> velocity
                        (v/add acceleration)
                        (v/limit maxspeed))
          ^Vec2 nposition (v/add position nvelocity)
          ^Vec2 nposition (Vec2. (m/wrap (- r) (+ w r) (.x nposition))
                                 (m/wrap (- r) (+ h r) (.y nposition)))]

      (-> canvas
          (set-color 175 175 175)
          (ellipse (.x nposition) (.y nposition) r r)
          (set-color :black)
          (ellipse (.x nposition) (.y nposition) r r true))
      
      (Vehicle. nposition nvelocity r maxspeed maxforce))))

(defn make-vehicle
  ""
  [x y]
  (Vehicle. (Vec2. x y)
            (Vec2. 0.0 0.0)
            12.0 3.0 0.2))

(def vehicles (atom (repeatedly 100 #(make-vehicle (r/drand w) (r/drand h)))))

(defn draw
  ""
  [canvas window _ _]
  (let [b @vehicles]

    (set-background canvas :white)
    
    (swap! vehicles (constantly (mapv #(apply-behaviors-and-draw % canvas b) b)))))

(def window (show-window (make-canvas w h) "Separation 6_7" draw))

(defmethod mouse-event ["Separation 6_7" :mouse-dragged] [e _]
  (swap! vehicles conj (make-vehicle (mouse-x e) (mouse-y e))))
