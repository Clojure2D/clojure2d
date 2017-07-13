(ns examples.NOC.ch06.flowfield-6-4
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v])
  (:import clojure2d.math.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn lookup-field
  ""
  [field ^Vec2 lookup]
  (Vec2. 0 0))


(defprotocol VehicleProto
  (apply-force [t f])
  (run [t f canvas]))

(deftype Vehicle [position
                  velocity
                  ^double r
                  maxforce maxspeed]
  VehicleProto
  (apply-force [_ f]
    (Vehicle. position velocity r maxforce maxspeed))
  (run [_ f canvas]
    (let [nvelocity (-> velocity
                        (v/add (-> (lookup-field f position)
                                   (v/mult maxspeed)
                                   (v/sub velocity)
                                   (v/limit maxforce)))
                        (v/limit maxspeed))
          ^Vec2 nposition (v/add position nvelocity)
          ^Vec2 nposition (Vec2. (m/wrap (- r) (+ ^int (width canvas) r) (.x nposition))
                                 (m/wrap (- r) (+ ^int (height canvas) r) (.y nposition)))])))
