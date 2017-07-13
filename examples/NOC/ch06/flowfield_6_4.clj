;; Run and click to change vector field

(ns examples.NOC.ch06.flowfield-6-4
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.joise :as j]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v])
  (:import clojure2d.math.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 640)
(def ^:const ^int h 360)
(def ^:const ^int resolution 15)

(def ^:const cols (int (/ w resolution)))
(def ^:const rows (int (/ h resolution)))

(defn make-field
  "Create sampled vector field using provided noise function and scales."
  ([noise-fn ^double sx ^double sy]
   (vec (for [^int i (range cols)]
          (vec (for [^int j (range rows)]
                 (let [theta (* m/TWO_PI ^double (noise-fn (* i sx) (* j sy)))]
                   (Vec2. (m/cos theta) (m/sin theta))))))))
  ([] (make-field (j/make-random-fractal-noise) (r/drand 0.005 0.1) (r/drand 0.005 0.1))))

(defn lookup-field
  "Lookup field value at point"
  [field ^Vec2 lookup]
  (let [column (m/iconstrain (/ (.x lookup) resolution) 0 (dec cols))
        row (m/iconstrain (/ (.y lookup) resolution) 0 (dec rows))]
    (get-in field [column row])))

(defn draw-field
  "Draw vector field"
  [canvas field]
  (dorun (map-indexed (fn [^long i vv]
                        (dorun (map-indexed (fn [^long j ^Vec2 v]
                                              (let [x (* i resolution)
                                                    y (* j resolution)]
                                                (-> canvas
                                                    (push-matrix)
                                                    (translate x y)
                                                    (rotate (v/heading v))
                                                    (line 0 0 (/ resolution 1.5) 0)
                                                    (pop-matrix)))) vv))) field))
  canvas)


(deftype Vehicle [position
                  velocity
                  maxforce
                  ^double maxspeed])

(defn make-vehicle
  "Create vehicle"
  []
  (Vehicle. (Vec2. (r/drand w) (r/drand h))
            (Vec2. 0 0) 
            (r/drand 0.1 0.5)
            (r/drand 2 5)))

(defn run-vehicle
  "Calculate new position and draw"
  [canvas ^Vehicle v f]
  (let [r (.maxspeed v)
        nvelocity (-> (.velocity v)
                      (v/add (-> (lookup-field f (.position v)) ;; take force from field
                                 (v/mult (.maxspeed v))
                                 (v/sub (.velocity v))
                                 (v/limit (.maxforce v))))
                      (v/limit (.maxspeed v)))
        ^Vec2 nposition (v/add (.position v) nvelocity)
        ^Vec2 nposition (Vec2. (m/wrap (- r) (+ w r) (.x nposition))
                               (m/wrap (- r) (+ h r) (.y nposition)))
        theta (+ (m/radians 90) ^double (v/heading nvelocity))
        r2 (+ r r)]

    (-> canvas
        (push-matrix)
        (translate (.x nposition) (.y nposition))
        (rotate theta) 
        (set-color 175 0 0 200)
        (triangle 0 (- r2) (- r) r2 r r2)
        (pop-matrix))

    (Vehicle. nposition nvelocity (.maxforce v) (.maxspeed v))))

;; field as atom, click to change
(def field (atom (make-field)))

(defn draw
  "Draw on canvas"
  [canvas _ _ state]
  (let [vehicles (or state (repeatedly 120 make-vehicle))]
    (-> canvas
        (set-background :linen)
        (set-color :black 100)
        (draw-field @field))
    (mapv #(run-vehicle canvas % @field) vehicles)))

(def window (show-window (make-canvas w h) "Flowfield 6_4" draw))

(defmethod mouse-event ["Flowfield 6_4" :mouse-pressed] [_]
  (reset! field (make-field)))
