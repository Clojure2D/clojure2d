(ns NOC.ch02.forces_many_mutual_boundaries
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v])
  (:import clojure2d.math.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 640)
(def ^:const ^int h 360)

(def ^:const ^double g 0.4)

(def ^:const ^int movers-no 20)

(defprotocol MoverProto
  (attract [m v m2])
  (update-and-draw [m other canvas]))

(deftype Mover [^Vec2 position
                ^Vec2 velocity
                ^double mass
                ^long id]
  MoverProto
  (attract [m1 v m2]
    (if (== id (.id ^Mover m2))
      v
      (let [force (v/sub (.position ^Mover m2) position)
            distance (m/constrain ^double (v/mag force) 5.0 25.0)
            strength (/ (* g mass (.mass ^Mover m2)) (m/sq distance))] 
        (v/add v (-> force
                     v/normalize
                     (v/mult strength)
                     (v/div mass))))))
  (update-and-draw [m other canvas]
    (let [acceleration (-> (reduce (partial attract m) (Vec2. 0.0 0.0) other)
                           (v/add (let [fx (cond
                                             (< (.x position) 50) 1.0
                                             (> (.x position) (- w 50)) -1.0
                                             :else 0.0)
                                        fy (cond
                                             (< (.y position) 50) 1.0
                                             (> (.y position) (- h 50)) -1.0
                                             :else 0.0)]
                                    (-> (Vec2. fx fy)
                                        (v/scale 0.1))))) 
          nvelocity (v/add velocity acceleration)
          ^Vec2 nposition (v/add position nvelocity)
          s (* 24.0 mass)]
      
      (-> canvas
          (set-color 175 175 175 200)
          (ellipse (.x nposition) (.y nposition) s s)
          (set-color :black)
          (ellipse (.x nposition) (.y nposition) s s true))

      (Mover. nposition nvelocity mass id))))

(def counter (make-counter))

(defn make-mover
  ""
  [x y m]
  (Mover. (Vec2. x y) (Vec2. 0 0) m (counter)))

(defn draw
  ""
  [canvas window framecount state]
  (let [movers (or state (repeatedly movers-no #(make-mover (r/drand w) (r/drand h) (r/drand 0.1 2))))]

    (-> canvas
        (set-background :white))

    (mapv #(update-and-draw % movers canvas) movers)))

(def window (show-window (make-canvas w h) "Mutual attraction with boundaries" draw))

