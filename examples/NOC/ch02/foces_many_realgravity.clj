(ns examples.NOC.ch02.forces-many-realgravity-2-3
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v])
  (:import clojure2d.math.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 640)
(def ^:const ^int h 360)

(def ^:const ^int number-of-movers 20)

(def wind (Vec2. 0.01 0.0))
(def gravity (Vec2. 0.0 0.1))

(deftype Mover [position velocity ^double mass])

(defn make-mover
  "Create Mover"
  []
  (->Mover (Vec2. 0.0 0.0)
           (Vec2. 0.0 0.0)
           (r/drand 1.0 4.0)))

(defn apply-force
  "Apply force"
  [a f mass]
  (v/add a (v/div f mass)))

(defn check-edges
  "Check window boundaries"
  [^Vec2 vel ^Vec2 pos]
  (let [[px vx] (if (> (.x pos) w) [w (* -1.0 (.x vel))]
                    (if (neg? (.x pos)) [0.0 (* -1.0 (.x vel))]
                        [(.x pos) (.x vel)]))
        [py vy] (if (> (.y pos) h) [h (* -1.0 (.y vel))]
                    [(.y pos) (.y vel)])]
    [(Vec2. vx vy) (Vec2. px py)]))

(defn move-mover
  "Move mover"
  [^Mover m]
  (let [ acc (-> (Vec2. 0.0 0.0)
                 (apply-force wind (.mass m))
                 (v/add gravity))
        vel (v/add (.velocity m) acc)
        pos (v/add (.position m) vel)
        [new-vel new-pos] (check-edges vel pos)]
    (->Mover new-pos new-vel (.mass m))))

(defn draw-and-move
  "Draw mover, move and return new one."
  [canvas ^Mover m]
  (let [size (* 16.0 ^double (.mass m))]
    (set-color canvas 0 0 0 127)
    (ellipse canvas (.x ^Vec2 (.position m)) (.y ^Vec2 (.position m)) size size false)
    (set-stroke canvas 2)
    (set-color canvas 0 0 0)
    (ellipse canvas (.x ^Vec2 (.position m)) (.y ^Vec2 (.position m)) size size true)
    (move-mover m)))

(defn draw
  "Draw movers on canvas"
  [canvas _ _ last-m]
  (set-background canvas 255 255 255)
  (let [m (or last-m (repeatedly number-of-movers make-mover))]
    (mapv (partial draw-and-move canvas) m)))

(def window (show-window (make-canvas w h) "NOC_2_3_forces_many_realgravity" draw))

