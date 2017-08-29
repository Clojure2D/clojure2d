(ns examples.NOC.ch02.fluidresistance-2-5
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v])
  (:import clojure2d.math.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 640)
(def ^:const ^int h 360)
(def ^:const ^int h2 (/ h 2))

(def ^:const ^int number-of-movers 9)

(def gravity (Vec2. 0.0 0.1))
(def ^:const ^double c 0.1)

(deftype Mover [position velocity ^double mass]
  Object
  (toString [_] (str position " : " velocity)))

(defn make-mover
  "Create Mover"
  []
  (->Mover (Vec2. (r/drand w) 0.0)
           (Vec2. 0.0 0.0)
           (r/drand 1.0 4.0)))

(defn apply-force
  "Apply force"
  [a f mass]
  (v/add a (v/div f mass)))

(defn check-edges
  "Check window boundaries"
  [^Vec2 velocity ^Vec2 pos]
  (if (> (.y pos) h)
    [(Vec2. (.x velocity) (* -0.9 (.y velocity))) (Vec2. (.x pos) h)]
    [velocity pos]))

(defn move-mover
  "Move mover"
  [^Mover m]
  (let [acc (-> (Vec2. 0.0 0.0)
                (apply-force (if (> (.y ^Vec2 (.position m)) h2)
                               (let [drag-magnitude (* c (m/sq (v/mag (.velocity m))))]
                                 (-> (.velocity m)
                                     (v/mult -1.0)
                                     (v/normalize)
                                     (v/mult drag-magnitude)))
                               (Vec2. 0.0 0.0)) (.mass m))
                (apply-force (-> (.velocity m)
                                 (v/normalize)
                                 (v/mult -0.05)) (.mass m))
                (v/add gravity))
        vel (v/add (.velocity m) acc)
        pos (v/add (.position m) vel)
        [new-vel new-pos] (check-edges vel pos)]
    (->Mover new-pos new-vel (.mass m))))

(defn draw-and-move
  "Draw mover, move and return new one."
  [canvas ^Mover m]
  (let [size (* 16.0 ^double (.mass m))]
    (set-color canvas 127 127 127 200)
    (ellipse canvas (.x ^Vec2 (.position m)) (.y ^Vec2 (.position m)) size size false)
    (set-stroke canvas 2)
    (set-color canvas 0 0 0)
    (ellipse canvas (.x ^Vec2 (.position m)) (.y ^Vec2 (.position m)) size size true)
    (move-mover m)))

(defn draw
  "Draw movers on canvas"
  [canvas window _ _]
  (-> canvas
      (set-background 255 255 255)
      (set-color 50 50 50)
      (rect 0 h2 w h2))
  (set-state! window (mapv (partial draw-and-move canvas) (get-state window))))

(def window (show-window {:canvas (make-canvas w h)
                          :window-name "NOC_2_5_fluidresistance"
                          :draw-fn draw
                          :state (repeatedly number-of-movers make-mover)}))

(defmethod mouse-event ["NOC_2_5_fluidresistance" :mouse-released] [_ _]
  (repeatedly number-of-movers make-mover))
