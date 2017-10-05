(ns NOC.ch03.exercise-cannon-3-2
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 640)
(def ^:const ^int h 360)

(def ^:const ^Vec2 pos (Vec2. 50 300))
(def ^:const ^Vec2 gravity (Vec2. 0.0 0.2))

(def ^:const ^double topspeed 10)

(defprotocol CannonBallProto
  (apply-force [cb v])
  (shoot-ball [cb angle])
  (update-ball [cb])
  (draw-ball [cb canvas]))

(declare make-cannon-ball)

(deftype CannonBall [position velocity acceleration shot]
  CannonBallProto
  (apply-force [_ v]
    (CannonBall. position velocity (v/add acceleration v) shot))
  (update-ball [cb]
    (if shot
      (let [nvelocity (v/limit (v/add velocity (v/add acceleration gravity)) topspeed)
            ^Vec2 nposition  (v/add position nvelocity)]
        (if (> (.y nposition) h)
          (make-cannon-ball)
          (CannonBall. nposition nvelocity (Vec2. 0 0) shot)))
      cb))
  (shoot-ball [_ angle]
    (CannonBall. position velocity (v/add acceleration (v/from-polar (Vec2. 10.0 angle))) true))
  (draw-ball [_ canvas]
    (-> canvas
        (push-matrix)
        (translate position)
        (set-color :white)
        (ellipse 0 0 16 16)
        (set-color :black)
        (ellipse 0 0 16 16 true)
        (pop-matrix))))

(defn make-cannon-ball
  ""
  []
  (CannonBall. pos (Vec2. 0 0) (Vec2. 0 0) false))

(defn draw
  ""
  [canvas window _ state]
  (let [ball (or state (make-cannon-ball))
        wstate (get-state window)
        angle (:angle wstate)
        shoot (:shoot wstate)]

    (when shoot (set-state! window (assoc wstate :shoot false)))
    
    (-> canvas
        (set-background :white)
        (set-stroke 2.0)
        (push-matrix)
        (translate pos)
        (rotate angle)
        (set-color :white)
        (rect 0 -5 50 10)
        (set-color :black)
        (rect 0 -5 50 10 true)
        (pop-matrix))

    (let [nball (if shoot (shoot-ball ball angle) ball)] 
      (draw-ball nball canvas)    
      (update-ball nball))))

(def canvas (make-canvas w h))
(def window (show-window {:canvas canvas
                          :window-name "Exercise cannon 3_2"
                          :draw-fn draw
                          :state {:angle (- m/QUARTER_PI)}}))

(defmethod key-pressed ["Exercise cannon 3_2" virtual-key] [e {:keys [^double angle shoot]}]
  {:angle (condp = (key-code e)
            :right (+ angle 0.1)
            :left (- angle 0.1)
            angle)
   :shoot shoot})

(defmethod key-pressed ["Exercise cannon 3_2" \space] [_ state]
  (assoc state :shoot true))

