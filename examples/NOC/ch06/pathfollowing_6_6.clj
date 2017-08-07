(ns NOC.ch06.pathfollowing-6-6
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v])
  (:import clojure2d.math.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 640)
(def ^:const ^int h 360)

(def ^:const ^double radius 20.0)
(def ^:const ^double r 4.0)
(def ^:const ^double r2 (+ r r))

(defn make-path []
  [(Vec2. -20.0 (/ h 2))
   (Vec2. (r/drand (/ w 2)) (r/drand (/ h 4) (* 0.75 h)))
   (Vec2. (r/drand (/ w 2) w) (r/drand (/ h 4) (* 0.75 h)))
   (Vec2. (+ w 20.0) (/ h 2))])

(defn draw-path
  ""
  [canvas p]
  (-> canvas
      (set-color 175 175 175)
      (set-stroke (+ radius radius))
      (path p)
      (set-color :black)
      (set-stroke 1.0)
      (path p)))

(defprotocol VehicleProto
  (follow-and-draw [v canvas window p]))

(defn get-normal-point
  ""
  [p a b]
  (let [ap (v/sub p a)
        ab (v/normalize (v/sub b a))]
    (-> ab 
        (v/mult (v/dot ap ab))
        (v/add a))))

(deftype Vehicle [^Vec2 position
                  velocity
                  maxspeed
                  maxforce]
  Object
  (toString [_] (str position " : " velocity))
  VehicleProto
  (follow-and-draw [_ canvas window p]
    (let [^Vec2 predict-pos (-> velocity
                                (v/normalize)
                                (v/mult 50.0)
                                (v/add position))
          [^double world-record
           ^Vec2 target
           ^Vec2 normal-point] (loop [i (int 0)
                                      current-record 100000000.0
                                      current-target nil
                                      current-normal-point nil]
                                 (let [^Vec2 a (p i)
                                       ^Vec2 b (p (inc i))
                                       ^Vec2 normal-point (get-normal-point predict-pos a b)
                                       normal-point (if (or (< (.x normal-point) (.x a))
                                                            (> (.x normal-point) (.x b)))
                                                      b
                                                      normal-point)
                                       distance (double (v/dist predict-pos normal-point))
                                       [new-record
                                        new-target
                                        new-normal-point] (if (< distance (double current-record))
                                                            [distance (-> (v/sub b a)
                                                                          (v/normalize)
                                                                          (v/mult 10.0)
                                                                          (v/add normal-point)) normal-point]
                                                            [current-record current-target current-normal-point])]
                                   (if (< i (- (count p) 2))
                                     (recur (inc i) (double new-record) new-target new-normal-point)
                                     [new-record new-target new-normal-point])))
          acceleration (if (and (> world-record radius) (not (= target position)))
                         (-> target
                             (v/sub position)
                             (v/normalize)
                             (v/mult maxspeed)
                             (v/sub velocity)
                             (v/limit maxforce))
                         (Vec2. 0.0 0.0))
          nvelocity (-> velocity
                        (v/add acceleration)
                        (v/limit maxspeed))
          ^Vec2 nposition (v/add position velocity)

          ^Vec2 end (last p)
          ^Vec2 start (first p)
          ^Vec2 nposition (if (> (.x nposition) (+ r (.x end)))
                            (Vec2. (- (.x start) r)
                                   (+ (.y start) (- (.y nposition) (.y end))))
                            nposition)
          theta (+ (m/radians 90) ^double (v/heading nvelocity))]

      (when (second (get-state window))
        (-> canvas
            (set-color :black 200)
            (line (.x position) (.y position ) (.x predict-pos) (.y predict-pos))
            (ellipse (.x predict-pos) (.y predict-pos) 4 4)

            (line (.x predict-pos) (.y predict-pos) (.x normal-point) (.y normal-point))
            (ellipse (.x normal-point) (.y normal-point) 4 4)

            (set-color (if (> world-record radius) :red :black))
            (ellipse (.x target) (.y target) 8 8)))

      (-> canvas
          (set-color 175 0 0)
          (push-matrix)
          (translate (.x nposition) (.y nposition))
          (rotate theta)
          (triangle 0 (- r2) (- r) r2 r r2)
          (pop-matrix))

      (Vehicle. nposition nvelocity maxspeed maxforce))))

(defn make-vehicle
  ""
  []
  (let [ms (r/drand 2.0 3.0)]
    (Vehicle. (Vec2. 0 (/ h 2))
              (Vec2. ms 0)
              ms
              (r/drand 0.04 0.1))))

(defn draw
  ""
  [canvas window framecount state]
  (let [vs (or state (repeatedly 5 make-vehicle))]
    (-> canvas
        (set-background :white)
        (draw-path (first (get-state window))))
    (mapv #(follow-and-draw % canvas window (first (get-state window))) vs)))

(def window (show-window {:canvas (make-canvas w h)
                          :window-name "Path following 6_6"
                          :draw-fn draw
                          :state [(make-path) true]}))

(defmethod mouse-event ["Path following 6_6" :mouse-released] [e [_ debug]]
  [(make-path) debug])

(defmethod key-pressed ["Path following 6_6" \space] [_ [p debug]]
  [p (not debug)])
