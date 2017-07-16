(ns NOC.ch06.pathfollowingsimple-6-5
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

(def debug (atom true))

(deftype PathSimple [start end])

(def ^PathSimple path-global
  (PathSimple. (Vec2. 0.0 (/ h 3.0))
               (Vec2. w (* 2 (/ h 3.0)))))

(defn draw-path
  ""
  [canvas]
  (let [^Vec2 start (.start path-global)
        ^Vec2 end (.end path-global)]
    (-> canvas
        (set-color :black 100)
        (set-stroke (+ radius radius))
        (line (.x start) (.y start) (.x end) (.y end))
        (set-color :black)
        (set-stroke 1.0)
        (line (.x start) (.y start) (.x end) (.y end)))))

(defprotocol VehicleProto
  (follow-and-draw [v canvas]))

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
  (follow-and-draw [_ canvas]
    (let [^Vec2 end (.end path-global)
          ^Vec2 start (.start path-global)

          ^Vec2 predict-pos (-> velocity
                                (v/normalize)
                                (v/mult 50.0)
                                (v/add position))
          ^Vec2 normal-point (get-normal-point predict-pos start end)
          ^Vec2 target (-> (v/sub end start)
                           (v/normalize)
                           (v/mult 10.0)
                           (v/add normal-point))
          ^double distance (v/dist predict-pos normal-point)
          acceleration (if (and (> distance radius) (not (= target position)))
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

          ^Vec2 nposition (if (> (.x nposition) (+ r (.x end)))
                            (Vec2. (- (.x start) r)
                                   (+ (.y start) (- (.y nposition) (.y end))))
                            nposition)
          theta (+ (m/radians 90) ^double (v/heading nvelocity))]

      (when @debug
        (-> canvas
            (set-color :black 200)
            (line (.x position) (.y position ) (.x predict-pos) (.y predict-pos))
            (ellipse (.x predict-pos) (.y predict-pos) 4 4)

            (line (.x predict-pos) (.y predict-pos) (.x normal-point) (.y normal-point))
            (ellipse (.x normal-point) (.y normal-point) 4 4)

            (set-color (if (> distance radius) :red :black))
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
        (draw-path))
    (mapv #(follow-and-draw % canvas) vs)))

(def window (show-window (make-canvas w h) "Path following simple 6_5" draw))

(defmethod key-pressed ["Path following simple 6_5" \space] [_]
  (swap! debug not))
