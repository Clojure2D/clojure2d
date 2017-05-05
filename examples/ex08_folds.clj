;; rework of https://generateme.wordpress.com/2014/12/11/story-of-one-picture/

(ns examples.ex08-folds
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v]
            [clojure2d.extra.variations :as vr]
            [clojure2d.extra.overlays :refer :all]
            [clojure.pprint :refer [pprint]])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:const ^long w 540)
(def ^:const ^long h 540)

(def ^:const ^double x1 -2.0)
(def ^:const ^double y1 -2.0)
(def ^:const ^double x2 2.0)
(def ^:const ^double y2 2.0)
(def ^:const ^double step (/ (- x2 x1) (* 1.8 w)))

(def ^:const ^double x1- (dec x1))
(def ^:const ^double y1- (dec y1))
(def ^:const ^double x2+ (inc x2))
(def ^:const ^double y2+ (inc y2))
(def ^:const ^long w- (dec w))
(def ^:const ^long h- (dec h))

(def ^:const ^double scale 1.0)

(def sinusoidal (vr/make-variation :sinusoidal 2.7 {}))

(def s80 (make-spots 60 [60 120 180] w h))
(def n80 (make-noise 60 w h))

(defn make-me
  ""
  [canvas disp]
  (let [field-config (vr/make-random-configuration)
        field (comp sinusoidal (vr/make-combination field-config))
        field (comp sinusoidal (vr/make-variation :juliac 1.0 {}))
        ] 

    (pprint field-config)
    
    (loop [y y1]
      (loop [x x1]
        
        (let [^Vec2 vv (v/mult (field (Vec2. x y)) scale)
              xx (m/norm (+ (.x vv) ^double (r/grand 0.0012)) x1- x2+ 0.0 w)
              yy (m/norm (+ (.y vv) ^double (r/grand 0.0012)) y1- y2+ 0.0 h)]
          (point canvas xx yy))

        (when (and @disp (< x x2)) (recur (+ x step))))
      (when (and @disp (< y y2)) (recur (+ y step)))))
  canvas)

(defn draw-folds
  ""
  [[canvas disp]]
  (with-canvas canvas
    (set-background 255 250 245)
    (set-color 35 35 35 16)
    (make-me disp)
    (image (render-noise n80 (@canvas 1)))
    (image (render-spots s80 (@canvas 1))))
  :done)

(defn example-08
  ""
  []
  (let [canvas (create-canvas w h)
        [_ disp] (show-window canvas "folds" w h 25)]

    (defmethod key-pressed ["folds" \space] [_]
      (save-canvas canvas (next-filename "results/ex08/" ".jpg")))

    [canvas disp]))

(draw-folds (example-08))

