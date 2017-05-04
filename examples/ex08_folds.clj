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

(def ^:const ^long w 600)
(def ^:const ^long h 600)

(def ^:const ^double x1 -3.1)
(def ^:const ^double y1 -3.1)
(def ^:const ^double x2 3.1)
(def ^:const ^double y2 3.1)
(def ^:const ^double step (/ (- x2 x1) (* 2.321 w)))

(def sinusoidal (vr/make-variation :sinusoidal 3.0 {}))

(def s80 (make-spots 80 [60 120 180] w h))
(def n80 (make-noise 80 w h))

(defn make-me
  ""
  [canvas disp]
  (let [field-config (vr/make-random-configuration)
        field (comp sinusoidal (vr/make-combination field-config))
        field (comp sinusoidal (vr/make-variation :taurus 1.0 {}))
        ] 

    (pprint field-config)
    
    (loop [y y1]
      (loop [x x1]
        
        (let [^Vec2 vv (field (Vec2. x y))
              xx (m/norm (+ (.x vv) ^double (r/grand 0.003)) x1 x2 20 (- w 20))
              yy (m/norm (+ (.y vv) ^double (r/grand 0.003)) y1 y2 20 (- h 20))]
          (point canvas xx yy))

        (when (and @disp (< x x2)) (recur (+ x step))))
      (when (and @disp (< y y2)) (recur (+ y step)))))
  canvas)

(defn draw-folds
  ""
  [[canvas disp]]
  (with-canvas canvas
    (set-background 250 250 250)
    (set-color 20 20 20 15)
    (set-stroke 0.9)
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
