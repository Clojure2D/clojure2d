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
(set! *unchecked-math* :warn-on-boxed)

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

(def ^:const ^double fscale 1.0)

(def s60 (future (make-spots 60 [60 120 180] w h)))
(def n60 (future (make-noise 60 w h)))

(defn make-me
  ""
  [canvas window]
  (let [field-config (vr/make-random-configuration)
        field (vr/make-combination field-config)
        ;; field (vr/make-variation :vibration 1.0 {})
        
        ] 

    (pprint field-config)
    
    (loop [y y1]
      (loop [x x1]
        
        (let [^Vec2 vv (v/mult (v/applyf (v/mult (field (Vec2. x y)) fscale) m/sin) 2.7)
              xx (m/norm (+ (.x vv) ^double (r/grand 0.0012)) x1- x2+ 0.0 w)
              yy (m/norm (+ (.y vv) ^double (r/grand 0.0012)) y1- y2+ 0.0 h)]
          (point canvas xx yy))

        (when (and (window-active? window) (< x x2)) (recur (+ x step))))
      (when (and (window-active? window) (< y y2)) (recur (+ y step)))))
  canvas)

(defn draw-folds
  ""
  [[canvas disp]]
  (with-canvas canvas
    (set-background 255 250 245)
    (set-color 35 35 35 16)
    (make-me disp)
    (image (render-noise @n60 (get-image canvas)))
    (image (render-spots @s60 (get-image canvas))))
  :done)

(defn example-08
  ""
  []
  (let [canvas (create-canvas w h)
        window (show-window canvas "folds" 15 nil)]

    (defmethod key-pressed ["folds" \space] [_]
      (save-canvas canvas (next-filename "results/ex08/" ".jpg")))

    [canvas window]))

(draw-folds (example-08))
