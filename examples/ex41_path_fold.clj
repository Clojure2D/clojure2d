(ns examples.ex41-path-fold
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
(m/use-primitive-operators)

(def ^:const ^long w 2048)
(def ^:const ^long h 2048)

(def ^:const ^double x1 -2.5)
(def ^:const ^double y1 -2.5)
(def ^:const ^double x2 2.5)
(def ^:const ^double y2 2.5)
(def ^:const ^double stepx (/ (- x2 x1) 71))
(def ^:const ^double stepy (/ (- y2 y1) 121))

(def ^:const ^double x1- (dec x1))
(def ^:const ^double y1- (dec y1))
(def ^:const ^double x2+ (inc x2))
(def ^:const ^double y2+ (inc y2))
(def ^:const ^long w- (dec w))
(def ^:const ^long h- (dec h))

(def ^:const ^double fscale 0.7)

(def s60 (future (make-spots w h {:alpha 30 :intensities [60 120 180]})))
(def n60 (future (make-noise w h {:alpha 20})))

(defn make-me
  ""
  [canvas window]
  (binding [vr/*skip-random-variations* true]
    (let [field-config (vr/make-random-configuration 3)
          field (vr/make-combination field-config)] 

      (pprint field-config)
      
      (loop [y y1]

        (let [p (for [x (range x1 (+ x2 stepx) stepx)
                      :let [nv (Vec2. x y)
                            ^Vec2 vv (v/add nv (v/mult (v/applyf (field nv) #(m/sin (* 2.0 ^double %))) 0.3))
                            xx (m/norm (.x vv) x1- x2+ 0.0 w)
                            yy (m/norm (.y vv) y1- y2+ 0.0 h)]]
                  (Vec2. xx yy))]
          (set-stroke canvas (m/norm (m/qcos (m/norm y y1 y2 (- m/PI) m/PI)) -1.0 1.0 1.0 4.0))
          (path-bezier canvas p false true))
        
        (when (and (window-active? window) (<= y y2)) (recur (+ y stepy))))))
  
  canvas)

(defn draw-folds
  ""
  [[canvas disp]]
  (with-canvas-> canvas
    (set-background 14 9 4)
    (set-color 151 219 233 128)
    (make-me disp)
    (image (render-noise (get-image canvas) @n60))
    (image (render-spots (get-image canvas) @s60))
    ;; (image (render-rgb-scanlines canvas {:scale 1.01}))
    )
  :done)

(defn example-41
  ""
  []
  (let [canvas (create-canvas w h)
        window (show-window {:canvas canvas
                             :window-name "folds on path"
                             :w (* 0.4 w)
                             :h (* 0.4 h)
                             :fps 15
                             :hint :high})]

    (defmethod key-pressed ["folds on path" \space] [_ _]
      (save canvas (next-filename "results/ex41/" ".jpg")))

    [canvas window]))

(draw-folds (example-41))


