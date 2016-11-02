;; rework of https://generateme.wordpress.com/2014/12/11/story-of-one-picture/

(ns examples.ex08-folds
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.extra.variations :as vr]
            [clojure2d.extra.overlays :refer :all])
  (:import [clojure2d.math.vector Vec2]
           [java.awt Color]))

(def ^:const w 600)
(def ^:const h 600)

(def ^:const x1 -3.0)
(def ^:const y1 -3.0)
(def ^:const x2 3.0)
(def ^:const y2 3.0)
(def ^:const step (/ (- x2 x1) (* 2.321 w)))

(def sinusoidal (vr/make-variation :sinusoidal 3.0 {}))

(def s80 (make-spots 80 [60 120 180] 646 800))
(def n80 (make-noise 80 646 800))

(defn create-field
  ""
  []
  (let [one-field? (m/brand 0.5)
        field-name1 (rand-nth vr/variation-list)
        field-name2 (rand-nth vr/variation-list)
        field (if one-field?
                (vr/make-variation field-name1 1.0 {})
                (comp (vr/make-variation field-name2 1.0 {}) (vr/make-variation field-name1 1.0 {})))]
    (if one-field?
      (println field-name1)
      (println (str field-name1 " " field-name2)))
    field))

(defn make-me
  ""
  [canvas disp]
  (let [field (create-field)]
    (loop [y y1]
      (loop [x x1]
        
        (let [^Vec2 vv (field (Vec2. x y))
              xx (m/norm (+ (.x vv) (m/grand 0.003)) x1 x2 20 (- w 20))
              yy (m/norm (+ (.y vv) (m/grand 0.003)) y1 y2 20 (- h 20))]
          (point canvas xx yy))

        (when (and @disp (< x x2)) (recur (+ x step))))
      (when (and @disp (< y y2)) (recur (+ y step)))))
  canvas)

(defn draw-folds
  ""
  [[canvas disp]]
  (with-canvas canvas
    (set-background (Color. 250 250 250))
    (set-color (Color. 20 20 20 15))
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
             (let [r (clojure2d.utils/to-hex (m/irand) 8)]
               (save-canvas canvas (str "results/ex08/" r ".jpg"))))

  [canvas disp]))

(draw-folds (example-08))
