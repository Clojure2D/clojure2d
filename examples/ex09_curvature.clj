;; https://generateme.wordpress.com/2016/05/04/curvature-from-noise/

(ns examples.ex09-curvature
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.math.joise :as n]
            [clojure2d.extra.variations :refer :all])
  (:import  [java.awt Color]
            [clojure2d.math.vector Vec2 Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:const width 800)
(def ^:const height 800)
(def ^:const border 200)

(def ^:const point-step 1.0)
(def ^:const point-size 0.7)
(def ^:const coord-scale 3.0)
(def ^:const angle-scale 7.0)

(defn make-particle
  ""
  []
  (Vec3. (m/drand border (- width border)) (m/drand border (- height border)) (m/drand m/TWO_PI)))

(defn move-particle
  ""
  [canvas ^Vec2 vshift fun noise ^Vec3 in]
  (let [nx (+ (.x in) (* point-step (m/qcos (.z in))))
        ny (+ (.y in) (* point-step (m/qsin (.z in))))
        xx (m/norm nx 0 width -1 1)
        yy (m/norm ny 0 height -1 1)
        ^Vec2 v (fun (v/mult (Vec2. xx yy) coord-scale))
        ^Vec2 vv (v/add v vshift)
        angle (+ (.z in) (* angle-scale (m/norm (noise (.x vv) (.y vv)) 0 1 -1 1)))]
    (with-canvas canvas
      (set-color (Color. 20 20 20 20))
      (set-stroke point-size)
      (point nx ny))
    (Vec3. nx ny angle)))

(defn example-09
  []
  (let [canvas (create-canvas width height)
        [frame running] (show-window canvas "curvature" width height 25)
        noise (n/make-random-fractal)
        variation1 (rand-nth variation-list)
        variation2 (rand-nth variation-list)
        vshift (Vec2. (m/drand -3 3) (m/drand -3 3))
        mv-fun (partial move-particle canvas vshift (comp (make-variation variation2 1.0 {}) (make-variation variation1 1.0 {})) noise)
        particles (repeatedly 5000 make-particle)]
    
    (defmethod key-pressed ["curvature" \space] [_]
      (let [r (to-hex (m/irand) 8)]
    (save-canvas canvas (str "results/ex09/" r ".jpg"))))

    (println (str variation1 " " variation2))

    (with-canvas canvas
      (set-background (Color. 240 240 240)))

    (loop [xs particles]
      (when @running
        (recur (doall (map mv-fun xs)))))
    
    ))


(example-09)

