;; https://generateme.wordpress.com/2016/05/04/curvature-from-noise/
;; forced movement

(ns examples.ex10-curvature2
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as rr]
            [clojure2d.math.vector :as v]
            [clojure2d.math.joise :as n]
            [clojure2d.extra.variations :refer :all])
  (:import [clojure2d.math.vector Vec2 Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:const ^long width 800)
(def ^:const ^long height 800)
(def ^:const ^long border 200)

(def ^:const ^double point-step 1.0) ; distance for particle in single step
(def ^:const ^double point-size 0.71) ; stroke size
(def ^:const ^double coord-scale 3.0) ; range for variation (-coord-scale, coord-scale)
(def ^:const ^double angle-scale m/TWO_PI) ; angle modifier
(def ^:const ^double shift-x-step 0.2) ; 
(def ^:const ^double shift-x-scale 3.0)
(def ^:const ^double shift-y-scale 10.0)
(def ^:const ^double shift-y-step 0.2)

(defn make-particle
  ""
  []
  (Vec3. 0.0 (rr/drand border (- height border)) (rr/drand m/TWO_PI)))

(defn move-particle
  ""
  [canvas ^Vec2 vshift fun noise time ^Vec3 in]
  (let [step-x-shift (* shift-x-step ^double (rr/noise time (/ (.x in) shift-x-scale)))
        ^double step-y-shift (m/norm (rr/noise (/ (.y in) shift-y-scale) time) 0 1 (- shift-y-step) shift-y-step)
        nx (+ (.x in) step-x-shift (* point-step (m/qcos (.z in))))
        ny (+ (.y in) step-y-shift (* point-step (m/qsin (.z in))))
        xx (m/norm nx 0 width -1 1)
        yy (m/norm ny 0 height -1 1)
        ^Vec2 v (fun (v/mult (Vec2. xx yy) coord-scale))
        ^Vec2 vv (v/add v vshift)
        n (* angle-scale ^double (m/norm (noise (.x vv) (.y vv)) 0 1 -1 1))
        angle (+ (.z in) n)]
    (point canvas nx ny)
    (Vec3. nx ny angle)))

(defn go-for-it
  ""
  [canvas particles running mv-fun]
  (loop [xs particles
         time (double 0.0)]
    (when @running
      (recur (mapv (partial mv-fun time) xs) (+ time 0.001))))
  canvas)


(defn example-10
  []
  (let [canvas (create-canvas width height)
        [frame running] (show-window canvas "curvature2" width height 25)
        noise (n/make-random-fractal)
        variation1 (rand-nth variation-list-not-random)
        variation2 (rand-nth variation-list-not-random)
        vshift (Vec2. (rr/drand -3 3) (rr/drand -3 3))
        mv-fun (partial move-particle canvas vshift (comp (make-variation variation2 1.0 {}) (make-variation variation1 1.0 {})) noise)
        particles (repeatedly 500 make-particle)]
    
    (defmethod key-pressed ["curvature2" \space] [_]
      (save-canvas canvas (next-filename "results/ex10/" ".jpg")))

    (println (str variation1 " " variation2))

    (with-canvas canvas
      (set-background 240 240 240)
      (set-color 20 20 20 20)
      (set-stroke point-size)
      (go-for-it particles running mv-fun))))


(example-10)

