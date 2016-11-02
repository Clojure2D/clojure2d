;; https://generateme.wordpress.com/2016/05/04/curvature-from-noise/
;; forced movement

(ns examples.ex10-curvature2
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.math.joise :as n]
            [clojure2d.utils :as u]
            [clojure2d.extra.variations :refer :all])
  (:import  [java.awt Color]
            [clojure2d.math.vector Vec2 Vec3]))

(def ^:const width 800)
(def ^:const height 800)
(def ^:const border 200)

(def ^:const point-step 1.0) ; distance for particle in single step
(def ^:const point-size 0.71) ; stroke size
(def ^:const coord-scale 3.0) ; range for variation (-coord-scale, coord-scale)
(def ^:const angle-scale m/TWO_PI) ; angle modifier
(def ^:const shift-x-step 0.2) ; 
(def ^:const shift-x-scale 3.0)
(def ^:const shift-y-scale 10.0)
(def ^:const shift-y-step 0.2)

(defn make-particle
  ""
  []
  (Vec3. 0 (m/drand border (- height border)) (m/drand m/TWO_PI)))

(defn move-particle
  ""
  [canvas ^Vec2 vshift fun noise time ^Vec3 in]
  (let [step-x-shift (* shift-x-step (m/noise time (/ (.x in) shift-x-scale)))
        step-y-shift (m/norm (m/noise (/ (.y in) shift-y-scale) time) 0 1 (- shift-y-step) shift-y-step)
        nx (+ (.x in) step-x-shift (* point-step (m/qcos (.z in))))
        ny (+ (.y in) step-y-shift (* point-step (m/qsin (.z in))))
        xx (m/norm nx 0 width -1 1)
        yy (m/norm ny 0 height -1 1)
        ^Vec2 v (fun (v/mult (Vec2. xx yy) coord-scale))
        ^Vec2 vv (v/add v vshift)
        n (* angle-scale (m/norm (noise (.x vv) (.y vv)) 0 1 -1 1))
        angle (+ (.z in) n)]
    (with-canvas canvas
      (set-color (Color. 20 20 20 20))
      (set-stroke point-size)
      (point nx ny))
    (Vec3. nx ny angle)))

(defn example-10
  []
  (let [canvas (create-canvas width height)
        [frame running] (show-window canvas "curvature2" width height 25)
        noise (n/make-random-fractal)
        variation1 :miller ;(rand-nth variation-list)
        variation2 :bcollide; (rand-nth variation-list)
        vshift (Vec2. (m/drand -3 3) (m/drand -3 3))
        mv-fun (partial move-particle canvas vshift (comp (make-variation variation2 1.0 {}) (make-variation variation1 1.0 {})) noise)
        particles (repeatedly 500 make-particle)]
    
    (defmethod key-pressed ["curvature2" \space] [_]
      (let [r (u/to-hex (m/irand) 8)]
    (save-canvas canvas (str "results/ex10/" r ".jpg"))))

    (println (str variation1 " " variation2))

    (with-canvas canvas
      (set-background (Color. 240 240 240)))

    (loop [xs particles
           time 0.0]
      (when @running
        (recur (doall (map (partial mv-fun time) xs)) (+ time 0.001))))
    
    ))


(example-10)

