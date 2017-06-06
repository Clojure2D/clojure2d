(ns examples.ex05-particles
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v]
            [clojure2d.math.joise :as n]
            [clojure2d.extra.variations :refer :all])
  (:import  [java.awt Color]
            [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^long width 1200)
(def ^:const ^long height 1200)

(def ^:const ^double point-step 15.0)
(def ^:const ^double rscale 25.0)
(def ^:const ^double angle-mult 16.0)
(def ^:const ^double point-size 0.9)
(def ^:const ^int alpha 20)

(defn make-particle
  ""
  []
  (Vec2. (r/drand width) (r/drand height)))

(defn move-particle
  ""
  [^Vec2 vrand line? fun noise canvas ^Vec2 in]
  (let [xx (m/norm (.x in) 0 width -2 2)
        yy (m/norm (.y in) 0 height -2 2)
        ^Vec2 vr (v/add vrand (Vec2. xx yy))
        ^Vec2 v (v/div (fun vr) rscale)
        ^double n (noise (.x v) (.y v))
        ang (* n m/TWO_PI angle-mult)
        nx (+ (.x in) (* point-step (m/qcos ang)))
        ny (+ (.y in) (* point-step (m/qsin ang))) 
        col (m/cnorm (m/sqrt n) 0 1 100 240)]
    (if (and (<= 80 ny (- height 81)) (<= 80 nx (- width 81)))
      (do
        (set-color canvas col col col alpha)
        
        (if line?
          (line canvas (.x in) (.y in) nx ny)
          (point canvas nx ny))
        
        (Vec2. nx ny))
      (make-particle))))

(defn example-05
  []
  (let [canvas (create-canvas width height)
        [frame running] (show-window canvas "particles" width height 25)
        noise (n/make-random-fractal)
        variation1 (rand-nth variation-list-not-random)
        variation2 (rand-nth variation-list-not-random)
        vrand (Vec2. (r/drand -1 1) (r/drand -1 1))
        mv-fun (partial move-particle vrand (r/brand) (comp (make-variation variation2 1.0 {}) (make-variation variation1 1.0 {})) noise)       
        particles (vec (repeatedly 25000 make-particle))
        looper (fn [canvas] (loop [xs particles]
                              (if @running
                                (recur (mapv (partial mv-fun canvas) xs))
                                canvas)))]
    
    (defmethod key-pressed ["particles" \space] [_]
      (save-canvas canvas (next-filename "results/ex05/" ".jpg")))

    (println (str variation1 " " variation2))

    (with-canvas canvas
      (set-background 10 10 10)
      (set-stroke point-size)
      looper)    
    
    ))

(example-05)

