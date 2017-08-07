(ns examples.ex05-particles
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v]
            [clojure2d.math.joise :as n]
            [clojure2d.extra.variations :refer :all]
            [clojure.pprint :refer [pprint]])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^long w 1200)
(def ^:const ^long h 1200)

(def ^:const ^double point-step 15.0)
(def ^:const ^double rscale 25.0)
(def ^:const ^double angle-mult 16.0)
(def ^:const ^double point-size 0.9)
(def ^:const ^int alpha 20)

(defn make-particle
  ""
  []
  (Vec2. (r/drand w) (r/drand h)))

(defn move-particle
  ""
  [^Vec2 vrand line? fun noise canvas ^Vec2 in]
  (let [xx (m/norm (.x in) 0 w -2 2)
        yy (m/norm (.y in) 0 h -2 2)
        ^Vec2 vr (v/add vrand (Vec2. xx yy))
        ^Vec2 v (v/div (fun vr) rscale)
        ^double n (noise (.x v) (.y v))
        ang (* n m/TWO_PI angle-mult)
        nx (+ (.x in) (* point-step (m/qcos ang)))
        ny (+ (.y in) (* point-step (m/qsin ang))) 
        col (m/cnorm (m/sqrt n) 0 1 100 240)]
    (if (and (<= 80 ny (- h 81)) (<= 80 nx (- w 81)))
      (do
        (set-color canvas col col col alpha)
        
        (if line?
          (line canvas (.x in) (.y in) nx ny)
          (point canvas nx ny))
        
        (Vec2. nx ny))
      (make-particle))))

(defn example-05
  []
  (binding [*skip-random-variations* true]
    (let [canvas (create-canvas w h)
          window (show-window canvas "particles" w h 25)
          noise (n/make-random-fractal-noise)
          field-config (make-random-configuration)
          field (make-combination field-config)
          vrand (Vec2. (r/drand -1 1) (r/drand -1 1))
          mv-fun (partial move-particle vrand (r/brand) field noise)       
          particles (repeatedly 25000 make-particle)
          looper (fn [canvas] (loop [xs particles]
                                (if (window-active? window)
                                  (recur (mapv (partial mv-fun canvas) xs))
                                  canvas)))]
      
      (defmethod key-pressed ["particles" \space] [_ _]
        (save-canvas canvas (next-filename "results/ex05/" ".jpg")))

      (pprint field-config)

      (with-canvas canvas
        (set-background 10 10 10)
        (set-stroke point-size)
        looper)    
      
      )))

(example-05)

