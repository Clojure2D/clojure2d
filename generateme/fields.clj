(ns generateme.fields
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v]
            [clojure2d.math.joise :as j]
            [clojure2d.extra.variations :as vr]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [clojure2d.extra.overlays :as o]
            [clojure.pprint :refer [pprint]])
  (:import [clojure2d.math.vector Vec2 Vec3 Vec4]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int width 2048)
(def ^:const ^int height 2048)
(def ^:const ^double bleft (* 0.05 width))
(def ^:const ^double bright (- width bleft))

(def noise-overlay (o/make-noise 60 width height))

(defn fields-config
  ""
  []
  (binding [vr/*skip-random-variations* true]
    (let [var-conf (vr/make-random-configuration (r/irand 4))]
      {:noise (if (r/brand 0.3) (j/make-random-fractal) (r/make-perlin-noise (r/irand) (r/irand 2 4)))
       :var-conf var-conf
       :variation (vr/make-combination var-conf) 
       :angle-mult (r/drand 1.0 50.0)
       :point-step (r/drand 1.0 32.0)
       :rscale (-> (r/drand)
                   m/sq
                   (* 30.0)
                   (+ m/EPSILON))
       :vshift (if (r/brand 0.2)
                 (Vec2. 0.0 0.0)
                 (Vec2. (r/drand -1.0 1.0) (r/drand -1.0 1.0)))})))

(def config (fields-config))


(defn make-particle
  ""
  []
  (Vec2. (r/drand width) (r/drand height)))

(def canvas (create-canvas width height))
(def window (show-window canvas "Fields" (* 0.4 width) (* 0.4 height) 25))

(defmethod key-pressed ["Fields" \space] [_]
  (save-canvas canvas (next-filename "generateme/fields/" ".png")))

(defn make-do-step
  ""
  [canvas {:keys [noise variation ^double angle-mult ^double point-step ^double rscale ^Vec2 vshift]}]
  (fn [^Vec2 v]
    (let [xx (m/norm (.x v) 0.0 width -3.0 3.0)
          yy (m/norm (.y v) 0.0 height -3.0 3.0)
          ^Vec2 vr (v/add vshift (Vec2. xx yy))
          ^Vec2 res (v/div (variation vr) rscale)
          ^double n (noise (.x res) (.y res))
          ang (* n m/TWO_PI angle-mult)
          nx (+ (.x v) (* point-step (m/sin ang)))
          ny (+ (.y v) (* point-step (m/cos ang)))]
      (if (and (<= bleft ny bright)
               (<= bleft nx bright))
        (do
          ;; (point canvas nx ny)
          (line canvas (.x v) (.y v) nx ny)
          (Vec2. nx ny))
        (make-particle))
      )))

(defn render-fields
  ""
  [canvas]
  (let [points (repeatedly 30000 make-particle)
        step-fn (make-do-step canvas config)]

    (pprint config)

    (loop [s points
           iter (int 0)]
      (if (and @(window 1) (< iter 500))
        (do
          (when (zero? ^int (mod iter 100)) (println iter))
          (recur (mapv step-fn s) (inc iter)))
        (println :done))))
  canvas)

(with-canvas canvas
  (set-background 240 240 240)
  (set-color 10 11 12 5)
  (render-fields)
  (image (o/render-noise noise-overlay (@canvas 1))))
