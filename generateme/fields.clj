(ns generateme.fields
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v]
            [clojure2d.math.joise :as j]
            [clojure2d.extra.variations :as vr]
            [clojure2d.extra.glitch :as g]
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
(def spots-overlay (o/make-spots 80 [30 60 120 180] width height))

(do

  (defn fields-config
    ""
    []
    (binding [vr/*skip-random-variations* true]
      (let [var-conf (vr/make-random-configuration (r/irand 4))
            palseq (filter #(< ^double (v/mx (c/set-alpha (first %) 0)) 50) (repeatedly #(:palette (g/color-reducer-machine))))
            vrange (double (r/drand 1 4))]
        {:noise (if (r/brand 0.3) (j/make-random-fractal) (r/make-perlin-noise (r/irand) (r/irand 2 4)))
         :col2 (ffirst palseq)
         :col1 (Vec4. 1 1 1 255)
         :var-conf var-conf
         :variation (vr/make-combination var-conf) 
         :angle-mult (r/drand 1.0 50.0)
         :point-step (r/drand 1.0 50.0)
         :with-noise (r/brand)
         :angle-phase (r/drand m/TWO_PI)
         :mnrange (- vrange)
         :mxrange vrange
         :rscale (-> (r/drand 0.1 1.0)
                     m/sq
                     (* 30.0))
         :vshift (if (r/brand 0.2)
                   (Vec2. 0.0 0.0)
                   (Vec2. (r/drand -1.0 1.0) (r/drand -1.0 1.0)))})))

  (def config (fields-config))


  (defn make-particle
    ""
    []
    (Vec3. (r/drand width) (r/drand height) (r/irand 5 200)))

  (def canvas (create-canvas width height))
  (def window (show-window canvas "Fields" (* 0.4 width) (* 0.4 height) 25))

  (defmethod key-pressed ["Fields" \space] [_]
    (save-canvas canvas (next-filename "generateme/fields/" ".png")))

  (defn make-do-step
    ""
    [canvas {:keys [noise variation ^double angle-mult ^double point-step ^double rscale ^Vec2 vshift
                    with-noise ^Vec4 col1 ^Vec4 col2 ^double mnrange ^double mxrange ^double angle-phase]}]
    (fn [^Vec3 v]
      (let [xx (m/norm (.x v) 0.0 width mnrange mxrange)
            yy (m/norm (.y v) 0.0 height mnrange mxrange)
            ^Vec2 vr (v/add vshift (Vec2. xx yy))
            ^Vec2 va (variation vr)
            n ^double (if with-noise
                        (let [^Vec2 res (v/div va rscale)]
                          ^double (noise (.x res) (.y res)))
                        (/ (+ m/PI ^double (v/heading va)) m/TWO_PI))
            ang (+ angle-phase (* n m/TWO_PI angle-mult))
            sa (* point-step (m/sin ang))
            ca (* point-step (m/cos ang))
            nx (+ (.x v) sa)
            ny (+ (.y v) ca)
            s (+ 2.0 (* n 6.0))
            col (c/set-alpha (v/interpolate col1 col2 n) 5)]
        (if (and (<= bleft ny bright)
                 (<= bleft nx bright)
                 (pos? (.z v)))
          (do
            ;; (point canvas nx ny)
            ;; (line canvas (.x v) (.y v) nx ny)
            (set-color canvas col)
            (ellipse canvas (.x v) (.y v) s s)
            (Vec3. nx ny (dec (.z v))))
          (make-particle))
        )))

  (defn render-fields
    ""
    [canvas]
    (let [points (repeatedly 7000 make-particle)
          step-fn (make-do-step canvas config)]

      (pprint config)

      (loop [s points
             iter (int 0)]
        (if (and @(window 1) (< iter 1200))
          (do
            (when (zero? ^int (mod iter 100)) (println iter))
            (recur (mapv step-fn s) (inc iter)))
          (println :done))))
    canvas)

  (with-canvas canvas
    (set-background 240 240 240)
    (set-color 10 11 12 5)
    (render-fields)
    (image (->> (@canvas 1)
                (o/render-noise noise-overlay)
                (o/render-spots spots-overlay)))))
