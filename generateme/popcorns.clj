(ns generateme.popcorns
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
(def ^:const ^double half (* 0.5 2048))
(def ^Vec2 halfv (Vec2. half half))

(def ^:const ^double bleft (* 0.05 width))
(def ^:const ^double bright (- width bleft))
(def ^:const ^double ring-size (* 0.3 bright))

(def noise-overlay (future (o/make-noise 60 width height)))
(def spots-overlay (future (o/make-spots 80 [30 60 120 180] width height)))

(defn popcorns-config
  ""
  []
  (binding [vr/*skip-random-variations* true]
    (let [var-conf (vr/make-random-configuration)
          palseq (filter #(< ^double (v/mx (c/set-alpha (first %) 0)) 70) (repeatedly #(:palette (g/color-reducer-machine))))
          vrange (double (r/drand 1 4))]
      {:noise (if (r/brand 0.3) (j/make-random-fractal) (r/make-perlin-noise (r/irand) (r/irand 2 4)))
       :col2 (ffirst palseq)
       :col1 (first (second palseq))
       :var-conf var-conf
       :variation (vr/make-combination var-conf) 
       :with-noise (r/brand)
       :mnrange (- vrange)
       :mxrange vrange
       :sinusoidal? (r/brand 0.7)
       :rscale (-> (r/drand 0.1 1.0)
                   m/sq
                   (* 30.0))
       :vshift (if (r/brand 0.2)
                 (Vec2. 0.0 0.0)
                 (Vec2. (r/drand -1.0 1.0) (r/drand -1.0 1.0)))})))

(defn get-noise
  ""
  [w f s ^Vec2 in]
  (if w
    (let [^Vec2 in (v/mult in s)]
      (Vec2. (- ^double (f (.x in) (.y in)) 0.5)
             (- ^double (f (.y in) (.x in) 0.3) 0.5)))
    in))


(defn make-particle
  ""
  []
  (v/add (v/mult (first (filter #(< 0.1 ^double (v/mag %) 1.0) (repeatedly #(Vec2. (r/drand -1.0 1.0) (r/drand -1.0 1.0))))) ring-size) halfv))

(def sinusoidal (vr/make-variation :sinusoidal 1.0 {}))

(def config (popcorns-config))

(do

  (def canvas (create-canvas width height))
  (def window (show-window canvas "Popcorns" (* 0.4 width) (* 0.4 height) 25))

  (defmethod key-pressed ["Popcorns" \space] [_]
    (save-canvas canvas (next-filename "generateme/popcorns/" ".png")))

  (defn make-do-step
    ""
    [canvas {:keys [noise variation ^double rscale ^Vec2 vshift
                    with-noise ^Vec4 col1 ^Vec4 col2 ^double mnrange ^double mxrange
                    sinusoidal?]}]
    (fn [^Vec2 v]
      (let [xx (m/norm (.x v) 0.0 width mnrange mxrange)
            yy (m/norm (.y v) 0.0 height mnrange mxrange)

            ^Vec2 resss (->> (Vec2. xx yy)
                             (v/add vshift)
                             variation 
                             (get-noise with-noise noise rscale))
            
            ^Vec2 resv (v/add v (if sinusoidal?
                                  (sinusoidal (v/mult resss m/TWO_PI))
                                  resss))

            n (m/abs (m/qsin (* m/TWO_PI  (.x resss) (.y resss))))
            
            s (+ 1.0 (* n 4.0))
            col (c/set-alpha (v/interpolate col1 col2 n) 5)]
        (if (and (<= bleft (.y resv) bright)
                 (<= bleft (.x resv) bright))
          (do
            ;; (point canvas nx ny)
            ;; (line canvas (.x v) (.y v) nx ny)
            (set-color canvas col)
            (ellipse canvas (.x resv) (.y resv) s s)
            resv)
          (make-particle)))))

  (defn render-popcorns
    ""
    [canvas]
    (let [points (repeatedly 8000 make-particle)
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
    (set-background 250 245 240)
    (set-color 1 2 3 5)
    (render-popcorns)
    (image (->> (@canvas 1)
                (o/render-noise @noise-overlay)
                (o/render-spots @spots-overlay)))))
