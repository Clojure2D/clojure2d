(ns generateme.rays2
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v]
            [clojure2d.math.joise :as j]
            [clojure2d.extra.variations :as vr]
            [clojure2d.pixels :as p]
            [clojure2d.extra.glitch :as g]
            [clojure2d.color :as c]
            [clojure.pprint :refer [pprint]])
  (:import [clojure2d.math.vector Vec2 Vec3 Vec4]
           [clojure2d.pixels BinPixels]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int width 2048)
(def ^:const ^int height 2048)
(def ^:const ^int half (int (/ width 2)))
(def ^:const ^int border (int (* 0.9 half)))

(defn rays-config
  ""
  []
  (binding [vr/*skip-random-variations* true]
    (let [var1-conf (vr/make-random-configuration (r/irand 4))
          palseq (filter #(< 120 (c/get-luma (first %)) 230) (repeatedly #(:palette (g/color-reducer-machine))))
          curl (+ m/TWO_PI ^double (r/drand (- m/PI) m/PI))
          scale-down (m/norm curl m/PI (+ m/PI m/TWO_PI) 100.0 1000.0) ]
      {:noise1 (if (r/brand 0.2) (j/make-random-fractal) (r/make-perlin-noise (r/irand) (r/irand 1 4)))
       :var1-conf var1-conf
       :var1 (vr/make-combination var1-conf) 
       :ray-length 5000
       :angle-step (/ m/TWO_PI 15432.0) 
       :col1 (ffirst palseq)
       :curl curl
       :scale-up (r/drand 0.5 4)
       :scale-down scale-down
       :col2 (first (second palseq))})))

(def config (rays-config))

(def canvas (create-canvas width height))
(def binpixels (p/make-binpixels [(- half) half (- half) half] width height))

(defn draw-on-canvas
  "Render BinPixels to canvas."
  [canvas fc state]
  (p/set-canvas-pixels canvas (p/to-pixels binpixels
                                           (Vec4. 15 5 5 255)
                                           {:alpha-gamma 1.5 :color-gamma 0.8 :intensity 0.5}))  )

(def window (show-window canvas "Rays" (* 0.4 width) (* 0.4 height) 1 #(draw-on-canvas %1 %2 %3)))

(defmethod key-pressed ["Rays" \space] [_]
  (save-canvas canvas (next-filename "generateme/rays/" ".jpg")))

(defn make-point
  ""
  [^double angle]
  (Vec3. (r/grand 20.0) (r/grand 20.0) (+ ^double (r/grand 0.01) angle)))

(defn make-do-step
  ""
  [{:keys [var1 noise1 ^Var4 col1 ^Var4 col2 ^double scale-up ^double scale-down ^double curl]}]
  (fn [^Vec3 v] 
    (let [^Vec2 v1 (v/mult (var1 (v/div (Vec2. (.x v) (.y v)) scale-down)) scale-up)
          n1 (* curl ^double (noise1 (.x v1)))
          n2 (* curl ^double (noise1 (.y v1)))
          s (m/sin (+ n1 (.z v)))
          c (m/cos (+ n2 (.z v)))
          ^Vec4 col (v/interpolate col1 col2 (m/abs (* s c)))
          nx (+ (.x v) s)
          ny (+ (.y v) c)]
      (p/add-pixel-bilinear binpixels nx ny (.x col) (.y col) (.z col))
      (if (and (< (m/abs nx) border)
               (< (m/abs ny) border))
        (Vec3. nx ny (.z v))
        (make-point (r/drand m/TWO_PI))))))

(defn render-rays
  ""
  []
  (let [rays (map make-point (range 0 m/TWO_PI (:angle-step config)))
        step-fn (make-do-step config)
        stop (int (:ray-length config))]

    (pprint config)
    
    (loop [s rays
           iter (int 0)]
      (if (and @(window 1) (< iter stop))
        (do
          (when (zero? ^long (mod iter 100))
            (println iter)
            (println (first s)))
          (recur (mapv step-fn s) (inc iter)))
        (println :done)))))

(render-rays)
