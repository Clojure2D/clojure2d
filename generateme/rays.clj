(ns generateme.rays
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

(defn rays-config
  ""
  []
  (binding [vr/*skip-random-variations* true]
    (let [var1-conf (vr/make-random-configuration (r/irand 4))
          palseq (filter #(< 110 (c/get-luma (first %)) 240) (repeatedly #(:palette (g/color-reducer-machine))))
          curl (+ m/TWO_PI ^double (r/drand (- m/PI) m/PI))
          scale-down (m/norm curl m/PI (+ m/PI m/TWO_PI) 200.0 1000.0)]
      {:noise1 (if (r/brand 0.2) (j/make-random-fractal) (r/make-perlin-noise (r/irand) (r/irand 2 4)))
       :var1-conf var1-conf
       :var1 (vr/make-combination var1-conf) 
       :ray-length 5000
       :angle-step (/ m/TWO_PI 19432.0) 
       :curl curl
       :scale-up (r/drand 1 4)
       :scale-down scale-down
       :assymetry (if (r/brand 0.7) (Vec2. 0.0 0.0) (Vec2. (r/drand -1.0 1.0) (r/drand -1.0 1.0)))
       :col1a (ffirst palseq)
       :col1b (second (first palseq))       
       :col2a (first (second palseq))
       :col2b (second (second palseq))
       :independent (r/brand 0.4)})))

(def config (rays-config))

(do

  (def half (int (* ^double (m/norm (:curl config) m/PI (+ m/PI m/TWO_PI) 1.0 0.7) width 0.5)))

  ;; (def half 2000)

  
  (def border (int (* 0.9 ^int half)))

  (def canvas (create-canvas width height))
  (def binpixels (p/make-binpixels [(- ^int half) half (- ^int half) half] width height))

  (defn draw-on-canvas
    "Render BinPixels to canvas."
    [canvas fc state]
    (p/set-canvas-pixels canvas (p/to-pixels binpixels
                                             (Vec4. 15 5 5 255)
                                             {:alpha-gamma 1.4 :color-gamma 1.2 :intensity 0.5 :saturation 1.6 :brightness 1.1}))  )

  (def window (show-window canvas "Rays" (* 0.4 width) (* 0.4 height) 1 #(draw-on-canvas %1 %2 %3)))

  (defmethod key-pressed ["Rays" \space] [_]
    (save-canvas canvas (next-filename "generateme/rays/" ".png")))

  (defn make-point
    ""
    [^double angle]
    (Vec3. (r/grand 20.0) (r/grand 20.0) (+ ^double (r/grand 0.01) angle)))

  (defn make-do-step
    ""
    [{:keys [var1 noise1 ^Var4 col1a ^Var4 col2a ^Var4 col1b ^Var4 col2b
             ^double scale-up ^double scale-down ^double curl ^Vec2 assymetry
             independent]}]
    (let [scscale (double (/ ^int half 1000.0))]
      (fn [^Vec3 v] 
        (let [^Vec2 v1 (-> (Vec2. (.x v) (.y v))
                           (v/div scale-down)
                           (v/add assymetry)
                           (var1)
                           (v/mult scale-up))
              ^double n1 (if independent (noise1 (.x v1)) (noise1 (.x v1) (.y v1)))
              ^double n2 (if independent (noise1 (.y v1) 1.11) (noise1 (.y v1) (.x v1) 1.11))
              s (m/sin (+ (* curl n1) (.z v)))
              c (m/cos (+ (* curl n2) (.z v)))
              c1 (v/interpolate col1a col2a n1)
              c2 (v/interpolate col1b col2b n2)
              ^Vec4 col (v/interpolate c1 c2 (m/abs (* s c)))
              nx (+ (.x v) (* scscale s))
              ny (+ (.y v) (* scscale c))]
          (p/add-pixel-bilinear binpixels nx ny (.x col) (.y col) (.z col))
          (if (and (< (m/abs nx) ^int border)
                   (< (m/abs ny) ^int border))
            (Vec3. nx ny (.z v))
            (make-point (r/drand m/TWO_PI)))))))

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
              (println iter))
            (recur (mapv step-fn s) (inc iter)))
          (println :done)))))

  (render-rays))

;; (repeatedly 3 render-rays)

