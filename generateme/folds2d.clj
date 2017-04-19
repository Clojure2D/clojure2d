(ns generateme.folds2d
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.joise :as j]
            [clojure2d.math.vector :as v]
            [clojure2d.extra.glitch :as g]
            [clojure2d.extra.variations :as vr]
            [clojure2d.color :as c]
            [clojure2d.core :as core]
            [clojure.pprint :refer [pprint]])
  (:import [clojure2d.pixels BinPixels]
           [clojure2d.math.vector Vec2 Vec3 Vec4]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int width 2048)
(def ^:const ^int height 2048)
(def ^:const ^int hwidth (int (/ width 2)))
(def ^:const ^int hheight (int (/ height 2)))

(defn make-jnoise
  "Create own version of joise noise (only simplex based, no cells)"
  []
  (j/make-noise (j/make-fractal {:type (rand-nth (keys j/fractal-type))
                                 :lacunarity (r/drand 1.0 3.0)
                                 :frequency (r/drand 1.0 3.0)
                                 :octaves [[1 (j/make-random-basis-module)]
                                           [1 (j/make-random-basis-module)]]})))

(defn get-random-noise
  ""
  []
  (rand-nth [(make-jnoise) r/noise j/perlin-noise]))


(def sinusoidal (vr/make-variation :sinusoidal 2.8 {}))

(defn create-field
  ""
  []
  (let [one-field? (r/brand 0.25)
        derivative? (r/brand 0.1)
                                        ; sinusoidal? (r/brand 0.25)
        field-name1 (rand-nth vr/variation-list-not-random)
        field-name2 (rand-nth vr/variation-list-not-random)
        field (if one-field?
                (vr/make-variation field-name1 1.0 {})
                (comp (vr/make-variation field-name2 1.0 {}) (vr/make-variation field-name1 1.0 {})))
        field (if derivative? (vr/derivative field) field)
                                        ;   field (if sinusoidal? (comp sinusoidal field) field)
        ]    
    ;; (when sinusoidal?
    ;; (print "sinusoidal "))
    (when derivative?
      (print "derivative "))
    (if one-field?
      (println field-name1)
      (println (str field-name1 " " field-name2)))
    field))

;; folds

(defn iterate-folds
  ""
  [^long n run? field]
  (let [bp (p/make-binpixels [-3.0 3.0 -3.0 3.0] width height)]
    (loop [iter (long 0)]
      (if (and @run? (< iter n))
        (let [^Vec2 xy (Vec2. (r/drand -3.0 3.0) (r/drand -3.0 3.0))
              ^Vec2 result (field xy)]
          (p/add-pixel-bilinear bp (.x result) (.y result) 20 20 20)
          (recur (inc iter)))
        bp))))


;; flows

(defn make-flow-particle
  "Create flow particle vector x,y - pos; z - angle; w - step direction"
  []
  (Vec3. (- (* 0.8 hwidth))
         (* height (+ (* 0.05 ^double (r/grand)) (/ (int (* 20.0 ^double (r/drand 0.2 0.8))) 20.0)))
         (r/drand m/TWO_PI)))

(defn make-move-particle-fn
  ""
  [^BinPixels bp {:keys [^double shift-x-step ^double shift-y-step ^double shift-x-scale ^double shift-y-scale
                         ^double point-step ^double angle-scale step-noise angle-noise field
                         ^Vec4 c1 ^Vec4 c2]}]
  (fn [^Vec3 in]
    (let [xs (* shift-x-scale (.x in))
          ys (* shift-y-scale (.y in))
          step-x (* shift-x-step ^double (step-noise xs ys) )
          step-y (* shift-y-step ^double (m/norm (step-noise ys xs) 0.0 1.0 (- shift-y-scale) shift-y-scale))
          nx (+ (.x in) step-x (* point-step (m/cos (.z in))))
          ny (+ (.y in) step-y (* point-step (m/sin (.z in))))
          xx (m/norm nx (- hwidth) width -3.0 3.0)
          yy (m/norm ny 0 height -3.0 3.0)
          ^Vec2 v (field (Vec2. xx yy))
          ^double anoise (angle-noise (.x v) (.y v))
          step-n (* angle-scale (* 2.0 (- anoise 0.5)))
          nz (+ (.z in) step-n)
          ^Vec4 c (v/interpolate c1 c2 anoise)]
      (p/add-pixel-bilinear bp nx ny (.x c) (.y c) (.z c))
      (if (or (< (- hwidth) nx hwidth)
              (< -1.0 ny height))
        (Vec3. nx ny nz)
        (make-flow-particle)))))

(defn iterate-flow
  ""
  [^long n run? particles config]
  (let [bp (p/make-binpixels [(- hwidth) hwidth 0 height] width height)
        mv (make-move-particle-fn bp config)]
    (loop [iter (long 0)
           p particles]
      (if (and run? (< iter n))
        (recur (inc iter)
               (doall (map mv p)))
        [bp p]))))

(defn draw-on-canvas
  "Render BinPixels to canvas."
  [canvas ^BinPixels bp]
  (p/set-canvas-pixels canvas (p/to-pixels bp
                                           (Vec4. 240 240 240 255)
                                           {:alpha-gamma 1.2 :color-gamma 2.6})))

;; Create canvas, windows, binpixels, configuration and iterate until window is closed
;; press `space` to save
;; close window to stop
(let [palseq (filter #(< 0 (c/get-luma (first %)) 20) (repeatedly #(:palette (g/color-reducer-machine))))
      config {:shift-x-step 0.1 :shift-y-step 2.0 :shift-x-scale 0.05 :shift-y-scale 0.01 :point-step 0.8 :angle-scale (* 10 m/TWO_PI)
              :step-noise (get-random-noise) :angle-noise (get-random-noise)
              :field (create-field)
              :c1 (ffirst palseq)
              :c2 (first (second palseq))}
      first-step 100
      steps-per-task 1000
      canvas (create-canvas width height)
      [_ run?] (show-window canvas "Folds2d" 800 800 5)
      particles (repeatedly 1000 make-flow-particle)]

  (defmethod key-pressed ["Folds2d" \space] [_]
    (save-canvas canvas (next-filename "generateme/folds2d/" ".png")))

  ;; first run
  (let [bp (iterate-flow first-step run? particles config)]

    (draw-on-canvas canvas (first bp))
    (loop [[prev p] bp]
      (if @run?
        (do
          (println "tick")
          (let [[newb currp] (iterate-flow steps-per-task run? p config)
                currb (p/merge-binpixels prev newb)]
            (draw-on-canvas canvas currb)
            (recur [currb currp])))
        (println :done)))))
