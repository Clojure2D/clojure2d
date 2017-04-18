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
           [clojure2d.math.vector Vec2 Vec4]))

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

(def sinusoidal (vr/make-variation :sinusoidal 2.8 {}))

(defn create-field
  ""
  []
  (let [one-field? (r/brand 0.25)
        derivative? (r/brand 0.15)
        sinusoidal? (r/brand 0.75)
        field-name1 (rand-nth vr/variation-list)
        field-name2 (rand-nth vr/variation-list)
        field (if one-field?
                (vr/make-variation field-name1 1.0 {})
                (comp (vr/make-variation field-name2 1.0 {}) (vr/make-variation field-name1 1.0 {})))
        field (if derivative? (vr/derivative field) field)
        field (if sinusoidal? (comp sinusoidal field) field)]
    (when sinusoidal?
      (print "sinusoidal "))
    (when derivative?
      (print "derivative "))
    (if one-field?
      (println field-name1)
      (println (str field-name1 " " field-name2)))
    field))


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



(defn draw-on-canvas
  "Render BinPixels to canvas."
  [canvas ^BinPixels bp]
  (p/set-canvas-pixels canvas (p/to-pixels bp
                                           (Vec4. 250 250 250 255)
                                           {:alpha-gamma 0.8 :brightness 1.0})))

;; Create canvas, windows, binpixels, configuration and iterate until window is closed
;; press `space` to save
;; close window to stop
(let [config (create-field)
      canvas (create-canvas width height)
      [_ run?] (show-window canvas "Harmonograph" 800 800 5)]

  (defmethod key-pressed ["Harmonograph" \space] [_]
    (save-canvas canvas (next-filename "generateme/folds2d/" ".png")))

  ;; first run
  (let [bp (iterate-folds first-step run? config)]

    (draw-on-canvas canvas bp)
    (loop [prev bp
           steps (long first-step)]
      (if @run?
        (do
          (println steps)
          (let [newb (reduce #(p/merge-binpixels %1 (deref %2)) prev
                             (doall (map (fn [_] (future (iterate-folds steps-per-task run? config)))
                                         (range available-tasks))))]
            (draw-on-canvas canvas newb)
            (recur newb
                   (+ steps (* available-tasks steps-per-task)))))
        (println :done)))))
