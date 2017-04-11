;; Harmonograph on noise
;;
;; Version rendered using BinPixels object (accumulative bins)

(ns examples.ex32-harmonograph
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.joise :as j]
            [clojure2d.math.vector :as v]
            [clojure2d.extra.glitch :as g])
  (:import [clojure2d.pixels BinPixels]
           [clojure2d.math.vector Vec4]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int width 1000)
(def ^:const ^int height 1000)
(def ^:const ^int hwidth (int (/ width 2)))

(defn make-random-config
  "Make random harmonograph configuration: f1-f4 frequencies, p1-p4 phases, a1-a4 amplitudes,
  nscalex/nscaley - noise scale, n1-n4 noise functions, pal palette"
  []
  (let [freqs (map #(/ (double %) m/TWO_PI) (range -30 31 (r/irand 1 6)))
        ^double a1 (r/drand hwidth)
        a2 (r/drand (- hwidth a1))
        ^double a3 (r/drand hwidth)
        a4 (- hwidth a3)
        ;; n [(j/make-random-fractal) (j/make-random-fractal) r/noise r/noise r/noise r/noise r/noise r/noise]
        n [r/noise j/perlin-noise]
        pal (:palette (g/color-reducer-machine))
        c1 (rand-nth pal)
        c2 (rand-nth pal)
        br (c/get-luma (v/interpolate c1 c2 0.5))]
    {:f1 (rand-nth freqs)
     :f2 (rand-nth freqs)
     :f3 (rand-nth freqs)
     :f4 (rand-nth freqs)
     :p1 (r/grand)
     :p2 (r/grand)
     :p3 (r/grand)
     :p4 (r/grand)
     :a1 a1
     :a2 a2
     :a3 a3
     :a4 a4
     :nscalex1 (/ 1.0 ^double (r/drand (/ width 15.0) width))
     :nscaley1 (/ 1.0 ^double (r/drand (/ height 15.0) height))
     :nscalex2 (/ 1.0 ^double (r/drand (/ width 15.0) width))
     :nscaley2 (/ 1.0 ^double (r/drand (/ height 15.0) height))
     :n1 (rand-nth n)
     :n2 (rand-nth n)
     :n3 (rand-nth n)
     :n4 (rand-nth n) 
     :c1 c1
     :c2 c2
     :bright (m/constrain (- 1.0 (/ br 240.0)) 0.0 1.0)}))

(def ^Vec4 white (Vec4. 250 250 250 255))

(defn iterate-harmonograph
  "Read configuration and do `n` iterations starting at time `start-time`, store everything in `BinPixels`."
  ^double [^long n ^double start-time ^BinPixels bp
           {:keys [^double f1 ^double f2 ^double f3 ^double f4
                   ^double p1 ^double p2 ^double p3 ^double p4
                   ^double a1 ^double a2 ^double a3 ^double a4
                   ^double nscalex1 ^double nscaley1
                   ^double nscalex2 ^double nscaley2
                   n1 n2 n3 n4
                   ^Vec4 c1 ^Vec4 c2
                   ^double bright]}]
  (loop [prevx (double 0.0)
         prevy (double 0.0)
         time (double start-time)
         iter (long 0)]
    (if (< iter n)
      (let [s1 (m/sin (+ (* time f2) p2))
            s2 (m/sin (+ (* time f3) p3))
            
            ^Vec4 col (-> c1
                          (v/interpolate c2 (m/abs (* s1 s2)))
                          (v/interpolate white bright))
            
            x (+ (* a1 (m/sin (+ (* time f1)
                                 (* m/TWO_PI ^double (n1 (* prevy nscaley2) p1))
                                 p1)))
                 (* a2 s1
                    ^double (n2 (* prevx nscalex1) (* prevy nscaley1))))
            y (+ (* a3 s2
                    ^double (n3 (* prevx nscalex2) (* prevy nscaley2)))
                 (* a4 (m/sin (+ (* time f4)
                                 (* m/TWO_PI ^double (n4 (* x nscalex1) p4))
                                 p4))))]
        
        (p/add-pixel bp x y (.x col) (.y col) (.z col))
        (recur x y (+ time 0.0051) (inc iter)))
      time)))

;; Create canvas, windows, binpixels, configuration and iterate until window is closed
;; press `space` to save
(let [config (make-random-config)
      canvas (create-canvas width height)
      window (show-window canvas "Harmonograph" width height 1)
      ^BinPixels bp (p/make-binpixels [-520 520 -520 520] width height)]

  (defmethod key-pressed ["Harmonograph" \space] [_]
    (save-canvas canvas (next-filename "results/ex32/" ".jpg")))
  
  (loop [time (iterate-harmonograph 10000 0.0 bp config)]
    (if @(second window)
      (do
        (println time)
        (p/set-canvas-pixels canvas (p/to-pixels bp (Vec4. 20 20 20 255)))
        (recur (iterate-harmonograph 1000000 time bp config)))
      (println :done))))


