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
            [clojure2d.extra.glitch :as g]
            [clojure2d.color :as c])
  (:import [clojure2d.pixels BinPixels]
           [clojure2d.math.vector Vec4]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int width 1000)
(def ^:const ^int height 1000)
(def ^:const ^int hwidth (int (/ width 2)))

(defn unit-fn ^double [_ _] 1.0)

(defn make-random-config
  "Make random harmonograph configuration: f1-f4 frequencies, p1-p4 phases, a1-a4 amplitudes,
  nscalex/nscaley - noise scale, n1-n4 noise functions (or value 1.0), c1-c2 colors, dampsteps"
  []
  (let [^int rngs (r/irand 8 32)
        freqs (map #(/ (double %) m/TWO_PI) (range (- rngs) rngs (r/irand 1 6)))
        ^double a1 (r/drand hwidth)
        a2 (r/drand (- hwidth a1))
        ^double a3 (r/drand hwidth)
        a4 (- hwidth a3)
        n [(j/make-random-fractal) (j/make-random-fractal) r/noise r/noise r/noise r/noise r/noise r/noise]
        pal (first (filter #(> (c/get-luma (first %)) 205) (repeatedly #(:palette (g/color-reducer-machine)))))
        c1 (first pal)
        c2 (rand-nth pal)]
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
     :n1 (if (r/brand 0.75) (rand-nth n) unit-fn)
     :n2 (if (r/brand 0.75) (rand-nth n) unit-fn)
     :n3 (if (r/brand 0.75) (rand-nth n) unit-fn)
     :n4 (if (r/brand 0.75) (rand-nth n) unit-fn)
     :c1 c1
     :c2 c2
     :dampstepsx (if (r/brand 0.2) (vec (range 0.6 1.2 (/ 1.0 (double (r/irand 1 8))))) nil)
     :dampstepsy (if (r/brand 0.2) (vec (range 0.6 1.2 (/ 1.0 (double (r/irand 1 8))))) nil)}))

(defn iterate-harmonograph
  "Read configuration and do `n` iterations starting at time `start-time`, store everything in `BinPixels`."
  [n start-time ^BinPixels bp run?
   {:keys [^double f1 ^double f2 ^double f3 ^double f4
           ^double p1 ^double p2 ^double p3 ^double p4
           ^double a1 ^double a2 ^double a3 ^double a4
           ^double nscalex1 ^double nscaley1
           ^double nscalex2 ^double nscaley2
           n1 n2 n3 n4
           ^Vec4 c1 ^Vec4 c2
           dampstepsx dampstepsy]}]
  (let [^int dampxc (if dampstepsx (count dampstepsx) 0)
        ^int dampyc (if dampstepsy (count dampstepsy) 0)]
    (loop [prevx (double 0.0)
           prevy (double 0.0)
           time (double start-time)
           iter (long 0)]
      (if (and @run? (< iter ^long n))
        (let [s1 (m/sin (+ (* time f2) p2))
              s2 (m/sin (+ (* time f3) p3))
              ^Vec4 col (v/interpolate c1 c2 (m/abs (* s1 s2)))
              
              ^double dampx (if (zero? dampxc) 1.0
                                (dampstepsx (int (m/norm (m/qsin (+ time p1)) -1.0 1.1 0.0 dampxc))))
              ^double dampy (if (zero? dampyc) 1.0
                                (dampstepsy (int (m/norm (m/qsin (+ time p4)) -1.0 1.1 0.0 dampyc))))              
              
              x (* dampx (+ (* a1 (m/sin (+ (* time f1)
                                            (* m/TWO_PI ^double (n1 (* prevy nscaley2) p1))
                                            p1)))
                            (* a2 s1 ^double (n2 (+ (* prevx nscalex1)) (* prevy nscaley1)))))
              y (* dampy (+ (* a3 s2 ^double (n3 (* (+ time prevx) nscalex2) (+ (* prevy nscaley2))))
                            (* a4 (m/sin (+ (* time f4)
                                            (* m/TWO_PI ^double (n4 (* x nscaley2) p4))
                                            p4)))))]
          
          (p/add-pixel-bilinear bp x y (.x col) (.y col) (.z col))
          (recur x y (+ time 0.0051234) (inc iter)))
        time))))

;; Create canvas, windows, binpixels, configuration and iterate until window is closed
;; press `space` to save
(let [config (make-random-config)
      canvas (create-canvas width height)
      [_ run?] (show-window canvas "Harmonograph" width height 5)
      ^BinPixels bp (p/make-binpixels [-520 520 -520 520] width height)]

  (defmethod key-pressed ["Harmonograph" \space] [_]
    (save-canvas canvas (next-filename "results/ex32/" ".jpg")))

  (loop [time (iterate-harmonograph 100000 0.0 bp run? config)]
    (if @run?
      (do
        (println time)
        (p/set-canvas-pixels canvas (p/to-pixels bp (Vec4. 20 20 20 255) (/ 1.0 2.5)))
        (recur (iterate-harmonograph 1000000 time bp run? config)))
      (println :done))))

