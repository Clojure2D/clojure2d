(ns generateme.fastnoise
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.math.random :as r]
            ;; [criterium.core :as crit]
            )
  (:import [clojure2d.java.noise NoiseConfig FBM]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def canvas (make-canvas 500 500))

(def window (show-window canvas "abc"))

(let [cfg (NoiseConfig. 1224 3 1 2.0 0.5 true)]
  (with-canvas [c canvas] 
    (dotimes [x 500]
      (dotimes [y 500]
        (let [n (* 255.0 (GradientNoise/fbm cfg 0 (/ x 50.0) (/ y 50.0)))]
          (set-color c n n n)
          (rect c x y 1 1))))))

(with-canvas [c canvas]
  (dotimes [x 500]
    (let [xx (/ x 500.0)
          b (- 500 (* 500.0 ^double (v/mag (v/vec2 xx (dec xx)))))]
      (rect c x b 1 1)
      )))

;; (ValueNoise/value cfg 1 2.13)

(defn with-noise
  "Get noise value with given Perlin object. Returned value is from [0,1] range."
  (^double [^Perlin n x] (* 0.5 (+ 1.0 (.getValue n x 0.5 0.5))))
  (^double [^Perlin n x y] (* 0.5 (+ 1.0 (.getValue n x y 0.5))))
  (^double [^Perlin n x y z] (* 0.5 (+ 1.0 (.getValue n x y z)))))

(defn make-perlin-noise
  "Create noise functions and bind with freshly created Perlin object.

  Parameters: 

  * seed (optional)
  * octaves (optional)

  Returns `noise` function."
  ([] (partial with-noise (Perlin.)))
  ([seed]
   (let [^Perlin n (Perlin.)]
     (.setSeed n seed)
     (partial with-noise n)))
  ([seed octaves]
   (let [^Perlin n (Perlin.)]
     (.setSeed n seed)
     (.setOctaveCount n octaves)
     (partial with-noise n))))

;; default noise function with random seed and default octave numbers
(def noise (make-perlin-noise 1337 6))

(def ^clojure2d.java.noise.NoiseConfig cfg (clojure2d.java.noise.NoiseConfig. 1337 1 3 6 2.0 0.5 true))

;; (def ^clojure2d.java.noise.ValueNoise nnn (clojure2d.java.noise.ValueNoise.))

(crit/quick-bench (dotimes [x (* 1000)] (FBM/noise cfg (/ x 123.123) (r/drand -10 10) (/ x 55)))) ;; 10ms

(crit/quick-bench (dotimes [x (* 1000)] (noise (/ x 123.123) (r/drand -10 10) (/ x 55))))


;; (aget (.perm cfg) 5)

;; (areduce (.valueLUT cfg) idx ret -111 (max ret (aget (.valueLUT cfg) idx)))


