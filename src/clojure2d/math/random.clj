;; # Namespace scope
;;
;; Namespaces defines:
;;
;; * random functions based on various RNG + default bindings based on default Java RNG
;; * int, long, float, double, gaussian and boolean random functions
;; * basic perlin noise (see `joise` namespace for several more noise functions)
;; * discrete noise function

(ns clojure2d.math.random
  (:require [clojure2d.math :refer :all]
            [clojure2d.math.vector :as v])
  (:import [clojure2d.math.vector Vec2]
           [org.apache.commons.math3.random RandomGenerator ISAACRandom JDKRandomGenerator MersenneTwister
            Well512a Well1024a Well19937a Well19937c Well44497a Well44497b
            RandomVectorGenerator HaltonSequenceGenerator SobolSequenceGenerator UnitSphereRandomVectorGenerator]
           [com.flowpowered.noise.module.source Perlin]))

;; ## Random function wrappers
;; 
;; RNG wrapper for various RNG source.
;; As a RNG source [Apache Commons Math](http://commons.apache.org/proper/commons-math/apidocs/org/apache/commons/math3/random/package-summary.html) project was chosen.
;;
;; You get following functions:
;;
;; * `make-randomizer` - create RNG object with given algorithm and optional seed (see RNG's list below) 
;; * `irandom`, `lrandom`, `frandom`, `drandom`, `grandom`, `brandom` - return random primitive value with given RNG (see more details below)
;; * `irand`, `lrand`, `frand`, `drand`, `grand`, `brand` - same as above but with default JDK Random object.
;;
;; The concept is as follows
;;
;; * create `Randomizer` protocol with more processing-like random functions for every primitive type: `int`, `long`, `float` and `double`. Additionally provide similar contract for Gaussian random, and boolean random with set probability (you set probability for getting `true`)
;; * enhance every given RNG class with `Randomizer` protocol. This is done by extension of RandomGenerator interface. Every RNG from Apache Commons Math implements it.
;; * create factory multimethod to generate RNG object for every class
;;
;; Currently supported RNGs:
;;
;; * `:jdk` - default java.util.Random
;; * `:mersenne` - MersenneTwister
;; * `:isaac` - ISAAC
;; * `:well512a`, `:well1024a`, `:well19937a`, `:well19937c`, `:well44497a`, `:well44497b` - several WELL variants

;; Type hinted functions generating random value
(defn next-random-value-long
  "Generate next long.

  * arity 0 - from 0 to maximum long value
  * arity 1 - from 0 to provided integer (excluded)
  * arity 2 - from the provided range (included, excluded)"
  (^long [^RandomGenerator r] (.nextLong r))
  (^long [^RandomGenerator r ^long mx] (mod (.nextLong r) mx))
  (^long [r ^long mn ^long mx]
   (let [diff (- mx mn)]
     (if (zero? diff) mn
         (+ mn (next-random-value-long r diff))))))

(defn next-random-value-double
  "Generate next double.

  * arity 0 - from 0 to 1 (exluded)
  * arity 1 - from 0 to provided double (excluded)
  * arity 2 - from the provided range (included, excluded)"
  (^double [^RandomGenerator r] (.nextDouble r))
  (^double [^RandomGenerator r ^double mx] (* (.nextDouble r) mx))
  (^double [r ^double mn ^double mx]
   (let [diff (- mx mn)]
     (if (zero? diff) mn
         (+ mn (next-random-value-double r diff))))))

(defn next-random-value-gaussian
  "Generate next random value from normal distribution.

  * arity 0 - N(0,1)
  * arity 1 - N(0,par)
  * arity 2 - N(par1,par2)"
  (^double [^RandomGenerator r] (.nextGaussian r))
  (^double [^RandomGenerator r ^double mx] (* (.nextGaussian r) mx))
  (^double [r ^double mn ^double mx]
   (let [diff (- mx mn)]
     (if (zero? diff) mn
         (+ mn (next-random-value-gaussian r diff))))))

;; Create protocol Randomizer with following functions:
;;
;; * `irandom` - return random integer from uniform distribution
;; * `drandom` - return random double from uniform distribution
;; * `lrandom` - return random long from uniform distribution
;; * `frandom` - return random float from uniform distribution
;; * `grandom` - return random double from gaussian distribution
;; * `brandom` - return random boolean (with or without given probability)
;;
;; Parameters are as follows:
;;
;; * `t` - RNG object
;; * `mx` - maximum random value, excluded (default: MAX_INT for integers and 1 for floats)
;; * `mn` - minimum random value, included (default: 0)
;; * `std` - standard deviation for Gaussian (default 1)
;; * `mean` - average for Gaussian (default 0)
;; * `thr` - expected probability to obtain `true`
;;
;; Call (RNG - RNG object):
;;
;; * `([il]random RNG)` - returns random number between 0 and maximum integer/long
;; * `([fd]random RNG)` - returns random number between 0 and 1 (float or double)
;; * `([ilfd]random RNG mx)` - returns random number between 0 and mx (excluded)
;; * `([ilfd]random RNG mn mx)` - returns random number betwee mn (included) and mx (excluded)
;; * `(grandom RNG)` - returns number from normal(0,1) distribution
;; * `(grandom RNG std)` - returns number from normal(0,std) distibution
;; * `(grandom RNG mean std)` - returns number from normal(mean,std) distibution
;; * `(brandom RNG)` - returns true or false with probability 50%
;; * `(brandom RNG thr)` - returns true with probability `thr` (false with probability `1-thr`)
(defprotocol Randomizer
  (irandom [t] [t mx] [t mn mx] "int random")
  (drandom [t] [t mx] [t mn mx] "double random")
  (lrandom [t] [t mx] [t mn mx] "long random")
  (frandom [t] [t mx] [t mn mx] "float random")
  (grandom [t] [t std] [t mean std] "gaussian random")
  (brandom [t] [t thr] "boolean random, with probability option"))

;; Extend RandomGenerator interface with functions created by macro `next-random-value-fn`. This way all RNG classes are enriched with new, more convenient functions.
;;
;; Note that `grandom` is under special care due to different [mn mx] range meaning.

(extend RandomGenerator
  Randomizer
  {:irandom (comp unchecked-int next-random-value-long)
   :lrandom next-random-value-long
   :frandom (comp float next-random-value-double)
   :drandom next-random-value-double
   :grandom (fn
              ([t] (next-random-value-gaussian t))
              ([t std] (next-random-value-gaussian t std))
              ([t ^double mean ^double std] (next-random-value-gaussian t mean (+ mean std))))
   :brandom (fn
              ([^RandomGenerator t] (.nextBoolean t))
              ([t ^double thr] (< (next-random-value-double t) thr)))})

;; Helper macro which creates RNG object of given class and/or seed.
(defmacro create-object-with-seed
  "Create object of the class with (or not) given seed"
  [cl seed]
  `(if-let [arg# ~seed]
     (new ~cl (int arg#))
     (new ~cl)))

;; Multimethod `make-randomizer` creates RNG object with new set of functions
;;
;;`(make-randomizer :well512a) => #object[org.apache.commons.math3.random.Well512a 0x1a8535ec "org.apache.commons.math3.random.Well512a@1a8535ec"]`
;;
;;`(make-randomizer :default 123) => #object[org.apache.commons.math3.random.JDKRandomGenerator 0x3f12abed "org.apache.commons.math3.random.JDKRandomGenerator@3f12abed"]`
;;
;; Couple of examples
;;
;;`(def my-rng (make-randomizer :isaac 5336)) => #'clojure2d.utils.random/my-rng`  
;;`(lrandom my-rng) => -2039938199907748741`  
;;`(lrandom my-rng 10 20) => 12`  
;;`(grandom my-rng) => -0.08506892961166786`  
;;`(grandom my-rng 100 100) => 75.13342709925789`  
;;`(brandom (make-randomizer :mersenne 123456) 0.05) => false`  
;;
;; Let's count all trues with probability 23.3% (5 tests)  
;;`(count (filter identity (repeatedly 100000 #(brandom my-rng 0.233))))`  
;; `=> 23344`  
;; `=> 23221`  
;; `=> 23537`  
;; `=> 23167`  
;; `=> 23328`  
(defmulti make-randomizer (fn [m & _] m))
(defmethod make-randomizer :mersenne [m & [seed]]
  (create-object-with-seed MersenneTwister seed))
(defmethod make-randomizer :isaac [m & [seed]]
  (create-object-with-seed ISAACRandom seed))
(defmethod make-randomizer :well512a [m & [seed]]
  (create-object-with-seed Well512a seed))
(defmethod make-randomizer :well1024a [m & [seed]]
  (create-object-with-seed Well1024a seed))
(defmethod make-randomizer :well19937a [m & [seed]]
  (create-object-with-seed Well19937a seed))
(defmethod make-randomizer :well19937c [m & [seed]]
  (create-object-with-seed Well19937c seed))
(defmethod make-randomizer :well44497a [m & [seed]]
  (create-object-with-seed Well44497a seed))
(defmethod make-randomizer :well44497b [m & [seed]]
  (create-object-with-seed Well44497b seed))
(defmethod make-randomizer :default [m & [seed]]
  (create-object-with-seed JDKRandomGenerator seed))

;; List of randomizers
(def randomizers [:mersenne :isaac :well512a :well1024a :well19937a :well19937c :well44497a :well44497b :jdk])

;; ### Default RNG
;;
;; Define easy to use functions with JDK randomizer
;;
;;`(drand 10) => 3.0752117891384048`  
;;`(drand 10 20) => 11.327971928466651`
(def default-random (make-randomizer :jdk))
(def frand (partial frandom default-random))
(def brand (partial brandom default-random))

(defn drand
  (^double [] (drandom default-random))
  (^double [mx] (drandom default-random mx))
  (^double [mn mx] (drandom default-random mn mx)))

(defn grand
  (^double [] (grandom default-random))
  (^double [mx] (grandom default-random mx))
  (^double [mn mx] (grandom default-random mn mx)))

(defn irand
  (^long [] (irandom default-random))
  (^long [mx] (irandom default-random mx))
  (^long [mn mx] (irandom default-random mn mx)))

(defn lrand
  (^long [] (lrandom default-random))
  (^long [mx] (lrandom default-random mx))
  (^long [mn mx] (lrandom default-random mn mx)))

;; ## Random Vector Sequences
;;
;; Couple of functions to generate sequences of numbers or vectors.
;; You can generate sequence of `double`, `Vec2`, `Vec3` or `Vec4` types. Just pass the size to creator function.
;;
;; To create generator call `make-sequence-generator` with generator name and vector size [1,4].
;; Following generators are available:
;;
;; * :halton - Halton low-discrepancy sequence; range [0,1]
;; * :sobol - Sobol low-discrepancy sequence; range [0,1]
;; * :sphere - uniformly random distributed on unit sphere
;; * :gaussian - gaussian distributed (mean=0, stddev=1)
;; * :default - uniformly random; range:[0,1]
;;
;; After creation you get function equivalent to `(repeatedly fn)`

(defn- commons-math-generators
  "Generators from commons math"
  [gen size]
  (let [s (iconstrain size 1 4)
        ^RandomVectorGenerator g (case gen
                                   :halton (HaltonSequenceGenerator. s)
                                   :sobol (SobolSequenceGenerator. s)
                                   :sphere (UnitSphereRandomVectorGenerator. s))
        gf (case s
             1 #(aget (.nextVector g) 0)
             2 #(v/array->vec2 (.nextVector g))
             3 #(v/array->vec3 (.nextVector g))
             4 #(v/array->vec4 (.nextVector g)))]
    #(repeatedly gf)))

(defn- random-generators
  "Random JDK generators"
  [gen size]
  (let [s (iconstrain size 1 4)
        g (case gen
            :default drand
            :gaussian grand)
        gf (case s
             1 drand
             2 (partial v/generate-vec2 g)
             3 (partial v/generate-vec3 g)
             4 (partial v/generate-vec4 g))]
    #(repeatedly gf)))

;; Sequence creators
(defmulti make-sequence-generator (fn [gen size] gen))
(defmethod make-sequence-generator :halton [gen size] (commons-math-generators gen size))
(defmethod make-sequence-generator :sobol [gen size] (commons-math-generators gen size))
(defmethod make-sequence-generator :sphere [gen size] (commons-math-generators gen size))
(defmethod make-sequence-generator :gaussian [gen size] (random-generators gen size))
(defmethod make-sequence-generator :default [gen size] (random-generators gen size))

;; ## Noise
;;
;; Basic perling noise function, see `clojure2d.math.joise` namespace for binding to composable noise system

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
(def noise (make-perlin-noise))

;; ### Discrete noise

(def ^:const ^double AM (/ 1.0 2147483647.0))

(defn discrete-noise
  "Discrete noise. Parameters:

  * X (long)
  * Y (long, optional)

  Returns double value from [0,1] range"
  (^double [^long X ^long Y]
   (let [X (unchecked-int X)
         Y (unchecked-int Y)
         n (unchecked-add-int X (unchecked-multiply-int Y 57))
         nn (unchecked-int (bit-xor n (bit-shift-left n 13)))
         nnn (unchecked-add-int 1376312589 (unchecked-multiply-int nn (unchecked-add-int 789221 (unchecked-multiply-int nn (unchecked-multiply-int nn 15731)))))]
     (* AM (unchecked-int (bit-and 0x7fffffff nnn)))))
  (^double [^long X]
   (discrete-noise X 0)))
