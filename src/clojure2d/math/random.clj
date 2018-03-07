;; # Namespace scope
;;
;; Namespaces defines:
;;
;; * random functions based on various RNG + default bindings based on default Java RNG
;; * int, long, float, double, gaussian and boolean random functions
;; * basic perlin noise (see `joise` namespace for several more noise functions)
;; * discrete noise function

(ns clojure2d.math.random
  "Various random and noise functions.

  Namespace defines various random number generators (RNGs), different types of random functions, sequence generators and noise functions.

  #### RNGs

  You can use a selection of various RNGs defined in [Apache Commons Math](http://commons.apache.org/proper/commons-math/apidocs/org/apache/commons/math3/random/package-summary.html) library.

  Currently supported RNGs:

  * `:jdk` - default java.util.Random
  * `:mersenne` - MersenneTwister
  * `:isaac` - ISAAC
  * `:well512a`, `:well1024a`, `:well19937a`, `:well19937c`, `:well44497a`, `:well44497b` - several WELL variants

  To create your RNG use [[make-rng]] multimethod. Pass RNG name and (optional) seed. Returned RNG is equipped with [[RNGProto]] protocol with methods: [[irandom]], [[lrandom]], [[frandom]] [[drandom]], [[grandom]], [[brandom]] which return random primitive value with given RNG.

  ```
  (let [rng (make-rng :isaac 1337)]
    (irandom rng))
  ```

  For conveniency default RNG (`:jdk`) with following functions are created: [[irand]], [[lrand]], [[frand]], [[drand]], [[grand]], [[brand]].

  Each prefix denotes returned type:

  * i - int
  * l - long
  * f - float
  * d - double
  * g - gaussian (double)
  * b - boolean

  Check individual function for parameters description.

  #### Random Vector Sequences

  Couple of functions to generate sequences of numbers or vectors.
  You can generate sequence of `double`, [[Vec2]], [[Vec3]] or [[Vec4]] types. Just pass the size to creator function.

  To create generator call [[make-sequence-generator]] with generator name and vector size [1,4].
  Following generators are available:

  * `:halton` - Halton low-discrepancy sequence; range [0,1]
  * `:sobol` - Sobol low-discrepancy sequence; range [0,1]
  * `:sphere` - uniformly random distributed on unit sphere
  * `:gaussian` - gaussian distributed (mean=0, stddev=1)
  * `:default` - uniformly random; range:[0,1]

  After creation you get function equivalent to `repeatedly`.

  #### Noise

  List of continuous noise functions (1d, 2d and 3d):

  * `:value` - value noise
  * `:gradient` - gradient noise (improved Ken Perlin version)
  * `:simplex` - simplex noise

  First two (`:value` and `:gradient`) can use 4 different interpolation types: `:none`, `:linear`, `:hermite` (cubic) and `:quintic`.
  
  All can be used as into:

  * Noise - pure noise value, create with [[make-single-noise]]
  * FBM - fractal brownian motion, create with [[make-fbm-noise]]
  * Billow - billow noise, [[make-billow-noise]]
  * RidgedMulti - ridged multi, [[make-ridgedmulti-noise]]

  Noise creation requires detailed configuration which is simple map of following keys:

  * `:seed` - seed as integer
  * `:noise-type` - type of noise: `:value`, `:gradient` (default), `:simplex`
  * `:interpolation` - type of interpolation (for value and gradient): `:none`, `:linear`, `:hermite` (default) or `:quintic`
  * `:octaves` - number of octaves for combined noise (like FBM), default: 6
  * `:lacunarity` - scaling factor for combined noise, default: 2.00
  * `:gain` - amplitude scaling factor for combined noise, default: 0.5
  * `:normalize?` - should be normalized to `[0,1]` range (true, default) or to `[-1,1]` range (false)

  For usage convenience 3 ready to use functions are prepared. Return is normalized to `[0,1]` range:

  * [[noise]] - Perlin Noise (gradient noise, 6 octaves, quintic interpolation)
  * [[vnoise]] - Value Noise (as in Processing, 6 octaves, hermite interpolation)
  * [[simplex]] - Simpled Noise (6 octaves)

  ##### Discrete Noise

  [[discrete-noise]] is a 1d or 2d hash function for given integers. Returns double from `[0,1]` range.
  "
  {:categories {:rand "Random number generation"
                :noise "Noise functions"
                :gen "Random sequence generation"}}
  (:require [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [metadoc.examples :refer :all])
  (:import [org.apache.commons.math3.random RandomGenerator ISAACRandom JDKRandomGenerator MersenneTwister
            Well512a Well1024a Well19937a Well19937c Well44497a Well44497b
            RandomVectorGenerator HaltonSequenceGenerator SobolSequenceGenerator UnitSphereRandomVectorGenerator]
           [clojure2d.java.noise Billow RidgedMulti FBM NoiseConfig Noise]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; Type hinted functions generating random value
(defn- next-random-value-long
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

(defn- next-random-value-double
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

(defn- next-random-value-gaussian
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

(defprotocol RNGProto
  "Defines set of random functions for different RNGs returning primitive values."
  (^{:categories #{:rand}}
   irandom [t] [t mx] [t mn mx]
   "Random integer from uniform distribution.
As default returns random integer from full integer range. 
When `mx` is passed, range is set to `[0, mx)`. When `mn` is passed, range is set to `[mn, mx)`.

See [[irand]].")
  (^{:categories #{:rand}}
   drandom [t] [t mx] [t mn mx]
   "Random double from uniform distribution.
As default returns random double from `[0,1)` range.
When `mx` is passed, range is set to `[0, mx)`. When `mn` is passed, range is set to `[mn, mx)`.

See [[drand]].")
  (^{:categories #{:rand}} lrandom [t] [t mx] [t mn mx]
   "Random long from uniform distribution.
As default returns random long from full long range. 
When `mx` is passed, range is set to `[0, mx)`. When `mn` is passed, range is set to `[mn, mx)`.

See [[lrand]].")
  (^{:categories #{:rand}} frandom [t] [t mx] [t mn mx]
   "Random float from uniform distribution.
As default returns random float from `[0,1)` range.
When `mx` is passed, range is set to `[0, mx)`. When `mn` is passed, range is set to `[mn, mx)`.

See [[frand]].")
  (^{:categories #{:rand}} grandom [t] [t std] [t mean std]
   "Random double from gaussian distribution.
As default returns random double from `N(0,1)`. 
When `std` is passed, `N(0,std)` is used. When `mean` is passed, distribution is set to `N(mean, std)`.

See [[grand]].")
  (^{:categories #{:rand}} brandom [t] [t thr]
   "Boolean random.
Returns true or false with equal probability. You can set probability for `true` setting `thr` (from `[0-1]` range).

See [[brand]].")
  (^{:categories #{:rand}} set-seed! [t v] "Sets seed. Returns RNG itself."))

;; Extend RandomGenerator interface with functions created by macro `next-random-value-fn`. This way all RNG classes are enriched with new, more convenient functions.
;;
;; Note that `grandom` is under special care due to different [mn mx] range meaning.

(extend RandomGenerator 
  RNGProto
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
              ([t ^double thr] (< (next-random-value-double t) thr)))
   :set-seed! #(do
                 (.setSeed ^RandomGenerator %1 (long %2))
                 %1)})

;; Helper macro which creates RNG object of given class and/or seed.
(defmacro ^:private create-object-with-seed
  "Create object of the class with (or not) given seed. Used to create RNG."
  [cl seed]
  `(if-let [arg# ~seed]
     (new ~cl (int arg#))
     (new ~cl)))

;; List of randomizers
(def ^{:categories #{:rand}
       :doc "List of all possible RNGs."
       :examples [(example "Contains" rng-list)]}
  rng-list [:mersenne :isaac :well512a :well1024a :well19937a :well19937c :well44497a :well44497b :jdk])

(defmulti make-rng
  "Create RNG for given name (as keyword) and optional seed. Return object enhanced with [[RNGProto]]. See: [[rng-list]] for names."
  {:categories #{:rand}}
  (fn [m & _] m))

(defmethod make-rng :mersenne [m & [seed]]
  (create-object-with-seed MersenneTwister seed))
(defmethod make-rng :isaac [m & [seed]]
  (create-object-with-seed ISAACRandom seed))
(defmethod make-rng :well512a [m & [seed]]
  (create-object-with-seed Well512a seed))
(defmethod make-rng :well1024a [m & [seed]]
  (create-object-with-seed Well1024a seed))
(defmethod make-rng :well19937a [m & [seed]]
  (create-object-with-seed Well19937a seed))
(defmethod make-rng :well19937c [m & [seed]]
  (create-object-with-seed Well19937c seed))
(defmethod make-rng :well44497a [m & [seed]]
  (create-object-with-seed Well44497a seed))
(defmethod make-rng :well44497b [m & [seed]]
  (create-object-with-seed Well44497b seed))
(defmethod make-rng :default [m & [seed]]
  (create-object-with-seed JDKRandomGenerator seed))

(add-examples make-rng
  (example-session "Creating" (make-rng :mersenne) (make-rng :isaac 1234))
  (example "Using" (irandom (make-rng :mersenne 999) 15 25)))

(defsnippet rngproto-snippet
  "Show [[RNGProto]] methods."
  (let [rng (make-rng :well44497b)]
    (f rng)))

(add-examples irandom (example-snippet "integer" rngproto-snippet irandom))
(add-examples lrandom (example-snippet "long" rngproto-snippet lrandom))
(add-examples drandom (example-snippet "double" rngproto-snippet drandom))
(add-examples frandom (example-snippet "float" rngproto-snippet frandom))
(add-examples grandom (example-snippet "gaussian double" rngproto-snippet grandom))
(add-examples brandom (example-snippet "boolean" rngproto-snippet brandom))

(add-examples set-seed!
  (example "Set seed for the RNG object" {:test-value 10} (let [rng (make-rng :isaac)]
                                                            (set-seed! rng 1234)
                                                            (irandom rng 10 15))))

;; ### Default RNG

(def ^{:doc "JDK default RNG"
       :categories #{:rand}
       :examples [(example-session "Usage"
                    (set-seed! default-rng 111)
                    (irandom default-rng)
                    (set-seed! default-rng 999)
                    (irandom default-rng)
                    (set-seed! default-rng 111)
                    (irandom default-rng))]}
  default-rng (make-rng :jdk))

(def ^{:doc "Random float number with JDK RNG."
       :categories #{:rand}
       :examples [(example-session "Usage" (frand) (frand 10) (frand 10 20))]}
  frand (partial frandom default-rng))

(def ^{:doc "Random boolean with JDK RNG."
       :categories #{:rand}
       :examples [(example-session "Usage" (brand) (brand 0.1))
                  (example "Count number of `true` values with probability 0.15" (count (filter true? (repeatedly 100000 #(brand 0.15)))))]} 
  brand (partial brandom default-rng))

(defn drand
  "Random double number with JDK RNG."
  {:categories #{:rand}
   :examples [(example-session "Usage" (drand) (drand 10) (drand 10 20))]}
  (^double [] (drandom default-rng))
  (^double [mx] (drandom default-rng mx))
  (^double [mn mx] (drandom default-rng mn mx)))

(defn grand
  "Random gaussian double number with JDK RNG."
  {:categories #{:rand}
   :examples [(example-session "Usage" (grand) (grand 10) (grand 10 20))]}
  (^double [] (grandom default-rng))
  (^double [stddev] (grandom default-rng stddev))
  (^double [mean stddev] (grandom default-rng mean stddev)))

(defn irand
  "Random integer number with JDK RNG."
  {:categories #{:rand}
   :examples [(example-session "Usage" (irand) (irand 10) (irand 10 20))]}
  (^long [] (irandom default-rng))
  (^long [mx] (irandom default-rng mx))
  (^long [mn mx] (irandom default-rng mn mx)))

(defn lrand
  "Random long number with JDK RNG."
  {:categories #{:rand}
   :examples [(example-session "Usage" (lrand) (lrand 10) (lrand 10 20))]}
  (^long [] (lrandom default-rng))
  (^long [mx] (lrandom default-rng mx))
  (^long [mn mx] (lrandom default-rng mn mx)))

(defmacro randval
  "Retrun value with given probability (default 0.5)"
  {:categories #{:rand}}
  ([v1 v2]
   `(if (brandom default-rng) ~v1 ~v2))
  ([prob v1 v2]
   `(if (brandom default-rng ~prob) ~v1 ~v2)))

(add-examples randval
  (example-session "Usage" (randval :val-one :val-two) (randval 0.001 :low-probability :high-probability))
  (example "Check probability of nil (should return value around 1000)." (count (filter nil? (repeatedly 1000000 #(randval 0.001 nil 101))))))

(defn- commons-math-generators
  "Generators from commons math"
  [gen ^long size]
  (let [s (m/constrain size 1 4)
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
  [gen ^long size]
  (let [s (m/constrain size 1 4)
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

(def ^{:doc "List of random sequence generator. See [[make-sequence-generator]]."
       :examples [(example "Generator names." gen-list)]
       :categories #{:gen}}
  gen-list [:halton :sobol :sphere :gaussian :uniform])


(defmulti
  ^{:doc "Create Sequence generator. See [[gen-list]] for names."
    :categories #{:gen}}
  make-sequence-generator (fn [gen size] gen))
(defmethod make-sequence-generator :halton [gen size] (commons-math-generators gen size))
(defmethod make-sequence-generator :sobol [gen size] (commons-math-generators gen size))
(defmethod make-sequence-generator :sphere [gen size] (commons-math-generators gen size))
(defmethod make-sequence-generator :gaussian [gen size] (random-generators gen size))
(defmethod make-sequence-generator :default [gen size] (random-generators gen size))

(add-examples make-sequence-generator
  (example "Usage (2d)" (let [gen (make-sequence-generator :halton 2)]
                          (take 5 (gen))))
  (example "Usage (1d)" (let [gen (make-sequence-generator :sobol 1)]
                          (take 5 (gen)))))

;; ## Noise

(def ^{:doc "List of possible noise interpolations as a map of names and values."
       :categories #{:noise}
       :examples [(example "List of names (keys)" (keys interpolations))]}
  interpolations {:none NoiseConfig/INTERPOLATE_NONE
                  :linear NoiseConfig/INTERPOLATE_LINEAR
                  :hermite NoiseConfig/INTERPOLATE_HERMITE
                  :quintic NoiseConfig/INTERPOLATE_QUINTIC})

(def ^{:doc "List of possible noise types as a map of names and values."
       :categories #{:noise}
       :examples [(example "List of names (keys)" (keys noise-types))]}
  noise-types {:value NoiseConfig/NOISE_VALUE
               :gradient NoiseConfig/NOISE_GRADIENT
               :simplex NoiseConfig/NOISE_SIMPLEX})

(defn- make-noise-config
  "Create noise configuration object based on map."
  [{:keys [seed noise-type interpolation octaves lacunarity gain normalize?]}]
  (NoiseConfig. seed
                (or (noise-types noise-type) NoiseConfig/NOISE_GRADIENT)
                (or (interpolations interpolation) NoiseConfig/INTERPOLATE_HERMITE)
                octaves lacunarity gain normalize?))

(defn- noise-config
  "Create FBM noise function for given configuration."
  ([] (noise-config {}))
  ([cfg]
   (make-noise-config (merge {:seed (irand)
                              :noise-type :gradient
                              :interpolation :hermite
                              :octaves 6
                              :lacunarity 2.00
                              :gain 0.5
                              :normalize? true} cfg))))

(def ^:private perlin-noise-config (noise-config {:interpolation :quintic}))
(def ^:private simplex-noise-config (noise-config {:noise-type :simplex}))
(def ^:private value-noise-config (noise-config {:noise-type :value}))

(defn vnoise
  "Value Noise.

  6 octaves, Hermite interpolation (cubic, h01)."
  {:categories #{:noise}
   :examples [(example-session "Usage"
                (vnoise 3.3)
                (vnoise 3.3 1.1)
                (vnoise 3.3 0.0 -0.1))]}
  (^double [^double x] (FBM/noise value-noise-config x))
  (^double [^double x ^double y] (FBM/noise value-noise-config x y))
  (^double [^double x ^double y ^double z] (FBM/noise value-noise-config x y z)))

(defn noise
  "Create improved Perlin Noise.

  6 octaves, quintic interpolation."
  {:categories #{:noise}
   :examples [(example-session "Usage"
                (noise 3.3)
                (noise 3.3 1.1)
                (noise 3.3 0.0 -0.1))]}
  (^double [^double x] (FBM/noise perlin-noise-config x))
  (^double [^double x ^double y] (FBM/noise perlin-noise-config x y))
  (^double [^double x ^double y ^double z] (FBM/noise perlin-noise-config x y z)))

(defn simplex
  "Create Simplex noise. 6 octaves."
  {:categories #{:noise}
   :examples [(example-session "Usage"
                (simplex 3.3)
                (simplex 3.3 1.1)
                (simplex 3.3 0.0 -0.1))]}
  (^double [^double x] (FBM/noise simplex-noise-config x))
  (^double [^double x ^double y] (FBM/noise simplex-noise-config x y))
  (^double [^double x ^double y ^double z] (FBM/noise simplex-noise-config x y z)))

(defmacro ^:private gen-noise-function
  "Generate various noise for static function"
  [method]
  `(fn [cfg#]
     (let [ncfg# (noise-config cfg#)]
       (fn
         ([x#] (~method ncfg# x#))
         ([x# y#] (~method ncfg# x# y#))
         ([x# y# z#] (~method ncfg# x# y# z#))))))

(def ^{:categories #{:noise}} make-single-noise (gen-noise-function Noise/noise))
(def ^{:categories #{:noise}} make-fbm-noise (gen-noise-function FBM/noise))
(def ^{:categories #{:noise}} make-billow-noise (gen-noise-function Billow/noise))
(def ^{:categories #{:noise}} make-ridgedmulti-noise (gen-noise-function RidgedMulti/noise))

(add-examples make-single-noise
  (example "Usage"
    (let [n (make-single-noise {:interpolation :linear})]
      (n 0.5 1.1 -1.3))))

(add-examples make-fbm-noise
  (example "Usage"
    (let [n (make-fbm-noise {:interpolation :linear
                             :noise-type :value})]
      (n 0.5 1.1 -1.3))))

(add-examples make-billow-noise
  (example "Usage"
    (let [n (make-billow-noise {:seed (irand)
                                :interpolation :none})]
      (n 0.5 1.1 -1.3))))

(add-examples make-ridgedmulti-noise
  (example "Usage"
    (let [n (make-ridgedmulti-noise {:octaves 3
                                     :lacunarity 2.1
                                     :gain 0.7
                                     :noise-type :simplex})]
      (n 0.5 1.1 -1.3))))


;; ### Discrete noise

(def ^:private ^:const ^double AM (/ 2147483647.0))

(defn discrete-noise
  "Discrete noise. Parameters:

  * X (long)
  * Y (long, optional)

  Returns double value from [0,1] range"
  {:categories #{:noise}
   :examples [(example-session "Example calls"
                (discrete-noise 123 444)
                (discrete-noise 123 444)
                (discrete-noise 123 445)
                (discrete-noise 123))]}
  (^double [^long X ^long Y]
   (let [X (unchecked-int X)
         Y (unchecked-int Y)
         n (unchecked-add-int X (unchecked-multiply-int Y 57))
         nn (unchecked-int (bit-xor n (<< n 13)))
         nnn (unchecked-add-int 1376312589 (unchecked-multiply-int nn (unchecked-add-int 789221 (unchecked-multiply-int nn (unchecked-multiply-int nn 15731)))))]
     (* AM (unchecked-int (bit-and 0x7fffffff nnn)))))
  (^double [^long X]
   (discrete-noise X 0)))

;;
