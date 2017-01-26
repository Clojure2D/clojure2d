;; ## Math functions
;;
;; Here you can find FastMath function wrappers plus some additional functions and constants.
;; Wrappers are made to provide Clojure functions.
;;
;; ### List of constants
;;
;; * Processing names: `PI`, `HALF_PI`, `QUARTER_PI`, `TWO_PI`, `TAU`, `E`
;; * Small quantity: `EPSILON`
;; * Logarithms: `LN2`, `LN10`, `LOG2E`, `LOG10E`, `INV_LOG_HALF`
;; * Square roots: `SQRT2`, `SQRT3`, `SQRT5`, `SQRTPI`
;; * Golden ration: `PHI`
;; * C (math.h) constants: `M_E`, `M_LOG2E`, `M_LOG10E`, `M_LN2`, `M_LN10`, `M_PI`, `M_PI_2`, `M_PI_4`, `M_1_PI`, `M_2_PI`, `M_2_SQRTPI`, `M_SQRT2`, `M_SQRT1_2`, `M_TWOPI`, `M_3PI_4`, `M_SQRTPI`, `M_LN2LO`, `M_LN2HI`, `M_SQRT3`, `M_IVLN10`, `M_LOG2_E`, `M_INVLN2`
;;
;; ### Trigonometry
;;
;; * `sin`, `cos`, `tan`, `cot`, `set`, `csc`
;; * `asin`, `acos`, `atan`, `acot`, `aset`, `acsc`, `atan2`
;; * `sinh`, `cosh`, `tanh`, `coth`, `seth`, `csch`
;; * `asinh`, `acosh`, `atanh`, `acoth`, `aseth`, `acsch`
;; * quick/fast versions: `qsin`, `qcos` 
;;
;; ### Powers / Logarithms / Roots
;;
;; * `log`/`ln`, `log2`, `log10`, `logb`
;; * `exp`, `pow`, `sq`/`pow2`, `pow3`
;; * `sqrt`, `cbrt`
;; * `hypot`
;; * `low-2-exp` `high-2-exp`
;; * quick/fast/safe versions: `qexp`, `qlog`, `qpow`/`fpow`, `safe-sqrt`, `qsqrt`
;;
;; ### Rounding
;;
;; * `floor`, `ceil`, `round`, `rint`
;; * `reminder`
;; * `abs`, `iabs`
;;
;; ### Other
;;
;; * `signum`
;; * `constrain`
;; * `norm`, `cnorm`
;; * `lerp`, `cos-interpolation`
;; * `wrap`

(ns clojure2d.math
  "FastMath wrappers + helper functions"
  (:require [clojure2d.math :as m])
  (:import [net.jafama FastMath]
           [org.apache.commons.math3.random RandomGenerator ISAACRandom JDKRandomGenerator MersenneTwister
            Well512a Well1024a Well19937a Well19937c Well44497a Well44497b]
           [com.flowpowered.noise.module.source Perlin]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; Bunch of math constants
(def ^:const ^double PI Math/PI)
(def ^:const ^double HALF_PI (/ PI 2.0))
(def ^:const ^double QUARTER_PI (/ PI 4.0))
(def ^:const ^double TWO_PI (* PI 2.0))
(def ^:const ^double TAU TWO_PI)
(def ^:const ^double E Math/E)

;; Very small number \\(\varepsilon\\)
(def ^:const ^double EPSILON 1.0e-10)

;; For single argument functions let define clojure functions through macro.
;; Macro generates sequence of `def` with respective function name.
(defmacro bind-math-names
  "Bind namespace name to fastmath function"
  [names]
  (cons 'do (for [n names]
              (let [s (gensym n)]
                `(def ~n (fn [~s] (. FastMath ~n ~s)))))))

;; Define functions
(bind-math-names [sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh exp log log10 sqrt cbrt])

;; Additional trigonometry functions
(def cot #(FastMath/tan (- HALF_PI ^double %)))
(def sec #(/ 1.0 (FastMath/cos %)))
(def csc #(/ 1.0 (FastMath/sin %)))

;; Quick and less accurate `sin` and `cos`
(def qsin #(FastMath/sinQuick %))
(def qcos #(FastMath/cosQuick %))

;; Additional cyclometric functions
(def acot #(- HALF_PI (FastMath/atan %)))
(def asec #(FastMath/acos (/ 1.0 ^double %)))
(def acsc #(FastMath/asin (/ 1.0 ^double %)))
(def atan2 #(FastMath/atan2 %1 %2))

;; Additional hyperbolic functions
(def coth #(/ 1.0 (FastMath/tanh %)))
(def sech #(/ 1.0 (FastMath/cosh %)))
(def csch #(/ 1.0 (FastMath/sinh %)))

;; Additional inverse hyperbolic functions
(def acoth #(FastMath/atanh (/ 1.0 ^double %)))
(def asech #(FastMath/acosh (/ 1.0 ^double %)))
(def acsch #(FastMath/asinh (/ 1.0 ^double %)))

;; Quick version of exponential \\(e^x\\)
(def qexp #(FastMath/expQuick %))

;; Alias for logarithm
(def ln log)

;; Few logarithm constants
;; \\(\ln 2\\)
(def ^:const ^double LN2 (log 2.0))
(def ^:const ^double LN2_2 (* 0.5 LN2))

;; \\(\ln 10\\)
(def ^:const ^double LN10 (log 10.0))

;; \\(\frac{1.0}{\ln{0.5}}\\)
(def ^:const ^double INV_LOG_HALF (/ 1.0 ^double (log 0.5)))

(defn log2
  "Log with base 2"
  [^double x]
  (/ (FastMath/log x) LN2))

;; \\(\log_b x\\)
(defn logb
  "Logarithm with base"
  [^double base ^double x]
  (/ (FastMath/log x) (FastMath/log base)))

(def qlog #(FastMath/logQuick %))

;; \\(\log_2 e\\)
(def ^:const ^double LOG2E (log2 E))

;; \\(\log_{10} e\\)
(def ^:const ^double LOG10E (log10 E))

;; Powers (normal, quick and fast)
(def pow #(FastMath/pow %1 %2))
(def qpow #(FastMath/powQuick %1 %2))

;; Fast version of power, second parameter should be integer
(def fpow #(FastMath/powFast %1 %2))

;; Square and cubic
(def sq #(FastMath/pow2 ^double %))
(def pow2 sq)
(def pow3 #(FastMath/pow3 ^double %))

(defn safe-sqrt
  "Safe sqrt, for value <= 0 result is 0"
  [^double value]
  (if (neg? value) 0 (sqrt value)))
(def qsqrt #(FastMath/sqrtQuick %))

;; \\(\sqrt{x^2+y^2}\\) and \\(\sqrt{x^2+y^2+z^2}\\)
(defn hypot
  "Hyponetuse"
  ([x y]
   (FastMath/hypot x y))
  ([x y z]
   (FastMath/hypot x y z)))

;; Rounding functions
(def floor ^double #(FastMath/floor ^double %))
(def ceil #(FastMath/ceil ^double %))
(def round #(FastMath/round ^double %))
(def rint #(FastMath/rint ^double %))

;; Find power of 2 exponent for double number where  
;; \\(2^(n-1)\leq x\leq 2^n\\)  
;; where n-1 is result of `low-2-exp` and n is result of `high-2-exp`
;; `(low-2-exp TWO_PI) => 2` \\(2^2\eq 4\leq 6.28\\)  
;; `(high-2-exp TWO_PI) => 3` \\(6.28\leq 2^3\eq 8\\)
(def low-2-exp (comp long floor log2))
(def high-2-exp (comp long ceil log2))

;; Modulo and abs
(def reminder #(FastMath/remainder %1 %2))
(def abs #(Math/abs ^double %))
(def iabs #(Math/abs ^long %))

;; More constants

;; \\(\sqrt{2}\\)
(def ^:const ^double SQRT2 (sqrt 2.0))
(def ^:const ^double SQRT2_2 (* 0.5 SQRT2))

;; \\(\sqrt{3}\\)
(def ^:const ^double SQRT3 (sqrt 3.0))

;; \\(\sqrt{5}\\)
(def ^:const ^double SQRT5 (sqrt 5.0))

;; \\(\sqrt{\pi}\\)
(def ^:const ^double SQRTPI (sqrt PI))

;; Golden ratio \\(\varphi\\)
(def ^:const ^double PHI (* (+ 1.0 SQRT5) 0.5))

;; math.h predefined constants names
(def ^:const ^double M_E E)
(def ^:const ^double M_LOG2E LOG2E)
(def ^:const ^double M_LOG10E LOG10E)
(def ^:const ^double M_LN2 LN2)
(def ^:const ^double M_LN10 LN10)
(def ^:const ^double M_PI PI)
(def ^:const ^double M_PI_2 HALF_PI)
(def ^:const ^double M_PI_4 QUARTER_PI)
(def ^:const ^double M_1_PI (/ 1.0 PI))
(def ^:const ^double M_2_PI (/ 2.0 PI))
(def ^:const ^double M_2_SQRTPI (/ 2.0 SQRTPI))
(def ^:const ^double M_SQRT2 SQRT2)
(def ^:const ^double M_SQRT1_2 (/ 1.0 SQRT2))

(def ^:const ^double M_TWOPI TWO_PI)
(def ^:const ^double M_3PI_4 (* PI 0.75))
(def ^:const ^double M_SQRT_PI SQRTPI)
(def ^:const ^double M_LN2LO 1.9082149292705877000E-10)
(def ^:const ^double M_LN2HI 6.9314718036912381649E-1)
(def ^:const ^double M_SQRT3 SQRT3)
(def ^:const ^double M_IVLN10 (/ 1.0 LN10))
(def ^:const ^double M_LOG2_E LN2)
(def ^:const ^double M_INVLN2 (/ 1.0 LN2))

(defn signum
  "Return 1 if the specified value is > 0, 0 if it is 0, -1 otherwise"
  [^double value]
  (cond (pos? value) 1
        (neg? value) -1
        :else 0))

(defn sgn
  "Return -1 when value is negative, 1 otherwise"
  [^double value]
  (if (neg? value) -1 1))

;;`(constrain 0.5 1 2) => 1`  
;;`(constrain 1.5 1 2) => 1.5`  
;;`(constrain 2.5 1 2) => 2`  
(defn constrain
  "Clamp value between mn and mx"
  [^double value ^double mn ^double mx]
  (if (> value mx) 
    mx
    (if (< value mn) 
      mn 
      value)))

;; Map value from range `[start1,stop1]` to new range `[start2,stop2]` or if new range is not given map to `[0,1]`
(defn norm
  "Processing map and norm"
  ([v start1 stop1 start2 stop2] ;; map
   (+ ^double start2 (* (- ^double stop2 ^double start2) ^double (norm v start1 stop1))))
  ([^double v ^double start ^double stop] ;; norm
   (if (== start stop)
     (if (< v start) 0 1)
     (/ (- v start) (- stop start)))))

;; Map and constrain values
;; `(cnorm 1.5 0 1 100 200) => 200`
(defn cnorm
  "Constrained version of norm"
  ([v start1 stop1 start2 stop2]
   (constrain (norm v start1 stop1 start2 stop2) start2 stop2))
  ([v start stop]
   (constrain (norm v start stop) 0 1)))

;; Linear interpolation between `start` and `stop`.
(defn lerp
  "Processing lerp"
  [^double start ^double stop ^double t]
  (let [t1 (- 1.0 t)]
    (+ (* t1 start) (* t stop))))

;; Cosine interpolation between `start` and `stop`
(defn cos-interpolation
  "oF interpolateCosine"
  [^double start ^double stop ^double t]
  (let [t1 (* 0.5 (- 1.0 ^double (cos (* t PI))))]
    (lerp start stop t1)))

(defn smoothstep
  "GL smoothstep"
  [^double start ^double stop ^double x]
  (let [t ^double (norm x start stop)]
    (* t t (- 3.0 (* 2.0 t)))))

;;`(wrap 0 -1 1) => 0.0`  
;;`(wrap -1.1 -1 1) => 0.8999999999999999`  
;;`(wrap 1.1 -1 1) => -0.8999999999999999`
(defn wrap
  "Wrap overflowed value into the range, ofWrap"
  [^double start ^double stop ^double value]
  (let [p (> start stop)
        from (if p stop start)
        to (if p start stop)
        cycle ^double (- to from)]
    (if (zero? cycle)
      to
      (->> cycle
           (/ (- value from))
           ^double (floor)
           (* cycle)
           (- value)))))

;;
;; ### Statistics
;;
;; Whole code is taken from public GIST: https://gist.github.com/scottdw/2960070
;;
;; * `mode`
;; * `quantile`
;; * `median`
;; * `mean`
;; * `standard-deviation` 
;; * `median-absolute-deviation`
;; * `lower-adjacent-value`, `upper-adjacent-value`
;; * `k-means`
;;
;; Additionally you can gather all statistics into one map by calling `stats-map` functions
;;

;; `(mode '(1 2 3 -1 -1 2 -1 11 111)) => -1`
(defn mode
  "Find the value that appears most often in a dataset"
  [vs]
  (let [fs (frequencies vs)]
    (first (last (sort-by second fs)))))

;; `(quantile 0.25 '(1 2 3 -1 -1 2 -1 11 111)) => -1.0`
;; `(quantile 0.75 '(1 2 3 -1 -1 2 -1 11 111)) => 7.0`
;; `(quantile 0.9  '(1 2 3 -1 -1 2 -1 11 111)) => 111`
(defn quantile
  "Calculate p-quantile of a list"
  (^double [^double p vs]
     (let [svs (sort vs)]
       (quantile p (count vs) svs (first svs) (last svs))))
  ([p c svs mn mx]
   (let [pic (* p (inc c))
         k (round pic)
         d (- pic k)
         ndk (if (zero? k) mn (nth svs (dec k)))]
       (cond
        (zero? k) mn
        (= c (dec k)) mx
        (= c k) mx
        :else (+ ndk (* d (- (nth svs k) ndk)))))))

;; `(median '(1 2 3 -1 -1 2 -1 11 111)) => 2.0`
(defn median
  "Calculate median of a list"
  ([vs] (quantile 0.5 vs))
  ([sz svs mn mx] (quantile 0.5 sz svs mn mx)))

;; `(mean '(1 2 3 -1 -1 2 -1 11 111)) => 14.11111111111111`
(defn mean
  "Calculate mean of a list"
  ([vs] (mean (reduce + vs) (count vs)))
  ([^double sm sz] (/ sm (double sz))))

;; `(standard-deviation '(1 2 3 -1 -1 2 -1 11 111)) => 34.43333154064031`
(defn standard-deviation
  "Calculate standard deviation of a list"
  ([vs]
   (standard-deviation vs (double (count vs)) (mean vs)))
  ([vs ^double sz ^double u]
   (sqrt (/ ^double (reduce + (map #(pow (- ^double % u) 2) vs)) sz))))

;; `(median-absolute-deviation '(1 2 3 -1 -1 2 -1 11 111))  => 3.0`
(defn median-absolute-deviation
  "Calculate MAD"
  ([vs]
     (median-absolute-deviation vs (median vs)))
  ([vs ^double m]
     (median (map #(abs (- ^double % m)) vs))))

;; `(lower-adjacent-value '(1 2 3 -1 -1 2 -1 11 111)) => -1`
(defn lower-adjacent-value
  ([vs]
   (let [q1 (quantile 0.25 vs)
         m (median vs)
         q3 (quantile 0.75 vs)]
       (lower-adjacent-value (sort vs) m (- q3 q1))))
  ([svs ^double m ^double qd]
     (let [l (- m qd)]
       (first (filter (partial < l) svs)))))

;; `(upper-adjacent-value '(1 2 3 -1 -1 2 -1 11 111)) => 3`
(defn upper-adjacent-value
  ([vs]
     (let [q1 (quantile 0.25 vs)
           m (median vs)
           q3 (quantile 0.75 vs)]
       (upper-adjacent-value (reverse (sort vs)) m (- q3 q1))))
  ([rsvs ^double m ^double qd]
     (let [l (+ m qd)]
       (first (filter #(< ^double % l) rsvs)))))

;; `(stats-map '(1 2 3 -1 -1 2 -1 11 111))`
;; `=> {:MAD 3.0, :Max 111, :Size 9, :LAV -1, :Mode -1, :Mean 14.11111111111111, :Q1 -1.0, :Q3 7.0, :Min -1, :Total 127, :SD 34.43333154064031, :UAV 3, :Median 2.0}`
(defn stats-map
  ([vs]
     (let [sz (count vs)
           svs (sort vs)
           rsvs (reverse svs)
           mn (first svs)
           mx (first rsvs)
           sm (reduce + vs)
           u (mean sm sz)
           mdn (median sz svs mn mx)
           q1 (quantile 0.25 sz svs mn mx)
           q3 (quantile 0.75 sz svs mn mx)
           sd (standard-deviation vs sz u)
           mad (median-absolute-deviation vs mdn)
           qd (- ^double q3 ^double q1)
           lav (lower-adjacent-value svs mdn qd)
           uav (upper-adjacent-value rsvs mdn qd)]
       {
        :Size sz
        :Min mn
        :Max mx
        :Mean u
        :Median mdn
        :Mode (mode vs)
        :Q1 q1
        :Q3 q3
        :Total sm
        :SD sd
        :MAD mad
        :LAV lav
        :UAV uav}))
  ([ks vs]
     (zipmap ks (map (stats-map vs) ks))))

(defn- closest-mean-fn
  [means]
  (fn [^double v] (reduce (partial min-key #(sq (- v ^double %))) means)))

;; `(k-means 4 '(1 2 3 -1 -1 2 -1 11 111)) => (-1.0 2.0 11.0 111.0)`
(defn k-means
  "k-means clustering"
  [^long k vs]
  (let [vs (map double vs)
        svs (set vs)]
    (if (> k (count svs))
      (sort svs)
      (loop [mns (sort (take k (shuffle svs)))
             pmns (repeat k Double/NaN)]
        (if (= mns pmns)
          mns
          (recur (sort (map mean (vals (group-by (closest-mean-fn mns) vs)))) mns))))))


;; ## Random function wrappers
;; 
;; Main goal for this namespace is to prepare various functions for various RNG algorithms.
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
;; * create `Randomizer` protocol with more processing-like random functions for every primitive type: `int`, `long`, `float`, `double` and `boolean`. Additionally provide similar contract for Gaussian random, and boolean random with set probability (you set probability for getting `true`)
;; * enhance every given RNG class with `Randomizer` protocol. This is done by extension of RandomGenerator interface. Every RNG from Apache Commons Math implements it.
;; * create factory multimethod to generate RNG object for every class
;;
;; Currently supported RNGs:
;;
;; * `:jdk` - default java.util.Random
;; * `:mersenne` - MersenneTwister
;; * `:isaac` - ISAAC
;; * `:well512a`, `:well1024a`, `:well19937a`, `:well19937c`, `:well44497a`, `:well44497b` - several WELL variants

(defn next-random-value-long
  ""
  (^long [^RandomGenerator r] (.nextLong r))
  (^long [^RandomGenerator r ^long mx] (mod (.nextLong r) mx))
  (^long [r ^long mn ^long mx]
   (let [diff (- mx mn)]
     (if (zero? diff) mn
         (+ mn (next-random-value-long r diff))))))

(defn next-random-value-double
  ""
  (^double [^RandomGenerator r] (.nextDouble r))
  (^double [^RandomGenerator r ^double mx] (* (.nextDouble r) mx))
  (^double [r ^double mn ^double mx]
   (let [diff (- mx mn)]
     (if (zero? diff) mn
         (+ mn (next-random-value-double r diff))))))

(defn next-random-value-gaussian
  ""
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
;; * `avg` - average for Gaussian (default 0)
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
;; * `(grandom RNG avg std)` - returns number from normal(avg,std) distibution
;; * `(brandom RNG)` - returns true or false with probability 50%
;; * `(brandom RNG thr)` - returns true with probability `thr` (false with probability `1-thr`)
(defprotocol Randomizer
  (irandom [t] [t mx] [t mn mx] "int random")
  (drandom [t] [t mx] [t mn mx] "double random")
  (lrandom [t] [t mx] [t mn mx] "long random")
  (frandom [t] [t mx] [t mn mx] "float random")
  (grandom [t] [t std] [t avg std] "gaussian random")
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
            ([^RandomGenerator t] (next-random-value-gaussian t))
            ([^RandomGenerator t std] (next-random-value-gaussian t std))
            ([^RandomGenerator t ^double avg ^double std] (next-random-value-gaussian t avg (+ avg std))))
   :brandom (fn
            ([^RandomGenerator t] (.nextBoolean t))
            ([^RandomGenerator t ^double thr] (< (next-random-value-double t) thr)))})

;; Helper macro which creates RNG object of given class and/or seed.
(defmacro create-object-with-seed
  "Create object of the class with (or not) given seed"
  [cl seed]
  `(if-let [arg# ~seed]
     (new ~cl (int arg#))
     (new ~cl)))

;; Multimethod `make-randomizer` creates object with new set of functions
;;
;;`(make-randomizer :well512a) => #object[org.apache.commons.math3.random.Well512a 0x1a8535ec "org.apache.commons.math3.random.Well512a@1a8535ec"]`
;;
;;`(make-randomizer :aaaa 123) => #object[org.apache.commons.math3.random.JDKRandomGenerator 0x3f12abed "org.apache.commons.math3.random.JDKRandomGenerator@3f12abed"]`
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

;; ### Default RNG
;;
;; Define easy to use functions with JDK randomizer
;;
;;`(drand 10) => 3.0752117891384048`  
;;`(drand 10 20) => 11.327971928466651`
(def default-random (make-randomizer :jdk))
(def irand (partial irandom default-random))
(def lrand (partial lrandom default-random))
(def frand (partial frandom default-random))
(def drand (partial drandom default-random))
(def grand (partial grandom default-random))
(def brand (partial brandom default-random))

;; ## Noise

(defn noise-with
  ""
  ([^Perlin n x] (* 0.5 (+ 1.0 (.getValue n x 0.5 0.5))))
  ([^Perlin n x y] (* 0.5 (+ 1.0 (.getValue n x y 0.5))))
  ([^Perlin n x y z] (* 0.5 (+ 1.0 (.getValue n x y z)))))

(defn make-perlin-noise
  ""
  ([] (partial noise-with (Perlin.)))
  ([seed]
   (let [^Perlin n (Perlin.)]
     (.setSeed n seed)
     (partial noise-with n)))
  ([seed octaves]
   (let [^Perlin n (Perlin.)]
     (.setSeed n seed)
     (.setOctaveCount n octaves)
     (partial noise-with n))))

(def noise (make-perlin-noise))

;;;

(def ^:const ^double AM (/ 1.0 2147483647))

(defn discrete-noise
  "Discrete noise"
  [X Y]
  (let [n (unchecked-add-int X (unchecked-multiply-int Y 57))
        nn (unchecked-int (bit-xor n (bit-shift-left n 13)))
        nnn (unchecked-add-int 1376312589 (unchecked-multiply-int nn (unchecked-add-int 789221 (unchecked-multiply-int nn (unchecked-multiply-int nn 15731)))))]
    (* AM (unchecked-int (bit-and 0x7fffffff nnn)))))
