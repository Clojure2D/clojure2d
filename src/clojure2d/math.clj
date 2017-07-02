;; # Namespace scope
;;
;; Collection of math function:
;;
;; * Several constants from Java, C, Processing, etc.
;; * Functions based on FastMath wrapped in Clojure functions (trigonometry, powers/logartihms/roots, rounding)
;; * Additional math functions (signum, constrain, interpolation)
;; * Statistics
;; * Noise and random

(ns clojure2d.math
  "Math functions"
  (:import [net.jafama FastMath]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; ## Math functions
;;
;; FastMath functions type hinted wrapped in processing function (to allow composition etc.).
;; All functions operate and return `double` or in some cases `long`.

;; Processing math constants
(def ^:const ^double PI Math/PI)
(def ^:const ^double HALF_PI (/ PI 2.0))
(def ^:const ^double THIRD_PI (/ PI 3.0))
(def ^:const ^double QUARTER_PI (/ PI 4.0))
(def ^:const ^double TWO_PI (* PI 2.0))
(def ^:const ^double TAU TWO_PI)
(def ^:const ^double E Math/E)

;; Very small number \\(\varepsilon\\)
(def ^:const ^double EPSILON 1.0e-10)

;; Common fractions
(def ^:const ^double THIRD (/ 1.0 3.0))
(def ^:const ^double TWO_THIRD (/ 2.0 3.0))
(def ^:const ^double SIXTH (/ 1.0 6.0))

;; Trigonometry
(defn sin ^double [^double v] (FastMath/sin v))
(defn cos ^double [^double v] (FastMath/cos v))
(defn tan ^double [^double v] (FastMath/tan v))
(defn asin ^double [^double v] (FastMath/asin v))
(defn acos ^double [^double v] (FastMath/acos v))
(defn atan ^double [^double v] (FastMath/atan v))
(defn sinh ^double [^double v] (FastMath/sinh v))
(defn cosh ^double [^double v] (FastMath/cosh v))
(defn tanh ^double [^double v] (FastMath/tanh v))
(defn asinh ^double [^double v] (FastMath/asinh v))
(defn acosh ^double [^double v] (FastMath/acosh v))
(defn atanh ^double [^double v] (FastMath/atanh v))

;; Quick and less accurate `sin` and `cos`
(defn qsin ^double [^double v] (FastMath/sinQuick v))
(defn qcos ^double [^double v] (FastMath/cosQuick v))

;; Additional trigonometry functions
(defn cot ^double [^double v] (FastMath/tan (- HALF_PI v)))
(defn sec ^double [^double v] (/ 1.0 (FastMath/cos v)))
(defn csc ^double [^double v] (/ 1.0 (FastMath/sin v)))

;; Additional cyclometric functions
(defn acot ^double [^double v] (- HALF_PI (FastMath/atan v)))
(defn asec ^double [^double v] (FastMath/acos (/ 1.0 v)))
(defn acsc ^double [^double v] (FastMath/asin (/ 1.0 v)))
(defn atan2 ^double [^double v1 ^double v2] (FastMath/atan2 v1 v2))

;; Additional hyperbolic functions
(defn coth ^double [^double v] (/ 1.0 (FastMath/tanh v)))
(defn sech ^double [^double v] (/ 1.0 (FastMath/cosh v)))
(defn csch ^double [^double v] (/ 1.0 (FastMath/sinh v)))

;; Additional inverse hyperbolic functions
(defn acoth ^double [^double v] (FastMath/atanh (/ 1.0 v)))
(defn asech ^double [^double v] (FastMath/acosh (/ 1.0 v)))
(defn acsch ^double [^double v] (FastMath/asinh (/ 1.0 v)))

;; exp and log
(defn exp ^double [^double v] (FastMath/exp v))
(defn log ^double [^double v] (FastMath/log v))
(defn log10 ^double [^double v] (FastMath/log10 v))

;; Roots (square and cubic)
(defn sqrt ^double [^double v] (FastMath/sqrt v))
(defn cbrt ^double [^double v] (FastMath/cbrt v))

;; Quick version of exponential \\(e^x\\)
(defn qexp ^double [^double v] (FastMath/expQuick v))

;; Radians to degrees (and opposite) conversions
(def ^:const ^double rad-in-deg (/ 180.0 PI))
(def ^:const ^double deg-in-rad (/ PI 180.0))
(defn radians ^double [^double deg] (* deg-in-rad deg))
(defn degrees ^double [^double rad] (* rad-in-deg rad))

;; Sinc
(defn sinc
  "Sinc function"
  ^double [^double v]
  (let [x (* PI (FastMath/abs v))]
    (if (< x 1.0e-5) 1.0
        (/ (FastMath/sin x) x))))

;; Alias for natural logarithm
(def ln log)

;; Few logarithm constants
;; \\(\ln 2\\)
(def ^:const ^double LN2 (log 2.0))
(def ^:const ^double INV_LN2 (/ 1.0 LN2))
(def ^:const ^double LN2_2 (* 0.5 LN2))

;; \\(\ln 10\\)
(def ^:const ^double LN10 (log 10.0))

;; \\(\frac{1.0}{\ln{0.5}}\\)
(def ^:const ^double INV_LOG_HALF (/ 1.0 ^double (log 0.5)))

(defn log2
  "Log with base 2"
  ^double [^double v]
  (* (FastMath/log v) INV_LN2))

;; \\(\log_b x\\)
(defn logb
  "Logarithm with base"
  ^double [^double base ^double v]
  (/ (FastMath/log v) (FastMath/log base)))

;; Quick logarithm
(defn qlog ^double [^double v] (FastMath/logQuick v))

;; \\(\log_2 e\\)
(def ^:const ^double LOG2E (log2 E))

;; \\(\log_{10} e\\)
(def ^:const ^double LOG10E (log10 E))

;; Powers (normal, quick and fast)
(defn pow ^double [^double v1 ^double v2] (FastMath/pow v1 v2))
(defn qpow ^double [^double v1 ^double v2] (FastMath/powQuick v1 v2))

;; Fast version of power, second parameter should be integer
(defn fpow ^double [^double v1 ^double v2] (FastMath/powFast v1 v2))

;; Square and cubic
(defn sq ^double [^double v] (FastMath/pow2 v))
(def pow2 sq)
(defn pow3 ^double [^double v] (FastMath/pow3 v))

(defn safe-sqrt
  "Safe sqrt, for value <= 0 result is 0"
  ^double [^double value]
  (if (neg? value) 0 (sqrt value)))
(defn qsqrt ^double [^double v] (FastMath/sqrtQuick v))

;; \\(\sqrt{x^2+y^2}\\) and \\(\sqrt{x^2+y^2+z^2}\\)
(defn hypot
  "Hyponetuse"
  (^double [^double x ^double y]
   (FastMath/hypot x y))
  (^double [^double x ^double y ^double z]
   (FastMath/hypot x y z)))

;; Rounding functions
(defn floor ^double [^double v] (FastMath/floor v))
(defn ceil ^double [^double v] (FastMath/ceil v))
(defn round ^long [^double v] (FastMath/round v))
(defn rint ^double [^double v] (FastMath/rint v))

;; fractional part, always returns values from 0.0 to 1.0 (exclusive)
(defn frac ^double [^double v] (FastMath/abs (- v (long v))))

;; Find power of 2 exponent for double number where  
;; \\(2^(n-1)\leq x\leq 2^n\\)  
;; where n-1 is result of `low-2-exp` and n is result of `high-2-exp`
;; `(low-2-exp TWO_PI) => 2` \\(2^2\eq 4\leq 6.28\\)  
;; `(high-2-exp TWO_PI) => 3` \\(6.28\leq 2^3\eq 8\\)
(def low-2-exp (comp long floor log2))
(def high-2-exp (comp long ceil log2))

;; Modulo and abs
(defn remainder ^double [^double v1 ^double v2] (FastMath/remainder v1 v2))
(defn abs ^double [^double v] (FastMath/abs v))
(defn iabs ^long [^long v] (FastMath/abs v))

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
  ^double [^double value]
  (cond (pos? value) 1.0
        (neg? value) -1.0
        :else 0.0))

(defn sgn
  "Return -1 when value is negative, 1 otherwise"
  ^double [^double value]
  (if (neg? value) -1.0 1.0))

;;`(constrain 0.5 1 2) => 1`  
;;`(constrain 1.5 1 2) => 1.5`  
;;`(constrain 2.5 1 2) => 2`  
(defn constrain
  "Clamp value between mn and mx"
  ^double [^double value ^double mn ^double mx]
  (if (> value mx) 
    mx
    (if (< value mn) 
      mn 
      value)))

(defn iconstrain
  "Clamp value between mn and mx (`long` optimized version)"
  ^long [^long value ^long mn ^long mx]
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
  (^double [^double v ^double start ^double stop] ;; norm
   (if (== start stop)
     (if (< v start) 0.0 1.0)
     (/ (- v start) (- stop start)))))

(defn make-norm
  "Make type hinted map/norm function"
  ([^double start ^double stop]
   (let [r (- stop start)]
     (fn ^double [^double v ^double dstart ^double dstop]
       (let [vn (/ (- v start) r)]
         (+ dstart (* (- dstop dstart) vn))))))
  ([^double start ^double stop ^double dstart ^double dstop]
   (let [r (- stop start)]
     (fn ^double [^double v]
       (let [vn (/ (- v start) r)]
         (+ dstart (* (- dstop dstart) vn)))))))

;; Map and constrain values
;; `(cnorm 1.5 0 1 100 200) => 200`
(defn cnorm
  "Constrained version of norm"
  ([v start1 stop1 start2 stop2]
   (constrain (norm v start1 stop1 start2 stop2) start2 stop2))
  (^double [v start stop]
   (constrain (norm v start stop) 0.0 1.0)))

;;; Interpolation functions

;; Linear interpolation between `start` and `stop`.
(defn lerp
  "Lerp function (same as in Processing)"
  ^double [^double start ^double stop ^double t]
  (+ start (* t (- stop start))))

(defmacro mlerp
  "lerp macro version"
  [start stop t]
  `(+ ~start (* ~t (- ~stop ~start))))

;; Cosine interpolation between `start` and `stop`
(defn cos-interpolation
  "oF interpolateCosine"
  ^double [^double start ^double stop ^double t]
  (mlerp start stop (* 0.5 (- 1.0 (cos (* t PI))))))

(defn smooth-interpolation
  "smoothstep based interpolation"
  ^double [^double start ^double stop ^double t]
  (mlerp start stop (* t t (- 3.0 (* 2.0 t)))))

(defn quad-interpolation
  ""
  ^double [^double start ^double stop ^double t]
  (mlerp start stop (let [t' (* 2.0 t)]
                      (if (< t' 1.0)
                        (* 0.5 (* t' t'))
                        (* -0.5 (dec (* (dec t') (- t' 3.0))))))))

(defn smoothstep
  "GL smoothstep"
  ^double [^double start ^double stop ^double x]
  (let [t (cnorm x start stop)]
    (* t t (- 3.0 (* 2.0 t)))))

;;`(wrap 0 -1 1) => 0.0`  
;;`(wrap -1.1 -1 1) => 0.8999999999999999`  
;;`(wrap 1.1 -1 1) => -0.8999999999999999`
(defn wrap
  "Wrap overflowed value into the range, ofWrap"
  ^double [^double start ^double stop ^double value]
  (let [p (> start stop)
        from (if p stop start)
        to (if p start stop)
        cycle (- to from)]
    (if (zero? cycle)
      to
      (->> cycle
           (/ (- value from))
           (floor)
           (* cycle)
           (- value)))))

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
   (let [pic (* ^double p (inc ^long c))
         k (round pic)
         d (- pic k)
         ^double ndk (if (zero? k) mn (nth svs (dec k)))]
     (cond
       (zero? k) mn
       (= c (dec k)) mx
       (= c k) mx
       :else (+ ndk (* d (- ^double (nth svs k) ndk)))))))

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
  "Calculate several statistics from the list and return as map"
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
       {:Size sz
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
