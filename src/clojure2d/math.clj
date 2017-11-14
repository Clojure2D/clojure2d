;; # Namespace scope
;;
;; Collection of math function:
;;
;; * Several constants from Java, C, Processing, etc.
;; * Functions based on FastMath exposed as macros or functions (trigonometry, powers/logartihms/roots, rounding)
;; * Primitive operators (as in primitive-math package)
;; * Additional math functions (signum, constrain, interpolation)
;; * Statistics

(ns clojure2d.math
  "Math functions"
  (:refer-clojure
   :exclude [* + - / > < >= <= == rem quot mod bit-or bit-and bit-xor bit-not bit-shift-left bit-shift-right unsigned-bit-shift-right inc dec zero? neg? pos? min max even? odd?])
  (:import [net.jafama FastMath]
           [clojure2d.java PrimitiveMath]
           [clojure.lang Numbers]
           [org.apache.commons.math3.special Erf]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; ## Macros

(defmacro ^:private javaclass-proxy
  "Wrapps operation into macro"
  ([class alt-name name]
   (let [f (symbol (str class "/" name))
         d (str  class " "  name " function.")]
     `(defmacro ~alt-name
        ([x#]
         (list '~f x#))
        ([x# y#]
         (list '~f x# y#))
        ([x# y# z#]
         (list '~f x# y# z#)))))
  ([class name]
   `(javaclass-proxy ~class ~name ~name)))

(defmacro ^:private fastmath-proxy [& rest] `(javaclass-proxy "net.jafama.FastMath" ~@rest))
(defmacro ^:private primitivemath-proxy [& rest] `(javaclass-proxy "clojure2d.java.PrimitiveMath" ~@rest))
(defmacro ^:private erf-proxy [& rest] `(javaclass-proxy "org.apache.commons.math3.special.Erf" ~@rest))

(defmacro ^:private variadic-proxy
  "Creates left-associative variadic forms for any operator.
  https://github.com/ztellman/primitive-math/blob/master/src/primitive_math.clj#L10"
  ([name]
   `(variadic-proxy ~name ~name))
  ([name fn]
   `(variadic-proxy ~name ~fn identity))
  ([name fn single-arg-form]
   (let [x-sym (gensym "x")
         fname (symbol (str "clojure2d.java.PrimitiveMath/" fn))
         doc (str "A primitive macro version of `" name "`")]
     `(defmacro ~name
        ~doc
        ([~x-sym]
         ~((eval single-arg-form) x-sym))
        ([x# y#]
         (list '~fname x# y#))
        ([x# y# ~'& rest#]
         (list* '~name (list '~name x# y#) rest#))))))

(defmacro ^:private variadic-predicate-proxy
  "Turns variadic predicates into multiple pair-wise comparisons.
  https://github.com/ztellman/primitive-math/blob/master/src/primitive_math.clj#L27"
  ([name]
   `(variadic-predicate-proxy ~name ~name))
  ([name fn]
   `(variadic-predicate-proxy ~name ~fn (constantly true)))
  ([name fn single-arg-form]
   (let [x-sym (gensym "x")
         fname (symbol (str "clojure2d.java.PrimitiveMath/" fn))
         doc (str "A primitive macro version of `" name "`")]
     `(defmacro ~name
        ~doc
        ([~x-sym]
         ~((eval single-arg-form) x-sym))
        ([x# y#]
         (list '~fname x# y#))
        ([x# y# ~'& rest#]
         (list 'clojure2d.java.PrimitiveMath/and (list '~name x# y#) (list* '~name y# rest#)))))))

;; ## Basic operations

(variadic-proxy + add)
(variadic-proxy - subtract (fn [x] `(list 'clojure2d.java.PrimitiveMath/negate ~x)))
(variadic-proxy * multiply)
(variadic-proxy / divide (fn [x] `(list 'clojure2d.java.PrimitiveMath/reciprocal ~x)))
(primitivemath-proxy inc)
(primitivemath-proxy dec)
(primitivemath-proxy rem remainder)
(primitivemath-proxy quot quotient)
(primitivemath-proxy mod modulus)
(variadic-proxy bit-and bitAnd)
(variadic-proxy bit-or bitOr)
(variadic-proxy bit-xor bitXor)
(primitivemath-proxy bit-not bitNot)
(variadic-proxy bool-and and)
(variadic-proxy bool-or or)
(variadic-proxy bool-xor xor)
(primitivemath-proxy bool-not not)
(variadic-proxy min)
(variadic-proxy max)
(primitivemath-proxy zero? isZero)
(primitivemath-proxy neg? isNeg)
(primitivemath-proxy pos? isPos)
(primitivemath-proxy even? isEven)
(primitivemath-proxy odd? isOdd)
(primitivemath-proxy << shiftLeft)
(primitivemath-proxy >> shiftRight)
(primitivemath-proxy >>> unsignedShiftRight)
(primitivemath-proxy bit-shift-left shiftLeft)
(primitivemath-proxy bit-shift-right shiftRight)
(primitivemath-proxy unsigned-bit-shift-right unsignedShiftRight)

(variadic-predicate-proxy < lt)
(variadic-predicate-proxy > gt)
(variadic-predicate-proxy <= lte)
(variadic-predicate-proxy >= gte)
(variadic-predicate-proxy == eq)
(variadic-predicate-proxy not== neq)

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

(def ^:const ^double MACHINE-EPSILON (* 0.5 (double (loop [d (double 1.0)]
                                                      (if (not== 1.0 (+ 1.0 (* d 0.5)))
                                                        (recur (* d 0.5))
                                                        d)))))
;; Common fractions
(def ^:const ^double THIRD (/ 3.0))
(def ^:const ^double TWO_THIRD (/ 2.0 3.0))
(def ^:const ^double SIXTH (/ 6.0))

;; Trigonometry
(fastmath-proxy sin)
(fastmath-proxy cos)
(fastmath-proxy tan)
(fastmath-proxy asin)
(fastmath-proxy acos)
(fastmath-proxy atan)
(fastmath-proxy sinh)
(fastmath-proxy cosh)
(fastmath-proxy tanh)
(fastmath-proxy asinh)
(fastmath-proxy acosh)
(fastmath-proxy atanh)

;; Quick and less accurate `sin` and `cos`
(fastmath-proxy qsin sinQuick)
(fastmath-proxy qcos cosQuick)

;; Additional trigonometry functions
(defn cot ^double [^double v] (FastMath/tan (- HALF_PI v)))
(defn sec ^double [^double v] (/ (FastMath/cos v)))
(defn csc ^double [^double v] (/ (FastMath/sin v)))

;; Additional cyclometric functions
(defn acot ^double [^double v] (- HALF_PI (FastMath/atan v)))
(defn asec ^double [^double v] (FastMath/acos (/ 1.0 v)))
(defn acsc ^double [^double v] (FastMath/asin (/ 1.0 v)))
(fastmath-proxy atan2)

;; Additional hyperbolic functions
(defn coth ^double [^double v] (/ (FastMath/tanh v)))
(defn sech ^double [^double v] (/ (FastMath/cosh v)))
(defn csch ^double [^double v] (/ (FastMath/sinh v)))

;; Additional inverse hyperbolic functions
(defn acoth ^double [^double v] (FastMath/atanh (/ v)))
(defn asech ^double [^double v] (FastMath/acosh (/ v)))
(defn acsch ^double [^double v] (FastMath/asinh (/ v)))

;; exp and log
(fastmath-proxy exp)
(fastmath-proxy log)
(fastmath-proxy log10)
;; Alias for natural logarithm
(fastmath-proxy ln log)

;; Roots (square and cubic)
(fastmath-proxy sqrt)
(fastmath-proxy cbrt)

;; Quick version of exponential \\(e^x\\)
(fastmath-proxy qexp expQuick)

;; Radians to degrees (and opposite) conversions
(def ^:const ^double rad-in-deg (/ 180.0 PI))
(def ^:const ^double deg-in-rad (/ PI 180.0))
(defn radians ^double [^double deg] (* deg-in-rad deg))
(defn degrees ^double [^double rad] (* rad-in-deg rad))

;; Erf
(erf-proxy erf)
(erf-proxy erfc)
(erf-proxy inv-erf erfInv)
(erf-proxy inv-erfc erfcInv)

;; Sinc
(defn sinc
  "Sinc function"
  ^double [^double v]
  (let [x (* PI (FastMath/abs v))]
    (if (< x 1.0e-5) 1.0
        (/ (FastMath/sin x) x))))

;; Few logarithm constants
;; \\(\ln 2\\)
(def ^:const ^double LN2 (log 2.0))
(def ^:const ^double INV_LN2 (/ LN2))
(def ^:const ^double LN2_2 (* 0.5 LN2))

;; \\(\ln 10\\)
(def ^:const ^double LN10 (log 10.0))

;; \\(\frac{1.0}{\ln{0.5}}\\)
(def ^:const ^double INV_LOG_HALF (/ (log 0.5)))

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
(fastmath-proxy qlog logQuick)

;; \\(\log_2 e\\)
(def ^:const ^double LOG2E (log2 E))

;; \\(\log_{10} e\\)
(def ^:const ^double LOG10E (log10 E))

;; Powers (normal, quick)
(fastmath-proxy pow)
(fastmath-proxy qpow powQuick)

;; Fast version of power, second parameter should be integer
(fastmath-proxy fpow powFast)

;; Square and cubic
(defn sq ^double [^double v] (* v v))
(defn pow2 ^double [^double v] (* v v))
(defn pow3 ^double [^double v] (* v (* v v)))

(defn safe-sqrt
  "Safe sqrt, for value <= 0 result is 0"
  ^double [^double value]
  (if (neg? value) 0.0 (sqrt value)))

;; Approximated sqrt via binary operations (error 1.0E-2)
(fastmath-proxy qsqrt sqrtQuick)
(fastmath-proxy rqsqrt invSqrtQuick)

;; \\(\sqrt{x^2+y^2}\\) and \\(\sqrt{x^2+y^2+z^2}\\)
(defn hypot
  "SQRT version of hypot - fast, not safe"
  (^double [^double x ^double y]
   (sqrt (+ (* x x) (* y y))))
  (^double [^double x ^double y ^double z]
   (sqrt (+ (* x x) (* y y) (* z z)))))

;; Let's not use
(comment fastmath-proxy hypot)

;; distance
(defn dist
  "Distance between points 2d"
  ^double [^double x1 ^double y1 ^double x2 ^double y2]
  (sqrt (+ (sq (- x2 x1)) (sq (- y2 y1)))))

(defn qdist
  "Quick version of distance between points 2d"
  ^double [^double x1 ^double y1 ^double x2 ^double y2]
  (qsqrt (+ (sq (- x2 x1)) (sq (- y2 y1)))))

;; Rounding functions
(defn floor ^double [^double v] (FastMath/floor v))
(defn ceil ^double [^double v] (FastMath/ceil v))
(defn round ^long [^double v] (FastMath/round v))
(fastmath-proxy rint)

;; Modulo and abs
(fastmath-proxy remainder)
(defn abs ^double [^double v] (FastMath/abs v))
(fastmath-proxy iabs)

;; fractional part, always returns values from 0.0 to 1.0 (exclusive)
(defmacro frac [v] `(abs (- ~v (unchecked-long ~v))))

;; Find power of 2 exponent for double number where  
;; \\(2^(n-1)\leq x\leq 2^n\\)  
;; where n-1 is result of `low-2-exp` and n is result of `high-2-exp`
;; `(low-2-exp TWO_PI) => 2` \\(2^2\eq 4\leq 6.28\\)  
;; `(high-2-exp TWO_PI) => 3` \\(6.28\leq 2^3\eq 8\\)
(defn low-2-exp ^long [v] (-> v log2 floor unchecked-long))
(defn high-2-exp ^long [v] (-> v log2 ceil unchecked-long))

(defn round-up-pow2
  "Round long to the next power of 2"
  ^long [^long v]
  (as-> (dec v) v
    (bit-or v (>> v 1))
    (bit-or v (>> v 2))
    (bit-or v (>> v 4))
    (bit-or v (>> v 8))
    (bit-or v (>> v 16))
    (bit-or v (>> v 32))
    (inc v)))

(defn next-float-up
  "Next double value."
  (^double [^double v]
   (let [ui (Double/doubleToRawLongBits (if (zero? v) 0.0 v))]
     (Double/longBitsToDouble (if (neg? v) (dec ui) (inc ui)))))
  (^double [^double v ^long delta]
   (let [ui (Double/doubleToRawLongBits (if (zero? v) 0.0 v))]
     (Double/longBitsToDouble (if (neg? v) (- ui delta) (+ ui delta))))))

(defn next-float-down
  "Prev double value."
  (^double [^double v]
   (let [ui (Double/doubleToRawLongBits (if (zero? v) 0.0 v))]
     (Double/longBitsToDouble (if (pos? v) (dec ui) (inc ui)))))
  (^double [^double v ^long delta]
   (let [ui (Double/doubleToRawLongBits (if (zero? v) 0.0 v))]
     (Double/longBitsToDouble (if (pos? v) (- ui delta) (+ ui delta))))))

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
(def ^:const ^double SQRT2PI (sqrt TWO_PI))

;; Golden ratio \\(\varphi\\)
(def ^:const ^double PHI (* (inc SQRT5) 0.5))

;; math.h predefined constants names
(def ^:const ^double M_E E)
(def ^:const ^double M_LOG2E LOG2E)
(def ^:const ^double M_LOG10E LOG10E)
(def ^:const ^double M_LN2 LN2)
(def ^:const ^double M_LN10 LN10)
(def ^:const ^double M_PI PI)
(def ^:const ^double M_PI_2 HALF_PI)
(def ^:const ^double M_PI_4 QUARTER_PI)
(def ^:const ^double M_1_PI (/ PI))
(def ^:const ^double M_2_PI (/ 2.0 PI))
(def ^:const ^double M_2_SQRTPI (/ 2.0 SQRTPI))
(def ^:const ^double M_SQRT2 SQRT2)
(def ^:const ^double M_SQRT1_2 (/ SQRT2))

(def ^:const ^double M_TWOPI TWO_PI)
(def ^:const ^double M_3PI_4 (* PI 0.75))
(def ^:const ^double M_SQRT_PI SQRTPI)
(def ^:const ^double M_LN2LO 1.9082149292705877000E-10)
(def ^:const ^double M_LN2HI 6.9314718036912381649E-1)
(def ^:const ^double M_SQRT3 SQRT3)
(def ^:const ^double M_IVLN10 (/ LN10))
(def ^:const ^double M_LOG2_E LN2)
(def ^:const ^double M_INVLN2 (/ LN2))

(defn signum
  "Return 1 if the specified value is > 0, 0 if it is 0, -1 otherwise"
  ^double [^double value]
  (cond (pos? value) 1.0
        (neg? value) -1.0
        true 0.0))

(defn sgn
  "Return -1 when value is negative, 1 otherwise"
  ^double [^double value]
  (if (neg? value) -1.0 1.0))

;;`(constrain 0.5 1 2) => 1`  
;;`(constrain 1.5 1 2) => 1.5`  
;;`(constrain 2.5 1 2) => 2`  
(defmacro constrain
  "Clamp value between mn and mx"
  [value mn mx]
  `(max (min ~value ~mx) ~mn))

;; Map value from range `[start1,stop1]` to new range `[start2,stop2]` or if new range is not given map to `[0,1]`
(defmacro ^:private normalize-macro
  [v start stop]
  `(if (== ~start ~stop)
     (if (< ~v ~start) 0.0 1.0)
     (/ (- ~v ~start) (- ~stop ~start))))

(defn norm
  "Processing map and norm"
  ([v start1 stop1 start2 stop2] ;; map
   (+ ^double start2 (* (- ^double stop2 ^double start2) (normalize-macro ^double v ^double start1 ^double stop1))))
  (^double [^double v ^double start ^double stop] ;; norm
   (normalize-macro v start stop)))

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
   (constrain ^double (norm v start1 stop1 start2 stop2) ^double start2 ^double stop2))
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
  "quad interpolation"
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

;; Primitive math eq
(defn eq 
  "Primitive math equality helper for doubles"
  [^double a ^double b]
  (== a b))

(defn med
  "MMedian of three values."
  ^double [^double a ^double b ^double c]
  (max (min a b) (min (max a b) c)))

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
       (== ^long c (dec k)) mx
       (== ^long c k) mx
       :else (+ ndk (* d (- ^double (nth svs k) ndk)))))))

;; `(median '(1 2 3 -1 -1 2 -1 11 111)) => 2.0`
(defn median
  "Calculate median of a list"
  ([vs] (quantile 0.5 vs))
  ([sz svs mn mx] (quantile 0.5 sz svs mn mx)))

;; `(mean '(1 2 3 -1 -1 2 -1 11 111)) => 14.11111111111111`
(defn mean
  "Calculate mean of a list"
  ([vs] (mean (reduce clojure.core/+ vs) (count vs)))
  ([^double sm ^long sz] (/ sm sz)))

;; `(standard-deviation '(1 2 3 -1 -1 2 -1 11 111)) => 34.43333154064031`
(defn standard-deviation
  "Calculate standard deviation of a list"
  ([vs]
   (standard-deviation vs (count vs) (mean vs)))
  ([vs ^long sz ^double u]
   (sqrt (/ ^double (reduce clojure.core/+ (map #(pow (- ^double % u) 2) vs)) sz))))

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
     (first (filter (partial clojure.core/< l) svs)))))

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
         sm (reduce clojure.core/+ vs)
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


;; ## Copy of primitive math machinery
;;
;; Simplified to be used after `ns` is defined.

(def ^:private vars-to-exclude
  '[* + - / > < >= <= == rem quot mod bit-or bit-and bit-xor bit-not bit-shift-left bit-shift-right unsigned-bit-shift-right inc dec zero? neg? pos? min max even? odd? bool-and bool-or bool-xor bool-not << >> >>> not==])

(defn- using-primitive-operators? []
  (= #'clojure2d.math/+ (resolve '+)))

(defn use-primitive-operators
  "Replaces Clojure's arithmetic and number coercion functions with primitive equivalents.  These are
   defined as macros, so they cannot be used as higher-order functions.  This is an idempotent operation.."
  []
  (when-not (using-primitive-operators?)
    (doseq [v vars-to-exclude]
      (ns-unmap *ns* v))
    (require (vector 'clojure2d.math :refer vars-to-exclude))))

(defn unuse-primitive-operators
  "Undoes the work of `use-primitive-operators`.  This is idempotent."
  []
  (when (using-primitive-operators?)
    (doseq [v vars-to-exclude]
      (ns-unmap *ns* v))
    (refer 'clojure.core)))
