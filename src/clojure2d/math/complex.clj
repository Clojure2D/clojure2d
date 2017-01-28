(ns clojure2d.math.complex
  (:require [clojure2d.math :as m]
            [clojure2d.math.vector :as v])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(deftype Complex [^double real ^double imag]
  Object
  (toString [_] (str "[" real ", " imag "]")))

(def ^:const I (Complex. 0.0 1.0))
(def ^:const I- (Complex. 0.0 -1.0))
(def ^:const ONE (Complex. 1.0 0.0))
(def ^:const TWO (Complex. 2.0 0.0))
(def ^:const ZERO (Complex. 0.0 0.0))

(defn ^Vec2 to-vec2
  ""
  [^Complex z]
  (Vec2. (.real z) (.imag z)))

(defn ^Complex from-vec2
  ""
  [^Vec2 v]
  (Complex. (.x v) (.y v)))

(defn abs
  "Absolute value of complex number"
  ^double [^Complex z]
  (m/hypot (.real z) (.imag z)))

(defn add
  "Add complex"
  [^Complex z1 ^Complex z2]
  (Complex. (+ (.real z1) (.real z2)) (+ (.imag z1) (.imag z2))))

(defn sub
  "Add complex"
  [^Complex z1 ^Complex z2]
  (Complex. (- (.real z1) (.real z2)) (- (.imag z1) (.imag z2))))

(defn conjugate
  "Conjugate"
  [^Complex z]
  (Complex. (.real z) (- (.imag z))))

(defn div
  "Divide two complex numbers"
  [^Complex z1 ^Complex z2]
  (let [a (.real z1)
        b (.imag z1)
        c (.real z2)
        d (.imag z2)
        den (+ (* c c) (* d d))]
    (if (zero? den)
      ZERO
      (Complex. (/ (+ (* a c) (* b d)) den)
                (/ (- (* b c) (* a d)) den)))))

(defn reciprocal
  "1/z"
  [^Complex z]
  (div ONE z))

(defn equals?
  "Do both complex numbers equals?"
  [^Complex z1 ^Complex z2]
  (and (= (.real z1) (.real z2)) (= (.imag z1) (.imag z2))))

(defn mult
  "Multiply complex"
  [^Complex z1 ^Complex z2]
  (let [a (.real z1)
        b (.imag z1)
        c (.real z2)
        d (.imag z2)]
    (Complex. (- (* a c) (* b d))
              (+ (* a d) (* b c)))))

(defn neg
  "Negate complex"
  [^Complex z]
  (Complex. (- (.real z)) (- (.imag z))))

(defn sqrt
  "Sqrt of complex number"
  [^Complex z]
  (let [x (.real z)
        y (.imag z)
        l (abs z)
        xx (m/sqrt (+ l x))
        yy (* (m/signum y) (m/sqrt (- l x)))]
    (Complex. (* m/SQRT2_2 xx) (* m/SQRT2_2 yy))))

(defn sqrt1z
  "Sqrt(1-z^2)"
  [^Complex z]
  (->> z
       (mult z)
       (sub ONE)
       (sqrt)))

(defn cos
  "cos"
  [^Complex z]
  (let [x (.real z)
        y (.imag z)]
    (Complex. (* (m/cos x) (m/cosh y))
              (* (- (m/sin x)) (m/sinh y)))))

(defn sin
  "sin"
  [^Complex z]
  (let [x (.real z)
        y (.imag z)]
    (Complex. (* (m/sin x) (m/cosh y))
              (* (m/cos x) (m/sinh y)))))

(defn cosh
  "cosh"
  [^Complex z]
  (let [x (.real z)
        y (.imag z)]
    (Complex. (* (m/cosh x) (m/cos y))
              (* (m/sinh x) (m/sin y)))))

(defn sinh
  "sinh"
  [^Complex z]
  (let [x (.real z)
        y (.imag z)]
    (Complex. (* (m/sinh x) (m/cos y))
              (* (m/cosh x) (m/sin y)))))

(defn tan
  "tan"
  [^Complex z]
  (let [aa (* 2 (.real z))
        bb (* 2 (.imag z))
        cc (+ (m/cos aa) (m/cosh bb))]
    (Complex. (/ (m/sin aa) cc)
              (/ (m/sinh bb) cc))))

(defn tanh
  "tanh"
  [^Complex z]
  (let [aa (* 2 (.real z))
        bb (* 2 (.imag z))
        cc (+ (m/cosh aa) (m/cos bb))]
    (Complex. (/ (m/sinh aa) cc)
              (/ (m/sin bb) cc))))


(defn exp
  "exp"
  [^Complex z]
  (let [e (m/exp (.real z))
        y (.imag z)]
    (Complex. (* e (m/cos y))
              (* e (m/sin y)))))

(defn log
  "log"
  [^Complex z]
  (Complex. (m/log (abs z))
            (m/atan2 (.imag z) (.real z))))

(defn acos
  "acos"
  [^Complex z]
  (->> (sqrt1z z)
       (mult I)
       (add z)
       (log)
       (mult I-)))

(defn asin
  "asin"
  [^Complex z]
  (->> (sqrt1z z)
       (add (mult I z))
       (log)
       (mult I-)))

(defn atan
  "atan"
  [^Complex z]
  (->> (sub I z)
       (div (add I z))
       (log)
       (mult (div I TWO ))))

(defn pow
  "power"
  [^Complex z1 ^Complex z2]
  (->> z1
       (log)
       (mult z2)
       (exp)))

(defn arg
  "argument"
  ^double [^Complex z]
  (m/atan2 (.imag z) (.real z)))
