;; # Namespace scope
;;
;; Functions to manipulate Vec2 as Complex numbers.
;; Implementation based on Apache Commons Math

(ns clojure2d.math.complex
  (:require [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [primitive-math :as prim])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(prim/use-primitive-operators)

(def ^:const I (Vec2. 0.0 1.0))
(def ^:const I- (Vec2. 0.0 -1.0))
(def ^:const ONE (Vec2. 1.0 0.0))
(def ^:const TWO (Vec2. 2.0 0.0))
(def ^:const ZERO (Vec2. 0.0 0.0))

(def abs v/mag)
(def add v/add)
(def sub v/sub)
(def arg v/heading)

(defn conjugate
  "Conjugate"
  [^Vec2 z]
  (Vec2. (.x z) (- (.y z))))

(defn div
  "Divide two complex numbers"
  [^Vec2 z1 ^Vec2 z2]
  (let [a (.x z1)
        b (.y z1)
        c (.x z2)
        d (.y z2)
        den (+ (* c c) (* d d))]
    (if (zero? den)
      ZERO
      (Vec2. (/ (+ (* a c) (* b d)) den)
             (/ (- (* b c) (* a d)) den)))))

(defn reciprocal
  "1/z"
  [^Vec2 z]
  (div ONE z))

(defn mult
  "Multiply complex"
  [^Vec2 z1 ^Vec2 z2]
  (let [a (.x z1)
        b (.y z1)
        c (.x z2)
        d (.y z2)]
    (Vec2. (- (* a c) (* b d))
           (+ (* a d) (* b c)))))

(defn neg
  "Negate complex"
  [^Vec2 z]
  (Vec2. (- (.x z)) (- (.y z))))

(defn sq
  "Square"
  [z]
  (mult z z))

(defn sqrt
  "Sqrt of complex number"
  [^Vec2 z]
  (let [x (.x z)
        y (.y z)
        ^double l (abs z)
        xx (m/sqrt (+ l x))
        yy (* (m/signum y) (m/sqrt (- l x)))]
    (Vec2. (* m/SQRT2_2 xx) (* m/SQRT2_2 yy))))

(defn sqrt1z
  "Sqrt(1-z^2)"
  [z]
  (->> z
       (mult z)
       (sub ONE)
       (sqrt)))

(defn cos
  "cos"
  [^Vec2 z]
  (let [x (.x z)
        y (.y z)]
    (Vec2. (* (m/cos x) (m/cosh y))
           (* (- (m/sin x)) (m/sinh y)))))

(defn sin
  "sin"
  [^Vec2 z]
  (let [x (.x z)
        y (.y z)]
    (Vec2. (* (m/sin x) (m/cosh y))
           (* (m/cos x) (m/sinh y)))))

(defn cosh
  "cosh"
  [^Vec2 z]
  (let [x (.x z)
        y (.y z)]
    (Vec2. (* (m/cosh x) (m/cos y))
           (* (m/sinh x) (m/sin y)))))

(defn sinh
  "sinh"
  [^Vec2 z]
  (let [x (.x z)
        y (.y z)]
    (Vec2. (* (m/sinh x) (m/cos y))
           (* (m/cosh x) (m/sin y)))))

(defn tan
  "tan"
  [^Vec2 z]
  (let [aa (* 2.0 (.x z))
        bb (* 2.0 (.y z))
        cc (+ (m/cos aa) (m/cosh bb))]
    (Vec2. (/ (m/sin aa) cc)
           (/ (m/sinh bb) cc))))

(defn tanh
  "tanh"
  [^Vec2 z]
  (let [aa (* 2.0 (.x z))
        bb (* 2.0 (.y z))
        cc (+ (m/cosh aa) (m/cos bb))]
    (Vec2. (/ (m/sinh aa) cc)
           (/ (m/sin bb) cc))))

(defn sec
  "cosecant"
  [^Vec2 z]
  (let [cc (+ (m/cos (* 2.0 (.x z)))
              (m/cosh (* 2.0 (.y z))))]
    (Vec2. (/ (* 2.0 (m/cos (.x z)) (m/cosh (.y z))) cc)
           (/ (* 2.0 (m/sin (.x z)) (m/sinh (.y z))) cc))))

(defn csc
  "cosecant"
  [^Vec2 z]
  (let [cc (- (m/cos (* 2.0 (.x z)))
              (m/cosh (* 2.0 (.y z))))]
    (Vec2. (/ (* 2.0 (m/cosh (.y z)) (m/sin (.x z))) cc)
           (/ (* 2.0 (m/cos (.x z)) (m/sinh (.y z))) cc))))


(defn exp
  "exp"
  [^Vec2 z]
  (let [e (m/exp (.x z))
        y (.y z)]
    (Vec2. (* e (m/cos y))
           (* e (m/sin y)))))

(defn log
  "log"
  [^Vec2 z]
  (Vec2. (m/log (abs z))
         (m/atan2 (.y z) (.x z))))

(defn acos
  "acos"
  [z]
  (->> (sqrt1z z)
       (mult I)
       (add z)
       (log)
       (mult I-)))

(defn asin
  "asin"
  [z]
  (->> (sqrt1z z)
       (add (mult I z))
       (log)
       (mult I-)))

(defn atan
  "atan"
  [z]
  (->> (sub I z)
       (div (add I z))
       (log)
       (mult (div I TWO ))))

(defn pow
  "power"
  [z1 z2]
  (->> z1
       (log)
       (mult z2)
       (exp)))
