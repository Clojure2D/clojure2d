;; # Namespace scope
;;
;; Functions to manipulate Vec2 as Complex numbers.
;; Implementation based on Apache Commons Math

(ns clojure2d.math.complex
  "Complex numbers functions. Based on Apache Commons Math.

  Complex number is represented as `Vec2` type (from [[clojure2d.math.vector]] namespace).

  To create complex number use [[complex]], [[vec2]] or [[->Vec2]].

  Graphs are generated with following snippet (where `f` is complex function).
  
  ```
  (defn generate-complex-graph
    [f canvas]
    (let [w (width canvas)
          h (height canvas)]
      (set-stroke canvas 1.5)
      (set-color canvas :white 60)
      (dotimes [x w]
        (dotimes [y h]
          (let [xx (m/norm x 0 w (- m/PI) m/PI)
                yy (m/norm y 0 h (- m/PI) m/PI)
                res (f (c/complex xx yy))
                resx (m/norm (res 0) (- m/PI) m/PI 0 w)
                resy (m/norm (res 1) (- m/PI) m/PI 0 h)]
            (point canvas resx resy))))))
  ```
  "
  {:categories {:trig "Trigonometry"
                :pow "Power / logarithm"}}
  (:require [clojure2d.math :as m]
            [clojure2d.math.vector :as v] 
            [metadoc.examples :refer :all])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const I (Vec2. 0.0 1.0))
(def ^:const I- (Vec2. 0.0 -1.0))
(def ^:const ONE (Vec2. 1.0 0.0))
(def ^:const TWO (Vec2. 2.0 0.0))
(def ^:const ZERO (Vec2. 0.0 0.0))

(defn complex
  "Create complex number. Represented as `Vec2`."
  {:examples [(example "New complex number." (complex 2 -1))]}
  [a b]
  (Vec2. a b))

(def ^{:doc "Absolute value" :examples [(example "Abs" (abs (complex 1 -3)))]} abs v/mag)
(def ^{:doc "Sum of two complex numbers." :examples [(example "Sum" (add I ONE))]} add v/add)
(def ^{:doc "Subtraction of two complex numbers." :examples [(example "Subtract" (sub ONE I-))]} sub v/sub)
(def ^{:doc "Argument (angle) of complex number." :examples [(example "Argument" (m/degrees (arg I-)))]} arg v/heading)

(defn conjugate
  "Complex conjugate. \\\\(\\bar{z}\\\\)"
  {:examples [(example "Conjugate" (conjugate I))]}
  [^Vec2 z]
  (Vec2. (.x z) (- (.y z))))

(defn div
  "Divide two complex numbers."
  {:examples [(example "Divide" (div (complex 1 2) (complex 3 4)))]}
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
  "\\\\(\\frac{1}{z}\\\\)"
  {:examples [(example "Reciprocal of real" (reciprocal TWO))
              (example "Reciprocal of complex" (reciprocal (complex 0 2)))]}
  [z]
  (div ONE z))

(defn mult
  "Multiply two complex numbers."
  {:examples [(example "Multiply" (mult (complex 1 2) (complex 3 4)))]}
  [^Vec2 z1 ^Vec2 z2]
  (let [a (.x z1)
        b (.y z1)
        c (.x z2)
        d (.y z2)]
    (Vec2. (- (* a c) (* b d))
           (+ (* a d) (* b c)))))

(defn neg
  "Negate complex number. \\\\(-z\\\\)"
  {:examples [(example "Negate." (neg (complex 1 2)))]}
  [z]
  (v/sub z))

(defn sq
  "Square complex number. \\\\(z^2\\\\)"
  {:examples [(example "Square." (sq (complex 1 2)))
              (example "\\\\(i^2\\\\)" (sq I))]}
  [z]
  (mult z z))

(defn sqrt
  "Sqrt of complex number. \\\\(\\sqrt{z}\\\\)"
  {:examples [(example "Square root of real." (sqrt (complex 2 0)))
              (example "Square root of complex." (sqrt (complex 2 2)))]}
  [^Vec2 z]
  (let [x (.x z)
        y (.y z)
        ^double l (abs z)
        xx (m/sqrt (+ l x))
        yy (* (m/signum y) (m/sqrt (- l x)))]
    (Vec2. (* m/SQRT2_2 xx) (* m/SQRT2_2 yy))))

(defn sqrt1z
  "\\\\(\\sqrt{1-z^2}\\\\)"
  {:examples [(example "Example 1" (sqrt1z (complex 2 3)))]}
  [z]
  (->> z
       (mult z)
       (sub ONE)
       (sqrt)))

(defn cos
  "cos"
  {:categories #{:trig}
   :examples [(example "cos(z)" (cos (complex 2 -1)))]}
  [^Vec2 z]
  (let [x (.x z)
        y (.y z)]
    (Vec2. (* (m/cos x) (m/cosh y))
           (* (- (m/sin x)) (m/sinh y)))))

(defn sin
  "sin"
  {:categories #{:trig}
   :examples [(example "sin(z)" (sin (complex 2 -1)))]}
  [^Vec2 z]
  (let [x (.x z)
        y (.y z)]
    (Vec2. (* (m/sin x) (m/cosh y))
           (* (m/cos x) (m/sinh y)))))

(defn cosh
  "cosh"
  {:categories #{:trig}
   :examples [(example "cosh(z)" (cosh (complex 2 -1)))]}
  [^Vec2 z]
  (let [x (.x z)
        y (.y z)]
    (Vec2. (* (m/cosh x) (m/cos y))
           (* (m/sinh x) (m/sin y)))))

(defn sinh
  "sinh"
  {:categories #{:trig}
   :examples [(example "sinh(z)" (sinh (complex 2 -1)))]}
  [^Vec2 z]
  (let [x (.x z)
        y (.y z)]
    (Vec2. (* (m/sinh x) (m/cos y))
           (* (m/cosh x) (m/sin y)))))

(defn tan
  "tan"
  {:categories #{:trig}
   :examples [(example "tan(z)" (tan (complex 2 -1)))]}
  [^Vec2 z]
  (let [aa (* 2.0 (.x z))
        bb (* 2.0 (.y z))
        cc (+ (m/cos aa) (m/cosh bb))]
    (Vec2. (/ (m/sin aa) cc)
           (/ (m/sinh bb) cc))))

(defn tanh
  "tanh"
  {:categories #{:trig}
   :examples [(example "tanh(z)" (tanh (complex 2 -1)))]}
  [^Vec2 z]
  (let [aa (* 2.0 (.x z))
        bb (* 2.0 (.y z))
        cc (+ (m/cosh aa) (m/cos bb))]
    (Vec2. (/ (m/sinh aa) cc)
           (/ (m/sin bb) cc))))

(defn sec
  "secant"
  {:categories #{:trig}
   :examples [(example "sec(z)" (sec (complex 2 -1)))]}
  [^Vec2 z]
  (let [cc (+ (m/cos (* 2.0 (.x z)))
              (m/cosh (* 2.0 (.y z))))]
    (Vec2. (/ (* 2.0 (m/cos (.x z)) (m/cosh (.y z))) cc)
           (/ (* 2.0 (m/sin (.x z)) (m/sinh (.y z))) cc))))

(defn csc
  "cosecant"
  {:categories #{:trig}
   :examples [(example "csc(z)" (csc (complex 2 -1)))]}
  [^Vec2 z]
  (let [cc (- (m/cos (* 2.0 (.x z)))
              (m/cosh (* 2.0 (.y z))))]
    (Vec2. (/ (* 2.0 (m/cosh (.y z)) (m/sin (.x z))) cc)
           (/ (* 2.0 (m/cos (.x z)) (m/sinh (.y z))) cc))))


(defn exp
  "exp"
  {:categories #{:pow}
   :examples [(example "exp(z)" (exp (complex 2 -1)))
              (example "\\\\(e^{i\\pi}+1\\\\)" (add (exp (complex 0 m/PI)) ONE))]}
  [^Vec2 z]
  (let [e (m/exp (.x z))
        y (.y z)]
    (Vec2. (* e (m/cos y))
           (* e (m/sin y)))))

(defn log
  "log"
  {:categories #{:pow}
   :examples [(example "log(z)" (log (complex 2 -1)))
              (example "log(e)" (log (complex m/E 0)))]}
  [^Vec2 z]
  (Vec2. (m/log (abs z))
         (m/atan2 (.y z) (.x z))))

(defn acos
  "acos"
  {:categories #{:trig}
   :examples [(example "acos(z)" (acos (complex 2 -1)))]}
  [z]
  (->> (sqrt1z z)
       (mult I)
       (add z)
       (log)
       (mult I-)))

(defn asin
  "asin"
  {:categories #{:trig}
   :examples [(example "asin(z)" (asin (complex 2 -1)))]}
  [z]
  (->> (sqrt1z z)
       (add (mult I z))
       (log)
       (mult I-)))

(defn atan
  "atan"
  {:categories #{:trig}
   :examples [(example "atan(z)" (atan (complex 2 -1)))]}
  [z]
  (->> (sub I z)
       (div (add I z))
       (log)
       (mult (div I TWO ))))

(defn pow
  "Power. \\\\(z_1^{z_2}\\\\)"
  {:categories #{:pow}
   :examples [(example "\\\\(\\sqrt{2}\\\\)" (pow TWO (complex 0.5 0.0)))
              (example "Complex power" (pow (complex 1 2) (complex 3 4)))]}
  [z1 z2]
  (->> z1
       (log)
       (mult z2)
       (exp)))

;;

;; (generate-graph-examples "c/" ".jpg" atan asin acos log exp csc sec tanh tan sinh sin cosh cos sqrt sq sqrt1z reciprocal)

;; (alter-docs)
