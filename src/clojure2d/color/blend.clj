;; http://www.pegtop.net/delphi/articles/blendmodes/
;; http://www.simplefilter.de/en/basics/mixmods.html

(ns clojure2d.color.blend
  "Blending modes functions for colors, gradients and palettes.

  Great collection of blending functions for color channel values. Channel values should be from `[0.0,255.0]` range.

  Use [[blend-colors]], [[blend-palettes]] and [[blend-gradients]] to blend colors, palettes and gradients.

  [[blend-colors]] uses simple alpha blending strategy as described [here](https://www.w3.org/TR/compositing-1/#blending).

  Blending can be done separately for each channel."
  (:refer-clojure :exclude [or and])
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [clojure2d.color :as c])
  (:import [fastmath.vector Vec4]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defmacro ^:private clamp255 [v] `(m/constrain ~v 0.0 255.0))
(defmacro ^:private mod255 [v] `(mod ~v 256.0))

(def ^:const ^:private ^double r255 (/ 255.0))
(def ^:const ^:private ^double r255-2 (/ (* 255.0 255.0)))
(def ^:const ^:private ^double rsqrt255 (/ (m/sqrt 255)))

(defn normal
  "Return second value only."
  ^double [^double _ ^double b] b)

(defn add
  "Add channel values (clamped)."
  ^double [^double a ^double b]
  (clamp255 (+ a b)))

(defn madd
  "Add channel values (mod 255)."
  ^double [^double a ^double b]
  (mod255 (+ a b)))

(defn subtract
  "Subtract channel values (clamped)."
  ^double [^double a ^double b]
  (clamp255 (- a b)))

(defn msubtract
  "Subtract channel values (mod 255)."
  ^double [^double a ^double b]
  (mod255 (- a b)))

(defn linearburn
  "Linear burn mode (clamped)."
  ^double [^double a ^double b]
  (clamp255 (- (+ a b) 255.0)))

(defn mlinearburn
  "Linear burn mode (mod 255)."
  ^double [^double a ^double b]
  (mod255 (- (+ a b) 255.0)))

(defn darken
  "Darken mode."
  ^double [^double a ^double b] 
  (min a b))

(defn lighten
  "Lighten mode."
  ^double [^double a ^double b]
  (max a b))

(defn multiply
  "Multiply channel values."
  ^double [^double a ^double b]
  (* a b r255))

(defn screen
  "Screen mode."
  ^double [^double a ^double b]
  (let [ra (- 255.0 a)
        rb (- 255.0 b)]
    (- 255.0 (* r255 rb ra ))))

(defn dodge
  "Dodge mode (clamped)."
  ^double [^double a ^double b]
  (clamp255 (/ (* 255.0 a) (max m/EPSILON (- 255.0 b)))))

(defn mdodge
  "Dodge mode (mod 255)."
  ^double [^double a ^double b]
  (mod255 (/ (* 255.0 a) (max m/EPSILON (- 255.0 b)))))

(defn inversedodge
  "Inverse dodge mode (clamped)."
  ^double [^double a ^double b]
  (clamp255 (/ (* 255.0 b) (max m/EPSILON (- 255.0 a)))))

(defn minversedodge
  "Inverse dodge mode (mod 255)."
  ^double [^double a ^double b]
  (mod255 (/ (* 255.0 b) (max m/EPSILON (- 255.0 a)))))

(defn softdodge
  "Soft dodge mode (clamped)."
  ^double [^double a ^double b]
  (if (<= (+ a b) 255.0)
    (clamp255 (/ (* 127.5 a) (max m/EPSILON (- 255.0 b))))
    (clamp255 (- 255.0 (* 127.5 (/ (- 255.0 b) (max m/EPSILON a)))))))

(defn msoftdodge
  "Soft dodge mode (mod 255)."
  ^double [^double a ^double b]
  (if (<= (+ a b) 255.0)
    (mod255 (/ (* 127.5 a) (max m/EPSILON (- 255.0 b))))
    (mod255 (- 255.0 (* 127.5 (/ (- 255.0 b) (max m/EPSILON a)))))))

(defn burn
  "Burn mode (clamped)."
  ^double [^double a ^double b]
  (clamp255 (* 255.0 (- 1.0 (/ (- 255.0 a) (max m/EPSILON b))))))

(defn mburn
  "Burn mode (mod 255)."
  ^double [^double a ^double b]
  (mod255 (* 255.0 (- 1.0 (/ (- 255.0 a) (max m/EPSILON b))))))

(defn inverseburn
  "Inverse burn mode (clamped)."
  ^double [^double a ^double b]
  (clamp255 (* 255.0 (- 1.0 (/ (- 255.0 b) (max m/EPSILON a))))))

(defn minverseburn
  "Inverse burn mode (mod 255)."
  ^double [^double a ^double b]
  (mod255 (* 255.0 (- 1.0 (/ (- 255.0 b) (max m/EPSILON a))))))

(defn softburn
  "Soft burn mode (clamped)."
  ^double [^double a ^double b]
  (if (<= (+ a b) 255.0)
    (clamp255 (* 127.5 (/ b (max m/EPSILON (- 255.0 a)))))
    (clamp255 (* 255.0 (- 0.5 (/ (- 255.0 a) (max m/EPSILON b)))))))

(defn msoftburn
  "Soft burn mode (mod 255)."
  ^double [^double a ^double b]
  (if (<= (+ a b) 255.0)
    (mod255 (* 127.5 (/ b (max m/EPSILON (- 255.0 a)))))
    (mod255 (* 255.0 (- 0.5 (/ (- 255.0 a) (max m/EPSILON b)))))))

(defn hardmix
  "Hard mix channel values."
  ^double [^double a ^double b]
  (let [t (- 255.0 b)]
    (if (< a t) 0.0 255.0)))

(defn linearlight
  "Linear light mode (clamped)."
  ^double [^double a ^double b]
  (clamp255 (- (+ b a a) 255.0)))

(defn mlinearlight
  "Linear light mode (mod 255)."
  ^double [^double a ^double b]
  (mod255 (- (+ b a a) 255.0)))

(defn stamp
  "Stamp mode (clamped)."
  ^double [^double a ^double b]
  (clamp255 (- (+ b b a) 255.0)))

(defn mstamp
  "Stamp mode (mod 255)."
  ^double [^double a ^double b]
  (mod255 (- (+ b b a) 255.0)))

(defn difference
  "Difference mode."
  ^double [^double a ^double b]
  (m/abs (- a b)))

(defn divide
  "Divide channel values (clamped)."
  ^double [^double a ^double b]
  (clamp255 (* 255.0 (/ a (max m/EPSILON b)))))

(defn mdivide
  "Divide channel values (mod 255)."
  ^double [^double a ^double b]
  (mod255 (* 255.0 (/ a (max m/EPSILON b)))))

(defn or
  "Bitwise `or` of channel values."
  ^double [^double a ^double b]
  (double (bit-and 0xff (bit-or (unchecked-long a) (unchecked-long b)))))

(defn and
  "Bitwise `and` of channel values."
  ^double [^double a ^double b]
  (double (bit-and 0xff (bit-and (unchecked-long a) (unchecked-long b)))))

(defn xor
  "Bitwise `xor` of channel values."
  ^double [^double a ^double b]
  (double (bit-and 0xff (bit-xor (unchecked-long a) (unchecked-long b)))))

(defn exclusion
  "Exclusion mode."
  ^double [^double a ^double b]
  (let [ab (* a b r255)]
    (- (+ a b) (+ ab ab))))

(defn pinlight
  "Pin light mode."
  ^double [^double a ^double b]
  (let [c (- (+ a a) 255.0)]
    (cond (< b c) c
          (clojure.core/and (<= c b) (< b (+ c 255.0))) b
          :else (+ c 255.0))))

(defn average
  "Average of two channel values."
  ^double [^double a ^double b]
  (/ (+ a b) 2.0))

(defn negation
  "Negation mode."
  ^double [^double a ^double b]
  (- 255.0 (m/abs (- 255.0 a b))))

(defn reflect
  "Reflect mode (clamped)."
  ^double [^double a ^double b]
  (clamp255 (/ (* a a) (max m/EPSILON (- 255.0 b)))))

(defn mreflect
  "Reflec mode (mod 255)."
  ^double [^double a ^double b]
  (mod255 (/ (* a a) (max m/EPSILON (- 255.0 b)))))

(defn glow
  "Glow mode (clamped)."
  ^double [^double a ^double b]
  (clamp255 (/ (* b b) (max m/EPSILON (- 255.0 a)))))

(defn mglow
  "Glow mode (mod 255)."
  ^double [^double a ^double b]
  (mod255 (/ (* b b) (max m/EPSILON (- 255.0 a)))))

(defn freeze
  "Freeze mode (clamped)."
  ^double [^double a ^double b]
  (clamp255 (- 255.0 (/ (m/sq (- 255.0 a)) (max m/EPSILON b)))))

(defn mfreeze
  "Freeze mode (mod 255)."
  ^double [^double a ^double b]
  (mod255 (- 255.0 (/ (m/sq (- 255.0 a)) (max m/EPSILON b)))))

(defn heat
  "Heat mode (clamped)."
  ^double [^double a ^double b]
  (clamp255 (- 255.0 (/ (m/sq (- 255.0 b)) (max m/EPSILON a)))))

(defn mheat
  "Heat mode (mod 255)."
  ^double [^double a ^double b]
  (mod255 (- 255.0 (/ (m/sq (- 255.0 b)) (max m/EPSILON a)))))

(defn overlay
  "Overlay mode."
  ^double [^double a ^double b]
  (if (< a 127.5)
    (* 2.0 (* a b r255))
    (- 255.0 (* 2.0 (* r255 (- 255.0 a) (- 255.0 b))))))

(defn hardlight
  "Hard light mode."
  ^double [^double a ^double b]
  (if (< b 127.5)
    (* 2.0 (* a b r255))
    (- 255.0 (* 2.0 (* r255 (- 255.0 a) (- 255.0 b))))))

(defn softlight
  "Soft light mode."
  ^double [^double a ^double b]
  (if (< b 127.5)
    (+ a (* (+ b b -255.0) (* a (- 255.0 a) r255-2)))
    (+ a (* (+ b b -255.0) (- (* (m/sqrt a) rsqrt255)
                              (* a r255))))))

(defn pegtoplight
  "Pegtop soft light mode."
  ^double [^double a ^double b]
  (+ (* 2.0  a b r255-2 (- 255.0 a))
     (* a a r255)))

(defn vividlight
  "Vivid light mode (clamped)."
  ^double [^double a ^double b]
  (if (< b 127.5)
    (clamp255 (- 255.0 (/ (* 255.0 (- 255.0 a)) (max m/EPSILON (+ b b)))))
    (clamp255 (/ (* 255.0 a) (max m/EPSILON (* 2.0 (- 255.0 b)))))))

(defn mvividlight
  "Vivid light mode (mod 255)."
  ^double [^double a ^double b]
  (if (< b 127.5)
    (mod255 (- 255.0 (/ (* 255.0 (- 255.0 a)) (max m/EPSILON (+ b b)))))
    (mod255 (/ (* 255.0 a) (max m/EPSILON (* 2.0 (- 255.0 b)))))))

(def blends {:normal normal
             :add add
             :madd madd
             :subtract subtract
             :msubtract msubtract
             :linearburn linearburn
             :mlinearburn mlinearburn
             :darken darken
             :lighten lighten
             :multiply multiply
             :screen screen
             :dodge dodge
             :mdodge mdodge
             :inversedodge inversedodge
             :minversedodge minversedodge
             :softdodge softdodge
             :msoftdodge msoftdodge
             :burn burn
             :mburn mburn
             :inverseburn inverseburn
             :minverseburn minverseburn
             :softburn softburn
             :msoftburn msoftburn
             :hardmix hardmix
             :linearlight linearlight
             :mlinearlight mlinearlight
             :stamp stamp
             :mstamp mstamp
             :difference difference
             :divide divide
             :mdivide mdivide
             :or or
             :and and
             :xor xor
             :exclusion exclusion
             :pinlight pinlight
             :average average
             :negation negation
             :reflect reflect
             :mreflect mreflect
             :glow glow
             :mglow mglow
             :freeze freeze
             :mfreeze mfreeze
             :heat heat
             :mheat mheat
             :overlay overlay
             :hardlight hardlight
             :softlight softlight
             :pegtoplight pegtoplight
             :vividlight vividlight
             :mvividlight mvividlight})

(def ^{:doc "List of all blending functions."} blends-list (sort (keys blends)))

(defn blend-colors
  "Blend two colors using simple alpha composing.

  Each channel can be blended using different function.

  Formula from [w3.org spec](https://www.w3.org/TR/compositing-1/#blending)."
  (^Vec4 [blend-fn cb cs] (blend-colors blend-fn blend-fn blend-fn cb cs))
  (^Vec4 [blend-fn1 blend-fn2 blend-fn3 cb cs]
   (let [cb (c/to-color cb)
         cs (c/to-color cs)
         ^double br (blend-fn1 (.x cb) (.x cs))
         ^double bg (blend-fn2 (.y cb) (.y cs))
         ^double bb (blend-fn3 (.z cb) (.z cs))]
     (if (clojure.core/and (>= (.w cb) 255.0)
                           (>= (.w cs) 255.0))
       (Vec4. br bg bb 255.0)
       (let [ab (* r255 (.w cb))
             as (* r255 (.w cs))
             a1 (* as (- 1.0 ab))
             a2 (* as ab)
             a3 (* (- 1.0 as) ab)
             a0 (+ as a3)]
         (Vec4. (/ (+ (* a1 (.x cs)) (* a2 br) (* a3 (.x cb))) a0)
                (/ (+ (* a1 (.y cs)) (* a2 bg) (* a3 (.y cb))) a0)
                (/ (+ (* a1 (.z cs)) (* a2 bb) (* a3 (.z cb))) a0)
                (* 255.0 a0)))))))

(defn blend-palettes
  "Blend two palettes.

  Each channel can be blended using different function."
  ([blend-fn pal1 pal2] (blend-palettes blend-fn blend-fn blend-fn pal1 pal2))
  ([blend-fn1 blend-fn2 blend-fn3 pal1 pal2] (mapv (partial blend-colors blend-fn1 blend-fn2 blend-fn3) pal1 pal2)))

(defn blend-gradients
  "Blend two gradients.
  
  Each channel can be blended using different function."
  ([blend-fn g1 g2] (blend-gradients blend-fn blend-fn blend-fn g1 g2))
  ([blend-fn1 blend-fn2 blend-fn3 g1 g2] (fn [^double t] (blend-colors blend-fn1 blend-fn2 blend-fn3 (g1 t) (g2 t)))))
