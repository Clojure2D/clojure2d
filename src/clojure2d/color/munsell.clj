;; Munsell Renotation System conversions, ported from colour-science (colour.notation.munsell).
;;
;; Copyright 2013 Colour Developers
;; The Munsell and Kubelka-Munk Toolbox: Copyright 2010-2018 Paul Centore (Gales Ferry, CT 06335, USA); used by permission.
;;
;; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(ns clojure2d.color.munsell
  "Munsell Renotation System: notation parsing, Munsell to `Yxy` conversion and Munsell value scales.

  A Munsell colour is written as hue, value and chroma, for example `4.2YR 8.1/5.3`, or as a neutral grey `N5`. Conversion interpolates the Munsell Renotation data (all, including extrapolated colours) as in Centore's algorithm, ported from colour-science (`colour.notation.munsell`, BSD-3-Clause, see the source header). The renotation data is defined under Illuminant C (CIE 1931 2 degree observer).

  Functions:

  - [[munsell-spec]] - parses and normalises a notation into a specification `[hue value chroma code]`.
  - [[munsell-to-Yxy]] - converts a Munsell colour to `Yxy` (`Y` in `0-100`), optionally adapted to another white point.
  - [[munsell-value]] and [[luminance]] - Munsell value from luminance `Y` and back, for several historical value scales.

  Hue letter codes follow colour-science: `B` 1, `BG` 2, `G` 3, `GY` 4, `Y` 5, `YR` 6, `R` 7, `RP` 8, `P` 9, `PB` 10."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.matrix :as mat]
            [fastmath.polynomials :as poly]
            [fastmath.solver :as solver]
            [fastmath.interpolation :as i]
            [clojure2d.color.whitepoints :as wp])
  (:import [fastmath.vector Vec2 Vec3 Vec4]
           [fastmath.matrix Mat3x3]))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

;; ## Value scales

(defn- astm-luminance
  "ASTM D1535 luminance factor (0-100) of Munsell value."
  ^double [^double V]
  (poly/mevalpoly V 0.0 1.1914 -0.22533 0.23352 -0.020484 0.00081939))

(defn- newhall-luminance
  "Newhall (1943) luminance factor relative to magnesium oxide."
  ^double [^double V]
  (poly/mevalpoly V 0.0 1.2219 -0.23111 0.23951 -0.021009 0.0008404))

(defn- osa-luminance
  "Newhall (1943) luminance scaled to the perfect diffuser (`Y(10)=100`)."
  ^double [^double V]
  (m// (newhall-luminance V) 1.02568))

(defn- mccamy-value
  ^double [^double Y]
  (if (m/<= Y 0.9)
    (m/* 0.87445 (m/pow Y 0.9967))
    (let [cY (m/cbrt Y)]
      (m/+ (m/- (m/* 2.49268 cY) 1.5614
                (m// 0.985 (m/+ (m/sq (m/- (m/* 0.1073 Y) 3.084)) 7.54)))
           (m// 0.0133 (m/pow Y 2.3))
           (m/* 0.0084 (m/sin (m/+ (m/* 4.1 cY) 1.0)))
           (m/* (m// 0.0221 Y) (m/sin (m/* 0.39 (m/- Y 2.0))))
           (m/- (m/* (m// 0.0037 (m/* 0.44 Y)) (m/sin (m/* 1.28 (m/- Y 0.53)))))))))

(defn- root
  "Root of the increasing function `f` on `[lo, hi]` shifted by `target`."
  ^double [f ^double target ^double lo ^double hi]
  (solver/find-root (fn ^double [^double x] (m/- ^double (f x) target)) lo hi))

(def ^:private mccamy-luminance-10
  (delay (root mccamy-value 10.0 0.0 200.0)))

(def ^:private scales
  "Scale keyword -> [Y->V V->Y] functions."
  {:priest [(fn ^double [^double Y] (m/* 10.0 (m/sqrt (m// Y 100.0))))
            (fn ^double [^double V] (m/sq V))]
   :munsell [(fn ^double [^double Y] (m/sqrt (m/- (m/* 1.4742 Y) (m/* 0.004743 Y Y))))
             (fn ^double [^double V] (.x ^Vec2 (solver/quadratic 0.004743 -1.4742 (m/sq V))))]
   :moon [(fn ^double [^double Y] (m/* 1.4 (m/pow Y 0.426)))
          (fn ^double [^double V] (m/pow (m// V 1.4) (m// 0.426)))]
   :saunderson [(fn ^double [^double Y] (m/- (m/* 2.357 (m/pow Y 0.343)) 1.52))
                (fn ^double [^double V] (m/pow (m// (m/+ V 1.52) 2.357) (m// 0.343)))]
   :ladd [(fn ^double [^double Y] (m/- (m/* 2.468 (m/cbrt Y)) 1.636))
          (fn ^double [^double V] (m/cb (m// (m/+ V 1.636) 2.468)))]
   :mccamy [mccamy-value
            (fn ^double [^double V] (root mccamy-value V 0.0 @mccamy-luminance-10))]
   :astm [(fn ^double [^double Y] (root astm-luminance Y 0.0 10.0))
          astm-luminance]
   :newhall [(fn ^double [^double Y] (root newhall-luminance Y 0.0 10.0))
             newhall-luminance]
   :osa [(fn ^double [^double Y] (root osa-luminance Y 0.0 10.0))
         osa-luminance]})

(defn- scale-fns
  [scale]
  (or (scales scale)
      (throw (ex-info (str "Unknown Munsell value scale: " scale) {:scale scale}))))

(def ^:private luminance-domains
  "Scale keyword -> [Y(0) Y(10)]."
  (delay (into {} (for [[k [_ v->y]] scales]
                    [k [(v->y 0.0) (v->y 10.0)]]))))

(defn luminance
  "Luminance factor `Y` of a Munsell value.

  Parameters:

  - `V` (double): Munsell value, from `0` (ideal black) to `10` (ideal white).
  - `scale` (keyword, optional): value scale, default `:astm`.

  Scales: `:astm` (ASTM D1535-08), `:newhall` (Newhall, Nickerson and Judd 1943, relative to magnesium oxide, `Y(10)` is about `102.57`), `:osa` (Newhall 1943 scaled to the perfect diffuser), `:priest` (Priest, Gibson and MacNicholas 1920), `:munsell` (Munsell, Sloan and Godlove 1933), `:moon` (Moon and Spencer 1943), `:saunderson` (Saunderson and Milner 1944), `:ladd` (Ladd and Pinney 1955), `:mccamy` (McCamy 1987).

  Returns `Y` on the `0-100` scale of the chosen method. For `:saunderson` and `:ladd` the luminance of value `0` is positive (about `0.28` and `0.29`).

  Throws `ex-info` for `V` outside `[0, 10]` and for an unknown scale.

  See also [[munsell-value]] (inverse)."
  (^double [^double V] (luminance V :astm))
  (^double [^double V scale]
   (let [[_ v->y] (scale-fns scale)]
     (when-not (m/<= 0.0 V 10.0)
       (throw (ex-info "Munsell value outside [0, 10]." {:scale scale :value V})))
     (v->y V))))

(defn munsell-value
  "Munsell value of a luminance factor `Y`.

  Parameters:

  - `Y` (double): luminance factor on the `0-100` scale.
  - `scale` (keyword, optional): value scale, default `:astm`; see [[luminance]] for the list.

  Returns the Munsell value `V` from `[0, 10]`.

  `Y` must lie between the luminance of value `0` and value `10` of the chosen scale (see [[luminance]]), so for example `(munsell-value 0.0 :saunderson)` is outside the domain. Throws `ex-info` outside the domain and for an unknown scale.

  See also [[luminance]] (inverse)."
  (^double [^double Y] (munsell-value Y :astm))
  (^double [^double Y scale]
   (let [[y->v] (scale-fns scale)
         [^double lo ^double hi] (@luminance-domains scale)]
     (when-not (m/<= lo Y hi)
       (throw (ex-info "Luminance outside the domain of the Munsell value scale." {:scale scale :value Y :min lo :max hi})))
     (y->v Y))))

;; ## Notation

(def ^:private FP "[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?")
(def ^:private GREY-PATTERN (re-pattern (str "(?i)^N(" FP ")")))
(def ^:private COLOUR-PATTERN
  (re-pattern (str "(?i)^(" FP ")\\s*(BG|GY|YR|RP|PB|B|G|Y|R|P)\\s*(" FP ")\\s*/\\s*([-+]?" FP ")")))

(def ^:private letter->code {"B" 1.0 "BG" 2.0 "G" 3.0 "GY" 4.0 "Y" 5.0 "YR" 6.0 "R" 7.0 "RP" 8.0 "P" 9.0 "PB" 10.0})

(defn- parse-notation
  ^Vec4 [^String notation]
  (if-let [[_ value] (re-find GREY-PATTERN notation)]
    (Vec4. ##NaN (parse-double value) ##NaN ##NaN)
    (if-let [[_ hue letter value chroma] (re-find COLOUR-PATTERN notation)]
      (Vec4. (parse-double hue) (parse-double value) (parse-double chroma) (letter->code (str/upper-case letter)))
      (throw (ex-info (str "Invalid Munsell notation: " notation) {:notation notation})))))

(defn- next-code
  "Letter code of the next (clockwise) hue letter, in `1..10`."
  ^double [^double code]
  (let [c (m/mod (m/inc code) 10.0)]
    (if (m/zero? c) 10.0 c)))

(defn- grey?
  [^Vec4 spec]
  (m/nan? (.x spec)))

(defn- normalise
  ^Vec4 [^Vec4 spec]
  (let [value (.y spec)]
    (when-not (m/<= 0.0 value 10.0)
      (throw (ex-info "Munsell value outside [0, 10]." {:value value})))
    (if (or (grey? spec) (m/nan? (.z spec)))
      (Vec4. ##NaN value ##NaN ##NaN)
      (let [hue (.x spec) chroma (.z spec) code (.w spec)]
        (when-not (m/<= 0.0 hue 10.0)
          (throw (ex-info "Munsell hue outside [0, 10]." {:hue hue})))
        (when-not (and (m/<= 1.0 code 10.0) (m/== code (m/rint code)))
          (throw (ex-info "Munsell hue letter code outside 1..10." {:code code})))
        (when (m/neg? chroma)
          (throw (ex-info "Negative Munsell chroma." {:chroma chroma})))
        (cond
          (m/zero? chroma) (Vec4. ##NaN value ##NaN ##NaN)
          (m/zero? hue) (Vec4. 10.0 value chroma (next-code code))
          :else spec)))))

(defn munsell-spec
  "Normalised Munsell specification of a notation or specification.

  Parameters:

  - `munsell`: a notation string, for example `4.2YR 8.1/5.3`, `5r 5/2`, `N5` (case insensitive, spaces allowed between parts), or a specification `[hue value chroma code]` (a `Vec4` or a sequence of four numbers), with greys as `[NaN value NaN NaN]`.

  Returns a `Vec4` `[hue value chroma code]` with hue in `(0, 10]` and letter code in `1..10` (see the namespace docstring). A hue of `0` is expressed as `10` of the previous letter (`0YR` is `10R`, `0P` is `10PB`). Greys, including any colour with chroma `0`, are returned as `[NaN value NaN NaN]`.

  Throws `ex-info` for a string which is not a Munsell notation, for value outside `[0, 10]`, hue outside `[0, 10]`, letter code outside `1..10` and negative chroma.

  See also [[munsell-to-Yxy]]."
  ^Vec4 [munsell]
  (normalise (cond
               (string? munsell) (parse-notation munsell)
               (instance? Vec4 munsell) munsell
               (and (sequential? munsell) (== 4 (count munsell))) (let [[h value c code] munsell]
                                                                    (Vec4. (double h) (double value) (double c) (double code)))
               :else (throw (ex-info "Munsell colour must be a notation string or a specification [hue value chroma code]."
                                     {:type (type munsell)})))))

;; ## Renotation data

(def ^{:private true :const true :tag 'long} VALUES 9)
(def ^{:private true :const true :tag 'long} CHROMAS 25)
(def ^{:private true :const true :tag 'long} SEGMENTS 40)

(def ^:private renotation
  (delay (let [d (-> (io/resource "color/munsell_renotation.edn") io/input-stream slurp edn/read-string)]
           {:xy (double-array (:xy d))
            :max-chroma (double-array (:max-chroma d))
            :radial (boolean-array (map pos? (:radial d)))})))

(def ^:private ^Vec2 xy-C (Vec2. (.z ^Vec4 wp/CIE-2-C) (.w ^Vec4 wp/CIE-2-C)))

(def ^{:private true :const true :tag 'double} HUE-THRESHOLD 0.001)

(defn- astm-hue
  "ASTM hue in `(0, 100]`."
  ^double [^double hue ^double code]
  (let [h (m/+ (m/* 10.0 (m/mod (m/- 7.0 code) 10.0)) hue)]
    (if (m/zero? h) 100.0 h)))

(defn- hue-index
  "Index of a standard hue (multiple of 2.5) in the renotation arrays."
  ^long [^double hue ^double code]
  (m/dec (m/round (m// (astm-hue hue code) 2.5))))

(def ^:private hue-angle-interpolator
  (i/linear [0.0 2.0 3.0 4.0 5.0 6.0 8.0 9.0 10.0] [0.0 45.0 70.0 135.0 160.0 225.0 255.0 315.0 360.0]))

(defn- hue-angle
  ^double [^double hue ^double code]
  (hue-angle-interpolator (m/mod (m/- (m/+ (m/mod (m/- 17.0 code) 10.0) (m// hue 10.0)) 0.5) 10.0)))

(defn- bounding-hues
  "Clockwise and counter-clockwise standard hues around `hue`: `[hue-cw code-cw hue-ccw code-ccw]`."
  ^Vec4 [^double hue ^double code]
  (if (m/zero? (m/mod hue 2.5))
    (if (m/zero? hue)
      (let [c (next-code code)] (Vec4. 10.0 c 10.0 c))
      (Vec4. hue code hue code))
    (let [hue-cw (m/* 2.5 (m/floor (m// hue 2.5)))
          hue-ccw (m/mod (m/+ hue-cw 2.5) 10.0)]
      (Vec4. (if (m/zero? hue-cw) 10.0 hue-cw)
             (if (m/zero? hue-cw) (next-code code) code)
             (if (m/zero? hue-ccw) 10.0 hue-ccw)
             code))))

(defn- renotation-xy
  "Renotation `xy` of a standard hue, integer value `1..9` and even chroma `2..50`."
  ^Vec2 [^double hue ^long value ^long chroma ^double code]
  (let [^doubles xy (:xy @renotation)
        idx (m/* 2 (m/+ (m/* (m/+ (m/* (hue-index hue code) VALUES) (m/dec value)) CHROMAS)
                        (m/dec (m/quot chroma 2))))
        x (aget xy idx)]
    (when (m/nan? x)
      (throw (ex-info "Specification is not in the Munsell renotation data." {:hue hue :value value :chroma chroma :code code})))
    (Vec2. x (aget xy (m/inc idx)))))

(defn- radial?
  "Interpolation method on the renotation ovoid for a non standard hue."
  [^double hue ^long value ^long chroma ^double code]
  (let [segment (long (m/floor (m// (astm-hue hue code) 2.5)))]
    (aget ^booleans (:radial @renotation)
          (m/+ (m/* (m/+ (m/* (m/dec value) CHROMAS) (m/dec (m/quot chroma 2))) SEGMENTS) segment))))

(defn- ovoid-xy
  "`xy` of a hue on the ovoid of an integer value `1..9` and even chroma `2..50`."
  ^Vec2 [^double hue ^long value ^long chroma ^double code]
  (let [snapped (m/* 2.5 (m/rint (m// hue 2.5)))]
    (if (m/< (m/abs (m/- hue snapped)) HUE-THRESHOLD)
      (if (m/zero? snapped)
        (renotation-xy 10.0 value chroma (next-code code))
        (renotation-xy snapped value chroma code))
      (let [^Vec4 bounds (bounding-hues hue code)
            ^Vec2 xy-minus (renotation-xy (.x bounds) value chroma (.y bounds))
            ^Vec2 xy-plus (renotation-xy (.z bounds) value chroma (.w bounds))
            ^Vec2 polar-minus (v/to-polar (v/sub xy-minus xy-C))
            ^Vec2 polar-plus (v/to-polar (v/sub xy-plus xy-C))
            phi-minus (m/degrees (.y polar-minus))
            phi-plus (let [p (m/degrees (.y polar-plus))]
                       (if (m/> (m/- phi-minus p) 180.0) (m/+ p 360.0) p))
            angle-upper (hue-angle (.z bounds) (.w bounds))
            angle (hue-angle hue code)
            angle-lower (let [a (hue-angle (.x bounds) (.y bounds))]
                          (if (m/zero? a) 360.0 a))
            shift? (m/> angle-lower angle-upper)
            angle (if (and shift? (m/<= angle-lower angle)) (m/- angle 360.0) angle)
            angle-lower (if shift? (m/- angle-lower 360.0) angle-lower)]
        (if (radial? hue value chroma code)
          (let [rho (m/norm angle angle-lower angle-upper (.x polar-minus) (.x polar-plus))
                phi (m/norm angle angle-lower angle-upper phi-minus phi-plus)]
            (v/add xy-C (v/from-polar (Vec2. rho (m/radians phi)))))
          (Vec2. (m/norm angle angle-lower angle-upper (.x xy-minus) (.x xy-plus))
                 (m/norm angle angle-lower angle-upper (.y xy-minus) (.y xy-plus))))))))

(defn- integer-value-xy
  "`xy` of a chromatic specification with integer value `1..9`."
  ^Vec2 [^double hue ^long value ^double chroma ^double code]
  (let [even? (m/zero? (m/mod chroma 2.0))
        chroma-minus (if even? (long chroma) (m/* 2 (long (m/floor (m// chroma 2.0)))))
        chroma-plus (if even? chroma-minus (m/+ chroma-minus 2))
        ^Vec2 xy-minus (if (m/zero? chroma-minus) xy-C (ovoid-xy hue value chroma-minus code))]
    (if (m/== chroma-minus chroma-plus)
      xy-minus
      (let [^Vec2 xy-plus (ovoid-xy hue value chroma-plus code)]
        (Vec2. (m/norm chroma chroma-minus chroma-plus (.x xy-minus) (.x xy-plus))
               (m/norm chroma chroma-minus chroma-plus (.y xy-minus) (.y xy-plus)))))))

(defn- maximum-chroma
  "Largest chroma covered by the renotation data for a normalised hue (`(0, 10]`), value (`[0, 10]`) and letter code (`1..10`).

  The smallest maximum over the bounding standard hues and integer values; falls linearly in luminance to `0` between values `9` and `10` and is `0` from `9.99`. Below value `1` it is `0` by design: the renotation rows at values `0.2-0.8` are not used, so such colours are greys."
  ^double [^double hue ^double value ^double code]
  (if (or (m/>= value 9.99) (m/< value 1.0))
    0.0
    (let [^doubles max-chroma (:max-chroma @renotation)
          ^Vec4 bounds (bounding-hues hue code)
          k-cw (m/* (hue-index (.x bounds) (.y bounds)) VALUES)
          k-ccw (m/* (hue-index (.z bounds) (.w bounds)) VALUES)
          value-minus (long (m/floor value))
          value-plus (if (m/== value (m/floor value)) value-minus (m/inc value-minus))
          cw-minus (aget max-chroma (m/+ k-cw (m/dec value-minus)))
          ccw-minus (aget max-chroma (m/+ k-ccw (m/dec value-minus)))]
      (if (m/<= value-plus 9)
        (m/min cw-minus ccw-minus
               (aget max-chroma (m/+ k-cw (m/dec value-plus)))
               (aget max-chroma (m/+ k-ccw (m/dec value-plus))))
        (let [L (astm-luminance value)
              L9 (astm-luminance 9.0)
              L10 (astm-luminance 10.0)]
          (m/min (m/norm L L9 L10 cw-minus 0.0)
                 (m/norm L L9 L10 ccw-minus 0.0)))))))

(def ^{:private true :const true :tag 'double} VALUE-THRESHOLD 0.001)

(defn- chromatic-xy
  "`xy` of a chromatic specification under Illuminant C, interpolated in luminance between integer values."
  ^Vec2 [^Vec4 spec]
  (let [hue (.x spec) value (.y spec) chroma (.z spec) code (.w spec)
        rounded (m/rint value)]
    (if (m/<= (m/abs (m/- value rounded)) VALUE-THRESHOLD)
      (if (m/== rounded 10.0) xy-C (integer-value-xy hue (long rounded) chroma code))
      (let [value-minus (long (m/floor value))
            value-plus (m/inc value-minus)
            ^Vec2 xy-minus (integer-value-xy hue value-minus chroma code)
            ^Vec2 xy-plus (if (m/== value-plus 10) xy-C (integer-value-xy hue value-plus chroma code))
            Y (astm-luminance value)
            Y-minus (astm-luminance value-minus)
            Y-plus (astm-luminance value-plus)]
        (Vec2. (m/norm Y Y-minus Y-plus (.x xy-minus) (.x xy-plus))
               (m/norm Y Y-minus Y-plus (.y xy-minus) (.y xy-plus)))))))

(defn- clamp-chroma
  ^Vec4 [^Vec4 spec]
  (if (grey? spec)
    spec
    (let [chroma (m/min (.z spec) (maximum-chroma (.x spec) (.y spec) (.w spec)))]
      (if (m/zero? chroma)
        (Vec4. ##NaN (.y spec) ##NaN ##NaN)
        (Vec4. (.x spec) (.y spec) chroma (.w spec))))))

;; ## Conversion

(def ^:private adaptation-matrix
  (memoize (fn [adaptation whitepoint]
             (wp/chromatic-adaptation-matrix adaptation wp/CIE-2-C whitepoint))))

(defn- adapt
  "Chromatic adaptation of `xyY` (`Vec3`, `Y` in 0-100) from Illuminant C, returned as `Yxy`."
  ^Vec4 [^Vec3 xyY ^Mat3x3 M ^Vec4 whitepoint]
  (let [Y (.z xyY)]
    (if (m/zero? Y)
      (Vec4. 0.0 (.z whitepoint) (.w whitepoint) 255.0)
      (let [x (.x xyY) y (.y xyY)
            Yy (m// Y y)
            ^Vec3 XYZ (mat/mulv M (Vec3. (m/* x Yy) Y (m/* (m/- 1.0 x y) Yy)))
            s (m/+ (.x XYZ) (.y XYZ) (.z XYZ))]
        (Vec4. (.y XYZ) (m// (.x XYZ) s) (m// (.y XYZ) s) 255.0)))))

(defn munsell-to-Yxy
  "Converts a Munsell colour to `Yxy`.

  The chromaticity is interpolated from the Munsell Renotation data, which is defined under Illuminant C. Chroma above the largest chroma of the renotation data for the hue and value is clamped to that maximum. Below value `1` the maximum is `0` by design (the renotation rows at values `0.2-0.8` are not used), so chromatic colours there become greys of the same value.

  Parameters:

  - `munsell`: notation string or specification, see [[munsell-spec]].
  - `options` (map, optional):
      - `:whitepoint` - white point (a `Vec4` from `clojure2d.color.whitepoints`, for example `wp/CIE-2-D65`) of the result, default `wp/CIE-2-C` (no adaptation).
      - `:adaptation` - chromatic adaptation method used for a different white point, default `:bradford`; see `clojure2d.color.whitepoints/chromatic-adaptation-matrix`.
      - `:scale` - Munsell value scale of the returned `Y`, default `:astm`; see [[luminance]]. Chromaticity always follows the ASTM D1535 scale of the renotation data.

  Returns a `Vec4` `[Y x y 255.0]` with `Y` in `0-100` (up to about `102.57` for the `:newhall` scale). Greys and black (`Y` equal `0`) get the chromaticity of the white point.

  Throws `ex-info` for an invalid notation or specification, see [[munsell-spec]].

  See also [[munsell-spec]], [[luminance]]."
  (^Vec4 [munsell] (munsell-to-Yxy munsell nil))
  (^Vec4 [munsell {:keys [whitepoint adaptation scale]
                   :or {whitepoint wp/CIE-2-C adaptation :bradford scale :astm}}]
   (let [spec (clamp-chroma (munsell-spec munsell))
         ^Vec2 xy (if (grey? spec) xy-C (chromatic-xy spec))
         Y (luminance (.y spec) scale)]
     (if (= whitepoint wp/CIE-2-C)
       (Vec4. Y (.x xy) (.y xy) 255.0)
       (adapt (Vec3. (.x xy) (.y xy) Y) (adaptation-matrix adaptation whitepoint) whitepoint)))))
