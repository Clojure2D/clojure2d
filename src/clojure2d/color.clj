;; # Namespace scope
;;
;; Color management functions:
;;
;; * Color is represented by Vec3, Vec4 or java.awt.Color
;; * Blending functions
;; * Color space converters
;; * Palette collections: 200 5-color palettes from colourlovers, paletton colors generator, Inigo Quilez palette generator
;; * Nearest color filter
;;
;; Generally color is represented by Vec4 containing R,G,B,A values from 0 to 255. Three conversion functions are defined by `ColorProto` which extends Vec3, Vec4, Keyword and java.awt.Color types.
;; Functions are:
;;
;; * `to-color` to get Vec4 object
;; * `to-awt-color` to get `java.awt.Color` object
;; * `to-luma` to get brightness as `double`
;;
;; `Vec4` is clojure2d color representation.
;; Keyword can be used when representing one of 140 html/css colornames (https://www.w3schools.com/colors/colors_names.asp). Eg. :linen
;;

(ns clojure2d.color
  (:require [clojure.xml :as xml]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v]
            [clojure.java.io :refer :all])
  (:import [clojure2d.math.vector Vec3 Vec4]           
           java.awt.Color))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; ## Clamping functions

;; First define some clamping functions

(defn clamp255
  "Clamp to 0-255 integer"
  ^long [^double a]
  (m/constrain (m/round a) 0 255))

(defn mod255
  "Leave 8 bits from long. Wraps input to 0-255 integer"
  ^long [^long a]
  (bit-and 0xff a))

(defmacro clamp1
  "Clamp to 0.0-1.0"
  [v]
  `(m/constrain ~v 0.0 1.0))

(defmacro mod1
  "Cut to 0.0-1.0"
  [v]
  `(m/frac ~v))

;; ## Color representation

;; Define `ColorProto` for representation conversions.
(defprotocol ColorProto
  (to-color [c])
  (to-awt-color [c]) 
  (to-luma [c]))

(defn- to-luma-fn
  "Local luma conversion function"
  ^double [^double r ^double g ^double b]
  (+ (* 0.212671 r)
     (* 0.715160 g)
     (* 0.072169 b)))

(declare html-awt-color)
(declare html-color)

;; Equip `Vec3`, `Vec4`, `Keyword` and `java.awt.Color` types with `ColorProto` functions.
;; For keyword use one of 140 names from html/css palettes (list: https://www.w3schools.com/colors/colors_names.asp)
(extend-protocol ColorProto
  Vec3
  (to-color [^Vec3 c]
    (Vec4. (.x c) (.y c) (.z c) 255))
  (to-awt-color [^Vec3 c]
    (Color. (clamp255 (.x c))
            (clamp255 (.y c))
            (clamp255 (.z c))))
  (to-luma ^double [^Vec3 c] (to-luma-fn (.x c) (.y c) (.z c)))
  Vec4
  (to-color [c] c)
  (to-awt-color [^Vec4 c]
    (Color.  (clamp255 (.x c))
             (clamp255 (.y c))
             (clamp255 (.z c))
             (clamp255 (.w c))))
  (to-luma ^double [^Vec4 c] (to-luma-fn (.x c) (.y c) (.z c)))
  clojure.lang.Keyword
  (to-color [n] (html-color n))
  (to-awt-color [n] (html-awt-color n))
  (to-luma [n] (to-luma (html-color n)))
  Color
  (to-color [^Color c]
    (Vec4. (.getRed c)
           (.getGreen c)
           (.getBlue c)
           (.getAlpha c)))
  (to-awt-color [c] c)
  (to-luma ^double [^Color c] (to-luma-fn (.getRed c) (.getGreen c) (.getBlue c)))
  nil
  (to-color [_] nil)
  (to-awt-color [_] nil))

(defn set-alpha
  "Set alpha channel and return `Vec4` representation."
  [c a]
  (let [^Vec4 v (to-color c)]
    (Vec4. (.x v) (.y v) (.z v) a)))

(defn set-awt-alpha
  "Set alpha channel and return `Color` representation."
  [c a]
  (let [^Color cc (to-awt-color c)]
    (Color. (.getRed cc)
            (.getGreen cc)
            (.getBlue cc)
            (clamp255 a))))

(defn make-awt-color
  "Create java.awt.Color object. Use with `core/set-awt-color` or `core/set-awt-background`."
  ([c]
   (to-awt-color c))
  ([c a]
   (set-awt-alpha c a))
  ([r g b]
   (Color. (clamp255 r)
           (clamp255 g)
           (clamp255 b)))
  ([r g b a]
   (Color. (clamp255 r)
           (clamp255 g)
           (clamp255 b)
           (clamp255 a))))

(defn make-color
  "Create Vec4 object as color representation. Use with `core/set-color` or `core/set-background`."
  ([c]
   (to-color c))
  ([c a]
   (set-alpha c a))
  ([r g b]
   (Vec4. (clamp255 r)
          (clamp255 g)
          (clamp255 b)
          255.0))
  ([r g b a]
   (Vec4. (clamp255 r)
          (clamp255 g)
          (clamp255 b)
          (clamp255 a))))

(declare to-HSB)

(defn get-hue
  "Get hue value from color (any representation). Based on HSB colorspace."
  ^double [c]
  (let [^Vec4 ret (to-HSB (to-color c))]
    (.x ret)))

;; ## Blending / Composing

;; Several color blending / composing functions. Used to compose two images (`Pixels`). See `core.pixels` namespace for filters.

;; Some blending functions require additional parameter. You can set it with following variable.
(def ^:dynamic ^double *blend-threshold* 0.5)

;; Prepare look-up table for int->double conversion.
(def r255 (double-array (map #(/ ^double % 255.0) (range 256))))

(defn get-r255 
  "Return color value (0-1) by index"
  ^double [^long idx]
  (aget ^doubles r255 idx))

;; Blend colors functions

(defn blend-values
  "Blend channel values with blending function."
  [f a b]
  (let [aa (get-r255 a)
        bb (get-r255 b)]
    (* 255.0 ^double (f aa bb))))

(defn blend-colors
  "Blend colors with blending function. Do not blend alpha on default."
  (^Vec4 [f c1 c2 alpha?]
   (let [^Vec4 cc1 (to-color c1)
         ^Vec4 cc2 (to-color c2)]
     (Vec4. (blend-values f (.x cc1) (.x cc2))
            (blend-values f (.y cc1) (.y cc2))
            (blend-values f (.z cc1) (.z cc2))
            (if alpha?
              (blend-values f (.w cc1) (.w cc2))
              (.w cc1)))))
  (^Vec4 [f c1 c2] (blend-colors c1 c2 false)))

;; Plenty of blending functions. Bleding functions operate on 0.0-1.0 values and return new value in the same range.

(defn blend-none
  "Return first value only. Do nothing."
  ^double [a b] a)

(defn blend-add
  "Add"
  ^double [^double a ^double b]
  (clamp1 (+ a b)))

(defn blend-madd
  "Modulus add"
  ^double [^double a ^double b]
  (mod1 (+ a b)))

(defn blend-subtract
  "Subtract"
  ^double [^double a ^double b]
  (clamp1 (- a b)))

(defn blend-msubtract
  "Modulus subtract"
  ^double [^double a ^double b]
  (mod1 (- a b)))

(defn blend-linearburn
  "Linear burn"
  ^double [^double a ^double b]
  (clamp1 (dec (+ a b))))

(defn blend-mlinearburn
  "Modulus linear burn"
  ^double [^double a ^double b]
  (mod1 (dec (+ a b))))

(defn blend-darken
  "Darken"
  ^double [^double a ^double b] 
  (min a b))

(defn blend-lighten
  "Lighten"
  ^double [^double a ^double b]
  (max a b))

(defn blend-multiply
  "Multiply"
  ^double [^double a ^double b]
  (* a b))

(defn blend-screen
  "Screen"
  ^double [^double a ^double b]
  (let [ra (- 1.0 a)
        rb (- 1.0 b)]
    (- 1.0 (* rb ra))))

(defn blend-dodge
  "Dodge"
  ^double [^double a ^double b]
  (clamp1 (/ a (- 1.0 b))))

(defn blend-mdodge
  "Modulus dodge"
  ^double [^double a ^double b]
  (mod1 (/ a (max 0.0001 (- 1.0 b)))))

(defn blend-burn
  "Burn"
  ^double [^double a ^double b]
  (clamp1 (- 1.0 (/ (- 1.0 a) b))))

(defn blend-mburn
  "Modulus burn"
  ^double [^double a ^double b]
  (mod1 (- 1.0 (/ (- 1.0 a) (max 0.0001 b)))))

(defn blend-hardmix
  "Hard mix"
  ^double [^double a ^double b]
  (let [t (- 1.0 b)]
    (cond (< a t) 0.0
          (> a t) 1.0
          :else a)))

(defn blend-linearlight
  "Linear light"
  ^double [^double a ^double b]
  (clamp1 (-> b
              (+ a)
              (+ a)
              (- 1.0))))

(defn blend-mlinearlight
  "Modulus linear light"
  ^double [^double a ^double b]
  (mod1 (-> b
            (+ a)
            (+ a)
            (- 1.0))))

(defn blend-pegtoplight
  "Pegtop light"
  ^double [^double a ^double b]
  (let [ab (* a b)]
    (clamp1 (->> b
                 (- 1.0)
                 (* a a)
                 (+ ab)
                 (+ ab)))))

(defn blend-mpegtoplight
  "Modulus pegtop light"
  ^double [^double a ^double b]
  (let [ab (* a b)]
    (mod1 (->> b
               (- 1.0)
               (* a a)
               (+ ab)
               (+ ab)))))

(defn blend-difference
  "Difference"
  ^double [^double a ^double b]
  (m/abs (- a b)))

(defn blend-divide
  "Divide"
  ^double [^double a ^double b]
  (clamp1 (/ a (+ b m/EPSILON))))

(defn blend-mdivide
  "Modulus divide"
  ^double [^double a ^double b]
  (mod1 (/ a (+ b m/EPSILON))))

(defn blend-or
  "Bitwise or"
  ^double [^double a ^double b]
  (let [aa (unchecked-long (* a 255.0))
        bb (unchecked-long (* b 255.0))]
    (get-r255 (bit-and 0xff (bit-or aa bb)))))

(defn blend-and
  "Bitwise and"
  ^double [^double a ^double b]
  (let [aa (unchecked-long (* a 255.0))
        bb (unchecked-long (* b 255.0))]
    (get-r255 (bit-and 0xff (bit-and aa bb)))))

(defn blend-xor
  "Bitwise xor"
  ^double [^double a ^double b]
  (let [aa (unchecked-long (* a 255.0))
        bb (unchecked-long (* b 255.0))]
    (get-r255 (bit-and 0xff (bit-xor aa bb)))))

(defn blend-exclusion
  "Exclusion"
  ^double [^double a ^double b]
  (let [ab (* a b)]
    (- (+ a b) (+ ab ab))))

(defn blend-pinlight-raw
  "Internal pinlight"
  ^double [^double a ^double b]
  (let [c (- (+ a a) 1.0)]
    (cond (< b c) c
          (bool-and (<= c b) (< b (+ c 1.0))) b
          :else (+ c 1.0))))

(defn blend-pinlight
  "Pinlight"
  ^double [a b]
  (clamp1 (blend-pinlight-raw a b)))

(defn blend-mpinlight
  "Modulus pinlight"
  ^double [a b]
  (mod1 (blend-pinlight-raw a b)))

(defn blend-opacity
  "Opacity (with `*blend-threshold*`)"
  (^double [^double a ^double b ^double thr]
   (m/mlerp a b thr))
  (^double [^double a ^double b]
   (m/lerp a b *blend-threshold*)))

(defn blend-overlay-raw
  "Internal overlay (with `*blend-threshold*`)"
  (^double [^double a ^double b ^double thr]
   (if (< a thr)
     (* 2.0 (* a b))
     (- 1.0 (* 2.0 (* (- 1.0 a) (- 1.0 b))))))
  (^double [a b]
   (blend-overlay-raw a b *blend-threshold*)))

(defn blend-overlay
  "Overlay"
  ^double [a b]
  (clamp1 (blend-overlay-raw a b)))

(defn blend-moverlay
  "Modulus overlay"
  ^double [a b]
  (mod1 (blend-overlay-raw a b)))

(defn blend-hardlight-raw
  "Internal hardlight (with `*blend-threshold*`)"
  (^double [^double a ^double b ^double thr]
   (if (< b thr)
     (* 2.0 (* a b))
     (- 1.0 (* 2.0 (* (- 1.0 a) (- 1.0 b))))))
  (^double [a b]
   (blend-hardlight-raw a b *blend-threshold*)))

(defn blend-hardlight
  "Hardlight"
  ^double [a b]
  (clamp1 (blend-hardlight-raw a b)))

(defn blend-mhardlight
  "Modulus hardlight"
  ^double [a b]
  (mod1 (blend-hardlight-raw a b)))

(defn blend-softlight-raw
  "Internal softlight (with `*blend-threshold*`)"
  (^double [^double a ^double b ^double thr]
   (if (< a thr)
     (->> b
          (* b)
          (- b)
          (* (- (+ a a) 1.0))
          (+ b))
     (-> b
         (m/sqrt)
         (- b)
         (* (- (+ a a) 1.0))
         (+ b))))
  (^double [a b]
   (blend-softlight-raw a b *blend-threshold*)))

(defn blend-softlight
  "Softlight"
  ^double [a b]
  (clamp1 (blend-softlight-raw a b)))

(defn blend-msoftlight
  "Modulus softlight"
  ^double [a b]
  (mod1 (blend-softlight-raw a b)))

(defn blend-vividlight-raw
  "Internal vividlight (with `*blend-threshold*`)"
  (^double [^double a ^double b ^double thr]
   (if (< a thr)
     (- 1.0 (/ (- 1.0 b) (+ (+ a a) m/EPSILON)))
     (->> a
          (- 1.0)
          (* 2.0)
          (+ m/EPSILON)
          (/ b))))
  (^double [a b]
   (blend-vividlight-raw a b *blend-threshold*)))

(defn blend-vividlight
  "Vividlight"
  ^double [a b]
  (clamp1 (blend-vividlight-raw a b)))

(defn blend-mvividlight
  "Modulus vividlight"
  ^double [a b]
  (mod1 (blend-vividlight-raw a b)))

(defn blend-darkthreshold
  "Dark thresholded (with `*blend-threshold*`)"
  (^double [^double a ^double b ^double thr]
   (if (< a thr) a b))
  (^double [a b]
   (blend-darkthreshold a b *blend-threshold*)))

(defn blend-lightthreshold
  "Light thresholded (with `*blend-threshold*`)"
  (^double [^double a ^double b ^double thr]
   (if (> a thr) a b))
  (^double [a b]
   (blend-lightthreshold a b *blend-threshold*)))

;; List of all blend functions stored in `blends` map
(def blends {:none blend-none
             :add blend-add
             :madd blend-madd
             :subtract blend-subtract
             :msubtract blend-msubtract
             :linearburn blend-linearburn
             :mlinearburn blend-mlinearburn
             :multiply blend-multiply
             :darken blend-darken
             :lighten blend-lighten
             :screen blend-screen
             :dodge blend-dodge
             :mdodge blend-mdodge
             :burn blend-burn
             :mburn blend-mburn
             :hardmix blend-hardmix
             :linearlight blend-linearlight
             :mlinearlight blend-mlinearlight
             :pegtoplight blend-pegtoplight
             :mpegtoplight blend-mpegtoplight
             :difference blend-difference
             :divide blend-divide
             :mdivide blend-mdivide
             :or blend-or
             :and blend-and
             :xor blend-xor
             :exclusion blend-exclusion
             :pinlight blend-pinlight
             :mpinlight blend-mpinlight
             :opacity blend-opacity
             :overlay blend-overlay
             :moverlay blend-moverlay
             :hardlight blend-hardlight
             :mhardlight blend-hardlight
             :softlight blend-softlight
             :msoftlight blend-msoftlight
             :vividlight blend-vividlight
             :mvividlight blend-mvividlight
             :darkthreshold blend-darkthreshold
             :lightthreshold blend-lightthreshold})

;; All names as list
(def blends-names (keys blends))

;; ## Colorspace functions

(defn- test-colors
  "to remove, check ranges"
  [f]
  (loop [cc (int 0)
         mnr (double Integer/MAX_VALUE)
         mxr (double Integer/MIN_VALUE)
         mng (double Integer/MAX_VALUE)
         mxg (double Integer/MIN_VALUE)
         mnb (double Integer/MAX_VALUE)
         mxb (double Integer/MIN_VALUE)]
    (let [r (bit-and 0xff (bit-shift-right cc 16))
          g (bit-and 0xff (bit-shift-right cc 8))
          b (bit-and 0xff cc)
          ^Vec4 res (f (Vec4. r g b 255))
          nmnr (if (< (.x res) mnr) (.x res) mnr)
          nmxr (if (> (.x res) mxr) (.x res) mxr)
          nmng (if (< (.y res) mng) (.y res) mng)
          nmxg (if (> (.y res) mxg) (.y res) mxg)
          nmnb (if (< (.z res) mnb) (.z res) mnb)
          nmxb (if (> (.z res) mxb) (.z res) mxb)]
      (if (< cc 0x1000000)
        (recur (inc cc) (double nmnr) (double nmxr) (double nmng) (double nmxg) (double nmnb) (double nmxb))
        [nmnr nmxr nmng nmxg nmnb nmxb]))))


;; ### CMY

(defn to-CMY
  "RGB -> CMY"
  [^Vec4 c]
  (Vec4. (- 255.0 (.x c))
         (- 255.0 (.y c))
         (- 255.0 (.z c))
         (.w c)))

(def from-CMY to-CMY)

;; ### OHTA

(defn to-OHTA
  "RGB -> OHTA, normalized"
  [^Vec4 c]
  (let [i1 (clamp255 (/ (+ (.x c) (.y c) (.z c)) 3.0))
        i2 (clamp255 (/ (+ 255.0 (- (.x c) (.z c))) 2.0))
        i3 (clamp255 (/ (+ 510.0 (.x c) (.z c) (- (+ (.y c) (.y c)))) 4.0))]
    (Vec4. i1 i2 i3 (.w c))))

(def ^:const ^double c46 (/ 4.0 6.0))

(defn from-OHTA
  "OHTA -> RGB"
  [^Vec4 c]
  (let [i1 (.x c) ; divided by 3
        i2 (- (.y c) 127.5) ; divided by 2
        i3 (- (* c46 (.z c)) 85.0) ; divided by 6
        r (clamp255 (+ i1 i2 i3))
        g (clamp255 (- i1 i3 i3))
        b (clamp255 (- (+ i1 i3) i2))]
    (Vec4. r g b (.w c))))

;; ### sRGB

(def ^:const ^double gamma-factor (/ 2.4))

(defn to-linear
  "Gamma correction"
  ^double [^double v]
  (if (> v 0.04045)
    (m/pow (/ (+ 0.055 v) 1.055) 2.4)
    (/ v 12.92)))

(defn from-linear
  "Gamma correction"
  ^double [^double v]
  (if (> v 0.0031308)
    (- (* 1.055 (m/pow v gamma-factor)) 0.055)
    (* v 12.92)))

(defn to-sRGB
  "Linear RGB to non-linear sRGB"
  [^Vec4 c]
  (v/vec4 (-> (Vec3. (.x c) (.y c) (.z c))
              (v/applyf (comp from-linear get-r255))
              (v/mult 255.0)
              (v/applyf clamp255))
          (.w c)))

(defn from-sRGB
  "Non-linear sRGB to linear RGB"
  [^Vec4 c]
  (v/vec4 (-> (Vec3. (.x c) (.y c) (.z c))
              (v/applyf (comp to-linear get-r255))
              (v/mult 255.0)
              (v/applyf clamp255))
          (.w c)))

;; ### XYZ

(def ^:const ^double xyz-xmax 0.9504716671128306)
(def ^:const ^double xyz-ymax 0.9999570331323426)
(def ^:const ^double xyz-zmax 1.0889782052041752)

(defn to-XYZ-raw
  "Pure RGB->XYZ conversion without corrections."
  ^Vec3 [^Vec3 c]
  (Vec3. (+ (* (.x c) 0.41239558896741421610) (* (.y c) 0.35758343076371481710) (* (.z c) 0.18049264738170157350))
         (+ (* (.x c) 0.21258623078559555160) (* (.y c) 0.71517030370341084990) (* (.z c) 0.07220049864333622685))
         (+ (* (.x c) 0.01929721549174694484) (* (.y c) 0.11918386458084853180) (* (.z c) 0.95049712513157976600))))

(defn- to-XYZ-
  "RGB->XYZ with corrections"
  [^Vec4 c]
  (let [xyz-raw (to-XYZ-raw (-> (Vec3. (.x c) (.y c) (.z c))
                                (v/applyf (comp to-linear get-r255))))]
    (v/vec4 xyz-raw (.w c))))

(defn to-XYZ
  "Normlized RGB->XYZ"
  [c]
  (let [^Vec4 cc (to-XYZ- c)]
    (Vec4. (clamp255 (m/norm (.x cc) 0.0 xyz-xmax 0.0 255.0))
           (clamp255 (m/norm (.y cc) 0.0 xyz-ymax 0.0 255.0))
           (clamp255 (m/norm (.z cc) 0.0 xyz-zmax 0.0 255.0))
           (.w cc))))

(defn from-XYZ-raw
  "Pure XYZ->RGB conversion."
  ^Vec3 [^Vec3 v]
  (Vec3. (+ (* (.x v)  3.2406) (* (.y v) -1.5372) (* (.z v) -0.4986))
         (+ (* (.x v) -0.9689) (* (.y v)  1.8758) (* (.z v)  0.0415))
         (+ (* (.x v)  0.0557) (* (.y v) -0.2040) (* (.z v)  1.0570))))

(defn- from-XYZ-
  "XYZ->RGB conversion with corrections"
  [^Vec4 c]
  (let [^Vec3 rgb-raw (v/mult (v/applyf (from-XYZ-raw (Vec3. (.x c) (.y c) (.z c))) from-linear) 255.0)]
    (v/vec4 rgb-raw (.w c))))

(defn from-XYZ
  "XYZ->RGB normalized"
  [^Vec4 c]
  (let [x (m/norm (.x c) 0.0 255.0 0.0 xyz-xmax)
        y (m/norm (.y c) 0.0 255.0 0.0 xyz-ymax)
        z (m/norm (.z c) 0.0 255.0 0.0 xyz-zmax)
        ^Vec4 rgb (from-XYZ- (Vec4. x y z (.w c)))]
    (v/applyf rgb clamp255)))

;; ### LUV

(def ^:const ^double D65X 0.950456)
(def ^:const ^double D65Z 1.088754)
(def ^:const ^double CIEEpsilon (/ 216.0 24389.0))
(def ^:const ^double CIEK (/ 24389.0 27.0))
(def ^:const ^double OneThird (/ 1.0 3.0))
(def ^:const ^double D65FX-4 (/ (* 4.0 D65X) (+ D65X 15.0 (* 3.0 D65Z))))
(def ^:const ^double D65FY-9 (/ 9.0 (+ D65X 15.0 (* 3.0 D65Z))))

(defn- perceptible-reciprocal
  "LUV reciprocal"
  ^double [^double x]
  (if (>= (m/abs x) m/EPSILON)
    (/ x)
    (/ (m/sgn x) m/EPSILON)))

(defn to-LUV
  "RGB->LUV normalized"
  [^Vec4 c]
  (let [^Vec4 xyz (to-XYZ- c)
        L (if (> (.y xyz) CIEEpsilon)
            (- (* 116.0 (m/pow (.y xyz) OneThird)) 16.0)
            (* (.y xyz) CIEK))
        alpha (perceptible-reciprocal (+ (.x xyz) (* 15.0 (.y xyz)) (* 3.0 (.z xyz))))
        L13 (* L 13.0)
        u (* L13 (- (* 4.0 alpha (.x xyz)) D65FX-4))
        v (* L13 (- (* 9.0 alpha (.y xyz)) D65FY-9))
        L (/ L 100.0)
        u (/ (+ u 134.0) 354.0)
        v (/ (+ v 140.0) 262.0)]
    (Vec4. (clamp255 (m/norm L 0.0 0.9999833859065517 0.0 255.0)) 
           (clamp255 (m/norm u 0.1438470144487729 0.8730615053231279 0.0 255.0))
           (clamp255 (m/norm v 0.022447496915761492 0.944255184334379 0.0 255.0))
           (.w c))))

(def ^:const ^double CIEK2Epsilon (* CIEK CIEEpsilon))

(defn from-LUV
  "LUV->RGB normalized"
  [^Vec4 c]
  (let [L (* 100.0 ^double (m/norm (.x c) 0 255 0.0 0.9999833859065517))
        u (- (* 354.0 ^double (m/norm (.y c) 0 255 0.1438470144487729 0.8730615053231279)) 134.0)
        v (- (* 262.0 ^double (m/norm (.z c) 0 255 0.022447496915761492 0.944255184334379)) 140.0)
        Y (if (> L CIEK2Epsilon)
            (m/pow (/ (+ L 16.0) 116.0) 3.0)
            (/ L CIEK))
        L13 (* 13.0 L)
        L52 (* 52.0 L)
        Y5 (* 5.0 Y)
        L13u (-> L52
                 (/ (+ u (* L13 D65FX-4)))
                 dec
                 (/ 3.0))
        X (/ (+ Y5 (* Y (- (/ (* 39.0 L) (+ v (* L13 D65FY-9))) 5.0))) (+ L13u OneThird))
        Z (- (* X L13u) Y5)
        ^Vec4 rgb (from-XYZ- (Vec4. X Y Z (.w c)))]
    (v/applyf rgb clamp255)))

;; ### LAB

(defn- to-lab-correct
  "LAB correction"
  ^double [^double v]
  (if (> v CIEEpsilon)
    (m/pow v OneThird)
    (/ (+ 16.0 (* v CIEK)) 116.0)))

(defn to-LAB
  "RGB->LAB normalized"
  [^Vec4 c]
  (let [^Vec4 xyz (to-XYZ- c)
        x (/ (.x xyz) D65X)
        y (.y xyz)
        z (/ (.z xyz) D65Z)
        x (to-lab-correct x)
        y (to-lab-correct y)
        z (to-lab-correct z)
        L (/ (- (* y 116.0) 16.0) 100.0)
        a (+ 0.5 (/ (* 500.0 (- x y)) 255.0))
        b (+ 0.5 (/ (* 200.0 (- y z)) 255.0))]
    (Vec4. (clamp255 (m/norm L 0.0 0.9999833859065517 0.0 255.0))
           (clamp255 (m/norm a 0.16203039020156618 0.8853278445843099 0.0 255.0))
           (clamp255 (m/norm b 0.07698923890750631 0.8705163895243013 0.0 255.0)) 
           (.w c))))

(defn from-lab-correct
  "LAB correction"
  ^double [^double v]
  (let [v3 (* v v v)]
    (if (> v3 CIEEpsilon)
      v3
      (/ (- (* 116.0 v) 16.0) CIEK))))

(defn from-LAB
  "LAB->RGB normalized"
  [^Vec4 c]
  (let [L (* 100.0 ^double (m/norm (.x c) 0.0 255.0 0.0 0.9999833859065517))
        ^double a (m/norm (.y c) 0.0 255.0 0.16203039020156618 0.8853278445843099)
        ^double b (m/norm (.z c) 0.0 255.0 0.07698923890750631 0.8705163895243013)
        y (/ (+ L 16.0) 116.0)
        x (* D65X (from-lab-correct (+ y (/ (* 255.0 (- a 0.5)) 500.0))))
        z (* D65Z (from-lab-correct (- y (/ (* 255.0 (- b 0.5)) 200.0))))
        y3 (* y y y)
        y (if (> y3 CIEEpsilon)
            y3
            (/ L CIEK))
        ^Vec4 rgb (from-XYZ- (Vec4. x y z (.w c)))]
    (v/applyf rgb clamp255)))

;; ### YXy (xyY)

(defn to-YXY
  "RGB->YXY"
  [^Vec4 c]
  (let [^Vec4 xyz (to-XYZ- c)
        d (+ (.x xyz) (.y xyz) (.z xyz))
        Y (m/norm (.y xyz) 0.0 0.9999570331323426 0.0 255.0)
        x (m/norm (/ (.x xyz) d) 0.150011724420108 0.6400884809339611 0.0 255.0)
        y (m/norm (/ (.y xyz) d) 0.060007548576610774 0.6000064972148145 0.0 255.0)]
    (v/applyf (Vec4. Y x y (.w c)) clamp255)))

(defn from-YXY
  "YXY->RGB"
  [^Vec4 c]
  (let [^double Y (m/norm (.x c) 0.0 255.0 0.0 0.9999570331323426)
        ^double x (m/norm (.y c) 0.0 255.0 0.150011724420108 0.6400884809339611)
        ^double y (m/norm (.z c) 0.0 255.0 0.060007548576610774 0.6000064972148145)
        Yy (/ Y y)
        X (* x Yy)
        Z (* (- 1.0 x y) Yy)
        ^Vec4 rgb (from-XYZ- (Vec4. X Y Z (.w c)))]
    (v/applyf rgb clamp255)))

;; ### HCL

(defn to-HCL
  "RGB->HCL"
  [^Vec4 c]
  (let [mx (max (.x c) (.y c) (.z c))
        chr (- mx (min (.x c) (.y c) (.z c)))
        h (* 255.0 (/ (if (zero? chr) 0.0
                          (if (== mx (.x c))
                            (rem (+ 6.0 (/ (- (.y c) (.z c)) chr)) 6.0)
                            (if (== mx (.y c))
                              (+ 2.0 (/ (- (.z c) (.x c)) chr))
                              (+ 4.0 (/ (- (.x c) (.y c)) chr))))) 6.0))
        luma (+ (* 0.298839 (.x c)) (* 0.586811 (.y c)) (* 0.114350 (.z c)))]
    (v/applyf (Vec4. h chr luma (.w c)) clamp255)))

(defn from-HCL
  "HCL->RGB"
  [^Vec4 c]
  (let [h (* 6.0 (get-r255 (.x c)))
        chr (.y c)
        l (.z c)
        x (* chr (- 1.0 (m/abs (dec (rem h 2.0)))))
        ^Vec3 rgb (cond
                    (and (<= 0.0 h) (< h 1.0)) (Vec3. chr x 0.0)
                    (and (<= 1.0 h) (< h 2.0)) (Vec3. x chr 0.0)
                    (and (<= 2.0 h) (< h 3.0)) (Vec3. 0.0 chr x)
                    (and (<= 3.0 h) (< h 4.0)) (Vec3. 0.0 x chr)
                    (and (<= 4.0 h) (< h 5.0)) (Vec3. x 0.0 chr)
                    :else                      (Vec3. chr 0.0 x))
        m (- l (* 0.298839 (.x rgb)) (* 0.586811 (.y rgb)) (* 0.114350 (.z rgb)))]
    (v/applyf (Vec4. (+ (.x rgb) m) (+ (.y rgb) m) (+ (.z rgb) m) (.w c)) clamp255)))

;; ### HSB

(defn to-HSB
  "RGB->HSB"
  [^Vec4 c]
  (let [mn (min (.x c) (.y c) (.z c))
        mx (max (.x c) (.y c) (.z c))
        delta (- mx mn)
        hsb (if (zero? mx) (Vec3. 0.0 0.0 0.0)
                (let [s (* 255.0 (/ delta mx))
                      h (if (zero? delta) 0.0
                            (/ (if (== mx (.x c))
                                 (/ (- (.y c) (.z c)) delta)
                                 (if (== mx (.y c))
                                   (+ 2.0 (/ (- (.z c) (.x c)) delta))
                                   (+ 4.0 (/ (- (.x c) (.y c)) delta)))) 6.0))]
                  (v/applyf (Vec3. (* 255.0 (if (neg? h) (inc h) h)) s mx) clamp255)))]
    (v/vec4 hsb (.w c))))

(defn from-HSB
  "HSB->RGB"
  [^Vec4 c]
  (if (zero? (.y c)) (Vec4. (.z c) (.z c) (.z c) (.w c))
      (let [h (get-r255 (.x c))
            s (get-r255 (.y c))
            b (get-r255 (.z c))
            h (* 6.0 (- h (m/floor h)))
            f (- h (m/floor h))
            p (* b (- 1.0 s))
            q (* b (- 1.0 (* s f)))
            t (* b (- 1.0 (* s (- 1.0 f))))
            rgb (case (unchecked-int h)
                  0 (Vec3. b t p)
                  1 (Vec3. q b p)
                  2 (Vec3. p b t)
                  3 (Vec3. p q b)
                  4 (Vec3. t p b)
                  5 (Vec3. b p q))            ]
        (v/vec4 (v/applyf (v/mult rgb 255.0) clamp255) (.w c)))))

;; ### HSI

(def ^:const ^double to-hsi-const (-> 180.0
                                      (/ m/PI)
                                      (/ 360.0)))

(defn to-HSI
  "RGB->HSI"
  [^Vec4 c]
  (let [i (/ (+ (.x c) (.y c) (.z c)) 3.0)]
    (if (zero? i) (Vec4. 0.0 0.0 0.0 (.w c))
        (let [s (- 1.0 (/ (min (.x c) (.y c) (.z c)) i))
              alpha (* 0.5 (- (* 2.0 (.x c)) (.y c) (.z c)))
              beta (* 0.8660254037844385 (- (.y c) (.z c)))
              hue (* to-hsi-const (m/atan2 beta alpha))
              hue (if (neg? hue) (inc hue) hue)]
          (v/applyf (Vec4. (* 255.0 hue) (* 255.0 s) i (.w c)) clamp255)))))

(def ^:const ^double from-hsi-const (/ m/PI 180.0))

(defn from-hsi-helper
  ""
  ^double [^Vec4 cc ^double h]
  (* (.z cc) (-> cc
                 .y
                 (* (m/cos (* h from-hsi-const)))
                 (/ (m/cos (* (- 60.0 h) from-hsi-const)))
                 inc)))

(defn from-HSI
  "HSI->RGB"
  [^Vec4 c]
  (let [^Vec4 cc (v/applyf c get-r255)
        h (* 360.0 (.x cc))
        h (- h (* 360.0 (m/floor (/ h 360.0))))
        v1 (* (.z cc) (- 1.0 (.y cc)))
        rgb (cond
              (< h 120.0) (let [b v1
                                r (from-hsi-helper cc h)
                                g (- (* 3.0 (.z cc)) r b)]
                            (Vec3. r g b))
              (< h 240.0) (let [r v1
                                g (from-hsi-helper cc (- h 120.0))
                                b (- (* 3.0 (.z cc)) r g)]
                            (Vec3. r g b))
              :else (let [g v1
                          b (from-hsi-helper cc (- h 240.0))
                          r (- (* 3.0 (.z cc)) g b)]
                      (Vec3. r g b)))]
    (v/vec4 (v/applyf (v/mult rgb 255.0) clamp255) (.w c))))

;; ### HWB

(defn to-HWB
  "RGB->HWB"
  [^Vec4 c]
  (let [w (min (.x c) (.y c) (.z c))
        v (max (.x c) (.y c) (.z c))
        h (if (== w v) 0.0
              (let [^double f (condp m/eq w
                                (.x c) (- (.y c) (.z c))
                                (.y c) (- (.z c) (.x c))
                                (.z c) (- (.x c) (.y c)))
                    ^double p (condp m/eq w
                                (.x c) 3.0
                                (.y c) 5.0
                                (.z c) 1.0)]
                (m/norm (/ (- p (/ f (- v w))) 6.0) 0.0 1.0 1.0 255.0)))]
    (v/applyf (Vec4. h w (- 255.0 v) (.w c)) clamp255)))

(defn from-HWB
  "HWB->RGB"
  [^Vec4 c]
  (if (< (.x c) 1.0) 
    (let [v (- 255.0 (.z c))]
      (Vec4. v v v (.w c)))
    (let [^double h (m/norm (.x c) 1.0 255.0 0.0 6.0)
          v (- 1.0 (/ (.z c) 255.0))
          w (/ (.y c) 255.0)
          i (unchecked-int (m/floor h))
          f (- h i)
          f (if (odd? (int i)) (- 1.0 f) f)
          n (+ w (* f (- v w)))
          rgb (case i
                0 (Vec3. v n w)
                1 (Vec3. n v w)
                2 (Vec3. w v n)
                3 (Vec3. w n v)
                4 (Vec3. n w v)
                5 (Vec3. v w n)
                6 (Vec3. v n w))]
      (v/vec4 (v/applyf (v/mult rgb 255.0) clamp255) (.w c)))))

;; ### YPbPr

(defn to-YPbPr
  "RGB -> YPbPr, normalized"
  [^Vec4 c]
  (let [y (+ (* 0.2126 (.x c))
             (* 0.7152 (.y c))
             (* 0.0722 (.z c)))
        pb (clamp255 (m/norm (- (.z c) y) -237.0 237.0 0.0 255.0))
        pr (clamp255 (m/norm (- (.x c) y) -201.0 201.0 0.0 255.0))]
    (Vec4. (clamp255 y) pb pr (.w c))))

(defn from-YPbPr
  "YPbPr -> RGB"
  [^Vec4 c]
  (let [b (+ (.x c) ^double (m/norm (.y c) 0.0 255.0 -237.0 237.0))
        r (+ (.x c) ^double (m/norm (.z c) 0.0 255.0 -201.0 201.0))
        g (/ (- (.x c) (* 0.2126 r) (* 0.0722 b)) 0.7152)]
    (v/applyf (Vec4. r g b (.w c)) clamp255)))

;; ### YDbDr

(defn to-YDbDr
  "RGB->YDbDr"
  [^Vec4 c]
  (let [Y (+ (* 0.299 (.x c)) (* 0.587 (.y c)) (* 0.114 (.z c)))
        Db (+ (* -0.45 (.x c)) (* -0.883 (.y c)) (* 1.333 (.z c)))
        Dr (+ (* -1.333 (.x c)) (* 1.116 (.y c)) (* 0.217 (.z c)))]
    (v/applyf (Vec4. Y
                     (m/norm Db -339.91499999999996 339.91499999999996 0.0 255.0)
                     (m/norm Dr -339.91499999999996 339.915 0.0 255.0)
                     (.w c)) clamp255)))

(defn from-YDbDr
  "YDbDr->RGB"
  [^Vec4 c]
  (let [Y (.x c)
        ^double Db (m/norm (.y c) 0.0 255.0 -339.91499999999996 339.91499999999996)
        ^double Dr (m/norm (.z c) 0.0 255.0 -339.91499999999996 339.915)
        r (+ Y (* 9.2303716147657e-05 Db) (* -0.52591263066186533 Dr))
        g (+ Y (* -0.12913289889050927 Db) (* 0.26789932820759876 Dr))
        b (+ Y (* 0.66467905997895482 Db) (* -7.9202543533108e-05 Dr))]
    (v/applyf (Vec4. r g b (.w c)) clamp255)))

;; ### YCbCr

;; JPEG version

(defn to-YCbCr
  "RGB->YCbCr"
  [^Vec4 c]
  (let [Y (+ (* 0.298839 (.x c)) (* 0.586811 (.y c)) (* 0.114350 (.z c)))
        Cb (+ 127.5 (* -0.168736 (.x c)) (* -0.331264 (.y c)) (* 0.5 (.z c)))
        Cr (+ 127.5 (* 0.5 (.x c)) (* -0.418688 (.y c)) (* -0.081312 (.z c)))]
    (v/applyf (Vec4. Y Cb Cr (.w c)) clamp255)))

(defn from-YCbCr
  "YCbCr->RGB"
  [^Vec4 c]
  (let [Cb (- (.y c) 127.5)
        Cr (- (.z c) 127.5)
        r (+ (* 0.99999999999914679361 (.x c)) (* -1.2188941887145875e-06 Cb) (* 1.4019995886561440468 Cr))
        g (+ (* 0.99999975910502514331 (.x c)) (* -0.34413567816504303521 Cb) (* -0.71413649331646789076 Cr))
        b (+ (* 1.00000124040004623180 (.x c)) (* 1.77200006607230409200 Cb) (* 2.1453384174593273e-06 Cr))]
    (v/applyf (Vec4. r g b (.w c)) clamp255)))

;; ### YUV

(defn to-YUV
  "RGB->YUV"
  [^Vec4 c]
  (let [Y (+ (* 0.298839 (.x c)) (* 0.586811 (.y c)) (* 0.114350 (.z c)))
        U (+ (* -0.147 (.x c)) (* -0.289 (.y c)) (* 0.436 (.z c)))
        V (+ (* 0.615 (.x c)) (* -0.515 (.y c)) (* -0.1 (.z c)))]
    (v/applyf (Vec4. Y 
                     (m/norm U -111.17999999999999 111.17999999999999 0.0 255.0)
                     (m/norm V -156.82500000000002 156.825 0.0 255.0)
                     (.w c)) clamp255)))

(defn from-YUV
  "YUV->RGB"
  [^Vec4 c]
  (let [Y (.x c)
        ^double U (m/norm (.y c) 0.0 255.0 -111.17999999999999 111.17999999999999)
        ^double V (m/norm (.z c) 0.0 255.0 -156.82500000000002 156.825)
        r (+ Y (* -3.945707070708279e-05 U) (* 1.1398279671717170825 V))
        g (+ Y (* -0.3946101641414141437 U) (* -0.5805003156565656797 V))
        b (+ Y (* 2.0319996843434342537 U) (* -4.813762626262513e-04 V))]
    (v/applyf (Vec4. r g b (.w c)) clamp255)))

;; ### YIQ

(defn to-YIQ
  "RGB->YIQ"
  [^Vec4 c]
  (let [Y (+ (* 0.298839 (.x c)) (* 0.586811 (.y c)) (* 0.114350 (.z c)))
        I (+ (* 0.595716 (.x c)) (* -0.274453 (.y c)) (* -0.321263 (.z c)))
        Q (+ (* 0.211456 (.x c)) (* -0.522591 (.y c)) (* 0.311135 (.z c)))]
    (v/applyf (Vec4. Y 
                     (m/norm I -151.90758 151.90758 0.0 255.0)
                     (m/norm Q -133.260705 133.260705 0.0 255.0)
                     (.w c)) clamp255)))

(defn from-YIQ
  "YIQ->RGB"
  [^Vec4 c]
  (let [Y (.x c)
        ^double I (m/norm (.y c) 0.0 255.0 -151.90758 151.90758)
        ^double Q (m/norm (.z c) 0.0 255.0 -133.260705 133.260705)
        r (+ Y (* +0.9562957197589482261 I) (* 0.6210244164652610754 Q))
        g (+ Y (* -0.2721220993185104464 I) (* -0.6473805968256950427 Q))
        b (+ Y (* -1.1069890167364901945 I) (* 1.7046149983646481374 Q))]
    (v/applyf (Vec4. r g b (.w c)) clamp255)))

;; ### YCgCo

(defn to-YCgCo
  "RGB->YCgCo"
  [^Vec4 c]
  (let [Y (+ (* 0.25 (.x c)) (* 0.5 (.y c)) ( * 0.25 (.z c)))
        Cg (+ 127.5 (+ (* -0.25 (.x c)) (* 0.5 (.y c)) ( * -0.25 (.z c))))
        Co (+ 127.5 (+ (* 0.5 (.x c)) (* -0.5 (.z c))))]
    (v/applyf (Vec4. Y Cg Co (.w c)) clamp255)))

(defn from-YCgCo
  "YCgCo->RGB"
  [^Vec4 c]
  (let [Cg (- (.y c) 127.5)
        Co (- (.z c) 127.5)
        tmp (- (.x c) Cg)]
    (v/applyf (Vec4. (+ Co tmp) (+ (.x c) Cg) (- tmp Co) (.w c)) clamp255)))

;; ### Grayscale

(defn to-Gray
  "RGB->Grayscale"
  [^Vec4 c]
  (let [^double l (to-luma c)]
    (Vec4. l l l (.w c))))

;; do nothing in reverse
(def from-Gray identity)

;; Just for a case "do nothing"
(def to-RGB identity)
(def from-RGB identity)

;; List of all color spaces with functions
(def colorspaces {:CMY   [to-CMY from-CMY]
                  :OHTA  [to-OHTA from-OHTA]
                  :XYZ   [to-XYZ from-XYZ]
                  :YXY   [to-YXY from-YXY]
                  :LUV   [to-LUV from-LUV]
                  :LAB   [to-LAB from-LAB]
                  :HCL   [to-HCL from-HCL]
                  :HSB   [to-HSB from-HSB]
                  :HSI   [to-HSI from-HSI]
                  :HWB   [to-HWB from-HWB]
                  :YPbPr [to-YPbPr from-YPbPr]
                  :YDbDr [to-YDbDr from-YDbDr]
                  :YCbCr [to-YCbCr from-YCbCr]
                  :YCgCo [to-YCgCo from-YCgCo]
                  :YUV   [to-YUV from-YUV]
                  :YIQ   [to-YIQ from-YIQ]
                  :Gray  [to-Gray from-Gray]
                  :sRGB  [to-sRGB from-sRGB]
                  :RGB   [to-RGB from-RGB]})

;; List of color spaces names
(def colorspaces-names (keys colorspaces))

(defn to-cs
  "Return colorspace converter by keyword (RGB -> ...)"
  [cs]
  ((cs colorspaces) 0))

(defn from-cs
  "Return colorspace converter by keyword (... -> RGB)"
  [cs]
  ((cs colorspaces) 1))

;; ## Palettes

(defn hex-to-vec
  "Convert hexadecimal color representation to color"
  [s]
  (let [x (Long/parseLong s 16)
        x1 (bit-and 0xff (>> x 16))
        x2 (bit-and 0xff (>> x 8))
        x3 (bit-and 0xff x)]
    (Vec4. x1 x2 x3 255)))

(defn hex-to-vecs
  "Convert list of hexadecimal strings into list of colors."
  [xs]
  (mapv hex-to-vec xs))

;; ### Colourlovers

;; Read and parse 200 best palettes taken from http://www.colourlovers.com/ (stored locally)
(def colourlovers-palettes
  (let [p1 (xml/parse (-> (resource "cl1.xml.gz") input-stream java.util.zip.GZIPInputStream.))
        p2 (xml/parse (-> (resource "cl2.xml.gz") input-stream java.util.zip.GZIPInputStream.))
        f (fn [xml-in] (map (fn [x] (map #((:content %) 0) (:content (first (filter #(= (:tag %) :colors) (:content ((:content xml-in) x))))))) (range 100)))
        l1 (f p1)
        l2 (f p2)]
    (mapv hex-to-vecs (concat l1 l2))))

;; ### Inigo Quilez

;; http://iquilezles.org/www/articles/palettes/palettes.htm

(defn make-iq-palette-fn
  "Create palette generator function with given parametrization"
  [a b c d]
  (fn [t]
    (let [^Vec3 cc (-> (->> t
                            (v/mult c)
                            (v/add d))
                       (v/mult m/TWO_PI)
                       (v/applyf #(m/cos %))
                       (v/emult b)
                       (v/add a))]
      (-> (Vec4. (.x cc) (.y cc) (.z cc) 1.0)
          (v/mult 255.0)
          (v/applyf clamp255)))))

(defn make-iq-random-palette
  "Create palette with cosinus generator. Input parameter: number of colors."
  [^double num]
  (let [a (v/generate-vec3 (partial r/drand 0.3 0.7))
        b (v/sub (Vec3. 1.0 1.0 1.1) a)
        c (v/generate-vec3 (partial r/drand 2))
        d (v/generate-vec3 r/drand)
        f (make-iq-palette-fn a b c d)]
    (mapv f (range 0.0 1.0 (/ num)))))

;; ### Paletton

;; Here you can find reimplementation of http://paletton.com palette generator.
;; You can create palette based on `hue` value with following options:
;;
;; * Palette type, one from `:monochromatic`, `:triad`, `:tetrad`
;; * Paletton presets, check `paletton-presets-names` value
;; * Complementary color
;; * Angle between colors for `:triad` and `:tetrad`

(def paletton-base-data
  (let [s (fn ^double [^double e ^double t ^double n] (if (== n -1.0) e
                                                          (+ e (/ (- t e) (inc n)))))
        i (fn ^double [^double e ^double t ^double n] (if (== n -1.0) t
                                                          (+ t (/ (- e t) (inc n)))))
        paletton-base-values   {:r  [1.0 1.0]
                                :rg [1.0 1.0]
                                :g  [1.0 0.8]
                                :gb [1.0 0.6]
                                :b  [0.85 0.7]
                                :br [1.0 0.65]}]
    {120.0 {:a (:r paletton-base-values)
            :b (:rg paletton-base-values)
            :f (fn ^double [^double e]
                 (if (== e 0.0) -1.0
                     (* 0.5 (m/tan (* m/HALF_PI (/ (- 120.0 e) 120.0))))))
            :fi (fn ^double [^double e]
                  (if (== e -1.0) 0.0
                      (- 120.0 (* 2.0 (/ (* (m/atan (/ e 0.5)) 120.0) m/PI)))))
            :g s
            :rgb (fn [e n r] (Vec4. e n r 255.0))}
     180.0 {:a (:rg paletton-base-values)
            :b (:g paletton-base-values)
            :f (fn ^double [^double e]
                 (if (== e 180.0) -1.0
                     (* 0.5 (m/tan (* m/HALF_PI (/ (- e 120.0) 60.0))))))
            :fi (fn ^double [^double e]
                  (if (== e -1.0) 180.0
                      (+ 120.0 (* 2.0 (/ (* (m/atan (/ e 0.5)) 60.0) m/PI)))))
            :g i
            :rgb (fn [e n r] (Vec4. n e r 255.0))}
     
     210.0 {:a (:g paletton-base-values)
            :b (:gb paletton-base-values)
            :f (fn ^double [^double e]
                 (if (== e 180.0) -1.0
                     (* 0.75 (m/tan (* m/HALF_PI (/ (- 210.0 e) 30.0))))))
            :fi (fn ^double [^double e]
                  (if (== e -1.0) 180.0
                      (- 210.0 (* 2.0 (/ (* (m/atan (/ e 0.75)) 30.0) m/PI)))))
            :g s
            :rgb (fn [e n r] (Vec4. r e n 255.0))}
     255.0 {:a (:gb paletton-base-values)
            :b (:b paletton-base-values)
            :f (fn ^double [^double e]
                 (if (== e 255.0) -1.0
                     (* 1.33 (m/tan (* m/HALF_PI (/ (- e 210.0) 45.0))))))
            :fi (fn ^double [^double e]
                  (if (== e -1.0) 255.0
                      (+ 210.0 (* 2.0 (/ (* (m/atan (/ e 1.33)) 45.0) m/PI)))))
            :g i
            :rgb (fn [e n r] (Vec4. r n e 255.0))}
     
     315.0 {:a (:b paletton-base-values)
            :b (:br paletton-base-values)
            :f (fn ^double [^double e]
                 (if (== e 255.0) -1.0
                     (* 1.33 (m/tan (* m/HALF_PI (/ (- 315.0 e) 60.0))))))
            :fi (fn ^double [^double e]
                  (if (== e -1.0) 255.0
                      (- 315.0 (* 2.0 (/ (* (m/atan (/ e 1.33)) 60.0) m/PI)))))
            :g s
            :rgb (fn [e n r] (Vec4. n r e 255.0))}
     360.0 {:a (:br paletton-base-values)
            :b (:r paletton-base-values)
            :f (fn ^double [^double e]
                 (if (== e 0.0) -1.0
                     (* 1.33 (m/tan (* m/HALF_PI (/ (- e 315.0) 45.0))))))
            :fi (fn ^double [^double e]
                  (if (== e -1.0) 0.0
                      (+ 315.0 (* 2.0 (/ (* (m/atan (/ e 1.33)) 45.0) m/PI)))))
            :g i
            :rgb (fn [e n r] (Vec4. e r n 255.0))}}))

(defn paletton-hsv-to-rgb
  "Paletton version of HSV to RGB converter"
  [^double hue ^double ks ^double kv]
  (let [ks (m/constrain ks 0.0 2.0)
        kv (m/constrain kv 0.0 2.0)
        h (mod hue 360.0)
        upd (fn ^double [^double e ^double t] (if (<= t 1.0)
                                                (* e t)
                                                (+ e (* (- 1.0 e) (dec t)))))
        {:keys [a b f g rgb]} (second (first (filter #(< h ^double (% 0)) paletton-base-data)))
        av (second a)
        bv (second b)
        as (first a)
        bs (first b)
        ^double n (f h)
        ^double v (upd (g av bv n) kv)
        ^double s (upd (g as bs n) ks)
        r (* 255.0 v)
        b (* r (- 1.0 s))
        g (if (== n -1.0) b
              (/ (+ r (* n b)) (inc n)))]
    (rgb r g b)))

(defn paletton-rgb-to-hue
  "Take paletton HUE from RGB"
  (^double [^double r ^double g ^double b]
   (if (== r g b)
     0.0
     (let [f (max r g b)
           p (min r g b)
           [^double l ^double i] (if (== f r)
                                   (if (== p b)
                                     [g (:fi (paletton-base-data 120.0))]
                                     [b (:fi (paletton-base-data 360.0))])
                                   (if (== f g)
                                     (if (== p r)
                                       [b (:fi (paletton-base-data 210.0))]
                                       [r (:fi (paletton-base-data 180.0))])
                                     (if (== p r)
                                       [g (:fi (paletton-base-data 255.0))]
                                       [r (:fi (paletton-base-data 315.0))])))
                                        ;d (/ (- f p) f) ;; saturation
                                        ;v (/ f 255.0)   ;; value
           s (i (if (== l p) -1.0
                    (/ (- f l) (- l p))))]
       s)))
  ([^Vec4 c] (paletton-rgb-to-hue (.x c) (.y c) (.z c))))

;; List of paletton presets
(def paletton-presets
  {:pale-light [[0.24649 1.78676] [0.09956 1.95603] [0.17209 1.88583] [0.32122 1.65929] [0.39549 1.50186]]
   :pastels-bright [[0.65667 1.86024] [0.04738 1.99142] [0.39536 1.89478] [0.90297 1.85419] [1.86422 1.8314]]
   :shiny [[1.00926 2] [0.3587 2] [0.5609 2] [2 0.8502] [2 0.65438]]
   :pastels-lightest [[0.34088 1.09786] [0.13417 1.62645] [0.23137 1.38072] [0.45993 0.92696] [0.58431 0.81098]]
   :pastels-very-light [[0.58181 1.32382] [0.27125 1.81913] [0.44103 1.59111] [0.70192 1.02722] [0.84207 0.91425]]
   :full [[1 1] [0.61056 1.24992] [0.77653 1.05996] [1.06489 0.77234] [1.25783 0.60685]]
   :pastels-light [[0.37045 0.90707] [0.15557 1.28367] [0.25644 1.00735] [0.49686 0.809] [0.64701 0.69855]]
   :pastels-med [[0.66333 0.8267] [0.36107 1.30435] [0.52846 0.95991] [0.78722 0.70882] [0.91265 0.5616]]
   :darker [[0.93741 0.68672] [0.68147 0.88956] [0.86714 0.82989] [1.12072 0.5673] [1.44641 0.42034]]
   :pastels-mid-pale [[0.38302 0.68001] [0.15521 0.98457] [0.26994 0.81586] [0.46705 0.54194] [0.64065 0.44875]]
   :pastels [[0.66667 0.66667] [0.33333 1] [0.5 0.83333] [0.83333 0.5] [1 0.33333]]
   :dark-neon [[0.94645 0.59068] [0.99347 0.91968] [0.93954 0.7292] [1.01481 0.41313] [1.04535 0.24368]]
   :pastels-dark [[0.36687 0.39819] [0.25044 0.65561] [0.319 0.54623] [0.55984 0.37953] [0.70913 0.3436]]
   :pastels-very-dark [[0.60117 0.41845] [0.36899 0.59144] [0.42329 0.44436] [0.72826 0.35958] [0.88393 0.27004]]
   :dark [[1.31883 0.40212] [0.9768 0.25402] [1.27265 0.30941] [1.21289 0.60821] [1.29837 0.82751]]
   :pastels-mid-dark [[0.26952 0.22044] [0.23405 0.52735] [0.23104 0.37616] [0.42324 0.20502] [0.54424 0.18483]]
   :pastels-darkest [[0.53019 0.23973] [0.48102 0.50306] [0.50001 0.36755] [0.6643 0.32778] [0.77714 0.3761]]
   :darkest [[1.46455 0.21042] [0.99797 0.16373] [0.96326 0.274] [1.56924 0.45022] [1.23016 0.66]]
   :almost-black [[0.12194 0.15399] [0.34224 0.50742] [0.24211 0.34429] [0.31846 0.24986] [0.52251 0.33869]]
   :almost-gray-dark [[0.10266 0.24053] [0.13577 0.39387] [0.11716 0.30603] [0.14993 0.22462] [0.29809 0.19255]]
   :almost-gray-darker [[0.07336 0.36815] [0.18061 0.50026] [0.09777 0.314] [0.12238 0.25831] [0.14388 0.1883]]
   :almost-gray-mid [[0.07291 0.59958] [0.19602 0.74092] [0.10876 0.5366] [0.15632 0.48229] [0.20323 0.42268]]
   :almost-gray-lighter [[0.06074 0.82834] [0.14546 0.97794] [0.10798 0.76459] [0.15939 0.68697] [0.22171 0.62926]]
   :almost-gray-light [[0.03501 1.59439] [0.23204 1.10483] [0.14935 1.33784] [0.07371 1.04897] [0.09635 0.91368]]})

;; List of preset names
(def paletton-presets-names (keys paletton-presets))

(defn make-monochromatic-palette
  "Create monochromatic palette from hue and preset."
  [hue preset]
  (mapv (fn [[ks kv]] (paletton-hsv-to-rgb hue ks kv)) preset))

(defmulti paletton-palette (fn [m hue & conf] m))

;; Following methods can be used to create paletton palettes.
;; As a dispatch use one of the types `:monochromatic`, `:triad` or `:tetrad`.
;; Parameters are `hue` value and configuration.

;; `:monochromatic` configuration
;;
;; * `:compl` - use complementary color? (true/false)
;; * `:preset` - what preset to use (one from `paletton-preset-names`)
(defmethod paletton-palette :monochromatic [_ hue & conf]
  (let [{compl :compl 
         preset :preset
         :or {compl false
              preset :full}} (first conf)
        ppreset (if (keyword? preset) (paletton-presets preset) preset)
        p (make-monochromatic-palette hue ppreset)]
    (if compl (vec (concat p (make-monochromatic-palette (+ ^double hue 180.0) ppreset))) p)))

;; `:triad` configuration
;;
;; * `:compl` - use complementary color? (true/false)
;; * `:preset` - what preset to use (one from `paletton-preset-names`)
;; * `:angle` - angle between main hue and two additional
;; * `:adj` - use adjacent version of triad (true/false)
(defmethod paletton-palette :triad [_ hue & conf]
  (let [{compl :compl
         preset :preset
         ^double angle :angle
         adj :adj
         :or {compl false
              preset :full
              angle 30.0
              adj true}} (first conf)
        chue (+ 180.0 ^double hue)
        hue1 (if adj (+ ^double hue angle) (+ chue angle))
        hue2 (if adj (- ^double hue angle) (- chue angle))
        ppreset (if (keyword? preset) (paletton-presets preset) preset)
        p1 (make-monochromatic-palette hue ppreset)
        p2 (make-monochromatic-palette hue1 ppreset)
        p3 (make-monochromatic-palette hue2 ppreset)
        p (vec (concat p1 p2 p3))]
    (if compl (vec (concat p (make-monochromatic-palette chue ppreset))) p)))

;; `:tetrad` configuration
;;
;; * `:preset` - what preset to use (one from `paletton-preset-names`)
;; * `:angle` - angle between main hue and additional color
(defmethod paletton-palette :tetrad [_ hue & conf]
  (let [{preset :preset
         ^double angle :angle
         :or {preset :full
              angle 30.0}} (first conf)
        p1 (paletton-palette :monochromatic hue {:preset preset :compl true})
        p2 (paletton-palette :monochromatic (+ angle ^double hue) {:preset preset :compl true})]
    (vec (concat p1 p2))))

;; ## Additional functions

(defn nearest-color
  "Find nearest color from a set. Input: distance function (default euclidean), list of target colors and source color."
  ([f xf c]
   (let [s (count xf)]
     (loop [i (int 0)
            currc c
            currdist Double/MAX_VALUE]
       (if (< i s)
         (let [c1 (xf i)
               dist (double (f c c1))]
           (recur (unchecked-inc i)
                  (if (< dist currdist) c1 currc)
                  (if (< dist currdist) dist currdist)))
         currc))))
  ([xf c]
   (nearest-color v/dist xf c)))

(defn make-reduce-color-filter
  "Define reduce color filter to use on `Pixels`."
  ([pal]
   (partial nearest-color pal))
  ([f pal]
   (partial nearest-color f pal)))

(defn- html-color-fn
  "Return color by name from html color list"
  [n]
  (let [html-colors {:aquamarine
                     {:hex "7FFFD4", :rgb {:r 127, :g 255, :b 212}},
                     :lime {:hex "00FF00", :rgb {:r 0, :g 255, :b 0}},
                     :deepskyblue
                     {:hex "00BFFF", :rgb {:r 0, :g 191, :b 255}},
                     :darksalmon
                     {:hex "E9967A", :rgb {:r 233, :g 150, :b 122}},
                     :antiquewhite
                     {:hex "FAEBD7", :rgb {:r 250, :g 235, :b 215}},
                     :mediumturquoise
                     {:hex "48D1CC", :rgb {:r 72, :g 209, :b 204}},
                     :slategray
                     {:hex "708090", :rgb {:r 112, :g 128, :b 144}},
                     :sienna
                     {:hex "A0522D", :rgb {:r 160, :g 82, :b 45}},
                     :orange
                     {:hex "FFA500", :rgb {:r 255, :g 165, :b 0}},
                     :navajowhite
                     {:hex "FFDEAD", :rgb {:r 255, :g 222, :b 173}},
                     :lavenderblush
                     {:hex "FFF0F5", :rgb {:r 255, :g 240, :b 245}},
                     :firebrick
                     {:hex "B22222", :rgb {:r 178, :g 34, :b 34}},
                     :orangered
                     {:hex "FF4500", :rgb {:r 255, :g 69, :b 0}},
                     :palevioletred
                     {:hex "DB7093", :rgb {:r 219, :g 112, :b 147}},
                     :lawngreen
                     {:hex "7CFC00", :rgb {:r 124, :g 252, :b 0}},
                     :seashell
                     {:hex "FFF5EE", :rgb {:r 255, :g 245, :b 238}},
                     :lightpink
                     {:hex "FFB6C1", :rgb {:r 255, :g 182, :b 193}},
                     :darkolivegreen
                     {:hex "556B2F", :rgb {:r 85, :g 107, :b 47}},
                     :aliceblue
                     {:hex "F0F8FF", :rgb {:r 240, :g 248, :b 255}},
                     :gray
                     {:hex "808080", :rgb {:r 128, :g 128, :b 128}},
                     :lightsteelblue
                     {:hex "B0C4DE", :rgb {:r 176, :g 196, :b 222}},
                     :whitesmoke
                     {:hex "F5F5F5", :rgb {:r 245, :g 245, :b 245}},
                     :darkgoldenrod
                     {:hex "B8860B", :rgb {:r 184, :g 134, :b 11}},
                     :tan
                     {:hex "D2B48C", :rgb {:r 210, :g 180, :b 140}},
                     :bisque
                     {:hex "FFE4C4", :rgb {:r 255, :g 228, :b 196}},
                     :white
                     {:hex "FFFFFF", :rgb {:r 255, :g 255, :b 255}},
                     :lightgreen
                     {:hex "90EE90", :rgb {:r 144, :g 238, :b 144}},
                     :darkseagreen
                     {:hex "8FBC8F", :rgb {:r 143, :g 188, :b 143}},
                     :crimson
                     {:hex "DC143C", :rgb {:r 220, :g 20, :b 60}},
                     :darkslategray
                     {:hex "2F4F4F", :rgb {:r 47, :g 79, :b 79}},
                     :mistyrose
                     {:hex "FFE4E1", :rgb {:r 255, :g 228, :b 225}},
                     :chocolate
                     {:hex "D2691E", :rgb {:r 210, :g 105, :b 30}},
                     :yellow
                     {:hex "FFFF00", :rgb {:r 255, :g 255, :b 0}},
                     :cadetblue
                     {:hex "5F9EA0", :rgb {:r 95, :g 158, :b 160}},
                     :navy {:hex "000080", :rgb {:r 0, :g 0, :b 128}},
                     :ghostwhite
                     {:hex "F8F8FF", :rgb {:r 248, :g 248, :b 255}},
                     :seagreen
                     {:hex "2E8B57", :rgb {:r 46, :g 139, :b 87}},
                     :green {:hex "008000", :rgb {:r 0, :g 128, :b 0}},
                     :mediumseagreen
                     {:hex "3CB371", :rgb {:r 60, :g 179, :b 113}},
                     :indigo
                     {:hex "4B0082", :rgb {:r 75, :g 0, :b 130}},
                     :olivedrab
                     {:hex "6B8E23", :rgb {:r 107, :g 142, :b 35}},
                     :cyan {:hex "00FFFF", :rgb {:r 0, :g 255, :b 255}},
                     :peachpuff
                     {:hex "FFDAB9", :rgb {:r 255, :g 218, :b 185}},
                     :limegreen
                     {:hex "32CD32", :rgb {:r 50, :g 205, :b 50}},
                     :mediumslateblue
                     {:hex "7B68EE", :rgb {:r 123, :g 104, :b 238}},
                     :violet
                     {:hex "EE82EE", :rgb {:r 238, :g 130, :b 238}},
                     :sandybrown
                     {:hex "F4A460", :rgb {:r 244, :g 164, :b 96}},
                     :yellowgreen
                     {:hex "9ACD32", :rgb {:r 154, :g 205, :b 50}},
                     :mediumspringgreen
                     {:hex "00FA9A", :rgb {:r 0, :g 250, :b 154}},
                     :steelblue
                     {:hex "4682B4", :rgb {:r 70, :g 130, :b 180}},
                     :rosybrown
                     {:hex "BC8F8F", :rgb {:r 188, :g 143, :b 143}},
                     :cornflowerblue
                     {:hex "6495ED", :rgb {:r 100, :g 149, :b 237}},
                     :ivory
                     {:hex "FFFFF0", :rgb {:r 255, :g 255, :b 240}},
                     :lightgoldenrodyellow
                     {:hex "FAFAD2", :rgb {:r 250, :g 250, :b 210}},
                     :salmon
                     {:hex "FA8072", :rgb {:r 250, :g 128, :b 114}},
                     :darkcyan
                     {:hex "008B8B", :rgb {:r 0, :g 139, :b 139}},
                     :peru
                     {:hex "CD853F", :rgb {:r 205, :g 133, :b 63}},
                     :cornsilk
                     {:hex "FFF8DC", :rgb {:r 255, :g 248, :b 220}},
                     :lightslategray
                     {:hex "778899", :rgb {:r 119, :g 136, :b 153}},
                     :blueviolet
                     {:hex "8A2BE2", :rgb {:r 138, :g 43, :b 226}},
                     :forestgreen
                     {:hex "228B22", :rgb {:r 34, :g 139, :b 34}},
                     :lightseagreen
                     {:hex "20B2AA", :rgb {:r 32, :g 178, :b 170}},
                     :gold {:hex "FFD700", :rgb {:r 255, :g 215, :b 0}},
                     :gainsboro
                     {:hex "DCDCDC", :rgb {:r 220, :g 220, :b 220}},
                     :darkorchid
                     {:hex "9932CC", :rgb {:r 153, :g 50, :b 204}},
                     :burlywood
                     {:hex "DEB887", :rgb {:r 222, :g 184, :b 135}},
                     :lightskyblue
                     {:hex "87CEFA", :rgb {:r 135, :g 206, :b 250}},
                     :chartreuse
                     {:hex "7FFF00", :rgb {:r 127, :g 255, :b 0}},
                     :snow
                     {:hex "FFFAFA", :rgb {:r 255, :g 250, :b 250}},
                     :moccasin
                     {:hex "FFE4B5", :rgb {:r 255, :g 228, :b 181}},
                     :honeydew
                     {:hex "F0FFF0", :rgb {:r 240, :g 255, :b 240}},
                     :aqua {:hex "00FFFF", :rgb {:r 0, :g 255, :b 255}},
                     :darkred
                     {:hex "8B0000", :rgb {:r 139, :g 0, :b 0}},
                     :mediumorchid
                     {:hex "BA55D3", :rgb {:r 186, :g 85, :b 211}},
                     :lightsalmon
                     {:hex "FFA07A", :rgb {:r 255, :g 160, :b 122}},
                     :saddlebrown
                     {:hex "8B4513", :rgb {:r 139, :g 69, :b 19}},
                     :wheat
                     {:hex "F5DEB3", :rgb {:r 245, :g 222, :b 179}},
                     :springgreen
                     {:hex "00FF7F", :rgb {:r 0, :g 255, :b 127}},
                     :darkblue
                     {:hex "00008B", :rgb {:r 0, :g 0, :b 139}},
                     :powderblue
                     {:hex "B0E0E6", :rgb {:r 176, :g 224, :b 230}},
                     :turquoise
                     {:hex "40E0D0", :rgb {:r 64, :g 224, :b 208}},
                     :blanchedalmond
                     {:hex "FFEBCD", :rgb {:r 255, :g 235, :b 205}},
                     :papayawhip
                     {:hex "FFEFD5", :rgb {:r 255, :g 239, :b 213}},
                     :slateblue
                     {:hex "6A5ACD", :rgb {:r 106, :g 90, :b 205}},
                     :lightblue
                     {:hex "ADD8E6", :rgb {:r 173, :g 216, :b 230}},
                     :skyblue
                     {:hex "87CEEB", :rgb {:r 135, :g 206, :b 235}},
                     :red {:hex "FF0000", :rgb {:r 255, :g 0, :b 0}},
                     :lightyellow
                     {:hex "FFFFE0", :rgb {:r 255, :g 255, :b 224}},
                     :blue {:hex "0000FF", :rgb {:r 0, :g 0, :b 255}},
                     :palegreen
                     {:hex "98FB98", :rgb {:r 152, :g 251, :b 152}},
                     :greenyellow
                     {:hex "ADFF2F", :rgb {:r 173, :g 255, :b 47}},
                     :khaki
                     {:hex "F0E68C", :rgb {:r 240, :g 230, :b 140}},
                     :maroon {:hex "800000", :rgb {:r 128, :g 0, :b 0}},
                     :midnightblue
                     {:hex "191970", :rgb {:r 25, :g 25, :b 112}},
                     :floralwhite
                     {:hex "FFFAF0", :rgb {:r 255, :g 250, :b 240}},
                     :deeppink
                     {:hex "FF1493", :rgb {:r 255, :g 20, :b 147}},
                     :paleturquoise
                     {:hex "AFEEEE", :rgb {:r 175, :g 238, :b 238}},
                     :darkkhaki
                     {:hex "BDB76B", :rgb {:r 189, :g 183, :b 107}},
                     :azure
                     {:hex "F0FFFF", :rgb {:r 240, :g 255, :b 255}},
                     :indianred
                     {:hex "CD5C5C", :rgb {:r 205, :g 92, :b 92}},
                     :darkviolet
                     {:hex "9400D3", :rgb {:r 148, :g 0, :b 211}},
                     :mediumpurple
                     {:hex "9370DB", :rgb {:r 147, :g 112, :b 219}},
                     :fuchsia
                     {:hex "FF00FF", :rgb {:r 255, :g 0, :b 255}},
                     :coral
                     {:hex "FF7F50", :rgb {:r 255, :g 127, :b 80}},
                     :mediumvioletred
                     {:hex "C71585", :rgb {:r 199, :g 21, :b 133}},
                     :lemonchiffon
                     {:hex "FFFACD", :rgb {:r 255, :g 250, :b 205}},
                     :mediumblue
                     {:hex "0000CD", :rgb {:r 0, :g 0, :b 205}},
                     :darkmagenta
                     {:hex "8B008B", :rgb {:r 139, :g 0, :b 139}},
                     :goldenrod
                     {:hex "DAA520", :rgb {:r 218, :g 165, :b 32}},
                     :darkorange
                     {:hex "FF8C00", :rgb {:r 255, :g 140, :b 0}},
                     :orchid
                     {:hex "DA70D6", :rgb {:r 218, :g 112, :b 214}},
                     :plum
                     {:hex "DDA0DD", :rgb {:r 221, :g 160, :b 221}},
                     :pink
                     {:hex "FFC0CB", :rgb {:r 255, :g 192, :b 203}},
                     :teal {:hex "008080", :rgb {:r 0, :g 128, :b 128}},
                     :magenta
                     {:hex "FF00FF", :rgb {:r 255, :g 0, :b 255}},
                     :lightgrey
                     {:hex "D3D3D3", :rgb {:r 211, :g 211, :b 211}},
                     :purple
                     {:hex "800080", :rgb {:r 128, :g 0, :b 128}},
                     :dodgerblue
                     {:hex "1E90FF", :rgb {:r 30, :g 144, :b 255}},
                     :darkturquoise
                     {:hex "00CED1", :rgb {:r 0, :g 206, :b 209}},
                     :mintcream
                     {:hex "F5FFFA", :rgb {:r 245, :g 255, :b 250}},
                     :hotpink
                     {:hex "FF69B4", :rgb {:r 255, :g 105, :b 180}},
                     :thistle
                     {:hex "D8BFD8", :rgb {:r 216, :g 191, :b 216}},
                     :royalblue
                     {:hex "4169E1", :rgb {:r 65, :g 105, :b 225}},
                     :darkgreen
                     {:hex "006400", :rgb {:r 0, :g 100, :b 0}},
                     :darkslateblue
                     {:hex "483D8B", :rgb {:r 72, :g 61, :b 139}},
                     :silver
                     {:hex "C0C0C0", :rgb {:r 192, :g 192, :b 192}},
                     :darkgray
                     {:hex "A9A9A9", :rgb {:r 169, :g 169, :b 169}},
                     :oldlace
                     {:hex "FDF5E6", :rgb {:r 253, :g 245, :b 230}},
                     :mediumaquamarine
                     {:hex "66CDAA", :rgb {:r 102, :g 205, :b 170}},
                     :brown
                     {:hex "A52A2A", :rgb {:r 165, :g 42, :b 42}},
                     :olive
                     {:hex "808000", :rgb {:r 128, :g 128, :b 0}},
                     :lightcoral
                     {:hex "F08080", :rgb {:r 240, :g 128, :b 128}},
                     :tomato
                     {:hex "FF6347", :rgb {:r 255, :g 99, :b 71}},
                     :lightcyan
                     {:hex "E0FFFF", :rgb {:r 224, :g 255, :b 255}},
                     :linen
                     {:hex "FAF0E6", :rgb {:r 250, :g 240, :b 230}},
                     :lavender
                     {:hex "E6E6FA", :rgb {:r 230, :g 230, :b 250}},
                     :dimgray
                     {:hex "696969", :rgb {:r 105, :g 105, :b 105}},
                     :palegoldenrod
                     {:hex "EEE8AA", :rgb {:r 238, :g 232, :b 170}},
                     :beige
                     {:hex "F5F5DC", :rgb {:r 245, :g 245, :b 220}},
                     :black {:hex "000000", :rgb {:r 0, :g 0, :b 0}}}
        c (:rgb (html-colors n))
        r (:r c)
        g (:g c)
        b (:b c)]
    (make-awt-color r g b)))

(def html-awt-color (memoize html-color-fn))
(def html-color (memoize #(to-color (html-awt-color %))))
