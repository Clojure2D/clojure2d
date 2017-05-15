(ns clojure2d.color
  (:require [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.math.random :as r]
            [clojure.xml :as xml]
            [clojure.java.io :as io])
  (:import [clojure2d.math.vector Vec4 Vec3]
           [java.awt Color]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:dynamic ^double *blend-threshold* 0.5)

(defn clamp255
  ""
  [^double a]
  (int (m/constrain a 0.0 255.0)))

(defn mod255
  ""
  ^long [^long a]
  (bit-and 0xff a))

(defn clamp1
  ""
  ^double [^double v]
  (m/constrain v 0.0 1.0))

(defn mod1
  ""
  ^double [^double v]
  (m/abs (- v (m/rint v))))

(defn get-luma
  "get luma from color"
  ^double [^Vec4 c]
  (+ (* 0.212671 (.x c))
     (* 0.715160 (.y c))
     (* 0.072169 (.z c))))

(defn get-luma3
  "get luma from color"
  ^double [^Vec3 c]
  (+ (* 0.212671 (.x c))
     (* 0.715160 (.y c))
     (* 0.072169 (.z c))))

(declare to-HSB)

(defn get-hue
  ""
  ^double [^Vec4 c]
  (let [^Vec4 ret (to-HSB c)]
    (.x ret)))

(defn get-hue360
  ""
  ^double [c]
  (* 1.407843137254902 (get-hue c)))

(defn to-color
  ""
  [^Vec4 v]
  (Color. ^int (clamp255 (.x v))
          ^int (clamp255 (.y v))
          ^int (clamp255 (.z v))
          ^int (clamp255 (.w v))))

(defn to-color3
  ""
  [^Vec3 v]
  (Color. ^int (clamp255 (.x v))
          ^int (clamp255 (.y v))
          ^int (clamp255 (.z v))))

(defn from-color
  ""
  [^Color c]
  (Vec4. (.getRed c)
         (.getGreen c)
         (.getBlue c)
         (.getAlpha c)))

(defn from-color3
  ""
  [^Color c]
  (Vec3. (.getRed c)
         (.getGreen c)
         (.getBlue c)))

(defn make-color
  ""
  ([c]
   (if (instance? Vec4 c)
     (to-color c)
     (if (instance? Vec3 c)
       (to-color3 c)
       c)))
  ([r g b]
   (Color. ^int (clamp255 r)
           ^int (clamp255 g)
           ^int (clamp255 b)))
  ([r g b a]
   (Color. ^int (clamp255 r)
           ^int (clamp255 g)
           ^int (clamp255 b)
           ^int (clamp255 a))))

(defn set-alpha
  "Set alpha channel"
  [^Vec4 v ^double a]
  (Vec4. (.x v) (.y v) (.z v) a))


;; blending part, operate on range 0-1

(def r255 (double-array (map #(/ ^double % 255.0) (range 256))))

;;; Blend colors functions

(defn convert-and-blend
  ""
  ^long [f ^long a ^long b]
  (let [aa (aget ^doubles r255 a)
        bb (aget ^doubles r255 b)]
    (long (* 255.0 ^double (f aa bb)))))

(defn blend-none
  ""
  ^double [a b] a)

(defn blend-add
  ""
  ^double [^double a ^double b]
  (clamp1 (+ a b)))

(defn blend-madd
  ""
  ^double [^double a ^double b]
  (mod1 (+ a b)))

(defn blend-subtract
  ""
  ^double [^double a ^double b]
  (clamp1 (- a b)))

(defn blend-msubtract
  ""
  ^double [^double a ^double b]
  (mod1 (- a b)))

(defn blend-linearburn
  ""
  ^double [^double a ^double b]
  (clamp1 (- (+ a b) 1.0)))

(defn blend-mlinearburn
  ""
  ^double [^double a ^double b]
  (mod1 (- (+ a b) 1.0)))

(defn blend-darken
  ""
  ^double [^double a ^double b]
  (if (> a b) b a))

(defn blend-lighten
  ""
  ^double [^double a ^double b]
  (if (< a b) b a))

(defn blend-multiply
  ""
  ^double [^double a ^double b]
  (* a b))

(defn blend-screen
  ""
  ^double [^double a ^double b]
  (let [ra (- 1.0 a)
        rb (- 1.0 b)]
    (->> ra
         (* rb)
         (- 1.0))))

(defn blend-dodge
  ""
  ^double [^double a ^double b]
  (clamp1 (->> b
               (- 1.0)
               (/ a))))

(defn blend-mdodge
  ""
  ^double [^double a ^double b]
  (mod1 (->> b
               (- 1.0)
               (/ a))))

(defn blend-burn
  ""
  ^double [^double a ^double b]
  (clamp1 (->> b
               (/ (- 1.0 a))
               (- 1.0))))

(defn blend-mburn
  ""
  ^double [^double a ^double b]
  (mod1 (->> b
             (/ (- 1.0 a))
             (- 1.0))))

(defn blend-hardmix
  ""
  ^double [^double a ^double b]
  (let [t (- 1.0 b)]
    (cond (< a t) 0.0
          (> a t) 1.0
          :else a)))

(defn blend-linearlight
  ""
  ^double [^double a ^double b]
  (clamp1 (-> b
              (+ a)
              (+ a)
              (- 1.0))))

(defn blend-mlinearlight
  ""
  ^double [^double a ^double b]
  (mod1 (-> b
            (+ a)
            (+ a)
            (- 1.0))))

(defn blend-pegtoplight
  ""
  ^double [^double a ^double b]
  (let [ab (* a b)]
    (clamp1 (->> b
                 (- 1.0)
                 (* a a)
                 (+ ab)
                 (+ ab)))))

(defn blend-mpegtoplight
  ""
  ^double [^double a ^double b]
  (let [ab (* a b)]
    (mod1 (->> b
               (- 1.0)
               (* a a)
               (+ ab)
               (+ ab)))))

(defn blend-difference
  ""
  ^double [^double a ^double b]
  (m/abs (- a b)))

(defn blend-divide
  ""
  ^double [^double a ^double b]
  (clamp1 (/ a (+ b m/EPSILON))))

(defn blend-mdivide
  ""
  ^double [^double a ^double b]
  (mod1 (/ a (+ b m/EPSILON))))

(defn blend-or
  ""
  ^double [^double a ^double b]
  (let [aa (long (* a 255.0))
        bb (long (* b 255.0))]
    (aget ^doubles r255 (bit-and 0xff (bit-or aa bb)))))

(defn blend-and
  ""
  ^double [^double a ^double b]
  (let [aa (long (* a 255.0))
        bb (long (* b 255.0))]
    (aget ^doubles r255 (bit-and 0xff (bit-and aa bb)))))

(defn blend-xor
  ""
  ^double [^double a ^double b]
  (let [aa (long (* a 255.0))
        bb (long (* b 255.0))]
    (aget ^doubles r255 (bit-and 0xff (bit-xor aa bb)))))

(defn blend-exclusion
  ""
  ^double [^double a ^double b]
  (let [ab (* a b)]
    (- (+ a b) (+ ab ab))))

(defn blend-pinlight-raw
  ""
  ^double [^double a ^double b]
  (let [c (- (+ a a) 1.0)]
    (cond (< b c) c
          (and (<= c b) (< b (+ c 1.0))) b
          :else (+ c 1.0))))

(defn blend-pinlight
  ""
  ^double [a b]
  (clamp1 (blend-pinlight-raw a b)))

(defn blend-mpinlight
  ""
  ^double [a b]
  (mod1 (blend-pinlight-raw a b)))

(defn blend-opacity
  ""
  (^double [^double a ^double b ^double thr]
   (m/lerp a b thr))
  (^double [^double a ^double b]
   (m/lerp a b *blend-threshold*)))

(defn blend-overlay-raw
  ""
  (^double [^double a ^double b ^double thr]
   (if (< a thr)
     (* 2.0 (* a b))
     (- 1.0 (* 2.0 (* (- 1.0 a) (- 1.0 b))))))
  (^double [a b]
   (blend-overlay-raw a b *blend-threshold*)))

(defn blend-overlay
  ""
  ^double [a b]
  (clamp1 (blend-overlay-raw a b)))

(defn blend-moverlay
  ""
  ^double [a b]
  (mod1 (blend-overlay-raw a b)))

(defn blend-hardlight-raw
  ""
  (^double [^double a ^double b ^double thr]
   (if (< b thr)
     (* 2.0 (* a b))
     (- 1.0 (* 2.0 (* (- 1.0 a) (- 1.0 b))))))
  (^double [a b]
   (blend-hardlight-raw a b *blend-threshold*)))

(defn blend-hardlight
  ""
  ^double [a b]
  (clamp1 (blend-hardlight-raw a b)))

(defn blend-mhardlight
  ""
  ^double [a b]
  (mod1 (blend-hardlight-raw a b)))

(defn blend-softlight-raw
  ""
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
  ""
  ^double [a b]
  (clamp1 (blend-softlight-raw a b)))

(defn blend-msoftlight
  ""
  ^double [a b]
  (mod1 (blend-softlight-raw a b)))

(defn blend-vividlight-raw
  ""
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
  ""
  ^double [a b]
  (clamp1 (blend-vividlight-raw a b)))

(defn blend-mvividlight
  ""
  ^double [a b]
  (mod1 (blend-vividlight-raw a b)))

(defn blend-darkthreshold
  ""
  (^double [^double a ^double b ^double thr]
   (if (< a thr) a b))
  (^double [a b]
   (blend-darkthreshold a b *blend-threshold*)))

(defn blend-lightthreshold
  ""
  (^double [^double a ^double b ^double thr]
   (if (> a thr) a b))
  (^double [a b]
   (blend-lightthreshold a b *blend-threshold*)))

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

(def blends-names (keys blends))

;;; Colorspace functions

(defn test-colors
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


;; CMY

(defn to-CMY
  "RGB -> CMY"
  [^Vec4 c]
  (Vec4. (- 255 (.x c))
         (- 255 (.y c))
         (- 255 (.z c))
         (.w c)))

(def from-CMY to-CMY)

;; OHTA

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

;; XYZ

(defn- xyz-correct
  ""
  ^double [^double v]
  (if (> v 0.04045)
    (m/pow (/ (+ 0.055 v) 1.055) 2.4)
    (/ v 12.92)))

(def ^:const ^double xyz-xmax 0.9504716671128306)
(def ^:const ^double xyz-ymax 0.9999570331323426)
(def ^:const ^double xyz-zmax 1.0889782052041752)

(defn to-XYZ-raw
  ""
  ^Vec3 [^Vec3 c]
  (Vec3. (+ (* (.x c) 0.41239558896741421610) (* (.y c) 0.35758343076371481710) (* (.z c) 0.18049264738170157350))
         (+ (* (.x c) 0.21258623078559555160) (* (.y c) 0.71517030370341084990) (* (.z c) 0.07220049864333622685))
         (+ (* (.x c) 0.01929721549174694484) (* (.y c) 0.11918386458084853180) (* (.z c) 0.95049712513157976600))))

(defn- to-XYZ-
  ""
  [^Vec4 c]
  (let [r (xyz-correct (/ (.x c) 255.0))
        g (xyz-correct (/ (.y c) 255.0))
        b (xyz-correct (/ (.z c) 255.0))
        ^Vec3 xyz-raw (to-XYZ-raw (Vec3. r g b))]
    (Vec4. (.x xyz-raw) (.y xyz-raw) (.z xyz-raw) (.w c))))

(defn to-XYZ
  ""
  [c]
  (let [^Vec4 cc (to-XYZ- c)]
    (Vec4. (clamp255 (m/norm (.x cc) 0.0 xyz-xmax 0.0 255.0))
           (clamp255 (m/norm (.y cc) 0.0 xyz-ymax 0.0 255.0))
           (clamp255 (m/norm (.z cc) 0.0 xyz-zmax 0.0 255.0))
           (.w cc))))

(def ^:const ^double xyz-f (/ 1.0 2.4))

(defn- xyz-decorrect
  ""
  ^double [^double v]
  (if (> v 0.0031308)
    (- (* 1.055 (m/pow v xyz-f)) 0.055)
    (* v 12.92)))

(defn from-XYZ-raw
  ""
  ^Vec3 [^Vec3 v]
  (Vec3. (+ (* (.x v)  3.2406) (* (.y v) -1.5372) (* (.z v) -0.4986))
         (+ (* (.x v) -0.9689) (* (.y v)  1.8758) (* (.z v)  0.0415))
         (+ (* (.x v)  0.0557) (* (.y v) -0.2040) (* (.z v)  1.0570))))

(defn- from-XYZ-
  ""
  [^Vec4 c]
  (let [^Vec3 rgb-raw (from-XYZ-raw (.x c) (.y c) (.z c))
        r (xyz-decorrect (.x rgb-raw))
        g (xyz-decorrect (.y rgb-raw))
        b (xyz-decorrect (.z rgb-raw))]
    (Vec4. (* 255.0 r)
           (* 255.0 g)
           (* 255.0 b)
           (.w c))))

(defn from-XYZ
  ""
  [^Vec4 c]
  (let [x (m/norm (.x c) 0.0 255.0 0.0 xyz-xmax)
        y (m/norm (.y c) 0.0 255.0 0.0 xyz-ymax)
        z (m/norm (.z c) 0.0 255.0 0.0 xyz-zmax)
        ^Vec4 rgb (from-XYZ- (Vec4. x y z (.w c)))]
    (v/applyf rgb clamp255)))

;; LUV

(def ^:const ^double D65X 0.950456)
(def ^:const ^double D65Z 1.088754)
(def ^:const ^double CIEEpsilon (/ 216.0 24389.0))
(def ^:const ^double CIEK (/ 24389.0 27.0))
(def ^:const ^double OneThird (/ 1.0 3.0))
(def ^:const ^double D65FX-4 (/ (* 4.0 D65X) (+ D65X 15 (* 3.0 D65Z))))
(def ^:const ^double D65FY-9 (/ 9.0 (+ D65X 15 (* 3.0 D65Z))))

(defn perceptible-reciprocal
  ""
  ^double [^double x]
  (if (>= (m/abs x) m/EPSILON)
    (/ 1.0 x)
    (/ (m/sgn x) m/EPSILON)))

(defn to-LUV
  ""
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
  ""
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

(defn- to-lab-correct
  ""
  ^double [^double v]
  (if (> v CIEEpsilon)
    (m/pow v OneThird)
    (/ (+ 16.0 (* v CIEK)) 116.0)))

(defn to-LAB
  ""
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
  ""
  ^double [^double v]
  (let [v3 (* v v v)]
    (if (> v3 CIEEpsilon)
      v3
      (/ (- (* 116.0 v) 16.0) CIEK))))

(defn from-LAB
  ""
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

(defn to-YXY
  ""
  [^Vec4 c]
  (let [^Vec4 xyz (to-XYZ- c)
        d (+ (.x xyz) (.y xyz) (.z xyz))
        Y (m/norm (.y xyz) 0.0 0.9999570331323426 0.0 255.0)
        x (m/norm (/ (.x xyz) d) 0.150011724420108 0.6400884809339611 0.0 255.0)
        y (m/norm (/ (.y xyz) d) 0.060007548576610774 0.6000064972148145 0.0 255.0)]
    (v/applyf (Vec4. Y x y (.w c)) clamp255)))

(defn from-YXY
  ""
  [^Vec4 c]
  (let [^double Y (m/norm (.x c) 0.0 255.0 0.0 0.9999570331323426)
        ^double x (m/norm (.y c) 0.0 255.0 0.150011724420108 0.6400884809339611)
        ^double y (m/norm (.z c) 0.0 255.0 0.060007548576610774 0.6000064972148145)
        Yy (/ Y y)
        X (* x Yy)
        Z (* (- 1.0 x y) Yy)
        ^Vec4 rgb (from-XYZ- (Vec4. X Y Z (.w c)))]
    (v/applyf rgb clamp255)))

(defn to-HCL
  ""
  [^Vec4 c]
  (let [mx (max (.x c) (.y c) (.z c))
        chr (- mx (min (.x c) (.y c) (.z c)))
        h (* 255.0 (/ (if (zero? chr) 0.0
                          (double (condp == mx
                                    (.x c) (rem (+ 6.0 (/ (- (.y c) (.z c)) chr)) 6.0)
                                    (.y c) (+ 2.0 (/ (- (.z c) (.x c)) chr))
                                    (.z c) (+ 4.0 (/ (- (.x c) (.y c)) chr))))) 6.0))
        luma (+ (* 0.298839 (.x c)) (* 0.586811 (.y c)) (* 0.114350 (.z c)))]
    (v/applyf (Vec4. h chr luma (.w c)) clamp255)))

(defn from-HCL
  ""
  [^Vec4 c]
  (let [h (* 6.0 (/ (.x c) 255.0))
        chr (.y c)
        l (.z c)
        x (* chr (- 1.0 (m/abs (dec ^double (rem h 2.0)))))
        rgb (cond
              (and (<= 0.0 h) (< h 1.0)) [chr x 0.0]
              (and (<= 1.0 h) (< h 2.0)) [x chr 0.0]
              (and (<= 2.0 h) (< h 3.0)) [0.0 chr x]
              (and (<= 3.0 h) (< h 4.0)) [0.0 x chr]
              (and (<= 4.0 h) (< h 5.0)) [x 0.0 chr]
              :else                      [chr 0.0 x])
        ^double r (rgb 0)
        ^double g (rgb 1)
        ^double b (rgb 2)
        m (- l (* 0.298839 r) (* 0.586811 g) (* 0.114350 b))]
    (v/applyf (Vec4. (+ r m) (+ g m) (+  m) (.w c)) clamp255)))

(defn to-HSB
  ""
  [^Vec4 c]
  (let [mn (min (.x c) (.y c) (.z c))
        mx (max (.x c) (.y c) (.z c))
        delta (- mx mn)
        [h s b] (if (zero? mx) [0.0 0.0 0.0]
                    (let [s (* 255.0 (/ delta mx))
                          h (if (zero? delta) 0.0 
                                (/ (double (condp == mx
                                             (.x c) (/ (- (.y c) (.z c)) delta)
                                             (.y c) (+ 2.0 (/ (- (.z c) (.x c)) delta))
                                             (.z c) (+ 4.0 (/ (- (.x c) (.y c)) delta)))) 6.0))]
                      [(* 255.0 (if (neg? h) (inc h) h)) s mx]))]
    (v/applyf (Vec4. h s b (.w c)) clamp255)))

(m/remainder 123.01 22.1)

(defn from-HSB
  ""
  [^Vec4 c]
  (if (zero? (.y c)) (Vec4. (.z c) (.z c) (.z c) (.w c))
    (let [h (/ (.x c) 255.0)
          s (/ (.y c) 255.0)
          b (/ (.z c) 255.0)
          h (* 6.0 (- h (m/floor h)))
          f (- h (m/floor h))
          p (* b (- 1.0 s))
          q (* b (- 1.0 (* s f)))
          t (* b (- 1.0 (* s (- 1.0 f))))
          rgb (condp == (int h)
                0 [b t p]
                1 [q b p]
                2 [p b t]
                3 [p q b]
                4 [t p b]
                5 [b p q])
          ^double r (rgb 0)
          ^double g (rgb 1)
          ^double b (rgb 2)]
      (v/applyf (Vec4. (* 255.0 r) (* 255.0 g) (* 255.0 b) (.w c)) clamp255))))


(def ^:const ^double to-hsi-const (-> 180.0
                                      (/ m/PI)
                                      (/ 360.0)))

(defn to-HSI
  ""
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
  ""
  [^Vec4 c]
  (let [^Vec4 cc (v/div c 255.0)
        h (* 360.0 (.x cc))
        h (- h (* 360.0 (m/floor (/ h 360.0))))
        v1 (* (.z cc) (- 1.0 (.y cc)))
        rgb (cond
              (< h 120.0) (let [b v1
                                r (from-hsi-helper cc h)
                                g (- (* 3.0 (.z cc)) r b)]
                            [r g b])
              (< h 240.0) (let [r v1
                                g (from-hsi-helper cc (- h 120.0))
                                b (- (* 3.0 (.z cc)) r g)]
                            [r g b])
              :else (let [g v1
                          b (from-hsi-helper cc (- h 240.0))
                          r (- (* 3.0 (.z cc)) g b)]
                      [r g b]))]
    (v/applyf (v/mult (Vec4. (rgb 0) (rgb 1) (rgb 2) (.w cc)) 255.0) clamp255)))

(defn to-HWB
  ""
  [^Vec4 c]
  (let [w (min (.x c) (.y c) (.z c))
        v (max (.x c) (.y c) (.z c))
        h (if (== w v) 0.0
              (let [^double f (condp == w
                                (.x c) (- (.y c) (.z c))
                                (.y c) (- (.z c) (.x c))
                                (.z c) (- (.x c) (.y c)))
                    ^double p (condp == w
                                (.x c) 3.0
                                (.y c) 5.0
                                (.z c) 1.0)]
                (m/norm (/ (- p (/ f (- v w))) 6.0) 0.0 1.0 1.0 255.0)))]
    (v/applyf (Vec4. h w (- 255.0 v) (.w c)) clamp255)))

(defn from-HWB
  ""
  [^Vec4 c]
  (if (< (.x c) 1.0) 
    (let [v (- 255.0 (.z c))]
      (Vec4. v v v (.w c)))
    (let [^double h (m/norm (.x c) 1.0 255.0 0.0 6.0)
          v (- 1.0 (/ (.z c) 255.0))
          w (/ (.y c) 255.0)
          i (int (m/floor h))
          f (- h i)
          f (if (odd? i) (- 1.0 f) f)
          n (+ w (* f (- v w)))
          rgb (condp == i
                0 [v n w]
                1 [n v w]
                2 [w v n]
                3 [w n v]
                4 [n w v]
                5 [v w n]
                6 [v n w])]
      (v/applyf (Vec4. (* 255.0 ^double (rgb 0)) (* 255.0 ^double (rgb 1)) (* 255.0 ^double (rgb 2)) (.w c)) clamp255))))


;; YPbPr
;; Luma + channel differences
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

;; 

(defn to-YDbDr
  ""
  [^Vec4 c]
  (let [Y (+ (* 0.299 (.x c)) (* 0.587 (.y c)) (* 0.114 (.z c)))
        Db (+ (* -0.45 (.x c)) (* -0.883 (.y c)) (* 1.333 (.z c)))
        Dr (+ (* -1.333 (.x c)) (* 1.116 (.y c)) (* 0.217 (.z c)))]
    (v/applyf (Vec4. Y
                     (m/norm Db -339.91499999999996 339.91499999999996 0.0 255.0)
                     (m/norm Dr -339.91499999999996 339.915 0.0 255.0)
                     (.w c)) clamp255)))

(defn from-YDbDr
  ""
  [^Vec4 c]
  (let [Y (.x c)
        ^double Db (m/norm (.y c) 0.0 255.0 -339.91499999999996 339.91499999999996)
        ^double Dr (m/norm (.z c) 0.0 255.0 -339.91499999999996 339.915)
        r (+ Y (* 9.2303716147657e-05 Db) (* -0.52591263066186533 Dr))
        g (+ Y (* -0.12913289889050927 Db) (* 0.26789932820759876 Dr))
        b (+ Y (* 0.66467905997895482 Db) (* -7.9202543533108e-05 Dr))]
    (v/applyf (Vec4. r g b (.w c)) clamp255)))

;; JPEG version
(defn to-YCbCr
  ""
  [^Vec4 c]
  (let [Y (+ (* 0.298839 (.x c)) (* 0.586811 (.y c)) (* 0.114350 (.z c)))
        Cb (+ 127.5 (* -0.168736 (.x c)) (* -0.331264 (.y c)) (* 0.5 (.z c)))
        Cr (+ 127.5 (* 0.5 (.x c)) (* -0.418688 (.y c)) (* -0.081312 (.z c)))]
    (v/applyf (Vec4. Y Cb Cr (.w c)) clamp255)))

(defn from-YCbCr
  ""
  [^Vec4 c]
  (let [Cb (- (.y c) 127.5)
        Cr (- (.z c) 127.5)
        r (+ (* 0.99999999999914679361 (.x c)) (* -1.2188941887145875e-06 Cb) (* 1.4019995886561440468 Cr))
        g (+ (* 0.99999975910502514331 (.x c)) (* -0.34413567816504303521 Cb) (* -0.71413649331646789076 Cr))
        b (+ (* 1.00000124040004623180 (.x c)) (* 1.77200006607230409200 Cb) (* 2.1453384174593273e-06 Cr))]
    (v/applyf (Vec4. r g b (.w c)) clamp255)))

(defn to-YUV
  ""
  [^Vec4 c]
  (let [Y (+ (* 0.298839 (.x c)) (* 0.586811 (.y c)) (* 0.114350 (.z c)))
        U (+ (* -0.147 (.x c)) (* -0.289 (.y c)) (* 0.436 (.z c)))
        V (+ (* 0.615 (.x c)) (* -0.515 (.y c)) (* -0.1 (.z c)))]
    (v/applyf (Vec4. Y 
                     (m/norm U -111.17999999999999 111.17999999999999 0.0 255.0)
                     (m/norm V -156.82500000000002 156.825 0.0 255.0)
                     (.w c)) clamp255)))

(defn from-YUV
  ""
  [^Vec4 c]
  (let [Y (.x c)
        ^double U (m/norm (.y c) 0.0 255.0 -111.17999999999999 111.17999999999999)
        ^double V (m/norm (.z c) 0.0 255.0 -156.82500000000002 156.825)
        r (+ Y (* -3.945707070708279e-05 U) (* 1.1398279671717170825 V))
        g (+ Y (* -0.3946101641414141437 U) (* -0.5805003156565656797 V))
        b (+ Y (* 2.0319996843434342537 U) (* -4.813762626262513e-04 V))]
    (v/applyf (Vec4. r g b (.w c)) clamp255)))


(defn to-YIQ
  ""
  [^Vec4 c]
  (let [Y (+ (* 0.298839 (.x c)) (* 0.586811 (.y c)) (* 0.114350 (.z c)))
        I (+ (* 0.595716 (.x c)) (* -0.274453 (.y c)) (* -0.321263 (.z c)))
        Q (+ (* 0.211456 (.x c)) (* -0.522591 (.y c)) (* 0.311135 (.z c)))]
    (v/applyf (Vec4. Y 
                     (m/norm I -151.90758 151.90758 0.0 255.0)
                     (m/norm Q -133.260705 133.260705 0.0 255.0)
                     (.w c)) clamp255)))

(defn from-YIQ
  ""
  [^Vec4 c]
  (let [Y (.x c)
        ^double I (m/norm (.y c) 0.0 255.0 -151.90758 151.90758)
        ^double Q (m/norm (.z c) 0.0 255.0 -133.260705 133.260705)
        r (+ Y (* +0.9562957197589482261 I) (* 0.6210244164652610754 Q))
        g (+ Y (* -0.2721220993185104464 I) (* -0.6473805968256950427 Q))
        b (+ Y (* -1.1069890167364901945 I) (* 1.7046149983646481374 Q))]
    (v/applyf (Vec4. r g b (.w c)) clamp255)))


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
                  :YUV   [to-YUV from-YUV]
                  :YIQ   [to-YIQ from-YIQ]})

(def colorspaces-names (keys colorspaces))

(defn to-cs
  "return colorspace converter by keyword (RGB -> ...)"
  [cs]
  ((cs colorspaces) 0))

(defn from-cs
  "return colorspace converter by keyword (... -> RGB)"
  [cs]
  ((cs colorspaces) 1))


;;;; read 200 palettes from colourlovers
;;

(defn hex-to-vec
  ""
  [s]
  (let [x (Long/parseLong s 16)
        x1 (bit-and 0xff (bit-shift-right x 16))
        x2 (bit-and 0xff (bit-shift-right x 8))
        x3 (bit-and 0xff x)]
    (Vec4. x1 x2 x3 255)))

(defn hex-to-vecs
  ""
  [xs]
  (mapv hex-to-vec xs))

(def palettes
  (let [p1 (xml/parse (io/file (io/resource "colourlovers1.xml")))
        p2 (xml/parse (io/file (io/resource "colourlovers2.xml")))
        f (fn [xml-in] (map (fn [x] (map #((:content %) 0) (->> x
                                                                ((:content xml-in))
                                                                :content
                                                                (filter #(= (:tag %) :colors))
                                                                first
                                                                :content))) (range 100)))
        l1 (f p1)
        l2 (f p2)]
    (mapv hex-to-vecs (concat l1 l2))))

;; 

;; http://iquilezles.org/www/articles/palettes/palettes.htm
(defn create-palette-fn
  ""
  [a b c d]
  (fn [t]
    (let [^Vec3 cc (-> (->> t
                            (v/mult c)
                            (v/add d))
                       (v/mult m/TWO_PI)
                       (v/applyf m/cos)
                       (v/emult b)
                       (v/add a))]
      (-> (Vec4. (.x cc) (.y cc) (.z cc) 1.0)
          (v/mult 255)
          (v/applyf clamp255)))))

(defn make-random-palette
  ""
  [^long num]
  (let [a (v/generate-vec3 (partial r/drand 0.3 0.7))
        b (v/sub (Vec3. 1.0 1.0 1.1) a)
        c (v/generate-vec3 (partial r/drand 2))
        d (v/generate-vec3 r/drand)
        f (create-palette-fn a b c d)]
    (mapv f (range 0.0 1.0 (/ 1.0 num)))))

;; paletton palettes

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
                      (- 210 (* 2.0 (/ (* (m/atan (/ e 0.75)) 30.0) m/PI)))))
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
  ""
  [^double hue ^double ks ^double kv]
  (let [ks (m/constrain ks 0.0 2.0)
        kv (m/constrain kv 0.0 2.0)
        ^double h (mod hue 360.0)
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
  ""
  (^double [^double r ^double g ^double b]
   (if (== r g b)
     0.0
     (let [f (max r g b)
           p (min r g b)
           [l i] (if (== f r)
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

(def paletton-presets-names (keys paletton-presets))

(defn make-monochromatic-palette
  ""
  [hue preset]
  (mapv (fn [[ks kv]] (paletton-hsv-to-rgb hue ks kv)) preset))

(defmulti paletton-palette (fn [m hue & conf] m))

(defmethod paletton-palette :monochromatic [_ hue & conf]
  (let [{compl :compl 
         preset :preset
         :or {compl false
              preset :full}} (first conf)
        ppreset (if (keyword? preset) (paletton-presets preset) preset)
        p (make-monochromatic-palette hue ppreset)]
    (if compl (vec (concat p (make-monochromatic-palette (+ ^double hue 180.0) ppreset))) p)))

(defmethod paletton-palette :triad [_ hue & conf]
  (let [{compl :compl
         preset :preset
         angle :angle
         adj :adj
         :or {compl false
              preset :full
              angle 30.0
              adj true}} (first conf)
        chue (+ 180.0 ^double hue)
        hue1 (if adj (+ ^double hue ^double angle) (+ chue ^double angle))
        hue2 (if adj (- ^double hue ^double angle) (- chue ^double angle))
        ppreset (if (keyword? preset) (paletton-presets preset) preset)
        p1 (make-monochromatic-palette hue ppreset)
        p2 (make-monochromatic-palette hue1 ppreset)
        p3 (make-monochromatic-palette hue2 ppreset)
        p (vec (concat p1 p2 p3))]
    (if compl (vec (concat p (make-monochromatic-palette chue ppreset))) p)))

(defmethod paletton-palette :tetrad [_ hue & conf]
  (let [{preset :preset
         angle :angle
         :or {preset :full
              angle 30.0}} (first conf)
        p1 (paletton-palette :monochromatic hue {:preset preset :compl true})
        p2 (paletton-palette :monochromatic (+ ^double angle ^double hue) {:preset preset :compl true})]
    (vec (concat p1 p2))))

;;

(defn nearest-color
  ""
  ([f xf c]
   (let [s (count xf)]
     (loop [i (int 0)
            currc c
            currdist (double Double/MAX_VALUE)]
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
  ""
  ([]
   (partial nearest-color (rand-nth palettes)))
  ([pal]
   (partial nearest-color pal))
  ([f pal]
   (partial nearest-color f pal)))

;; 
