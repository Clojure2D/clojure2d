(ns clojure2d.color
  (:require [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure.xml :as xml]
            [clojure.java.io :as io])
  (:import [clojure2d.math.vector Vec4 Vec3]
           [java.awt Color]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:dynamic *blend-threshold* 128)

(defn clamp255
  ""
  [a]
  (m/constrain (m/round a) 0 255))

(defn mod255
  ""
  [a]
  (bit-and 0xff (int a)))

(defn get-luma
  "get luma from color"
  [^Vec4 c]
  (m/round (+ (* 0.2126 (.x c))
              (* 0.7152 (.y c))
              (* 0.0722 (.z c)))))

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


;; blending part

(defn- umult
  ""
  ([a b]
   (-> (unchecked-int a)
       (* (unchecked-int b))
       (/ 255.0)
       (int)))
  ([a b c]
   (-> (unchecked-int a)
       (* (unchecked-int b)
          (unchecked-int c))
       (/ 65025.0)
       (int))))

(defn- udiv
  ""
  [a b]
  (let [bb (if (zero? b) 1 b)
        aa (unchecked-int a)] 
    (-> aa
        (bit-shift-left 8)
        (- aa)
        (/ (unchecked-int bb))
        (int))))

(defn- uadd
  ""
  [a b]
  (+ (unchecked-int a) (unchecked-int b)))

(defn- usub
  ""
  [a b]
  (- (unchecked-int a) (unchecked-int b)))

;;; Blend colors functions

(defn blend-none
  ""
  [a b] a)

(def blend-add (comp clamp255 uadd))
(def blend-madd (comp mod255 uadd))

(def blend-subtract (comp clamp255 #(usub %2 %1)))
(def blend-msubtract (comp mod255 #(usub %2 %1)))

(defn blend-linearburn-raw
  ""
  [a b]
  (usub (uadd a b) 255))

(def blend-linearburn (comp clamp255 blend-linearburn-raw))
(def blend-mlinearburn (comp mod255 blend-linearburn-raw))

(defn blend-darken
  ""
  [a b]
  (if (> a b) b a))

(defn blend-lighten
  ""
  [a b]
  (if (< a b) b a))

(def blend-multiply umult)

(defn blend-screen
  ""
  [a b]
  (let [ra (usub 255 a)
        rb (usub 255 b)]
    (->> ra
         (umult rb)
         (usub 255))))

(defn blend-dodge-raw
  ""
  [a b]
  (->> b
       (usub 255)
       (udiv a)))

(def blend-dodge (comp clamp255 blend-dodge-raw))
(def blend-mdodge (comp mod255 blend-dodge-raw))

(defn blend-burn-raw
  ""
  [a b]
  (->> b
       (udiv (usub 255 a))
       (usub 255)))

(def blend-burn (comp clamp255 blend-burn-raw))
(def blend-mburn (comp mod255 blend-burn-raw))

(defn blend-hardmix
  ""
  [a b]
  (let [t (usub 255 b)]
    (cond (< a t) 0
          (> a t) 255
          :else a)))

(defn blend-linearlight-raw
  ""
  [a b]
  (-> b
      (uadd a)
      (uadd a)
      (usub 255)))

(def blend-linearlight (comp clamp255 blend-linearlight-raw))
(def blend-mlinearlight (comp mod255 blend-linearlight-raw))

(defn blend-pegtoplight-raw
  ""
  [a b]
  (let [ab (umult a b)]
    (->> b
         (usub 255)
         (umult a a)
         (uadd ab)
         (uadd ab))))

(def blend-pegtoplight (comp clamp255 blend-pegtoplight-raw))
(def blend-mpegtoplight (comp mod255 blend-pegtoplight-raw))

(defn blend-difference
  ""
  [a b]
  (let [a-b (usub a b)]
    (if (neg? a-b)
      (- a-b)
      a-b)))

(def blend-divide (comp clamp255 udiv))
(def blend-mdivide (comp mod255 udiv))

(def blend-or (comp mod255 bit-or))
(def blend-and (comp mod255 bit-and))
(def blend-xor (comp mod255 bit-xor))

(defn blend-exclusion
  ""
  [a b]
  (let [ab (umult a b)]
    (usub (uadd a b) (uadd ab ab))))

(defn blend-pinlight-raw
  ""
  [a b]
  (let [c (usub (uadd a a) 255)]
    (cond (< b c) c
          (and (<= c b) (< b (uadd c 255))) b
          :else (uadd c 255))))

(def blend-pinlight (comp clamp255 blend-pinlight-raw))
(def blend-mpinlight (comp mod255 blend-pinlight-raw))

(defn blend-opacity
  ""
  ([a b thr]
   (int (m/lerp a b (/ thr 255.0))))
  ([a b]
   (blend-opacity a b *blend-threshold*)))

(defn blend-overlay-raw
  ""
  ([a b thr]
   (if (< a thr)
     (* 2 (umult a b))
     (usub 255 (* 2 (umult (usub 255 a) (usub 255 b))))))
  ([a b]
   (blend-overlay-raw a b *blend-threshold*)))

(def blend-overlay (comp clamp255 blend-overlay-raw))
(def blend-moverlay (comp mod255 blend-overlay-raw))

(defn blend-hardlight-raw
  ""
  ([a b thr]
   (if (< b thr)
     (* 2 (umult a b))
     (usub 255 (* 2 (umult (usub 255 a) (usub 255 b))))))
  ([a b]
   (blend-hardlight-raw a b *blend-threshold*)))

(def blend-hardlight (comp clamp255 blend-hardlight-raw))
(def blend-mhardlight (comp mod255 blend-hardlight-raw))

(def ^:const SQRT255 (/ 255.0 (m/sqrt 255.0)))

(defn blend-softlight-raw
  ""
  ([a b thr]
   (if (< a thr)
     (->> b
          (umult b)
          (usub b)
          (umult (usub (uadd a a) 255))
          (uadd b))
     (-> b
         (m/sqrt)
         (* SQRT255)
         (int)
         (usub b)
         (umult (usub (uadd a a) 255))
         (uadd b))))
  ([a b]
   (blend-softlight-raw a b *blend-threshold*)))

(def blend-softlight (comp clamp255 blend-softlight-raw))
(def blend-msoftlight (comp mod255 blend-softlight-raw))

(defn blend-vividlight-raw
  ""
  ([a b thr]
   (if (< a thr)
     (usub 255 (udiv (usub 255 b) (uadd a a)))
     (->> a
         (usub 255)
         (* 2)
         (udiv b))))
  ([a b]
   (blend-vividlight-raw a b *blend-threshold*)))

(def blend-vividlight (comp clamp255 blend-vividlight-raw))
(def blend-mvividlight (comp mod255 blend-vividlight-raw))

(defn blend-darkthreshold
  ""
  ([a b thr]
   (if (< a thr) a b))
  ([a b]
   (blend-darkthreshold a b *blend-threshold*)))

(defn blend-lightthreshold
  ""
  ([a b thr]
   (if (> a thr) a b))
  ([a b]
   (blend-lightthreshold a b *blend-threshold*)))

(def blends {:none blend-none
             :add blend-add
             :madd blend-madd
             :subtract blend-subtract
             :msubtract blend-msubtract
             :linearburn blend-linearburn
             :mlinearburn blend-mlinearburn
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

(def ^:const c46 (/ 4.0 6.0))

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
  [v]
  (if (> v 0.04045)
    (m/pow (/ (+ 0.055 v) 1.055) 2.4)
    (/ v 12.92)))

(def ^:const xyz-xmax 0.9504716671128306)
(def ^:const xyz-ymax 0.9999570331323426)
(def ^:const xyz-zmax 1.0889782052041752)

(defn- to-XYZ-
  ""
  [^Vec4 c]
  (let [r (xyz-correct (/ (.x c) 255.0))
        g (xyz-correct (/ (.y c) 255.0))
        b (xyz-correct (/ (.z c) 255.0))
        x (+ (* r 0.41239558896741421610) (* g 0.35758343076371481710) (* b 0.18049264738170157350))
        y (+ (* r 0.21258623078559555160) (* g 0.71517030370341084990) (* b 0.07220049864333622685))
        z (+ (* r 0.01929721549174694484) (* g 0.11918386458084853180) (* b 0.95049712513157976600))]
    (Vec4. x y z (.w c))))

(defn to-XYZ
  ""
  [c]
  (let [^Vec4 cc (to-XYZ- c)]
    (Vec4. (clamp255 (m/norm (.x cc) 0.0 xyz-xmax 0.0 255.0))
           (clamp255 (m/norm (.y cc) 0.0 xyz-ymax 0.0 255.0))
           (clamp255 (m/norm (.z cc) 0.0 xyz-zmax 0.0 255.0))
           (.w cc))))

(def ^:const xyz-f (/ 1.0 2.4))

(defn- xyz-decorrect
  ""
  [v]
  (if (> v 0.0031308)
    (- (* 1.055 (m/pow v xyz-f)) 0.055)
    (* v 12.92)))

(defn- from-XYZ-
  ""
  [^Vec4 c]
  (let [x (.x c)
        y (.y c)
        z (.z c)
        r (xyz-decorrect (+ (* x  3.2406) (* y -1.5372) (* z -0.4986)))
        g (xyz-decorrect (+ (* x -0.9689) (* y  1.8758) (* z  0.0415)))
        b (xyz-decorrect (+ (* x  0.0557) (* y -0.2040) (* z  1.0570)))]
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

(def ^:const D65X 0.950456)
(def ^:const D65Z 1.088754)
(def ^:const CIEEpsilon (/ 216.0 24389.0))
(def ^:const CIEK (/ 24389.0 27.0))
(def ^:const OneThird (/ 1.0 3.0))
(def ^:const D65FX-4 (/ (* 4.0 D65X) (+ D65X 15 (* 3.0 D65Z))))
(def ^:const D65FY-9 (/ 9.0 (+ D65X 15 (* 3.0 D65Z))))

(defn perceptible-reciprocal
  ""
  [x]
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

(def ^:const CIEK2Epsilon (* CIEK CIEEpsilon))

(defn from-LUV
  ""
  [^Vec4 c]
  (let [L (* 100.0 (m/norm (.x c) 0 255 0.0 0.9999833859065517))
        u (- (* 354.0 (m/norm (.y c) 0 255 0.1438470144487729 0.8730615053231279)) 134.0)
        v (- (* 262.0 (m/norm (.z c) 0 255 0.022447496915761492 0.944255184334379)) 140.0)
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
  [v]
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
  [v]
  (let [v3 (* v v v)]
    (if (> v3 CIEEpsilon)
      v3
      (/ (- (* 116.0 v) 16.0) CIEK))))

(defn from-LAB
  ""
  [^Vec4 c]
  (let [L (* 100.0 (m/norm (.x c) 0.0 255.0 0.0 0.9999833859065517))
        a (m/norm (.y c) 0.0 255.0 0.16203039020156618 0.8853278445843099)
        b (m/norm (.z c) 0.0 255.0 0.07698923890750631 0.8705163895243013)
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
  (let [Y (m/norm (.x c) 0.0 255.0 0.0 0.9999570331323426)
        x (m/norm (.y c) 0.0 255.0 0.150011724420108 0.6400884809339611)
        y (m/norm (.z c) 0.0 255.0 0.060007548576610774 0.6000064972148145)
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
        h (* 255.0 (/ (if (zero? chr) 0
                          (condp = mx
                            (.x c) (mod (+ 6.0 (/ (- (.y c) (.z c)) chr)) 6.0)
                            (.y c) (+ 2.0 (/ (- (.z c) (.x c)) chr))
                            (.z c) (+ 4.0 (/ (- (.x c) (.y c)) chr)))) 6.0))
        luma (+ (* 0.298839 (.x c)) (* 0.586811 (.y c)) (* 0.114350 (.z c)))
        ]
    (v/applyf (Vec4. h chr luma (.w c)) clamp255)))

(defn from-HCL
  ""
  [^Vec4 c]
  (let [h (* 6.0 (/ (.x c) 255.0))
        chr (.y c)
        l (.z c)
        x (* chr (- 1.0 (m/abs (dec (mod h 2.0)))))
        [r g b] (cond
                  (and (<= 0.0 h) (< h 1.0)) [chr x 0]
                  (and (<= 1.0 h) (< h 2.0)) [x chr 0]
                  (and (<= 2.0 h) (< h 3.0)) [0 chr x]
                  (and (<= 3.0 h) (< h 4.0)) [0 x chr]
                  (and (<= 4.0 h) (< h 5.0)) [x 0 chr]
                  :else                      [chr 0 x])
        m (- l (* 0.298839 r) (* 0.586811 g) (* 0.114350 b))]
    (v/applyf (Vec4. (+ r m) (+ g m) (+ b m) (.w c)) clamp255)))

(defn to-HSB
  ""
  [^Vec4 c]
  (let [mn (min (.x c) (.y c) (.z c))
        mx (max (.x c) (.y c) (.z c))
        delta (- mx mn)
        [h s b] (if (zero? mx) [0.0 0.0 0.0]
                    (let [s (* 255.0 (/ delta mx))
                          h (if (zero? delta) 0.0 
                                (/ (condp = mx
                                     (.x c) (/ (- (.y c) (.z c)) delta)
                                     (.y c) (+ 2.0 (/ (- (.z c) (.x c)) delta))
                                     (.z c) (+ 4.0 (/ (- (.x c) (.y c)) delta))) 6.0))]
                      [(* 255.0 (if (neg? h) (inc h) h)) s mx]))]
    (v/applyf (Vec4. h s b (.w c)) clamp255)))

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
          [r g b] (condp = (int h)
                    0 [b t p]
                    1 [q b p]
                    2 [p b t]
                    3 [p q b]
                    4 [t p b]
                    5 [b p q])]
      (v/applyf (Vec4. (* 255.0 r) (* 255.0 g) (* 255.0 b) (.w c)) clamp255))))


(def ^:const to-hsi-const (-> 180.0
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

(def ^:const from-hsi-const (/ m/PI 180.0))

(defn from-hsi-helper
  ""
  [^Vec4 cc h]
  (* (.z cc) (-> cc
                 .y
                 (* (m/cos (* h from-hsi-const)))
                 (/ (m/cos (* (- 60.0 h) from-hsi-const)))
                 inc)))

(defn from-HSI
  ""
  [^Vec4 c]
  (let [^Vec4 cc (v/div c 255)
        h (* 360.0 (.x cc))
        h (- h (* 360.0 (m/floor (/ h 360.0))))
        v1 (* (.z cc) (- 1.0 (.y cc)))
        [r g b] (cond
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
    (v/applyf (v/mult (Vec4. r g b (.w cc)) 255.0) clamp255)))

(defn to-HWB
  ""
  [^Vec4 c]
  (let [w (min (.x c) (.y c) (.z c))
        v (max (.x c) (.y c) (.z c))
        h (if (= w v) 0.0
              (let [f (condp = w
                        (.x c) (- (.y c) (.z c))
                        (.y c) (- (.z c) (.x c))
                        (.z c) (- (.x c) (.y c)))
                    p (condp = w
                        (.x c) 3.0
                        (.y c) 5.0
                        (.z c) 1.0)]
                (m/norm (/ (- p (/ f (- v w))) 6.0) 0.0 1.0 1.0 255.0)))]
    (v/applyf (Vec4. h w (- 255 v) (.w c)) clamp255)))

(defn from-HWB
  ""
  [^Vec4 c]
  (if (zero? (.x c)) 
    (let [v (- 255.0 (.z c))]
      (Vec4. v v v (.w c)))
    (let [h (m/norm (.x c) 1.0 255.0 0.0 6.0)
          v (- 1.0 (/ (.z c) 255.0))
          w (/ (.y c) 255.0)
          i (int (m/floor h))
          f (- h i)
          f (if (odd? i) (- 1.0 f) f)
          n (+ w (* f (- v w)))
          [r g b] (condp == i
                    0 [v n w]
                    1 [n v w]
                    2 [w v n]
                    3 [w n v]
                    4 [n w v]
                    5 [v w n]
                    6 [v n w])]
      (v/applyf (Vec4. (* 255.0 r) (* 255.0 g) (* 255.0 b) (.w c)) clamp255))))

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
  (let [b (+ (.x c) (m/norm (.y c) 0.0 255.0 -237.0 237.0))
        r (+ (.x c) (m/norm (.z c) 0.0 255.0 -201.0 201.0))
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
        Db (m/norm (.y c) 0.0 255.0 -339.91499999999996 339.91499999999996)
        Dr (m/norm (.z c) 0.0 255.0 -339.91499999999996 339.915)
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
        U (m/norm (.y c) 0.0 255.0 -111.17999999999999 111.17999999999999)
        V (m/norm (.z c) 0.0 255.0 -156.82500000000002 156.825)
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
        I (m/norm (.y c) 0.0 255.0 -151.90758 151.90758)
        Q (m/norm (.z c) 0.0 255.0 -133.260705 133.260705)
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

(defn- hex-to-vec
  ""
  [s]
  (let [x (Integer/parseInt s 16)
        x1 (bit-and 0xff (bit-shift-right x 16))
        x2 (bit-and 0xff (bit-shift-right x 8))
        x3 (bit-and 0xff x)]
    (Vec4. x1 x2 x3 255)))

(defn- hex-to-vecs
  ""
  [xs]
  (vec (map hex-to-vec xs)))

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
    (vec (map hex-to-vecs (concat l1 l2)))))

;; 

;; http://iquilezles.org/www/articles/palettes/palettes.htm
(defn create-palette-fn
  ""
  [^Vec3 a ^Vec3 b ^Vec3 c ^Vec3 d]
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
  [num]
  (let [a (v/generate-vec3 (partial m/drand 0.3 0.7))
        b (v/sub (Vec3. 1.0 1.0 1.1) a)
        c (v/generate-vec3 (partial m/drand 2))
        d (v/generate-vec3 m/drand)
        f (create-palette-fn a b c d)]
    (vec (map #(f %) (range 0.0 1.0 (/ 1.0 num))))))

;;

(defn nearest-color
  ""
  ([f xf ^Vec4 c]
   (first (reduce (fn [[currc currdist] ^Vec4 c1]
                    (let [dist (f c1 c)]
                      (if (< dist currdist)
                        [c1 dist]
                        [currc currdist]))) [c Double/MAX_VALUE] xf)))
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
