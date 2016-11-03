(ns clojure2d.color
  (:require [clojure2d.math :as m]
            [clojure2d.math.vector :as v])
  (:import [clojure2d.math.vector Vec4]
           [java.awt Color]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:dynamic *blend-threshold* 128)

(defn clamp255
  ""
  [a]
  (int (m/constrain (m/round a) 0 255)))

(def mod255 (partial bit-and 0xff))

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

(defn from-color
  ""
  [^Color c]
  (Vec4. (.getRed c)
         (.getGreen c)
         (.getBlue c)
         (.getAlpha c)))

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
         mnr ^int Integer/MAX_VALUE
         mxr ^int Integer/MIN_VALUE
         mng ^int Integer/MAX_VALUE
         mxg ^int Integer/MIN_VALUE
         mnb ^int Integer/MAX_VALUE
         mxb ^int Integer/MIN_VALUE]
    (let [r (bit-and 0xff (bit-shift-right cc 16))
          g (bit-and 0xff (bit-shift-right cc 8))
          b (bit-and 0xff cc)
          ^Vec4 res (f (Vec4. r g b 255))
          nmnr (int (if (< (.x res) mnr) (.x res) mnr))
          nmxr (int (if (> (.x res) mxr) (.x res) mxr))
          nmng (int (if (< (.y res) mng) (.y res) mng))
          nmxg (int (if (> (.y res) mxg) (.y res) mxg))
          nmnb (int (if (< (.z res) mnb) (.z res) mnb))
          nmxb (int (if (> (.z res) mxb) (.z res) mxb))]
      (if (< cc 0x1000000)
        (recur (inc cc) nmnr nmxr nmng nmxg nmnb nmxb)
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
        i3 (clamp255 (/ (+ 510.0 (+ (.x c) (.z c) (- (+ (.y c) (.y c))))) 4.0))]
    (Vec4. i1 i2 i3 (.w c))))

(def ^:const c46 (/ 4.0 6.0))

(defn from-OHTA
  "OHTA -> RGB"
  [^Vec4 c]
  (let [i1 (.x c) ; divided by 3
        i2  (- (.y c) 127.5) ; divided by 2
        i3 (- (* c46 (.z c)) 85.0) ; divided by 6
        r (clamp255 (+ i1 i2 i3))
        g (clamp255 (- i1 i3 i3))
        b (clamp255 (- (+ i1 i3) i2))]
    (Vec4. r g b (.w c))))

;; YPbPr

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
    (Vec4. (clamp255 r) (clamp255 g) (clamp255 b) (.w c))))

;;(test-colors (comp from-YPbPr to-YPbPr))


