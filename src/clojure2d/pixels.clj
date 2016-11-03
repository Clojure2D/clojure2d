(ns clojure2d.pixels
  (:require [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.color :as c]
            [clojure2d.utils :as u]
            [criterium.core :as b])
  (:import [clojure2d.math.vector Vec4 Vec2]
           [java.awt.image BufferedImage]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; :zero, :edge :wrap or channel value 0-255
(def ^:dynamic *pixels-edge* :edge)

;; how many cores we have?
(def ^:const cores (.availableProcessors (Runtime/getRuntime)))

(defprotocol PixelsProto
  (get-value [pixels ch idx] [pixels ch x y])
  (get-color [pixels idx] [pixels x y])
  (set-value [pixels ch idx v] [pixels ch x y v])
  (set-color [pixels idx v] [pixels x y v])
  (idx->pos [pixels idx])
  (get-channel [pixels ch])
  (set-channel [pixels ch v]))

(deftype Pixels [^ints p w h planar size pos]
  PixelsProto

  (get-channel [_ ch]
    (let [^ints res (int-array size)
          off (* ch size)]
      (if planar
        (System/arraycopy p ^int off res 0 ^int size)
        (u/amap! p idx (aget p ^int (+ ch (bit-shift-left idx 2)))))
      res))

  (set-channel [_ ch v]
    (let [off (* ch size)]
      (when planar
        (System/arraycopy ^ints v 0 p ^int off ^int size)))) 
  ;; TODO: implement mutating copy of array into interleaved version of pixels

  (get-value [_ ch idx]
    (aget p ^int (pos ch idx)))

  (get-value [pixels ch x y]
    (if (or (neg? x)
            (neg? y)
            (>= x w)
            (>= y h))
      (condp = *pixels-edge*
        :zero 0
        :edge (get-value pixels ch (int (m/constrain x 0 (dec w))) (int (m/constrain y 0 (dec h))))
        :wrap (get-value pixels ch (int (m/wrap x 0 w)) (int (m/wrap y 0 h)))
        *pixels-edge*)
      (get-value pixels ch (+ x (* y w)))))

  (get-color [_ idx]
    (Vec4. (aget p ^int (pos 0 idx))
           (aget p ^int (pos 1 idx))
           (aget p ^int (pos 2 idx))
           (aget p ^int (pos 3 idx))))

  (get-color [pixels x y]
    (if (or (neg? x)
            (neg? y)
            (>= x w)
            (>= y h))
      (condp = *pixels-edge*
        :zero (Vec4. 0 0 0 255)
        :edge (get-color pixels (int (m/constrain x 0 (dec w))) (int (m/constrain y 0 (dec h))))
        :wrap (get-color pixels (int (m/wrap x 0 w)) (int (m/wrap y 0 h)))
        (Vec4. *pixels-edge* *pixels-edge* *pixels-edge* 255))
      (get-color pixels (+ x (* y w)))))

  (set-value [_ ch idx v]
    (aset-int p ^int (pos ch idx) v)
    p)

  (set-value [pixels ch x y v]
    (set-value pixels ch (+ x (* y w)) v))

  (set-color [_ idx v]
    (let [^Vec4 v v]
      (aset-int p ^int (pos 0 idx) (.x v))
      (aset-int p ^int (pos 1 idx) (.y v))
      (aset-int p ^int (pos 2 idx) (.z v))
      (aset-int p ^int (pos 3 idx) (.w v)))
    p)

  (set-color [pixels x y v]
    (set-color pixels (+ x (* y w)) v))

  (idx->pos [_ idx]
    (let [y (quot idx w)
          x (rem idx w)]
      (Vec2. x y)))
  
  Object
  (toString [_] (str "pixels (" w ", " h "), " (if planar "planar" "interleaved"))))

;; operate on pixels

(defn make-value-selector
  ""
  [planar size]
  (if planar
    (fn [ch idx]
      (+ idx (* ch size)))
    (fn [ch idx]
      (+ ch (bit-shift-left idx 2)))))

(defn make-pixels
  "Pixels constructors, sets valid channel value selector (pos) depending on layout"
  ([^ints a w h planar]
   (let [size (* w h)
         pos (make-value-selector planar size)]
     (Pixels. a w h planar size pos)))
  ([w h]
   (make-pixels w h true))
  ([w h planar]
   (make-pixels (int-array (* 4 w h)) w h planar)))

(defn replace-pixels
  ""
  ([^Pixels p ^ints a planar]
   (Pixels. a (.w p) (.h p) planar (.size p) (make-value-selector planar (.size p))))
  ([^Pixels p a]
   (replace-pixels p a (.planar p))))

(defn clone-pixels
  "Clone Pixels"
  [^Pixels p]
  (replace-pixels p (u/array-clone (.p p))))

;; interleaved/planar

(defn to-planar
  "Convert interleaved (native) layout of pixels to planar"
  [^Pixels p]
  (let [f (fn [x]
            (let [q (quot x (.size p))
                  r (rem x (.size p))]
              (+ q (bit-shift-left r 2))))]
    (replace-pixels p (amap ^ints (.p p) idx ret (aget ^ints (.p p) (f idx))) true)))

(defn from-planar
  "Convert planar pixel layout to interleaved"
  [^Pixels p]
  (let [f (fn [x]
            (let [q (bit-shift-right x 0x2)
                  r (bit-and x 0x3)]
              (+ q (* (.size p) r))))]
    (replace-pixels p (amap ^ints (.p p) idx ret (aget ^ints (.p p) (f idx))) false)))

;; load and save

(defn  get-image-pixels
  "take pixels from the buffered image"
  ([^BufferedImage b x y w h]
   (let [size (* 4 w h)
         ^ints p (.. b
                     (getRaster)
                     (getPixels ^int x ^int y ^int w ^int h ^ints (int-array size)))]
     (to-planar (make-pixels p w h false))))
  ([^BufferedImage b]
   (get-image-pixels b 0 0 (.getWidth b) (.getHeight b))))

(defn set-image-pixels
  ""
  ([^BufferedImage b x y ^Pixels pin]
   (let [^Pixels p (if (.planar pin) (from-planar pin) pin)] 
     (.. b
               (getRaster)
               (setPixels ^int x ^int y ^int (.w p) ^int (.h p) ^ints (.p p))))
   b)
  ([^BufferedImage b ^Pixels p]
   (set-image-pixels b 0 0 p)))

(defn image-from-pixels
  ""
  [^Pixels p]
  (let [^BufferedImage bimg (BufferedImage. (.w p) (.h p) BufferedImage/TYPE_INT_ARGB)]
    (set-image-pixels bimg p)))

;; processors

(defn filter-colors
  ""
  [f ^Pixels p]
  (let [^Pixels target (clone-pixels p)
        pre-step (max 40000 (/ (.size p) cores))
        step (int (m/ceil pre-step))
        parts (range 0 (.size p) step)
        ftrs (doall
              (map
               #(future (let [end (min (+ % step) (.size p))]
                          (loop [idx (int %)]
                            (when (< idx end)
                              (set-color target idx (f (get-color p idx)))
                              (recur (inc idx))))))
               parts))]
    (dorun (map deref ftrs))
    target))

(defn filter-channel
  "Filter one channel, write result into target. Works only on planar"
  [f ch ^Pixels target ^Pixels p]
  (let [size (.size target)
        start (long (* ch size))
        stop (long (* (inc ch) size))]
    (loop [idx start]
      (when (< idx stop)
        (aset-int (.p target) idx (f (aget ^ints (.p p) idx)))
        (recur (inc idx))))
    true))

(defn filter-channel-xy
  "Filter one channel, write result into target. Works on planar/interleaved"
  [f ch ^Pixels target p]
  (u/doloop
   [y (.h target)]
   (u/doloop
    [x (.w target)]
    (set-value target ch x y (f ch p x y))))
  true)

(defn filter-channels
  "Filter channels parallelly"
  ([f0 f1 f2 f3 p]
   (let [target (clone-pixels p)
         ch0 (future (if f0 (f0 0 target p) false))
         ch1 (future (if f1 (f1 1 target p) false))
         ch2 (future (if f2 (f2 2 target p) false))
         ch3 (future (if f3 (f3 3 target p) false))]
     (dorun (map deref [ch0 ch1 ch2 ch3]))
     target))
  ([f p]
   (filter-channels f f f f p))
  ([f do-alpha p]
   (if do-alpha
     (filter-channels f f f f p)
     (filter-channels f f f nil p))))

(defn blend-channel
  "Blend one channel, write result into target. Works only on planar"
  [f ch ^Pixels target ^Pixels p1 ^Pixels p2]
  (let [size (.size target)
        start (long (* ch size))
        stop (long (* (inc ch) size))]
    (loop [idx start]
      (when (< idx stop)
        (aset-int (.p target) idx (f (aget ^ints (.p p1) idx) (aget ^ints (.p p2) idx)))
        (recur (inc idx))))
    true))

(defn blend-channel-xy
  "Blend one channel, write result into target. Works on planar/interleaved"
  [f ch ^Pixels target p1 p2]
  (u/doloop
   [y (.h target)]
   (u/doloop
    [x (.w target)]
    (set-value target ch x y (f ch p1 p2 x y))))
  true)

(defn blend-channels
  "Blend channels parallelly"
  ([f0 f1 f2 f3 p1 p2]
   (let [target (clone-pixels p1)
         ch0 (future (if f0 (f0 0 target p1 p2) false))
         ch1 (future (if f1 (f1 1 target p1 p2) false))
         ch2 (future (if f2 (f2 2 target p1 p2) false))
         ch3 (future (if f3 (f3 3 target p1 p2) false))]
     (dorun (map deref [ch0 ch1 ch2 ch3]))
     target))
  ([f p1 p2]
   (blend-channels f f f f p1 p2))
  ([f do-alpha p1 p2]
   (if do-alpha
     (blend-channels f f f f p1 p2)
     (blend-channels f f f nil p1 p2))))

;; compose channels

(defn make-compose-f
  ""
  [n]
  (cond 
    (keyword? n) (partial blend-channel (n c/blends))
    (nil? n) nil
    :else (partial blend-channel n)))

(def make-compose (memoize make-compose-f))

(defn compose-channels
  "Compose channels with blending functions"
  ([n1 n2 n3 n4 p1 p2]
   (blend-channels (make-compose n1) (make-compose n2) (make-compose n3) (make-compose n4) p1 p2))
  ([n p1 p2]
   (compose-channels n n n n p1 p2))
  ([n do-alpha p1 p2]
   (if do-alpha
     (compose-channels n n n n p1 p2)
     (compose-channels n n n nil p1 p2))))

;; convolution

(defn get-3x3
  ""
  [ch ^Pixels p x y]
  [(get-value p ch (dec x) (dec y))
   (get-value p ch x  (dec y))
   (get-value p ch (inc x) (dec y))
   (get-value p ch (dec x) y)
   (get-value p ch x  y)
   (get-value p ch (inc x) y)
   (get-value p ch (dec x) (inc y))
   (get-value p ch x  (inc y))
   (get-value p ch (inc x) (inc y))])

(defn get-cross-f
  ""
  [f ch ^Pixels p x y]
  (f (get-value p ch x (dec y))
     (f (get-value p ch (dec x) y)
        (f (get-value p ch x y)
           (f (get-value p ch (inc x) y)
              (get-value p ch x (inc y)))))))

(def dilate-filter (partial filter-channel-xy (partial get-cross-f max)))
(def erode-filter (partial filter-channel-xy (partial get-cross-f min)))

;; gaussian blur done
;; http://blog.ivank.net/fastest-gaussian-blur.html

(defn box-blur-v
  ""
  [r w h ^ints in]
  (let [size (* w h)
        ^ints target (int-array size)
        iarr (/ 1.0 (inc (+ r r)))
        r+ (inc r)
        rang (range (- r+) r)]
    (u/doloop 
     [x w]
     (let [val (reduce #(+ %1 (u/aget-2d in w h x %2)) 0 rang)]
       (loop [y (int 0)
              v (int val)]
         (when (< y h)
           (let [nv (int (- (+ v (u/aget-2d in w h x (+ y r)))
                            (u/aget-2d in w h x (- y r+))))]
             (aset-int target ^int (+ x (* w y)) (* nv iarr))
             (recur (inc y) nv))))))
    target))

(defn box-blur-h
  ""
  [r w h ^ints in]
  (let [size (* w h)
        ^ints target (int-array size)
        iarr (/ 1.0 (inc (+ r r)))
        r+ (inc r)
        rang (range (- r+) r)]
    (u/doloop
     [y h]
     (let [val (reduce #(+ %1 (u/aget-2d in w h %2 y)) 0 rang)
           off (* w y)]
       (loop [x (int 0)
              v (int val)]
         (when (< x w)
           (let [nv (int (- (+ v (u/aget-2d in w h (+ x r) y))
                            (u/aget-2d in w h (- x r+) y)))]
             (aset-int target ^int (+ x off) (* nv iarr))
             (recur (inc x) nv))))))
    target))

(defn box-blur
  ""
  [r ch ^Pixels target ^Pixels p]
  (let [buf (get-channel p ch)
        result (box-blur-v r (.w p) (.h p) (box-blur-h r (.w p) (.h p) buf))]
    (set-channel target ch result))
  nil)

(defn make-radius-blur
  ""
  ([f radius]
   (partial f radius))
  ([f]
   (make-radius-blur f 2)))

(def make-box-blur (partial make-radius-blur box-blur))

(def box-blur-1 (make-box-blur 1))
(def box-blur-2 (make-box-blur 2))
(def box-blur-3 (make-box-blur 3))
(def box-blur-5 (make-box-blur 5))

(defn radius-for-gauss
  ""
  [sigma n]
  (let [sigma* (* 12 sigma sigma)
        w-ideal (-> sigma*
                    (/ n)
                    (inc)
                    (m/sqrt)
                    (m/floor)
                    (int))
        wl (if (even? w-ideal) (dec w-ideal) w-ideal)
        wu (+ wl 2)
        m-ideal (/ (- sigma* (* n (m/sq wl)) (* 4 n wl) (* 3 n))
                   (- (* -4 wl) 4))
        m (int (m/round m-ideal))]
    (vec (map #(/ (dec (if (< % m) wl wu)) 2) (range n)))))

(def radius-for-gauss-memo (memoize radius-for-gauss))

(defn gauss-blur
  "Proceed with Gaussian Blur on channel, save result to target"
  [r ch ^Pixels target ^Pixels p]
  (let [bxs (radius-for-gauss-memo r 3)
        buf (get-channel p ch)
        res (->> buf
                 (box-blur-h (bxs 0) (.w p) (.h p))
                 (box-blur-v (bxs 0) (.w p) (.h p))
                 (box-blur-h (bxs 1) (.w p) (.h p))
                 (box-blur-v (bxs 1) (.w p) (.h p))
                 (box-blur-h (bxs 2) (.w p) (.h p))
                 (box-blur-v (bxs 2) (.w p) (.h p)))]
    (set-channel target ch res)
    nil))

(def make-gaussian-blur (partial make-radius-blur gauss-blur))

(def gaussian-blur-1 (make-gaussian-blur 1))
(def gaussian-blur-2 (make-gaussian-blur 2))
(def gaussian-blur-3 (make-gaussian-blur 3))
(def gaussian-blur-5 (make-gaussian-blur 5))

;; posterize done

(defn posterize-levels-fun
  ""
  [numlev]
  (let [f (fn [idx] (-> idx
                        (* numlev)
                        (bit-shift-right 8)
                        (* 255.0)
                        (/ (dec numlev))))
        ^ints tmp (int-array 256)]
    (amap tmp idx ret ^int (f idx))))

(def posterize-levels (memoize posterize-levels-fun))

(defn posterize-pixel
  ""
  [^ints levels v]
  (aget levels v))

(defn make-posterize
  ""
  ([numlev]
   (let [nl (m/constrain numlev 2 255)
         ^ints levels (posterize-levels nl)]
     (partial filter-channel (partial posterize-pixel levels))))
  ([]
   (make-posterize 6)))

(def posterize-4 (make-posterize 4))
(def posterize-8 (make-posterize 8))
(def posterize-16 (make-posterize 16))

;; threshold

(defn threshold-pixel
  ""
  [thr v]
  (if (< v thr) 255 0))

(defn make-threshold
  ""
  ([thr]
   (let [t (int (* 256 thr))]
     (partial filter-channel (partial threshold-pixel t))))
  ([]
   (make-threshold 0.5)))

(def threshold-25 (make-threshold 0.25))
(def threshold-50 (make-threshold))
(def threshold-75 (make-threshold 0.75))

;; Median and Quantile filters

(defn quantile
  "Fast quantile on 9 values"
  [n vs]
  (let [vss (vec (sort vs))]
    (vss n)))

(defn make-quantile-filter
  ""
  [v]
  (let [q (int (m/constrain v 0 8))]
    (partial filter-channel-xy (comp (partial quantile q) get-3x3))))

(def median-filter (make-quantile-filter 4))

;; Tint

(defn calc-tint
  ""
  [a b]
  (bit-shift-right (bit-and 0xff00 (unchecked-int (* a b))) 8))

(defn make-tint-map
  ""
  [r g b a]
  (vec (map #(amap ^ints (int-array 256) idx ret ^int (calc-tint idx %)) [r g b a])))

(defn make-tinter
  ""
  [^ints a]
  (fn [v] (aget a ^int v)))

(defn make-tint-filter
  ""
  ([r g b a]
   (let [tm (make-tint-map r g b a)]
     (fn [ch target p]
       (let [tinter (make-tinter (tm ch))]
         (filter-channel tinter ch target p)))))
  ([r g b]
   (make-tint-filter r g b 256)))

(defn normalize
  "Normalize channel values to whole (0-255) range"
  [ch target ^Pixels p]
  (let [[mn mx] (loop [idx (int 0)
                       currmn (int Integer/MAX_VALUE)
                       currmx (int Integer/MIN_VALUE)]
                  (if (< idx (.size p))
                    (let [v (get-value p ch idx)]
                      (recur (inc idx)
                             (int (if (< v currmn) v currmn))
                             (int (if (> v currmx) v currmx))))
                    [currmn currmx]))]
    (filter-channel #(m/norm % mn mx 0 255) ch target p)))

(defn- equalize-make-histogram
  ""
  [ch ^Pixels p]
  (let [^doubles hist (double-array 256 0.0)
        d (/ 1.0 (.size p))]
    (loop [idx (int 0)]
      (if (< idx (.size p))
        (let [c (get-value p ch idx)]
          (aset-double hist c (+ d ^double (aget hist c)))
          (recur (inc idx)))
        hist))))

(defn- equalize-make-lookup
  ""
  [^doubles hist]
  (let [^ints lookup (int-array 256 0)]
    (loop [currmn (int Integer/MAX_VALUE)
           currmx (int Integer/MIN_VALUE)
           sum (double 0.0)
           idx (int 0)]
      (if (< idx 256)
        (let [currsum (+ sum (aget hist idx))
              val (int (m/constrain (m/round (* sum 255.0)) 0 255))]
          (aset-int lookup idx val)
          (recur (int (if (< val currmn) val currmn))
                 (int (if (> val currmx) val currmx))
                 (double currsum)
                 (inc idx)))
        lookup))))

(defn equalize-filter
  "Equalize histogram"
  [ch target p]
  (let [^ints lookup (equalize-make-lookup (equalize-make-histogram ch p))]
    (filter-channel #(aget lookup %) ch target p)))
