(ns clojure2d.pixels
  (:require [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.math.random :as r]
            [clojure2d.color :as c]
            [clojure2d.core :as core]
            [criterium.core :as b])
  (:import [clojure2d.math.vector Vec4 Vec3 Vec2]
           [clojure.lang PersistentVector]
           [java.awt.image BufferedImage]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; :zero, :edge :wrap or channel value 0-255
(def ^:dynamic *pixels-edge* :edge)

(defprotocol PixelsProto
  (get-value [pixels ch idx] [pixels ch x y])
  (get-color [pixels idx] [pixels x y])
  (set-value [pixels ch idx v] [pixels ch x y v])
  (set-color [pixels idx v] [pixels x y v])
  (idx->pos [pixels idx])
  (get-channel [pixels ch])
  (set-channel [pixels ch v]))

(deftype Pixels [^ints p ^long w ^long h planar ^long size pos]
  PixelsProto

  (get-channel [_ ch]
    (let [^ints res (int-array size)
          off (* ^long ch size)]
      (if planar
        (System/arraycopy p off res 0 size)
        (dotimes [idx size]
          (aset res idx (aget p (+ ^long ch (bit-shift-left idx 2))))))
      res))

  (set-channel [_ ch v]
    (let [off (* ^long ch size)]
      (when planar
        (System/arraycopy ^ints v 0 p off size)))) 
  ;; TODO: implement mutating copy of array into interleaved version of pixels

  (get-value [_ ch idx]
    (aget ^ints p (pos ch idx)))

  (get-value [pixels ch x y]
    (if (or (neg? ^long x)
            (neg? ^long y)
            (>= ^long x w)
            (>= ^long y h))
      (condp = *pixels-edge*
        :zero 0
        :edge (get-value pixels ch (m/iconstrain x 0 (dec w)) (m/iconstrain y 0 (dec h)))
        :wrap (get-value pixels ch (int (m/wrap 0 w x)) (int (m/wrap 0 h y)))
        *pixels-edge*)
      (get-value pixels ch (+ ^long x (* ^long y w)))))

  (get-color [_ idx]
    (Vec4. (aget ^ints p ^long (pos 0 idx))
           (aget ^ints p ^long (pos 1 idx))
           (aget ^ints p ^long (pos 2 idx))
           (aget ^ints p ^long (pos 3 idx))))

  (get-color [pixels x y]
    (if (or (neg? ^long x)
            (neg? ^long y)
            (>= ^long x w)
            (>= ^long y h))
      (condp = *pixels-edge*
        :zero (Vec4. 0 0 0 255)
        :edge (get-color pixels (m/iconstrain x 0 (dec w)) ((m/iconstrain y 0 (dec h))))
        :wrap (get-color pixels (int (m/wrap 0 w x)) (int (m/wrap 0 h y)))
        (Vec4. *pixels-edge* *pixels-edge* *pixels-edge* 255))
      (get-color pixels (+ ^long x (* ^long y w)))))

  (set-value [_ ch idx v]
    (aset ^ints p (int (pos ch idx)) (int v))
    p)

  (set-value [pixels ch x y v]
    (set-value pixels ch (+ ^long x (* ^long y w)) v))

  (set-color [_ idx v]
    ""
    (let [^Vec4 v v]
      (aset ^ints p ^long (pos 0 idx) (int (.x v)))
      (aset ^ints p ^long (pos 1 idx) (int (.y v)))
      (aset ^ints p ^long (pos 2 idx) (int (.z v)))
      (aset ^ints p ^long (pos 3 idx) (int (.w v))))
    p)

  (set-color [pixels x y v]
    (set-color pixels (+ ^long x (* ^long y w)) v))

  (idx->pos [_ idx]
    (let [y (quot ^long idx w)
          x (rem ^long idx w)]
      (Vec2. x y)))
  
  Object
  (toString [_] (str "pixels (" w ", " h "), " (if planar "planar" "interleaved"))))

;; operate on pixels

(defn make-value-selector
  ""
  [planar ^long size]
  (if planar
    (fn ^long [^long ch ^long idx]
      (+ idx (* ch size)))
    (fn ^long [^long ch ^long idx]
      (+ ch (bit-shift-left idx 2)))))

(defn make-pixels
  "Pixels constructors, sets valid channel value selector (pos) depending on layout"
  ([^ints a ^long w ^long h planar]
   (let [size (* w h)
         pos (make-value-selector planar size)]
     (Pixels. a w h planar size pos)))
  ([w h]
   (make-pixels w h true))
  ([^long w ^long h planar]
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
  (let [len (alength ^ints (.p p))
        res (int-array len)]
    (do
      (System/arraycopy (.p p) 0 ^ints res 0 len)
      (replace-pixels p res))))

;; interleaved/planar

(defn to-planar
  "Convert interleaved (native) layout of pixels to planar"
  [^Pixels p]
  (let [f (fn ^long [^long x]
            (let [q (quot x (.size p))
                  r (rem x (.size p))]
              (+ q (bit-shift-left r 2))))]
    (replace-pixels p (amap ^ints (.p p) idx ret (int (aget ^ints (.p p) (int (f idx))))) true)))

(defn from-planar
  "Convert planar pixel layout to interleaved"
  [^Pixels p]
  (let [f (fn ^long [^long x]
            (let [q (bit-shift-right x 0x2)
                  r (bit-and x 0x3)]
              (+ q (* (.size p) r))))]
    (replace-pixels p (amap ^ints (.p p) idx ret (int (aget ^ints (.p p) (int (f idx))))) false)))

;; load and save

(defn  get-image-pixels
  "take pixels from the buffered image"
  ([^BufferedImage b x y w h planar?]
   (let [size (* 4 ^long w ^long h)
         ^ints p (.. b
                     (getRaster)
                     (getPixels ^int x ^int y ^int w ^int h ^ints (int-array size)))
         pixls (make-pixels p w h false)]
     (if planar?
       (to-planar pixls)
       pixls)))
  ([b x y w h]
   (get-image-pixels b x y w h true))
  ([^BufferedImage b planar?]
   (get-image-pixels b 0 0 (.getWidth b) (.getHeight b) planar?))
  ([b]
   (get-image-pixels b true)))

(defn set-image-pixels
  ""
  ([^BufferedImage b x y ^Pixels pin]
   (let [^Pixels p (if (.planar pin) (from-planar pin) pin)] 
     (.. b
         (getRaster)
         (setPixels ^int x ^int y (int (.w p)) (int (.h p)) ^ints (.p p))))
   b)
  ([^BufferedImage b ^Pixels p]
   (set-image-pixels b 0 0 p)))

(defn image-from-pixels
  ""
  [^Pixels p]
  (let [^BufferedImage bimg (BufferedImage. (.w p) (.h p) BufferedImage/TYPE_INT_ARGB)]
    (set-image-pixels bimg p)))

;;

;; taking and putting pixel array (as ints)

(defn get-canvas-pixels
  ""
  ([canvas x y w h planar?]
   (get-image-pixels (@canvas 1) x y w h planar?))
  ([canvas x y w h]
   (get-image-pixels (@canvas 1) x y w h true))
  ([canvas]
   (get-image-pixels (@canvas 1)))
  ([canvas planar?]
   (get-image-pixels (@canvas 1) planar?)))

(defn set-canvas-pixels
  ""
  ([canvas p x y]
   (let [[_ b] @canvas]
     (set-image-pixels b x y p)))
  ([canvas p]
   (let [[_ b] @canvas]
     (set-image-pixels b p))))

;;

(defn load-pixels
  "Load pixels from file"
  [n]
  (get-image-pixels (core/load-image n)))

(defn save-pixels
  "Save pixels to file"
  [p n]
  (core/save-image (image-from-pixels p) n)
  p)


;; processors

(defn filter-colors
  ""
  [f ^Pixels p]
  (let [^Pixels target (clone-pixels p)
        pre-step (max 40000 ^long (/ (.size p) core/available-tasks))
        step (int (m/ceil pre-step))
        parts (range 0 (.size p) step)
        ftrs (doall
              (map
               #(future (let [end (min (+ ^long % step) (.size p))]
                          (loop [idx (int %)]
                            (when (< idx end)
                              (set-color target idx (f (get-color p idx)))
                              (recur (unchecked-inc idx))))))
               parts))]
    (dorun (map deref ftrs))
    target))

(defn filter-channel
  "Filter one channel, write result into target. Works only on planar"
  [f ^long ch ^Pixels target ^Pixels p]
  (let [size (.size target)
        start (* ch size)
        stop (* (inc ch) size)]
    (loop [idx (int start)]
      (when (< idx stop)
        (aset ^ints (.p target) idx (int (f (aget ^ints (.p p) idx))))
        (recur (unchecked-inc idx))))
    true))

(defn filter-channel-xy
  "Filter one channel, write result into target. Works on planar/interleaved"
  [f ch ^Pixels target p]
  (dotimes [y (.h target)]
    (dotimes [x (.w target)]
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
        start (int (* ^long ch size))
        stop (int (* (inc ^long ch) size))]
    (loop [idx start]
      (when (< idx stop)
        (aset ^ints (.p target) idx (int (c/convert-and-blend f (aget ^ints (.p p1) idx) (aget ^ints (.p p2) idx))))
        (recur (unchecked-inc idx))))
    true))

(defn blend-channel-xy
  "Blend one channel, write result into target. Works on planar/interleaved"
  [f ch ^Pixels target p1 p2]
  (dotimes [y (.h target)]
   (dotimes [x (.w target)]
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
  [ch ^Pixels p ^long x ^long y]
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
  (f ^int (get-value p ch x (dec ^long y))
     (f ^int (get-value p ch (dec ^long x) y)
        (f ^int (get-value p ch x y)
           (f ^int (get-value p ch (inc ^long x) y)
              ^int (get-value p ch x (inc ^long y)))))))

(def dilate-filter (partial filter-channel-xy (partial get-cross-f max)))
(def erode-filter (partial filter-channel-xy (partial get-cross-f min)))

;; gaussian blur done
;; http://blog.ivank.net/fastest-gaussian-blur.html

(defn make-aget-2d
  ""
  [^ints array ^long w ^long h]
  (fn local-aget-2d ^long [^long x ^long y]
    (if (or (neg? x)
            (neg? y)
            (>= x w)
            (>= y h))
      (local-aget-2d (m/iconstrain x 0 (dec w)) (m/iconstrain y 0 (dec h)))
      (aget array (+ x (* y w))))))

(defn box-blur-v
  ""
  [^long r ^long w ^long h ^ints in]
  (if (zero? r) in
      (let [aget-2d (make-aget-2d in w h)
            size (* w h)
            ^ints target (int-array size)
            iarr (/ 1.0 (inc (+ r r)))
            r+ (inc r)
            rang (range (- r+) r)]
        (dotimes [x w]
          (let [val (reduce #(+ ^long %1 ^long (aget-2d x %2)) 0 rang)]
            (loop [y (int 0)
                   v (int val)]
              (when (< y h)
                (let [nv (- (+ v ^long (aget-2d x (+ y r)))
                            ^long (aget-2d x (- y r+)))]
                  (aset ^ints target (+ x (* w y)) (int (* nv iarr)))
                  (recur (unchecked-inc y) nv))))))
        target)))

(defn box-blur-h
  ""
  [^long r ^long w ^long h ^ints in]
  (if (zero? r) in
      (let [aget-2d (make-aget-2d in w h)
            size (* w h)
            ^ints target (int-array size)
            iarr (/ 1.0 (inc (+ r r)))
            r+ (inc r)
            rang (range (- r+) r)]
        (dotimes [y h]
          (let [val (reduce #(+ ^long %1 ^long (aget-2d %2 y)) 0 rang)
                off (* w y)]
            (loop [x (int 0)
                   v (int val)]
              (when (< x w)
                (let [nv (- (+ v ^long (aget-2d (+ x r) y))
                            ^long (aget-2d (- x r+) y))]
                  (aset ^ints target (+ x off) (int (* nv iarr)))
                  (recur (unchecked-inc x) nv))))))
        target)))

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
  [^double sigma ^long n]
  (let [sigma* (* 12 sigma sigma)
        w-ideal (-> sigma*
                    (/ n)
                    (inc)
                    (m/sqrt)
                    (m/floor))
        wl (if (even? (int w-ideal)) (dec w-ideal) w-ideal)
        wu (+ wl 2)
        m-ideal (/ (- sigma* (* n (m/sq wl)) (* 4 n wl) (* 3 n))
                   (- (* -4 wl) 4))
        m (m/round m-ideal)]
    (vec (map #(int (/ (dec (if (< ^int % m) wl wu)) 2)) (range n)))))

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
  [^long numlev]
  (let [f (fn ^long [^long idx] (-> idx
                                    (* numlev)
                                    (bit-shift-right 8)
                                    (* 255)
                                    (/ (dec numlev))))
        ^ints tmp (int-array 256)]
    (amap tmp idx ret ^long (f idx))))

(def posterize-levels (memoize posterize-levels-fun))

(defn posterize-pixel
  ""
  [levels v]
  (aget ^ints levels (int v)))

(defn make-posterize
  ""
  ([^long numlev]
   (let [nl (m/iconstrain numlev 2 255)
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
  ^long [^long thr ^long v]
  (if (< v thr) 255 0))

(defn make-threshold
  ""
  ([^double thr]
   (let [t (long (* 256 thr))]
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
  [^long v]
  (let [q (m/iconstrain v 0 8)]
    (partial filter-channel-xy (comp (partial quantile q) get-3x3))))

(def median-filter (make-quantile-filter 4))

;; Tint

(defn calc-tint
  ""
  ^long [^long a ^long b]
  (bit-shift-right (bit-and 0xff00 (* a b)) 8))

(defn make-tint-map
  ""
  [r g b a]
  (vec (map #(amap ^ints (int-array 256) idx ret ^long (calc-tint idx %)) [r g b a])))

(defn make-tinter
  ""
  [^ints a]
  (fn [v] (aget ^ints a (int v))))

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
  (let [sz (.size p)
        [mn mx] (loop [idx (int 0)
                       currmn (long 1000)
                       currmx (long -1000)]
                  (if (< idx sz)
                    (let [^long v (get-value p ch idx)]
                      (recur (unchecked-inc idx)
                             (min v currmn)
                             (max v currmx)))
                    [currmn currmx]))
        mnd (double mn)
        mxd (double mx)]
    (filter-channel #(* 255.0 (m/norm % mnd mxd)) ch target p)))

(def normalize-filter normalize)

(defn- equalize-make-histogram
  ""
  [ch ^Pixels p]
  (let [^doubles hist (double-array 256 0.0)
        sz (.size p)
        d (double (/ 1.0 (.size p)))]
    (loop [idx (int 0)]
      (if (< idx sz)
        (let [c (get-value p ch idx)]
          (aset ^doubles hist c (+ d (aget ^doubles hist c)))
          (recur (unchecked-inc idx)))
        hist))))

(defn- equalize-make-lookup
  ""
  [^doubles hist]
  (let [^ints lookup (int-array 256 0)]
    (loop [currmn Integer/MAX_VALUE
           currmx Integer/MIN_VALUE
           sum (double 0.0)
           idx (int 0)]
      (if (< idx 256)
        (let [currsum (+ sum ^double (aget ^doubles hist idx))
              val (m/round (m/constrain (* sum 255.0) 0.0 255.0))]
          (aset ^ints lookup idx val)
          (recur (min val currmn)
                 (max val currmx)
                 currsum
                 (unchecked-inc idx)))
        lookup))))

(defn equalize-filter
  "Equalize histogram"
  [ch target p]
  (let [^ints lookup (equalize-make-lookup (equalize-make-histogram ch p))]
    (filter-channel #(aget ^ints lookup ^int %) ch target p)))

(defn negate-filter
  ""
  [ch target p]
  (filter-channel #(- 255 ^int %) ch target p))


;;;;;;;;;;; Accumulator Bins for pixels - in progress

(defprotocol BinPixelsProto
  (add-at-position [binpixels p s ch1 ch2 ch3]) ;; p - position, s - scale/value, ch1-ch3 - values
  (add-pixel-bilinear [binpixels x y ch1 ch2 ch3])
  (add-pixel-simple [binpixels x y ch1 ch2 ch3] [binpixels x y v ch1 ch2 ch3])
  (to-pixels [binpixels background config] [binpixels background] [binpixels]))

(deftype BinPixels [^doubles bins ;; general sum of hits
                    ^doubles ch1 ;; sum of color values, channel1 (red)
                    ^doubles ch2 ;; channel2
                    ^doubles ch3 ;; channel3
                    ^long sizex ^long sizey ^long sizex+ ;; size of the buffer
                    fnormx fnormy] ;; normalization functions
  Object
  (toString [_] (str "[" sizex "," sizey "]"))
  BinPixelsProto

  (add-at-position [t p s v1 v2 v3]
    (let [^long p p
          ^double s s]
      (aset bins p (+ (aget bins p) s))
      (aset ch1 p (+ (aget ch1 p) (* s ^double v1)))
      (aset ch2 p (+ (aget ch2 p) (* s ^double v2)))
      (aset ch3 p (+ (aget ch3 p) (* s ^double v3)))))
  
  (add-pixel-simple [t x y v v1 v2 v3]
    (let [ivx (long (fnormx x))
          ivy (long (fnormy y))
          p (+ ivx (* ivy sizex+))]
      (when (and (< -1 ivx sizex) (< -1 ivy sizey)) 
        (add-at-position t p v v1 v2 v3)))
    t)
  
  (add-pixel-simple [t x y v1 v2 v3]
    (add-pixel-simple t x y 1.0 v1 v2 v3))
  
  (add-pixel-bilinear [t x y v1 v2 v3]
    (let [^double vx (fnormx x)
          ^double vy (fnormy y)
          ivx (long vx)
          ivy (long vy)]
      (when (and (< -1 ivx sizex) (< -1 ivy sizey)) 
        (let [restx (- vx ivx)
              resty (- vy ivy)
              ivx+ (inc ivx)
              ivy+ (inc ivy)]

          (add-at-position t (+ ivx (* ivy sizex+)) (* restx resty) v1 v2 v3)
          (add-at-position t (+ ivx+ (* ivy sizex+)) (* (- 1.0 restx) resty) v1 v2 v3)
          (add-at-position t (+ ivx (* ivy+ sizex+)) (* restx (- 1.0 resty)) v1 v2 v3)
          (add-at-position t (+ ivx+ (* ivy+ sizex+)) (* (- 1.0 restx) (- 1.0 resty)) v1 v2 v3))))
    t)
  
  (to-pixels [_ background {:keys [^double alpha-gamma ^double intensity ^double color-gamma
                                   ^double saturation ^double brightness]
                            :or {alpha-gamma 2.0 intensity 0.8 color-gamma 1.1 saturation 1.1 brightness 1.0}}] 
    (let [binsmax (double (areduce bins idx ret Double/MIN_VALUE (max ret ^double (aget bins idx))))
          ^Vec4 background background ;; background color
          rintensity (- 1.0 intensity) ;; complementary to intensity
          agamma (/ 1.0 alpha-gamma) ;; gamma factor for alpha
          cgamma (/ 1.0 color-gamma) ;; gamma factor for color
          mxlog (/ 1.0 (m/log (inc binsmax))) ;; logarithm of max value
          rmx (/ 1.0 binsmax) ;; reverse of max
          ^Pixels p (make-pixels sizex sizey) ;; target
          ^Vec4 multiplier (Vec4. 1.0 saturation brightness 1.0)] ;; multiply vector for brightness/saturation
      (dotimes [y sizey]
        (let [row+ (* y sizex+)
              row (* y sizex)]
          (dotimes [x sizex]
            (let [idx (+ x row+)
                  hit (aget bins idx)] ;; how many hits to bins
              (if (zero? hit)
                (set-color p (+ x row) background) ;; nothing? set background
                (let [rhit (/ 1.0 hit) ;; hit reverse
                      loghit (* (m/log (inc hit)) mxlog) ;; log map for hit, result: 0.0-1.0
                      ;; loghit (m/log2 (inc (* rmx hit))) ;; log map for hit, result: 0.0-1.0
                      alpha (m/pow loghit agamma) ;; gamma for alpha
                      col1 (aget ^doubles c/r255 (m/iconstrain (int (* rhit (aget ch1 idx))) 0 255)) ;; normalized red
                      col2 (aget ^doubles c/r255 (m/iconstrain (int (* rhit (aget ch2 idx))) 0 255)) ;; green
                      col3 (aget ^doubles c/r255 (m/iconstrain (int (* rhit (aget ch3 idx))) 0 255)) ;; and blue
                      c1 (* 255.0 (+ (* intensity col1)
                                     (* rintensity (m/pow col1 cgamma)))) ;; interpolate between color and gamma(color), intensity based
                      c2 (* 255.0 (+ (* intensity col2)
                                     (* rintensity (m/pow col2 cgamma))))
                      c3 (* 255.0 (+ (* intensity col3)
                                     (* rintensity (m/pow col3 cgamma)))) 
                      color (c/from-HSB (v/emult multiplier (c/to-HSB (v/applyf (Vec4. c1 c2 c3 255.0) c/clamp255))))] ;; apply brightness, saturation factors
                  (set-color p (+ x row) (v/interpolate background color alpha)))))))) ;; store!
      p))
  (to-pixels [t background] (to-pixels t background {}))
  (to-pixels [t] (to-pixels t (Vec4. 0.0 0.0 0.0 0.0) {})))

(defn make-binpixels 
  "Create BinPixels type"
  ^BinPixels [[^double rminx ^double rmaxx ^double rminy ^double rmaxy] ^long sizex ^long sizey]
  (let [sizex+ (inc sizex)
        sizey+ (inc sizey)
        bins (double-array (* sizex+ sizey+))
        ch1 (double-array (* sizex+ sizey+))
        ch2 (double-array (* sizex+ sizey+))
        ch3 (double-array (* sizex+ sizey+))
        fnormx (m/make-norm rminx rmaxx 0 sizex)
        fnormy (m/make-norm rminy rmaxy 0 sizey)
        diff (min (m/abs (- rmaxx rminx)) (m/abs (- rmaxy rminy)))]
    (BinPixels. bins ch1 ch2 ch3 sizex sizey sizex+ fnormx fnormy)))

(defn merge-binpixels
  "Add two binpixels and recalculate min/max. Be sure a and b are equal. Use this function to merge results created in separated threads"
  ^BinPixels [^BinPixels a ^BinPixels b]
  (let [ch1 (future (amap ^doubles (.ch1 a) idx ret (+ (aget ^doubles (.ch1 a) idx) (aget ^doubles (.ch1 b) idx))))
        ch2 (future (amap ^doubles (.ch2 a) idx ret (+ (aget ^doubles (.ch2 a) idx) (aget ^doubles (.ch2 b) idx))))
        ch3 (future (amap ^doubles (.ch3 a) idx ret (+ (aget ^doubles (.ch3 a) idx) (aget ^doubles (.ch3 b) idx))))
        ^doubles bins (amap ^doubles (.bins a) idx ret (+ (aget ^doubles (.bins a) idx) (aget ^doubles (.bins b) idx)))]
    (BinPixels. bins @ch1 @ch2 @ch3 (.sizex a) (.sizey a) (.sizex+ a) (.fnormx a) (.fnormy a))))
