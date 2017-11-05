;; # Namespace scope
;;
;; Pixels represent raw color data take from image or canvas.
;; `pixels` namespace consist of:
;;
;; * Pixels type definition with set of mutating functions
;; * Parallel filter processors
;; * Filters
;; * BinPixels to support log-density (bin based) rendering process.
;;
;; Pixels are stored in `ints` array as separeted channels in one of two layouts: planar (default) and interleaved.
;; To operate on pixels you have several methods:
;;
;; * whole channel, channel value or color getters and setters for specified position and/or channel
;; * creators - rom and to: file, image, canvas
;; * layout converters
;; * parallel processors for filtering colors or channels; composition of `pixels`
;; * filters: `dilate`, `erode`, `box-blur`, `gaussian-blur`, `posterize`, `threshold`, `quantile`, `tint`, `normalize`, `equalize`, `negate`.

(ns clojure2d.pixels
  (:require [clojure2d.color :as c]
            [clojure2d.core :as core]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v])
  (:import clojure2d.core.Canvas
           [clojure2d.math.vector Vec2 Vec4]
           java.awt.image.BufferedImage
           [clojure.lang Counted]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; `*pixels-edge*` describe what is the value of pixel when accessing from outside the image.
;; Possible values are:
;;
;; * `:zero` - set `0` 
;; * `:edge` - value from the edge of the image (left, right, top, bottom or corners)
;; * `:wrap` - wrap around image
;; * or channel value 0-255 - set specific value
;;
(def ^:dynamic *pixels-edge* :edge)

;; ## Pixels type

(declare image-from-pixels)

(defprotocol PixelsProto
  "Functions for accessing and setting channel values or colors."
  (get-value [pixels ch idx] [pixels ch x y] "get channel value by index or position")
  (get-color [pixels idx] [pixels x y] "get color by index or position")
  (set-value [pixels ch idx v] [pixels ch x y v] "set channel value by index or position")
  (set-color [pixels idx v] [pixels x y v] "set color value by index or position")
  (idx->pos [pixels idx] "convert index to position")
  (get-channel [pixels ch] "return whole `ints` array with chosen channel")
  (set-channel [pixels ch v] "set whole channel (as `ints` array)"))

;; Pixels type contains:
;;
;; * p - color channel values as `ints`
;; * w - width
;; * h - height
;; * planar? - is it planar layout? Note: some filters work only on planar layout
;; * size - channel size
;; * pos - function returning actual position in `p` array from channel and index
;;
;; Pixels type implements also `ImageProto` from core namespace
(deftype Pixels [^ints p ^long w ^long h planar? ^long size pos]
  core/ImageProto
  (get-image [p] (image-from-pixels p))
  (width [_] w)
  (height [_] h)
  (save [p n]
    (core/save-image (image-from-pixels p) n)
    p)
  (convolve [p t]
    (core/convolve (image-from-pixels p) t))
  (get-pixel [p x y] (get-color p x y))
  Counted
  (count [_] size)
  
  PixelsProto

  (get-channel [_ ch]
    (let [^ints res (int-array size)
          off (* ^long ch size)]
      (if planar?
        (System/arraycopy p off res 0 size)
        (dotimes [idx size]
          (aset res idx (aget p (+ ^long ch (<< idx 2))))))
      res))

  (set-channel [_ ch v]
    (let [off (* ^long ch size)]
      (when planar?
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
        :edge (get-value pixels ch (m/constrain ^long x 0 (dec w)) (m/constrain ^long y 0 (dec h)))
        :wrap (get-value pixels ch (unchecked-int (m/wrap 0 w x)) (unchecked-int (m/wrap 0 h y)))
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
        :edge (get-color pixels (m/constrain ^long x 0 (dec w)) ((m/constrain ^long y 0 (dec h))))
        :wrap (get-color pixels (unchecked-int (m/wrap 0 w x)) (unchecked-int (m/wrap 0 h y)))
        (Vec4. *pixels-edge* *pixels-edge* *pixels-edge* 255))
      (get-color pixels (+ ^long x (* ^long y w)))))

  (set-value [_ ch idx v]
    (aset ^ints p (unchecked-int (pos ch idx)) (unchecked-int v))
    p)

  (set-value [pixels ch x y v]
    (set-value pixels ch (+ ^long x (* ^long y w)) v))

  (set-color [_ idx v] 
    (let [^Vec4 v v]
      (aset ^ints p ^long (pos 0 idx) (c/clamp255 (.x v)))
      (aset ^ints p ^long (pos 1 idx) (c/clamp255 (.y v)))
      (aset ^ints p ^long (pos 2 idx) (c/clamp255 (.z v)))
      (aset ^ints p ^long (pos 3 idx) (c/clamp255 (.w v))))
    p)

  (set-color [pixels x y v]
    (set-color pixels (+ ^long x (* ^long y w)) v))

  (idx->pos [_ idx]
    (let [y (quot ^long idx w)
          x (rem ^long idx w)]
      (Vec2. x y)))
  
  Object
  (toString [_] (str "pixels (" w ", " h "), " (if planar? "planar" "interleaved"))))

;; ## Pixels creators

(defn- make-value-selector
  "Create function which returns position in internal `Pixels` array for given index. Based on layout."
  [planar? ^long size]
  (if planar?
    (fn ^long [^long ch ^long idx]
      (+ idx (* ch size)))
    (fn ^long [^long ch ^long idx]
      (+ ch (<< idx 2)))))

(defn make-pixels
  "Create empty `Pixels` in given layout, sets valid channel value selector (pos) depending on layout"
  ([^ints a ^long w ^long h planar?]
   (let [size (* w h)
         pos (make-value-selector planar? size)]
     (Pixels. a w h planar? size pos)))
  ([w h]
   (make-pixels w h true))
  ([^long w ^long h planar?]
   (make-pixels (int-array (* 4 w h)) w h planar?)))

(defn replace-pixels
  "Create new `Pixels` for given raw `ints` pixels"
  ([^Pixels p ^ints a planar?]
   (Pixels. a (.w p) (.h p) planar? (.size p) (make-value-selector planar? (.size p))))
  ([^Pixels p a]
   (replace-pixels p a (.planar? p))))

(defn clone-pixels
  "Clone Pixels, returns new object"
  [^Pixels p]
  (let [len (alength ^ints (.p p))
        res (int-array len)]
    (System/arraycopy (.p p) 0 ^ints res 0 len)
    (replace-pixels p res)))

;; ## Layout converters

(defn to-planar
  "Convert interleaved (Java2d native) layout of pixels to planar"
  [^Pixels p]
  (let [f (fn ^long [^long x]
            (let [q (quot x (.size p))
                  r (rem x (.size p))]
              (+ q (<< r 2))))]
    (replace-pixels p (amap ^ints (.p p) idx ret (unchecked-int (aget ^ints (.p p) (unchecked-int (f idx))))) true)))

(defn from-planar
  "Convert planar pixel layout to interleaved"
  [^Pixels p]
  (let [f (fn ^long [^long x]
            (let [q (>> x 0x2)
                  r (bit-and x 0x3)]
              (+ q (* (.size p) r))))]
    (replace-pixels p (amap ^ints (.p p) idx ret (unchecked-int (aget ^ints (.p p) (unchecked-int (f idx))))) false)))

;; ## Pixels converters

(defn get-image-pixels
  "Create Pixels from image, convert to planar layout on default"
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
  ([b planar?]
   (get-image-pixels b 0 0 (core/width b) (core/height b) planar?))
  ([b]
   (get-image-pixels b true)))

(defn set-image-pixels!
  "Set pixels to image, mutating it"
  ([^BufferedImage b x y ^Pixels pin]
   (let [^Pixels p (if (.planar? pin) (from-planar pin) pin)] 
     (.. b
         (getRaster)
         (setPixels ^int x ^int y (unchecked-int (.w p)) (unchecked-int (.h p)) ^ints (.p p))))
   b)
  ([b p]
   (set-image-pixels! b 0 0 p)))

(defn image-from-pixels
  "Create image from given pixels."
  [^Pixels p]
  (let [bimg (BufferedImage. (.w p) (.h p) BufferedImage/TYPE_INT_ARGB)]
    (set-image-pixels! bimg p)))

(defn get-canvas-pixels
  "Get pixels from canvas"
  ([^Canvas canvas x y w h planar?]
   (get-image-pixels (.buffer canvas) x y w h planar?))
  ([^Canvas canvas x y w h]
   (get-image-pixels (.buffer canvas) x y w h true))
  ([^Canvas canvas]
   (get-image-pixels (.buffer canvas)))
  ([^Canvas canvas planar?]
   (get-image-pixels (.buffer canvas) planar?)))

(defn set-canvas-pixels!
  "Set pixels to canvas"
  ([^Canvas canvas p x y]
   (set-image-pixels! (.buffer canvas) x y p))
  ([^Canvas canvas p]
   (set-image-pixels! (.buffer canvas) p)))

(defn load-pixels
  "Load pixels from file"
  [n]
  (get-image-pixels (core/load-image n)))

;; ## Processors
;;
;; Following functions are used to filter pixels in parallel way. You can filter channel values with selected filter or you can filter whole colors. It's done in parallel way using as many cores you have. Processor are immutable (which can lead to memory issues).

(defn filter-colors-xy
  "Filter colors. Filtering function should accept `Vec4` color and position as x,y values and return `Vec4`."
  [f ^Pixels p]
  (let [target (clone-pixels p)
        pre-step (max 40000 (/ (.size p) core/available-tasks))
        step (unchecked-int (m/ceil pre-step))
        parts (range 0 (.size p) step)
        ftrs (doall
              (map
               #(future (let [end (min (+ ^long % step) (.size p))]
                          (loop [idx (unchecked-int %)]
                            (when (< idx end)
                              (let [^Vec2 pos (idx->pos p idx)]
                                (set-color target idx (f (get-color p idx) (.x pos) (.y pos))))
                              (recur (inc idx))))))
               parts))]
    (run! deref ftrs)
    target))

(defn filter-colors
  "Filter pixels color-wise with given function. Filtering function should accept and return color as `Vec4`. You can use color space conversion functions defined in `color` namespace."
  [f p]
  (filter-colors-xy (fn [c _ _] (f c)) p))

(defn filter-channel
  "Filter one channel, write result into target. Works only on planar.
   Passed function should accept and return `int`."
  [f ^long ch ^Pixels target ^Pixels p]
  (let [size (.size target)
        start (* ch size)
        stop (* (inc ch) size)]
    (loop [idx (unchecked-int start)]
      (when (< idx stop)
        (aset ^ints (.p target) idx ^int (f (aget ^ints (.p p) idx)))
        (recur (unchecked-inc idx))))
    true))

(defn filter-channel-xy
  "Filter one channel, write result into target. Works on planar/interleaved.
   Function parameters are: channel, pixels, x and y position."
  [f ch ^Pixels target p]
  (dotimes [y (.h target)]
    (dotimes [x (.w target)]
      (set-value target ch x y (f ch p x y))))
  true)

(defn filter-channels
  "Filter channels parallelly with filtering function. Build filtering function from `filter-channel` or `filter-channel-xy` helpers as with filter aplied partially. Filtering function parameters are: channel, target and source pixels.

  You can pass one filter for all channels, separately for channels, you can skip alpha channel, or select channels individually (just pass `nil`). All channels are processed in 4 future threads."
  ([f0 f1 f2 f3 p]
   (let [target (clone-pixels p)
         ch0 (future (when f0 (f0 0 target p)))
         ch1 (future (when f1 (f1 1 target p)))
         ch2 (future (when f2 (f2 2 target p)))
         ch3 (future (when f3 (f3 3 target p)))]
     (run! deref [ch0 ch1 ch2 ch3])
     target))
  ([f p]
   (filter-channels f f f f p))
  ([f do-alpha p]
   (if do-alpha
     (filter-channels f f f f p)
     (filter-channels f f f nil p))))

(defn blend-channel
  "Blend one channel, write result into target. Works only on planar. Blending function should accept two `int` values (as channel values) and return `int`. If you want to use blending functions which are predefined in `color` namespace use `compose-channels` filter. This is "
  [f ch ^Pixels target ^Pixels p1 ^Pixels p2]
  (let [size (.size target)
        start (* ^long ch size)
        stop (* (inc ^long ch) size)]
    (loop [idx start]
      (when (< idx stop)
        (aset ^ints (.p target) idx ^int (f (aget ^ints (.p p1) idx) (aget ^ints (.p p2) idx)))
        (recur (unchecked-inc idx))))
    true))

(defn blend-channel-xy
  "Blend one channel, write result into target. Works on planar/interleaved. Blending function should accept channel, target and two source `pixels`."
  [f ch ^Pixels target p1 p2]
  (dotimes [y (.h target)]
    (dotimes [x (.w target)]
      (set-value target ch x y (f ch p1 p2 x y))))
  true)

(defn blend-channels
  "Blend channels parallelly. Similar to `filter-channels`. Bleding function should accept: channel, target and two source pixels"
  ([f0 f1 f2 f3 p1 p2]
   (let [target (clone-pixels p1)
         ch0 (future (when f0 (f0 0 target p1 p2)))
         ch1 (future (when f1 (f1 1 target p1 p2)))
         ch2 (future (when f2 (f2 2 target p1 p2)))
         ch3 (future (when f3 (f3 3 target p1 p2)))]
     (run! deref [ch0 ch1 ch2 ch3])
     target))
  ([f p1 p2]
   (blend-channels f f f f p1 p2))
  ([f do-alpha p1 p2]
   (if do-alpha
     (blend-channels f f f f p1 p2)
     (blend-channels f f f nil p1 p2))))

;; ## Compose channels filter

(defn make-compose-f
  "Create compose blending function"
  [n]
  (cond 
    (keyword? n) (partial blend-channel (partial c/blend-values (n c/blends)))
    (nil? n) nil
    :else (partial blend-channel (partial c/blend-values n))))

(def make-compose (memoize make-compose-f))

(defn compose-channels
  "Compose channels with blending functions. You can give blending method name as keyword defined in `blends-names` variable from `color` namespace. Or it can be blending function which accepts 2 doubles from 0.0 to 1.0 and returns double (0.0 - 1.0). Behaviour is similar to `filter-channels` or `blend-channels` method."
  ([n1 n2 n3 n4 p1 p2]
   (blend-channels (make-compose n1) (make-compose n2) (make-compose n3) (make-compose n4) p1 p2))
  ([n p1 p2]
   (compose-channels n n n n p1 p2))
  ([n do-alpha p1 p2]
   (if do-alpha
     (compose-channels n n n n p1 p2)
     (compose-channels n n n nil p1 p2))))

;; ## Filters

;; ### Value accessors

(defn get-3x3
  "Return 9 channel values around given position."
  [ch p ^long x ^long y]
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
  "Return value from 5 (cross) values using binary functions."
  [f ch p x y]
  (f (get-value p ch x (dec ^long y))
     (f (get-value p ch (dec ^long x) y)
        (f (get-value p ch x y)
           (f (get-value p ch (inc ^long x) y)
              (get-value p ch x (inc ^long y)))))))

;; Create dilate and erode filters
(def dilate-filter (partial filter-channel-xy (partial get-cross-f #(max ^int %1 ^int %2))))
(def erode-filter (partial filter-channel-xy (partial get-cross-f #(min ^int %1 ^int %2))))

(defn- make-aget-2d
  "2d `ints` array getter with `:edge` boundary."
  [^ints array ^long w ^long h]
  (fn local-aget-2d ^long [^long x ^long y]
    (if (bool-or (neg? x)
                 (neg? y)
                 (>= x w)
                 (>= y h))
      (local-aget-2d (m/constrain x 0 (dec w)) (m/constrain y 0 (dec h)))
      (aget array (+ x (* y w))))))

;; ### Blurs

;; Algorithm based on http://blog.ivank.net/fastest-gaussian-blur.html

(defn box-blur-v
  "Vertical pass for blur."
  [^long r ^long w ^long h ^ints in]
  (if (zero? r) in
      (let [aget-2d (make-aget-2d in w h)
            size (* w h)
            ^ints target (int-array size)
            iarr (/ (inc (+ r r)))
            r+ (inc r)
            rang (range (- r+) r)]
        (dotimes [x w]
          (let [val (reduce #(+ ^long %1 ^long (aget-2d x %2)) 0 rang)]
            (loop [y (int 0)
                   v (int val)]
              (when (< y h)
                (let [nv (- (+ v ^long (aget-2d x (+ y r)))
                            ^long (aget-2d x (- y r+)))]
                  (aset ^ints target (+ x (* w y)) (unchecked-int (* (double nv) iarr)))
                  (recur (unchecked-inc y) nv))))))
        target)))

(defn box-blur-h
  "Horizontal pass for blur."
  [^long r ^long w ^long h ^ints in]
  (if (zero? r) in
      (let [aget-2d (make-aget-2d in w h)
            size (* w h)
            ^ints target (int-array size)
            iarr (/ (inc (+ r r)))
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
                  (aset ^ints target (+ x off) (unchecked-int (* (double nv) iarr)))
                  (recur (unchecked-inc x) nv))))))
        target)))

(defn box-blur
  "Box blur two passes for given radius."
  [r ch ^Pixels target ^Pixels p]
  (let [buf (get-channel p ch)
        result (box-blur-v r (.w p) (.h p) (box-blur-h r (.w p) (.h p) buf))]
    (set-channel target ch result))
  nil)

(defn make-radius-blur
  "Create blur with given blur filter and radius"
  ([f radius]
   (partial f radius))
  ([f]
   (make-radius-blur f 2)))

;; Box blur creator
(def make-box-blur (partial make-radius-blur box-blur))

;; Create 4 box blurs for radiuses 1,2,3 and 5
(def box-blur-1 (make-box-blur 1))
(def box-blur-2 (make-box-blur 2))
(def box-blur-3 (make-box-blur 3))
(def box-blur-5 (make-box-blur 5))

(defn radius-for-gauss
  "Calculate radius for gaussian blur."
  [^double sigma ^double n]
  (let [sigma* (* 12.0 sigma sigma)
        w-ideal (-> sigma*
                    (/ n)
                    (inc)
                    (m/sqrt)
                    (m/floor))
        wl (if (even? (unchecked-int w-ideal)) (dec w-ideal) w-ideal)
        wu (+ wl 2.0)
        m-ideal (/ (- sigma* (* n (m/sq wl)) (* 4.0 n wl) (* 3.0 n))
                   (- (* -4.0 wl) 4.0))
        m (m/round m-ideal)]
    (vec (map #(int (/ (dec (if (< ^long % m) wl wu)) 2.0)) (range n)))))

(def radius-for-gauss-memo (memoize radius-for-gauss))

(defn gauss-blur
  "Proceed with Gaussian Blur on channel, save result to target for given radius"
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

;; Gaussian blur creator
(def make-gaussian-blur (partial make-radius-blur gauss-blur))

;; Create 4 gaussian blurs for radiuses: 1,2,3 and 5
(def gaussian-blur-1 (make-gaussian-blur 1))
(def gaussian-blur-2 (make-gaussian-blur 2))
(def gaussian-blur-3 (make-gaussian-blur 3))
(def gaussian-blur-5 (make-gaussian-blur 5))

;; ### Posterize

(defn posterize-levels-fun
  "Calculate levels"
  [^long numlev]
  (let [f (fn ^long [^long idx] (-> idx
                                    (* numlev)
                                    (>> 8)
                                    (* 255)
                                    (/ (dec numlev))))
        ^ints tmp (int-array 256)]
    (amap tmp idx ret ^long (f idx))))

(def posterize-levels (memoize posterize-levels-fun))

(defn posterize-pixel
  "Posterize one pixel"
  [levels v]
  (aget ^ints levels (unchecked-int v)))

(defn make-posterize
  "Create posterize filter"
  ([^long numlev]
   (let [nl (m/constrain numlev 2 255)
         ^ints levels (posterize-levels nl)]
     (partial filter-channel (partial posterize-pixel levels))))
  ([]
   (make-posterize 6)))

;; Create 3 posterize filters for: 4, 8 and 16 channel values.
(def posterize-4 (make-posterize 4))
(def posterize-8 (make-posterize 8))
(def posterize-16 (make-posterize 16))

;; ### Threshold

(defn threshold-pixel
  "Threshold one pixel."
  ^long [^long thr ^long v]
  (if (< v thr) 255 0))

(defn make-threshold
  "Create threshold filter for given value."
  ([^double thr]
   (let [t (unchecked-long (* 256.0 thr))]
     (partial filter-channel (partial threshold-pixel t))))
  ([]
   (make-threshold 0.5)))

;; Create 3 threshold filters for 0.25, 0.5 and 0.75 threshold values.
(def threshold-25 (make-threshold 0.25))
(def threshold-50 (make-threshold))
(def threshold-75 (make-threshold 0.75))

;; ### Median and Quantile filters

(defn quantile
  "Fast quantile on 9 values"
  [n vs]
  (let [vss (vec (sort vs))]
    (vss n)))

(defn make-quantile-filter
  "Create quantile filter for values from 0 to 8"
  [^long v]
  (let [q (m/constrain v 0 8)]
    (partial filter-channel-xy (comp (partial quantile q) get-3x3))))

;; Create median filter int terms of quantile filter.
(def median-filter (make-quantile-filter 4))

;; ### Tint

(defn- calc-tint
  "Calculate tint"
  ^long [^long a ^long b]
  (>> (bit-and 0xff00 (* a b)) 8))

(defn- make-tint-map
  "Create tint lookup tables."
  [r g b a]
  (vec (map #(amap ^ints (int-array 256) idx ret ^long (calc-tint idx %)) [r g b a])))

(defn- make-tinter
  "Lookup wrapper"
  [^ints a]
  (fn [v] (aget ^ints a ^int v)))

(defn make-tint-filter
  "Create tint filter"
  ([r g b a]
   (let [tm (make-tint-map r g b a)]
     (fn [ch target p]
       (let [tinter (make-tinter (tm ch))]
         (filter-channel tinter ch target p)))))
  ([r g b]
   (make-tint-filter r g b 256)))

;; ### Modulate filter
;;
;; Multiply color channels by specified values from 0-2

(defn make-modulate-filter
  ""
  ([ch1 ch2 ch3 ch4]
   (fn [^long ch target p]
     (let [chv (case ch
                 0 ch1
                 1 ch2
                 2 ch3
                 ch4)]
       (filter-channel #(m/constrain (* (double %) ^long chv) 0.0 255.0) ch target p))))
  ([ch1 ch2 ch3]
   (make-modulate-filter ch1 ch2 ch3 1.0)))

;; ### Normalize

(defn normalize
  "Normalize channel values to whole (0-255) range"
  [ch target ^Pixels p]
  (let [sz (.size p)
        [mn mx] (loop [idx (int 0)
                       currmn (long 1000)
                       currmx (long -1000)]
                  (if (< idx sz)
                    (let [^int v (get-value p ch idx)]
                      (recur (unchecked-inc idx)
                             (min v currmn)
                             (max v currmx)))
                    [currmn currmx]))
        mnd (double mn)
        mxd (double mx)]
    (filter-channel #(* 255.0 (m/norm % mnd mxd)) ch target p)))

;; create an alias
(def normalize-filter normalize)

;; ### Equalize histogram

(defn- equalize-make-histogram
  "Create histogram"
  [ch ^Pixels p]
  (let [^doubles hist (double-array 256 0.0)
        sz (.size p)
        d (/ (.size p))]
    (loop [idx (int 0)]
      (if (< idx sz)
        (let [c (get-value p ch idx)]
          (aset ^doubles hist c (+ d (aget ^doubles hist c)))
          (recur (unchecked-inc idx)))
        hist))))

(defn- equalize-make-lookup
  "Make lookup table based on histogram."
  [^doubles hist]
  (let [^ints lookup (int-array 256 0)]
    (loop [currmn Integer/MAX_VALUE
           currmx Integer/MIN_VALUE
           sum 0.0
           idx 0]
      (if (< idx 256)
        (let [currsum (+ sum (aget ^doubles hist idx))
              val (m/round (m/constrain (* sum 255.0) 0.0 255.0))]
          (aset ^ints lookup idx val)
          (recur (min val currmn)
                 (max val currmx)
                 currsum
                 (unchecked-inc idx)))
        lookup))))

(defn equalize-filter
  "Equalize histogram filter"
  [ch target p]
  (let [^ints lookup (equalize-make-lookup (equalize-make-histogram ch p))]
    (filter-channel #(aget ^ints lookup ^int %) ch target p)))

;; ### Negate

(defn negate-filter
  ""
  [ch target p]
  (filter-channel #(- 255 ^int %) ch target p))

;; ## Log-density rendering
;;
;; This is a helper type to support log-density rendering where each pixel is represented by "bin". Each bin counts number of hits which is later converted to valid colour. Each color channe (red, green and blue) has its own bin.
;; Log-density rendering gives smooth output and was developed for fractal flames rendering. See: http://flam3.com/flame_draves.pdf
;; After rendering `BinPixels` can be converted to `Pixels` using given configuration. You can treat it as RAW photo development.
;; Configuration is a map of values:
;;
;; * alpha-gamma - gamma for transparency (default: 2.0)
;; * color-gamma - gamma for color (default: 1.1)
;; * intensity - color intensity (default: 0.8)
;; * saturation - default: 1.0
;; * brightness - default: 1.0
;;
;; You can pass also background color for your render. Default is fully transparent black.

(defprotocol BinPixelsProto
  "BinPixel functions"
  (add-at-position [binpixels p s ch1 ch2 ch3] "add given ch values multiplied by scale for the position p")
  (add-pixel-bilinear [binpixels x y ch1 ch2 ch3] "add channel values to position x,y in bilinear mode (4 bins affected)")
  (add-pixel-simple [binpixels x y ch1 ch2 ch3] [binpixels x y s ch1 ch2 ch3] "add channel values to position x,y in linear mode (optionally with scale")
  (to-pixels [binpixels background config] [binpixels background] [binpixels] "render to pixels"))

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
  
  (add-pixel-simple [t x y s v1 v2 v3]
    (let [ivx (long (fnormx x))
          ivy (long (fnormy y))
          p (+ ivx (* ivy sizex+))]
      (when (and (< -1 ivx sizex) (< -1 ivy sizey)) 
        (add-at-position t p s v1 v2 v3)))
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
                            :or {alpha-gamma 2.0 intensity 0.8 color-gamma 1.1 saturation 1.0 brightness 1.0}}] 
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
                      col1 (aget ^doubles c/r255 (m/constrain (* rhit (aget ch1 idx)) 0 255)) ;; normalized red
                      col2 (aget ^doubles c/r255 (m/constrain (* rhit (aget ch2 idx)) 0 255)) ;; green
                      col3 (aget ^doubles c/r255 (m/constrain (* rhit (aget ch3 idx)) 0 255)) ;; and blue
                      c1 (* 255.0 (+ (* intensity col1)
                                     (* rintensity (m/pow col1 cgamma)))) ;; interpolate between color and gamma(color), intensity based
                      c2 (* 255.0 (+ (* intensity col2)
                                     (* rintensity (m/pow col2 cgamma))))
                      c3 (* 255.0 (+ (* intensity col3)
                                     (* rintensity (m/pow col3 cgamma))))
                      color (v/applyf (Vec4. c1 c2 c3 255.0) c/clamp255)
                      color (if (bool-and (== 1.0 saturation) (== 1.0 brightness))
                              color
                              (c/from-HSB (v/applyf (v/emult multiplier (c/to-HSB color)) c/clamp255)))] ;; apply brightness, saturation factors
                  (set-color p (+ x row) (v/interpolate background color alpha)))))))) ;; store!
      p))
  (to-pixels [t background] (to-pixels t background {}))
  (to-pixels [t] (to-pixels t (Vec4. 0.0 0.0 0.0 0.0) {}))
  core/ImageProto
  (get-image [b] (core/get-image (to-pixels b)))
  (width [_] sizex)
  (height [_] sizey)
  (save [b n] (core/save (to-pixels b) n))
  (convolve [b t] (core/convolve (to-pixels b) t))
  (get-pixel [b x y] (core/get-pixel (to-pixels b) x y)))

(defn make-binpixels 
  "Create BinPixels object"
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
  "Paralelly merge two binpixels. Be sure a and b are equal. Use this function to merge results created in separated threads"
  ^BinPixels [^BinPixels a ^BinPixels b]
  (let [ch1 (future (amap ^doubles (.ch1 a) idx ret (+ (aget ^doubles (.ch1 a) idx) (aget ^doubles (.ch1 b) idx))))
        ch2 (future (amap ^doubles (.ch2 a) idx ret (+ (aget ^doubles (.ch2 a) idx) (aget ^doubles (.ch2 b) idx))))
        ch3 (future (amap ^doubles (.ch3 a) idx ret (+ (aget ^doubles (.ch3 a) idx) (aget ^doubles (.ch3 b) idx))))
        ^doubles bins (amap ^doubles (.bins a) idx ret (+ (aget ^doubles (.bins a) idx) (aget ^doubles (.bins b) idx)))]
    (BinPixels. bins @ch1 @ch2 @ch3 (.sizex a) (.sizey a) (.sizex+ a) (.fnormx a) (.fnormy a))))
