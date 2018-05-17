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
  "Operations on pixel levels.

  ## Content

  Namespace defines three main concepts:

  * Pixels - channel values packed into array.
  * Processors - parallel Pixels processing functions (like filters).
  * Bins - log density renderer

  ## Pixels

  Pixels is type which represents image as int array divided into color channels. Layout is linear and interleaved which means that array is 1D and each pixel is represented by four consecutive values R, G, B, A. After first row goes second and so on.

  Pixels allows mutation, you can read and set channel value or color:

  * [[get-value]], [[set-value]] - read or set channel value for given pixel and channel. Value should be within `[0-255]` range.
  * [[get-color]], [[set-color]] - read or set color for given pixel. Returned color has [[Vec4]] type.
  * [[get-channel]], [[set-channel]] - read or set whole channel as `ints`.

  Pixel access can be made by `(x,y)` coordinates or by index which is equivalent to `(+ x (* y width))`.

  Pixels implement [[ImageProto]].

  ### Creation / conversions

  To create empty Pixels, call [[pixels]].

  You can also get and set Pixels from and to Images and Canvases or read from file.

  ## Processors

  Library supports several processing functions and helpers to parallely manipulate channels or colors. All functions are not destrictive, that means new object is created to store result of manipulation. Every processor accept one or more filtering functions which do the job.
  There are three main functions:

  * [[filter-colors]] - to process colors. Uses function `f` which accepts color and should return color. Can be used to convert Pixels between different color spaces.
  * [[filter-channels]] - to process channel values. Uses function `f` which accepts channel number (values from 0 to 3), target Pixels and source Pixels and returns integer. You can provide different function for every channel. Can be used to apply filter (like blur).
  * [[blend-channels]] - to process pair of Pixels. Uses function `f` which accepts channel number, target and two Pixel values. [[compose-channels]] wrapper can be used to compose two Pixels using one of the blending functions defined in [[clojure2d.colors]] namespace.

  Additionally other processing functions are prepared in case you want write own filters or converters:

  * [[filter-colors-xy]] - process colors using function `f` which accepts Pixels and current position.
  * [[filter-channel]] - iterate through channel, `f` accepts channel value and returns new channel value
  * [[filter-channel-xy]] - iterate through channel, `f` accepts channel, Pixels and x,y position
  * [[blend-channels]] and [[blend-channel-xy]] - similar to two above, `f` accepts two Pixels instead of one.

  ### Color space

  To convert whole Pixels into different color space use [[filter-colors]] and pass one of the color space conversion functions defined under [[colorspaces*]]. Always use normalized version.

  ```
  (filter-colors c/to-HSB* pixels-object)
  ```
  
  ### Filters

  There are several ready to use filters. All defined under [[filters-list]] variable. Some of the filters are creators and should be called with parametrization.

  ```
  (filter-channels gaussian-blur-3 pixels-object)
  (filter-channels (posterize 10) pixels-object)
  ```

  ### Composing

  To compose two Pixels use [[compose-channels]] and use name of composing function defined [[blends-list]]. Instead of name you can pass custom composing function.

  ```
  (compose-channels :multiply pixels-1 pixels-2)
  ```
  
  ## Log Density Rendering

  Log Density Renderer was orginally created for fractal flames rendering and produces very smooth results. Details are described in this [paper](http://flam3.com/flame.pdf).

  Renderer is point based (no other primitives) and supports selection of antialiasing (reconstruction) filters. Density estimation is not supported.

  Rendering algorithm collects color channels values and counts number of hits for each pixel. For each pixel weighted average of all color values is calculated and log of number of hits gives alpha value. Pixel color is blended with background using alpha.

  ### Rendering

  First you have to create renderer with [[renderer]] function. By default no filter is used. 
  In case you want to use filter call with: filter name as keyword (see below), optional: filter radius (default 2.0) and other filter parameters.

  To set point call [[set-color]].
  
  #### Antialiasing filters

  Below you have list of all available antialiasing filters:
  
  * :gaussian - two parameters, radius and alpha (default: 2.0)
  * :box - one parameter, radius (use 0.5)
  * :sinc - Lanczos filter, two parameters, radius and tau (default: 1.0)
  * :mitchell - Mitchell-Netravali filter, three parameters, radius, B and C (default: 1/3)
  * :cubic - one parameter, radius
  * :catmull - one parameter, radius
  * :triangle - one parameter, radius
  * :cosinebell - two parameters, radius and xm (default: 0.5)
  * :blackmann-harris - one parameter, radius

  ### Converting

  To convert renderer to Pixels just call [[to-pixels]] method with optional configuration. Configuration gives you possibility to control process of transformation to RGB data.

  Configuration is a map with following fields:

  * :background - color of the background (default: :black)
  * :gamma-alpha - gamma correction for alpha, to adjust blending strength.
  * :gamma-color - gamma correction for color, to adjust intensity
  * :intensity:
      1.0 - use calculated color
      0.0 - use gamma corrected color
      (0.0-1.0) - mix between above
  * :saturation - adjust saturation (0-2)
  * :brightness - adjust brightness (0-2)
  * :contrast - adjust contrast (0-2)
  
  ### Parallel rendering

  Construction of renderer enables parallel computing. Just create as many renderers as you want (you may use [[available-tasks]] value), run rendering in separate threads and then merge result with [[merge-renderers]]."
  {:metadoc/categories {:pix "Pixels"
                        :filt "Filters"
                        :ld "Log density renderer"}}
  (:require [clojure2d.color :as c]
            [clojure2d.core :as core]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.random :as r])
  (:import [clojure2d.core Canvas Window]
           [fastmath.vector Vec2 Vec4]
           [java.awt.image BufferedImage]
           [clojure.lang Counted]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;; 
;;
(def ^{:doc "If you access pixels which is outside possible range. You'll always get some value. You can control what is returned by setting this variable. Possible values are:

  * `:zero` - set `0` 
  * `:edge` - value from the edge of the image (left, right, top, bottom or corners) (default)
  * `:wrap` - wrap around image
  * or channel value 0-255 - set specific value"}
  ^:dynamic *pixels-edge* :edge)

;; ## Pixels type

(defprotocol PixelsProto
  "Functions for accessing and setting channel values or colors. PixelsProto is used in following types:

  * `Pixels` - all functions
  * `Image`, `Canvas`, `Window` - Only [[get-value]] and [[get-color]] for given position and conversion to Pixels. Accessing color or channel value is slow.
  * `Low density renderer` - Only [[set-color]], [[get-color]] and conversion to Pixels.  "
  (^{:metadoc/categories #{:pix}} get-value [pixels ch x y] [pixels ch idx] "Get channel value by index or position.")
  (^{:metadoc/categories #{:pix :ld}} get-color [pixels x y] [pixels idx] "Get color by index or position. In case of low density rendering returns current average color without alpha value.")
  (^{:metadoc/categories #{:pix}} set-value [pixels ch x y v] [pixels ch idx v] "Set channel value by index or position")
  (^{:metadoc/categories #{:pix :ld}} set-color [pixels x y v] [pixels idx v] "Set color value by index or position.")
  (^{:metadoc/categories #{:pix}} get-channel [pixels ch] "Return whole `ints` array with chosen channel")
  (^{:metadoc/categories #{:pix}} set-channel [pixels ch v] "Set whole channel (as `ints` array)")
  (^{:metadoc/categories #{:pix :ld}} to-pixels [pixels] [pixels cfg] "Convert to Pixels. For low density rendering provide configuration. Works with Image/Canvas/Window and low density renderer."))

(deftype Pixels [^ints p ^int w ^int h ^int size]
  
  core/ImageProto
  (get-image [_] (clojure2d.java.Pixels/imageFromPixels p w h))
  (width [_] w)
  (height [_] h)
  (save [px n]
    (core/save-image (clojure2d.java.Pixels/imageFromPixels p w h) n)
    px)
  (convolve [_ t]
    (core/convolve (clojure2d.java.Pixels/imageFromPixels p w h) t))
  
  Counted
  (count [_] size)
  
  PixelsProto
  (get-channel [_ ch] (clojure2d.java.Pixels/getChannel p ch))
  (set-channel [px ch v] (clojure2d.java.Pixels/setChannel p ch v) px) 

  (get-value [_ ch idx]
    (clojure2d.java.Pixels/getValue p ch idx))
  
  (get-value [_ ch x y]
    (condp = *pixels-edge*
      :zero (clojure2d.java.Pixels/getValue p ch x y w h 0)
      :edge (clojure2d.java.Pixels/getValue p ch x y w h -1)
      :wrap (clojure2d.java.Pixels/getValue p ch x y w h -2)
      (clojure2d.java.Pixels/getValue p ch x y w h *pixels-edge*)))

  (get-color [_ idx]
    (clojure2d.java.Pixels/getColor p idx))
  
  (get-color [_ x y]
    (condp = *pixels-edge*
      :zero (clojure2d.java.Pixels/getColor p x y w h 0)
      :edge (clojure2d.java.Pixels/getColor p x y w h -1)
      :wrap (clojure2d.java.Pixels/getColor p x y w h -2)
      (clojure2d.java.Pixels/getColor p x y w h *pixels-edge*)))

  (set-value [px ch x y v]
    (clojure2d.java.Pixels/setValue p ch x y w v)
    px)

  (set-value [px ch idx v]
    (clojure2d.java.Pixels/setValue p ch idx v)
    px)
  
  (set-color [px x y v]
    (clojure2d.java.Pixels/setColor p x y w (c/to-color v))
    px)

  (set-color [px idx v]
    (clojure2d.java.Pixels/setColor p idx (c/to-color v))
    px)

  (to-pixels [p] p)
  
  Object
  (toString [_] (str "pixels (" w ", " h ")")))

(defn pixels
  "Create empty `Pixels` object with [w,h] dimensions.

  Optionally you can pass `ints` array `a` as a buffer. Size of array should be `(* 4 w h)`."
  {:metadoc/categories #{:pix}}
  ([^ints a ^long w ^long h]
   (let [size (* w h)]
     (Pixels. a w h size)))
  ([^long w ^long h]
   (pixels (int-array (* 4 w h)) w h)))

(defn clone-pixels
  "Clone Pixels, returns new object"
  {:metadoc/categories #{:pix}}
  [^Pixels p]
  (let [len (alength ^ints (.p p))
        res (int-array len)]
    (System/arraycopy (.p p) 0 ^ints res 0 len)
    (pixels res (.w p) (.h p))))

;; ## Pixels converters

(defn- get-image-pixels
  "Create Pixels from image, convert to planar layout on default"
  ([b x y w h]
   (pixels (clojure2d.java.Pixels/getImagePixels b x y w h) w h))
  ([^BufferedImage b]
   (pixels (clojure2d.java.Pixels/getImagePixels b) (.getWidth b) (.getHeight b))))

(defn set-image-pixels!
  "Set `Pixels` to image, mutating it.

  Optionally you can set position when Pixels object has smaller size."
  {:metadoc/categories #{:pix}}
  ([^BufferedImage b x y ^Pixels pin]
   (clojure2d.java.Pixels/setImagePixels b x y (.w pin) (.h pin) (.p pin))
   b)
  ([b p]
   (set-image-pixels! b 0 0 p)))

(defn- get-canvas-pixels
  "Get pixels from canvas"
  ([^Canvas canvas x y w h]
   (get-image-pixels (.buffer canvas) x y w h))
  ([^Canvas canvas]
   (get-image-pixels (.buffer canvas))))

(defn set-canvas-pixels!
  "Set `Pixels` to canvas. See [[set-image-pixels!]]."
  {:metadoc/categories #{:pix}}
  ([^Canvas canvas x y p]
   (set-image-pixels! (.buffer canvas) x y p)
   canvas)
  ([^Canvas canvas p]
   (set-image-pixels! (.buffer canvas) p)
   canvas))

(defn load-pixels
  "Load `Pixels` from file."
  {:metadoc/categories #{:pix}}
  [n]
  (get-image-pixels (core/load-image n)))

(extend BufferedImage
  PixelsProto
  {:to-pixels (fn [^BufferedImage i] (get-image-pixels i))
   :get-color (fn [^BufferedImage i ^long x ^long y]
                (if (bool-or (< x 0)
                             (< y 0)
                             (>= x (.getWidth i))
                             (>= y (.getHeight i)))
                  (c/color 0 0 0)
                  (let [b (int-array 1)
                        ^java.awt.image.Raster raster (.getRaster i)]
                    (.getDataElements raster x y b)
                    (let [v (aget b 0)
                          b (bit-and v 0xff)
                          g (bit-and (>> v 8) 0xff)
                          r (bit-and (>> v 16) 0xff)
                          a (bit-and (>> v 24) 0xff)]
                      (if (== (.getNumBands raster) 3)
                        (c/color r g b)
                        (c/color r g b a))))))
   :get-value (fn [^BufferedImage i ^long ch x y]
                (let [c (get-color i x y)]
                  (case ch
                    0 (c/ch0 c)
                    1 (c/ch1 c)
                    2 (c/ch2 c)
                    3 (c/alpha c))))}
  )

(extend Canvas
  PixelsProto
  {:to-pixels (fn [^Canvas c] (get-canvas-pixels c))
   :get-color (fn [^Canvas c x y] (get-color (.buffer c) x y))
   :get-value (fn [^Canvas c ch x y] (get-value (.buffer c) ch x y))})

(extend Window
  PixelsProto
  {:to-pixels (fn [^Window w] (get-canvas-pixels @(.buffer w)))
   :get-color (fn [^Window w x y] (get-color @(.buffer w) x y))
   :get-value (fn [^Window c ch x y] (get-value @(.buffer c) ch x y))})


(defn filter-colors
  "Filter colors.

  Filtering function should accept `Vec4` color and return `Vec4`."
  {:metadoc/categories #{:filt}}
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
                              (set-color target idx (f (get-color p idx)))
                              (recur (unchecked-inc idx))))))
               parts))]
    (run! deref ftrs)
    target))

(defn filter-colors-xy
  "Filter colors.

  Filtering function should accept Pixels, position as x,y values and return `Vec4`."
  {:metadoc/categories #{:filt}}
  [f ^Pixels p]
  (let [target (clone-pixels p)
        pre-step (max 10 (/ (.w p) core/available-tasks))
        step (unchecked-int (m/ceil pre-step))
        parts (range 0 (.w p) step)
        ftrs (doall
              (map
               #(future (let [end (min (+ ^long % step) (.w p))]
                          (loop [x (unchecked-int %)]
                            (when (< x end)
                              (dotimes [y (.h p)]
                                (set-color target x y (f p x y)))
                              (recur (unchecked-inc x))))))
               parts))]
    (run! deref ftrs)
    target))

(defn filter-channel
  "Filter one channel, write result into target.

  This is helper function to create own filters.

  Function parameter is channel value and should return new value."
  {:metadoc/categories #{:filt}}
  [f ch ^Pixels target p]
  (dotimes [idx (.size target)]
    (set-value target ch idx (f (get-value p ch idx))))
  true)

(defn filter-channel-xy
  "Filter one channel, write result into target.

  This is helper function to create own filter.
  
  Function parameters are: channel, pixels, x and y position.

  Note: channel is first parameter."
  {:metadoc/categories #{:filt}}
  [f ch ^Pixels target p]
  (dotimes [y (.h target)]
    (dotimes [x (.w target)]
      (set-value target ch x y (f ch p x y))))
  true)

(defn filter-channels
  "Filter channels parallelly with filtering function. Build filtering function using `filter-channel` or `filter-channel-xy` helpers as with filter aplied partially. Filtering function parameters are: channel, target and source pixels.

  When you pass one filter, three RGB channels will be processed (arity: 2). To enable alpha set `do-alpha` parameter to `true` (arity: 3). You can also use different filter for every channel separately (arity 5). Set `nil` to skip particular channel."
  {:metadoc/categories #{:filt}}
  ([f0 f1 f2 f3 p]
   (let [target (clone-pixels p)
         ch0 (future (when f0 (f0 0 target p)))
         ch1 (future (when f1 (f1 1 target p)))
         ch2 (future (when f2 (f2 2 target p)))
         ch3 (when f3 (f3 3 target p))]
     (run! deref [ch0 ch1 ch2])
     target))
  ([f p]
   (filter-channels f f f nil p))
  ([f do-alpha p]
   (if do-alpha
     (filter-channels f f f f p)
     (filter-channels f f f nil p))))

(defn blend-channel
  "Blend one channel, write result into target.

  Blending function should two channel values and return new channel value.

  This should be considered as helper function for [[blend-channels]]."
  {:metadoc/categories #{:filt}}
  [f ch ^Pixels target p1 p2]
  (dotimes [i (.size target)]
    (set-value target ch i (f (get-value p1 ch i) (get-value p2 ch i))))
  true)

(defn blend-channel-xy
  "Blend one channel, write result into target.
  
  Blending function should accept channel, two `Pixels` and position x,y."
  {:metadoc/categories #{:filt}}
  [f ch ^Pixels target p1 p2]
  (dotimes [x (.w target)]
    (dotimes [y (.h target)]
      (set-value target ch x y (f ch p1 p2 x y))))
  true)

(defn blend-channels
  "Blend channels parallelly.

  Similar to `filter-channels`. Bleding function should accept: channel, target and two source pixels."
  {:metadoc/categories #{:filt}}
  ([f0 f1 f2 f3 p1 p2]
   (let [target (clone-pixels p1)
         ch0 (future (when f0 (f0 0 target p1 p2)))
         ch1 (future (when f1 (f1 1 target p1 p2)))
         ch2 (future (when f2 (f2 2 target p1 p2)))
         ch3 (when f3 (f3 3 target p1 p2))]
     (run! deref [ch0 ch1 ch2])
     target))
  ([f p1 p2]
   (blend-channels f f f nil p1 p2))
  ([f do-alpha p1 p2]
   (if do-alpha
     (blend-channels f f f f p1 p2)
     (blend-channels f f f nil p1 p2))))

;; ## Compose channels filter

(defn- make-compose-f
  "Create compose blending function"
  [n]
  (cond 
    (keyword? n) (partial blend-channel (partial c/blend-values (c/blends n)))
    (nil? n) nil
    :else (partial blend-channel (partial c/blend-values n))))

(def ^:private make-compose (memoize make-compose-f))

(defn compose-channels
  "Compose channels with blending functions.

  You can give blending method name as keyword defined in [[blends-names]]. Or it can be blending function which accepts 2 doubles from 0.0 to 1.0 and returns double (0.0 - 1.0). It's a wrapper for [[blend-channels]] function."
  {:metadoc/categories #{:filt}}
  ([n1 n2 n3 n4 p1 p2]
   (blend-channels (make-compose n1) (make-compose n2) (make-compose n3) (make-compose n4) p1 p2))
  ([n p1 p2]
   (compose-channels n n n nil p1 p2))
  ([n do-alpha p1 p2]
   (if do-alpha
     (compose-channels n n n n p1 p2)
     (compose-channels n n n nil p1 p2))))

;; ## Filters
(defn- make-quantile
  ""
  [no]
  (fn [ch ^Pixels target ^Pixels p]
    (clojure2d.java.filter.Quantile/process (.p p) (.p target) ch (.w p) (.h p) no)))

(def ^{:metadoc/categories #{:filt} :doc "Dilate filter. See: [[dilate-cross]]."} dilate (make-quantile 0))
(def ^{:metadoc/categories #{:filt} :doc "Quantile (2/9) filter"} quantile-1 (make-quantile 1))
(def ^{:metadoc/categories #{:filt} :doc "Quantile (3/9) filter"} quantile-2 (make-quantile 2))
(def ^{:metadoc/categories #{:filt} :doc "Quantile (4/9) filter"} quantile-3 (make-quantile 3))
(def ^{:metadoc/categories #{:filt} :doc "Median filter"} median (make-quantile 4))
(def ^{:metadoc/categories #{:filt} :doc "Quantile (6/9) filter"} quantile-5 (make-quantile 5))
(def ^{:metadoc/categories #{:filt} :doc "Quantile (7/9) filter"} quantile-6 (make-quantile 6))
(def ^{:metadoc/categories #{:filt} :doc "Quantile (8/9) filter"} quantile-7 (make-quantile 7))
(def ^{:metadoc/categories #{:filt} :doc "Erode filter. See: [[erode-cross]]."} erode (make-quantile 8))

(defn- make-quantile-cross
  ""
  [no]
  (fn [ch ^Pixels target ^Pixels p]
    (clojure2d.java.filter.Quantile/processCross (.p p) (.p target) ch (.w p) (.h p) no)))

(def ^{:metadoc/categories #{:filt} :doc "Dilate using 5 pixels. See: [[dilate]]."} dilate-cross (make-quantile-cross 0))
(def ^{:metadoc/categories #{:filt} :doc "Erode using 5 pixels. See: [[erode]]."} erode-cross (make-quantile-cross 4))

;; ### Blurs

(defn horizontal-blur
  "Create horizontal blur for given radius."
  {:metadoc/categories #{:filt}}
  [radius]
  (fn [ch ^Pixels target ^Pixels p]
    (clojure2d.java.filter.Blur/horizontalBlur (.p p) (.p target) ch (.w p) (.h p) radius)))

(def ^{:metadoc/categories #{:filt}} horizontal-blur-1 (horizontal-blur 1))
(def ^{:metadoc/categories #{:filt}} horizontal-blur-2 (horizontal-blur 2))
(def ^{:metadoc/categories #{:filt}} horizontal-blur-3 (horizontal-blur 3))
(def ^{:metadoc/categories #{:filt}} horizontal-blur-5 (horizontal-blur 5))

(defn vertical-blur
  "Create vertical blur for given radius."
  {:metadoc/categories #{:filt}}
  [radius]
  (fn [ch ^Pixels target ^Pixels p]
    (clojure2d.java.filter.Blur/verticalBlur (.p p) (.p target) ch (.w p) (.h p) radius)))

(def ^{:metadoc/categories #{:filt}} vertical-blur-1 (vertical-blur 1))
(def ^{:metadoc/categories #{:filt}} vertical-blur-2 (vertical-blur 2))
(def ^{:metadoc/categories #{:filt}} vertical-blur-3 (vertical-blur 3))
(def ^{:metadoc/categories #{:filt}} vertical-blur-5 (vertical-blur 5))

(defn box-blur
  "Create box blur for given radius."
  {:metadoc/categories #{:filt}}
  [radius]
  (fn [ch ^Pixels target ^Pixels p]
    (clojure2d.java.filter.Blur/boxBlur (.p p) (.p target) ch (.w p) (.h p) radius)))

(def ^{:metadoc/categories #{:filt}} box-blur-1 (box-blur 1))
(def ^{:metadoc/categories #{:filt}} box-blur-2 (box-blur 2))
(def ^{:metadoc/categories #{:filt}} box-blur-3 (box-blur 3))
(def ^{:metadoc/categories #{:filt}} box-blur-5 (box-blur 5))

(defn gaussian-blur
  "Create gaussian blur for given radius."
  {:metadoc/categories #{:filt}}
  [radius]
  (fn [ch ^Pixels target ^Pixels p]
    (clojure2d.java.filter.Blur/gaussianBlur (.p p) (.p target) ch (.w p) (.h p) radius)))

(def ^{:metadoc/categories #{:filt}} gaussian-blur-1 (gaussian-blur 1))
(def ^{:metadoc/categories #{:filt}} gaussian-blur-2 (gaussian-blur 2))
(def ^{:metadoc/categories #{:filt}} gaussian-blur-3 (gaussian-blur 3))
(def ^{:metadoc/categories #{:filt}} gaussian-blur-5 (gaussian-blur 5))

;; ### Posterize

(defn posterize
  "Create posterize filter for given radius."
  {:metadoc/categories #{:filt}}
  [levels]
  (fn [ch ^Pixels target ^Pixels p]
    (clojure2d.java.filter.Posterize/process (.p p) (.p target) ch levels)))

;; Create 3 posterize filters for: 4, 8 and 16 channel values.
(def ^{:metadoc/categories #{:filt}} posterize-4 (posterize 4))
(def ^{:metadoc/categories #{:filt}} posterize-8 (posterize 8))
(def ^{:metadoc/categories #{:filt}} posterize-16 (posterize 16))

;; ### Threshold

(defn threshold
  "Create threshold filter.

  You can pass `amount` from 0-1. or range.

  Note if you want b&w result first convert to gray color."
  {:metadoc/categories #{:filt}}
  ([amount]
   (fn [ch ^Pixels target ^Pixels p]
     (clojure2d.java.filter.Threshold/process (.p p) (.p target) ch amount)))
  ([amount-low amount-high]
   (fn [ch ^Pixels target ^Pixels p]
     (clojure2d.java.filter.Threshold/process (.p p) (.p target) ch amount-low amount-high))))

;; Create 3 threshold filters for 0.25, 0.5 and 0.75 threshold values.
(def ^{:metadoc/categories #{:filt}} threshold-25 (threshold 0.25))
(def ^{:metadoc/categories #{:filt}} threshold-50 (threshold 0.5))
(def ^{:metadoc/categories #{:filt}} threshold-75 (threshold 0.75))

(defn normalize
  "Normalize channel values to full range."
  {:metadoc/categories #{:filt}}
  [ch ^Pixels target ^Pixels p]
  (clojure2d.java.filter.Normalize/process (.p p) (.p target) ch))

(defn equalize
  "Equalize histogram."
  {:metadoc/categories #{:filt}}
  [ch ^Pixels target ^Pixels p]
  (clojure2d.java.filter.Equalize/process (.p p) (.p target) ch))

(defn negate
  "Negate filer."
  {:metadoc/categories #{:filt}}  
  [ch ^Pixels target ^Pixels p]
  (clojure2d.java.filter.Negate/process (.p p) (.p target) ch))

(defn solarize
  "Solarize filter."
  {:metadoc/categories #{:filt}}
  [ch ^Pixels target ^Pixels p]
  (clojure2d.java.filter.Solarize/process (.p p) (.p target) ch))

;; ### Tint

(defn tint
  "Create tinting filter.

  * one color - tint
  * two colors - interpolate between `col-low` for black and `col-high` for white.
  * three colors - like above but uses also `mid-color` for mid tones."
  {:metadoc/categories #{:filt}}
  ([col]
   (let [^Vec4 col (c/to-color col)]
     (fn [^long ch ^Pixels target ^Pixels p]
       (case ch
         0 (clojure2d.java.filter.Tint/process1 (.p p) (.p target) 0 (.x col))
         1 (clojure2d.java.filter.Tint/process1 (.p p) (.p target) 1 (.y col))
         2 (clojure2d.java.filter.Tint/process1 (.p p) (.p target) 2 (.z col))
         3 (clojure2d.java.filter.Tint/process1 (.p p) (.p target) 3 (.w col))))))
  ([col-low col-high]
   (let [^Vec4 col-low (c/to-color col-low)
         ^Vec4 col-high (c/to-color col-high)]
     (fn [^long ch ^Pixels target ^Pixels p]
       (case ch
         0 (clojure2d.java.filter.Tint/process2 (.p p) (.p target) 0 (.x col-low) (.x col-high))
         1 (clojure2d.java.filter.Tint/process2 (.p p) (.p target) 1 (.y col-low) (.y col-high))
         2 (clojure2d.java.filter.Tint/process2 (.p p) (.p target) 2 (.z col-low) (.z col-high))
         3 (clojure2d.java.filter.Tint/process2 (.p p) (.p target) 3 (.w col-low) (.w col-high))))))
  ([col-low col-mid col-high]
   (let [^Vec4 col-low (c/to-color col-low)
         ^Vec4 col-high (c/to-color col-high)
         ^Vec4 col-mid (c/to-color col-mid)]
     (fn [^long ch ^Pixels target ^Pixels p]
       (case ch
         0 (clojure2d.java.filter.Tint/process3 (.p p) (.p target) 0 (.x col-low) (.x col-mid) (.x col-high))
         1 (clojure2d.java.filter.Tint/process3 (.p p) (.p target) 1 (.y col-low) (.y col-mid) (.y col-high))
         2 (clojure2d.java.filter.Tint/process3 (.p p) (.p target) 2 (.z col-low) (.z col-mid) (.z col-high))
         3 (clojure2d.java.filter.Tint/process3 (.p p) (.p target) 3 (.w col-low) (.w col-mid) (.w col-high)))))))

(defn modulate
  "Create modulate channel values filter.

  Great to work with hue based color spaces.

  Values from [0-2]."
  {:metadoc/categories #{:filt}}
  [^double amt]
  (fn [^long ch ^Pixels target ^Pixels p]
    (clojure2d.java.filter.Modulate/process (.p p) (.p target) ch amt)))

(defn brightness-contrast
  "Create brightness / contrast filter.

  Values from [0-2]."
  {:metadoc/categories #{:filt}}
  ([^double brightness ^double contrast]
   (fn [^long ch ^Pixels target ^Pixels p]
     (clojure2d.java.filter.ContrastBrightness/process (.p p) (.p target) ch brightness contrast)))
  ([^double brightness]
   (fn [^long ch ^Pixels target ^Pixels p]
     (clojure2d.java.filter.ContrastBrightness/process (.p p) (.p target) ch brightness 1.0))))

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

(defrecord LDRenderer [^clojure2d.java.LogDensity buff ^long w ^long h]
  PixelsProto
  (set-color [r x y c]
    (.addPixel buff x y (c/to-color c))
    r)
  (get-color [r x y]
    (let [x (unchecked-int x)
          y (unchecked-int y)
          a (fastmath.java.Array/get2d (.a buff) w x y)]
      (Vec4. (/ (fastmath.java.Array/get2d (.r buff) w x y) a)
             (/ (fastmath.java.Array/get2d (.g buff) w x y) a)
             (/ (fastmath.java.Array/get2d (.b buff) w x y) a)
             255.0)))
  (to-pixels [r] (to-pixels r {}))
  (to-pixels [r {:keys [background ^double gamma-alpha ^double gamma-color ^double intensity
                        ^double saturation ^double brightness ^double contrast]
                 :or {background :black
                      gamma-alpha 1.0
                      gamma-color 1.0
                      intensity 1.0
                      saturation 1.0
                      brightness 1.0
                      contrast 1.0}}]
    (let [res (pixels (.toPixels buff ^Vec4 (c/to-color background)
                                 gamma-alpha gamma-color intensity) w h)
          res (if-not (bool-and (== 1.0 contrast)
                                (== 1.0 brightness))
                (filter-channels (brightness-contrast brightness contrast) res)
                res)
          res (if-not (== 1.0 saturation)
                (->> res
                     (filter-colors c/to-HSL*)
                     (filter-channels nil (modulate saturation) nil nil)
                     (filter-colors c/from-HSL*))
                res)]
      res))
  core/ImageProto
  (get-image [b] (core/get-image (to-pixels b)))
  (width [_] w)
  (height [_] h)
  (save [b n] (core/save (to-pixels b) n))
  (convolve [b t] (core/convolve (to-pixels b) t)))

(defn- create-filter
  ""
  [filter ^double filter-radius filter-params]
  (condp clojure.core/= filter
    :gaussian (clojure2d.java.reconstruction.Gaussian. filter-radius (or (first filter-params) 2.0))
    :box (clojure2d.java.reconstruction.Box. filter-radius)
    :sinc (clojure2d.java.reconstruction.Sinc. filter-radius (or (first filter-params) 1.0))
    :mitchell (clojure2d.java.reconstruction.Mitchell. filter-radius (or (first filter-params) m/THIRD) (or (second filter-params) m/THIRD))
    :cubic (clojure2d.java.reconstruction.Mitchell. filter-radius 1.0 0.0)
    :catmull-rom (clojure2d.java.reconstruction.Mitchell. filter-radius 0.0 0.5)
    :triangle (clojure2d.java.reconstruction.Triangle. filter-radius)
    :cosinebell (clojure2d.java.reconstruction.CosineBell. filter-radius (or (first filter-params) 0.5))
    :blackmann-harris (clojure2d.java.reconstruction.BlackmanHarris. filter-radius)
    nil))

(defn renderer
  "Create"
  {:metadoc/categories #{:ld}}
  ([w h filter filter-radius & filter-params]
   (LDRenderer. (clojure2d.java.LogDensity. w h (create-filter filter filter-radius filter-params))
                w h))
  ([w h filter] (renderer w h filter 2.0 nil))
  ([w h] (renderer w h :none 2.0 nil)))

(defn merge-renderers
  "Paralelly merge two binpixels. Be sure a and b are equal. Use this function to merge results created in separated threads"
  {:metadoc/categories #{:ld}}
  ^LDRenderer [^LDRenderer a ^LDRenderer b]
  (let [ch0 (future (.merge ^clojure2d.java.LogDensity (.buff a) (.buff b) 0))
        ch1 (future (.merge ^clojure2d.java.LogDensity (.buff a) (.buff b) 1))
        ch2 (future (.merge ^clojure2d.java.LogDensity (.buff a) (.buff b) 2))
        ch3 (.merge ^clojure2d.java.LogDensity (.buff a) (.buff b) 3)]
    (run! deref [ch0 ch1 ch2])
    a))



