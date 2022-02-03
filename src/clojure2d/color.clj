(ns clojure2d.color
  "Color functions.

  This namespace contains color manipulation functions which can be divided into following groups:

  * Color creators
  * Channel manipulations
  * Conversions
  * Palettes / gradients
  * Distances

  ## Representation

  Color can be represented by following types:
  
  * fastmath `Vec4` - this is core type representing 3 color channels and alpha (RGBA). Values are `double` type from `[0-255]` range. [[color]], [[gray]] creators returns `Vec4` representation.
  * fastmath `Vec3` - 3 channels (RGB), assuming `alpha` set to value of `255`.
  * fastmath `Vec2` - gray with alpha
  * `java.awt.Color` - Java AWT representation. Creators are [[awt-color]], [[awt-gray]]. Use [[to-awt-color]] to convert to this representation.
  * `keyword` - one of the defined names (see [[named-colors-list]])
  * `Integer` - packed ARGB value. If value is less than `0xff000000`, alpha is set to `0xff`. Example: `0xffaa01`.
  * `String` - CSS (`#rgb` or `#rgba`) or string containg hexadecimal representation (\"ffaa01\")
  * any `seqable` - list, vector containing 2-4 elements. Conversion is done by applying content to [[color]] function.

  To create color from individual channel values use [[color]] function. To create gray for given intensity call [[gray]].

  By default color is treated as `RGB` with values from ranges `[0.0-255.0]` inclusive.

  All funtions internally convert any color representation to `Vec4` type using [[to-color]] function..
  
  [Coloured list of all names](../static/colors.html)
  
  ## Color/ channel manipulations

  You can access individual channels by calling on of the following:

  * [[red]] or [[ch0]] - to get first channel value.
  * [[green]] or [[ch1]] - to get second channel value.
  * [[blue]] or [[ch2]] - to get third channel value.
  * [[alpha]] - to get alpha value.
  * [[luma]] - to get luma or brightness (range from `0` (black) to `255` (white)).
  * [[hue]] - to get hue value in degrees (range from 0 to 360). Hexagon projection.
  * [[hue-polar]] - to get hue from polar transformation.

  [[set-ch0]], [[set-ch1]], [[set-ch2]] and [[set-alpha]] return new color with respective channel set to new value.

  General [[set-channel]] and [[get-channel]] can work with any colorspace.

  To make color darker/brighter use [[darken]] / [[lighten]] functions. Operations are done in `Lab` color space.

  To change saturation call [[saturate]] / [[desaturate]]. Operations are done in `LCH` color space.

  You can also [[modulate]] color (ie. multiply by given value).
  
  ## Conversions

  Color can be converted from RGB to other color space (and back). List of color spaces are listed under [[colorspaces-list]] variable. There are two types of conversions:

  * raw - with names `to-XXX` and `from-XXX` where `XXX` is color space name. Every color space has it's own value range for each channel. `(comp from-XXX to-XXX)` acts almost as identity.
  * normalized - with names `to-XXX*` and `from-XXX*` where `XXX` is color space name. `to-XXX*` returns values normalized to `[0-255]` range. `from-XXX*` expects also channel values in range `[0-255]`.

  NOTE: there is no information which color space is used. It's just a matter of your interpretation.

  NOTE 2: be aware that converting function do not clamp any values to/from expected range.

  Color space conversion functions are collected in two maps [[colorspaces]] for raw and [[colorspaces*]] for normalized functions. Keys are color space names as `keyword` and values are vectors with `to-` fn as first and `from-` fn as second element.
    
  ## Palettes / gradients

  ### Links

  List of all defined colors and palettes:
  
  * [Named palettes](../static/palettes/index.html)
  * [Gradients](../static/gradients/index.html)
  
  ### Palette

  Palette is just sequence of colors.

  There are plenty of them predefined or can be generated:

  * [[palette]] to access palette by keyword (from presets), by number (from colourlovers). Use [[palette]] to resample palette or convert gradient to palette.
  * [[paletton-palette]] function to generate palette of type: `:monochromatic`, `:triad`, `:tetrad` with complementary color for given hue and configuration. See also [Paletton](http://paletton.com) website for details.

  Call [[palette]] without any parameters to get list of predefined gradients.
  
  ### Gradient

  Gradient is continuous functions which accepts value from `[0-1]` range and returns color. Call [[gradient]] to create one.

  Call [[gradient]] without any parameters to obtain list of predefined gradients.
  
  Use [[gradient]] to convert any palette to gradient or access predefined gradients by keyword.
  
  ### Conversions

  To convert palette to gradient call [[gradient]] function. You can set interpolation method and colorspace.
  To convert gradient to palette call [[palette]] function.

  Call [[palette]] to resample palette to other number of colors. Internally input palette is converted to gradient and sampled back.

  Use [[lerp]], [[lerp+]], [[mix]], [[average]] to mix two colors in different ways.
  
  ## Distances

  Several functions to calculate distance between colors (`euclidean`, `delta-xxx` etc.).

  ## References

  * https://vis4.net/chromajs/
  * https://github.com/thi-ng/color
  * https://github.com/nschloe/colorio"
  {:metadoc/categories {:ops "Color/channel operations"
                        :conv "Color conversions"
                        :gr "Gradients"
                        :pal "Colors, palettes"
                        :interp "Interpolation"
                        :dist "Distance"}}
  (:require [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [fastmath.stats :as stat]
            [fastmath.interpolation :as i]
            [fastmath.clustering :as cl]
            [fastmath.easings :as e]
            [clojure2d.protocols :as pr]
            [clojure.java.io :refer [input-stream resource]])
  (:import [fastmath.vector Vec2 Vec3 Vec4]           
           [java.awt Color]
           [clojure.lang APersistentVector ISeq]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defonce ^:private meta-conv #{:conv})
(defonce ^:private meta-ops #{:ops})

;; load the stuff to delay

;;;;;;;;;
;; read
;;;;;;;;;

(defn- read-edn
  [n]
  (-> (resource n)
      (input-stream)
      (slurp)
      (read-string)))

;; color names

(defonce ^:private color-presets-delay
  (delay (into {} (map (fn [[k v]]
                         [k (pr/to-color v)]) (read-edn "color_presets.edn")))))

(defn named-colors-list
  "Return list of the named colors."
  {:metadoc/categories #{:pal}}
  [] (keys @color-presets-delay))

;; palettes / gradients

(defn- load-edn-file-
  [prefix n]
  (read-edn (str prefix "c2d_" n ".edn")))

(defonce ^:private load-edn-file (memoize load-edn-file-))

(defn- get-palette-or-gradient
  [prefix k]
  (let [nm (cond
             (keyword? k) (namespace k)
             (integer? k) "colourlovers"
             :else k)
        p (load-edn-file prefix nm)]
    (p k)))

;; ## Clamping functions

;; First define some clamping functions

(defn- clamp255
  "Constrain value `a` to 0-255 double.

  Use to ensure that value is in RGB range.
  Accepts and returns `double`.

  See also [[lclamp255]], [[clamp]] and [[lclamp]]."
  {:metadoc/categories meta-ops}
  ^double [^double a]
  (m/constrain a 0 255))

(defn- lclamp255
  "Constrain value `a` to 0-255 long (rounding if necessary).

  Use to ensure that value is in RGB range.

  See also [[clamp255]], [[clamp]] and [[lclamp]]."
  {:metadoc/categories meta-ops}
  ^long [^double a]
  (m/constrain (m/round a) 0 255))


(defn possible-color?
  "Check if given argument can be considered as color.

  Check is done by analyzing type of the argument.
  
  See also [[valid-color?]]."
  {:metadoc/categories #{:pal}}
  [c]
  (or (string? c)
      (and (not (seqable? c))
           (satisfies? pr/ColorProto c))
      (and (seqable? c)
           (let [v (first c)]
             (and (number? v)
                  (< (long v) 0x01000000))))))

(defn possible-palette?
  "Check if given argument can be considered as palette.

  Check is done by analyzing type of the argument."
  {:metadoc/categories #{:pal}}
  [c]
  (and (seqable? c)
       (not (possible-color? c))
       (not (fn? c))))

(defn valid-color?
  "Check if given argument is valid color.

  Check is done by trying to convert to color representation.
  
  Returns color when valid.

  See also [[possible-color?]]"
  {:metadoc/categories #{:pal}}
  [c]
  (try
    (pr/to-color c)
    (catch Exception _ false)))

;; ## Color representation

(defn to-color
  "Convert any color representation to `Vec4` vector."
  {:metadoc/categories meta-ops}
  ^Vec4 [c] (pr/to-color c))

(defn to-awt-color
  "Convert any color representation to `java.awt.Color`."
  {:metadoc/categories meta-ops}
  ^java.awt.Color [c] (pr/to-awt-color c))

(defn luma
  "Returns luma"
  {:metadoc/categories meta-ops}
  ^double [c] (pr/luma c))

(defn- luma-fn
  "Local luma conversion function"
  ^double [^double r ^double g ^double b]
  (+ (* 0.212671 r)
     (* 0.715160 g)
     (* 0.072169 b)))

(declare from-sRGB)

(defn relative-luma
  "Returns relative luminance"
  {:metadoc/categories meta-ops}
  ^double [c]
  (let [^Vec4 c (from-sRGB c)]
    (luma-fn (.x c) (.y c) (.z c))))

(defn red
  "Returns red (first channel) value."
  {:metadoc/categories meta-ops}
  ^double [c] (pr/red c))

(defn green
  "Returns green (second channel) value."
  {:metadoc/categories meta-ops}
  ^double [c] (pr/green c))

(defn blue
  "Returns blue (third channel) value."
  {:metadoc/categories meta-ops}
  ^double [c] (pr/blue c))

(defn alpha
  "Returns alpha value."
  {:metadoc/categories meta-ops}
  ^double [c] (pr/alpha c))

(defn ch0
  "Returns first channel value. Same as [[red]]."
  {:metadoc/categories meta-ops}
  ^double [c] (pr/red c))

(defn ch1
  "Returns second channel value. Same as [[green]]."
  {:metadoc/categories meta-ops}
  ^double [c] (pr/green c))

(defn ch2
  "Returns third channel value. Same as [[blue]]."
  {:metadoc/categories meta-ops}
  ^double [c] (pr/blue c))

(defn hue-polar
  "Hue value of color (any representation). Returns angle (0-360).
  
  Uses polar transformation. See also [[hue]]."
  {:metadoc/categories meta-ops}
  ^double [c]
  (let [^Vec4 c (pr/to-color c)
        a (* 0.5 (- (+ (.x c) (.x c)) (.y c) (.z c)))
        b (* 0.8660254037844386 (- (.y c) (.z c)))
        h (m/degrees (m/atan2 b a))]
    (if (neg? h) (+ 360.0 h) h)))

(declare to-HC)

(defn hue
  "Hue value of color (any representation). Returns angle (0-360).
  
  Uses hexagonal transformation. See also [[hue-polar]]."
  {:metadoc/categories meta-ops}
  ^double [c]
  (let [^Vec4 ret (to-HC (pr/to-color c))] (.x ret)))

(defn set-alpha
  "Set alpha channel and return new color"
  {:metadoc/categories meta-ops}
  ^Vec4 [c a]
  (let [^Vec4 v (pr/to-color c)]
    (Vec4. (.x v) (.y v) (.z v) a)))

(defn set-red
  "Set red channel and return new color."
  {:metadoc/categories meta-ops}
  ^Vec4 [c val]
  (let [^Vec4 v (pr/to-color c)]
    (Vec4. val (.y v) (.z v) (.w v))))

(defn set-green
  "Set green channel and return new color."
  {:metadoc/categories meta-ops}
  ^Vec4 [c val]
  (let [^Vec4 v (pr/to-color c)]
    (Vec4. (.x v) val (.z v) (.w v))))

(defn set-blue
  "Set blue channel and return new color"
  {:metadoc/categories meta-ops}
  ^Vec4 [c val]
  (let [^Vec4 v (pr/to-color c)]
    (Vec4. (.x v) (.y v) val (.w v))))

(defn set-ch0
  "Set red channel and return new color."
  {:metadoc/categories meta-ops}
  ^Vec4 [c val]
  (let [^Vec4 v (pr/to-color c)]
    (Vec4. val (.y v) (.z v) (.w v))))

(defn set-ch1
  "Set green channel and return new color."
  {:metadoc/categories meta-ops}
  ^Vec4 [c val]
  (let [^Vec4 v (pr/to-color c)]
    (Vec4. (.x v) val (.z v) (.w v))))

(defn set-ch2
  "Set blue channel and return new color"
  {:metadoc/categories meta-ops}
  ^Vec4 [c val]
  (let [^Vec4 v (pr/to-color c)]
    (Vec4. (.x v) (.y v) val (.w v))))

(defn set-awt-alpha
  "Set alpha channel and return `Color` representation."
  {:metadoc/categories meta-ops}
  ^Color [c a]
  (let [^Color cc (pr/to-awt-color c)]
    (Color. (.getRed cc)
            (.getGreen cc)
            (.getBlue cc)
            (lclamp255 a))))

(defn awt-color
  "Create java.awt.Color object.

  See also [[color]], [[gray]]."
  {:metadoc/categories meta-ops}
  (^Color [c]
   (pr/to-awt-color c))
  (^Color [c a]
   (set-awt-alpha c a))
  (^Color [^double r ^double g ^double b]
   (Color. (lclamp255 r)
           (lclamp255 g)
           (lclamp255 b)))
  (^Color [^double r ^double g ^double b ^double a]
   (Color. (lclamp255 r)
           (lclamp255 g)
           (lclamp255 b)
           (lclamp255 a))))

(defn color
  "Create Vec4 object as color representation.

  Arity: 

  * 1 - convert to `Vec4` from any color. Same as [[to-color]]
  * 2 - sets color alpha
  * 3 - sets r,g,b with alpha 255
  * 4 - sets r,g,b and alpha
  
  See also [[gray]]. [[awt-color]], [[awt-gray]]."
  {:metadoc/categories meta-ops}
  (^Vec4 [c]
   (pr/to-color c))
  (^Vec4 [c a]
   (set-alpha c a))
  (^Vec4 [^double r ^double g ^double b]
   (Vec4. (clamp255 r)
          (clamp255 g)
          (clamp255 b)
          255.0))
  (^Vec4 [^double r ^double g ^double b ^double a]
   (Vec4. (clamp255 r)
          (clamp255 g)
          (clamp255 b)
          (clamp255 a))))

(defn clamp
  "Clamp all color channels to `[0-255]` range."
  {:metadoc/categories meta-ops}
  [c]
  (v/fmap (pr/to-color c) clamp255))

(defn lclamp
  "Clamp all color channels to `[0-255]` range. Round if necessary."
  {:metadoc/categories meta-ops}
  [c]
  (v/fmap (pr/to-color c) lclamp255))

(defn gray
  "Create grayscale color based on intensity `v`. Optional parameter alpha `a`.

  See also [[color]]"
  {:metadoc/categories meta-ops}
  (^Vec4 [^double v] (color v v v))
  (^Vec4 [^double v ^double a] (color v v v a)))

(defn awt-gray
  "Create grayscale color based on intensity `v`. Optional parameter alpha `a`.

  AWT version of [[gray]]. See also [[awt-color]]"
  {:metadoc/categories meta-ops}
  (^Color [^double v] (awt-color v v v))
  (^Color [^double v ^double a] (awt-color v v v a)))

(defn- string->long
  "Parse color string and convert to long"
  [^String s]
  (let [^String s (if (= (first s) \#) (subs s 1) s)
        cs (count s)]
    (if (and (= cs 8)
             (= (subs s 6) "00"))
      (set-alpha (pr/to-color (Long/parseLong (subs s 0 6) 16)) 0)
      (Long/parseLong (condp = cs
                        1 (str s s s s s s)
                        2 (str s s s)
                        3 (let [[r g b] s] (str r r g g b b))
                        8 (str (subs s 6) (subs s 0 6))
                        s) 16))))

(extend-protocol pr/ColorProto
  Vec2
  (to-color ^Vec4 [^Vec2 c]
    (Vec4. (.x c) (.x c) (.x c) (.y c)))
  (to-awt-color ^Color [^Vec2 c]
    (let [v (lclamp255 (.x c))]      
      (Color. v v v (lclamp255 (.y c)))))
  (luma ^double [^Vec2 c] (.x c))
  (red ^double [^Vec2 c] (.x c))
  (green ^double [^Vec2 c] (.x c))
  (blue ^double [^Vec2 c] (.x c))
  (alpha ^double [^Vec2 c] (.y c))
  Vec3
  (to-color ^Vec4 [^Vec3 c]
    (Vec4. (.x c) (.y c) (.z c) 255))
  (to-awt-color ^Color [^Vec3 c]
    (Color. (lclamp255 (.x c))
            (lclamp255 (.y c))
            (lclamp255 (.z c))))
  (luma ^double [^Vec3 c] (luma-fn (.x c) (.y c) (.z c)))
  (red ^double [^Vec3 c] (.x c))
  (green ^double [^Vec3 c] (.y c))
  (blue ^double [^Vec3 c] (.z c))
  (alpha ^double [_] 255.0)
  Vec4
  (to-color ^Vec4 [c] c)
  (to-awt-color ^Color [^Vec4 c]
    (Color.  (lclamp255 (.x c))
             (lclamp255 (.y c))
             (lclamp255 (.z c))
             (lclamp255 (.w c))))
  (luma ^double [^Vec4 c] (luma-fn (.x c) (.y c) (.z c)))
  (red ^double [^Vec4 c] (.x c))
  (green ^double [^Vec4 c] (.y c))
  (blue ^double [^Vec4 c] (.z c))
  (alpha ^double [^Vec4 c] (.w c))
  clojure.lang.Keyword
  (to-color ^Vec4 [n] (@color-presets-delay n))
  (to-awt-color ^Color [n] (to-awt-color (@color-presets-delay n)))
  (luma ^double [n] (pr/luma (@color-presets-delay n)))
  (red ^double [n] (pr/red (@color-presets-delay n)))
  (green ^double [n] (pr/green (@color-presets-delay n)))
  (blue ^double [n] (pr/blue (@color-presets-delay n)))
  (alpha ^double [n] (pr/alpha (@color-presets-delay n)))
  Color
  (to-color ^Vec4 [^Color c]
    (Vec4. (.getRed c)
           (.getGreen c)
           (.getBlue c)
           (.getAlpha c)))
  (to-awt-color ^Color [c] c)
  (luma ^double [^Color c] (luma-fn (.getRed c) (.getGreen c) (.getBlue c)))
  (red ^double [^Color c] (.getRed c))
  (green ^double [^Color c] (.getGreen c))
  (blue ^double [^Color c] (.getBlue c))
  (alpha ^double [^Color c] (.getAlpha c))
  nil
  (to-color [_] nil)
  (to-awt-color [_] nil)
  (alpha [_] nil)
  (red [_] nil)
  (green [_] nil)
  (blue [_] nil)
  Long
  (alpha ^double [^long c] (bit-and 0xff (>> c 24)))
  (red ^double [^long c] (bit-and 0xff (>> c 16)))
  (green ^double [^long c] (bit-and 0xff (>> c 8)))
  (blue ^double [^long c] (bit-and 0xff c))
  (to-color ^Vec4 [^long c] (Vec4. (bit-and 0xff (>> c 16))
                                   (bit-and 0xff (>> c 8))
                                   (bit-and 0xff c)
                                   (if (zero? (bit-and 0xff000000 c)) 255.0 (bit-and 0xff (>> c 24)))))
  (to-awt-color ^Color [c] (pr/to-awt-color (pr/to-color c)))
  (luma ^double [c] (pr/luma (pr/to-color c)))
  Integer
  (alpha ^double [c] (bit-and 0xff (>> ^int c 24)))
  (red ^double [c] (bit-and 0xff (>> ^int c 16)))
  (green ^double [c] (bit-and 0xff (>> ^int c 8)))
  (blue ^double [c] (bit-and 0xff ^int c))
  (to-color ^Vec4 [c] (Vec4. (bit-and 0xff (>> ^int c 16))
                             (bit-and 0xff (>> ^int c 8))
                             (bit-and 0xff ^int c)
                             (if (zero? (bit-and 0xff000000 c)) 255.0 (bit-and 0xff (>> ^int c 24)))))
  (to-awt-color ^Color [c] (pr/to-awt-color (pr/to-color c)))
  (luma ^double [c] (pr/luma (pr/to-color c)))
  String
  (alpha ^double [^String c] (pr/alpha (string->long c)))
  (red ^double [^String c] (pr/red (string->long c)))
  (green ^double [^String c] (pr/green (string->long c)))
  (blue ^double [^String c] (pr/blue (string->long c)))
  (to-color ^Vec4 [^String c] (pr/to-color (string->long c)))
  (to-awt-color ^Color [^String c] (pr/to-awt-color (pr/to-color (string->long c))))
  (luma ^double [^String c] (pr/luma (pr/to-color (string->long c))))
  APersistentVector
  (to-color ^Vec4 [c] (case (count c)
                        0 (Vec4. 0.0 0.0 0.0 255.0)
                        1 (gray (c 0))
                        2 (gray (c 0) (c 1))
                        3 (Vec4. (c 0) (c 1) (c 2) 255.0)
                        (Vec4. (c 0) (c 1) (c 2) (c 3))))
  (alpha ^double [c] (get c 3 255.0))
  (red ^double [c] (c 0))
  (green ^double [c] (c 1))
  (blue ^double [c] (c 2))
  (to-awt-color ^Color [c] (pr/to-awt-color (pr/to-color c)))
  (luma ^double [c] (pr/luma (pr/to-color c)))
  ISeq
  (to-color ^Vec4 [c] (case (count c)
                        0 (Vec4. 0.0 0.0 0.0 255.0)
                        1 (gray (first c))
                        2 (gray (first c) (second c))
                        3 (Vec4. (first c) (second c) (nth c 2) 255.0)
                        (Vec4. (first c) (second c) (nth c 2) (nth c 3 255.0))))
  (alpha ^double [c] (nth c 3 255.0))
  (red ^double [c] (first c))
  (green ^double [c] (second c))
  (blue ^double [c] (nth c 2))
  (to-awt-color ^Color [c] (pr/to-awt-color (pr/to-color c)))
  (luma [c] ^double (pr/luma (pr/to-color c))))

;;

(defn format-hex
  "Convert color to hex string (css).

  When alpha is lower than 255.0, #rgba is returned."
  {:metadoc/categories meta-ops}
  ^String [c]
  (let [^Vec4 c (pr/to-color c)
        s (str "#" (format "%02x" (lclamp255 (.x c)))
               (format "%02x" (lclamp255 (.y c)))
               (format "%02x" (lclamp255 (.z c))))
        a (lclamp255 (.w c))]
    (if (< a 255) (str s (format "%02x" a)) s)))

(defn pack
  "Pack color to ARGB 32bit integer."
  {:metadoc/categories meta-ops}
  [c]
  (unchecked-int (bit-or (<< (lclamp255 (pr/alpha c)) 24)
                         (<< (lclamp255 (pr/red c)) 16)
                         (<< (lclamp255 (pr/green c)) 8)
                         (lclamp255 (pr/blue c)))))

(defn black?
  "Check if color is black"
  [c]
  (let [^Vec4 v (pr/to-color c)]
    (and (zero? (.x v))
         (zero? (.y v))
         (zero? (.z v)))))

(defn not-black?
  "Check if color is not black"
  [c]
  (let [^Vec4 v (pr/to-color c)]
    (or (pos? (.x v))
        (pos? (.y v))
        (pos? (.z v)))))


;; ## Colorspace functions
;;
;; Conversion from RGB to specific color space always converts to range 0-255
;; Reverse conversion is not normalized and can exceed 0-255 range

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
          ^Vec4 res (f (Vec4. r g b 255.0))
          nmnr (if (< (.x res) mnr) (.x res) mnr)
          nmxr (if (> (.x res) mxr) (.x res) mxr)
          nmng (if (< (.y res) mng) (.y res) mng)
          nmxg (if (> (.y res) mxg) (.y res) mxg)
          nmnb (if (< (.z res) mnb) (.z res) mnb)
          nmxb (if (> (.z res) mxb) (.z res) mxb)]
      (if (< cc 0x1000000)
        (recur (inc cc) (double nmnr) (double nmxr) (double nmng) (double nmxg) (double nmnb) (double nmxb))
        {:min-r nmnr :max-r nmxr :min-g nmng :max-g nmxg :min-b nmnb :max-b nmxb}))))

;; ### CMY

(defn to-CMY
  "RGB -> CMY"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (Vec4. (- 255.0 (.x c))
           (- 255.0 (.y c))
           (- 255.0 (.z c))
           (.w c))))

(def ^{:doc "CMY -> RGB" :metadoc/categories meta-conv} from-CMY to-CMY)
(def ^{:doc "CMY -> RGB, alias for [[to-CMY]]" :metadoc/categories meta-conv} to-CMY* to-CMY)
(def ^{:doc "CMY -> RGB, alias for [[from-CMY]]" :metadoc/categories meta-conv} from-CMY* to-CMY)

;; ### OHTA

(defn to-OHTA
  "RGB -> OHTA

  Returned ranges:

  * I1: 0.0 - 255.0
  * I2: -127.5 - 127.5
  * I3: -127.5 - 127.5"
  {:metadoc/categories meta-conv}
  ^Vec4 [c] 
  (let [^Vec4 c (pr/to-color c)
        i1 (/ (+ (.x c) (.y c) (.z c)) 3.0)
        i2 (* 0.5 (- (.x c) (.z c)))
        i3 (* 0.25 (- (* 2.0 (.y c)) (.x c) (.z c)))]
    (Vec4. i1 i2 i3 (.w c))))

(def ^:private ^:const ohta-s (Vec4. 0.0 127.5 127.5 0.0))

(defn to-OHTA*
  "RGB -> OHTA, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (v/add (to-OHTA c) ohta-s))

(def ^:private ^:const ^double c23- (/ -2.0 3.0))
(def ^:private ^:const ^double c43 (/ 4.0 3.0))

(defn from-OHTA
  "OHTA -> RGB

  For ranges, see [[to-OHTA]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        i1 (.x c)
        i2 (.y c)
        i3 (.z c)
        r (+ i1 i2 (* c23- i3))
        g (+ i1 (* c43 i3))
        b (+ i1 (- i2) (* c23- i3))]
    (Vec4. r g b (.w c))))

(defn from-OHTA*
  "OHTA -> RGB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (from-OHTA (v/sub c ohta-s)))

;; ### sRGB

(def ^:private ^:const ^double gamma-factor (/ 2.4))

(defn to-linear
  "Gamma correction (gamma=2.4), darken"
  {:metadoc/categories meta-conv}
  ^double [^double v]
  (if (> v 0.04045)
    (m/pow (/ (+ 0.055 v) 1.055) 2.4)
    (/ v 12.92)))

(defn from-linear
  "Gamma correction (gamma=1/2.4), lighten"
  {:metadoc/categories meta-conv}
  ^double [^double v]
  (if (> v 0.0031308)
    (- (* 1.055 (m/pow v gamma-factor)) 0.055)
    (* v 12.92)))

(defn to-sRGB
  "RGB -> sRGB"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (v/vec4 (-> (Vec3. (.x c) (.y c) (.z c))
                (v/div 255.0)
                (v/fmap from-linear)
                (v/mult 255.0))
            (.w c))))

(defn from-sRGB
  "sRGB -> RGB"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (v/vec4 (-> (Vec3. (.x c) (.y c) (.z c))
                (v/div 255.0)
                (v/fmap to-linear)
                (v/mult 255.0))
            (.w c))))

(def ^{:doc "linear RGB -> sRGB" :metadoc/categories meta-conv} to-sRGB* to-sRGB)
(def ^{:doc "sRGB -> linear RGB" :metadoc/categories meta-conv} from-sRGB* from-sRGB)

;; ### Oklab

(defn to-Oklab
  "RGB -> Oklab

  https://bottosson.github.io/posts/oklab/

  * L: 0.0 - 1.0
  * a: -0.234 - 0.276
  * b: -0.312 - 0.199"
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        l (m/cbrt (/ (+ (* 0.4122214708 (.x c)) (* 0.5363325363 (.y c)) (* 0.0514459929 (.z c))) 255.0))
        m (m/cbrt (/ (+ (* 0.2119034982 (.x c)) (* 0.6806995451 (.y c)) (* 0.1073969566 (.z c))) 255.0))
        s (m/cbrt (/ (+ (* 0.0883024619 (.x c)) (* 0.2817188376 (.y c)) (* 0.6299787005 (.z c))) 255.0))]
    (Vec4. (+ (* 0.2104542553 l) (* 0.7936177850 m) (* -0.0040720468 s))
           (+ (* 1.9779984951 l) (* -2.4285922050 m) (* 0.4505937099 s))
           (+ (* 0.0259040371 l) (* 0.7827717662 m) (* -0.8086757660 s))
           (.w c))))

(defn to-Oklab*
  "RGB -> Oklab, normalized"
  ^Vec4 [c]
  (let [^Vec4 c (to-Oklab c)]
    (Vec4. (m/mnorm (.x c) 0.0 0.9999999934735462 0.0 255.0)
           (m/mnorm (.y c) -0.23388757418790818 0.27621675349252356 0.0 255.0)
           (m/mnorm (.z c) -0.3115281476783752  0.19856975465179516 0.0 255.0)
           (.w c))))

(defn from-Oklab
  "Oklab -> RGB, see [[to-Oklab]]"
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        l (m/cb (+ (.x c) (* 0.3963377774 (.y c)) (* 0.2158037573 (.z c))))
        m (m/cb (+ (.x c) (* -0.1055613458 (.y c)) (* -0.0638541728 (.z c))))
        s (m/cb (+ (.x c) (* -0.0894841775 (.y c)) (* -1.2914855480 (.z c))))]
    (Vec4. (* 255.0 (+ (* 4.0767416621  l) (* -3.3077115913 m) (* 0.2309699292 s)))
           (* 255.0 (+ (* -1.2684380046 l) (* 2.6097574011 m) (* -0.3413193965 s)))
           (* 255.0 (+ (* -0.0041960863 l) (* -0.7034186147 m) (* 1.7076147010 s)))
           (.w c))))

(defn from-Oklab*
  "RGB -> Oklab, normalized"
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-Oklab (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 0.9999999934735462)
                       (m/mnorm (.y c) 0.0 255.0 -0.23388757418790818 0.27621675349252356)
                       (m/mnorm (.z c) 0.0 255.0 -0.3115281476783752  0.19856975465179516)
                       (.w c)))))

;; ### XYZ

(def ^:private ^:const ^double D65X 95.047)
(def ^:private ^:const ^double D65Y 100.0)
(def ^:private ^:const ^double D65Z 108.883)

(defn- to-XYZ-
  "Pure RGB->XYZ conversion without corrections."
  ^Vec3 [^Vec3 c]
  (Vec3. (+ (* (.x c) 0.4124) (* (.y c) 0.3576) (* (.z c) 0.1805))
         (+ (* (.x c) 0.2126) (* (.y c) 0.7152) (* (.z c) 0.0722))
         (+ (* (.x c) 0.0193) (* (.y c) 0.1192) (* (.z c) 0.9505))))

(defn to-XYZ
  "RGB -> XYZ

  Returned ranges (D65):

  * X: 0.0 - 95.047
  * Y: 0.0 - 100.0
  * Z: 0.0 - 108.883"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        xyz-raw (to-XYZ- (-> (Vec3. (.x c) (.y c) (.z c))
                             (v/div 255.0)
                             (v/fmap to-linear)
                             (v/mult 100.0)))]
    (v/vec4 xyz-raw (.w c))))

(defn to-XYZ*
  "RGB -> XYZ, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-XYZ c)]
    (Vec4. (m/mnorm (.x cc) 0.0 D65X 0.0 255.0)
           (m/mnorm (.y cc) 0.0 D65Y 0.0 255.0)
           (m/mnorm (.z cc) 0.0 D65Z 0.0 255.0)
           (.w cc))))

(defn- from-XYZ-
  "Pure XYZ->RGB conversion."
  ^Vec3 [^Vec3 v]
  (Vec3. (+ (* (.x v)  3.2406) (* (.y v) -1.5372) (* (.z v) -0.4986))
         (+ (* (.x v) -0.9689) (* (.y v)  1.8758) (* (.z v)  0.0415))
         (+ (* (.x v)  0.0557) (* (.y v) -0.2040) (* (.z v)  1.0570))))

(defn from-XYZ
  "XYZ -> RGB

  For ranges, see [[to-XYZ]]"
  {:metadoc/categories meta-conv}
  ^Vec4 [c] 
  (let [^Vec4 c (pr/to-color c)
        rgb-raw (v/mult (v/fmap (from-XYZ- (v/div (Vec3. (.x c) (.y c) (.z c)) 100.0)) from-linear) 255.0)]
    (v/vec4 rgb-raw (.w c))))

(defn from-XYZ*
  "XYZ -> RGB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        x (m/mnorm (.x c) 0.0 255.0 0.0 D65X)
        y (m/mnorm (.y c) 0.0 255.0 0.0 D65Y)
        z (m/mnorm (.z c) 0.0 255.0 0.0 D65Z)]
    (from-XYZ (Vec4. x y z (.w c)))))

;;

(def ^:private ^:const ^double CIEEpsilon (/ 216.0 24389.0))
(def ^:private ^:const ^double CIEK (/ 24389.0 27.0))
(def ^:private ^:const ^double REF-U (/ (* 4.0 D65X) (+ D65X (* 15.0 D65Y) (* 3.0 D65Z))))
(def ^:private ^:const ^double REF-V (/ (* 9.0 D65Y) (+ D65X (* 15.0 D65Y) (* 3.0 D65Z))))

;; ### XYB, https://observablehq.com/@mattdesl/perceptually-smooth-multi-color-linear-gradients

(defn- convert-mix
  ^double [^double m]
  (-> m (max 0.0) m/cbrt (+ -0.15595420054924863)))

(defn to-XYB
  "sRGB -> XYB

  * X: -0.015386116472573375 - 0.02810008316127735
  * Y: 0.0 - 0.8453085619621623
  * Z: 0.0 - 0.8453085619621623"
  ^Vec4 [c]
  (let [^Vec4 c (from-sRGB c)
        r (convert-mix (m/muladd 0.001176470588235294 (.x c)
                                 (m/muladd 0.00243921568627451 (.y c)
                                           (m/muladd 3.0588235294117644E-4 (.z c)
                                                     0.0037930732552754493))))
        g (convert-mix (m/muladd 9.019607843137256E-4 (.x c)
                                 (m/muladd 0.0027137254901960788 (.y c)
                                           (m/muladd 3.0588235294117644E-4 (.z c)
                                                     0.0037930732552754493))))
        b (convert-mix (m/muladd 9.545987813548164E-4 (.x c)
                                 (m/muladd 8.030095852743851E-4 (.y c)
                                           (m/muladd 0.002163960260821779 (.z c)
                                                     0.0037930732552754493))))]
    (Vec4. (* 0.5 (- r g)) (* 0.5 (+ r g)) b (.w c))))

(defn to-XYB*
  "sRGB -> XYB, normalized"
  ^Vec4 [c]
  (let [^Vec4 c (to-XYB c)]
    (Vec4. (m/mnorm (.x c) -0.015386116472573375 0.02810008316127735 0.0 255.0)
           (m/mnorm (.y c) 0.0 0.8453085619621623 0.0 255.0)
           (m/mnorm (.z c) 0.0 0.8453085619621623 0.0 255.0)
           (.w c))))

(defn from-XYB
  "XYB -> sRGB"
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        gamma-r (- (+ (.y c) (.x c)) -0.15595420054924863)
        gamma-g (- (.y c) (.x c) -0.15595420054924863)
        gamma-b (- (.z c) -0.15595420054924863)
        mixed-r (m/muladd (* gamma-r gamma-r) gamma-r -0.0037930732552754493)
        mixed-g (m/muladd (* gamma-g gamma-g) gamma-g -0.0037930732552754493)
        mixed-b (m/muladd (* gamma-b gamma-b) gamma-b -0.0037930732552754493)
        r (m/muladd -41.9788641 mixed-b (m/muladd -2516.0707 mixed-g (* 2813.04956 mixed-r)))
        g (m/muladd -41.9788641 mixed-b (m/muladd 1126.78645 mixed-g (* -829.807582 mixed-r)))
        b (m/muladd 496.211701 mixed-b (m/muladd 691.795377 mixed-g (* -933.007078 mixed-r)))]
    (to-sRGB (Vec4. r g b (.w c)))))

(defn from-XYB*
  "XYB -> sRGB, normalized"
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-XYB (Vec4. (m/mnorm (.x c) 0.0 255.0 -0.015386116472573375 0.02810008316127735)
                     (m/mnorm (.y c) 0.0 255.0 0.0 0.8453085619621623)
                     (m/mnorm (.z c) 0.0 255.0 0.0 0.8453085619621623)
                     (.w c)))))

;; ### RYB
;; https://web.archive.org/web/20120302090118/http://www.insanit.net/tag/rgb-to-ryb/

(defn to-RYB
  "RGB -> RYB"
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        w (min (.x c) (.y c) (.z c))
        r (- (.x c) w)
        g (- (.y c) w)
        b (- (.z c) w)
        mg (max r g b)
        y (min r g)
        r (- r y)
        g (- g y)
        nz? (and (not (zero? b))
                 (not (zero? g)))
        g (if nz? (/ g 2.0) g)
        b (if nz? (/ b 2.0) b)
        y (+ y g)
        b (+ b g)
        my (max r y b)
        n (if-not (zero? my) (/ mg my) 1.0)]
    (Vec4. (m/muladd r n w)
           (m/muladd y n w)
           (m/muladd b n w)
           (.w c))))

(def ^{:doc "RGB -> RYB, normalized" :metadoc/categories meta-conv} to-RYB* to-RYB)

(defn from-RYB
  "RYB -> RGB"
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        w (min (.x c) (.y c) (.z c))
        r (- (.x c) w)
        y (- (.y c) w)
        b (- (.z c) w)
        my (max r y b)
        g (min y b)
        y (- y g)
        b (- b g)
        nz? (and (not (zero? b))
                 (not (zero? g)))
        b (if nz? (* 2.0 b) b)
        g (if nz? (* 2.0 g) g)
        r (+ r y)
        g (+ g y)
        mg (max r g b)
        n (if-not (zero? mg) (/ my mg) 1.0)]
    (Vec4. (m/muladd r n w)
           (m/muladd g n w)
           (m/muladd b n w)
           (.w c))))

(def ^{:doc "RYB -> RGB, normalized" :metadoc/categories meta-conv} from-RYB* from-RYB)

;; ### LAB

(defn- to-lab-correct
  "LAB correction"
  ^double [^double v]
  (if (> v CIEEpsilon)
    (m/cbrt v)
    (/ (+ 16.0 (* v CIEK)) 116.0)))

(defn to-LAB
  "RGB -> LAB

  Returned ranges:

  * L: 0.0 - 100.0
  * a: -86.18 - 98.25
  * b: -107.86 - 94.48"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [xyz (to-XYZ c)
        x (to-lab-correct (/ (.x xyz) D65X))
        y (to-lab-correct (/ (.y xyz) D65Y))
        z (to-lab-correct (/ (.z xyz) D65Z))
        L (- (* y 116.0) 16.0)
        a (* 500.0 (- x y))
        b (* 200.0 (- y z))]
    (Vec4. L a b (.w xyz))))

(defn to-LAB*
  "RGB -> LAB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-LAB c)]
    (Vec4. (m/mnorm (.x cc) 0.0 100.0 0.0 255.0)
           (m/mnorm (.y cc) -86.18463649762525 98.25421868616108 0.0 255.0)
           (m/mnorm (.z cc) -107.86368104495168 94.48248544644461 0.0 255.0)
           (.w cc))))

(defn- from-lab-correct
  "LAB correction"
  ^double [^double v]
  (let [v3 (* v v v)]
    (if (> v3 CIEEpsilon)
      v3
      (/ (- (* 116.0 v) 16.0) CIEK))))

(defn from-LAB
  "LAB -> RGB,

  For ranges, see [[to-LAB]]"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        y (/ (+ (.x c) 16.0) 116.0)
        x (* D65X (from-lab-correct (+ y (/ (.y c) 500.0))))
        z (* D65Z (from-lab-correct (- y (/ (.z c) 200.0))))]
    (from-XYZ (Vec4. x (* D65Y (from-lab-correct y)) z (.w c)))))

(defn from-LAB*
  "LAB -> RGB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-LAB (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 100.0)
                     (m/mnorm (.y c) 0.0 255.0 -86.18463649762525 98.25421868616108)
                     (m/mnorm (.z c) 0.0 255.0 -107.86368104495168 94.48248544644461)
                     (.w c)))))


;;

(defn to-LUV
  "RGB -> LUV

  Returned ranges:

  * L: 0.0 - 100.0
  * u: -83.08 - 175.05
  * v: -134.12 - 107.40"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-XYZ c)
        uv-factor (+ (.x cc) (* 15.0 (.y cc)) (* 3.0 (.z cc)))]
    (if (zero? uv-factor)
      (Vec4. 0.0 0.0 0.0 (.w cc))
      (let [uv-factor* (/ uv-factor)
            var-u (* 4.0 (.x cc) uv-factor*)
            var-v (* 9.0 (.y cc) uv-factor*)
            var-y (to-lab-correct (/ (.y cc) 100.0))
            L (- (* 116.0 var-y) 16.0)] 
        (Vec4. L
               (* 13.0 L (- var-u REF-U))
               (* 13.0 L (- var-v REF-V))
               (.w cc))))))

(defn to-LUV*
  "RGB -> LUV, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-LUV c)]
    (Vec4. (m/mnorm (.x cc) 0.0 100.0 0.0 255.0)
           (m/mnorm (.y cc) -83.07975193131836 175.05303573649485 0.0 255.0)
           (m/mnorm (.z cc) -134.1160763907768 107.40136474095397 0.0 255.0)
           (.w cc))))

(defn from-LUV
  "LUV -> RGB

  For ranges, see [[to-LUV]]"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (if (zero? (.x c))
      (Vec4. 0.0 0.0 0.0 (.w c))
      (let [var-y (from-lab-correct (/ (+ (.x c) 16.0) 116.0))
            var-u (+ REF-U (/ (.y c) (* 13.0 (.x c))))
            var-v (+ REF-V (/ (.z c) (* 13.0 (.x c))))
            Y (* 100.0 var-y)
            X (/ (* -9.0 Y var-u) (- (* (- var-u 4.0) var-v) (* var-u var-v)))]
        (from-XYZ (Vec4. X
                         Y
                         (/ (- (* 9.0 Y) (* 15.0 var-v Y) (* var-v X)) (* 3.0 var-v))
                         (.w c)))))))

(defn from-LUV*
  "LUV -> RGB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-LUV (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 100.0)
                     (m/mnorm (.y c) 0.0 255.0 -83.07975193131836 175.05303573649485)
                     (m/mnorm (.z c) 0.0 255.0 -134.1160763907768 107.40136474095397)
                     (.w c)))))

;; HLab

(def ^:private ^:const ^double Ka (* (/ 175.0 198.04) (+ D65X D65Y)))
(def ^:private ^:const ^double Kb (* (/ 70.0 218.11) (+ D65Y D65Z)))

(defn to-HunterLAB
  "RGB -> HunterLAB

  Returned ranges:

  * L: 0.0 - 100.0
  * a: -69.08 - 109.48
  * b: -199.78 - 55.72"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-XYZ c)
        X (/ (.x cc) D65X)
        Y (/ (.y cc) D65Y)
        Z (/ (.z cc) D65Z)]
    (if (zero? Y)
      (Vec4. 0.0 0.0 0.0 (.w cc))
      (let [sqrtY (m/sqrt Y)]
        (Vec4. (* 100.0 sqrtY)
               (* Ka (/ (- X Y) sqrtY))
               (* Kb (/ (- Y Z) sqrtY))
               (.w cc))))))

(defn to-HunterLAB*
  "RGB -> HunterLAB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-HunterLAB c)]
    (Vec4. (m/mnorm (.x cc) 0.0 100.0 0.0 255.0)
           (m/mnorm (.y cc) -69.08211393661531 109.48378856734126 0.0 255.0)
           (m/mnorm (.z cc) -199.78221402287008  55.7203132978682 0.0 255.0)
           (.w cc))))

(defn from-HunterLAB
  "HunterLAB -> RGB

  For ranges, see [[to-HunterLAB]]"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (if (zero? (.x c))
      (Vec4. 0.0 0.0 0.0 (.w c))
      (let [Y (* 100.0 (m/sq (/ (.x c) D65Y)))
            Y' (/ Y D65Y)
            sqrtY' (m/sqrt Y')
            X (* D65X (+ (* (/ (.y c) Ka) sqrtY') Y'))
            Z (- (* D65Z (- (* (/ (.z c) Kb) sqrtY') Y')))]
        (from-XYZ (Vec4. X Y Z (.w c)))))))

(defn from-HunterLAB*
  "HunterLAB -> RGB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-HunterLAB (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 100.0)
                           (m/mnorm (.y c) 0.0 255.0 -69.08211393661531 109.48378856734126)
                           (m/mnorm (.z c) 0.0 255.0 -199.78221402287008  55.7203132978682)
                           (.w c)))))

;;

(defn to-LCH
  "RGB -> LCH

  Returned ranges:

  * L: 0.0 - 100.0
  * C: 0.0 - 133.82
  * H: 0.0 - 360.0"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-LAB c)
        H (m/atan2 (.z cc) (.y cc))
        Hd (if (pos? H)
             (m/degrees H)
             (- 360.0 (m/degrees (m/abs H))))
        C (m/hypot-sqrt (.y cc) (.z cc))]
    (Vec4. (.x cc) C Hd (.w cc))))

(defn to-LCH*
  "RGB -> LCH, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-LCH c)]
    (Vec4. (m/mnorm (.x cc) 0.0 100.0 0.0 255.0)
           (m/mnorm (.y cc) 0.0 133.81586201619496 0.0 255.0)
           (m/mnorm (.z cc) 2.1135333225536313E-5  360.0 0.0 255.0)
           (.w cc))))

(defn from-LCH
  "LCH -> RGB

  For ranges, see [[to-LCH]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        h (m/radians (.z c))
        a (* (.y c) (m/cos h))
        b (* (.y c) (m/sin h))]
    (from-LAB (Vec4. (.x c) a b (.w c)))))

(defn from-LCH*
  "LCH -> RGB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-LCH (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 100.0)
                     (m/mnorm (.y c) 0.0 255.0 0.0 133.81586201619496)
                     (m/mnorm (.z c) 0.0 255.0 2.1135333225536313E-5 360.0)
                     (.w c)))))

;; ### Yxy (xyY)

(defn to-Yxy
  "RGB -> Yxy

  Returned ranges:

  * Y: 0.0 - 100.0
  * x: 0.15 - 0.64
  * y: 0.06 - 0.60"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [xyz (to-XYZ c)
        d (+ (.x xyz) (.y xyz) (.z xyz))]
    (if (zero? d)
      (Vec4. 0.0 0.3127159072215825 0.3290014805066623 (.w xyz))
      (Vec4. (.y xyz)
             (/ (.x xyz) d)
             (/ (.y xyz) d)
             (.w xyz)))))

(defn to-Yxy*
  "RGB -> Yxy, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-Yxy c)]
    (Vec4. (m/mnorm (.x cc) 0.0 100.0 0.0 255.0)
           (m/mnorm (.y cc) 0.0 0.640074499456775 0.0 255.0)
           (m/mnorm (.z cc) 0.0 0.6000000000000001 0.0 255.0)
           (.w cc))))

(defn from-Yxy
  "Yxy -> RGB

  For ranges, see [[to-Yxy]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (if (zero? (.x c))
      (Vec4. 0.0 0.0 0.0 (.w c))
      (let [Yy (/ (.x c) (.z c))
            X (* (.y c) Yy) 
            Z (* (- 1.0 (.y c) (.z c)) Yy)]
        (from-XYZ (Vec4. X (.x c) Z (.w c)))))))

(defn from-Yxy*
  "Yxy -> RGB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-Yxy (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 100.0)
                     (m/mnorm (.y c) 0.0 255.0 0.0 0.640074499456775)
                     (m/mnorm (.z c) 0.0 255.0 0.0 0.6000000000000001)
                     (.w c)))))

;; ### LMS - normalized D65
(defn to-LMS
  "RGB -> LMS, D65

  Ranges: 0.0 - 100.0"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [c (to-XYZ c)]
    (Vec4. (+ (* 0.40024 (.x c)) (* 0.7076 (.y c)) (* -0.08081 (.z c)))
           (+ (* -0.2263 (.x c)) (* 1.16532 (.y c)) (* 0.0457 (.z c)))
           (* 0.91822 (.z c))
           (.w c))))

(defn to-LMS*
  "RGB -> LMS, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-LMS c)]
    (Vec4. (m/mnorm (.x cc) 0.0 100.00260300000001 0.0 255.0)
           (m/mnorm (.y cc) 0.0 99.998915 0.0 255.0)
           (m/mnorm (.z cc) 0.0 99.994158 0.0 255.0)
           (.w cc))))

(defn from-LMS
  "LMS -> RGB, D65

  Ranges: 0.0 - 100.0"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-XYZ (Vec4. (+ (* 1.8599363874558397 (.x c)) (* -1.1293816185800916 (.y c)) (* 0.2198974095961933 (.z c)))
                     (+ (* 0.3611914362417676 (.x c)) (* 0.6388124632850422 (.y c)) (* -0.0000063705968386499 (.z c)))
                     (* 1.0890636230968613 (.z c))
                     (.w c)))))

(defn from-LMS*
  "LMS -> RGB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-LMS (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 100.00260300000001)
                     (m/mnorm (.y c) 0.0 255.0 0.0 99.998915)
                     (m/mnorm (.z c) 0.0 255.0 0.0 99.994158)
                     (.w c)))))

;; IPT

(defmacro ^:private spow 
  "Symmetric pow"
  [v e]
  `(if (neg? ~v)
     (- (m/pow (- ~v) ~e))
     (m/pow ~v ~e)))

(defn- spow-043
  ^double [^double v]
  (spow v 0.43))

(defn- spow-r043
  ^double [^double v]
  (spow v 2.3255813953488373))

(defn to-IPT
  "RGB -> IPT

  Ranges:

  * I: 0.0 - 7.244
  * Cp: -3.285 - 4.8
  * Ct: -5.422 - 4.72"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [c (to-XYZ c)
        ^Vec3 LMS (-> (Vec3. (+ (* 0.4002 (.x c)) (* 0.7075 (.y c)) (* -0.0807 (.z c)))
                             (+ (* -0.228 (.x c)) (* 1.15 (.y c)) (* 0.0612 (.z c)))
                             (* 0.9184 (.z c)))
                      (v/fmap spow-043))]
    (Vec4. (+ (* 0.4 (.x LMS)) (* 0.4 (.y LMS)) (* 0.2 (.z LMS)))
           (+ (* 4.455 (.x LMS)) (* -4.851 (.y LMS)) (* 0.396 (.z LMS)))
           (+ (* 0.8056 (.x LMS)) (* 0.3572 (.y LMS)) (* -1.1628 (.z LMS)))
           (.w c))))

(defn to-IPT*
  "RGB -> IPT, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-IPT c)]
    (Vec4. (m/mnorm (.x cc) 0.0 7.2443713084435615 0.0 255.0)
           (m/mnorm (.y cc) -3.2846335885160194 4.799977261009928 0.0 255.0)
           (m/mnorm (.z cc) -5.422400706730331 4.719620894528216 0.0 255.0)
           (.w cc))))

(defn from-IPT
  "IPT -> RGB"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        ^Vec3 LMS' (-> (Vec3. (+ (* 1.0000000000000002 (.x c)) (* 0.0975689305146139 (.y c)) (* 0.2052264331645916 (.z c)))
                              (+ (* 0.9999999999999999 (.x c)) (* -0.1138764854731471 (.y c)) (* 0.13321715836999806 (.z c)))
                              (+ (* 0.9999999999999999 (.x c)) (* 0.0326151099170664 (.y c)) (* -0.6768871830691793 (.z c))))
                       (v/fmap spow-r043))]
    (from-XYZ (Vec4. (+ (* 1.8502429449432056 (.x LMS')) (* -1.1383016378672328 (.y LMS')) (* 0.23843495850870136 (.z LMS')))
                     (+ (* 0.3668307751713486 (.x LMS')) (* 0.6438845448402355 (.y LMS')) (* -0.010673443584379992 (.z LMS')))
                     (* 1.088850174216028 (.z LMS'))
                     (.w c)))))

(defn from-IPT*
  "LMS -> RGB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-IPT (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 7.2443713084435615)
                     (m/mnorm (.y c) 0.0 255.0 -3.2846335885160194 4.799977261009928)
                     (m/mnorm (.z c) 0.0 255.0 -5.422400706730331 4.719620894528216)
                     (.w c)))))

;; Jab https://www.osapublishing.org/oe/abstract.cfm?uri=oe-25-13-15131

(def ^:private ^:const ^double jab-b 1.15)
(def ^:private ^:const ^double jab-g 0.66)
(def ^:private ^:const ^double jab-rb (/ jab-b))
(def ^:private ^:const ^double jab-rg (/ jab-g))
(def ^:private ^:const ^double jab-b- (dec jab-b))
(def ^:private ^:const ^double jab-g- (dec jab-g))
(def ^:private ^:const ^double jab-c1 (/ 3424.0 (m/fpow 2.0 12)))
(def ^:private ^:const ^double jab-c2 (/ 2413.0 (m/fpow 2.0 7)))
(def ^:private ^:const ^double jab-c3 (/ 2392.0 (m/fpow 2.0 7)))
(def ^:private ^:const ^double jab-n (/ 2610.0 (m/fpow 2.0 14)))
(def ^:private ^:const ^double jab-p (* 1.7 (/ 2523.0 (m/fpow 2.0 5))))
(def ^:private ^:const ^double jab-rn (/ jab-n))
(def ^:private ^:const ^double jab-rp (/ jab-p))
(def ^:private ^:const ^double jab-d -0.56)
(def ^:private ^:const ^double jab-d+ (inc jab-d))
(def ^:private ^:const ^double jab-d0 1.6295499532821566e-11)

(defn- jab-lms->lms' 
  ^double [^double v]
  (let [v (m/pow (/ v 10000.0) jab-n)]
    (m/pow (/ (+ jab-c1 (* jab-c2 v))
              (inc (* jab-c3 v))) jab-p)))

(defn to-JAB
  "RGB -> JzAzBz

  Jab https://www.osapublishing.org/oe/abstract.cfm?uri=oe-25-13-15131

  Ranges:

  * J: 0.0 - 0.17
  * a: -0.09 - 0.11
  * b: -0.156 - 0.115"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [c (to-XYZ c)
        X' (- (* jab-b (.x c)) (* jab-b- (.z c)))
        Y' (- (* jab-g (.y c)) (* jab-g- (.x c)))
        ^Vec3 LMS' (-> (Vec3. (+ (* 0.41478972 X') (* 0.579999 Y') (* 0.0146480 (.z c)))
                              (+ (* -0.2015100 X') (* 1.120649 Y') (* 0.0531008 (.z c)))
                              (+ (* -0.0166008 X') (* 0.264800 Y') (* 0.6684799 (.z c))))
                       (v/fmap jab-lms->lms'))
        ^Vec3 Iab (Vec3. (+ (* 0.5 (.x LMS')) (* 0.5 (.y LMS')))
                         (+ (* 3.524000 (.x LMS')) (* -4.066708 (.y LMS')) (* 0.542708 (.z LMS')))
                         (+ (* 0.199076 (.x LMS')) (* 1.096799 (.y LMS')) (* -1.295875 (.z LMS'))))]
    (Vec4. (- (/ (* jab-d+ (.x Iab))
                 (inc (* jab-d (.x Iab)))) jab-d0) (.y Iab) (.z Iab) (.w c))))

(defn to-JAB*
  "RGB -> JzAzBz, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-JAB c)]
    (Vec4. (m/mnorm (.x cc) -3.2311742677852644E-26 0.16717463103478347 0.0 255.0)
           (m/mnorm (.y cc) -0.09286319310837648 0.1090265140291988 0.0 255.0)
           (m/mnorm (.z cc) -0.15632173559361429 0.11523306877502998 0.0 255.0)
           (.w cc))))

(defn- jab-lms'->lms 
  ^double [^double v]
  (let [v (m/pow v jab-rp)]
    (* 10000.0 (m/pow (/ (- jab-c1 v)
                         (- (* jab-c3 v) jab-c2)) jab-rn))))

(defn from-JAB
  "JzAzBz -> RGB

  For ranges, see [[to-JAB]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        J+ (+ jab-d0 (.x c))
        I (/ J+ (- jab-d+ (* jab-d J+)))
        ^Vec3 LMS (-> (Vec3. (+ (* 1.0000000000000002 I) (* 0.1386050432715393 (.y c)) (* 0.05804731615611886 (.z c)))
                             (+ (* 0.9999999999999999 I) (* -0.1386050432715393 (.y c)) (* -0.05804731615611886 (.z c)))
                             (+ (* 0.9999999999999998 I) (* -0.09601924202631895 (.y c)) (* -0.8118918960560388 (.z c))))
                      (v/fmap jab-lms'->lms))
        ^Vec3 XYZ' (Vec3. (+ (* 1.9242264357876069 (.x LMS)) (* -1.0047923125953657 (.y LMS)) (* 0.037651404030617994 (.z LMS)))
                          (+ (* 0.350316762094999 (.x LMS)) (* 0.7264811939316552 (.y LMS)) (* -0.06538442294808501 (.z LMS)))
                          (+ (* -0.09098281098284752 (.x LMS)) (* -0.3127282905230739 (.y LMS)) (* 1.5227665613052603 (.z LMS))))
        X (* jab-rb (+ (.x XYZ') (* jab-b- (.z XYZ'))))]
    (from-XYZ (Vec4. X
                     (* jab-rg (+ (.y XYZ') (* jab-g- X)))
                     (.z XYZ') (.w c)))))

(defn from-JAB*
  "JzAzBz -> RGB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-JAB (Vec4. (m/mnorm (.x c) 0.0 255.0 -3.2311742677852644E-26 0.16717463103478347)
                     (m/mnorm (.y c) 0.0 255.0 -0.09286319310837648 0.1090265140291988)
                     (m/mnorm (.z c) 0.0 255.0 -0.15632173559361429 0.11523306877502998)
                     (.w c)))))

;;

(defn to-JCH
  "RGB -> JCH

  Hue based color space derived from JAB
  
  Ranges:

  * J: 0.0 - 0.167
  * C: 0.0 - 0.159
  * H: 0.0 - 360.0"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-JAB c)
        H (m/atan2 (.z cc) (.y cc))
        Hd (if (pos? H)
             (m/degrees H)
             (- 360.0 (m/degrees (m/abs H))))
        C (m/hypot-sqrt (.y cc) (.z cc))]
    (Vec4. (.x cc) C Hd (.w cc))))

(defn to-JCH*
  "RGB -> JCH, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-JCH c)]
    (Vec4. (m/mnorm (.x cc) -3.2311742677852644E-26, 0.16717463103478347 0.0 255.0)
           (m/mnorm (.y cc) 1.2924697071141057E-26, 0.15934590856406236 0.0 255.0)
           (m/mnorm (.z cc) 1.0921476445810189E-5, 359.99995671898046 0.0 255.0)
           (.w cc))))

(defn from-JCH
  "JCH -> RGB

  For ranges, see [[to-JCH]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        h (m/radians (.z c))
        a (* (.y c) (m/cos h))
        b (* (.y c) (m/sin h))]
    (from-JAB (Vec4. (.x c) a b (.w c)))))

(defn from-JCH*
  "JCH -> RGB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-JCH (Vec4. (m/mnorm (.x c) 0.0 255.0 -3.2311742677852644E-26, 0.16717463103478347)
                     (m/mnorm (.y c) 0.0 255.0 1.2924697071141057E-26, 0.15934590856406236)
                     (m/mnorm (.z c) 0.0 255.0 1.0921476445810189E-5, 359.99995671898046)
                     (.w c)))))


;; Hue based

(defn- to-HC
  "Calculate hue and chroma"
  ^Vec4 [^Vec4 c]
  (let [M (max (.x c) (.y c) (.z c))
        m (min (.x c) (.y c) (.z c))
        C (- M m)
        ^double h (if (zero? C)
                    0.0
                    (let [rC (/ C)]
                      (cond
                        (== M (.x c)) (mod (* rC (- (.y c) (.z c))) 6.0)
                        (== M (.y c)) (+ 2.0 (* rC (- (.z c) (.x c))))
                        :else (+ 4.0 (* rC (- (.x c) (.y c)))))))]
    (Vec4. (* 60.0 h) C M m)))

(defn- from-HCX
  "Convert HCX to RGB"
  ^Vec3 [^double h ^double c ^double x]
  (cond
    (<= 0.0 h 1.0) (Vec3. c x 0.0)
    (<= 1.0 h 2.0) (Vec3. x c 0.0)
    (<= 2.0 h 3.0) (Vec3. 0.0 c x)
    (<= 3.0 h 4.0) (Vec3. 0.0 x c)
    (<= 4.0 h 5.0) (Vec3. x 0.0 c)
    :else (Vec3. c 0.0 x)))

(def ^:private ^:const ^double n360->255 (/ 255.0 359.7647058823529))
(def ^:private ^:const ^double n255->360 (/ 359.7647058823529 255.0))

(defn- normalize-HSx
  "Make output range 0-255"
  ^Vec4 [^Vec4 c]
  (Vec4. (* n360->255 (.x c))
         (* 255.0 (.y c))
         (* 255.0 (.z c))
         (.w c)))

(defn- denormalize-HSx 
  "Make output range native to HSx colorspaces"
  ^Vec4 [^Vec4 c]
  (Vec4. (* n255->360 (.x c))
         (/ (.y c) 255.0)
         (/ (.z c) 255.0)
         (.w c)))

(defn- wrap-hue 
  "Wrap hue to enable interpolations"
  ^double [^double h]
  (cond
    (neg? h) (+ h 360.0)
    (> h 360.0) (- h 360.0)
    :else h))

;; HSI

(defn to-HSI
  "RGB -> HSI

  Ranges:

  * H: 0.0 - 360
  * S: 0.0 - 1.0
  * I: 0.0 - 1.0"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        hc (to-HC c)
        I (/ (+ (.x c) (.y c) (.z c)) 3.0)
        S (if (zero? I) 0.0
              (- 1.0 (/ (.w hc) I)))]
    (Vec4. (.x hc) S (/ I 255.0) (.w c))))

(defn from-HSI
  "HSI -> RGB

  For ranges, see [[to-HSI]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        h' (/ (wrap-hue (.x c)) 60.0)
        z (- 1.0 (m/abs (dec (mod h' 2.0))))
        C (/ (* 3.0 (.z c) (.y c)) (inc z))
        X (* C z)
        m (* (.z c) (- 1.0 (.y c)))
        rgb' (v/add (from-HCX h' C X) (Vec3. m m m))]
    (v/vec4 (v/mult rgb' 255.0) (.w c))))

(def ^{:metadoc/categories meta-conv :doc "RGB -> HSI, normalized"} to-HSI* (comp normalize-HSx to-HSI))
(def ^{:metadoc/categories meta-conv :doc "HSI -> RGB, normalized"} from-HSI* (comp from-HSI denormalize-HSx pr/to-color))

;; HSV

(defn to-HSV
  "RGB -> HSV

    Ranges:

  * H: 0.0 - 360
  * S: 0.0 - 1.0
  * V: 0.0 - 1.0"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        hc (to-HC c)
        V (.z hc)
        S (if (zero? V) 0.0
              (/ (.y hc) V))]
    (Vec4. (.x hc) S (/ V 255.0) (.w c))))

(defn from-HSV
  "HSV -> RGB

  Same as HSB.
  
  For ranges, see [[to-HSV]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        C (* (.y c) (.z c))
        h' (/ (wrap-hue (.x c)) 60.0)
        X (* C (- 1.0 (m/abs (dec (mod h' 2.0)))))
        m (- (.z c) C)
        ^Vec3 rgb' (v/add (from-HCX h' C X) (Vec3. m m m))]
    (v/vec4 (v/mult rgb' 255.0) (.w c))))

(def ^{:metadoc/categories meta-conv :doc "RGB -> HSV, normalized"} to-HSV* (comp normalize-HSx to-HSV))
(def ^{:metadoc/categories meta-conv :doc "HSV -> RGB, normalized"} from-HSV* (comp from-HSV denormalize-HSx pr/to-color))

;; HSB = HSV

(def ^{:metadoc/categories meta-conv :doc "RGB -> HSB(V), normalized (see [[to-HSV]])"} to-HSB to-HSV)
(def ^{:metadoc/categories meta-conv :doc "HSB(V) -> RGB, normalized (see [[from-HSV]])"} from-HSB from-HSV)
(def ^{:metadoc/categories meta-conv :doc "RGB -> HSB(V) (see [[to-HSV*]])"} to-HSB* to-HSV*)
(def ^{:metadoc/categories meta-conv :doc "HSB(V) -> RGB (see [[from-HSV*]])"} from-HSB* from-HSV*)

;; HSL

(defn to-HSL
  "RGB -> HSL

  Ranges:

  * H: 0.0 - 360
  * S: 0.0 - 1.0
  * L: 0.0 - 1.0"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        hc (to-HC c)
        L (/ (* 0.5 (+ (.z hc) (.w hc))) 255.0)
        S (if (or (== 1.0 L)
                  (zero? (.y hc))) 0.0
              (/ (.y hc) (- 1.0 (m/abs (dec (+ L L))))))]
    (Vec4. (.x hc) (/ S 255.0) L (.w c))))

(defn from-HSL
  "HSL -> RGB

  For ranges, see [[to-HSL]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        C (* (.y c) (- 1.0 (m/abs (dec (+ (.z c) (.z c))))))
        h' (/ (wrap-hue (.x c)) 60.0)
        X (* C (- 1.0 (m/abs (dec (mod h' 2.0)))))
        m (- (.z c) (* 0.5 C))
        ^Vec3 rgb' (v/add (from-HCX h' C X) (Vec3. m m m))]
    (v/vec4 (v/mult rgb' 255.0) (.w c))))

(def ^{:metadoc/categories meta-conv :doc "RGB -> HSL, normalized"} to-HSL* (comp normalize-HSx to-HSL))
(def ^{:metadoc/categories meta-conv :doc "HSL -> RGB, normalized"} from-HSL* (comp from-HSL denormalize-HSx pr/to-color))

;; HCL
;; http://w3.uqo.ca/missaoui/Publications/TRColorSpace.zip

(defn to-HCL
  "RGB -> HCL, by Sarifuddin and Missaou.

  lambda = 3.0
  
  Returned ranges:

  * H: -180.0 - 180.0
  * C: 0.0 - 170.0
  * L: 0.0 - 135.266"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        r (.x c)
        g (.y c)
        b (.z c)
        mn (min r g b)
        mx (max r g b)
        Q (m/exp (if (zero? mx) 0.0 (* 0.03 (/ mn mx))))
        L (* 0.5 (+ (* Q mx) (* (dec Q) mn)))
        gb- (- g b)
        rg- (- r g)
        C (* (/ Q 3.0) (+ (m/abs rg-)
                          (m/abs gb-)
                          (m/abs (- b r))))
        H (m/degrees (if (zero? gb-) 0.0 (m/atan (/ gb- rg-))))
        H (cond
            (and (>= rg- 0.0) (>= gb- 0,0)) (* H m/TWO_THIRD)
            (and (>= rg- 0.0) (neg? gb-)) (* 2.0 m/TWO_THIRD H)
            (and (neg? rg-) (>= gb- 0,0)) (+ (* 2.0 m/TWO_THIRD H) 180.0)
            (and (neg? rg-) (neg? gb-)) (- (* m/TWO_THIRD H) 180.0) 
            :else H)]
    (Vec4. H C L (.w c))))

(defn from-HCL
  "HCL -> RGB, by Sarifuddin and Missaou.

  For accepted ranges, see [[to-HCL]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        H (m/constrain (.x c) -179.8496181535773 180.0)
        C (* 3.0 (.y c))
        L (* 4.0 (.z c))
        Q (* 2.0 (m/exp (* 0.03 (- 1.0 (/ C L)))))
        mn (/ (- L C) (* 2.0 (dec Q)))
        mx (+ mn (/ C Q))]
    (cond
      (<= 0.0 H 60.0) (let [t (m/tan (m/radians (* 1.5 H)))]
                        (Vec4. mx (/ (+ (* mx t) mn) (inc t)) mn (.w c)))
      (<= 60.0 H 120.0) (let [t (m/tan (m/radians (* 0.75 (- H 180.0))))]
                          (Vec4. (/ (- (* mx (inc t)) mn) t) mx mn (.w c)))
      (<= 120.0 H 180.0) (let [t (m/tan (m/radians (* 0.75 (- H 180.0))))]
                           (Vec4. mn mx (- (* mx (inc t)) (* mn t)) (.w c)))
      (<= -60.0 H 0.0) (let [t (m/tan (m/radians (* 0.75 H)))]
                         (Vec4. mx mn (- (* mn (inc t)) (* mx t)) (.w c)))
      (<= -120.0 H -60.0) (let [t (m/tan (m/radians (* 0.75 H)))]
                            (Vec4. (/ (- (* mn (inc t)) mx) t) mn mx (.w c)))
      (<= -180.0 H -120.0) (let [t (m/tan (m/radians (* 1.5 (+ H 180.0))))]
                             (Vec4. mn (/ (+ (* mn t) mx) (inc t)) mx (.w c))))))

(defn to-HCL*
  "RGB -> HCL, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c] 
  (let [cc (to-HCL c)]
    (Vec4. (m/mnorm (.x cc) -179.8496181535773 180.0 0.0 255.0)
           (* 255.0 (/ (.y cc) 170.0))
           (* 255.0 (/ (.z cc) 135.26590615814683))
           (.w cc))))

(defn from-HCL*
  "HCL -> RGB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-HCL (Vec4. (m/mnorm (.x c) 0.0 255.0 -179.8496181535773 180.0)
                     (* 170.0 (/ (.y c) 255.0))
                     (* 135.26590615814683 (/ (.z c) 255.0))
                     (.w c)))))

;; ### HWB

;; HWB - A More Intuitive Hue-Based Color Model
;; by Alvy Ray Smitch and Eric Ray Lyons, 1995-1996

(defn to-HWB
  "RGB -> HWB

  HWB - A More Intuitive Hue-Based Color Model
  by Alvy Ray Smitch and Eric Ray Lyons, 1995-1996

  Ranges:

  * H: 0.0 - 360.0
  * W: 0.0 - 1.0
  * B: 0.0 - 1.0"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        w (min (.x c) (.y c) (.z c))
        v (max (.x c) (.y c) (.z c))
        h (if (== w v) 0
              (let [^double f (condp m/eq w
                                (.x c) (- (.y c) (.z c))
                                (.y c) (- (.z c) (.x c))
                                (.z c) (- (.x c) (.y c)))
                    ^double p (condp m/eq w
                                (.x c) 3.0
                                (.y c) 5.0
                                (.z c) 1.0)]
                (* 60.0 (- p (/ f (- v w))))))]
    (Vec4. h (/ w 255.0) (- 1.0 (/ v 255.0)) (.w c))))

(defn to-HWB*
  "RGB - HWB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-HWB c)]
    (Vec4. (* 255.0 (/ (.x cc) 360.0))
           (* 255.0 (.y cc))
           (* 255.0 (.z cc))
           (.w cc))))

(defn from-HWB
  "HWB -> RGB

  For ranges, see [[to-HWB]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (if (zero? (.x c)) 
      (let [v (* 255.0 (- 1.0 (.z c)))]
        (Vec4. v v v (.w c)))
      (let [h (/ (.x c) 60.0)
            v (- 1.0 (.z c))
            w (.y c)
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
        (v/vec4 (v/mult rgb 255.0) (.w c))))))

(defn from-HWB*
  "HWB -> RGB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-HWB (Vec4. (* 360.0 (/ (.x c) 255.0))
                     (/ (.y c) 255.0)
                     (/ (.z c) 255.0)
                     (.w c)))))

;; ### GLHS
;;
;; Color Theory and Modeling for Computer Graphics, Visualization, and Multimedia Applications (The Springer International Series in Engineering and Computer Science) by Haim Levkowitz

;; Page 79, minimizer
(def ^:private ^:const ^double weight-max 0.7)
(def ^:private ^:const ^double weight-mid 0.1)
(def ^:private ^:const ^double weight-min 0.2)

(defn to-GLHS
  "RGB -> GLHS

  Color Theory and Modeling for Computer Graphics, Visualization, and Multimedia Applications (The Springer International Series in Engineering and Computer Science) by Haim Levkowitz

  Weights: 0.2 (min), 0.1 (mid), 0.7 (max).

  Ranges:
  
  * L: 0.0 - 1.0
  * H: 0.0 - 360.0
  * S: 0.0 - 1.0"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        mx (max (.x c) (.y c) (.z c))
        md (stat/median-3 (.x c) (.y c) (.z c))
        mn (min (.x c) (.y c) (.z c))]
    (if (== mx mn)
      (Vec4. (/ mx 255.0) 0 0 (.w c))
      (let [l (+ (* weight-max mx) (* weight-mid md) (* weight-min mn))
            r (/ (- mx mn))
            e (* (- md mn) r)
            ^long k (cond
                      (and (> (.x c) (.y c)) (>= (.y c) (.z c))) 0
                      (and (>= (.y c) (.x c)) (> (.x c) (.z c))) 1
                      (and (> (.y c) (.z c)) (>= (.z c) (.x c))) 2
                      (and (>= (.z c) (.y c)) (> (.y c) (.x c))) 3
                      (and (> (.z c) (.x c)) (>= (.x c) (.y c))) 4
                      :else 5)
            f (if (even? k)
                e
                (* (- mx md) r))
            h (* 60.0 (+ k f))
            lq (* 255.0 (+ (* weight-mid e) weight-max))
            s (if (<= l lq)
                (/ (- l mn) l)
                (/ (- mx l) (- 255.0 l)))]
        (Vec4. (/ l 255.0) h s (.w c))))))

(defn to-GLHS*
  "RGB -> GLHS, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c] 
  (let [cc (to-GLHS c)]
    (Vec4. (* 255.0 (.x cc))
           (m/mnorm (.y cc) 0.0 359.7647058823529 0.0 255.0)
           (* 255.0 (.z cc))
           (.w cc))))

(defn from-GLHS
  "GLHS -> RGB

  For ranges, see [[to-GLHS]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        l (* 255.0 (.x c))]
    (if (zero? (.z c))
      (Vec4. l l l (.w c))
      (let [h (/ (wrap-hue (.y c)) 60.0)
            k (long (m/floor h))
            f (- h k)
            fp (if (even? k) f (- 1.0 f))
            wfw (+ (* weight-mid fp) weight-max)
            lq (* 255.0 wfw)
            s (.z c)
            ^Vec3 rgb (if (<= l lq)
                        (let [mn (* (- 1.0 s) l)
                              md (/ (+ (* fp l) (* mn (- (* (- 1.0 fp) weight-max) (* fp weight-min)))) wfw)
                              mx (/ (- (- l (* md weight-mid)) (* mn weight-min)) weight-max)]
                          (Vec3. mn md mx))
                        (let [mx (+ (* s 255.0) (* (- 1.0 s) l))
                              md (/ (- (* (- 1.0 fp) l) (* mx (- (* (- 1.0 fp) weight-max) (* fp weight-min))))
                                    (+ (* (- 1.0 fp) weight-mid) weight-min))
                              mn (/ (- (- l (* mx weight-max)) (* md weight-mid)) weight-min)]
                          (Vec3. mn md mx)))]
        (case k
          0 (Vec4. (.z rgb) (.y rgb) (.x rgb) (.w c))
          1 (Vec4. (.y rgb) (.z rgb) (.x rgb) (.w c))
          2 (Vec4. (.x rgb) (.z rgb) (.y rgb) (.w c))
          3 (Vec4. (.x rgb) (.y rgb) (.z rgb) (.w c))
          4 (Vec4. (.y rgb) (.x rgb) (.z rgb) (.w c))
          5 (Vec4. (.z rgb) (.x rgb) (.y rgb) (.w c))
          6 (Vec4. (.z rgb) (.y rgb) (.x rgb) (.w c)))))))

(defn from-GLHS*
  "GLHS -> RGB"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-GLHS (Vec4. (/ (.x c) 255.0)
                      (m/mnorm (.y c) 0.0 255.0 0.0 359.7647058823529)
                      (/ (.z c) 255.0)
                      (.w c)))))

;; ### YPbPr

(defn to-YPbPr
  "RGB -> YPbPr

  Ranges:

  * Y: 0.0 - 255.0
  * Pb: -236.6 - 236.6
  * Pr: -200.8 - 200.8"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        y (+ (* 0.2126 (.x c))
             (* 0.7152 (.y c))
             (* 0.0722 (.z c)))
        pb (- (.z c) y)
        pr (- (.x c) y)]
    (Vec4. y pb pr (.w c))))

(defn to-YPbPr*
  "RGB -> YPbPr, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-YPbPr c)]
    (Vec4. (.x cc)
           (m/mnorm (.y cc) -236.589 236.589 0.0 255.0)
           (m/mnorm (.z cc) -200.787 200.787 0.0 255.0)
           (.w cc))))

(defn from-YPbPr
  "YPbPr -> RGB

  For ranges, see [[to-YPbPr]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        b (+ (.x c) (.y c))
        r (+ (.x c) (.z c))
        g (/ (- (.x c) (* 0.2126 r) (* 0.0722 b)) 0.7152)]
    (Vec4. r g b (.w c))))

(defn from-YPbPr*
  "YPbPr -> RGB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-YPbPr (Vec4. (.x c)
                       (m/mnorm (.y c) 0.0 255.0 -236.589 236.589)
                       (m/mnorm (.z c) 0.0 255.0 -200.787 200.787)
                       (.w c)))))

;; ### YDbDr

(defn to-YDbDr
  "RGB -> YDbDr

  Ranges:

  * Y: 0.0 - 255.0
  * Db: -340.0 - 340.0
  * Dr: -340.0 - 340.0"
  {:metadoc/categories meta-conv}
  ^Vec4 [c] 
  (let [^Vec4 c (pr/to-color c)
        Y (+ (* 0.299 (.x c)) (* 0.587 (.y c)) (* 0.114 (.z c)))
        Db (+ (* -0.45 (.x c)) (* -0.883 (.y c)) (* 1.333 (.z c)))
        Dr (+ (* -1.333 (.x c)) (* 1.116 (.y c)) (* 0.217 (.z c)))]
    (Vec4. Y Db Dr (.w c))))

(defn to-YDbDr*
  "RGB -> YDbDr"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-YDbDr c)]
    (Vec4. (.x cc)
           (m/mnorm (.y cc) -339.91499999999996 339.91499999999996 0.0 255.0)
           (m/mnorm (.z cc) -339.91499999999996 339.915 0.0 255.0)
           (.w cc))))

(defn from-YDbDr
  "YDbDr -> RGB

  For ranges, see [[to-YDbDr]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        Y (.x c)
        Db (.y c)
        Dr (.z c)
        r (+ Y (* 9.2303716147657e-05 Db) (* -0.52591263066186533 Dr))
        g (+ Y (* -0.12913289889050927 Db) (* 0.26789932820759876 Dr))
        b (+ Y (* 0.66467905997895482 Db) (* -7.9202543533108e-05 Dr))]
    (Vec4. r g b (.w c))))

(defn from-YDbDr*
  "YDbDr -> RGB, normalized"
  {:metadoc/categories meta-conv}
  [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-YDbDr (Vec4. (.x c)
                       (m/mnorm (.y c) 0.0 255.0 -339.91499999999996 339.91499999999996)
                       (m/mnorm (.z c) 0.0 255.0 -339.91499999999996 339.915)
                       (.w c)))))


;; ### YCbCr

;; JPEG version

(def ^:private ^:const y-norm ohta-s)

(defn to-YCbCr
  "RGB -> YCbCr

  Used in JPEG.

  Ranges;
  
  * Y: 0.0 - 255.0
  * Cb: -127.5 - 127.5
  * Cr: -127.5 - 127.5"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        Y (+ (* 0.298839 (.x c)) (* 0.586811 (.y c)) (* 0.114350 (.z c)))
        Cb (+ (* -0.168736 (.x c)) (* -0.331264 (.y c)) (* 0.5 (.z c)))
        Cr (+ (* 0.5 (.x c)) (* -0.418688 (.y c)) (* -0.081312 (.z c)))]
    (Vec4. Y Cb Cr (.w c))))

(defn to-YCbCr*
  "RGB -> YCbCr, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (v/add (to-YCbCr c) y-norm))

(defn from-YCbCr
  "YCbCr -> RGB

  For ranges, see [[to-YCbCr]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        Cb (.y c)
        Cr (.z c)
        r (+ (* 0.99999999999914679361 (.x c)) (* -1.2188941887145875e-06 Cb) (* 1.4019995886561440468 Cr))
        g (+ (* 0.99999975910502514331 (.x c)) (* -0.34413567816504303521 Cb) (* -0.71413649331646789076 Cr))
        b (+ (* 1.00000124040004623180 (.x c)) (* 1.77200006607230409200 Cb) (* 2.1453384174593273e-06 Cr))]
    (Vec4. r g b (.w c))))

(defn from-YCbCr*
  "YCbCr -> RGB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (from-YCbCr (v/sub (pr/to-color c) y-norm)))

;; ### YUV

(defn to-YUV
  "RGB -> YUV

  Ranges:

  * Y: 0.0 - 255.0
  * u: -111.2 - 111.2
  * v: -156.8 - 156.8"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (Vec4. (+ (* 0.298839 (.x c)) (* 0.586811 (.y c)) (* 0.114350 (.z c)))
           (+ (* -0.147 (.x c)) (* -0.289 (.y c)) (* 0.436 (.z c)))
           (+ (* 0.615 (.x c)) (* -0.515 (.y c)) (* -0.1 (.z c)))
           (.w c))))

(defn to-YUV*
  "RGB -> YUV, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-YUV c)]
    (Vec4. (.x cc)
           (m/mnorm (.y cc) -111.17999999999999 111.17999999999999 0.0 255.0)
           (m/mnorm (.z cc) -156.82500000000002 156.825 0.0 255.0)
           (.w cc))))

(defn from-YUV
  "YUV -> RGB

  For ranges, see [[to-YUV]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        Y (.x c)
        U (.y c)
        V (.z c)
        r (+ Y (* -3.945707070708279e-05 U) (* 1.1398279671717170825 V))
        g (+ Y (* -0.3946101641414141437 U) (* -0.5805003156565656797 V))
        b (+ Y (* 2.0319996843434342537 U) (* -4.813762626262513e-04 V))]
    (Vec4. r g b (.w c))))

(defn from-YUV*
  "YUV -> RGB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-YUV (Vec4. (.x c)
                     (m/mnorm (.y c) 0.0 255.0 -111.17999999999999 111.17999999999999)
                     (m/mnorm (.z c) 0.0 255.0 -156.82500000000002 156.825)
                     (.w c)))))

;; ### YIQ

(defn to-YIQ
  "RGB -> YIQ

  Ranges:

  * Y: 0.0 - 255.0
  * I: -151.9 - 151.9
  * Q: -133.26 - 133.26"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (Vec4. (+ (* 0.298839 (.x c)) (* 0.586811 (.y c)) (* 0.114350 (.z c)))
           (+ (* 0.595716 (.x c)) (* -0.274453 (.y c)) (* -0.321263 (.z c)))
           (+ (* 0.211456 (.x c)) (* -0.522591 (.y c)) (* 0.311135 (.z c)))
           (.w c))))

(defn to-YIQ*
  "RGB -> YIQ, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-YIQ c)]
    (Vec4. (.x cc)
           (m/mnorm (.y cc) -151.90758 151.90758 0.0 255.0)
           (m/mnorm (.z cc) -133.260705 133.260705 0.0 255.0)
           (.w cc))))

(defn from-YIQ
  "YIQ -> RGB

  For ranges, see [[to-YIQ]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        Y (.x c)
        I (.y c)
        Q (.z c)
        r (+ Y (* +0.9562957197589482261 I) (* 0.6210244164652610754 Q))
        g (+ Y (* -0.2721220993185104464 I) (* -0.6473805968256950427 Q))
        b (+ Y (* -1.1069890167364901945 I) (* 1.7046149983646481374 Q))]
    (Vec4. r g b (.w c))))

(defn from-YIQ*
  "YIQ -> RGB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        I (m/mnorm (.y c) 0.0 255.0 -151.90758 151.90758)
        Q (m/mnorm (.z c) 0.0 255.0 -133.260705 133.260705)]
    (from-YIQ (Vec4. (.x c) I Q (.w c)))))

;; ### YCgCo

(defn to-YCgCo
  "RGB -> YCgCo

  Ranges:
  
  * Y: 0.0 - 255.0
  * Cg: -127.5 - 127.5
  * Co: -127.5 - 127.5"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        Y (+ (* 0.25 (.x c)) (* 0.5 (.y c)) (* 0.25 (.z c)))
        Cg (+ (* -0.25 (.x c)) (* 0.5 (.y c)) (* -0.25 (.z c)))
        Co (+ (* 0.5 (.x c)) (* -0.5 (.z c)))]
    (Vec4. Y Cg Co (.w c))))

(defn to-YCgCo*
  "RGB -> YCgCo, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (v/add (to-YCgCo c) y-norm))

(defn from-YCgCo
  "YCgCo -> RGB

  For ranges, see [[to-YCgCo]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        Cg (.y c)
        Co (.z c)
        tmp (- (.x c) Cg)]
    (Vec4. (+ Co tmp) (+ (.x c) Cg) (- tmp Co) (.w c))))

(defn from-YCgCo*
  "YCgCo -> RGB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (from-YCgCo (v/sub (pr/to-color c) y-norm)))

;; Cubehelix

(def ^:private ^:const ^:double ch-a -0.14861)
(def ^:private ^:const ^:double ch-b 1.78277)
(def ^:private ^:const ^:double ch-c -0.29227)
(def ^:private ^:const ^:double ch-d -0.90649)
(def ^:private ^:const ^:double ch-e 1.97294)
(def ^:private ^:const ^:double ch-ed (* ch-e ch-d))
(def ^:private ^:const ^:double ch-eb (* ch-e ch-b))
(def ^:private ^:const ^:double ch-bc-da (- (* ch-b ch-c) (* ch-d ch-a)))
(def ^:private ^:const ^:double ch-bc-da+ed-eb-r (/ (+ ch-bc-da ch-ed (- ch-eb))))

(defn to-Cubehelix
  "RGB -> Cubehelix

  D3 version
  
  Ranges:

  * H: 0.0 - 360.0
  * S: 0.0 - 4.61
  * L: 0.0 - 1.0"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        r (/ (.x c) 255.0)
        g (/ (.y c) 255.0)
        b (/ (.z c) 255.0)
        l (* ch-bc-da+ed-eb-r (+ (* ch-bc-da b) (* ch-ed r) (- (* ch-eb g))))
        bl (- b l)
        k (/ (- (* ch-e (- g l)) (* ch-c bl)) ch-d)
        s (/ (m/sqrt (+ (* k k) (* bl bl)))
             (* ch-e l (- 1.0 l)))]
    (if (Double/isNaN s)
      (Vec4. 0.0 0.0 l (.w c))
      (let [h (- (* (m/atan2 k bl) m/rad-in-deg) 120.0)]
        (Vec4. (if (neg? h) (+ h 360.0) h) s l (.w c))))))

(defn to-Cubehelix*
  "RGB -> Cubehelix, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-Cubehelix c)]
    (Vec4. (m/mnorm (.x cc) 0.0 359.9932808311505 0.0 255.0)
           (m/mnorm (.y cc) 0.0 4.61438686803972 0.0 255.0)
           (* 255.0 (.z cc))
           (.w cc))))

(defn from-Cubehelix
  "Cubehelix -> RGB

  For ranges, see [[to-Cubehelix]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        h (* (+ (.x c) 120.0) m/deg-in-rad)
        l (.z c)
        a (* (.y c) l (- 1.0 l))
        cosh (m/cos h)
        sinh (m/sin h)]
    (Vec4. (* 255.0 (+ l (* a (+ (* ch-a cosh) (* ch-b sinh)))))
           (* 255.0 (+ l (* a (+ (* ch-c cosh) (* ch-d sinh)))))
           (* 255.0 (+ l (* a ch-e cosh)))
           (.w c))))

(defn from-Cubehelix*
  "Cubehelix -> RGB, normalized"
  {:metadoc/categories meta-conv}
  ^Vec4 [c] 
  (let [^Vec4 c (pr/to-color c)
        cc (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 359.9932808311505)
                  (m/mnorm (.y c) 0.0 255.0 0.0 4.61438686803972)
                  (/ (.z c) 255.0)
                  (.w c))]
    (from-Cubehelix cc)))

;; OSA
;; https://github.com/nschloe/colorio/blob/master/colorio/_osa.py

(defn to-OSA
  "OSA-UCS -> RGB

  https://github.com/nschloe/colorio/blob/master/colorio/_osa.py

  Ranges:

  * L: -13.5 - 7.14
  * g (around): -20.0 - 12.0 + some extreme values
  * j (around): -20.0 - 14.0 + some extreme values

  Note that for some cases (like `(to-OSA [18 7 4])`) function returns some extreme values."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [xyz (to-XYZ c)
        X (.x xyz)
        Y (.y xyz)
        Z (.z xyz)
        sum-xyz (+ X Y Z)
        x (if (zero? X) 0.0 (/ X sum-xyz))
        y (if (zero? Y) 0.0 (/ Y sum-xyz))
        K (+ (* 4.4934 x x)
             (* 4.3034 y y)
             (* -4.276 x y)
             (* -1.3744 x)
             (* -2.5643 y)
             1.8103)
        Y0 (* Y K)
        Y03 (- (m/cbrt Y0) m/TWO_THIRD)
        L' (* 5.9 (+ Y03 (* 0.042 (m/cbrt (- Y0 30.0)))))
        C (/ L' (* 5.9 Y03))
        R3 (m/cbrt (+ (* 0.7990 X) (* 0.4194 Y) (* -0.1648 Z)))
        G3 (m/cbrt (+ (* -0.4493 X) (* 1.3265 Y) (* 0.0927 Z)))
        B3 (m/cbrt (+ (* -0.1149 X) (* 0.3394 Y) (* 0.7170 Z)))
        a (+ (* -13.7 R3) (* 17.7 G3) (* -4.0 B3))
        b (+ (* 1.7 R3) (* 8.0 G3) (* -9.7 B3))
        L (/ (- L' 14.3993) m/SQRT2)]
    (Vec4. L (* C a) (* C b) (.w xyz))))

(defonce ^:private ^:const ^double OSA-v (* 0.042 0.042 0.042))
(defonce ^:private ^:const ^double OSA-30v (* 30.0 OSA-v))
(defonce ^:private ^:const ^double OSA-a (- (inc OSA-v)))
(defonce ^:private ^:const ^double OSA-detr (/ -139.68999999999997))
(defonce ^:private ^:const ^double OSA-omega 4.957506551095124)

(deftype OSAFDF [^double fomega ^double dfomega ^double X ^double Y ^double Z])

(defn from-OSA
  "RGB -> OSA-UCS

  For ranges, see [[to-OSA]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 col (pr/to-color c)
        L (.x col)
        g (.y col)
        j (.z col)
        L' (+ (* m/SQRT2 L) 14.3993)
        u (+ (/ L' 5.9) m/TWO_THIRD)
        a OSA-a
        b (* 3.0 u)
        c (* -3.0 u u)
        d (+ (* u u u) OSA-30v)
        p (/ (- (* 3.0 a c)
                (* b b))
             (* 3.0 a a))
        aa27 (* 27.0 a a)
        q (/ (+ (* 2.0 b b b)
                (* -9.0 a b c)
                (* aa27 d))
             (* aa27 a))
        q2 (* 0.5 q)
        s (m/sqrt (+ (m/sq q2)
                     (m/pow3 (* p m/THIRD))))
        t (- (+ (m/cbrt (- s q2))
                (m/cbrt (- (- q2) s)))
             (/ b (* 3.0 a)))
        Y0 (* t t t)
        C (/ L' (* 5.9 (- t m/TWO_THIRD)))
        a (/ g C)
        b (/ j C)
        ap (* OSA-detr (+ (* -9.7 a) (* 4.0 b)))
        bp (* OSA-detr (+ (* -8.0 a) (* 17.7 b)))
        f-df (fn [^double omega]
               (let [cbrt-R omega
                     cbrt-G (+ omega ap)
                     cbrt-B (+ omega bp)
                     R (m/pow3 cbrt-R)
                     G (m/pow3 cbrt-G)
                     B (m/pow3 cbrt-B)
                     X (+ (* 1.06261827e+00 R) (* -4.12091749e-01 G) (* 2.97517985e-01 B))
                     Y (+ (* 3.59926645e-01 R) (* 6.40072108e-01 G) (* -2.61830489e-05 B))
                     Z (+ (* -8.96301459e-05 R) (* -3.69023452e-01 G) (* 1.44239010e+00 B))
                     sum-xyz (+ X Y Z)
                     x (if (zero? X) 0.0 (/ X sum-xyz))
                     y (if (zero? Y) 0.0 (/ Y sum-xyz))
                     K (+ (* 4.4934 x x)
                          (* 4.3034 y y)
                          (* -4.276 x y)
                          (* -1.3744 x)
                          (* -2.5643 y)
                          1.8103)
                     f (- (* Y K) Y0)
                     dR (* 3.0 (m/sq cbrt-R))
                     dG (* 3.0 (m/sq cbrt-G))
                     dB (* 3.0 (m/sq cbrt-B))
                     dX (+ (* 1.06261827e+00 dR) (* -4.12091749e-01 dG) (* 2.97517985e-01 dB))
                     dY (+ (* 3.59926645e-01 dR) (* 6.40072108e-01 dG) (* -2.61830489e-05 dB))
                     dZ (+ (* -8.96301459e-05 dR) (* -3.69023452e-01 dG) (* 1.44239010e+00 dB))
                     dsum-xyz (+ dX dY dZ)
                     dx (if (zero? X) 0.0 (/ (- (* dX sum-xyz) (* X dsum-xyz)) (* sum-xyz sum-xyz)))
                     dy (if (zero? Y) 0.0 (/ (- (* dY sum-xyz) (* Y dsum-xyz)) (* sum-xyz sum-xyz)))
                     dK (+ (* 4.4934 2.0 x dx)
                           (* 4.3034 2.0 y dy)
                           (* -4.276 (+ (* dx y) (* x dy)))
                           (* -1.3744 dx)
                           (* -2.5643 dy))
                     df (+ (* dY K) (* Y dK))]
                 (OSAFDF. f df X Y Z)))]
    (loop [iter (int 0)
           omega OSA-omega]
      (let [^OSAFDF res (f-df omega)]
        (if (and (< iter 20)
                 (> (.fomega res) 1.0e-10))
          (recur (inc iter)
                 (- omega (/ (.fomega res)
                             (.dfomega res))))
          (from-XYZ (Vec4. (.X res) (.Y res) (.Z res) (.w col))))))))

(defn to-OSA*
  "OSA-UCS -> RGB, normalized

  Note that due to some extreme outliers, normalization is triple cubic root for `g` and `j`."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [c (to-OSA c)]
    (Vec4. (m/mnorm (.x c) -13.507581921540849 7.1379048733958435 0.0 255.0)
           (m/mnorm (m/cbrt (m/cbrt (m/cbrt (.y c)))) -1.4057783957453063 1.4175468647969818 0.0 255.0)
           (m/mnorm (m/cbrt (m/cbrt (m/cbrt (.z c)))) -1.4073863219389389 1.4228002556769352 0.0 255.0)
           (.w c))))

(defn from-OSA*
  "OSA-UCS -> RGB, normalized

  Note that due to some extreme outliers, normalization is triple cubic power for `g` and `j`."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-OSA (Vec4. (m/mnorm (.x c) 0.0 255.0 -13.507581921540849 7.1379048733958435)
                     (m/pow3 (m/pow3 (m/pow3 (m/mnorm (.y c) 0.0 255.0 -1.4057783957453063 1.4175468647969818))))
                     (m/pow3 (m/pow3 (m/pow3 (m/mnorm (.z c) 0.0 255.0 -1.4073863219389389 1.4228002556769352))))
                     (.w c)))))

;; ### Grayscale

(defn to-Gray
  "RGB -> Grayscale"
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        l (luma c)]
    (Vec4. l l l (.w c))))

(def ^{:doc "RGB -> Grayscale" :metadoc/categories meta-conv} to-Gray* to-Gray)

;; do nothing in reverse
(def ^{:doc "RGB -> Grayscale" :metadoc/categories meta-conv} from-Gray to-Gray)
(def ^{:doc "RGB -> Grayscale" :metadoc/categories meta-conv} from-Gray* to-Gray)

;; Just for a case "do nothing"
(def ^{:doc "Alias for [[to-color]]" :metadoc/categories meta-conv} to-RGB pr/to-color)
(def ^{:doc "Alias for [[to-color]]" :metadoc/categories meta-conv} from-RGB pr/to-color)
(def ^{:doc "Alias for [[to-color]]" :metadoc/categories meta-conv} to-RGB* pr/to-color)
(def ^{:doc "Alias for [[to-color]]" :metadoc/categories meta-conv} from-RGB* pr/to-color)

;; List of all color spaces with functions
(def ^{:doc "Map of all color spaces functions.

* key - name as keyword
* value - vector with functions containing to-XXX and from-XXX converters."
     :metadoc/categories meta-conv}
  colorspaces {:CMY   [to-CMY from-CMY]
               :OHTA  [to-OHTA from-OHTA]
               :XYZ   [to-XYZ from-XYZ]
               :XYB   [to-XYB from-XYB]
               :RYB   [to-RYB from-RYB]
               :Yxy   [to-Yxy from-Yxy]
               :LMS   [to-LMS from-LMS]
               :IPT   [to-IPT from-IPT]
               :LUV   [to-LUV from-LUV]
               :LAB   [to-LAB from-LAB]
               :Oklab [to-Oklab from-Oklab]
               :JAB   [to-JAB from-JAB]
               :HunterLAB  [to-HunterLAB from-HunterLAB]
               :LCH   [to-LCH from-LCH]
               :JCH   [to-JCH from-JCH]
               :HCL   [to-HCL from-HCL]
               :HSB   [to-HSB from-HSB]
               :HSI   [to-HSI from-HSI]
               :HSL   [to-HSL from-HSL]
               :HSV   [to-HSV from-HSV]
               :HWB   [to-HWB from-HWB]
               :GLHS  [to-GLHS from-GLHS]
               :YPbPr [to-YPbPr from-YPbPr]
               :YDbDr [to-YDbDr from-YDbDr]
               :YCbCr [to-YCbCr from-YCbCr]
               :YCgCo [to-YCgCo from-YCgCo]
               :YUV   [to-YUV from-YUV]
               :YIQ   [to-YIQ from-YIQ]
               :Gray  [to-Gray from-Gray]
               :sRGB  [to-sRGB from-sRGB]
               :Cubehelix [to-Cubehelix from-Cubehelix]
               :OSA [to-OSA from-OSA]
               :RGB   [to-color to-color]})

(def ^{:doc "Map of all color spaces functions (normalized).

* key - name as keyword
* value - vector with functions containing to-XXX* and from-XXX* converters."
     :metadoc/categories meta-conv}
  colorspaces* {:CMY   [to-CMY* from-CMY*]
                :OHTA  [to-OHTA* from-OHTA*]
                :XYZ   [to-XYZ* from-XYZ*]
                :XYB   [to-XYB* from-XYB*]
                :RYB   [to-RYB* from-RYB*]
                :Yxy   [to-Yxy* from-Yxy*]
                :LMS   [to-LMS* from-LMS*]
                :IPT   [to-IPT* from-IPT*]
                :LUV   [to-LUV* from-LUV*]
                :LAB   [to-LAB* from-LAB*]
                :Oklab [to-Oklab* from-Oklab*]
                :JAB   [to-JAB* from-JAB*]
                :HunterLAB  [to-HunterLAB* from-HunterLAB*]
                :LCH   [to-LCH* from-LCH*]
                :JCH   [to-JCH* from-JCH*]
                :HCL   [to-HCL* from-HCL*]
                :HSB   [to-HSB* from-HSB*]
                :HSI   [to-HSI* from-HSI*]
                :HSL   [to-HSL* from-HSL*]
                :HSV   [to-HSV* from-HSV*]
                :HWB   [to-HWB* from-HWB*]
                :GLHS  [to-GLHS* from-GLHS*]
                :YPbPr [to-YPbPr* from-YPbPr*]
                :YDbDr [to-YDbDr* from-YDbDr*]
                :YCbCr [to-YCbCr* from-YCbCr*]
                :YCgCo [to-YCgCo* from-YCgCo*]
                :YUV   [to-YUV* from-YUV*]
                :YIQ   [to-YIQ* from-YIQ*]
                :Gray  [to-Gray from-Gray]
                :sRGB  [to-sRGB* from-sRGB*]
                :Cubehelix [to-Cubehelix* from-Cubehelix*]
                :OSA [to-OSA* from-OSA*]
                :RGB   [to-color to-color]})

;; List of color spaces names
(def ^{:doc "List of all color space names." :metadoc/categories meta-conv} colorspaces-list (sort (keys colorspaces)))

(defn set-channel
  "Set chosen channel with given value. Works with any color space."
  {:metadoc/categories meta-ops}
  (^Vec4 [col colorspace ^long channel ^double val]
   (let [[to from] (colorspaces colorspace)
         ^Vec4 c (to col)]
     (clamp (from (case channel
                    0 (Vec4. val (.y c) (.z c) (.w c))
                    1 (Vec4. (.x c) val (.z c) (.w c))
                    2 (Vec4. (.x c) (.y c) val (.w c))
                    3 (Vec4. (.x c) (.y c) (.z c) val)
                    c)))))
  (^Vec4 [col ^long channel ^double val]
   (let [^Vec4 c (pr/to-color col)]
     (clamp (case channel
              0 (Vec4. val (.y c) (.z c) (.w c))
              1 (Vec4. (.x c) val (.z c) (.w c))
              2 (Vec4. (.x c) (.y c) val (.w c))
              3 (Vec4. (.x c) (.y c) (.z c) val)
              c)))))

(defn get-channel
  "Get chosen channel. Works with any color space."
  {:metadoc/categories meta-ops}
  (^double [col colorspace ^long channel]
   (let [to (first (colorspaces colorspace))]
     (get-channel (to col) channel)))
  (^double [col ^long channel]
   (case channel
     0 (ch0 col)
     1 (ch1 col)
     2 (ch2 col)
     3 (alpha col)
     ##NaN)))

(defn color-converter
  "Create function which converts provided color from `cs` color space using `ch-scale` as maximum value. (to simulate Processing `colorMode` fn).

  Arity:

  * 1 - returns from-XXX* function
  * 2 - sets the same maximum value for each channel
  * 3 - sets individual maximum value without alpha, which is set to 0-255 range
  * 4 - all channels have it's own individual maximum value."
  {:metadoc/categories meta-conv}
  ([cs ch1-scale ch2-scale ch3-scale ch4-scale]
   (let [colorspace-fn (second (colorspaces* cs))]
     (fn [v] 
       (let [^Vec4 v (pr/to-color v)
             ch1 (* 255.0 (/ (.x v) ^double ch1-scale))
             ch2 (* 255.0 (/ (.y v) ^double ch2-scale))
             ch3 (* 255.0 (/ (.z v) ^double ch3-scale))
             ch4 (* 255.0 (/ (.w v) ^double ch4-scale))]
         (colorspace-fn (v/fmap (Vec4. ch1 ch2 ch3 ch4) clamp255))))))
  ([cs ch1-scale ch2-scale ch3-scale] (color-converter cs ch1-scale ch2-scale ch3-scale 255.0))
  ([cs ch-scale] (color-converter cs ch-scale ch-scale ch-scale ch-scale))
  ([cs] (second (colorspaces* cs))))

;; black body

(defn- kelvin-red
  ^double [^double k ^double lnk]
  (if (< k 0.65)
    255.0
    (min 255.0 (* 255.0 (from-linear (+ 0.32068362618584273
                                        (* 0.19668730877673762 (m/pow (+ -0.21298613432655075 k) -1.5139012907556737))
                                        (* -0.013883432789258415 lnk)))))))

(defn- kelvin-green1
  ^double [^double k ^double lnk]
  (let [eek (+ k -0.44267061967913873)]
    (max 0.0 (* 255.0 (from-linear (+ 1.226916242502167
                                      (* -1.3109482654223614 eek eek eek (m/exp (* eek -5.089297600846147)))
                                      (* 0.6453936305542096 lnk)))))))

(defn- kelvin-green2
  ^double [^double k ^double lnk]
  (* 255.0 (from-linear (+ 0.4860175851734596
                           (* 0.1802139719519286 (m/pow (+ -0.14573069517701578 k) -1.397716496795082))
                           (* -0.00803698899233844 lnk)))))


(defn- kelvin-green
  ^double [^double k ^double lnk]
  (cond
    (< k 0.08) 0.0
    (< k 0.655)(kelvin-green1 k lnk)
    :else (kelvin-green2 k lnk)))

(defn- kelvin-blue
  ^double [^double k ^double lnk]
  (cond
    (< k 0.19) 0.0
    (> k 0.66) 255.0
    :else (let [eek (+ k -1.1367244820333684)]
            (m/constrain (* 255.0 (from-linear (+ 1.677499032830161
                                                  (* -0.02313594016938082 eek eek eek
                                                     (m/exp (* eek -4.221279555918655)))
                                                  (* 1.6550275798913296 lnk)))) 0.0 255.0))))

(def ^:private temperature-name-to-K
  {:candle 1800.0
   :sunrise 2500.0
   :sunset 2500.0
   :lightbulb 2900.0
   :morning 3500.0
   :moonlight 4000.0
   :midday 5500.0
   :cloudy-sky 6500.0
   :blue-sky 10000.0
   :warm 2900.0
   :white 4250.0
   :sunlight 4800.0
   :cool 7250.0})

(def temperature-names (sort (keys temperature-name-to-K)))

(defn temperature
  "Color representing given black body temperature `t` in Kelvins (or name as keyword).

  Reference: CIE 1964 10 degree CMFs
  
  Using improved interpolation functions.

  Possible temperature names: `:candle`, `:sunrise`, `:sunset`, `:lightbulb`, `:morning`, `:moonlight`, `:midday`, `:cloudy-sky`, `:blue-sky`, `:warm`, `:cool`, `:white`, `:sunlight`"
  {:metadoc/categories #{:pal}}
  ^Vec4 [t]
  (let [k (* 0.0001 ^double (get temperature-name-to-K t t))
        lnk (m/ln k)]
    (Vec4. (kelvin-red k lnk)
           (kelvin-green k lnk)
           (kelvin-blue k lnk)
           255.0)))

;; http://iquilezles.org/www/articles/palettes/palettes.htm

(defn- cosine-coefficients
  "Computes coefficients defining a cosine gradient between
  the two given colors. The colors can be in any color space,
  but the resulting gradient will always be computed in RGB.

  amp = (R1 - R2) / 2
  dc = R1 - amp
  freq = -0.5

  Code borrowed from thi.ng/color"
  ([c1 c2]
   (let [c1 (v/vec3 (pr/red c1) (pr/green c1) (pr/blue c1))
         c2 (v/vec3 (pr/red c2) (pr/green c2) (pr/blue c2))
         amp (v/mult (v/sub c1 c2) 0.5)
         offset (v/sub c1 amp)]
     [(v/div offset 255.0)
      (v/div amp 255.0)
      (v/vec3 -0.500 -0.500 -0.500)
      (v/vec3)])))

(defn iq-gradient
  "Create gradient generator function with given parametrization or two colors.

  See http://iquilezles.org/www/articles/palettes/palettes.htm and https://github.com/thi-ng/color/blob/master/src/gradients.org#gradient-coefficient-calculation

  Parameters should be `Vec3` type."
  {:metadoc/categories #{:gr}}
  ([c1 c2]
   (apply iq-gradient (cosine-coefficients c1 c2)))
  ([a b c d]
   (fn ^Vec4 [^double t]
     (let [^Vec3 cc (apply v/vec3 (-> (->> (v/mult c t)
                                           (v/add d))
                                      (v/mult m/TWO_PI)
                                      (v/fmap #(m/cos %))
                                      (v/emult b)
                                      (v/add a)))]
       (-> (Vec4. (.x cc) (.y cc) (.z cc) 1.0)
           (v/mult 255.0)
           (v/fmap clamp255))))))

;; --------------

(defonce ^:private paletton-base-data
  (let [s (fn ^double [^double e ^double t ^double n] (if (== n -1.0) e
                                                          (+ e (/ (- t e) (inc n)))))
        i (fn ^double [^double e ^double t ^double n] (if (== n -1.0) t
                                                          (+ t (/ (- e t) (inc n)))))
        d120 {:a [1.0 1.0]
              :b [1.0 1.0]
              :f (fn ^double [^double e]
                   (if (zero? e) -1.0
                       (* 0.5 (m/tan (* m/HALF_PI (/ (- 120.0 e) 120.0))))))
              :fi (fn ^double [^double e]
                    (if (== e -1.0) 0.0
                        (- 120.0 (* 2.0 (/ (* (m/atan (/ e 0.5)) 120.0) m/PI)))))
              :g s
              :rgb (fn [e n r] (Vec4. e n r 255.0))}
        d180 {:a [1.0 1.0]
              :b [1.0 0.8]
              :f (fn ^double [^double e]
                   (if (== e 180.0) -1.0
                       (* 0.5 (m/tan (* m/HALF_PI (/ (- e 120.0) 60.0))))))
              :fi (fn ^double [^double e]
                    (if (== e -1.0) 180.0
                        (+ 120.0 (* 2.0 (/ (* (m/atan (/ e 0.5)) 60.0) m/PI)))))
              :g i
              :rgb (fn [e n r] (Vec4. n e r 255.0))}
        d210 {:a [1.0 0.8]
              :b [1.0 0.6]
              :f (fn ^double [^double e]
                   (if (== e 180.0) -1.0
                       (* 0.75 (m/tan (* m/HALF_PI (/ (- 210.0 e) 30.0))))))
              :fi (fn ^double [^double e]
                    (if (== e -1.0) 180.0
                        (- 210.0 (* 2.0 (/ (* (m/atan (/ e 0.75)) 30.0) m/PI)))))
              :g s
              :rgb (fn [e n r] (Vec4. r e n 255.0))}
        d255 {:a [1.0 0.6]
              :b [0.85 0.7]
              :f (fn ^double [^double e]
                   (if (== e 255.0) -1.0
                       (* 1.33 (m/tan (* m/HALF_PI (/ (- e 210.0) 45.0))))))
              :fi (fn ^double [^double e]
                    (if (== e -1.0) 255.0
                        (+ 210.0 (* 2.0 (/ (* (m/atan (/ e 1.33)) 45.0) m/PI)))))
              :g i
              :rgb (fn [e n r] (Vec4. r n e 255.0))}
        d315 {:a [0.85 0.7]
              :b [1.0 0.65]
              :f (fn ^double [^double e]
                   (if (== e 255.0) -1.0
                       (* 1.33 (m/tan (* m/HALF_PI (/ (- 315.0 e) 60.0))))))
              :fi (fn ^double [^double e]
                    (if (== e -1.0) 255.0
                        (- 315.0 (* 2.0 (/ (* (m/atan (/ e 1.33)) 60.0) m/PI)))))
              :g s
              :rgb (fn [e n r] (Vec4. n r e 255.0))}
        d360 {:a [1.0 0.65]
              :b [1.0 1.0]
              :f (fn ^double [^double e]
                   (if (zero? e) -1.0
                       (* 1.33 (m/tan (* m/HALF_PI (/ (- e 315.0) 45.0))))))
              :fi (fn ^double [^double e]
                    (if (== e -1.0) 0.0
                        (+ 315.0 (* 2.0 (/ (* (m/atan (/ e 1.33)) 45.0) m/PI)))))
              :g i
              :rgb (fn [e n r] (Vec4. e r n 255.0))}]
    (fn [^double h]
      (condp clojure.core/> h
        120.0 d120
        180.0 d180
        210.0 d210
        255.0 d255
        315.0 d315
        360.0 d360))))

(defn- paletton-hsv-to-rgb
  "Paletton version of HSV to RGB converter"
  [^double hue ^double ks ^double kv]
  (let [ks (m/constrain ks 0.0 2.0)
        kv (m/constrain kv 0.0 2.0)
        h (mod hue 360.0)
        upd (fn ^double [^double e ^double t] (if (<= t 1.0)
                                                (* e t)
                                                (+ e (* (- 1.0 e) (dec t)))))
        {:keys [a b f g rgb]} (paletton-base-data h)
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

(defn hue-paletton
  "Convert color to paletton HUE (which is different than hexagon or polar conversion)."
  {:metadoc/categories meta-ops}
  (^double [^double r ^double g ^double b]
   (if (== r g b)
     0.0
     (let [f (max r g b)
           p (min r g b)
           [^double l i] (if (== f r)
                           (if (== p b)
                             [g (:fi (paletton-base-data 119.0))]
                             [b (:fi (paletton-base-data 359.0))])
                           (if (== f g)
                             (if (== p r)
                               [b (:fi (paletton-base-data 209.0))]
                               [r (:fi (paletton-base-data 179.0))])
                             (if (== p r)
                               [g (:fi (paletton-base-data 254.0))]
                               [r (:fi (paletton-base-data 314.0))])))
           ;; d (/ (- f p) f) ;; saturation
           ;; v (/ f 255.0)   ;; value
           s (i (if (== l p) -1.0
                    (/ (- f l) (- l p))))]
       s)))
  ([c] (let [cc (to-color c)]
         (hue-paletton (.x cc) (.y cc) (.z cc)))))

;; List of paletton presets
(defonce ^:private paletton-presets
  {:pale-light          [[0.24649 1.78676] [0.09956 1.95603] [0.17209 1.88583] [0.32122 1.65929] [0.39549 1.50186]]
   :pastels-bright      [[0.65667 1.86024] [0.04738 1.99142] [0.39536 1.89478] [0.90297 1.85419] [1.86422 1.8314]]
   :shiny               [[1.00926 2] [0.3587 2] [0.5609 2] [2 0.8502] [2 0.65438]]
   :pastels-lightest    [[0.34088 1.09786] [0.13417 1.62645] [0.23137 1.38072] [0.45993 0.92696] [0.58431 0.81098]]
   :pastels-very-light  [[0.58181 1.32382] [0.27125 1.81913] [0.44103 1.59111] [0.70192 1.02722] [0.84207 0.91425]]
   :full                [[1 1] [0.61056 1.24992] [0.77653 1.05996] [1.06489 0.77234] [1.25783 0.60685]]
   :pastels-light       [[0.37045 0.90707] [0.15557 1.28367] [0.25644 1.00735] [0.49686 0.809] [0.64701 0.69855]]
   :pastels-med         [[0.66333 0.8267] [0.36107 1.30435] [0.52846 0.95991] [0.78722 0.70882] [0.91265 0.5616]]
   :darker              [[0.93741 0.68672] [0.68147 0.88956] [0.86714 0.82989] [1.12072 0.5673] [1.44641 0.42034]]
   :pastels-mid-pale    [[0.38302 0.68001] [0.15521 0.98457] [0.26994 0.81586] [0.46705 0.54194] [0.64065 0.44875]]
   :pastels             [[0.66667 0.66667] [0.33333 1] [0.5 0.83333] [0.83333 0.5] [1 0.33333]]
   :dark-neon           [[0.94645 0.59068] [0.99347 0.91968] [0.93954 0.7292] [1.01481 0.41313] [1.04535 0.24368]]
   :pastels-dark        [[0.36687 0.39819] [0.25044 0.65561] [0.319 0.54623] [0.55984 0.37953] [0.70913 0.3436]]
   :pastels-very-dark   [[0.60117 0.41845] [0.36899 0.59144] [0.42329 0.44436] [0.72826 0.35958] [0.88393 0.27004]]
   :dark                [[1.31883 0.40212] [0.9768 0.25402] [1.27265 0.30941] [1.21289 0.60821] [1.29837 0.82751]]
   :pastels-mid-dark    [[0.26952 0.22044] [0.23405 0.52735] [0.23104 0.37616] [0.42324 0.20502] [0.54424 0.18483]]
   :pastels-darkest     [[0.53019 0.23973] [0.48102 0.50306] [0.50001 0.36755] [0.6643 0.32778] [0.77714 0.3761]]
   :darkest             [[1.46455 0.21042] [0.99797 0.16373] [0.96326 0.274] [1.56924 0.45022] [1.23016 0.66]]
   :almost-black        [[0.12194 0.15399] [0.34224 0.50742] [0.24211 0.34429] [0.31846 0.24986] [0.52251 0.33869]]
   :almost-gray-dark    [[0.10266 0.24053] [0.13577 0.39387] [0.11716 0.30603] [0.14993 0.22462] [0.29809 0.19255]]
   :almost-gray-darker  [[0.07336 0.36815] [0.18061 0.50026] [0.09777 0.314] [0.12238 0.25831] [0.14388 0.1883]]
   :almost-gray-mid     [[0.07291 0.59958] [0.19602 0.74092] [0.10876 0.5366] [0.15632 0.48229] [0.20323 0.42268]]
   :almost-gray-lighter [[0.06074 0.82834] [0.14546 0.97794] [0.10798 0.76459] [0.15939 0.68697] [0.22171 0.62926]]
   :almost-gray-light   [[0.03501 1.59439] [0.23204 1.10483] [0.14935 1.33784] [0.07371 1.04897] [0.09635 0.91368]]})

;; List of preset names
(def ^{:doc "Paletton presets names"
       :metadoc/categories #{:pal}}
  paletton-presets-list (keys paletton-presets))

(defn- paletton-monochromatic-palette
  "Create monochromatic palette for given `hue` (0-360) and `preset` values."
  [hue preset]
  (mapv (fn [[ks kv]] (paletton-hsv-to-rgb hue ks kv)) preset))

(defmulti paletton
  "Create [paletton](http://paletton.com/) palette.

  Input:

  * type - one of: `:monochromatic` (one hue), `:triad` (three hues), `:tetrad` (four hues)
  * hue - paletton version of hue (use [[hue-paletton]] to get hue value).
  * configuration as a map

  Configuration consist:

  * `:preset` - one of [[paletton-presets-list]], default `:full`.
  * `:compl` - generate complementary color?, default `false`. Works with `:monochromatic` and `:triad`
  * `:angle` - hue angle for additional colors for `:triad` and `:tetrad`.
  * `:adj` - for `:triad` only, generate adjacent (default `true`) values or not."
  {:metadoc/categories #{:pal}}
  (fn [m _ & _] m))

(defmethod paletton :monochromatic [_ hue & conf]
  (let [{compl :compl 
         preset :preset
         :or {compl false
              preset :full}} (first conf)
        ppreset (if (keyword? preset) (paletton-presets preset) preset)
        p (paletton-monochromatic-palette hue ppreset)]
    (if compl (vec (concat p (paletton-monochromatic-palette (+ ^double hue 180.0) ppreset))) p)))

(defmethod paletton :triad [_ hue & conf]
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
        p1 (paletton-monochromatic-palette hue ppreset)
        p2 (paletton-monochromatic-palette hue1 ppreset)
        p3 (paletton-monochromatic-palette hue2 ppreset)
        p (vec (concat p1 p2 p3))]
    (if compl (vec (concat p (paletton-monochromatic-palette chue ppreset))) p)))

(defmethod paletton :tetrad [_ hue & conf]
  (let [{preset :preset
         ^double angle :angle
         :or {preset :full
              angle 30.0}} (first conf)
        p1 (paletton :monochromatic hue {:preset preset :compl true})
        p2 (paletton :monochromatic (+ angle ^double hue) {:preset preset :compl true})]
    (vec (concat p1 p2))))

;; ## Additional functions

(defn delta-c
  "Delta C* distance"
  {:metadoc/categories #{:dist}}
  [c1 c2]
  (let [^Vec4 c1 (to-LAB c1)
        ^Vec4 c2 (to-LAB c2)]
    (- (m/hypot-sqrt (.y c2) (.z c2))
       (m/hypot-sqrt (.y c1) (.z c1)))))

(defn delta-h
  "Delta H* distance"
  {:metadoc/categories #{:dist}}
  [c1 c2]
  (let [c1 (to-LAB c1)
        c2 (to-LAB c2)
        xde (- (m/hypot-sqrt (.y c2) (.z c2))
               (m/hypot-sqrt (.y c1) (.z c1)))]
    (m/safe-sqrt (- (+ (m/sq (- (.y c1) (.y c2)))
                       (m/sq (- (.z c1) (.z c2))))
                    (* xde xde)))))

(defn- euclidean-
  "Euclidean distance between colors"
  {:metadoc/categories #{:dist}}
  (^double [cs-conv c1 c2]
   (v/dist (cs-conv (pr/to-color c1))
           (cs-conv (pr/to-color c2))))
  (^double [c1 c2]
   (v/dist (pr/to-color c1) (pr/to-color c2))))

(def ^{:metadoc/categories #{:dist}
       :doc "Delta E CIE distance (euclidean in LAB colorspace."}
  delta-e-cie (partial euclidean- to-LAB))

(def ^{:metadoc/categories #{:dist}
       :doc "Euclidean distance in RGB"}
  euclidean euclidean-)

(defn delta-e-cmc
  "Delta E CMC distance.

  Parameters `l` and `c` defaults to 1.0. Other common settings is `l=2.0` and `c=1.0`."
  {:metadoc/categories #{:dist}}
  (^double [c1 c2] (delta-e-cmc c1 c2 1.0 1.0))
  (^double [c1 c2 ^double l ^double c ]
   (let [^Vec4 c1 (to-LAB c1)
         ^Vec4 c2 (to-LAB c2)
         L1 (.x c1)
         L2 (.x c2)
         a1 (.y c1)
         a2 (.y c2)
         b1 (.z c1)
         b2 (.z c2)
         H (m/degrees (m/atan2 b1 a1))
         H1 (if (neg? H) (+ H 360.0) H)
         C1 (m/hypot-sqrt a1 b1)
         C2 (m/hypot-sqrt a2 b2)
         dC (- C1 C2)
         dL (- L1 L2)
         da (- a1 a2)
         db (- b1 b2)
         dH (- (+ (m/sq da) (m/sq db)) (m/sq dC))
         C12 (* C1 C1)
         C14 (* C12 C12)
         F (m/sqrt (/ C14 (+ C14 1900.0)))
         T (if (<= 164.0 H1 345.0)
             (+ 0.56 (m/abs (* 0.2 (m/cos (m/radians (+ H1 168.0))))))
             (+ 0.36 (m/abs (* 0.4 (m/cos (m/radians (+ H1 35.0)))))))
         SL (if (< L1 16.0)
              0.511
              (/ (* 0.040975 L1)
                 (inc (* 0.01765 L1))))
         SC (+ 0.638 (/ (* 0.0638 C1)
                        (inc (* 0.0131 C1))))
         SH (* SC (inc (* F (dec T))))]
     (m/sqrt (+ (m/sq (/ dL (* l SL))) (m/sq (/ dC (* c SC))) (/ dH (m/sq SH)))))))

(defn delta-e-jab
  "Delta e calculated in JAB color space."
  {:metadoc/categories #{:dist}}
  [c1 c2]
  (let [^Vec4 c1 (to-JAB c1)
        ^Vec4 c2 (to-JAB c2)
        J1 (.x c1)
        J2 (.x c2)
        a1 (.y c1)
        a2 (.y c2)
        b1 (.z c1)
        b2 (.z c2)
        C1 (m/hypot-sqrt a1 b1)
        C2 (m/hypot-sqrt a2 b2)
        h1 (m/atan (/ b1 a1))
        h2 (m/atan (/ b2 a2))]
    (m/hypot-sqrt (- J2 J1)
                  (- C2 C1)
                  (* 2.0 (m/sqrt (* C1 C2)) (m/sin (* 0.5 (- h2 h1)))))))

(defn contrast-ratio
  "WCAG contrast ratio.

  Based on YUV luma."
  {:metadoc/categories #{:dist}}
  [c1 c2]
  (let [l1 (/ (relative-luma c1) 255.0)
        l2 (/ (relative-luma c2) 255.0)]
    (if (> l1 l2)
      (/ (+ l1 0.05) (+ l2 0.05))
      (/ (+ l2 0.05) (+ l1 0.05)))))

(defn- nd-lab-interval
  "Minimal difference values"
  [^double s ^double p]
  (v/mult (Vec3. (+ 10.16 (/ 1.5 s))
                 (+ 10.68 (/ 3.08 s))
                 (+ 10.70 (/ 5.74 s))) p))

(defn noticable-different?
  "Returns noticable difference (true/false) between colors.

  Defined in: https://research.tableau.com/sites/default/files/2014CIC_48_Stone_v3.pdf

  Implementation from: https://github.com/connorgr/d3-jnd/blob/master/src/jnd.js"
  {:metadoc/categories #{:dist}}
  ([c1 c2] (noticable-different? c1 c2 0.1 0.5))
  ([c1 c2 ^double s ^double p ]
   (let [c1 (to-LAB c1)
         c2 (to-LAB c2)
         ^Vec4 diff (v/abs (v/sub c1 c2))
         ^Vec3 nd (nd-lab-interval s p)]
     (or (>= (.x diff) (.x nd))
         (>= (.y diff) (.y nd))
         (>= (.z diff) (.z nd))))))

(defn nearest-color
  "Find nearest color from a set. Input: distance function (default euclidean), list of target colors and source color."
  {:metadoc/categories #{:dist}}
  ([pal c dist-fn]
   (let [s (count pal)
         c (pr/to-color c)]
     (loop [i (int 0)
            currc c
            currdist Double/MAX_VALUE]
       (if (< i s)
         (let [c1 (nth pal i)
               dist (m/abs (dist-fn c c1))]
           (recur (unchecked-inc i)
                  (if (< dist currdist) c1 currc)
                  (if (< dist currdist) dist currdist)))
         currc))))
  ([pal c] (nearest-color pal c euclidean))
  ([pal] (partial nearest-color pal)))

(defn average
  "Average colors in given `colorspace` (default: `:RGB`)"
  {:metadoc/categories #{:interp}}
  ([cs colorspace]
   (let [[to from] (colorspaces colorspace)]
     (from (v/average-vectors (map to cs)))))
  ([cs]
   (v/average-vectors (map pr/to-color cs))))

(defn lerp
  "Linear interpolation of two colors.

  See also [[lerp+]], [[lerp-]], [[gradient]] and [[mix]]"
  {:metadoc/categories #{:interp}}
  ([c1 c2  colorspace ^double t]
   (let [[to from] (colorspaces colorspace)]
     (from (lerp (to c1) (to c2) t))))
  ([c1 c2] (lerp c1 c2 0.5))
  ([c1 c2 ^double t]
   (v/interpolate (pr/to-color c1) (pr/to-color c2) t)))

(defn lerp+
  "Linear interpolation of two colors conserving luma of the first color.

  Amount: strength of the blend (defaults to 0.25)

  See also [[lerp-]]."
  {:metadoc/categories #{:interp}}
  ([c1 c2] (lerp+ c1 c2 0.25))
  ([c1 c2 ^double amount]
   (let [res (lerp c1 c2 amount)]
     (set-channel res :LAB 0 (ch0 (to-LAB c1))))))

(defn lerp-
  "Linear interpolation of two colors conserving luma of the second color.

  Amount: strength of the blend (defaults to 0.25).

  See also [[lerp+]]."
  ([c1 c2] (lerp- c1 c2 0.25))
  ([c1 c2 ^double amount]
   (lerp+ c2 c1 (- 1.0 amount))))

(defn- mix-interpolator
  ^double [^double a ^double b ^double t]
  (m/sqrt (+ (* a a (- 1.0 t))
             (* b b t))))

(defn mix
  "Mix colors in given optional `colorspace` (default: `:RGB`) and optional ratio (default: 0.5).

  chroma.js way"
  {:metadoc/categories #{:interp}}
  (^Vec4 [c1 c2 colorspace ^double t]
   (let [[to from] (colorspaces* colorspace)]
     (from (mix (to c1) (to c2) t))))
  (^Vec4 [c1 c2] (mix c1 c2 0.5))
  (^Vec4 [c1 c2 ^double t]
   (let [^Vec4 c1 (pr/to-color c1)
         ^Vec4 c2 (pr/to-color c2)]
     (Vec4. (mix-interpolator (.x c1) (.x c2) t)
            (mix-interpolator (.y c1) (.y c2) t)
            (mix-interpolator (.z c1) (.z c2) t)
            (m/mlerp (.w c1) (.w c2) t)))))

(defn negate
  "Negate color (subract from 255.0)"
  (^Vec4 [c] (negate c false))
  (^Vec4 [c alpha?]
   (let [^Vec4 c (pr/to-color c)]
     (Vec4. (- 255.0 (.x c))
            (- 255.0 (.y c))
            (- 255.0 (.z c))
            (if alpha? (- 255.0 (.w c)) (.w c))))))

;; https://github.com/ProfJski/ArtColors/blob/master/RYB.cpp#L230
(defn mixsub
  "Subtractive color mix in given optional `colorspace` (default: `:RGB`) and optional ratio (default: 0.5)"
  {:metadoc/categories #{:interp}}
  (^Vec4 [c1 c2 colorspace ^double t]
   (let [[to from] (colorspaces colorspace)]
     (from (mixsub (to c1) (to c2) t))))
  (^Vec4 [c1 c2] (mixsub c1 c2 0.5))
  (^Vec4 [c1 c2 ^double t]
   (let [c1 (pr/to-color c1)
         c2 (pr/to-color c2)
         mixed (lerp c1 c2 t)
         
         c (negate c1)
         d (negate c2)
         ^Vec4 f (set-alpha (v/clamp (v/sub (v/sub (Vec4. 255.0 255.0 255.0 255.0) c) d) 0.0 255.0) (alpha mixed))
         ;; normalized distance: opaque white - transparent black
         cd (* 4.0 t (- 1.0 t) (/ (v/dist c1 c2) 510.0))]
     (lerp mixed f cd))))

(declare gradient)

(def ^:private vec4-one (Vec4. 1.0 1.0 1.0 1.0))
(defn tinter
  "Creates fn to tint color using other color(s).

  `tint-colors` can be color, palette or gradient."
  {:metadoc/categories #{:pal}}
  ([tint-colors] (tinter tint-colors {}))
  ([tint-colors gradient-params]
   (if-let [tc (valid-color? tint-colors)]
     (let [tint (v/add tc vec4-one)]
       (fn [c]
         (let [c (v/add (pr/to-color c) vec4-one)]
           (clamp (v/mult (v/sqrt (v/div (v/emult c tint) 65536.0)) 255.0)))))
     (let [g (if (fn? tint-colors)
               tint-colors
               (gradient tint-colors gradient-params))]
       (fn [c]
         (let [^Vec4 c (pr/to-color c)
               ^Vec4 cc (v/div c 255.0)]
           (Vec4. (pr/red (g (.x cc)))
                  (pr/green (g (.y cc)))
                  (pr/blue (g (.z cc)))
                  (.w c))))))))

;; color reduction using x-means

(defn reduce-colors
  "Reduce colors using x-means clustering in given `colorspace` (default `:RGB`).

  Use for long sequences (for example to generate palette from image)."
  {:metadoc/categories #{:pal}}
  ([xs number-of-colors] (reduce-colors xs number-of-colors :RGB))
  ([xs number-of-colors colorspace]
   (let [[to from] (colorspaces* colorspace)]     
     (sort-by luma ;; sort by brightness
              (for [{:keys [data]} (-> (map to xs) ;; convert to given colorspace
                                       (cl/x-means number-of-colors) ;; clustering
                                       (cl/regroup))] ;; reshape
                (->> (map pack data) ;; pack colors into integers
                     (stat/modes) ;; find colors which appears most often
                     (map (comp pr/to-color unchecked-long)) ;; convert back to colors
                     (v/average-vectors) ;; average vectors if necessary
                     (from))))))) ;; convert back to RGB

;; colors

(defn brighten
  "Change luma for givent color by given amount.

  Works in LAB color space. Default amount is 1.0 and means change luma in LAB of 18.0.

  See [[darken]]."
  {:metadoc/categories meta-ops}
  (^Vec4 [col] (brighten col 1.0))
  (^Vec4 [col ^double amt]
   (let [c (to-LAB col)]
     (clamp (from-LAB (Vec4. (max 0.0 (+ (.x c) (* 18.0 amt))) (.y c) (.z c) (.w c)))))))

(defn darken
  "Change luma for givent color by given amount.
  
  Works in LAB color space. Default amount is 1.0 and means change luma in LAB of -18.0.

  See [[brighten]]."
  {:metadoc/categories meta-ops}
  (^Vec4 [col] (brighten col -1.0))
  (^Vec4 [col ^double amt] (brighten col (- amt))))

(defn saturate
  "Change color saturation in LCH color space."
  {:metadoc/categories meta-ops}
  (^Vec4 [col] (saturate col 1.0))
  (^Vec4 [col ^double amt]
   (let [c (to-LCH col)
         ns (max 0.0 (+ (.y c) (* 18.0 amt)))]
     (clamp (from-LCH (Vec4. (.x c) ns (.z c) (.w c)))))))

(defn desaturate
  "Change color saturation in LCH color space."
  {:metadoc/categories meta-ops}
  (^Vec4 [col] (saturate col -1.0))
  (^Vec4 [col ^double amt] (saturate col (- amt))))

(defn adjust
  "Adjust (add) given value to a chosen channel. Works with any color space."
  {:metadoc/categories meta-ops}
  (^Vec4 [col colorspace ^long channel ^double value]
   (let [[to from] (colorspaces colorspace)
         ^Vec4 c (to col)]
     (clamp (from (case channel
                    0 (Vec4. (+ value (.x c)) (.y c) (.z c) (.w c))
                    1 (Vec4. (.x c) (+ value (.y c)) (.z c) (.w c))
                    2 (Vec4. (.x c) (.y c) (+ value (.z c)) (.w c))
                    3 (Vec4. (.x c) (.y c) (.z c) (+ value (.w c)))
                    c)))))
  (^Vec4 [col ^long channel ^double value]
   (let [^Vec4 c (pr/to-color col)]
     (clamp (case channel
              0 (Vec4. (+ value (.x c)) (.y c) (.z c) (.w c))
              1 (Vec4. (.x c) (+ value (.y c)) (.z c) (.w c))
              2 (Vec4. (.x c) (.y c) (+ value (.z c)) (.w c))
              3 (Vec4. (.x c) (.y c) (.z c) (+ value (.w c)))
              c)))))

(defn modulate
  "Modulate (multiply) chosen channel by given amount. Works with any color space."
  {:metadoc/categories meta-ops}
  (^Vec4 [col colorspace ^long channel ^double amount]
   (let [[to from] (colorspaces colorspace)
         ^Vec4 c (to col)]
     (clamp (from (case channel
                    0 (Vec4. (* amount (.x c)) (.y c) (.z c) (.w c))
                    1 (Vec4. (.x c) (* amount (.y c)) (.z c) (.w c))
                    2 (Vec4. (.x c) (.y c) (* amount (.z c)) (.w c))
                    3 (Vec4. (.x c) (.y c) (.z c) (* amount (.w c)))
                    c)))))
  (^Vec4 [col ^long channel ^double amount]
   (let [^Vec4 c (pr/to-color col)]
     (clamp (case channel
              0 (Vec4. (* amount (.x c)) (.y c) (.z c) (.w c))
              1 (Vec4. (.x c) (* amount (.y c)) (.z c) (.w c))
              2 (Vec4. (.x c) (.y c) (* amount (.z c)) (.w c))
              3 (Vec4. (.x c) (.y c) (.z c) (* amount (.w c)))
              c)))))

(defn adjust-temperature
  "Adjust temperature of color, palette or gradient.

  Default amount: 0.35
  
  See [[temperature]] and [[lerp+]]."
  {:metadoc/categories meta-ops}
  ([in temp] (adjust-temperature in temp 0.35))
  ([in temp amount]
   (let [t (temperature temp)
         l #(lerp+ % t amount)]
     (cond
       (fn? in) (comp l in) ;; gradient
       (not (possible-color? in)) (mapv l in) ;; palette
       :else (l in)))))

;;

(defn- interpolated-gradient
  [palette-or-gradient-name {:keys [colorspace interpolation domain to? from?]
                             :or {colorspace :RGB
                                  to? true from? true
                                  interpolation :linear-smile}}]
  (let [[to from] (colorspaces colorspace)
        to (if to? to identity)
        from (if from? from identity)
        cpalette (map (comp to pr/to-color) palette-or-gradient-name)]
    (if (and (keyword? interpolation)
             (contains? e/easings-list interpolation))

      ;; easings
      (let [c1 (first cpalette)
            c2 (second cpalette)
            easing (e/easings-list interpolation)]
        (fn ^Vec4 [^double t]
          (v/fmap (from (v/interpolate c1 c2 (easing t))) clamp255)))

      ;; interpolation
      (let [r (or domain 
                  (map (fn [^long v] (m/mnorm v 0.0 (dec (count cpalette)) 0.0 1.0)) (range (count cpalette))))
            c0 (map pr/red cpalette)
            c1 (map pr/green cpalette)
            c2 (map pr/blue cpalette)
            c3 (map pr/alpha cpalette)
            ifn (if (keyword? interpolation) (i/interpolators-1d-list interpolation) interpolation)
            i0 (ifn r c0)
            i1 (ifn r c1)
            i2 (ifn r c2)
            i3 (ifn r c3)] 
        (fn ^Vec4 [^double t]
          (let [ct (m/constrain t 0.0 1.0)]
            (v/fmap (from (v/vec4 (i0 ct)
                                  (i1 ct)
                                  (i2 ct)
                                  (i3 ct))) clamp255)))))))

(def ^{:metadoc/categories #{:gr}
       :doc "Cubehelix gradient generator from two colors."}
  gradient-cubehelix #(interpolated-gradient % {:colorspace :Cubehelix :to? false :interpolation :linear}))

(defonce ^{:private true} basic-gradients-delay
  (delay (merge (into {} (for [[k v] {:rainbow1              [[0.5 0.5 0.5] [0.5 0.5 0.5] [1.0 1.0 1.0] [0 0.3333 0.6666]]
                                      :rainbow2              [[0.5 0.5 0.5] [0.666 0.666 0.666] [1.0 1.0 1.0] [0 0.3333 0.6666]]
                                      :rainbow3              [[0.5 0.5 0.5] [0.75 0.75 0.75] [1.0 1.0 1.0] [0 0.3333 0.6666]]
                                      :rainbow4              [[0.5 0.5 0.5] [1 1 1] [1.0 1.0 1.0] [0 0.3333 0.6666]]
                                      :yellow-magenta-cyan   [[1 0.5 0.5] [0.5 0.5 0.5] [0.75 1.0 0.6666] [0.8 1.0 0.3333]]
                                      :orange-blue           [[0.5 0.5 0.5] [0.5 0.5 0.5] [0.8 0.8 0.5] [0 0.2 0.5]]
                                      :green-magenta         [[0.6666 0.5 0.5] [0.5 0.6666 0.5] [0.6666 0.666 0.5] [0.2 0.0 0.5]]
                                      :green-red             [[0.5 0.5 0] [0.5 0.5 0] [0.5 0.5 0] [0.5 0.0 0]]
                                      :green-cyan            [[0.0 0.5 0.5] [0 0.5 0.5] [0.0 0.3333 0.5] [0.0 0.6666 0.5]]
                                      :yellow-red            [[0.5 0.5 0] [0.5 0.5 0] [0.1 0.5 0] [0.0 0.0 0]]
                                      :blue-cyan             [[0.0 0.5 0.5] [0 0.5 0.5] [0.0 0.5 0.3333] [0.0 0.5 0.6666]]
                                      :red-blue              [[0.5 0 0.5] [0.5 0 0.5] [0.5 0 0.5] [0 0 0.5]]
                                      :yellow-green-blue     [[0.650 0.5 0.310] [-0.650 0.5 0.6] [0.333 0.278 0.278] [0.660 0.0 0.667]]
                                      :blue-white-red        [[0.660 0.56 0.680] [0.718 0.438 0.720] [0.520 0.8 0.520] [-0.430 -0.397 -0.083]]
                                      :cyan-magenta          [[0.610 0.498 0.650] [0.388 0.498 0.350] [0.530 0.498 0.620] [3.438 3.012 4.025]]
                                      :yellow-purple-magenta [[0.731 1.098 0.192] [0.358 1.090 0.657] [1.077 0.360 0.328] [0.965 2.265 0.837]]
                                      :green-blue-orange     [[0.892 0.725 0.000] [0.878 0.278 0.725] [0.332 0.518 0.545] [2.440 5.043 0.732]]
                                      :orange-magenta-blue   [[0.821 0.328 0.242] [0.659 0.481 0.896] [0.612 0.340 0.296] [2.820 3.026 -0.273]]
                                      :blue-magenta-orange   [[0.938 0.328 0.718] [0.659 0.438 0.328] [0.388 0.388 0.296] [2.538 2.478 0.168]]
                                      :magenta-green         [[0.590 0.811 0.120] [0.410 0.392 0.590] [0.940 0.548 0.278] [-4.242 -6.611 -4.045]]}]
                           [k (apply iq-gradient v)])) ;; thi.ng presets
                {:iq-1 (iq-gradient (Vec3. 0.5 0.5 0.5)
                                    (Vec3. 0.5 0.5 0.5)
                                    (Vec3. 1.0 1.0 1.0)
                                    (Vec3. 0.0 0.33 0.67))
                 :iq-2 (iq-gradient (Vec3. 0.5 0.5 0.5)
                                    (Vec3. 0.5 0.5 0.5)
                                    (Vec3. 1.0 1.0 1.0)
                                    (Vec3. 0.0 0.1 0.2))
                 :iq-3 (iq-gradient (Vec3. 0.5 0.5 0.5)
                                    (Vec3. 0.5 0.5 0.5)
                                    (Vec3. 1.0 1.0 1.0)
                                    (Vec3. 0.3 0.2 0.2))
                 :iq-4 (iq-gradient (Vec3. 0.5 0.5 0.5)
                                    (Vec3. 0.5 0.5 0.5)
                                    (Vec3. 1.0 1.0 0.5)
                                    (Vec3. 0.8 0.9 0.3))
                 :iq-5 (iq-gradient (Vec3. 0.5 0.5 0.5)
                                    (Vec3. 0.5 0.5 0.5)
                                    (Vec3. 1.0 0.7 0.4)
                                    (Vec3. 0.0 0.15 0.2))
                 :iq-6 (iq-gradient (Vec3. 0.5 0.5 0.5)
                                    (Vec3. 0.5 0.5 0.5)
                                    (Vec3. 2.0 1.0 0.0)
                                    (Vec3. 0.5 0.2 0.25))
                 :iq-7 (iq-gradient (Vec3. 0.8 0.5 0.4)
                                    (Vec3. 0.2 0.4 0.2)
                                    (Vec3. 2.0 1.0 1.0)
                                    (Vec3. 0.0 0.25 0.25))
                 :cubehelix (gradient-cubehelix [(Vec4. 300.0 0.5 0.0 255.0) (Vec4. -240 0.5 1.0 255.0)])
                 :warm (gradient-cubehelix [(Vec4. -100.0 0.75 0.35 255.0) (Vec4. 80.0 1.5 0.8 255.0)])
                 :cool (gradient-cubehelix [(Vec4. 260.0 0.75 0.35 255.0) (Vec4. 80.0 1.5 0.8 255.0)])
                 :rainbow (fn [^double t] (let [ts (m/abs (- t 0.5))]
                                           (from-Cubehelix (Vec4. (- (* t 360.0) 100.0)
                                                                  (- 1.5 (* 1.5 ts))
                                                                  (- 0.8 (* 0.9 ts))
                                                                  255.0))))
                 :black-body (fn [^double t] (temperature (m/mlerp 1000 15000 t)))})))

(defn- get-gradient-from-file-
  [k]
  (let [g (get-palette-or-gradient "gradients/" k)]
    (if (map? g)
      (interpolated-gradient (:c g) {:domain (:p g)})
      (interpolated-gradient g {}))))

(defonce ^:private get-gradient-from-file (memoize get-gradient-from-file-))

(defn- get-gradient
  [k]
  (if (namespace k)
    (get-gradient-from-file k)
    (@basic-gradients-delay k)))

(defonce ^:private gradients-list-
  (delay (let [g (set (concat (keys @basic-gradients-delay)
                              (read-edn "gradients/all-gradients.edn")))]
           {:set g
            :seq (vec (sort g))})))

(defn gradient
  "Return gradient function.
  
  Grandient function accepts value from 0 to 1 and returns interpolated color.

  Parameters;

  * `palette-or-gradient-name` - name of the [predefined gradient](../static/gradients.html) or palette (seq of colors).
  * (optional) additional parameters map:
      * `:colorspace` - operate in given color space. Default: `:RGB`.
      * `:interpolation` - interpolation name or interpolation function. Interpolation can be one of [[interpolators-1d-list]] or [[easings-list]]. When easings is used, only first two colors are interpolated. Default: `:linear-smile`.
      * `:to?` - turn on/off conversion to given color space. Default: `true`.
      * `:from?` - turn on/off conversion from given color space. Default: `true`.
      * `:domain` - interpolation domain as seq of values for each color.

  When called without parameters random gradient is returned."
  {:metadoc/categories #{:gr}}
  ([] (gradient (rand-nth (:seq @gradients-list-))))
  ([palette-or-gradient-name] (gradient palette-or-gradient-name {}))
  ([palette-or-gradient-name options]
   (cond (keyword? palette-or-gradient-name) (get-gradient palette-or-gradient-name)
         (and (= (:interpolation options) :iq)
              (seq? palette-or-gradient-name)) (apply iq-gradient palette-or-gradient-name)       
         :else (interpolated-gradient palette-or-gradient-name options))))

;;

(defonce ^:private palettes-list-
  (delay (let [p (set (read-edn "palettes/all-palettes.edn"))]
           {:set p
            :seq (vec (sort-by keyword p))})))

(defn- get-palette-
  [k]
  (when (contains? (:set @palettes-list-) k)
    (mapv pr/to-color (get-palette-or-gradient "palettes/" k))))

(defonce ^:private get-palette (memoize get-palette-))

(defn palette
  "Get palette.

  If argument is a keyword, returns one from palette presets.
  If argument is a number, returns one from colourlovers palettes.
  If argument is a gradient, returns 5 or `number-of-colors` samples.

  If called without argument, random palette is returned
  
  Optionally you can pass number of requested colors and other parameters as in [[resample]]"
  {:metadoc/categories #{:pal}}
  ([] (palette (rand-nth (:seq @palettes-list-))))
  ([p]
   (cond
     (or (keyword? p)
         (integer? p)) (get-palette p)
     (fn? p) (palette p 5)
     :else (vec p)))
  ([p number-of-colors] (palette p number-of-colors {}))
  ([p number-of-colors gradient-params]
   (vec (if (fn? p)
          (m/sample p number-of-colors)
          (m/sample (gradient (palette p) gradient-params) number-of-colors)))))

(defn- find-gradient-or-palette
  ([lst] (:seq @lst))
  ([lst regex] (filter (comp (partial re-find regex) str) (:seq @lst)))
  ([lst group regex] (filter (fn [k]
                               (and (= (namespace k) group)
                                    (re-find regex (name k)))) (:seq @lst))))

(def find-gradient (partial find-gradient-or-palette gradients-list-))
(def find-palette (partial find-gradient-or-palette palettes-list-))

(defn resample
  "Resample palette.

  Internally it's done by creating gradient and sampling back to colors. You can pass [[gradient]] parameters like colorspace, interpolator name and domain."
  {:metadoc/categories #{:pal}}
  ([pal number-of-colors] (resample pal number-of-colors {}))
  ([pal number-of-colors gradient-params] (palette pal number-of-colors gradient-params)))

(defn correct-luma
  "Create palette or gradient with corrected luma to be linear.

  See [here](https://www.vis4.net/blog/2013/09/mastering-multi-hued-color-scales/#combining-bezier-interpolation-and-lightness-correction)"
  {:metadoc/categories #{:pal :gr}}
  ([palette-or-gradient] (correct-luma palette-or-gradient {}))
  ([palette-or-gradient gradient-params]
   (if (fn? palette-or-gradient) ;; gradient
     (let [l0 (ch0 (to-LAB (palette-or-gradient 0.0)))
           l1 (ch0 (to-LAB (palette-or-gradient 1.0)))
           xs (range 0.0 1.005 0.005)
           ls (map (fn [^double v] (ch0 (to-LAB (palette-or-gradient v)))) xs)
           i (i/linear-smile ls xs)]
       (fn ^Vec4 [^double t] (palette-or-gradient (i (m/lerp l0 l1 t)))))
     (let [n (count palette-or-gradient)
           g (correct-luma (gradient palette-or-gradient gradient-params))]
       (palette g n)))))


;;;

(defn- iq-random-gradient
  "Create random iq gradient."
  {:metadoc/categories #{:gr}}
  []
  (r/randval
   (let [a (v/generate-vec3 (partial r/drand 0.2 0.8))
         b (v/generate-vec3 (partial r/drand 0.2 0.8))
         c (v/generate-vec3 (partial r/drand 2))
         d (v/generate-vec3 r/drand)]
     (iq-gradient a b c d))
   (let [pal (paletton :monochromatic (r/irand 360) {:compl true :preset (rand-nth [:pastels :pastels-med :full :shiny :dark :pastels-mid-dark :dark-neon :darker])})]
     (iq-gradient (second pal) (first (drop 5 pal))))))

(defn random-palette
  "Generate random palette from all collections defined in clojure2d.color namespace."
  {:metadoc/categories #{:gr}}
  []
  (condp clojure.core/> (r/drand)
    0.1 (resample (iq-random-gradient) (r/irand 3 10))
    0.3 (palette (int (rand 500)))
    0.6 (let [p (palette)]
          (if (> (count p) 15) (resample p 15) p))
    0.8 (resample (gradient) (r/irand 3 10))
    (let [h (r/drand 360)
          t (rand-nth [:monochromatic :triad :triad :triad :triad :triad :tetrad :tetrad :tetrad])
          conf {:compl (r/brand 0.6)
                :angle (r/drand 10.0 90.0)
                :adj (r/brand)
                :preset (rand-nth paletton-presets-list)}]
      (paletton t h conf))))

(defn random-gradient
  "Generate random gradient function."
  {:metadoc/categories #{:pal}}
  []
  (let [gpars {:colorspace (r/randval :RGB (rand-nth colorspaces-list))
               :interpolation (r/randval :linear-smile :cubic-spline)}]
    (condp clojure.core/> (r/drand)
      0.1 (iq-random-gradient)
      0.6 (gradient (palette) gpars)
      0.7 (gradient (rest (paletton :monochromatic (r/drand 360)
                                    {:preset (rand-nth paletton-presets-list)})) gpars)
      (gradient))))

(defn random-color
  "Generate random color"
  {:metadoc/categories #{:pal}}
  ([alpha] (set-alpha (random-color) alpha))
  ([] (r/randval 0.2 (rand-nth (named-colors-list))
                 (r/randval 0.5
                            (rand-nth (random-palette))
                            (Vec4. (r/drand 255.0) (r/drand 255.0) (r/drand 255.0) 255.0)))))

;; Color Vision Deficiency
;; https://github.com/chromelens/chromelens/blob/master/lenses/filters/

(defmacro gen-cvd-multiplication-fn
  [r1 r2 r3 r4 r5
   g1 g2 g3 g4 g5
   b1 b2 b3 b4 b5]
  (let [c (with-meta (symbol "c") {:tag `Vec4})]
    `(fn [c#]
       (let [~c (from-sRGB c#)]
         (to-sRGB (Vec4. (+ (* ~r1 (.x ~c)) (* ~r2 (.y ~c)) (* ~r3 (.z ~c)) (* ~r4 (.w ~c)) ~r5)
                         (+ (* ~g1 (.x ~c)) (* ~g2 (.y ~c)) (* ~g3 (.z ~c)) (* ~g4 (.w ~c)) ~g5)
                         (+ (* ~b1 (.x ~c)) (* ~b2 (.y ~c)) (* ~b3 (.z ~c)) (* ~b4 (.w ~c)) ~b5)
                         (.w ~c)))))))

(def ^:private cvd-converters
  {:achromatomaly (gen-cvd-multiplication-fn 0.618,0.320,0.062,0,0,0.163,0.775,0.062,0,0,0.163,0.320,0.516,0,0)
   :achromatopsia (gen-cvd-multiplication-fn 0.299,0.587,0.114,0,0,0.299,0.587,0.114,0,0,0.299,0.587,0.114,0,0)
   :deuteranomaly (gen-cvd-multiplication-fn 0.8,0.2,0,0,0,0.258,0.742,0,0,0,0,0.142,0.858,0,0)
   :deuteranopia  (gen-cvd-multiplication-fn 0.625,0.375,0,0,0,0.7,0.3,0,0,0,0,0.3,0.7,0,0)
   :protanomaly   (gen-cvd-multiplication-fn 0.817,0.183,0,0,0,0.333,0.667,0,0,0,0,0.125,0.875,0,0)
   :protanopia    (gen-cvd-multiplication-fn 0.567,0.433,0,0,0,0.558,0.442,0,0,0,0,0.242,0.758,0,0)
   :tritanomaly   (gen-cvd-multiplication-fn 0.967,0.033,0,0,0,0,0.733,0.267,0,0,0,0.183,0.817,0,0)
   :tritanopia    (gen-cvd-multiplication-fn 0.95,0.05,0,0,0,0,0.433,0.567,0,0,0,0.475,0.525,0,0)})

(def cvd-list (sort (keys cvd-converters)))

(defn cvd-lens
  ^Vec4 [c cvd-kind]
  (let [f (get cvd-converters cvd-kind identity)]
    (if-let [c (valid-color? c)]
      (f c)
      (if (fn? c)
        (comp f c)
        (mapv f c)))))
