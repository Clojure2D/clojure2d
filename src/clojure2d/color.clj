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
  * `keyword` - one of the defined names (see [[named-colors-list]]); names without a namespace also as strings, case-insensitive (`Red`, `dark-blue`)
  * `Integer` or `Long` - packed ARGB value `0xAARRGGBB`. When the top byte is 0 the color is opaque (alpha 255), so `0xffaa01` is opaque orange; a fully transparent packed color cannot be expressed. See [[pack]].
  * `String` - hex notation `#rgb`, `#rgba`, `#rrggbb` or `#rrggbbaa`, case-insensitive, `#` optional; one or two hex digits give a gray (`ab` is `#ababab`). See [[format-hex]].
  * any `seqable` (list, vector) or `double`, `float`, `int`, `long` array - 0 elements: black; 1: gray; 2: gray and alpha; 3: RGB; 4 or more: RGBA (the first four).

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

  Munsell colours, given as a notation like `4.2YR 8.1/5.3` or as a specification, can be converted with [[munsell-to-Yxy]] and [[munsell-to-sRGB]]. Parsing, Munsell value scales and conversion details are in `clojure2d.color.munsell`.
    
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

  Use [[lerp]], [[lerp+]], [[mix]], [[average]], [[mixbox]] to mix two colors in different ways.
  
  ## Distances

  Several functions to calculate difference between colors, `delta-E*-xxx` etc.

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
            [fastmath.interpolation.linear :as li]
            [fastmath.ml.clustering :as cl]
            [fastmath.easings :as e]
            [clojure2d.protocols :as pr]
            [clojure2d.color.whitepoints :as wp]
            [clojure2d.color.rgb :as rgb]
            [clojure2d.color.munsell :as munsell]
            [clojure.java.io :refer [input-stream resource]]
            [clojure.edn :as edn]
            [fastmath.matrix :as mat])
  (:import [fastmath.vector Vec2 Vec3 Vec4]
           [java.awt Color]
           [clojure.lang APersistentVector ISeq Seqable]
           [com.scrtwpns Mixbox]))

(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defonce ^:private meta-conv #{:conv})
(defonce ^:private meta-ops #{:ops})

;; load the stuff to delay

;;;;;;;;;
;; read
;;;;;;;;;

(defn- read-edn [n] (-> (resource n) input-stream slurp edn/read-string))

;; color names

(defonce ^:private color-presets-delay
  (delay (into {} (map (fn [[k v]]
                         [k (pr/to-color v)]) (read-edn "color_presets.edn")))))

(defn named-colors-list
  "Returns the names of all predefined colors.

  The names are keywords: the 148 CSS Color 4 names (e.g. `:red`, `:rebeccapurple`), hyphenated variants of the multi-word CSS names (e.g. `:dark-blue`), `:amber` (a clojure2d-specific color, `#b28c00`) and `:docc/...` colors from A Dictionary of Colour Combinations by Sanzo Wada (via mattdesl/dictionary-of-colour-combinations). Every name can be used wherever a color is expected.

  Returns an unordered sequence of keywords. See also [[to-color]], [[palette]]."
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

(def ^:private color-array-classes
  "Array classes accepted by [[to-color]]: `double`, `float`, `int`, `long`."
  #{(Class/forName "[D") (Class/forName "[F") (Class/forName "[I") (Class/forName "[J")})

(defn- color-array?
  "Is `c` an array of a type [[to-color]] accepts?"
  [c]
  (contains? color-array-classes (class c)))

(defn possible-color?
  "Checks by type whether `c` looks like a single color.

  - `c` any value

  Returns true for strings, keywords, packed integers (`Long`, `Integer`), `Vec2`, `Vec3`, `Vec4` and `java.awt.Color`, and for a sequential collection (vector, list, sequence) or a `double`, `float`, `int`, `long` array of 1 to 4 numbers, each within `[-255,255]`. Other collections (sets, maps, Java collections) are not colors. The value is not converted, so an unknown name or an invalid hex string also passes. A short sequence of small numbers is ambiguous: `[16 32]` is a gray with alpha, not a palette of two packed colors. See also [[valid-color?]], [[possible-palette?]]."
  {:metadoc/categories #{:pal}}
  [c]
  (cond
    (or (string? c) (keyword? c) (instance? Long c) (instance? Integer c)
        (instance? Vec2 c) (instance? Vec3 c) (instance? Vec4 c) (instance? Color c)) true
    (or (sequential? c) (color-array? c)) (let [s (take 5 c)
                       n (count s)]
                   (and (<= 1 n 4)
                        (every? #(and (number? %) (<= (m/abs (double %)) 255.0)) s)))
    :else false))

(defn possible-palette?
  "Checks by type whether `c` looks like a palette, a sequence of colors.

  - `c` any value

  Returns true for a non-empty sequential collection (vector, list, sequence) which is not itself a color according to [[possible-color?]] and whose every element is a possible color. nil, maps, sets, strings and functions (gradients) are not palettes. The elements are not converted. See also [[possible-color?]], [[valid-color?]], [[palette]]."
  {:metadoc/categories #{:pal}}
  [c]
  (and (sequential? c)
       (boolean (seq c))
       (not (possible-color? c))
       (every? possible-color? c)))

(defn valid-color?
  "Checks whether `c` is a valid color by converting it.

  - `c` any value

  Returns the `Vec4` from [[to-color]] when the conversion succeeds, false otherwise (nil, an unknown name, an invalid hex string, an unsupported type). See also [[possible-color?]]."
  {:metadoc/categories #{:pal}}
  [c]
  (try
    (pr/to-color c)
    (catch Exception _ false)))

;; ## Color representation

(defn to-color
  "Converts any color representation to `Vec4` (RGBA), without clamping.

  - `c` one of: `Vec4` (returned as is), `Vec3` (alpha 255), `Vec2` (gray, alpha), keyword (a name of [[named-colors-list]]), packed ARGB integer (top byte 0 means opaque), hex string (`#rgb`, `#rgba`, `#rrggbb`, `#rrggbbaa`, `#` optional, case-insensitive, 1 or 2 digits for a gray), color name string (case-insensitive, names without a namespace, e.g. `Red`, `dark-blue`), `java.awt.Color`, sequence or `double`/`float`/`int`/`long` array (0 elements black, 1 gray, 2 gray and alpha, 3 RGB, 4 or more RGBA)

  Returns a `Vec4`; values are taken as given, so a `Vec4` from another colorspace passes through. Throws `ex-info` with `{:color c}` for nil, an unknown color name or a string which is neither hex nor a color name. See also [[color]] (clamped), [[valid-color?]], [[to-awt-color]], [[format-hex]]."
  {:metadoc/categories meta-ops}
  ^Vec4 [c] (pr/to-color c))

(defn to-awt-color
  "Converts any color representation to `java.awt.Color`.

  - `c` color, any type accepted by [[to-color]]

  Channels and alpha are rounded and clamped to integers in `[0,255]`. See also [[awt-color]], [[to-color]]."
  {:metadoc/categories meta-ops}
  ^java.awt.Color [c] (pr/to-awt-color c))

(defn luma
  "Returns the luma of a color: the luminance weights applied to the gamma-encoded channels.

  Formula: `0.2126740 r + 0.7151511 g + 0.0721750 b` (the Y row of the sRGB to XYZ matrix of [[to-XYZ]], from the sRGB primaries and the CIE 1931 2° D65 white) on the channels as given, without linearization.

  - `c` color, any type accepted by [[to-color]]

  Returns a double, `[0,255]` for RGB colors (white 255). See also [[relative-luma]] (linear), [[to-Gray]]."
  {:metadoc/categories meta-ops}
  ^double [c] (pr/luma c))

(def ^:private srgb-to-xyz-matrix
  "sRGB (linear) to XYZ matrix of `(rgb/rgbs :sRGB)`, derived from the sRGB primaries and `wp/CIE-2-D65`; read once into the constants below."
  (vec (mat/mat->seq (:rgb-to-xyz (rgb/rgbs :sRGB)))))

(def ^:private xyz-to-srgb-matrix
  "Inverse of [[srgb-to-xyz-matrix]], from `(rgb/rgbs :sRGB)`."
  (vec (mat/mat->seq (:xyz-to-rgb (rgb/rgbs :sRGB)))))

(def ^{:private true :const true :tag 'double} srgb->xyz-00 (srgb-to-xyz-matrix 0))
(def ^{:private true :const true :tag 'double} srgb->xyz-01 (srgb-to-xyz-matrix 1))
(def ^{:private true :const true :tag 'double} srgb->xyz-02 (srgb-to-xyz-matrix 2))
(def ^{:private true :const true :tag 'double} srgb->xyz-10 (srgb-to-xyz-matrix 3))
(def ^{:private true :const true :tag 'double} srgb->xyz-11 (srgb-to-xyz-matrix 4))
(def ^{:private true :const true :tag 'double} srgb->xyz-12 (srgb-to-xyz-matrix 5))
(def ^{:private true :const true :tag 'double} srgb->xyz-20 (srgb-to-xyz-matrix 6))
(def ^{:private true :const true :tag 'double} srgb->xyz-21 (srgb-to-xyz-matrix 7))
(def ^{:private true :const true :tag 'double} srgb->xyz-22 (srgb-to-xyz-matrix 8))
(def ^{:private true :const true :tag 'double} xyz->srgb-00 (xyz-to-srgb-matrix 0))
(def ^{:private true :const true :tag 'double} xyz->srgb-01 (xyz-to-srgb-matrix 1))
(def ^{:private true :const true :tag 'double} xyz->srgb-02 (xyz-to-srgb-matrix 2))
(def ^{:private true :const true :tag 'double} xyz->srgb-10 (xyz-to-srgb-matrix 3))
(def ^{:private true :const true :tag 'double} xyz->srgb-11 (xyz-to-srgb-matrix 4))
(def ^{:private true :const true :tag 'double} xyz->srgb-12 (xyz-to-srgb-matrix 5))
(def ^{:private true :const true :tag 'double} xyz->srgb-20 (xyz-to-srgb-matrix 6))
(def ^{:private true :const true :tag 'double} xyz->srgb-21 (xyz-to-srgb-matrix 7))
(def ^{:private true :const true :tag 'double} xyz->srgb-22 (xyz-to-srgb-matrix 8))

(defn- luma-fn
  "Weighted sum of channels with the Y row of the sRGB to XYZ matrix of [[to-XYZ]] (from `(rgb/rgbs :sRGB)`)."
  ^double [^double r ^double g ^double b]
  (+ (* srgb->xyz-10 r)
     (* srgb->xyz-11 g)
     (* srgb->xyz-12 b)))

(declare from-sRGB)

(defn relative-luma
  "Returns the relative luminance of an sRGB color, scaled to `[0,255]`.

  The channels are linearized (sRGB transfer function) and weighted with the Y row of the sRGB to XYZ matrix: `0.2126740 r + 0.7151511 g + 0.0721750 b`. Equals the Y of [[to-XYZ]] times 2.55; divide by 255 for the WCAG relative luminance.

  - `c` color, any type accepted by [[to-color]]

  Returns a double, `[0,255]` (white 255). See also [[luma]], [[contrast-ratio]]."
  {:metadoc/categories meta-ops}
  ^double [c]
  (let [^Vec4 c (from-sRGB c)]
    (luma-fn (.x c) (.y c) (.z c))))

(defn red
  "Returns the first channel value (red for RGB colors).

  - `c` color, any type accepted by [[to-color]]

  Returns a double, not clamped. For a `Vec4` holding another colorspace (e.g. from `to-LAB`) it is that colorspace's first channel. Same as [[ch0]]; see also [[set-red]]."
  {:metadoc/categories meta-ops}
  ^double [c] (pr/red c))

(defn green
  "Returns the second channel value (green for RGB colors).

  - `c` color, any type accepted by [[to-color]]

  Returns a double, not clamped. For a `Vec4` holding another colorspace (e.g. from `to-LAB`) it is that colorspace's second channel. Same as [[ch1]]; see also [[set-green]]."
  {:metadoc/categories meta-ops}
  ^double [c] (pr/green c))

(defn blue
  "Returns the third channel value (blue for RGB colors).

  - `c` color, any type accepted by [[to-color]]

  Returns a double, not clamped. For a `Vec4` holding another colorspace (e.g. from `to-LAB`) it is that colorspace's third channel. Same as [[ch2]]; see also [[set-blue]]."
  {:metadoc/categories meta-ops}
  ^double [c] (pr/blue c))

(defn alpha
  "Returns the alpha value of a color.

  - `c` color, any type accepted by [[to-color]]

  Returns a double, not clamped; 255 for colors without alpha (`Vec3`, 3-element sequences, packed integers whose top byte is 0). See also [[set-alpha]]."
  {:metadoc/categories meta-ops}
  ^double [c] (pr/alpha c))

(defn ch0
  "Returns the first channel value (red for RGB colors).

  - `c` color, any type accepted by [[to-color]]

  Returns a double, not clamped. For a `Vec4` holding another colorspace (e.g. from `to-LAB`) it is that colorspace's first channel. Same as [[red]]; see also [[set-ch0]], [[get-channel]]."
  {:metadoc/categories meta-ops}
  ^double [c] (pr/red c))

(defn ch1
  "Returns the second channel value (green for RGB colors).

  - `c` color, any type accepted by [[to-color]]

  Returns a double, not clamped. For a `Vec4` holding another colorspace (e.g. from `to-LAB`) it is that colorspace's second channel. Same as [[green]]; see also [[set-ch1]], [[get-channel]]."
  {:metadoc/categories meta-ops}
  ^double [c] (pr/green c))

(defn ch2
  "Returns the third channel value (blue for RGB colors).

  - `c` color, any type accepted by [[to-color]]

  Returns a double, not clamped. For a `Vec4` holding another colorspace (e.g. from `to-LAB`) it is that colorspace's third channel. Same as [[blue]]; see also [[set-ch2]], [[get-channel]]."
  {:metadoc/categories meta-ops}
  ^double [c] (pr/blue c))

(defn hue-polar
  "Returns the hue of a color in degrees, using the polar (circular) model.

  Formula: `atan2(sqrt(3)/2 * (g - b), r - (g + b)/2)`, mapped to the range from 0 to 360 (a tiny negative angle rounds to 360). It equals the hexagonal [[hue]] at primary and secondary colors and differs slightly between them.

  - `c` color, any type accepted by [[to-color]]

  Returns a double; grays (equal channels) give 0. See also [[hue]]."
  {:metadoc/categories meta-ops}
  ^double [c]
  (let [^Vec4 c (pr/to-color c)
        a (* 0.5 (- (+ (.x c) (.x c)) (.y c) (.z c)))
        b (* 0.8660254037844386 (- (.y c) (.z c)))
        h (m/degrees (m/atan2 b a))]
    (if (neg? h) (+ 360.0 h) h)))

(declare to-HC)

(defn hue
  "Returns the hue of a color in degrees, using the hexagonal model of HSV and HSL.

  The hue is the position on the hexagon spanned by the primaries and secondaries: red 0, yellow 60, green 120, cyan 180, blue 240, magenta 300.

  - `c` color, any type accepted by [[to-color]]

  Returns a double from 0 to 360 (a tiny negative hexagon position rounds to 360); grays (equal channels) give 0. See also [[hue-polar]], [[to-HSV]]."
  {:metadoc/categories meta-ops}
  ^double [c]
  (let [^Vec4 ret (to-HC (pr/to-color c))] (.x ret)))

(defn set-alpha
  "Returns the color with alpha set to `a`.

  - `c` color, any type accepted by [[to-color]]
  - `a` new alpha, not clamped

  Returns a `Vec4`. See also [[alpha]], [[color]] (clamping), [[set-awt-alpha]]."
  {:metadoc/categories meta-ops}
  ^Vec4 [c a]
  (let [^Vec4 v (pr/to-color c)]
    (Vec4. (.x v) (.y v) (.z v) a)))

(defn set-red
  "Returns the color with the first channel (red for RGB colors) set to `val`.

  - `c` color, any type accepted by [[to-color]]
  - `val` new channel value, not clamped

  Returns a `Vec4`. See also [[red]], [[set-ch0]], [[set-channel]]."
  {:metadoc/categories meta-ops}
  ^Vec4 [c val]
  (let [^Vec4 v (pr/to-color c)]
    (Vec4. val (.y v) (.z v) (.w v))))

(defn set-green
  "Returns the color with the second channel (green for RGB colors) set to `val`.

  - `c` color, any type accepted by [[to-color]]
  - `val` new channel value, not clamped

  Returns a `Vec4`. See also [[green]], [[set-ch1]], [[set-channel]]."
  {:metadoc/categories meta-ops}
  ^Vec4 [c val]
  (let [^Vec4 v (pr/to-color c)]
    (Vec4. (.x v) val (.z v) (.w v))))

(defn set-blue
  "Returns the color with the third channel (blue for RGB colors) set to `val`.

  - `c` color, any type accepted by [[to-color]]
  - `val` new channel value, not clamped

  Returns a `Vec4`. See also [[blue]], [[set-ch2]], [[set-channel]]."
  {:metadoc/categories meta-ops}
  ^Vec4 [c val]
  (let [^Vec4 v (pr/to-color c)]
    (Vec4. (.x v) (.y v) val (.w v))))

(defn set-ch0
  "Returns the color with the first channel (red for RGB colors) set to `val`.

  - `c` color, any type accepted by [[to-color]]
  - `val` new channel value, not clamped

  Returns a `Vec4`. See also [[ch0]], [[set-red]], [[set-channel]]."
  {:metadoc/categories meta-ops}
  ^Vec4 [c val]
  (let [^Vec4 v (pr/to-color c)]
    (Vec4. val (.y v) (.z v) (.w v))))

(defn set-ch1
  "Returns the color with the second channel (green for RGB colors) set to `val`.

  - `c` color, any type accepted by [[to-color]]
  - `val` new channel value, not clamped

  Returns a `Vec4`. See also [[ch1]], [[set-green]], [[set-channel]]."
  {:metadoc/categories meta-ops}
  ^Vec4 [c val]
  (let [^Vec4 v (pr/to-color c)]
    (Vec4. (.x v) val (.z v) (.w v))))

(defn set-ch2
  "Returns the color with the third channel (blue for RGB colors) set to `val`.

  - `c` color, any type accepted by [[to-color]]
  - `val` new channel value, not clamped

  Returns a `Vec4`. See also [[ch2]], [[set-blue]], [[set-channel]]."
  {:metadoc/categories meta-ops}
  ^Vec4 [c val]
  (let [^Vec4 v (pr/to-color c)]
    (Vec4. (.x v) (.y v) val (.w v))))

(defn set-awt-alpha
  "Returns the color as `java.awt.Color` with alpha set to `a`.

  - `c` color, any type accepted by [[to-color]]
  - `a` new alpha, rounded and clamped to `[0,255]`

  See also [[set-alpha]], [[awt-color]]."
  {:metadoc/categories meta-ops}
  ^Color [c a]
  (let [^Color cc (pr/to-awt-color c)]
    (Color. (.getRed cc)
            (.getGreen cc)
            (.getBlue cc)
            (lclamp255 a))))

(defn awt-color
  "Creates a `java.awt.Color`.

  Arities:

  - `[c]` converts any color, as [[to-awt-color]]
  - `[c a]` converts any color and sets alpha `a`, as [[set-awt-alpha]]
  - `[r g b]` from channel values, alpha 255
  - `[r g b a]` from channel values and alpha

  Channel values and alpha are rounded and clamped to integers in `[0,255]`. See also [[awt-gray]], [[color]]."
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
  "Creates a color, `Vec4` (RGBA) with channels and alpha clamped to `[0,255]`.

  Arities:

  - `[c]` any color (see [[to-color]]), clamped
  - `[c a]` any color with alpha set to `a`, clamped
  - `[r g b]` from channel values, alpha 255
  - `[r g b a]` from channel values and alpha

  Unlike [[to-color]], which converts without clamping, `color` always returns a valid RGBA color. See also [[gray]], [[awt-color]]."
  {:metadoc/categories meta-ops}
  (^Vec4 [c]
   (let [^Vec4 v (pr/to-color c)]
     (Vec4. (clamp255 (.x v)) (clamp255 (.y v)) (clamp255 (.z v)) (clamp255 (.w v)))))
  (^Vec4 [c ^double a]
   (let [^Vec4 v (pr/to-color c)]
     (Vec4. (clamp255 (.x v)) (clamp255 (.y v)) (clamp255 (.z v)) (clamp255 a))))
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
  "Clamps all channels, alpha included, to `[0,255]`.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. See also [[lclamp]]."
  {:metadoc/categories meta-ops}
  ^Vec4 [c]
  (v/fmap (pr/to-color c) clamp255))

(defn lclamp
  "Clamps all channels, alpha included, to `[0,255]` and rounds them to integers (half up).

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with integer-valued doubles. See also [[clamp]]."
  {:metadoc/categories meta-ops}
  ^Vec4 [c]
  (v/fmap (pr/to-color c) lclamp255))

(defn scale
  "Multiplies the channels of a color by `v`.

  - `c` color, any type accepted by [[to-color]]
  - `v` factor
  - `alpha?` (optional, default false) when truthy alpha is multiplied too

  Returns a `Vec4`, not clamped. See also [[scale-down]], [[scale-up]]."
  {:metadoc/categories meta-ops}
  (^Vec4 [c ^double v] (scale c v false))
  (^Vec4 [c ^double v alpha?]
   (let [^Vec4 c (pr/to-color c)]
     (Vec4. (* v (.x c))
            (* v (.y c))
            (* v (.z c))
            (if alpha? (* v (.w c)) (.w c))))))

(defn scale-down
  "Divides the channels by 255: an RGB color goes to the `[0,1]` scale.

  - `c` color, any type accepted by [[to-color]]
  - `alpha?` (optional, default false) when truthy alpha is divided too

  Returns a `Vec4`. See also [[scale-up]], [[scale]]."
  {:metadoc/categories meta-ops}
  (^Vec4 [c] (scale-down c false))
  (^Vec4 [c alpha?] (scale c (/ 255.0) alpha?)))

(defn scale-up
  "Multiplies the channels by 255: a color on the `[0,1]` scale goes to `[0,255]`.

  - `c` color, any type accepted by [[to-color]]
  - `alpha?` (optional, default false) when truthy alpha is multiplied too

  Returns a `Vec4`. See also [[scale-down]], [[scale]]."
  {:metadoc/categories meta-ops}
  (^Vec4 [c] (scale-up c false))
  (^Vec4 [c alpha?] (scale c 255.0 alpha?)))

(defn gray
  "Creates a gray color, `Vec4` with all three channels equal to `v`.

  - `v` intensity
  - `a` alpha (optional, default 255)

  Values are clamped to `[0,255]`. See also [[color]], [[awt-gray]]."
  {:metadoc/categories meta-ops}
  (^Vec4 [^double v] (color v v v))
  (^Vec4 [^double v ^double a] (color v v v a)))

(defn awt-gray
  "Creates a gray `java.awt.Color`.

  - `v` intensity, used for all three channels
  - `a` alpha (optional, default 255)

  Values are rounded and clamped to `[0,255]`. See also [[gray]], [[awt-color]]."
  {:metadoc/categories meta-ops}
  (^Color [^double v] (awt-color v v v))
  (^Color [^double v ^double a] (awt-color v v v a)))

(defn- hex-string->color
  "Parses hex notation (`#` optional) to `Vec4`: 1 digit `v` is `vvvvvv`, 2 digits `vv` are `vvvvvv`, `rgb`, `rgba` (digits doubled), `rrggbb`, `rrggbbaa`. Returns nil for other text."
  [^String input]
  (let [^String s (if (= (first input) \#) (subs input 1) input)]
    (when (and (#{1 2 3 4 6 8} (count s)) (re-matches #"[0-9a-fA-F]+" s))
      (let [^String s (case (count s)
                        1 (str s s s s s s)
                        2 (str s s s)
                        3 (let [[r g b] s] (str r r g g b b))
                        4 (let [[r g b a] s] (str r r g g b b a a))
                        s)
            rgb (Long/parseLong (subs s 0 (min 6 (count s))) 16)]
        (if (= 8 (count s))
          (Vec4. (bit-and 0xff (m/>> rgb 16)) (bit-and 0xff (m/>> rgb 8)) (bit-and 0xff rgb) (Long/parseLong (subs s 6) 16))
          (pr/to-color (Long/parseLong s 16)))))))

(defn- gray-vec4
  "Unclamped gray `Vec4`."
  (^Vec4 [^double v] (Vec4. v v v 255.0))
  (^Vec4 [^double v ^double a] (Vec4. v v v a)))

(defn- named-color
  "The `Vec4` of a predefined color name; throws `ex-info` with `{:color k}` for an unknown name."
  ^Vec4 [k]
  (or (@color-presets-delay k)
      (throw (ex-info (str "Unknown color name: " k) {:color k}))))

(defn- nil-color-error
  "Throws `ex-info` with `{:color nil}`: nil is not a color."
  []
  (throw (ex-info "nil is not a color" {:color nil})))

(defn- color-name-string->color
  "The `Vec4` of a predefined color name given as a string, case-insensitive, names without a namespace only; nil otherwise."
  [^String input]
  (let [k (keyword (.toLowerCase input))]
    (when (nil? (namespace k))
      (@color-presets-delay k))))

(defn- string->color
  "Parses hex notation or a color name; throws `ex-info` with `{:color input}` otherwise."
  ^Vec4 [^String input]
  (or (hex-string->color input)
      (color-name-string->color input)
      (throw (ex-info (str "Invalid color string: " input) {:color input}))))

(defn- packed-alpha
  "Alpha of a packed ARGB integer: the top byte, or 255 when the top byte is 0."
  ^double [^long c]
  (let [a (bit-and 0xff (m/>> c 24))]
    (if (zero? a) 255.0 a)))

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
  (to-color ^Vec4 [n] (named-color n))
  (to-awt-color ^Color [n] (to-awt-color (named-color n)))
  (luma ^double [n] (pr/luma (named-color n)))
  (red ^double [n] (pr/red (named-color n)))
  (green ^double [n] (pr/green (named-color n)))
  (blue ^double [n] (pr/blue (named-color n)))
  (alpha ^double [n] (pr/alpha (named-color n)))
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
  (to-color [_] (nil-color-error))
  (to-awt-color [_] (nil-color-error))
  (luma [_] (nil-color-error))
  (alpha [_] (nil-color-error))
  (red [_] (nil-color-error))
  (green [_] (nil-color-error))
  (blue [_] (nil-color-error))
  Long
  (alpha ^double [^long c] (packed-alpha c))
  (red ^double [^long c] (bit-and 0xff (m/>> c 16)))
  (green ^double [^long c] (bit-and 0xff (m/>> c 8)))
  (blue ^double [^long c] (bit-and 0xff c))
  (to-color ^Vec4 [^long c] (Vec4. (bit-and 0xff (m/>> c 16))
                                   (bit-and 0xff (m/>> c 8))
                                   (bit-and 0xff c)
                                   (packed-alpha c)))
  (to-awt-color ^Color [c] (pr/to-awt-color (pr/to-color c)))
  (luma ^double [c] (pr/luma (pr/to-color c)))
  Integer
  (alpha ^double [c] (packed-alpha (long c)))
  (red ^double [c] (bit-and 0xff (m/>> ^int c 16)))
  (green ^double [c] (bit-and 0xff (m/>> ^int c 8)))
  (blue ^double [c] (bit-and 0xff ^int c))
  (to-color ^Vec4 [c] (Vec4. (bit-and 0xff (m/>> ^int c 16))
                             (bit-and 0xff (m/>> ^int c 8))
                             (bit-and 0xff ^int c)
                             (packed-alpha (long c))))
  (to-awt-color ^Color [c] (pr/to-awt-color (pr/to-color c)))
  (luma ^double [c] (pr/luma (pr/to-color c)))
  String
  (alpha ^double [^String c] (.w (string->color c)))
  (red ^double [^String c] (.x (string->color c)))
  (green ^double [^String c] (.y (string->color c)))
  (blue ^double [^String c] (.z (string->color c)))
  (to-color ^Vec4 [^String c] (string->color c))
  (to-awt-color ^Color [^String c] (pr/to-awt-color (string->color c)))
  (luma ^double [^String c] (pr/luma (string->color c)))
  APersistentVector
  (to-color ^Vec4 [c] (case (count c)
                        0 (Vec4. 0.0 0.0 0.0 255.0)
                        1 (gray-vec4 (c 0))
                        2 (gray-vec4 (c 0) (c 1))
                        3 (Vec4. (c 0) (c 1) (c 2) 255.0)
                        (Vec4. (c 0) (c 1) (c 2) (c 3))))
  (alpha ^double [c] (pr/alpha (pr/to-color c)))
  (red ^double [c] (pr/red (pr/to-color c)))
  (green ^double [c] (pr/green (pr/to-color c)))
  (blue ^double [c] (pr/blue (pr/to-color c)))
  (to-awt-color ^Color [c] (pr/to-awt-color (pr/to-color c)))
  (luma ^double [c] (pr/luma (pr/to-color c)))
  ISeq
  (to-color ^Vec4 [c] (case (count (take 4 c))
                        0 (Vec4. 0.0 0.0 0.0 255.0)
                        1 (gray-vec4 (first c))
                        2 (gray-vec4 (first c) (second c))
                        3 (Vec4. (first c) (second c) (nth c 2) 255.0)
                        (Vec4. (first c) (second c) (nth c 2) (nth c 3 255.0))))
  (alpha ^double [c] (pr/alpha (pr/to-color c)))
  (red ^double [c] (pr/red (pr/to-color c)))
  (green ^double [c] (pr/green (pr/to-color c)))
  (blue ^double [c] (pr/blue (pr/to-color c)))
  (to-awt-color ^Color [c] (pr/to-awt-color (pr/to-color c)))
  (luma ^double [c] (pr/luma (pr/to-color c)))
  Seqable
  (to-color ^Vec4 [c] (pr/to-color (seq c)))
  (alpha ^double [c] (pr/alpha (seq c)))
  (red ^double [c] (pr/red (seq c)))
  (green ^double [c] (pr/green (seq c)))
  (blue ^double [c] (pr/blue (seq c)))
  (to-awt-color ^Color [c] (pr/to-awt-color (seq c)))
  (luma ^double [c] (pr/luma (seq c))))

(extend-type (Class/forName "[D")
  pr/ColorProto
  (to-color ^Vec4 [c] (case (alength ^doubles c)
                        0 (Vec4. 0.0 0.0 0.0 255.0)
                        1 (gray-vec4 (aget ^doubles c 0))
                        2 (gray-vec4 (aget ^doubles c 0) (aget ^doubles c 1))
                        3 (Vec4. (aget ^doubles c 0) (aget ^doubles c 1) (aget ^doubles c 2) 255.0)
                        (Vec4. (aget ^doubles c 0) (aget ^doubles c 1) (aget ^doubles c 2) (aget ^doubles c 3))))
  (alpha ^double [c] (pr/alpha (pr/to-color c)))
  (red ^double [c] (pr/red (pr/to-color c)))
  (green ^double [c] (pr/green (pr/to-color c)))
  (blue ^double [c] (pr/blue (pr/to-color c)))
  (to-awt-color ^Color [c] (pr/to-awt-color (pr/to-color c)))
  (luma ^double [c] (pr/luma (pr/to-color c))))

(extend-type (Class/forName "[I")
  pr/ColorProto
  (to-color ^Vec4 [c] (case (alength ^ints c)
                        0 (Vec4. 0.0 0.0 0.0 255.0)
                        1 (gray-vec4 (aget ^ints c 0))
                        2 (gray-vec4 (aget ^ints c 0) (aget ^ints c 1))
                        3 (Vec4. (aget ^ints c 0) (aget ^ints c 1) (aget ^ints c 2) 255.0)
                        (Vec4. (aget ^ints c 0) (aget ^ints c 1) (aget ^ints c 2) (aget ^ints c 3))))
  (alpha ^double [c] (pr/alpha (pr/to-color c)))
  (red ^double [c] (pr/red (pr/to-color c)))
  (green ^double [c] (pr/green (pr/to-color c)))
  (blue ^double [c] (pr/blue (pr/to-color c)))
  (to-awt-color ^Color [c] (pr/to-awt-color (pr/to-color c)))
  (luma ^double [c] (pr/luma (pr/to-color c))))

(extend-type (Class/forName "[F")
  pr/ColorProto
  (to-color ^Vec4 [c] (case (alength ^floats c)
                        0 (Vec4. 0.0 0.0 0.0 255.0)
                        1 (gray-vec4 (aget ^floats c 0))
                        2 (gray-vec4 (aget ^floats c 0) (aget ^floats c 1))
                        3 (Vec4. (aget ^floats c 0) (aget ^floats c 1) (aget ^floats c 2) 255.0)
                        (Vec4. (aget ^floats c 0) (aget ^floats c 1) (aget ^floats c 2) (aget ^floats c 3))))
  (alpha ^double [c] (pr/alpha (pr/to-color c)))
  (red ^double [c] (pr/red (pr/to-color c)))
  (green ^double [c] (pr/green (pr/to-color c)))
  (blue ^double [c] (pr/blue (pr/to-color c)))
  (to-awt-color ^Color [c] (pr/to-awt-color (pr/to-color c)))
  (luma ^double [c] (pr/luma (pr/to-color c))))

(extend-type (Class/forName "[J")
  pr/ColorProto
  (to-color ^Vec4 [c] (case (alength ^longs c)
                        0 (Vec4. 0.0 0.0 0.0 255.0)
                        1 (gray-vec4 (aget ^longs c 0))
                        2 (gray-vec4 (aget ^longs c 0) (aget ^longs c 1))
                        3 (Vec4. (aget ^longs c 0) (aget ^longs c 1) (aget ^longs c 2) 255.0)
                        (Vec4. (aget ^longs c 0) (aget ^longs c 1) (aget ^longs c 2) (aget ^longs c 3))))
  (alpha ^double [c] (pr/alpha (pr/to-color c)))
  (red ^double [c] (pr/red (pr/to-color c)))
  (green ^double [c] (pr/green (pr/to-color c)))
  (blue ^double [c] (pr/blue (pr/to-color c)))
  (to-awt-color ^Color [c] (pr/to-awt-color (pr/to-color c)))
  (luma ^double [c] (pr/luma (pr/to-color c))))

;;

(defn format-hex
  "Formats a color as a CSS hex string.

  - `c` color, any type accepted by [[to-color]]

  Returns lowercase `#rrggbb`, or `#rrggbbaa` when the rounded alpha is below 255. Channels and alpha are rounded and clamped to `[0,255]`. [[to-color]] reads the result back. See also [[pack]]."
  {:metadoc/categories meta-ops}
  ^String [c]
  (let [^Vec4 c (pr/to-color c)
        s (str "#" (format "%02x" (lclamp255 (.x c)))
               (format "%02x" (lclamp255 (.y c)))
               (format "%02x" (lclamp255 (.z c))))
        a (lclamp255 (.w c))]
    (if (< a 255) (str s (format "%02x" a)) s)))

(defn pack
  "Packs a color into a 32-bit ARGB integer, `0xAARRGGBB`.

  - `c` color, any type accepted by [[to-color]]

  Channels and alpha are rounded and clamped to `[0,255]`; the result is an `int`, negative when alpha is 128 or more. [[to-color]] reads an integer whose top byte is 0 as an opaque color, so a fully transparent color does not round-trip. See also [[format-hex]], [[quil]]."
  {:metadoc/categories meta-ops}
  [c]
  (unchecked-int (bit-or (m/<< (lclamp255 (pr/alpha c)) 24)
                         (m/<< (lclamp255 (pr/red c)) 16)
                         (m/<< (lclamp255 (pr/green c)) 8)
                         (lclamp255 (pr/blue c)))))

(def ^{:doc "Packs a color into a 32-bit ARGB integer, the color type of Quil and Processing. Alias of [[pack]]."
       :metadoc/categories meta-ops} quil pack)

(defn black?
  "Checks whether a color is black: red, green and blue, clamped to `[0,255]`, are all 0.

  - `c` color, any type accepted by [[to-color]]

  Alpha is ignored; negative channels count as 0. See also [[not-black?]]."
  [c]
  (let [^Vec4 v (clamp c)]
    (and (zero? (.x v))
         (zero? (.y v))
         (zero? (.z v)))))

(defn not-black?
  "Checks whether a color is not black: any of red, green and blue, clamped to `[0,255]`, is above 0.

  - `c` color, any type accepted by [[to-color]]

  Alpha is ignored. See also [[black?]]."
  [c]
  (let [^Vec4 v (clamp c)]
    (or (pos? (.x v))
        (pos? (.y v))
        (pos? (.z v)))))

;; ## Colorspace functions
;;
;; Conversion from RGB to specific color space always converts to range 0-255
;; Reverse conversion is not normalized and can exceed 0-255 range


;; ### CMY

(defn to-CMY
  "Converts an sRGB color to CMY, the subtractive complement of RGB.

  `C = 255 - R`, `M = 255 - G`, `Y = 255 - B`; the conversion is its own inverse.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: C, M, Y in `[0,255]`. See also [[from-CMY]], [[to-CMY*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (Vec4. (- 255.0 (.x c))
           (- 255.0 (.y c))
           (- 255.0 (.z c))
           (.w c))))

(def ^{:doc "CMY to sRGB, the same function as [[to-CMY]] (the conversion is its own inverse)." :metadoc/categories meta-conv} from-CMY to-CMY)
(def ^{:doc "The same as [[to-CMY]]: its channels are already in `[0,255]`." :metadoc/categories meta-conv} to-CMY* to-CMY)
(def ^{:doc "The same as [[from-CMY]]: its channels are already in `[0,255]`." :metadoc/categories meta-conv} from-CMY* to-CMY)

;; ### OHTA

(defn to-OHTA
  "Converts an sRGB color to the Ohta I1I2I3 space (Ohta, Kanade and Sakai 1980).

  `I1 = (R + G + B) / 3`, `I2 = (R - B) / 2`, `I3 = (2G - R - B) / 4`, on the 0-255 scale.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: I1 0 to 255, I2 -127.5 to 127.5, I3 -127.5 to 127.5. See also [[from-OHTA]], [[to-OHTA*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb] 
  (let [^Vec4 c (pr/to-color srgb)
        i1 (/ (+ (.x c) (.y c) (.z c)) 3.0)
        i2 (* 0.5 (- (.x c) (.z c)))
        i3 (* 0.25 (- (* 2.0 (.y c)) (.x c) (.z c)))]
    (Vec4. i1 i2 i3 (.w c))))

(def ^:private ^:const ohta-s (Vec4. 0.0 127.5 127.5 0.0))

(defn to-OHTA*
  "Converts an sRGB color to OHTA with channels mapped to `[0,255]`.

  The channels of [[to-OHTA]] with 127.5 added to the two chroma channels. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-OHTA*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb]
  (v/add (to-OHTA srgb) ohta-s))

(def ^{:private true :const true :tag 'double} c23- (/ -2.0 3.0))
(def ^{:private true :const true :tag 'double} c43 (/ 4.0 3.0))

(defn from-OHTA
  "Converts OHTA to sRGB, the inverse of [[to-OHTA]].

  - `c` OHTA color, any type accepted by [[to-color]], with channels in the ranges of [[to-OHTA]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-OHTA*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [ohta]
  (let [^Vec4 c (pr/to-color ohta)
        i1 (.x c)
        i2 (.y c)
        i3 (.z c)
        r (+ i1 i2 (* c23- i3))
        g (+ i1 (* c43 i3))
        b (+ i1 (- i2) (* c23- i3))]
    (Vec4. r g b (.w c))))

(defn from-OHTA*
  "Converts OHTA with channels in `[0,255]`, as produced by [[to-OHTA*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-OHTA]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [ohta*]
  (from-OHTA (v/sub (pr/to-color ohta*) ohta-s)))

;; ### sRGB

(defn to-sRGB
  "Encodes linear RGB to sRGB: each channel `255 · linear-to-srgb(c / 255)`, channels 0-255; alpha is kept.

  The input is taken as linear RGB (0-255). This is also the `:sRGB` entry of [[colorspaces]]: unlike the other `to-...` conversions, which take sRGB input, `:sRGB` treats its input as linear and encodes it, so it is the inverse of `:linearRGB`, not an identity.

  See also [[from-sRGB]], `clojure2d.color.rgb/linear-to-srgb`, [[->linear-RGB-to-RGB]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [linear-rgb]
  (let [^Vec4 c (pr/to-color linear-rgb)]
    (v/vec4 (-> (Vec3. (.x c) (.y c) (.z c))
                (v/div 255.0)
                (v/fmap rgb/linear-to-srgb)
                (v/mult 255.0))
            (.w c))))

(defn from-sRGB
  "Decodes sRGB to linear RGB: each channel `255 · srgb-to-linear(c / 255)`, channels 0-255; alpha is kept.

  Also available as [[to-linearRGB]] (the `:linearRGB` entry of [[colorspaces]]).

  See also [[to-sRGB]], `clojure2d.color.rgb/srgb-to-linear`, [[->RGB-to-linear-RGB]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb]
  (let [^Vec4 c (pr/to-color srgb)]
    (v/vec4 (-> (Vec3. (.x c) (.y c) (.z c))
                (v/div 255.0)
                (v/fmap rgb/srgb-to-linear)
                (v/mult 255.0))
            (.w c))))

(defn RGB-to-RGB1
  "Scales RGB channels from 0-255 to 0-1; alpha is kept (0-255).

  Accepts any value accepted by [[to-color]]. Returns `Vec4`.

  See also [[RGB1-to-RGB]]."
  ^Vec4 [rgb]
  (let [^Vec4 c (pr/to-color rgb)]
    (Vec4. (/ (.x c) 255.0)
           (/ (.y c) 255.0)
           (/ (.z c) 255.0)
           (.w c))))

(defn RGB1-to-RGB
  "Scales RGB channels from 0-1 to 0-255; alpha is kept.

  Accepts any value accepted by [[to-color]]. Returns `Vec4`.

  See also [[RGB-to-RGB1]]."
  ^Vec4 [rgb1]
  (let [^Vec4 c (pr/to-color rgb1)]
    (Vec4. (* (.x c) 255.0)
           (* (.y c) 255.0)
           (* (.z c) 255.0)
           (.w c))))

(def ^{:doc "The same as [[to-sRGB]]: its channels are already in `[0,255]`." :metadoc/categories meta-conv} to-sRGB* to-sRGB)
(def ^{:doc "The same as [[from-sRGB]]: its channels are already in `[0,255]`." :metadoc/categories meta-conv} from-sRGB* from-sRGB)

;; ### Oklab

(defn- linear->Oklab
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        l (m/cbrt (+ (* 0.4122214694707629 (.x c)) (* 0.5363325372617349 (.y c)) (* 0.051445993267502196 (.z c))))
        m (m/cbrt (+ (* 0.2119034958178251 (.x c)) (* 0.6806995506452345 (.y c)) (* 0.10739695353694051 (.z c))))
        s (m/cbrt (+ (* 0.08830245919005637 (.x c)) (* 0.2817188391361215 (.y c)) (* 0.6299787016738223 (.z c))))]
    (Vec4. (+ (* 0.21045426830931396 l) (* 0.7936177747023053 m) (* -0.0040720430116192585 s))
           (+ (* 1.9779985324311686 l) (* -2.42859224204858 m) (* 0.450593709617411 s))
           (+ (* 0.025904042465547734 l) (* 0.7827717124575297 m) (* -0.8086757549230774 s))
           (.w c))))

(defn to-Oklab
  "Converts an sRGB color to Oklab.

  Linear sRGB to LMS, cube root, to Lab, with the high-precision matrices of coloraide (rows summing exactly to 1, so grays have a = b = 0); B. Ottosson, A perceptual color space for image processing (2020).

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: L 0 to 1, a -0.234 to 0.276, b -0.312 to 0.199. See also [[from-Oklab]], [[to-Oklab*]]."
  ^Vec4 [srgb]
  (linear->Oklab (scale (from-sRGB srgb) 0.00392156862745098)))

(defn to-Oklab*
  "Converts an sRGB color to Oklab with channels mapped to `[0,255]`.

  Each channel of [[to-Oklab]] is mapped linearly from its range (L 0 to 1, a -0.234 to 0.276, b -0.312 to 0.199) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-Oklab*]]."
  ^Vec4 [srgb]
  (let [^Vec4 c (to-Oklab srgb)]
    (Vec4. (m/mnorm (.x c) 0.0 1.0 0.0 255.0)
           (m/mnorm (.y c) -0.23388758093655793 0.2762164082234283 0.0 255.0)
           (m/mnorm (.z c) -0.3115281656775778 0.19856971763168108 0.0 255.0)
           (.w c))))

(defn- Oklab->linear
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        l (m/cb (+ (.x c) (* 0.3963377773761749 (.y c)) (* 0.21580375730991364 (.z c))))
        m (m/cb (+ (.x c) (* -0.10556134581565857 (.y c)) (* -0.0638541728258133 (.z c))))
        s (m/cb (+ (.x c) (* -0.08948417752981186 (.y c)) (* -1.2914855480194092 (.z c))))]
    (Vec4. (+ (* 4.076741636075959  l) (* -3.307711539258062 m) (* 0.2309699031821041 s))
           (+ (* -1.2684379732850313 l) (* 2.6097573492876878 m) (* -0.3413193760026569 s))
           (+ (* -0.004196076138675526 l) (* -0.703418617935936 m) (* 1.7076146940746113 s))
           (.w c))))

(defn from-Oklab
  "Converts Oklab to sRGB, the inverse of [[to-Oklab]].

  - `c` Oklab color, any type accepted by [[to-color]], with channels in the ranges of [[to-Oklab]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-Oklab*]]."
  ^Vec4 [oklab]
  (to-sRGB (scale (Oklab->linear oklab) 255.0)))

(defn from-Oklab*
  "Converts Oklab with channels in `[0,255]`, as produced by [[to-Oklab*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-Oklab]]."
  ^Vec4 [oklab*]
  (let [^Vec4 c (pr/to-color oklab*)]
    (from-Oklab (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 1.0)
                       (m/mnorm (.y c) 0.0 255.0 -0.23388758093655793 0.2762164082234283)
                       (m/mnorm (.z c) 0.0 255.0 -0.3115281656775778 0.19856971763168108)
                       (.w c)))))

;;

(defn to-luma-color-hue
  "Converts a Lab-like color to its polar form: lightness, chroma and hue in degrees.

  `C = hypot(a, b)`, `H = atan2(b, a)` in `[0,360)`; the first channel and alpha are kept.

  - `to` (optional): a conversion from sRGB (for example [[to-LAB]]) applied to `c` first
  - `c` color, any type accepted by [[to-color]], channels `[L a b]`

  Returns a `Vec4` `[L C H alpha]`. For `a = b = 0` the hue is 0. See also [[from-luma-color-hue]], [[to-LCH]]."
  (^Vec4 [to c] (to-luma-color-hue (to c)))
  (^Vec4 [c]
   (let [^Vec4 cc (pr/to-color c)
         H (m/atan2 (.z cc) (.y cc))
         Hd (m/degrees (if (neg? H) (+ H m/TWO_PI) H))
         C (m/hypot-sqrt (.y cc) (.z cc))]
     (Vec4. (.x cc) C Hd (.w cc)))))

(defn from-luma-color-hue
  "Converts the polar form `[L C H]` (hue in degrees) back to a Lab-like color, the inverse of [[to-luma-color-hue]].

  `a = C cos(H)`, `b = C sin(H)`; the first channel and alpha are kept.

  - `from` (optional): a conversion to sRGB (for example [[from-LAB]]) applied to the result
  - `c` color, any type accepted by [[to-color]], channels `[L C H]`

  Returns a `Vec4` `[L a b alpha]`, or the result of `from`."
  (^Vec4 [from c] (from (from-luma-color-hue c)))
  (^Vec4 [c]
   (let [^Vec4 c (pr/to-color c)
         h (m/radians (.z c))]
     (Vec4. (.x c) (* (.y c) (m/cos h)) (* (.y c) (m/sin h)) (.w c)))))

;;

(defn to-Oklch
  "Converts an sRGB color to Oklch, the polar form of [[to-Oklab]].

  Hue in degrees; for grays the hue is undefined (rounding noise).

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: L 0 to 1, C 0 to 0.322, H 0 to 360. See also [[from-Oklch]], [[to-Oklch*]]."
  ^Vec4 [srgb]
  (to-luma-color-hue to-Oklab srgb))

(defn to-Oklch*
  "Converts an sRGB color to Oklch with channels mapped to `[0,255]`.

  Each channel of [[to-Oklch]] is mapped linearly from its range (L 0 to 1, C 0 to 0.322, H 0 to 360) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-Oklch*]]."
  ^Vec4 [srgb]
  (let [^Vec4 c (to-Oklch srgb)]
    (Vec4. (m/mnorm (.x c) 0.0 1.0 0.0 255.0)
           (m/mnorm (.y c) 0.0 0.3224909877053726 0.0 255.0)
           (m/mnorm (.z c) 0.0 359.9985606653069 0.0 255.0)
           (.w c))))

(defn from-Oklch
  "Converts Oklch to sRGB, the inverse of [[to-Oklch]].

  - `c` Oklch color, any type accepted by [[to-color]], with channels in the ranges of [[to-Oklch]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-Oklch*]]."
  ^Vec4 [oklch]
  (from-luma-color-hue from-Oklab oklch))

(defn from-Oklch*
  "Converts Oklch with channels in `[0,255]`, as produced by [[to-Oklch*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-Oklch]]."
  ^Vec4 [oklch*]
  (let [^Vec4 c (pr/to-color oklch*)]
    (from-Oklch (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 1.0)
                       (m/mnorm (.y c) 0.0 255.0 0.0 0.3224909877053726)
                       (m/mnorm (.z c) 0.0 255.0 0.0 359.9985606653069)
                       (.w c)))))

;; Okhsv, Okhsl, Okhwb
;; https://bottosson.github.io/posts/colorpicker/

(defn- toe
  ^double [^double x]
  (let [v (- (* 1.170873786407767 x) 0.206)]
    (* 0.5 (+ v (m/sqrt (+ (m/sq v) (* 0.14050485436893204 x)))))))

(defn- inv-toe
  ^double [^double x]
  (/ (+ (* x x) (* 0.206 x))
     (* 1.170873786407767 (+ x 0.03))))

(def ^{:private true :const true :tag 'double} oklab-achromatic-chroma
  "Oklab chroma below which a color counts as gray in Okhsl and Okhsv (rounding noise of the matrices)."
  1.0e-12)

(defn- oklab-achromatic?
  "Is the Oklab color black, white (L at 0 or 1) or gray (chroma below [[oklab-achromatic-chroma]])? Okhsl and Okhsv give saturation 0 for such colors, as coloraide."
  [^Vec4 lab]
  (or (<= (.x lab) 0.0) (>= (.x lab) 1.0)
      (< (m/hypot-sqrt (.y lab) (.z lab)) oklab-achromatic-chroma)))

(defn- ->ST
  ^Vec2 [^Vec2 cusp]
  (Vec2. (/ (.y cusp) (.x cusp))
         (/ (.y cusp) (- 1.0 (.x cusp)))))

(defmacro ^:private oklab-poly
  [k0 k1 k2 k3 k4 wl wm ws]
  `(Vec4. (+ ~k0 (* ~k1 ~'a) (* ~k2 ~'b) (* ~k3 ~'a ~'a) (* ~k4 ~'a ~'b)) ~wl ~wm ~ws))

(defn- compute-max-saturation
  ^double [^double a ^double b]
  (let [^Vec4 v (cond
                  (> (- (* -1.8817031 a)
                        (* 0.80936501 b)) 1.0)
                  (oklab-poly 1.19086277 1.76576728 0.59662641 0.75515197 0.56771245
                              4.076741636075959 -3.307711539258062 0.2309699031821041)

                  (> (- (* 1.8144408 a)
                        (* 1.19445267 b)) 1.0)
                  (oklab-poly 0.73956515 -0.45954404 0.08285427 0.12541073 -0.14503204
                              -1.2684379732850313 2.6097573492876878 -0.3413193760026569)

                  :else (oklab-poly 1.35733652 -0.00915799 -1.15130210 -0.50559606 0.00692167
		                    -0.004196076138675526 -0.703418617935936 1.7076146940746113))
        S (.x v)
        wl (.y v)
        wm (.z v)
        ws (.w v)
        kl (+ (* 0.3963377773761749 a) (* 0.21580375730991364 b))
        km (+ (* -0.10556134581565857 a) (* -0.0638541728258133 b))
        ks (+ (* -0.08948417752981186 a) (* -1.2914855480194092 b))
        l- (inc (* S kl))
        m- (inc (* S km))
        s- (inc (* S ks))
        l (* l- l- l-)
        m (* m- m- m-)
        s (* s- s- s-)
        lds (* 3.0 kl l- l-)
        mds (* 3.0 km m- m-)
        sds (* 3.0 ks s- s-)
        lds2 (* 6.0 kl kl l-)
        mds2 (* 6.0 km km m-)
        sds2 (* 6.0 ks ks s-)
        f (+ (* wl l) (* wm m) (* ws s))
        f1 (+ (* wl lds) (* wm mds) (* ws sds))
        f2 (+ (* wl lds2) (* wm mds2) (* ws sds2))]
    (- S (/ (* f f1)
            (- (* f1 f1) (* 0.5 f f2))))))

(defn- find-cusp
  ^Vec2 [^double a ^double b]
  (let [S-cusp (compute-max-saturation a b)
        ^Vec4 rgb-at-max (Oklab->linear (Vec4. 1.0 (* S-cusp a) (* S-cusp b) 1.0))
        L-cusp (m/cbrt (/ (max (.x rgb-at-max) (.y rgb-at-max) (.z rgb-at-max))))]
    (Vec2. L-cusp (* L-cusp S-cusp))))

(defn to-Okhsv
  "Converts an sRGB color to Okhsv, an HSV-like space built on Oklab.

  B. Ottosson, Two new color spaces for color picking (2021), with coloraide's constants. Hue in 0-1 (not degrees); black, white and grays have hue 0 and saturation 0. Saturation can slightly exceed 1 (up to 1.012) for blue hues on the gamut boundary, a property of Ottosson's approximation.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: H 0 to 1, S 0 to 1.012, V 0 to 1. See also [[from-Okhsv]], [[to-Okhsv*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb]
  (let [^Vec4 lab (to-Oklab srgb)]
    (if (oklab-achromatic? lab)
      (Vec4. 0.0 0.0 (toe (.x lab)) (.w lab))
      (let [L (.x lab)
            C (m/hypot-sqrt (.y lab) (.z lab))
            a- (/ (.y lab) C)
            b- (/ (.z lab) C)
            h (+ 0.5 (/ (* 0.5 (m/atan2 (- (.z lab)) (- (.y lab)))) m/PI))
            ^Vec2 ST-max (->ST (find-cusp a- b-))
            S-max (.x ST-max)
            T-max (.y ST-max)
            k (- 1.0 (/ 0.5 S-max))
            t (/ T-max (+ C (* L T-max)))
            Lv (* t L)
            Cv (* t C)
            Lvt (inv-toe Lv)
            Cvt (/ (* Cv Lvt) Lv)
            ^Vec4 rgb-scale (Oklab->linear (Vec4. Lvt (* a- Cvt) (* b- Cvt) 1.0))
            scale-L (m/cbrt (/ (max (.x rgb-scale) (.y rgb-scale) (.z rgb-scale) 0.0)))
            L (/ L scale-L)
            #_#_ C (/ C scale-L) ;; it's in original source code
            tL (toe L)
            #_#_ C (/ (* C tL) L) 
            L tL
            v (/ L Lv)
            s (* (+ 0.5 T-max)
                 (/ Cv (+ (* 0.5 T-max)
                          (* T-max k Cv))))]
        (Vec4. h s v (.w lab))))))

(defn to-Okhsv*
  "Converts an sRGB color to Okhsv with channels mapped to `[0,255]`.

  Each channel of [[to-Okhsv]] is mapped linearly from its range (H 0 to 1, S 0 to 1.012, V 0 to 1) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-Okhsv*]]."
  ^Vec4 [srgb]
  (let [^Vec4 c (to-Okhsv srgb)]
    (Vec4. (m/mnorm (.x c) 0.0 0.9999960018480747 0.0 255.0)
           (m/mnorm (.y c) 0.0 1.0118674635509015 0.0 255.0)
           (m/mnorm (.z c) 0.0 1.0000000000000084 0.0 255.0)
           (.w c))))

(defn from-Okhsv
  "Converts Okhsv to sRGB, the inverse of [[to-Okhsv]].

  - `c` Okhsv color, any type accepted by [[to-color]], with channels in the ranges of [[to-Okhsv]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-Okhsv*]]."
  ^Vec4 [okhsv]
  (let [^Vec4 hsv (pr/to-color okhsv)]
    (if (zero? (.z hsv))
      (Vec4. 0.0 0.0 0.0 (.w hsv))
      (let [h (* m/TWO_PI (.x hsv))
            a- (m/cos h)
            b- (m/sin h)
            ^Vec2 ST-max (->ST (find-cusp a- b-))
            S-max (.x ST-max)
            T-max (.y ST-max)
            k (- 1.0 (/ 0.5 S-max))
            r (/ (+ 0.5 (- T-max (* T-max k (.y hsv)))))
            Lv (- 1.0 (* 0.5 (.y hsv) r))
            Cv (* 0.5 (.y hsv) T-max r)
            L (* (.z hsv) Lv)
            C (* (.z hsv) Cv)
            Lvt (inv-toe Lv)
            Cvt (/ (* Cv Lvt) Lv)
            Lnew (inv-toe L)
            C (/ (* C Lnew) L)
            L Lnew
            ^Vec4 rgb-scale (Oklab->linear (Vec4. Lvt (* a- Cvt) (* b- Cvt) 1.0))
            scaleL (m/cbrt (/ (max (.x rgb-scale) (.y rgb-scale) (.z rgb-scale))))
            L (* L scaleL)
            C (* C scaleL)]
        (from-Oklab (Vec4. L (* C a-) (* C b-) (.w hsv)))))))

(defn from-Okhsv*
  "Converts Okhsv with channels in `[0,255]`, as produced by [[to-Okhsv*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-Okhsv]]."
  ^Vec4 [okhsv*]
  (let [^Vec4 c (pr/to-color okhsv*)]
    (from-Okhsv (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 0.9999960018480747)
                       (m/mnorm (.y c) 0.0 255.0 0.0 1.0118674635509015)
                       (m/mnorm (.z c) 0.0 255.0 0.0 1.0000000000000084)
                       (.w c)))))

;; Okhwb

(defn to-Okhwb
  "Converts an sRGB color to Okhwb, the HWB form of [[to-Okhsv]].

  Whiteness `(1 - s) v`, blackness `1 - v` (Ottosson); hue in 0-1. Whiteness can be slightly negative where Okhsv saturation exceeds 1.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: H 0 to 1, W -0.0112 to 1, B 0 to 1. See also [[from-Okhwb]], [[to-Okhwb*]]."
  ^Vec4 [srgb]
  (let [^Vec4 c (to-Okhsv srgb)]
    (Vec4. (.x c) (* (- 1.0 (.y c)) (.z c)) (- 1.0 (.z c)) (.w c))))

(defn to-Okhwb*
  "Converts an sRGB color to Okhwb with channels mapped to `[0,255]`.

  Each channel of [[to-Okhwb]] is mapped linearly from its range (H 0 to 1, W -0.0112 to 1, B 0 to 1) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-Okhwb*]]."
  ^Vec4 [srgb]
  (let [^Vec4 c (to-Okhwb srgb)]
    (Vec4. (m/mnorm (.x c) 0.0 0.9999960018480747 0.0 255.0)
           (m/mnorm (.y c) -0.011237511218579275 1.0 0.0 255.0)
           (m/mnorm (.z c) -8.43769498715119E-15 1.0 0.0 255.0)
           (.w c))))

(defn from-Okhwb
  "Converts Okhwb to sRGB, the inverse of [[to-Okhwb]].

  - `c` Okhwb color, any type accepted by [[to-color]], with channels in the ranges of [[to-Okhwb]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-Okhwb*]]."
  ^Vec4 [okhwb]
  (let [^Vec4 c (pr/to-color okhwb)
        v (- 1.0 (.z c))]
    (if (zero? v)
      (Vec4. 0.0 0.0 0.0 (.w c))
      (from-Okhsv (Vec4. (.x c) (- 1.0 (/ (.y c) v)) v (.w c))))))

(defn from-Okhwb*
  "Converts Okhwb with channels in `[0,255]`, as produced by [[to-Okhwb*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-Okhwb]]."
  ^Vec4 [okhwb*]
  (let [^Vec4 c (pr/to-color okhwb*)]
    (from-Okhwb (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 0.9999960018480747)
                       (m/mnorm (.y c) 0.0 255.0 -0.011237511218579275 1.0)
                       (m/mnorm (.z c) 0.0 255.0 -8.43769498715119E-15 1.0)
                       (.w c)))))

;; Okhsl

(defn- get-ST-mid
  ^Vec2 [^double a ^double b]
  (Vec2. (+ 0.11516993 (/ (+ 7.44778970 (* 4.15901240 b)
                             (* a (+ -2.19557347 (* 1.75198401 b)
                                     (* a (+ -2.13704948 (* -10.02301043 b)
                                             (* a (+ -4.24894561 (* 5.38770819 b)
                                                     (* 4.69891013 a))))))))))
         (+ 0.11239642 (/ (+ 1.61320320 (* -0.68124379 b)
                             (* a (+ 0.40370612 (* 0.90148123 b)
                                     (* a (+ -0.27087943 (* 0.61223990 b)
                                             (* a (+ 0.00299215 (* -0.45399568 b)
                                                     (* -0.14661872 a))))))))))))

(defn- find-gamut-intersection
  ^double [^double a ^double b ^double L ^Vec2 cusp]
  ;; L0 = L1 = L, C1 = 1
  (if (<= L (.x cusp))
    (/ (* (.y cusp) L) (.x cusp))
    (let [t (/ (* (.y cusp) (dec L)) (dec (.x cusp)))
          #_#_ dL 0.0
          #_#_ dC 1.0
          kl (+ (* 0.3963377773761749 a) (* 0.21580375730991364 b))
          km (+ (* -0.10556134581565857 a) (* -0.0638541728258133 b))
          ks (+ (* -0.08948417752981186 a) (* -1.2914855480194092 b))
          L (+ (* L (- 1.0 t)) (* L t))
          C t
          l- (+ L (* C kl))
          m- (+ L (* C km))
          s- (+ L (* C ks))
          l (* l- l- l-)
          m (* m- m- m-)
          s (* s- s- s-)
          ldt (* 3.0 kl l- l-)
          mdt (* 3.0 km m- m-)
          sdt (* 3.0 ks s- s-)
          ldt2 (* 6.0 kl kl l-)
          mdt2 (* 6.0 km km m-)
          sdt2 (* 6.0 ks ks s-)
          r (dec (+ (* 4.076741636075959 l) (* -3.307711539258062 m) (* 0.2309699031821041 s)))
          r1 (+ (* 4.076741636075959 ldt) (* -3.307711539258062 mdt) (* 0.2309699031821041 sdt))
          r2 (+ (* 4.076741636075959 ldt2) (* -3.307711539258062 mdt2) (* 0.2309699031821041 sdt2))
          ur (/ r1 (- (* r1 r1) (* 0.5 r r2)))
          tr (* -1.0 r ur)
          g (dec (+ (* -1.2684379732850313 l) (* 2.6097573492876878 m) (* -0.3413193760026569 s)))
          g1 (+ (* -1.2684379732850313 ldt) (* 2.6097573492876878 mdt) (* -0.3413193760026569 sdt))
          g2 (+ (* -1.2684379732850313 ldt2) (* 2.6097573492876878 mdt2) (* -0.3413193760026569 sdt2))
          ug (/ g1 (- (* g1 g1) (* 0.5 g g2)))
          tg (* -1.0 g ug)
          b(dec (+ (* -0.004196076138675526 l) (* -0.703418617935936 m) (* 1.7076146940746113 s)))
          b1 (+ (* -0.004196076138675526 ldt) (* -0.703418617935936 mdt) (* 1.7076146940746113 sdt))
          b2 (+ (* -0.004196076138675526 ldt2) (* -0.703418617935936 mdt2) (* 1.7076146940746113 sdt2))
          ub (/ b1 (- (* b1 b1) (* 0.5 b b2)))
          tb (* -1.0 b ub)
          tr (if (>= ur 0.0) tr Double/MAX_VALUE)
          tg (if (>= ug 0.0) tg Double/MAX_VALUE)
          tb (if (>= ub 0.0) tb Double/MAX_VALUE)]
      (+ t (min tr tg tb)))))

(defn- get-Cs
  ^Vec3 [^double L ^double a ^double b]
  (let [^Vec2 cusp (find-cusp a b)
        C-max (find-gamut-intersection a b L cusp)
        ^Vec2 ST-max (->ST cusp)
        L- (- 1.0 L)
        k (/ C-max (min (* L (.x ST-max)) (* L- (.y ST-max))))
        ^Vec2 ST-mid (get-ST-mid a b)
        Ca (* L (.x ST-mid))
        Cb (* L- (.y ST-mid))
        C-mid (* 0.9 k (m/sqrt (m/sqrt (/ (+ (/ (m/sq (* Ca Ca)))
                                             (/ (m/sq (* Cb Cb))))))))
        Ca (* L 0.4)
        Cb (* L- 0.8)
        C-0 (m/sqrt (/ (+ (/ (* Ca Ca))
                          (/ (* Cb Cb)))))]
    (Vec3. C-0 C-mid C-max)))

(defn to-Okhsl
  "Converts an sRGB color to Okhsl, an HSL-like space built on Oklab.

  B. Ottosson, Two new color spaces for color picking (2021), with coloraide's constants. Hue in 0-1 (not degrees); black, white and grays (Oklab chroma below 1e-12) have hue 0 and saturation 0. Saturation can slightly exceed 1 (up to 1.005) for blue hues on the gamut boundary, a property of Ottosson's approximation.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: H 0 to 1, S 0 to 1.005, L 0 to 1. See also [[from-Okhsl]], [[to-Okhsl*]]."
  ^Vec4 [srgb]
  (let [^Vec4 lab (to-Oklab srgb)]
    (if (oklab-achromatic? lab)
      (Vec4. 0.0 0.0 (toe (.x lab)) (.w lab))
      (let [C (m/hypot-sqrt (.y lab) (.z lab))
            a- (/ (.y lab) C)
            b- (/ (.z lab) C)
            L (.x lab)
            h (+ 0.5 (* 0.5 (/ (m/atan2 (- (.z lab)) (- (.y lab))) m/PI)))
            ^Vec3 cs (get-Cs L a- b-)
            C-0 (.x cs)
            C-mid (.y cs)
            C-max (.z cs)
            s (if (< C C-mid)
                (let [k1 (* 0.8 C-0)
                      k2 (- 1.0 (/ k1 C-mid))]
                  (* 0.8 (/ C (+ k1 (* k2 C)))))
                (let [k1 (/ (* 0.3125 C-mid C-mid) C-0)
                      k2 (- 1.0 (/ k1 (- C-max C-mid)))]
                  (+ 0.8 (* 0.2 (/ (- C C-mid)
                                   (+ k1 (* k2 (- C C-mid))))))))]
        (Vec4. h s (toe L) (.w lab))))))

(defn to-Okhsl*
  "Converts an sRGB color to Okhsl with channels mapped to `[0,255]`.

  Each channel of [[to-Okhsl]] is mapped linearly from its range (H 0 to 1, S 0 to 1.005, L 0 to 1) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-Okhsl*]]."
  ^Vec4 [srgb]
  (let [^Vec4 c (to-Okhsl srgb)]
    (Vec4. (m/mnorm (.x c) 0.0 0.9999960018480747 0.0 255.0)
           (m/mnorm (.y c) 0.0 1.004816253335688 0.0 255.0)
           (m/mnorm (.z c) 0.0 1.0 0.0 255.0)
           (.w c))))

(defn from-Okhsl
  "Converts Okhsl to sRGB, the inverse of [[to-Okhsl]].

  - `c` Okhsl color, any type accepted by [[to-color]], with channels in the ranges of [[to-Okhsl]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Lightness 0 gives black and lightness 1 white, for any hue and saturation. Alpha is kept. See also [[from-Okhsl*]]."
  ^Vec4 [okhsl]
  (let [^Vec4 hsl (pr/to-color okhsl)]
    (cond
      (zero? (.z hsl)) (Vec4. 0.0 0.0 0.0 (.w hsl))
      ;; lightness 1 is white for any saturation (1 - L = 0 in get-Cs)
      (== 1.0 (.z hsl)) (Vec4. 255.0 255.0 255.0 (.w hsl))
      :else
      (let [h (* m/TWO_PI (.x hsl))
            a- (m/cos h)
            b- (m/sin h)
            L (inv-toe (.z hsl))
            ^Vec3 cs (get-Cs L a- b-)
            C-0 (.x cs)
            C-mid (.y cs)
            C-max (.z cs)
            C (if (< (.y hsl) 0.8)
                (let [t (* 1.25 (.y hsl))
                      k1 (* 0.8 C-0)
                      k2 (- 1.0 (/ k1 C-mid))]
                  (/ (* t k1) (- 1.0 (* t k2))))
                (let [t (/ (- (.y hsl) 0.8) 0.2)
                      k1 (/ (* 0.3125 C-mid C-mid) C-0)
                      k2 (- 1.0 (/ k1 (- C-max C-mid)))]
                  (+ C-mid (/ (* t k1) (- 1.0 (* t k2))))))]
        (from-Oklab (Vec4. L (* C a-) (* C b-) (.w hsl)))))))

(defn from-Okhsl*
  "Converts Okhsl with channels in `[0,255]`, as produced by [[to-Okhsl*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-Okhsl]]."
  ^Vec4 [okhsl*]
  (let [^Vec4 c (pr/to-color okhsl*)]
    (from-Okhsl (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 0.9999960018480747)
                       (m/mnorm (.y c) 0.0 255.0 0.0 1.004816253335688)
                       (m/mnorm (.z c) 0.0 255.0 0.0 1.0)
                       (.w c)))))

;; ### XYZ

(defn ->XYZ-to-XYZ
  "Creates a chromatic adaptation converter of XYZ colors between two white points.

  Parameters:

  - `adaptation-method` (keyword, optional): `:bradford` (default), `:xyz-scaling`, `:von-kries`, `:sharp`, `:fairchild`, `:cat97`, `:cat97-colour`, `:cat97s`, `:cat2000`, `:cat02`, `:cat02brill2008`, `:cat16`, `:bianco2010` or `:bianco2010-pc`; see `clojure2d.color.whitepoints/chromatic-adaptation-methods`.
  - `source-wp`, `destination-wp` (`Vec4`): white points from `clojure2d.color.whitepoints`, for example `CIE-2-C` or `CIE-2-D65`.

  Returns a function of one XYZ color (any value accepted by [[to-color]], any scale, for example 0-100 as [[to-XYZ]]) returning the adapted XYZ as `Vec4`; alpha is kept.

  Throws `ex-info` with `{:method adaptation-method}` for an unknown method.

  See also `clojure2d.color.whitepoints/chromatic-adaptation-matrix`, [[->RGB-to-RGB]]."
  ([source-wp destination-wp] (->XYZ-to-XYZ :bradford source-wp destination-wp))
  ([adaptation-method source-wp destination-wp]
   (let [M (wp/chromatic-adaptation-matrix adaptation-method source-wp destination-wp)]
     (fn ^Vec4 [xyz]
       (let [^Vec4 c (pr/to-color xyz)
             ^Vec3 c3 (mat/mulv M (Vec3. (.x c) (.y c) (.z c)))]
         (Vec4. (.x c3) (.y c3) (.z c3) (.w c)))))))

(defn XYZ-to-XYZ1
  "Scales XYZ from the 0-100 range (`Y` of white = 100, as [[to-XYZ]]) to 0-1; alpha is kept.

  Accepts any value accepted by [[to-color]]. Returns `Vec4`.

  See also [[XYZ1-to-XYZ]]."
  ^Vec4 [xyz]
  (let [^Vec4 c (pr/to-color xyz)]
    (Vec4. (* 0.01 (.x c))
           (* 0.01 (.y c))
           (* 0.01 (.z c))
           (.w c))))

(defn XYZ1-to-XYZ
  "Scales XYZ from the 0-1 range to 0-100 (as [[to-XYZ]]); alpha is kept.

  Accepts any value accepted by [[to-color]]. Returns `Vec4`.

  See also [[XYZ-to-XYZ1]]."
  ^Vec4 [xyz1]
  (let [^Vec4 c (pr/to-color xyz1)]
    (Vec4. (* 100.0 (.x c))
           (* 100.0 (.y c))
           (* 100.0 (.z c))
           (.w c))))

(defn- to-XYZ-
  "Pure RGB->XYZ conversion without corrections."
  ^Vec3 [^Vec3 c]
  (Vec3. (+ (* (.x c) srgb->xyz-00) (* (.y c) srgb->xyz-01) (* (.z c) srgb->xyz-02))
         (+ (* (.x c) srgb->xyz-10) (* (.y c) srgb->xyz-11) (* (.z c) srgb->xyz-12))
         (+ (* (.x c) srgb->xyz-20) (* (.y c) srgb->xyz-21) (* (.z c) srgb->xyz-22))))

(defn to-XYZ1
  "Converts an sRGB color to CIE XYZ on the 0-1 scale (Y of white 1).

  Linear sRGB (channels decoded with `clojure2d.color.rgb/srgb-to-linear`) times the matrix of `(clojure2d.color.rgb/rgbs :sRGB)`, derived from the sRGB primaries and the CIE 1931 2° D65 white `wp/CIE-2-D65`; white maps exactly to that white point.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`; ranges for colors in the sRGB gamut: X 0 to 0.9505, Y 0 to 1, Z 0 to 1.0888. See also [[to-XYZ]], [[from-XYZ1]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb]
  (let [^Vec4 c (pr/to-color srgb)
        xyz-raw (to-XYZ- (-> (Vec3. (.x c) (.y c) (.z c))
                             (v/div 255.0)
                             (v/fmap rgb/srgb-to-linear)))]
    (v/vec4 xyz-raw (.w c))))

(defn to-XYZ
  "Converts an sRGB color to CIE XYZ on the 0-100 scale (Y of white 100).

  Linear sRGB (channels decoded with `clojure2d.color.rgb/srgb-to-linear`) times the matrix of `(clojure2d.color.rgb/rgbs :sRGB)`, derived from the sRGB primaries and the CIE 1931 2° D65 white `wp/CIE-2-D65`; white maps exactly to that white point. All XYZ-based colorspaces (LAB, LUV, Yxy, ...) start here.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`; ranges for colors in the sRGB gamut: X 0 to 95.047, Y 0 to 100, Z 0 to 108.883. See also [[from-XYZ]], [[to-XYZ*]], [[->RGB-to-XYZ]] (other RGB spaces)."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb]
  (XYZ1-to-XYZ (to-XYZ1 srgb)))

(defn to-XYZ*
  "Converts an sRGB color to XYZ with channels mapped to `[0,255]`.

  Each channel of [[to-XYZ]] is mapped linearly from 0 to the white point value (95.047, 100, 108.883) to `[0,255]`, so white maps to 255. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-XYZ*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb]
  (let [cc (to-XYZ srgb)]
    (Vec4. (m/mnorm (.x cc) 0.0 95.04715475817799 0.0 255.0)
           (m/mnorm (.y cc) 0.0 100.0 0.0 255.0)
           (m/mnorm (.z cc) 0.0 108.8829656285427 0.0 255.0)
           (.w cc))))

(def ^{:doc "Alias of [[to-XYZ*]]: XYZ with channels mapped to `[0,255]`." :metadoc/categories meta-conv} to-XYZ1* to-XYZ*)

(defn- from-XYZ-
  "Pure XYZ->RGB conversion."
  ^Vec3 [^Vec3 v]
  (Vec3. (+ (* (.x v) xyz->srgb-00) (* (.y v) xyz->srgb-01) (* (.z v) xyz->srgb-02))
         (+ (* (.x v) xyz->srgb-10) (* (.y v) xyz->srgb-11) (* (.z v) xyz->srgb-12))
         (+ (* (.x v) xyz->srgb-20) (* (.y v) xyz->srgb-21) (* (.z v) xyz->srgb-22))))

(defn from-XYZ1
  "Converts CIE XYZ on the 0-1 scale to sRGB, the inverse of [[to-XYZ1]].

  - `c` XYZ color, any type accepted by [[to-color]], Y of white 1

  Returns an RGB `Vec4`, not clamped: colors outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-XYZ]]."
  ^Vec4 [xyz1]
  (let [^Vec4 c (pr/to-color xyz1)
        rgb-raw (v/mult (v/fmap (from-XYZ- (Vec3. (.x c) (.y c) (.z c))) rgb/linear-to-srgb) 255.0)]
    (v/vec4 rgb-raw (.w c))))

(defn from-XYZ
  "Converts CIE XYZ on the 0-100 scale to sRGB, the inverse of [[to-XYZ]].

  - `c` XYZ color, any type accepted by [[to-color]], Y of white 100

  Returns an RGB `Vec4`, not clamped: colors outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-XYZ*]], [[->XYZ-to-RGB]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [xyz1] 
  (from-XYZ1 (XYZ-to-XYZ1 xyz1)))

(defn from-XYZ*
  "Converts XYZ with channels in `[0,255]`, as produced by [[to-XYZ*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-XYZ]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [xyz*]
  (let [^Vec4 c (pr/to-color xyz*)
        x (m/mnorm (.x c) 0.0 255.0 0.0 95.04715475817799)
        y (m/mnorm (.y c) 0.0 255.0 0.0 100.0)
        z (m/mnorm (.z c) 0.0 255.0 0.0 108.8829656285427)]
    (from-XYZ (Vec4. x y z (.w c)))))

(def ^{:doc "Alias of [[from-XYZ*]]." :metadoc/categories meta-conv} from-XYZ1* from-XYZ*)

;; https://en.wikipedia.org/wiki/CIE_1960_color_space#Relation_to_CIE_XYZ

(defn XYZ-to-UCS
  "Converts XYZ (0-100, as [[to-XYZ]]) to the CIE 1960 UCS tristimulus values.

  `U = 2X / 3`, `V = Y`, `W = (-X + 3Y + Z) / 2`, on XYZ scaled to 0-1.

  - `xyz` color, any type accepted by [[to-color]]

  Returns a `Vec4` `[U V W alpha]`. See also [[UCS-to-XYZ]], [[to-UCS]]."
  ^Vec4 [xyz]
  (let [^Vec4 c (XYZ-to-XYZ1 xyz)]
    (Vec4. (* m/TWO_THIRD (.x c))
           (.y c)
           (* -0.5 (- (.x c) (* 3.0 (.y c)) (.z c)))
           (.w c))))

(defn to-UCS
  "Converts an sRGB color to the CIE 1960 UCS tristimulus values U, V, W.

  `U = 2X / 3`, `V = Y`, `W = (-X + 3Y + Z) / 2`, from [[to-XYZ]] scaled to 0-1.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: U 0 to 0.634, V 0 to 1, W 0 to 1.569. See also [[from-UCS]], [[to-UCS*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb]
  (-> srgb to-XYZ XYZ-to-UCS))

(defn to-UCS*
  "Converts an sRGB color to UCS with channels mapped to `[0,255]`.

  Each channel of [[to-UCS]] is mapped linearly from its range (U 0 to 0.634, V 0 to 1, W 0 to 1.569) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-UCS*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb]
  (let [^Vec4 cc (to-UCS srgb)]
    (Vec4. (m/mnorm (.x cc) 0.0 0.6336476983878533 0.0 255.0)
           (* (.y cc) 255.0)
           (m/mnorm (.z cc) -0.0 1.5691790543518245 0.0 255.0)
           (.w cc))))

(defn UCS-to-XYZ
  "Converts CIE 1960 UCS tristimulus values to XYZ (0-100), the inverse of [[XYZ-to-UCS]].

  - `ucs` color, any type accepted by [[to-color]], channels `[U V W]`

  Returns a `Vec4` `[X Y Z alpha]`. See also [[from-UCS]]."
  ^Vec4 [ucs]
  (let [^Vec4 c (pr/to-color ucs)]
    (XYZ1-to-XYZ (Vec4. (* 1.5 (.x c))
                        (.y c)
                        (+ (* 1.5 (.x c))
                           (* -3.0 (.y c))
                           (* 2.0 (.z c)))
                        (.w c)))))

(defn from-UCS
  "Converts UCS to sRGB, the inverse of [[to-UCS]].

  - `c` UCS color, any type accepted by [[to-color]], with channels in the ranges of [[to-UCS]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-UCS*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [ucs]
  (-> ucs UCS-to-XYZ from-XYZ))

(defn from-UCS*
  "Converts UCS with channels in `[0,255]`, as produced by [[to-UCS*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-UCS]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [ucs*]
  (let [^Vec4 cc (pr/to-color ucs*)]
    (from-UCS (Vec4. (m/mnorm (.x cc) 0.0 255.0 0.0 0.6336476983878533)
                     (/ (.y cc) 255.0)
                     (m/mnorm (.z cc) 0.0 255.0 -0.0 1.5691790543518245)
                     (.w cc)))))

;; ### XYB, https://observablehq.com/@mattdesl/perceptually-smooth-multi-color-linear-gradients

(def ^{:private true :const true :tag 'double} xyb-bias
  "libjxl opsin absorbance bias of [[to-XYB]] (opsin matrix: libjxl kOpsinAbsorbanceMatrix / 255)."
  0.0037930732552754493)

(defn- convert-mix
  ^double [^double m]
  (-> m (max 0.0) m/cbrt (+ -0.15595420054924863)))

(defn to-XYB
  "Converts an sRGB color to XYB, the JPEG XL color space.

  libjxl `LinearRGBToXYB`: opsin absorbance matrix on linear RGB, bias 0.0037930732552754493, cube root; `X = (L - M) / 2`, `Y = (L + M) / 2`, `B = S`. B is the raw third channel, not the B - Y of the scaled XYB used for encoding.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: X -0.0154 to 0.0281, Y 0 to 0.845, B 0 to 0.845. See also [[from-XYB]], [[to-XYB*]]."
  ^Vec4 [srgb]
  (let [^Vec4 c (from-sRGB srgb)
        r (convert-mix (m/muladd 0.001176470588235294 (.x c)
                                 (m/muladd 0.00243921568627451 (.y c)
                                           (m/muladd 3.0588235294117644E-4 (.z c) xyb-bias))))
        g (convert-mix (m/muladd 9.019607843137256E-4 (.x c)
                                 (m/muladd 0.0027137254901960788 (.y c)
                                           (m/muladd 3.0588235294117644E-4 (.z c) xyb-bias))))
        b (convert-mix (m/muladd 9.545987813548164E-4 (.x c)
                                 (m/muladd 8.030095852743851E-4 (.y c)
                                           (m/muladd 0.002163960260821779 (.z c) xyb-bias))))]
    (Vec4. (* 0.5 (- r g)) (* 0.5 (+ r g)) b (.w c))))

(defn to-XYB*
  "Converts an sRGB color to XYB with channels mapped to `[0,255]`.

  Each channel of [[to-XYB]] is mapped linearly from its range (X -0.0154 to 0.0281, Y 0 to 0.845, B 0 to 0.845) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-XYB*]]."
  ^Vec4 [srgb]
  (let [^Vec4 c (to-XYB srgb)]
    (Vec4. (m/mnorm (.x c) -0.015386116472573375 0.02810008316127735 0.0 255.0)
           (m/mnorm (.y c) 0.0 0.8453085619621623 0.0 255.0)
           (m/mnorm (.z c) 0.0 0.8453085619621623 0.0 255.0)
           (.w c))))

(defn from-XYB
  "Converts XYB to sRGB, the inverse of [[to-XYB]].

  - `c` XYB color, any type accepted by [[to-color]], with channels in the ranges of [[to-XYB]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-XYB*]]."
  ^Vec4 [xyb]
  (let [^Vec4 c (pr/to-color xyb)
        gamma-r (- (+ (.y c) (.x c)) -0.15595420054924863)
        gamma-g (- (.y c) (.x c) -0.15595420054924863)
        gamma-b (- (.z c) -0.15595420054924863)
        mixed-r (- (* gamma-r gamma-r gamma-r) xyb-bias)
        mixed-g (- (* gamma-g gamma-g gamma-g) xyb-bias)
        mixed-b (- (* gamma-b gamma-b gamma-b) xyb-bias)]
    ;; exact inverse of the opsin matrix of to-XYB
    (to-sRGB (Vec4. (+ (* 2813.0495606831655 mixed-r) (* -2516.070696573549 mixed-g) (* -41.978864109616275 mixed-b))
                    (+ (* -829.8075821739781 mixed-r) (* 1126.7864462835944 mixed-g) (* -41.978864109616275 mixed-b))
                    (+ (* -933.0070781119886 mixed-r) (* 691.7953767137035 mixed-g) (* 496.211701398285 mixed-b))
                    (.w c)))))

(defn from-XYB*
  "Converts XYB with channels in `[0,255]`, as produced by [[to-XYB*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-XYB]]."
  ^Vec4 [xyb*]
  (let [^Vec4 c (pr/to-color xyb*)]
    (from-XYB (Vec4. (m/mnorm (.x c) 0.0 255.0 -0.015386116472573375 0.02810008316127735)
                     (m/mnorm (.y c) 0.0 255.0 0.0 0.8453085619621623)
                     (m/mnorm (.z c) 0.0 255.0 0.0 0.8453085619621623)
                     (.w c)))))

;; ### RYB
;; https://web.archive.org/web/20120302090118/http://www.insanit.net/tag/rgb-to-ryb/

(defn to-RYB
  "Converts an sRGB color to RYB, red-yellow-blue.

  Arah J. Leonard's algorithm (2009, insanit.net): removes the white, takes yellow from red and green, splits the remaining green, rescales and adds the white back. A heuristic, not a paint mixing model; `from-RYB` is its exact inverse.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: R, Y, B in `[0,255]`. See also [[from-RYB]], [[to-RYB*]]."
  ^Vec4 [srgb]
  (let [^Vec4 c (pr/to-color srgb)
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

(def ^{:doc "The same as [[to-RYB]]: its channels are already in `[0,255]`." :metadoc/categories meta-conv} to-RYB* to-RYB)

(defn from-RYB
  "Converts RYB to sRGB, the inverse of [[to-RYB]].

  - `c` RYB color, any type accepted by [[to-color]], with channels in the ranges of [[to-RYB]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-RYB*]]."
  ^Vec4 [ryb]
  (let [^Vec4 c (pr/to-color ryb)
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

(def ^{:doc "The same as [[from-RYB]]: its channels are already in `[0,255]`." :metadoc/categories meta-conv} from-RYB* from-RYB)

;; ### LAB

(def ^{:private true :const true :tag 'double} CIEEpsilon (/ 216.0 24389.0))
(def ^{:private true :const true :tag 'double} CIEK (/ 24389.0 27.0))

(defn- to-lab-correct
  "LAB correction"
  ^double [^double v]
  (if (> v CIEEpsilon)
    (m/cbrt v)
    (/ (+ 16.0 (* v CIEK)) 116.0)))

(defn XYZ-to-LAB
  "Converts XYZ (0-100, as [[to-XYZ]]) to CIE 1976 L*a*b* relative to a white point.

  - `xyz` color, any type accepted by [[to-color]]
  - `whitepoint` (`Vec4` `[X Z x y]`, optional): reference white from `clojure2d.color.whitepoints`, default `wp/CIE-2-D65`

  Returns a `Vec4` `[L a b alpha]`, L 0 to 100 for colors not brighter than the white. See also [[LAB-to-XYZ]], [[to-LAB]]."
  (^Vec4 [xyz] (XYZ-to-LAB xyz wp/CIE-2-D65))
  (^Vec4 [xyz ^Vec4 whitepoint]
   (let [xyz (XYZ-to-XYZ1 xyz)
         x (to-lab-correct (/ (.x xyz) (.x whitepoint)))
         y (to-lab-correct (.y xyz))
         z (to-lab-correct (/ (.z xyz) (.y whitepoint)))
         L (- (* y 116.0) 16.0)
         a (* 500.0 (- x y))
         b (* 200.0 (- y z))]
     (Vec4. L a b (.w xyz)))))

(defn to-LAB
  "Converts an sRGB color to CIE 1976 L*a*b*.

  Computed from [[to-XYZ]]; for other white points see [[XYZ-to-LAB]]. White point CIE 1931 2° D65.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: L 0 to 100, a -86.18 to 98.23, b -107.86 to 94.48. See also [[from-LAB]], [[to-LAB*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb]
  (-> srgb to-XYZ XYZ-to-LAB))

(defn to-LAB*
  "Converts an sRGB color to LAB with channels mapped to `[0,255]`.

  Each channel of [[to-LAB]] is mapped linearly from its range (L 0 to 100, a -86.18 to 98.23, b -107.86 to 94.48) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-LAB*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb]
  (let [cc (to-LAB srgb)]
    (Vec4. (m/mnorm (.x cc) 0.0 100.00000000000003 0.0 255.0)
           (m/mnorm (.y cc) -86.18286702469524 98.2341915216855 0.0 255.0)
           (m/mnorm (.z cc) -107.86017303214177 94.47797355519816 0.0 255.0)
           (.w cc))))

(defn- from-lab-correct
  "LAB correction"
  ^double [^double v]
  (let [v3 (* v v v)]
    (if (> v3 CIEEpsilon)
      v3
      (/ (- (* 116.0 v) 16.0) CIEK))))

(defn LAB-to-XYZ
  "Converts CIE 1976 L*a*b* to XYZ (0-100) relative to a white point, the inverse of [[XYZ-to-LAB]].

  - `lab` color, any type accepted by [[to-color]], channels `[L a b]`
  - `whitepoint` (`Vec4` `[X Z x y]`, optional): reference white from `clojure2d.color.whitepoints`, default `wp/CIE-2-D65`

  Returns a `Vec4` `[X Y Z alpha]`. See also [[from-LAB]]."
  (^Vec4 [lab] (LAB-to-XYZ lab wp/CIE-2-D65))
  (^Vec4 [lab ^Vec4 whitepoint]
   (let [^Vec4 c (pr/to-color lab)
         y (/ (+ (.x c) 16.0) 116.0)
         x (* (.x whitepoint) (from-lab-correct (+ y (/ (.y c) 500.0))))
         z (* (.y whitepoint) (from-lab-correct (- y (/ (.z c) 200.0))))]
     (XYZ1-to-XYZ (Vec4. x (from-lab-correct y) z (.w c))))))

(defn from-LAB
  "Converts LAB to sRGB, the inverse of [[to-LAB]].

  - `c` LAB color, any type accepted by [[to-color]], with channels in the ranges of [[to-LAB]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-LAB*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [lab]
  (-> lab LAB-to-XYZ from-XYZ))

(defn from-LAB*
  "Converts LAB with channels in `[0,255]`, as produced by [[to-LAB*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-LAB]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [lab*]
  (let [^Vec4 c (pr/to-color lab*)]
    (from-LAB (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 100.00000000000003)
                     (m/mnorm (.y c) 0.0 255.0 -86.18286702469524 98.2341915216855)
                     (m/mnorm (.z c) 0.0 255.0 -107.86017303214177 94.47797355519816)
                     (.w c)))))

;;

(defn XYZ-to-LUV
  "Converts XYZ (0-100, as [[to-XYZ]]) to CIE 1976 L*u*v* relative to a white point.

  - `xyz` color, any type accepted by [[to-color]]
  - `whitepoint` (`Vec4` `[X Z x y]`, optional): reference white from `clojure2d.color.whitepoints`, default `wp/CIE-2-D65`

  Returns a `Vec4` `[L u v alpha]`; black (`X + 15Y + 3Z = 0`) gives `[0 0 0]`. See also [[LUV-to-XYZ]], [[to-LUV]]."
  (^Vec4 [xyz] (XYZ-to-LUV xyz wp/CIE-2-D65))
  (^Vec4 [xyz ^Vec4 whitepoint]
   (let [^Vec4 cc (pr/to-color xyz)
         uv-factor (+ (.x cc) (* 15.0 (.y cc)) (* 3.0 (.z cc)))]
     (if (zero? uv-factor)
       (Vec4. 0.0 0.0 0.0 (.w cc))
       (let [uv-factor* (/ uv-factor)
             var-u (* 4.0 (.x cc) uv-factor*)
             var-v (* 9.0 (.y cc) uv-factor*)
             var-y (to-lab-correct (/ (.y cc) 100.0))
             ref-uv-factor (/ (+ (.x whitepoint) 15.0 (* 3.0 (.y whitepoint))))
             ref-u (* 4.0 (.x whitepoint) ref-uv-factor)
             ref-v (* 9.0 ref-uv-factor)
             L (- (* 116.0 var-y) 16.0)] 
         (Vec4. L
                (* 13.0 L (- var-u ref-u))
                (* 13.0 L (- var-v ref-v))
                (.w cc)))))))

(defn to-LUV
  "Converts an sRGB color to CIE 1976 L*u*v*.

  Computed from [[to-XYZ]]; for other white points see [[XYZ-to-LUV]]. White point CIE 1931 2° D65.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: L 0 to 100, u -83.08 to 175.02, v -134.10 to 107.40. See also [[from-LUV]], [[to-LUV*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb]
  (-> srgb to-XYZ XYZ-to-LUV))

(defn to-LUV*
  "Converts an sRGB color to LUV with channels mapped to `[0,255]`.

  Each channel of [[to-LUV]] is mapped linearly from its range (L 0 to 100, u -83.08 to 175.02, v -134.10 to 107.40) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-LUV*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb]
  (let [cc (to-LUV srgb)]
    (Vec4. (m/mnorm (.x cc) 0.0 100.00000000000003 0.0 255.0)
           (m/mnorm (.y cc) -83.07787320021112 175.01527629434406 0.0 255.0)
           (m/mnorm (.z cc) -134.1029792892179 107.39848324418001 0.0 255.0)
           (.w cc))))

(defn LUV-to-XYZ
  "Converts CIE 1976 L*u*v* to XYZ (0-100) relative to a white point, the inverse of [[XYZ-to-LUV]].

  - `luv` color, any type accepted by [[to-color]], channels `[L u v]`
  - `whitepoint` (`Vec4` `[X Z x y]`, optional): reference white from `clojure2d.color.whitepoints`, default `wp/CIE-2-D65`

  Returns a `Vec4` `[X Y Z alpha]`; `L = 0` gives black. See also [[from-LUV]]."
  (^Vec4 [luv] (LUV-to-XYZ luv wp/CIE-2-D65))
  (^Vec4 [luv ^Vec4 whitepoint]
   (let [^Vec4 c (pr/to-color luv)]
     (if (zero? (.x c))
       (Vec4. 0.0 0.0 0.0 (.w c))
       (let [ref-uv-factor (/ (+ (.x whitepoint) 15.0 (* 3.0 (.y whitepoint))))
             ref-u (* 4.0 (.x whitepoint) ref-uv-factor)
             ref-v (* 9.0 ref-uv-factor)
             var-y (from-lab-correct (/ (+ (.x c) 16.0) 116.0))
             var-u (+ ref-u (/ (.y c) (* 13.0 (.x c))))
             var-v (+ ref-v (/ (.z c) (* 13.0 (.x c))))
             Y (* 100.0 var-y)
             X (/ (* -9.0 Y var-u) (- (* (- var-u 4.0) var-v) (* var-u var-v)))]
         (Vec4. X
                Y
                (/ (- (* 9.0 Y) (* 15.0 var-v Y) (* var-v X)) (* 3.0 var-v))
                (.w c)))))))

(defn from-LUV
  "Converts LUV to sRGB, the inverse of [[to-LUV]].

  - `c` LUV color, any type accepted by [[to-color]], with channels in the ranges of [[to-LUV]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-LUV*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [luv]
  (-> luv LUV-to-XYZ from-XYZ))

(defn from-LUV*
  "Converts LUV with channels in `[0,255]`, as produced by [[to-LUV*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-LUV]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [luv*]
  (let [^Vec4 c (pr/to-color luv*)]
    (from-LUV (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 100.00000000000003)
                     (m/mnorm (.y c) 0.0 255.0 -83.07787320021112 175.01527629434406)
                     (m/mnorm (.z c) 0.0 255.0 -134.1029792892179 107.39848324418001)
                     (.w c)))))

;; HunterLab

(def ^{:private true :const true :tag 'double} Ka (/ 17500.0 198.04))
(def ^{:private true :const true :tag 'double} Kb (/ 7000.0 218.11))

(defn XYZ-to-HunterLAB
  "Converts XYZ (0-100, as [[to-XYZ]]) to Hunter Lab relative to a white point.

  Ka and Kb are computed from the white point: `175 / 198.04 * (Xn + Yn)`, `70 / 218.11 * (Yn + Zn)`.

  - `xyz` color, any type accepted by [[to-color]]
  - `whitepoint` (`Vec4` `[X Z x y]`, optional): reference white from `clojure2d.color.whitepoints`, default `wp/CIE-2-D65`

  Returns a `Vec4` `[L a b alpha]`; `Y = 0` gives `[0 0 0]`. See also [[HunterLAB-to-XYZ]], [[to-HunterLAB]]."
  (^Vec4 [xyz] (XYZ-to-HunterLAB xyz wp/CIE-2-D65))
  (^Vec4 [xyz ^Vec4 whitepoint]
   (let [cc (XYZ-to-XYZ1 xyz)]
     (if (zero? (.y cc))
       (Vec4. 0.0 0.0 0.0 (.w cc))
       (let [X (/ (.x cc) (.x whitepoint))
             sqrtY (m/sqrt (.y cc))
             Z (/ (.z cc) (.y whitepoint))
             ka (* Ka (m/inc (.x whitepoint)))
             kb (* Kb (m/inc (.y whitepoint)))]
         (Vec4. (* 100.0 sqrtY)
                (* ka (/ (- X (.y cc)) sqrtY))
                (* kb (/ (- (.y cc) Z) sqrtY))
                (.w cc)))))))

(defn to-HunterLAB
  "Converts an sRGB color to Hunter Lab (Hunter 1948).

  Computed from [[to-XYZ]]; Ka and Kb from the white point by the textbook formulas `175 / 198.04 * (Xn + Yn)` and `70 / 218.11 * (Yn + Zn)` (colour-science uses a table value of Kb for D65 differing by about 0.2). White point CIE 1931 2° D65.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: L 0 to 100, a -69.08 to 109.46, b -199.78 to 55.72. See also [[from-HunterLAB]], [[to-HunterLAB*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb]
  (-> srgb to-XYZ XYZ-to-HunterLAB))

(defn to-HunterLAB*
  "Converts an sRGB color to HunterLAB with channels mapped to `[0,255]`.

  Each channel of [[to-HunterLAB]] is mapped linearly from its range (L 0 to 100, a -69.08 to 109.46, b -199.78 to 55.72) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-HunterLAB*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb]
  (let [cc (to-HunterLAB srgb)]
    (Vec4. (m/mnorm (.x cc) 0.0 100.00000000000003 0.0 255.0)
           (m/mnorm (.y cc) -69.07993055735508 109.4569884467629 0.0 255.0)
           (m/mnorm (.z cc) -199.77816803831797 55.71963987860276 0.0 255.0)
           (.w cc))))

(defn HunterLAB-to-XYZ
  "Converts Hunter Lab to XYZ (0-100) relative to a white point, the inverse of [[XYZ-to-HunterLAB]].

  - `hunterlab` color, any type accepted by [[to-color]], channels `[L a b]`
  - `whitepoint` (`Vec4` `[X Z x y]`, optional): reference white from `clojure2d.color.whitepoints`, default `wp/CIE-2-D65`

  Returns a `Vec4` `[X Y Z alpha]`; `L = 0` gives black. See also [[from-HunterLAB]]."
  (^Vec4 [hunterlab] (HunterLAB-to-XYZ hunterlab wp/CIE-2-D65))
  (^Vec4 [hunterlab ^Vec4 whitepoint]
   (let [^Vec4 c (pr/to-color hunterlab)]
     (if (zero? (.x c))
       (Vec4. 0.0 0.0 0.0 (.w c))
       (let [Y (m/sq (/ (.x c) 100.0))
             sqrtY (m/sqrt Y)
             ka (* Ka (m/inc (.x whitepoint)))
             kb (* Kb (m/inc (.y whitepoint)))
             X (* (.x whitepoint) (+ (* (/ (.y c) ka) sqrtY) Y))
             Z (- (* (.y whitepoint) (- (* (/ (.z c) kb) sqrtY) Y)))]
         (XYZ1-to-XYZ (Vec4. X Y Z (.w c))))))))

(defn from-HunterLAB
  "Converts HunterLAB to sRGB, the inverse of [[to-HunterLAB]].

  - `c` HunterLAB color, any type accepted by [[to-color]], with channels in the ranges of [[to-HunterLAB]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-HunterLAB*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [hunterlab]
  (-> hunterlab HunterLAB-to-XYZ from-XYZ))

(defn from-HunterLAB*
  "Converts HunterLAB with channels in `[0,255]`, as produced by [[to-HunterLAB*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-HunterLAB]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [hunterlab*]
  (let [^Vec4 c (pr/to-color hunterlab*)]
    (from-HunterLAB (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 100.00000000000003)
                           (m/mnorm (.y c) 0.0 255.0 -69.07993055735508 109.4569884467629)
                           (m/mnorm (.z c) 0.0 255.0 -199.77816803831797 55.71963987860276)
                           (.w c)))))

;;

(defn XYZ-to-LCH
  "Converts XYZ (0-100, as [[to-XYZ]]) to CIE LCh(ab), the polar form of [[XYZ-to-LAB]].

  - `xyz` color, any type accepted by [[to-color]]
  - `whitepoint` (`Vec4` `[X Z x y]`, optional): reference white from `clojure2d.color.whitepoints`, default `wp/CIE-2-D65`

  Returns a `Vec4` `[L C H alpha]`, hue in degrees. See also [[LCH-to-XYZ]], [[to-LCH]]."
  (^Vec4 [xyz] (XYZ-to-LCH xyz wp/CIE-2-D65))
  (^Vec4 [xyz whitepoint]
   (to-luma-color-hue (XYZ-to-LAB xyz whitepoint))))

(defn to-LCH
  "Converts an sRGB color to CIE LCh(ab), the polar form of [[to-LAB]].

  Hue in degrees; for grays the hue is undefined (rounding noise). White point CIE 1931 2° D65.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: L 0 to 100, C 0 to 133.81, H 0 to 360. See also [[from-LCH]], [[to-LCH*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb]
  (to-luma-color-hue to-LAB srgb))

(defn to-LCH*
  "Converts an sRGB color to LCH with channels mapped to `[0,255]`.

  Each channel of [[to-LCH]] is mapped linearly from its range (L 0 to 100, C 0 to 133.81, H 0 to 360) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-LCH*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb]
  (let [cc (to-LCH srgb)]
    (Vec4. (m/mnorm (.x cc) 0.0 100.00000000000003 0.0 255.0)
           (m/mnorm (.y cc) 0.0 133.8075258647932 0.0 255.0)
           (m/mnorm (.z cc) 0.0 359.9932032350659 0.0 255.0)
           (.w cc))))

(defn LCH-to-XYZ
  "Converts CIE LCh(ab) (hue in degrees) to XYZ (0-100), the inverse of [[XYZ-to-LCH]].

  - `lch` color, any type accepted by [[to-color]], channels `[L C H]`
  - `whitepoint` (`Vec4` `[X Z x y]`, optional): reference white from `clojure2d.color.whitepoints`, default `wp/CIE-2-D65`

  Returns a `Vec4` `[X Y Z alpha]`. See also [[from-LCH]]."
  (^Vec4 [lch] (LCH-to-XYZ lch wp/CIE-2-D65))
  (^Vec4 [lch whitepoint]
   (LAB-to-XYZ (from-luma-color-hue lch) whitepoint)))

(defn from-LCH
  "Converts LCH to sRGB, the inverse of [[to-LCH]].

  - `c` LCH color, any type accepted by [[to-color]], with channels in the ranges of [[to-LCH]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-LCH*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [lch]
  (from-luma-color-hue from-LAB lch))

(defn from-LCH*
  "Converts LCH with channels in `[0,255]`, as produced by [[to-LCH*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-LCH]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [lch*]
  (let [^Vec4 c (pr/to-color lch*)]
    (from-LCH (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 100.00000000000003)
                     (m/mnorm (.y c) 0.0 255.0 0.0 133.8075258647932)
                     (m/mnorm (.z c) 0.0 255.0 0.0 359.9932032350659)
                     (.w c)))))

;; LCHuv

(defn XYZ-to-LCHuv
  "Converts XYZ (0-100, as [[to-XYZ]]) to CIE LCh(uv), the polar form of [[XYZ-to-LUV]].

  - `xyz` color, any type accepted by [[to-color]]
  - `whitepoint` (`Vec4` `[X Z x y]`, optional): reference white from `clojure2d.color.whitepoints`, default `wp/CIE-2-D65`

  Returns a `Vec4` `[L C H alpha]`, hue in degrees. See also [[LCHuv-to-XYZ]], [[to-LCHuv]]."
  (^Vec4 [xyz] (XYZ-to-LCHuv xyz wp/CIE-2-D65))
  (^Vec4 [xyz whitepoint]
   (to-luma-color-hue (XYZ-to-LUV xyz whitepoint))))

(defn to-LCHuv
  "Converts an sRGB color to CIE LCh(uv), the polar form of [[to-LUV]].

  Hue in degrees; for grays the hue is undefined (rounding noise). White point CIE 1931 2° D65.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: L 0 to 100, C 0 to 179.04, H 0 to 360. See also [[from-LCHuv]], [[to-LCHuv*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb]
  (to-luma-color-hue to-LUV srgb))

(defn to-LCHuv*
  "Converts an sRGB color to LCHuv with channels mapped to `[0,255]`.

  Each channel of [[to-LCHuv]] is mapped linearly from its range (L 0 to 100, C 0 to 179.04, H 0 to 360) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-LCHuv*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb]
  (let [cc (to-LCHuv srgb)]
    (Vec4. (m/mnorm (.x cc) 0.0 100.00000000000003 0.0 255.0)
           (m/mnorm (.y cc) 0.0 179.0416171138321 0.0 255.0)
           (m/mnorm (.z cc) 0.0 359.99882265236886 0.0 255.0)
           (.w cc))))

(defn LCHuv-to-XYZ
  "Converts CIE LCh(uv) (hue in degrees) to XYZ (0-100), the inverse of [[XYZ-to-LCHuv]].

  - `lchuv` color, any type accepted by [[to-color]], channels `[L C H]`
  - `whitepoint` (`Vec4` `[X Z x y]`, optional): reference white from `clojure2d.color.whitepoints`, default `wp/CIE-2-D65`

  Returns a `Vec4` `[X Y Z alpha]`. See also [[from-LCHuv]]."
  (^Vec4 [lchuv] (LCHuv-to-XYZ lchuv wp/CIE-2-D65))
  (^Vec4 [lchuv whitepoint]
   (LUV-to-XYZ (from-luma-color-hue lchuv) whitepoint)))

(defn from-LCHuv
  "Converts LCHuv to sRGB, the inverse of [[to-LCHuv]].

  - `c` LCHuv color, any type accepted by [[to-color]], with channels in the ranges of [[to-LCHuv]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-LCHuv*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [lchuv]
  (from-luma-color-hue from-LUV lchuv))

(defn from-LCHuv*
  "Converts LCHuv with channels in `[0,255]`, as produced by [[to-LCHuv*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-LCHuv]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [lchuv*]
  (let [^Vec4 c (pr/to-color lchuv*)]
    (from-LCHuv (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 100.00000000000003)
                       (m/mnorm (.y c) 0.0 255.0 0.0 179.0416171138321)
                       (m/mnorm (.z c) 0.0 255.0 0.0 359.99882265236886)
                       (.w c)))))

;; ### Yxy (xyY)

(defn XYZ-to-Yxy
  "Converts XYZ to CIE xyY: luminance Y (in the scale of the input) and chromaticity x, y.

  For black (`X + Y + Z = 0`) the chromaticity of the white point is returned (Lindbloom convention).

  - `xyz` color, any type accepted by [[to-color]]
  - `whitepoint` (`Vec4` `[X Z x y]`, optional): white point used for black, default `wp/CIE-2-D65`

  Returns a `Vec4` `[Y x y alpha]`. See also [[Yxy-to-XYZ]], [[to-Yxy]]."
  (^Vec4 [xyz] (XYZ-to-Yxy xyz wp/CIE-2-D65))
  (^Vec4 [xyz ^Vec4 whitepoint]
   (let [^Vec4 xyz (pr/to-color xyz)
         d (+ (.x xyz) (.y xyz) (.z xyz))]
     (if (zero? d)
       (Vec4. 0.0 (.z whitepoint) (.w whitepoint) (.w xyz))
       (Vec4. (.y xyz)
              (/ (.x xyz) d)
              (/ (.y xyz) d)
              (.w xyz))))))

(defn to-Yxy
  "Converts an sRGB color to CIE xyY, luminance Y and chromaticity x, y.

  Computed from [[to-XYZ]]; for black the white-point chromaticity is used (Lindbloom convention), so black round-trips. White point CIE 1931 2° D65.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: Y 0 to 100, x 0.15 to 0.64, y 0.06 to 0.60. See also [[from-Yxy]], [[to-Yxy*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb]
  (XYZ-to-Yxy (to-XYZ srgb)))

(defn to-Yxy*
  "Converts an sRGB color to Yxy with channels mapped to `[0,255]`.

  Each channel of [[to-Yxy]] is mapped linearly from its range (Y 0 to 100, x 0.15 to 0.64, y 0.06 to 0.60) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-Yxy*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb]
  (let [cc (to-Yxy srgb)]
    (Vec4. (m/mnorm (.x cc) 0.0 100.00000000000004 0.0 255.0)
           (m/mnorm (.y cc) 0.14999999999999994 0.6400000000000001 0.0 255.0)
           (m/mnorm (.z cc) 0.05999999999999998 0.6000000000000001 0.0 255.0)
           (.w cc))))

(defn Yxy-to-XYZ
  "Converts CIE xyY to XYZ (in the scale of Y), the inverse of [[XYZ-to-Yxy]].

  - `yxy` color, any type accepted by [[to-color]], channels `[Y x y]`

  Returns a `Vec4` `[X Y Z alpha]`; `Y = 0` gives black. Infinite for `y = 0` with `Y > 0`. See also [[from-Yxy]]."
  ^Vec4 [yxy]
  (let [^Vec4 c (pr/to-color yxy)]
    (if (zero? (.x c))
      (Vec4. 0.0 0.0 0.0 (.w c))
      (let [Yy (/ (.x c) (.z c))
            X (* (.y c) Yy) 
            Z (* (- 1.0 (.y c) (.z c)) Yy)]
        (Vec4. X (.x c) Z (.w c))))))

(defn from-Yxy
  "Converts Yxy to sRGB, the inverse of [[to-Yxy]].

  - `c` Yxy color, any type accepted by [[to-color]], with channels in the ranges of [[to-Yxy]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-Yxy*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [yxy]
  (from-XYZ (Yxy-to-XYZ yxy)))

(defn from-Yxy*
  "Converts Yxy with channels in `[0,255]`, as produced by [[to-Yxy*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-Yxy]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [yxy*]
  (let [^Vec4 c (pr/to-color yxy*)]
    (from-Yxy (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 100.00000000000004)
                     (m/mnorm (.y c) 0.0 255.0 0.14999999999999994 0.6400000000000001)
                     (m/mnorm (.z c) 0.0 255.0 0.05999999999999998 0.6000000000000001)
                     (.w c)))))

;; ### Munsell

(defn munsell-to-Yxy
  "Converts a Munsell colour to `Yxy` (by default chromatically adapted to D65).

  Parameters:

  - `munsell`: notation string, for example `4.2YR 8.1/5.3` or `N5`, or a specification `[hue value chroma code]`; see `clojure2d.color.munsell/munsell-spec`.
  - `options` (map, optional):
      - `:whitepoint` - white point (a `Vec4` from `clojure2d.color.whitepoints`) of the result, default `wp/CIE-2-D65`. Use `wp/CIE-2-C` for the Illuminant C values of the Munsell Renotation data.
      - `:adaptation` - chromatic adaptation method, default `:bradford`.
      - `:scale` - Munsell value scale of `Y`, default `:astm`; see `clojure2d.color.munsell/luminance`.

  Returns a `Vec4` `[Y x y 255.0]`, `Y` in `0-100`, directly usable with [[from-Yxy]] for the default white point. Chroma beyond the Munsell Renotation data is clamped to the maximum for the hue and value.

  Throws `ex-info` for an invalid notation or specification.

  See also [[munsell-to-sRGB]], `clojure2d.color.munsell/munsell-to-Yxy` (Illuminant C by default)."
  {:metadoc/categories meta-conv}
  (^Vec4 [munsell] (munsell-to-Yxy munsell nil))
  (^Vec4 [munsell options]
   (munsell/munsell-to-Yxy munsell (merge {:whitepoint wp/CIE-2-D65} options))))

(defn munsell-to-sRGB
  "Converts a Munsell colour to sRGB.

  The Illuminant C colour of the Munsell Renotation data is adapted to D65 and converted with [[from-Yxy]].

  Parameters:

  - `munsell`: notation string or specification, see [[munsell-to-Yxy]].
  - `options` (map, optional): `:adaptation` (default `:bradford`) and `:scale` (default `:astm`) as in [[munsell-to-Yxy]]. The white point is always D65.

  Returns a `Vec4` sRGB colour with channels in `0-255` and alpha `255`. Values are not clamped: many Munsell colours lie outside the sRGB gamut and give channels below `0` or above `255`.

  Throws `ex-info` for an invalid notation or specification.

  See also [[munsell-to-Yxy]]."
  {:metadoc/categories meta-conv}
  (^Vec4 [munsell] (munsell-to-sRGB munsell nil))
  (^Vec4 [munsell options]
   (from-Yxy (munsell-to-Yxy munsell (assoc options :whitepoint wp/CIE-2-D65)))))

;; UVW, https://en.wikipedia.org/wiki/CIE_1964_color_space

(defn- xy->uv
  (^Vec2 [^Vec2 xy] (xy->uv (.x xy) (.y xy)))
  (^Vec2 [^double x ^double y]
   (let [d (/ (+ (* 12.0 y)
                 (* -2.0 x)
                 3.0))]
     (Vec2. (* 4.0 x d)
            (* 6.0 y d)))))

(defn- uv->xy
  (^Vec2 [^Vec2 uv] (uv->xy (.x uv) (.y uv)))
  (^Vec2 [^double u ^double v]
   (let [d (/ (+ (* 2.0 u)
                 (* -8.0 v)
                 4.0))]
     (Vec2. (* 3.0 u d)
            (* 2.0 v d)))))

(defn XYZ-to-UVW
  "Converts XYZ (0-100, as [[to-XYZ]]) to CIE 1964 U*V*W* relative to a white point.

  `W = 25 Y^(1/3) - 17`, `U = 13 W (u - u0)`, `V = 13 W (v - v0)` with CIE 1960 u, v chromaticities; black takes the white chromaticity (U = V = 0, W = -17).

  - `xyz` color, any type accepted by [[to-color]]
  - `whitepoint` (`Vec4` `[X Z x y]`, optional): reference white from `clojure2d.color.whitepoints`, default `wp/CIE-2-D65`

  Returns a `Vec4` `[U V W alpha]`. See also [[UVW-to-XYZ]], [[to-UVW]]."
  (^Vec4 [xyz] (XYZ-to-UVW xyz wp/CIE-2-D65))
  (^Vec4 [xyz ^Vec4 whitepoint]
   (let [^Vec4 c (XYZ-to-Yxy xyz whitepoint)
         ^Vec2 uv0 (xy->uv (Vec2. (.z whitepoint) (.w whitepoint)))
         ^Vec2 uv (v/sub (xy->uv (Vec2. (.y c) (.z c))) uv0)
         W (- (* 25.0 (m/cbrt (.x c))) 17.0)
         W13 (* 13.0 W)]
     (Vec4. (* W13 (.x uv)) (* W13 (.y uv)) W (.w c)))))

(defn to-UVW
  "Converts an sRGB color to CIE 1964 U*V*W*.

  Computed from [[to-XYZ]] via the CIE 1960 u, v chromaticities; for black the white-point chromaticity is used. White point CIE 1931 2° D65.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: U -82.16 to 171.81, V -87.17 to 70.81, W -17 to 99.04. See also [[from-UVW]], [[to-UVW*]]."
  ^Vec4 [srgb]
  (XYZ-to-UVW (to-XYZ srgb)))

(defn to-UVW*
  "Converts an sRGB color to UVW with channels mapped to `[0,255]`.

  Each channel of [[to-UVW]] is mapped linearly from its range (U -82.16 to 171.81, V -87.17 to 70.81, W -17 to 99.04) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-UVW*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [srgb]
  (let [cc (to-UVW srgb)]
    (Vec4. (m/mnorm (.x cc) -82.16458690584015 171.8059820026561 0.0 255.0)
           (m/mnorm (.y cc) -87.16899888988799 70.81189146321279 0.0 255.0)
           (m/mnorm (.z cc) -17.0 99.03972084031949 0.0 255.0)
           (.w cc))))

(defn UVW-to-XYZ
  "Converts CIE 1964 U*V*W* to XYZ (0-100) relative to a white point, the inverse of [[XYZ-to-UVW]].

  For `W = 0` the chromaticity is undetermined; the white chromaticity is used.

  - `uvw` color, any type accepted by [[to-color]], channels `[U V W]`
  - `whitepoint` (`Vec4` `[X Z x y]`, optional): reference white from `clojure2d.color.whitepoints`, default `wp/CIE-2-D65`

  Returns a `Vec4` `[X Y Z alpha]`. See also [[from-UVW]]."
  (^Vec4 [uvw] (UVW-to-XYZ uvw wp/CIE-2-D65))
  (^Vec4 [uvw ^Vec4 whitepoint]
   (let [^Vec4 c (pr/to-color uvw)
         ^Vec2 uv0 (xy->uv (Vec2. (.z whitepoint) (.w whitepoint)))
         Y (m/cb (/ (+ 17.0 (.z c)) 25.0))
         ^Vec2 xy (if (zero? (.z c))
                    (uv->xy uv0)
                    (let [W13 (/ (* 13.0 (.z c)))]
                      (uv->xy (Vec2. (+ (* (.x c) W13) (.x uv0))
                                     (+ (* (.y c) W13) (.y uv0))))))]
     (Yxy-to-XYZ (Vec4. Y (.x xy) (.y xy) (.w c))))))

(defn from-UVW
  "Converts UVW to sRGB, the inverse of [[to-UVW]].

  - `c` UVW color, any type accepted by [[to-color]], with channels in the ranges of [[to-UVW]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-UVW*]]."
  ^Vec4 [uvw]
  (from-XYZ (UVW-to-XYZ uvw)))

(defn from-UVW*
  "Converts UVW with channels in `[0,255]`, as produced by [[to-UVW*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-UVW]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [uvw*]
  (let [^Vec4 cc (pr/to-color uvw*)]
    (from-UVW (Vec4. (m/mnorm (.x cc) 0.0 255.0 -82.16458690584015 171.8059820026561)
                     (m/mnorm (.y cc) 0.0 255.0 -87.16899888988799 70.81189146321279)
                     (m/mnorm (.z cc) 0.0 255.0 -17.0 99.03972084031949)
                     (.w cc)))))

;; ### LMS - normalized D65

(defn ->XYZ-to-LMS
  "Creates a converter from XYZ to the cone response (LMS) space of a chromatic adaptation method.

  Parameters:

  - `method` (keyword): a key of `clojure2d.color.whitepoints/chromatic-adaptation-methods`, for example `:bradford`, `:von-kries` or `:cat16`.

  Returns a function of one XYZ color (any value accepted by [[to-color]]) returning `Vec4` `[L M S alpha]` = `Ma · XYZ`, in the scale of the input. No white point normalisation is applied.

  Throws `ex-info` with `{:method method}` for an unknown method.

  See also [[->LMS-to-XYZ]], [[to-LMS]] (fixed von Kries matrix, from RGB)."
  [method]
  (let [m (first (wp/chromatic-adaptation-transform method))]
    (fn XYZ-to-LMS ^Vec4 [xyz]
      (let [^Vec4 xyz (pr/to-color xyz)]
        (v/vec4 (mat/mulv m (Vec3. (.x xyz) (.y xyz) (.z xyz))) (.w xyz))))))

(defn to-LMS
  "Converts an sRGB color to LMS cone responses with the Hunt-Pointer-Estevez matrix normalized to D65.

  Computed from [[to-XYZ]]; white is about (100, 100, 100). For other cone matrices see [[->XYZ-to-LMS]].

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: L 0 to 100.003, M 0 to 99.999, S 0 to 99.979. See also [[from-LMS]], [[to-LMS*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [c (to-XYZ c)]
    (Vec4. (+ (* 0.40024 (.x c)) (* 0.7076 (.y c)) (* -0.08081 (.z c)))
           (+ (* -0.2263 (.x c)) (* 1.16532 (.y c)) (* 0.0457 (.z c)))
           (* 0.91822 (.z c))
           (.w c))))

(defn to-LMS*
  "Converts an sRGB color to LMS with channels mapped to `[0,255]`.

  Each channel of [[to-LMS]] is mapped linearly from its range (L 0 to 100.003, M 0 to 99.999, S 0 to 99.979) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-LMS*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-LMS c)]
    (Vec4. (m/mnorm (.x cc) 0.0 100.00284076797065 0.0 255.0)
           (m/mnorm (.y cc) 0.0 99.99878040744876 0.0 255.0)
           (m/mnorm (.z cc) 0.0 99.9785166994405 0.0 255.0)
           (.w cc))))

(defn ->LMS-to-XYZ
  "Creates a converter from the cone response (LMS) space of a chromatic adaptation method back to XYZ.

  Parameters:

  - `method` (keyword): a key of `clojure2d.color.whitepoints/chromatic-adaptation-methods`, for example `:bradford`, `:von-kries` or `:cat16`.

  Returns a function of one LMS color (any value accepted by [[to-color]]) returning `Vec4` `[X Y Z alpha]` = `Ma-1 · LMS`; inverse of [[->XYZ-to-LMS]] for the same method.

  Throws `ex-info` with `{:method method}` for an unknown method.

  See also [[->XYZ-to-LMS]], [[from-LMS]]."
  [method]
  (let [m (second (wp/chromatic-adaptation-transform method))]
    (fn LMS-to-XYZ ^Vec4 [lms]
      (let [^Vec4 lms (pr/to-color lms)]
        (v/vec4 (mat/mulv m (Vec3. (.x lms) (.y lms) (.z lms))) (.w lms))))))

(defn from-LMS
  "Converts LMS to sRGB, the inverse of [[to-LMS]].

  - `c` LMS color, any type accepted by [[to-color]], with channels in the ranges of [[to-LMS]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-LMS*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-XYZ (Vec4. (+ (* 1.8599363874558397 (.x c))
                        (* -1.1293816185800916 (.y c))
                        (* 0.2198974095961933 (.z c)))
                     (+ (* 0.3611914362417676 (.x c))
                        (* 0.6388124632850422 (.y c))
                        (* -0.0000063705968386499 (.z c)))
                     (* 1.0890636230968613 (.z c))
                     (.w c)))))

(defn from-LMS*
  "Converts LMS with channels in `[0,255]`, as produced by [[to-LMS*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-LMS]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-LMS (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 100.00284076797065)
                     (m/mnorm (.y c) 0.0 255.0 0.0 99.99878040744876)
                     (m/mnorm (.z c) 0.0 255.0 0.0 99.9785166994405)
                     (.w c)))))

;; IPT
;;
;; https://www.researchgate.net/publication/221677980_Development_and_Testing_of_a_Color_Space_IPT_with_Improved_Hue_Uniformity

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
  "Converts an sRGB color to IPT (Ebner and Fairchild 1998).

  Computed from [[to-XYZ]] scaled to 0-1. White point CIE 1931 2° D65.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: I 0 to 1, P -0.45 to 0.66, T -0.75 to 0.65. See also [[from-IPT]], [[to-IPT*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [c (to-XYZ1 c)
        ^Vec3 LMS (-> (Vec3. (+ (* 0.4002 (.x c)) (* 0.7075 (.y c)) (* -0.0807 (.z c)))
                             (+ (* -0.228 (.x c)) (* 1.15 (.y c)) (* 0.0612 (.z c)))
                             (* 0.9184 (.z c)))
                      (v/fmap spow-043))]
    (Vec4. (+ (* 0.4 (.x LMS)) (* 0.4 (.y LMS)) (* 0.2 (.z LMS)))
           (+ (* 4.455 (.x LMS)) (* -4.851 (.y LMS)) (* 0.396 (.z LMS)))
           (+ (* 0.8056 (.x LMS)) (* 0.3572 (.y LMS)) (* -1.1628 (.z LMS)))
           (.w c))))

(defn to-IPT*
  "Converts an sRGB color to IPT with channels mapped to `[0,255]`.

  Each channel of [[to-IPT]] is mapped linearly from its range (I 0 to 1, P -0.45 to 0.66, T -0.75 to 0.65) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-IPT*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-IPT c)]
    (Vec4. (m/mnorm (.x cc) 0.0 0.9999878910004701 0.0 255.0)
           (m/mnorm (.y cc) -0.4533923140526134 0.6624340080755238 0.0 255.0)
           (m/mnorm (.z cc) -0.7484709719141499 0.6514647094410253 0.0 255.0)
           (.w cc))))

(defn from-IPT
  "Converts IPT to sRGB, the inverse of [[to-IPT]].

  - `c` IPT color, any type accepted by [[to-color]], with channels in the ranges of [[to-IPT]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-IPT*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        ^Vec3 LMS' (-> (Vec3. (+ (.x c) (* 0.0975689305146139 (.y c)) (* 0.2052264331645916 (.z c)))
                              (+ (.x c) (* -0.1138764854731471 (.y c)) (* 0.13321715836999806 (.z c)))
                              (+ (.x c) (* 0.0326151099170664 (.y c)) (* -0.6768871830691793 (.z c))))
                       (v/fmap spow-r043))]
    (from-XYZ1 (Vec4. (+ (* 1.8502429449432056 (.x LMS'))
                         (* -1.1383016378672328 (.y LMS'))
                         (* 0.23843495850870136 (.z LMS')))
                      (+ (* 0.3668307751713486 (.x LMS'))
                         (* 0.6438845448402355 (.y LMS'))
                         (* -0.010673443584379992 (.z LMS')))
                      (* 1.088850174216028 (.z LMS'))
                      (.w c)))))

(defn from-IPT*
  "Converts IPT with channels in `[0,255]`, as produced by [[to-IPT*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-IPT]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-IPT (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 0.9999878910004701)
                     (m/mnorm (.y c) 0.0 255.0 -0.4533923140526134 0.6624340080755238)
                     (m/mnorm (.z c) 0.0 255.0 -0.7484709719141499 0.6514647094410253)
                     (.w c)))))

;; IgPgTg
;; https://www.ingentaconnect.com/content/ist/jpi/2020/00000003/00000002/art00002#

(defn- spow-0427
  ^double [^double v]
  (spow v 0.427))

(defn- spow-r0427
  ^double [^double v]
  (spow v 2.34192037470726))

(defn to-IgPgTg
  "Converts an sRGB color to IgPgTg (Hellwig and Fairchild 2020).

  Computed from [[to-XYZ]] scaled to 0-1. White point CIE 1931 2° D65.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: Ig 0 to 0.97, Pg -0.35 to 0.39, Tg -0.41 to 0.44. See also [[from-IgPgTg]], [[to-IgPgTg*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [c (to-XYZ1 c)
        ^Vec3 LMS (-> (Vec3. (/ (+ (* 2.968 (.x c)) (* 2.741 (.y c)) (* -0.649 (.z c))) 18.36)
                             (/ (+ (* 1.237 (.x c)) (* 5.969 (.y c)) (* -0.173 (.z c))) 21.46)
                             (/ (+ (* -0.318 (.x c)) (* 0.387 (.y c)) (* 2.311 (.z c))) 19435.0))
                      (v/fmap spow-0427))]
    (Vec4. (+ (* 0.117 (.x LMS)) (* 1.464 (.y LMS)) (* 0.13 (.z LMS)))
           (+ (* 8.285 (.x LMS)) (* -8.361 (.y LMS)) (* 21.4 (.z LMS)))
           (+ (* -1.208 (.x LMS)) (* 2.412 (.y LMS)) (* -36.53 (.z LMS)))
           (.w c))))

(defn to-IgPgTg*
  "Converts an sRGB color to IgPgTg with channels mapped to `[0,255]`.

  Each channel of [[to-IgPgTg]] is mapped linearly from its range (Ig 0 to 0.97, Pg -0.35 to 0.39, Tg -0.41 to 0.44) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-IgPgTg*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-IgPgTg c)]
    (Vec4. (m/mnorm (.x cc) 0.0 0.9741526429696793 0.0 255.0)
           (m/mnorm (.y cc) -0.3538244852247028 0.39398506387930815 0.0 255.0)
           (m/mnorm (.z cc) -0.41178987146752777 0.43676723923156996 0.0 255.0)
           (.w cc))))

(def ^:private igpgtg-vec3 (Vec3. 18.36 21.46 19435.0))

(defn from-IgPgTg
  "Converts IgPgTg to sRGB, the inverse of [[to-IgPgTg]].

  - `c` IgPgTg color, any type accepted by [[to-color]], with channels in the ranges of [[to-IgPgTg]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-IgPgTg*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        ^Vec3 LMS' (-> (Vec3. (+ (* 0.5818464618992453 (.x c))
                                 (* 0.12331854793907805 (.y c))
                                 (* 0.07431308420320755 (.z c)))
                              (+ (* 0.6345481937914152 (.x c))
                                 (* -0.009437923746683726 (.y c))
                                 (* -0.0032707446752298776 (.z c)))
                              (+ (* 0.022656986516578295 (.x c))
                                 (* -0.004701151874826374 (.y c))
                                 (* -0.030048158824914566 (.z c))))
                       (v/fmap spow-r0427)
                       (v/emult igpgtg-vec3))]
    (from-XYZ1 (Vec4. (+ (* 0.4343486855574634 (.x LMS'))
                         (* -0.2063623701142843 (.y LMS'))
                         (* 0.1065303361735277 (.z LMS')))
                      (+ (* -0.08785463778363382 (.x LMS'))
                         (* 0.2084634664799235 (.y LMS'))
                         (* -0.009066845616854849 (.z LMS')))
                      (+ (* 0.07447971736457794 (.x LMS'))
                         (* -0.06330532030466153 (.y LMS'))
                         (* 0.4488903142176134 (.z LMS')))
                      (.w c)))))

(defn from-IgPgTg*
  "Converts IgPgTg with channels in `[0,255]`, as produced by [[to-IgPgTg*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-IgPgTg]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-IgPgTg (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 0.9741526429696793)
                        (m/mnorm (.y c) 0.0 255.0 -0.3538244852247028 0.39398506387930815)
                        (m/mnorm (.z c) 0.0 255.0 -0.41178987146752777 0.43676723923156996)
                        (.w c)))))

;; Jab https://www.osapublishing.org/oe/abstract.cfm?uri=oe-25-13-15131

(def ^{:private true :const true :tag 'double} jab-b 1.15)
(def ^{:private true :const true :tag 'double} jab-g 0.66)
(def ^{:private true :const true :tag 'double} jab-rb (/ jab-b))
(def ^{:private true :const true :tag 'double} jab-rg (/ jab-g))
(def ^{:private true :const true :tag 'double} jab-b- (dec jab-b))
(def ^{:private true :const true :tag 'double} jab-g- (dec jab-g))
(def ^{:private true :const true :tag 'double} jab-c1 (/ 3424.0 (m/fpow 2.0 12)))
(def ^{:private true :const true :tag 'double} jab-c2 (/ 2413.0 (m/fpow 2.0 7)))
(def ^{:private true :const true :tag 'double} jab-c3 (/ 2392.0 (m/fpow 2.0 7)))
(def ^{:private true :const true :tag 'double} jab-n (/ 2610.0 (m/fpow 2.0 14)))
(def ^{:private true :const true :tag 'double} jab-p (* 1.7 (/ 2523.0 (m/fpow 2.0 5))))
(def ^{:private true :const true :tag 'double} jab-rn (/ jab-n))
(def ^{:private true :const true :tag 'double} jab-rp (/ jab-p))
(def ^{:private true :const true :tag 'double} jab-d -0.56)
(def ^{:private true :const true :tag 'double} jab-d+ (inc jab-d))
(def ^{:private true :const true :tag 'double} jab-d0 1.6295499532821566e-11)

(defn- jab-lms->lms' 
  ^double [^double v]
  (let [v (m/pow (/ v 10000.0) jab-n)]
    (m/pow (/ (+ jab-c1 (* jab-c2 v))
              (inc (* jab-c3 v))) jab-p)))

(defn to-JAB
  "Converts an sRGB color to Jzazbz (Safdar et al. 2017).

  Computed from absolute XYZ with the white at 100 cd/m2 (colour-science convention; other implementations use 203 cd/m2).

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: J 0 to 0.167, a -0.093 to 0.109, b -0.156 to 0.115. See also [[from-JAB]], [[to-JAB*]]."
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
  "Converts an sRGB color to JAB with channels mapped to `[0,255]`.

  Each channel of [[to-JAB]] is mapped linearly from its range (J 0 to 0.167, a -0.093 to 0.109, b -0.156 to 0.115) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-JAB*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-JAB c)]
    (Vec4. (m/mnorm (.x cc) 0.0 0.16717359341064056 0.0 255.0)
           (m/mnorm (.y cc) -0.09286163878172667 0.10900444448852457 0.0 255.0)
           (m/mnorm (.z cc) -0.15632010151179077 0.11522920261649011 0.0 255.0)
           (.w cc))))

(defn- jab-lms'->lms
  "Inverse PQ of one Jzazbz LMS' channel; rounding noise around black (a slightly negative channel or base) gives 0 instead of NaN."
  ^double [^double v]
  (let [v (m/pow (max 0.0 v) jab-rp)]
    (* 10000.0 (m/pow (max 0.0 (/ (- jab-c1 v)
                                  (- (* jab-c3 v) jab-c2))) jab-rn))))

(defn from-JAB
  "Converts JAB to sRGB, the inverse of [[to-JAB]].

  - `c` JAB color, any type accepted by [[to-color]], with channels in the ranges of [[to-JAB]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-JAB*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        J+ (+ jab-d0 (.x c))
        I (/ J+ (- jab-d+ (* jab-d J+)))
        ^Vec3 LMS (-> (Vec3. (+ I (* 0.1386050432715393 (.y c)) (* 0.05804731615611886 (.z c)))
                             (+ I (* -0.1386050432715393 (.y c)) (* -0.05804731615611886 (.z c)))
                             (+ I (* -0.09601924202631895 (.y c)) (* -0.8118918960560388 (.z c))))
                      (v/fmap jab-lms'->lms))
        ^Vec3 XYZ' (Vec3. (+ (* 1.9242264357876069 (.x LMS)) (* -1.0047923125953657 (.y LMS)) (* 0.037651404030617994 (.z LMS)))
                          (+ (* 0.350316762094999 (.x LMS)) (* 0.7264811939316552 (.y LMS)) (* -0.06538442294808501 (.z LMS)))
                          (+ (* -0.09098281098284752 (.x LMS)) (* -0.3127282905230739 (.y LMS)) (* 1.5227665613052603 (.z LMS))))
        X (* jab-rb (+ (.x XYZ') (* jab-b- (.z XYZ'))))]
    (from-XYZ (Vec4. X
                     (* jab-rg (+ (.y XYZ') (* jab-g- X)))
                     (.z XYZ') (.w c)))))

(defn from-JAB*
  "Converts JAB with channels in `[0,255]`, as produced by [[to-JAB*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-JAB]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-JAB (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 0.16717359341064056)
                     (m/mnorm (.y c) 0.0 255.0 -0.09286163878172667 0.10900444448852457)
                     (m/mnorm (.z c) 0.0 255.0 -0.15632010151179077 0.11522920261649011)
                     (.w c)))))

;;

(defn to-JCH
  "Converts an sRGB color to JzCzhz, the polar form of [[to-JAB]].

  Hue in degrees; for grays the hue is undefined (rounding noise). Absolute white at 100 cd/m2.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: J 0 to 0.167, C 0 to 0.159, H 0 to 360. See also [[from-JCH]], [[to-JCH*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (to-luma-color-hue to-JAB c))

(defn to-JCH*
  "Converts an sRGB color to JCH with channels mapped to `[0,255]`.

  Each channel of [[to-JCH]] is mapped linearly from its range (J 0 to 0.167, C 0 to 0.159, H 0 to 360) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-JCH*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-JCH c)]
    (Vec4. (m/mnorm (.x cc) 0.0 0.16717359341064056 0.0 255.0)
           (m/mnorm (.y cc) 0.0 0.15934815044956527 0.0 255.0)
           (m/mnorm (.z cc) 0.0 359.99740455792465 0.0 255.0)
           (.w cc))))

(defn from-JCH
  "Converts JCH to sRGB, the inverse of [[to-JCH]].

  - `c` JCH color, any type accepted by [[to-color]], with channels in the ranges of [[to-JCH]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-JCH*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (from-luma-color-hue from-JAB c))

(defn from-JCH*
  "Converts JCH with channels in `[0,255]`, as produced by [[to-JCH*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-JCH]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-JCH (Vec4. (m/mnorm (.x c) 0.0 255.0 0.0 0.16717359341064056)
                     (m/mnorm (.y c) 0.0 255.0 0.0 0.15934815044956527)
                     (m/mnorm (.z c) 0.0 255.0 0.0 359.99740455792465)
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

(def ^{:private true :const true :tag 'double} n360->255 (/ 255.0 360.0))
(def ^{:private true :const true :tag 'double} n255->360 (/ 360.0 255.0))

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
  (m/wrap 0.0 360.0 h))

;; HSI

(defn to-HSI
  "Converts an sRGB color to HSI, hue, saturation and intensity.

  Intensity is the mean of the channels, saturation `1 - min / I`; hue in degrees on the RGB hexagon, 0 for grays.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: H 0 to 360, S 0 to 1, I 0 to 1. See also [[from-HSI]], [[to-HSI*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        hc (to-HC c)
        I (/ (+ (.x c) (.y c) (.z c)) 3.0)
        S (if (zero? I) 0.0
              (- 1.0 (/ (.w hc) I)))]
    (Vec4. (.x hc) S (/ I 255.0) (.w c))))

(defn from-HSI
  "Converts HSI to sRGB, the inverse of [[to-HSI]].

  - `c` HSI color, any type accepted by [[to-color]], with channels in the ranges of [[to-HSI]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-HSI*]]."
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

(def ^{:doc "Converts an sRGB color to HSI with channels mapped to `[0,255]`.

  Each channel of [[to-HSI]] is mapped linearly from its range (H 0 to 360, S 0 to 1, I 0 to 1) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-HSI*]]." :metadoc/categories meta-conv} to-HSI* (comp normalize-HSx to-HSI))
(def ^{:doc "Converts HSI with channels in `[0,255]`, as produced by [[to-HSI*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-HSI]]." :metadoc/categories meta-conv} from-HSI* (comp from-HSI denormalize-HSx pr/to-color))

;; HSV

(defn to-HSV
  "Converts an sRGB color to HSV (HSB), hue, saturation and value.

  Hexagonal hue in degrees (0 for grays), value `max`, saturation `(max - min) / max`.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: H 0 to 360, S 0 to 1, V 0 to 1. See also [[from-HSV]], [[to-HSV*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        hc (to-HC c)
        V (.z hc)
        S (if (zero? V) 0.0
              (/ (.y hc) V))]
    (Vec4. (.x hc) S (/ V 255.0) (.w c))))

(defn from-HSV
  "Converts HSV to sRGB, the inverse of [[to-HSV]].

  - `c` HSV color, any type accepted by [[to-color]], with channels in the ranges of [[to-HSV]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-HSV*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        C (* (.y c) (.z c))
        h' (/ (wrap-hue (.x c)) 60.0)
        X (* C (- 1.0 (m/abs (dec (mod h' 2.0)))))
        m (- (.z c) C)
        ^Vec3 rgb' (v/add (from-HCX h' C X) (Vec3. m m m))]
    (v/vec4 (v/mult rgb' 255.0) (.w c))))

(def ^{:doc "Converts an sRGB color to HSV with channels mapped to `[0,255]`.

  Each channel of [[to-HSV]] is mapped linearly from its range (H 0 to 360, S 0 to 1, V 0 to 1) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-HSV*]]." :metadoc/categories meta-conv} to-HSV* (comp normalize-HSx to-HSV))
(def ^{:doc "Converts HSV with channels in `[0,255]`, as produced by [[to-HSV*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-HSV]]." :metadoc/categories meta-conv} from-HSV* (comp from-HSV denormalize-HSx pr/to-color))

;; HSB = HSV

(def ^{:doc "Alias of [[to-HSV]]: HSB is another name of HSV." :metadoc/categories meta-conv} to-HSB to-HSV)
(def ^{:doc "Alias of [[from-HSV]]." :metadoc/categories meta-conv} from-HSB from-HSV)
(def ^{:doc "Alias of [[to-HSV*]]." :metadoc/categories meta-conv} to-HSB* to-HSV*)
(def ^{:doc "Alias of [[from-HSV*]]." :metadoc/categories meta-conv} from-HSB* from-HSV*)

;; paletton HSV

(declare paletton-hsv-to-rgb)
(declare paletton-rgb-to-hsv)

(defn to-PalettonHSV
  "Converts an sRGB color to the HSV variant used by [[paletton]].

  The author's reverse engineering of the Paletton color wheel: hue in degrees on the Paletton (RYB-like) wheel, saturation and value up to 2.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: H 0 to 360, S 0 to 2, V 0 to 2. See also [[from-PalettonHSV]], [[to-PalettonHSV*]]."
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (paletton-rgb-to-hsv (.x c) (.y c) (.z c) (.w c))))

(defn to-PalettonHSV*
  "Converts an sRGB color to PalettonHSV with channels mapped to `[0,255]`.

  Hue of [[to-PalettonHSV]] times 255/360, saturation and value times 127.5. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-PalettonHSV*]]."
  ^Vec4 [c]
  (let [^Vec4 c (to-PalettonHSV c)]
    (Vec4. (* n360->255 (.x c))
           (* 127.5 (.y c))
           (* 127.5 (.z c))
           (.w c))))

(defn from-PalettonHSV
  "Converts PalettonHSV to sRGB, the inverse of [[to-PalettonHSV]].

  - `c` PalettonHSV color, any type accepted by [[to-color]], with channels in the ranges of [[to-PalettonHSV]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-PalettonHSV*]]."
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (paletton-hsv-to-rgb (.x c) (.y c) (.z c) (.w c))))

(defn from-PalettonHSV*
  "Converts PalettonHSV with channels in `[0,255]`, as produced by [[to-PalettonHSV*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-PalettonHSV]]."
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-PalettonHSV (Vec4. (* n255->360 (.x c))
                             (/ (.y c) 127.5)
                             (/ (.z c) 127.5)
                             (.w c)))))

;; HSL


(defn to-HSL
  "Converts an sRGB color to HSL, hue, saturation and lightness.

  Hexagonal hue in degrees (0 for grays), lightness `(max + min) / 2`.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: H 0 to 360, S 0 to 1, L 0 to 1. See also [[from-HSL]], [[to-HSL*]]."
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
  "Converts HSL to sRGB, the inverse of [[to-HSL]].

  - `c` HSL color, any type accepted by [[to-color]], with channels in the ranges of [[to-HSL]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-HSL*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        C (* (.y c) (- 1.0 (m/abs (dec (+ (.z c) (.z c))))))
        h' (/ (wrap-hue (.x c)) 60.0)
        X (* C (- 1.0 (m/abs (dec (mod h' 2.0)))))
        m (- (.z c) (* 0.5 C))
        ^Vec3 rgb' (v/add (from-HCX h' C X) (Vec3. m m m))]
    (v/vec4 (v/mult rgb' 255.0) (.w c))))

(def ^{:doc "Converts an sRGB color to HSL with channels mapped to `[0,255]`.

  Each channel of [[to-HSL]] is mapped linearly from its range (H 0 to 360, S 0 to 1, L 0 to 1) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-HSL*]]." :metadoc/categories meta-conv} to-HSL* (comp normalize-HSx to-HSL))
(def ^{:doc "Converts HSL with channels in `[0,255]`, as produced by [[to-HSL*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-HSL]]." :metadoc/categories meta-conv} from-HSL* (comp from-HSL denormalize-HSx pr/to-color))

;; HCL
;; http://w3.uqo.ca/missaoui/Publications/TRColorSpace.zip

(defn to-HCL
  "Converts an sRGB color to HCL (Sarifuddin and Missaoui 2005).

  `Q = exp(0.03 min / max)`, `L = (Q max + (Q - 1) min) / 2`, `C = Q (|R - G| + |G - B| + |B - R|) / 3`, hue from the channel differences, in degrees from -180 to 180, 0 for grays. [[from-HCL]] solves for Q exactly instead of the paper's estimate, so colors round-trip.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: H -180 to 180, C 0 to 170, L 0 to 135.27. See also [[from-HCL]], [[to-HCL*]]."
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
            (and (>= rg- 0.0) (>= gb- 0.0)) (* H m/TWO_THIRD)
            (and (>= rg- 0.0) (neg? gb-)) (* 2.0 m/TWO_THIRD H)
            (and (neg? rg-) (>= gb- 0.0)) (+ (* 2.0 m/TWO_THIRD H) 180.0)
            (and (neg? rg-) (neg? gb-)) (- (* m/TWO_THIRD H) 180.0) 
            :else H)]
    (Vec4. H C L (.w c))))

(defn- hcl-q-step
  "One fixed-point step for Q of the HCL inverse: from Q to max, min (given C, L), then `exp(0.03 min / max)`."
  ^double [^double C ^double L ^double Q]
  (let [d (/ (* 1.5 C) Q)
        mx (/ (+ (* 2.0 L) (* (dec Q) d)) (dec (* 2.0 Q)))]
    (m/exp (* 0.03 (/ (- mx d) mx)))))

(defn- hcl->max-min
  "Largest and smallest RGB channel of an HCL color, solved exactly: the forward conversion gives `max - min = 3 C / (2 Q)` and `L = (Q max + (Q - 1) min) / 2` with `Q = exp(0.03 min / max)`; Q is the fixed point of [[hcl-q-step]], found with Steffensen (Aitken) acceleration, 2-3 steps to 1e-15. The published inverse (Sarifuddin and Missaoui, colour-science) estimates Q from C / L instead and round-trips only within about 0.06. Black (L = 0) gives zeros."
  ^Vec2 [^double C ^double L]
  (if (<= L 0.0)
    (Vec2. 0.0 0.0)
    (let [Q (double
             (loop [iter (long 0)
                    Q0 (m/exp 0.015)]
               (let [Q1 (hcl-q-step C L Q0)
                     Q2 (hcl-q-step C L Q1)
                     den (+ (- Q2 Q1 Q1) Q0)
                     Q (if (zero? den) Q2 (- Q0 (/ (m/sq (- Q1 Q0)) den)))]
                 (if (or (>= iter 20) (<= (m/abs (- Q Q0)) (* 1.0e-15 Q)))
                   Q
                   (recur (inc iter) Q)))))
          d (/ (* 1.5 C) Q)
          mx (/ (+ (* 2.0 L) (* (dec Q) d)) (dec (* 2.0 Q)))]
      (Vec2. mx (- mx d)))))

(defn from-HCL
  "Converts HCL to sRGB, the inverse of [[to-HCL]].

  Q is solved exactly by fixed-point iteration (the published inverse estimates it from C / L and round-trips only within about 0.06); black gives black.

  - `c` HCL color, any type accepted by [[to-color]], with channels in the ranges of [[to-HCL]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-HCL*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        H       (m/wrap -180.0 180.0 (.x c))
        ^Vec2 mx-mn (hcl->max-min (.y c) (.z c))
        mx      (.x mx-mn)
        mn      (.y mx-mn)]
    (cond
      (<= mx 0.0)          (Vec4. 0.0 0.0 0.0 (.w c))
      (<= 0.0 H 60.0)      (let [t (m/tan (m/radians (* 1.5 H)))]
                             (Vec4. mx (/ (+ (* mx t) mn) (inc t)) mn (.w c)))
      (<= 60.0 H 120.0)    (let [t (m/tan (m/radians (* 0.75 (- H 180.0))))]
                             (Vec4. (/ (- (* mx (inc t)) mn) t) mx mn (.w c)))
      (<= 120.0 H 180.0)   (let [t (m/tan (m/radians (* 0.75 (- H 180.0))))]
                             (Vec4. mn mx (- (* mx (inc t)) (* mn t)) (.w c)))
      (<= -60.0 H 0.0)     (let [t (m/tan (m/radians (* 0.75 H)))]
                             (Vec4. mx mn (- (* mn (inc t)) (* mx t)) (.w c)))
      (<= -120.0 H -60.0)  (let [t (m/tan (m/radians (* 0.75 H)))]
                             (Vec4. (/ (- (* mn (inc t)) mx) t) mn mx (.w c)))
      :else (let [t (m/tan (m/radians (* 1.5 (+ H 180.0))))]
              (Vec4. mn (/ (+ (* mn t) mx) (inc t)) mx (.w c))))))

(defn to-HCL*
  "Converts an sRGB color to HCL with channels mapped to `[0,255]`.

  Each channel of [[to-HCL]] is mapped linearly from its range (H -180 to 180, C 0 to 170, L 0 to 135.27) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-HCL*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c] 
  (let [cc (to-HCL c)]
    (Vec4. (m/mnorm (.x cc) -179.8496181535773 180.0 0.0 255.0)
           (* 255.0 (/ (.y cc) 170.0))
           (* 255.0 (/ (.z cc) 135.26590615814683))
           (.w cc))))

(defn from-HCL*
  "Converts HCL with channels in `[0,255]`, as produced by [[to-HCL*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-HCL]]."
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
  "Converts an sRGB color to HWB, hue, whiteness and blackness.

  A. R. Smith, E. R. Lyons, HWB - A More Intuitive Hue-Based Color Model (1996): whiteness `min`, blackness `1 - max`; hue in degrees, 0 for grays.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: H 0 to 360, W 0 to 1, B 0 to 1. See also [[from-HWB]], [[to-HWB*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (to-HSV c)]
    (Vec4. (.x c) (* (- 1.0 (.y c)) (.z c)) (- 1.0 (.z c)) (.w c))))

(defn from-HWB
  "Converts HWB to sRGB, the inverse of [[to-HWB]].

  - `c` HWB color, any type accepted by [[to-color]], with channels in the ranges of [[to-HWB]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-HWB*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        w (.y c)
        b (.z c)
        w+b (+ w b)
        w (if (> w+b 1.0) (/ w w+b) w)
        b- (if (> w+b 1.0) (- 1.0 (/ b w+b)) (- 1.0 b))]
    (from-HSV (Vec4. (.x c) (if (zero? w) 1.0 (- 1.0 (/ w b-))) b- (.w c)))))

(def ^{:doc "Converts an sRGB color to HWB with channels mapped to `[0,255]`.

  Each channel of [[to-HWB]] is mapped linearly from its range (H 0 to 360, W 0 to 1, B 0 to 1) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-HWB*]]." :metadoc/categories meta-conv} to-HWB* (comp normalize-HSx to-HWB))
(def ^{:doc "Converts HWB with channels in `[0,255]`, as produced by [[to-HWB*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-HWB]]." :metadoc/categories meta-conv} from-HWB* (comp from-HWB denormalize-HSx pr/to-color))


;; ### GLHS
;;
;; Color Theory and Modeling for Computer Graphics, Visualization, and Multimedia Applications (The Springer International Series in Engineering and Computer Science) by Haim Levkowitz

;; Page 79, minimizer
(def ^{:private true :const true :tag 'double} weight-max 0.7)
(def ^{:private true :const true :tag 'double} weight-mid 0.1)
(def ^{:private true :const true :tag 'double} weight-min 0.2)

(defn to-GLHS
  "Converts an sRGB color to GLHS, the generalized lightness, hue and saturation model of Levkowitz and Herman.

  H. Levkowitz, Color Theory and Modeling for Computer Graphics, Visualization, and Multimedia Applications: weights wmin 0.2, wmid 0.1, wmax 0.7, the closest approximation to the Munsell space (p. 79). Channel order L, H, S; hue in degrees, 0 for grays.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: L 0 to 1, H 0 to 360, S 0 to 1. See also [[from-GLHS]], [[to-GLHS*]]."
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
  "Converts an sRGB color to GLHS with channels mapped to `[0,255]`.

  Each channel of [[to-GLHS]] is mapped linearly from its range (L 0 to 1, H 0 to 360, S 0 to 1) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-GLHS*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c] 
  (let [cc (to-GLHS c)]
    (Vec4. (* 255.0 (.x cc))
           (m/mnorm (.y cc) 0.0 359.7647058823529 0.0 255.0)
           (* 255.0 (.z cc))
           (.w cc))))

(defn from-GLHS
  "Converts GLHS to sRGB, the inverse of [[to-GLHS]].

  - `c` GLHS color, any type accepted by [[to-color]], with channels in the ranges of [[to-GLHS]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-GLHS*]]."
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
  "Converts GLHS with channels in `[0,255]`, as produced by [[to-GLHS*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-GLHS]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-GLHS (Vec4. (/ (.x c) 255.0)
                      (m/mnorm (.y c) 0.0 255.0 0.0 359.7647058823529)
                      (/ (.z c) 255.0)
                      (.w c)))))

;; ### YPbPr

(defn to-YPbPr
  "Converts an sRGB color to YPbPr (BT.709, full range).

  `Y = 0.2126 R + 0.7152 G + 0.0722 B`, `Pb = (B - Y) / 1.8556`, `Pr = (R - Y) / 1.5748` on the 0-255 scale.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: Y 0 to 255, Pb -127.5 to 127.5, Pr -127.5 to 127.5. See also [[from-YPbPr]], [[to-YPbPr*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        y (+ (* 0.2126 (.x c))
             (* 0.7152 (.y c))
             (* 0.0722 (.z c)))
        pb (/ (- (.z c) y) 1.8556)
        pr (/ (- (.x c) y) 1.5748)]
    (Vec4. y pb pr (.w c))))

(defn to-YPbPr*
  "Converts an sRGB color to YPbPr with channels mapped to `[0,255]`.

  Each channel of [[to-YPbPr]] is mapped linearly from its range (Y 0 to 255, Pb -127.5 to 127.5, Pr -127.5 to 127.5) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-YPbPr*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-YPbPr c)]
    (Vec4. (.x cc)
           (m/mnorm (.y cc) -127.5 127.5 0.0 255.0)
           (m/mnorm (.z cc) -127.5 127.5 0.0 255.0)
           (.w cc))))

(defn from-YPbPr
  "Converts YPbPr to sRGB, the inverse of [[to-YPbPr]].

  - `c` YPbPr color, any type accepted by [[to-color]], with channels in the ranges of [[to-YPbPr]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-YPbPr*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        b (+ (.x c) (* 1.8556 (.y c)))
        r (+ (.x c) (* 1.5748 (.z c)))
        g (/ (- (.x c) (* 0.2126 r) (* 0.0722 b)) 0.7152)]
    (Vec4. r g b (.w c))))

(defn from-YPbPr*
  "Converts YPbPr with channels in `[0,255]`, as produced by [[to-YPbPr*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-YPbPr]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-YPbPr (Vec4. (.x c)
                       (m/mnorm (.y c) 0.0 255.0 -127.5 127.5)
                       (m/mnorm (.z c) 0.0 255.0 -127.5 127.5)
                       (.w c)))))

;; ### YDbDr

(defn to-YDbDr
  "Converts an sRGB color to YDbDr, the SECAM color space.

  BT.601 luma `Y = 0.299 R + 0.587 G + 0.114 B`, `Db = -0.450 R - 0.883 G + 1.333 B`, `Dr = -1.333 R + 1.116 G + 0.217 B`, on the 0-255 scale.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: Y 0 to 255, Db -339.92 to 339.92, Dr -339.92 to 339.92. See also [[from-YDbDr]], [[to-YDbDr*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c] 
  (let [^Vec4 c (pr/to-color c)
        Y (+ (* 0.299 (.x c)) (* 0.587 (.y c)) (* 0.114 (.z c)))
        Db (+ (* -0.45 (.x c)) (* -0.883 (.y c)) (* 1.333 (.z c)))
        Dr (+ (* -1.333 (.x c)) (* 1.116 (.y c)) (* 0.217 (.z c)))]
    (Vec4. Y Db Dr (.w c))))

(defn to-YDbDr*
  "Converts an sRGB color to YDbDr with channels mapped to `[0,255]`.

  Each channel of [[to-YDbDr]] is mapped linearly from its range (Y 0 to 255, Db -339.92 to 339.92, Dr -339.92 to 339.92) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-YDbDr*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-YDbDr c)]
    (Vec4. (.x cc)
           (m/mnorm (.y cc) -339.915 339.915 0.0 255.0)
           (m/mnorm (.z cc) -339.915 339.915 0.0 255.0)
           (.w cc))))

(defn from-YDbDr
  "Converts YDbDr to sRGB, the inverse of [[to-YDbDr]].

  - `c` YDbDr color, any type accepted by [[to-color]], with channels in the ranges of [[to-YDbDr]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-YDbDr*]]."
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
  "Converts YDbDr with channels in `[0,255]`, as produced by [[to-YDbDr*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-YDbDr]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-YDbDr (Vec4. (.x c)
                       (m/mnorm (.y c) 0.0 255.0 -339.915 339.915)
                       (m/mnorm (.z c) 0.0 255.0 -339.915 339.915)
                       (.w c)))))


;; ### YCbCr

;; JPEG version

(def ^:private ^:const y-norm ohta-s)

(defn to-YCbCr
  "Converts an sRGB color to YCbCr (JPEG, full range, BT.601).

  `Y = 0.299 R + 0.587 G + 0.114 B`, `Cb = (B - Y) / 1.772`, `Cr = (R - Y) / 1.402` on the 0-255 scale, chroma centered at 0 (no 128 offset); the inverse is the exact inverse matrix.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: Y 0 to 255, Cb -127.5 to 127.5, Cr -127.5 to 127.5. See also [[from-YCbCr]], [[to-YCbCr*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (Vec4. (+ (* 0.299 (.x c)) (* 0.587 (.y c)) (* 0.114 (.z c)))
           (+ (* -0.16873589164785552 (.x c)) (* -0.3312641083521444 (.y c)) (* 0.5 (.z c)))
           (+ (* 0.5 (.x c)) (* -0.4186875891583452 (.y c)) (* -0.08131241084165479 (.z c)))
           (.w c))))

(defn to-YCbCr*
  "Converts an sRGB color to YCbCr with channels mapped to `[0,255]`.

  The channels of [[to-YCbCr]] with 127.5 added to the two chroma channels. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-YCbCr*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (v/add (to-YCbCr c) y-norm))

(defn from-YCbCr
  "Converts YCbCr to sRGB, the inverse of [[to-YCbCr]].

  - `c` YCbCr color, any type accepted by [[to-color]], with channels in the ranges of [[to-YCbCr]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-YCbCr*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (Vec4. (+ (.x c) (* 1.402 (.z c)))
           (+ (.x c) (* -0.3441362862010221 (.y c)) (* -0.7141362862010221 (.z c)))
           (+ (.x c) (* 1.772 (.y c)))
           (.w c))))

(defn from-YCbCr*
  "Converts YCbCr with channels in `[0,255]`, as produced by [[to-YCbCr*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-YCbCr]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (from-YCbCr (v/sub (pr/to-color c) y-norm)))

;; ### YUV

(defn to-YUV
  "Converts an sRGB color to YUV (analog PAL, BT.601 luma).

  `Y = 0.299 R + 0.587 G + 0.114 B`, `U = -0.147 R - 0.289 G + 0.436 B`, `V = 0.615 R - 0.515 G - 0.1 B` on the 0-255 scale; the inverse is the exact inverse matrix.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: Y 0 to 255, U -111.18 to 111.18, V -156.83 to 156.83. See also [[from-YUV]], [[to-YUV*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (Vec4. (+ (* 0.299 (.x c)) (* 0.587 (.y c)) (* 0.114 (.z c)))
           (+ (* -0.147 (.x c)) (* -0.289 (.y c)) (* 0.436 (.z c)))
           (+ (* 0.615 (.x c)) (* -0.515 (.y c)) (* -0.1 (.z c)))
           (.w c))))

(defn to-YUV*
  "Converts an sRGB color to YUV with channels mapped to `[0,255]`.

  Each channel of [[to-YUV]] is mapped linearly from its range (Y 0 to 255, U -111.18 to 111.18, V -156.83 to 156.83) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-YUV*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-YUV c)]
    (Vec4. (.x cc)
           (m/mnorm (.y cc) -111.18 111.18 0.0 255.0)
           (m/mnorm (.z cc) -156.825 156.825 0.0 255.0)
           (.w cc))))

(defn from-YUV
  "Converts YUV to sRGB, the inverse of [[to-YUV]].

  - `c` YUV color, any type accepted by [[to-color]], with channels in the ranges of [[to-YUV]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-YUV*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (Vec4. (+ (.x c) (* -3.945707070708279E-5 (.y c)) (* 1.139827967171717 (.z c)))
           (+ (.x c) (* -0.39461016414141414 (.y c)) (* -0.5805003156565657 (.z c)))
           (+ (.x c) (* 2.0319996843434343 (.y c)) (* -4.8137626262625125E-4 (.z c)))
           (.w c))))

(defn from-YUV*
  "Converts YUV with channels in `[0,255]`, as produced by [[to-YUV*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-YUV]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-YUV (Vec4. (.x c)
                     (m/mnorm (.y c) 0.0 255.0 -111.18 111.18)
                     (m/mnorm (.z c) 0.0 255.0 -156.825 156.825)
                     (.w c)))))

;; ### YIQ

(defn to-YIQ
  "Converts an sRGB color to YIQ (analog NTSC, BT.601 luma).

  `Y = 0.299 R + 0.587 G + 0.114 B`, `I = 0.595716 R - 0.274453 G - 0.321263 B`, `Q = 0.211456 R - 0.522591 G + 0.311135 B` on the 0-255 scale; the inverse is the exact inverse matrix.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: Y 0 to 255, I -151.91 to 151.91, Q -133.26 to 133.26. See also [[from-YIQ]], [[to-YIQ*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (Vec4. (+ (* 0.299 (.x c)) (* 0.587 (.y c)) (* 0.114 (.z c)))
           (+ (* 0.595716 (.x c)) (* -0.274453 (.y c)) (* -0.321263 (.z c)))
           (+ (* 0.211456 (.x c)) (* -0.522591 (.y c)) (* 0.311135 (.z c)))
           (.w c))))

(defn to-YIQ*
  "Converts an sRGB color to YIQ with channels mapped to `[0,255]`.

  Each channel of [[to-YIQ]] is mapped linearly from its range (Y 0 to 255, I -151.91 to 151.91, Q -133.26 to 133.26) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-YIQ*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-YIQ c)]
    (Vec4. (.x cc)
           (m/mnorm (.y cc) -151.90758 151.90758 0.0 255.0)
           (m/mnorm (.z cc) -133.260705 133.260705 0.0 255.0)
           (.w cc))))

(defn from-YIQ
  "Converts YIQ to sRGB, the inverse of [[to-YIQ]].

  - `c` YIQ color, any type accepted by [[to-color]], with channels in the ranges of [[to-YIQ]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-YIQ*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (Vec4. (+ (.x c) (* 0.9562957197589482 (.y c)) (* 0.6210244164652611 (.z c)))
           (+ (.x c) (* -0.27212209931851045 (.y c)) (* -0.647380596825695 (.z c)))
           (+ (.x c) (* -1.1069890167364902 (.y c)) (* 1.7046149983646481 (.z c)))
           (.w c))))

(defn from-YIQ*
  "Converts YIQ with channels in `[0,255]`, as produced by [[to-YIQ*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-YIQ]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        I (m/mnorm (.y c) 0.0 255.0 -151.90758 151.90758)
        Q (m/mnorm (.z c) 0.0 255.0 -133.260705 133.260705)]
    (from-YIQ (Vec4. (.x c) I Q (.w c)))))

;; ### YCgCo

(defn to-YCgCo
  "Converts an sRGB color to YCgCo (Malvar and Sullivan 2003, H.264).

  `Y = R/4 + G/2 + B/4`, `Cg = -R/4 + G/2 - B/4`, `Co = R/2 - B/2`, on the 0-255 scale; exact inverse.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: Y 0 to 255, Cg -127.5 to 127.5, Co -127.5 to 127.5. See also [[from-YCgCo]], [[to-YCgCo*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        Y (+ (* 0.25 (.x c)) (* 0.5 (.y c)) (* 0.25 (.z c)))
        Cg (+ (* -0.25 (.x c)) (* 0.5 (.y c)) (* -0.25 (.z c)))
        Co (+ (* 0.5 (.x c)) (* -0.5 (.z c)))]
    (Vec4. Y Cg Co (.w c))))

(defn to-YCgCo*
  "Converts an sRGB color to YCgCo with channels mapped to `[0,255]`.

  The channels of [[to-YCgCo]] with 127.5 added to the two chroma channels. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-YCgCo*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (v/add (to-YCgCo c) y-norm))

(defn from-YCgCo
  "Converts YCgCo to sRGB, the inverse of [[to-YCgCo]].

  - `c` YCgCo color, any type accepted by [[to-color]], with channels in the ranges of [[to-YCgCo]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-YCgCo*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        Cg (.y c)
        Co (.z c)
        tmp (- (.x c) Cg)]
    (Vec4. (+ Co tmp) (+ (.x c) Cg) (- tmp Co) (.w c))))

(defn from-YCgCo*
  "Converts YCgCo with channels in `[0,255]`, as produced by [[to-YCgCo*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-YCgCo]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (from-YCgCo (v/sub (pr/to-color c) y-norm)))

;; Cubehelix

(def ^{:private true :const true :tag 'double} ch-a -0.14861)
(def ^{:private true :const true :tag 'double} ch-b 1.78277)
(def ^{:private true :const true :tag 'double} ch-c -0.29227)
(def ^{:private true :const true :tag 'double} ch-d -0.90649)
(def ^{:private true :const true :tag 'double} ch-e 1.97294)
(def ^{:private true :const true :tag 'double} ch-ed (* ch-e ch-d))
(def ^{:private true :const true :tag 'double} ch-eb (* ch-e ch-b))
(def ^{:private true :const true :tag 'double} ch-bc-da (- (* ch-b ch-c) (* ch-d ch-a)))
(def ^{:private true :const true :tag 'double} ch-bc-da+ed-eb-r (/ (+ ch-bc-da ch-ed (- ch-eb))))

(defn to-Cubehelix
  "Converts an sRGB color to Cubehelix, a hue-saturation-lightness space built on Green's cubehelix scheme.

  D3 (d3-color) formulation of D. A. Green (2011). Hue in degrees; for grays the hue is undefined and set by the formula.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: H 0 to 360, S 0 to 4.614, L 0 to 1. See also [[from-Cubehelix]], [[to-Cubehelix*]]."
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
    (if (m/nan? s)
      (Vec4. 0.0 0.0 l (.w c))
      (let [h (- (* (m/atan2 k bl) m/rad-in-deg) 120.0)]
        (Vec4. (if (neg? h) (+ h 360.0) h) s l (.w c))))))

(defn to-Cubehelix*
  "Converts an sRGB color to Cubehelix with channels mapped to `[0,255]`.

  Each channel of [[to-Cubehelix]] is mapped linearly from its range (H 0 to 360, S 0 to 4.614, L 0 to 1) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-Cubehelix*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [cc (to-Cubehelix c)]
    (Vec4. (m/mnorm (.x cc) 0.0 359.9932808311505 0.0 255.0)
           (m/mnorm (.y cc) 0.0 4.61438686803972 0.0 255.0)
           (* 255.0 (.z cc))
           (.w cc))))

(defn from-Cubehelix
  "Converts Cubehelix to sRGB, the inverse of [[to-Cubehelix]].

  - `c` Cubehelix color, any type accepted by [[to-color]], with channels in the ranges of [[to-Cubehelix]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-Cubehelix*]]."
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
  "Converts Cubehelix with channels in `[0,255]`, as produced by [[to-Cubehelix*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-Cubehelix]]."
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
  "Converts an sRGB color to OSA-UCS Ljg (MacAdam 1974).

  Computed from [[to-XYZ]] as in colorio: `L = (Lambda - 14.3993) / sqrt(2)` (colour-science uses 14.4, L differs by 4.95e-4). Near black the model is ill-conditioned: j and g reach extreme values (thousands, e.g. j -3658 for RGB 18 7 4) and [[from-OSA]] is accurate to about 1e-6 there.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: L -13.51 to 7.14, j and g roughly -20 to 20 except near black. See also [[from-OSA]], [[to-OSA*]]."
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
    (Vec4. L (* C b) (* C a) (.w xyz))))

(def ^{:private true :const true :tag 'double} OSA-v (* 0.042 0.042 0.042))
(def ^{:private true :const true :tag 'double} OSA-30v (* 30.0 OSA-v))
(def ^{:private true :const true :tag 'double} OSA-a (- (inc OSA-v)))
(def ^{:private true :const true :tag 'double} OSA-detr (/ -139.68999999999997))
(def ^{:private true :const true :tag 'double} OSA-omega 4.957506551095124)

(deftype OSAFDF [^double fomega ^double dfomega ^double X ^double Y ^double Z])

(defn from-OSA
  "Converts OSA to sRGB, the inverse of [[to-OSA]].

  - `c` OSA color, any type accepted by [[to-color]], with channels in the ranges of [[to-OSA]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-OSA*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 col (pr/to-color c)
        L (.x col)
        j(.y col)
        g (.z col)
        L' (+ (* m/SQRT2 L) 14.3993)
        u (+ (/ L' 5.9) m/TWO_THIRD)
        b (* 3.0 u)
        c (* -3.0 u u)
        d (+ (* u u u) OSA-30v)
        p (/ (- (* 3.0 OSA-a c)
                (* b b))
             (* 3.0 OSA-a OSA-a))
        aa27 (* 27.0 OSA-a OSA-a)
        q (/ (+ (* 2.0 b b b)
                (* -9.0 OSA-a b c)
                (* aa27 d))
             (* aa27 OSA-a))
        q2 (* 0.5 q)
        s (m/sqrt (+ (m/sq q2)
                     (m/pow3 (* p m/THIRD))))
        t (- (+ (m/cbrt (- s q2))
                (m/cbrt (- (- q2) s)))
             (/ b (* 3.0 OSA-a)))
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
                     ;; exact inverse of the OSA RGB matrix of to-OSA
                     X (+ (* 1.062618265125947 R) (* -0.4120917485671994 G) (* 0.297517984916228 B))
                     Y (+ (* 0.359926645484816 R) (* 0.6400721081119282 G) (* -2.6183048923376638E-5 B))
                     Z (+ (* -8.963014585108617E-5 R) (* -0.36902345244568985 G) (* 1.4423901018042944 B))
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
                     dX (+ (* 1.062618265125947 dR) (* -0.4120917485671994 dG) (* 0.297517984916228 dB))
                     dY (+ (* 0.359926645484816 dR) (* 0.6400721081119282 dG) (* -2.6183048923376638E-5 dB))
                     dZ (+ (* -8.963014585108617E-5 dR) (* -0.36902345244568985 dG) (* 1.4423901018042944 dB))
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
           omega (double OSA-omega)]
      (let [^OSAFDF res (f-df omega)]
        (if (and (< iter 50)
                 (> (m/abs (.fomega res)) 1.0e-12))
          (recur (inc iter)
                 (- omega (/ (.fomega res)
                             (.dfomega res))))
          (from-XYZ (Vec4. (.X res) (.Y res) (.Z res) (.w col))))))))

(defn to-OSA*
  "Converts an sRGB color to OSA with channels mapped to `[0,255]`.

  L of [[to-OSA]] is mapped linearly; j and g first go through a triple cube root, because of their extreme values near black, then are mapped linearly. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-OSA*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [c (to-OSA c)]
    (Vec4. (m/mnorm (.x c) -13.507581921540849 7.137750782376464 0.0 255.0)
           (m/mnorm (m/cbrt (m/cbrt (m/cbrt (.y c)))) -1.319517853779513 1.2174744944510014 0.0 255.0)
           (m/mnorm (m/cbrt (m/cbrt (m/cbrt (.z c)))) -1.2467116799814266 1.1826709008309546 0.0 255.0)
           (.w c))))

(defn from-OSA*
  "Converts OSA with channels in `[0,255]`, as produced by [[to-OSA*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-OSA]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)]
    (from-OSA (Vec4. (m/mnorm (.x c) 0.0 255.0 -13.507581921540849 7.137750782376464)
                     (m/pow3 (m/pow3 (m/pow3 (m/mnorm (.y c) 0.0 255.0 -1.319517853779513 1.2174744944510014))))
                     (m/pow3 (m/pow3 (m/pow3 (m/mnorm (.z c) 0.0 255.0 -1.2467116799814266 1.1826709008309546))))
                     (.w c)))))

;; ### DIN99 family

(def ^:private din99-config
  {:din99 {:l99mult 105.509 :lmult 0.0158
           :cosc (m/cos (m/radians 16.0)) :sinc (m/sin (m/radians 16.0))
           :fmult 0.7 :c99mult 1.0 :gmult (/ 9.0 200.0) :hshift 0.0 :gdiv (/ 9.0 200.0)
           :r [0.0 100.00031169207742], :g [-27.450364194288877 36.17532020126586], :b [-33.3955437863943 31.15506888258738]}
   :din99b {:l99mult 303.67 :lmult 0.0039
            :cosc (m/cos (m/radians 26.0)) :sinc (m/sin (m/radians 26.0))
            :fmult 0.83 :c99mult 23.0 :gmult 0.075 :hshift (m/radians 26.0) :gdiv 1.0
            :r [0.0 99.99966889479349], :g [-40.110091234836794 45.52419926337991], :b [-40.48996132587376 44.36616730084986]}
   ;; DIN 6176: hue rotated back by 26 degrees (as culori, coloraide)
   :din99o {:l99mult 303.67 :lmult 0.0039
            :cosc (m/cos (m/radians 26.0)) :sinc (m/sin (m/radians 26.0))
            :fmult 0.83 :c99mult 1.0 :gmult 0.075 :hshift (m/radians 26.0) :gdiv 0.0435
            :r [0.0 99.99966889479349], :g [-40.09004621173093 45.50144853911036], :b [-40.46972646264244 44.34399530319826]}
   :din99c {:l99mult 317.65 :lmult 0.0037
            :cosc 1.0 :sinc 0.0
            :fmult 0.94 :c99mult 23.0 :gmult 0.066 :hshift 0.0 :gdiv 1.0 :kuehni 1.1
            :r [0.0 99.99963151018667], :g [-38.50249399671165 45.17090440252161], :b [-45.676929680471495 44.22399928478519]}
   :din99d {:l99mult 325.22 :lmult 0.0036
            :cosc (m/cos (m/radians 50.0)) :sinc (m/sin (m/radians 50.0))
            :fmult 1.14 :c99mult 22.5 :gmult 0.06 :hshift (m/radians 50.0) :gdiv 1.0 :kuehni 1.12
            :r [0.0 100.0001740520318], :g [-37.5279922707718 44.1720101745847], :b [-45.79074026102887 43.68393527066125]}})

(defn- kuehni-whitepoint
  "CIE-2-D65 with Kuehni's correction of X (`c0 Xn - (c0 - 1) Zn`), the white of DIN99c and DIN99d."
  ^Vec4 [^double c0]
  (let [^Vec4 w wp/CIE-2-D65]
    (wp/XZ->coordinates (- (* c0 (.x w)) (* (dec c0) (.y w))) (.y w))))

(defn- make-to-kuehni-LAB
  "sRGB to CIELAB with Kuehni's correction `X' = c0 X - (c0 - 1) Z` applied to the color and the white (Cui et al. 2002, DIN99c and DIN99d)."
  [^double c0]
  (let [white (kuehni-whitepoint c0)
        c0- (dec c0)]
    (fn ^Vec4 [c]
      (let [^Vec4 xyz (to-XYZ c)]
        (XYZ-to-LAB (Vec4. (- (* c0 (.x xyz)) (* c0- (.z xyz))) (.y xyz) (.z xyz) (.w xyz)) white)))))

(defn- make-from-kuehni-LAB
  "Inverse of [[make-to-kuehni-LAB]]: CIELAB relative to the corrected white, then `X = (X' + (c0 - 1) Z) / c0`."
  [^double c0]
  (let [white (kuehni-whitepoint c0)
        c0- (dec c0)]
    (fn ^Vec4 [lab]
      (let [^Vec4 xyz (LAB-to-XYZ lab white)]
        (from-XYZ (Vec4. (/ (+ (.x xyz) (* c0- (.z xyz))) c0) (.y xyz) (.z xyz) (.w xyz)))))))

(defn- make-to-DIN99
  [n]
  (let [{:keys [^double cosc ^double sinc ^double fmult ^double hshift ^double c99mult
                ^double gmult ^double gdiv ^double l99mult ^double lmult kuehni]} (din99-config n)
        ->lab (if kuehni (make-to-kuehni-LAB kuehni) to-LAB)]
    (fn ^Vec4 [c]
      (let [^Vec4 c (->lab c)
            e (+ (* cosc (.y c)) (* sinc (.z c)))
            f (* fmult (- (* cosc (.z c)) (* sinc (.y c))))
            G (m/hypot-sqrt e f)
            h-ef (+ (m/atan2 f e) hshift)
            C99 (* c99mult (/ (m/log1p (* gmult G)) gdiv))
            a99 (* C99 (m/cos h-ef))
            b99 (* C99 (m/sin h-ef))
            L99 (* l99mult (m/log1p (* lmult (.x c))))]
        (Vec4. L99 a99 b99 (.w c))))))

(defn- make-to-DIN99*
  [f n]
  (let [{[^double minr ^double maxr] :r
         [^double ming ^double maxg] :g
         [^double minb ^double maxb] :b} (din99-config n)]
    (fn ^Vec4 [c]
      (let [^Vec4 c (f c)]
        (Vec4. (m/mnorm (.x c) minr maxr 0.0 255.0)
               (m/mnorm (.y c) ming maxg 0.0 255.0)
               (m/mnorm (.z c) minb maxb 0.0 255.0)
               (.w c))))))

(defn- make-from-DIN99
  [n]
  (let [{:keys [^double cosc ^double sinc ^double fmult ^double hshift ^double c99mult
                ^double gmult ^double gdiv ^double l99mult ^double lmult kuehni]} (din99-config n)
        C99mult (/ gdiv c99mult)
        lab-> (if kuehni (make-from-kuehni-LAB kuehni) from-LAB)]
    (fn ^Vec4 [c]
      (let [^Vec4 c (pr/to-color c)
            h99 (- (m/atan2 (.z c) (.y c)) hshift)
            C99 (m/hypot-sqrt (.y c) (.z c))
            G (/ (m/expm1 (* C99mult C99)) gmult)
            e (* G (m/cos h99))
            f (/ (* G (m/sin h99)) fmult)
            a (- (* e cosc) (* f sinc))
            b (+ (* e sinc) (* f cosc))
            L (/ (m/expm1 (/ (.x c) l99mult)) lmult)]
        (lab-> (Vec4. L a b (.w c)))))))

(defn- make-from-DIN99*
  [f n]
  (let [{[^double minr ^double maxr] :r
         [^double ming ^double maxg] :g
         [^double minb ^double maxb] :b} (din99-config n)]
    (fn ^Vec4 [c]
      (let [^Vec4 c (pr/to-color c)]
        (f (Vec4. (m/mnorm (.x c) 0.0 255.0 minr maxr)
                  (m/mnorm (.y c) 0.0 255.0 ming maxg)
                  (m/mnorm (.z c) 0.0 255.0 minb maxb)
                  (.w c)))))))

(def ^{:doc "Converts an sRGB color to DIN99, a CIELAB-based uniform space (DIN 6176, ASTM D2244-07).

  Computed from [[to-LAB]] (CIE 1931 2° D65); logarithmic lightness, rotated and compressed chroma.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: L 0 to 100, a -27.45 to 36.18, b -33.40 to 31.15. See also [[from-DIN99]], [[to-DIN99*]]." :metadoc/categories meta-conv} to-DIN99 (make-to-DIN99 :din99))
(def ^{:doc "Converts an sRGB color to DIN99b, a CIELAB-based uniform space (Cui et al. 2002).

  Computed from [[to-LAB]] (CIE 1931 2° D65), with the DIN99b constants and a 26° hue rotation.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: L 0 to 100, a -40.11 to 45.52, b -40.49 to 44.37. See also [[from-DIN99b]], [[to-DIN99b*]]." :metadoc/categories meta-conv} to-DIN99b (make-to-DIN99 :din99b))
(def ^{:doc "Converts an sRGB color to DIN99o, a CIELAB-based uniform space (DIN 6176, Cui et al. 2002).

  Computed from [[to-LAB]] (CIE 1931 2° D65): logarithmic lightness, chroma rotated by 26°, compressed by 0.83 and rotated back (as culori and coloraide).

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: L 0 to 100, a -40.09 to 45.50, b -40.47 to 44.34. See also [[from-DIN99o]], [[to-DIN99o*]]." :metadoc/categories meta-conv} to-DIN99o (make-to-DIN99 :din99o))
(def ^{:doc "Converts an sRGB color to DIN99c, a CIELAB-based uniform space (Cui et al. 2002).

  Computed from [[to-XYZ]] with Kuehni's correction `X' = 1.1 X - 0.1 Z` applied to the color and to the CIE 1931 2° D65 white, then CIELAB and the DIN99c constants (as colour-science).

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: L 0 to 100, a -38.50 to 45.17, b -45.68 to 44.22. See also [[from-DIN99c]], [[to-DIN99c*]]." :metadoc/categories meta-conv} to-DIN99c (make-to-DIN99 :din99c))
(def ^{:doc "Converts an sRGB color to DIN99d, a CIELAB-based uniform space (Cui et al. 2002).

  Computed from [[to-XYZ]] with Kuehni's correction `X' = 1.12 X - 0.12 Z` applied to the color and to the CIE 1931 2° D65 white, then CIELAB and the DIN99d constants, 50° hue rotation (as colour-science).

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: L 0 to 100, a -37.53 to 44.17, b -45.79 to 43.68. See also [[from-DIN99d]], [[to-DIN99d*]]." :metadoc/categories meta-conv} to-DIN99d (make-to-DIN99 :din99d))
(def ^{:doc "Converts an sRGB color to DIN99 with channels mapped to `[0,255]`.

  Each channel of [[to-DIN99]] is mapped linearly from its range (L 0 to 100, a -27.45 to 36.18, b -33.40 to 31.15) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-DIN99*]]." :metadoc/categories meta-conv} to-DIN99* (make-to-DIN99* to-DIN99 :din99))
(def ^{:doc "Converts an sRGB color to DIN99b with channels mapped to `[0,255]`.

  Each channel of [[to-DIN99b]] is mapped linearly from its range (L 0 to 100, a -40.11 to 45.52, b -40.49 to 44.37) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-DIN99b*]]." :metadoc/categories meta-conv} to-DIN99b* (make-to-DIN99* to-DIN99b :din99b))
(def ^{:doc "Converts an sRGB color to DIN99o with channels mapped to `[0,255]`.

  Each channel of [[to-DIN99o]] is mapped linearly from its range (L 0 to 100, a -40.09 to 45.50, b -40.47 to 44.34) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-DIN99o*]]." :metadoc/categories meta-conv} to-DIN99o* (make-to-DIN99* to-DIN99o :din99o))
(def ^{:doc "Converts an sRGB color to DIN99c with channels mapped to `[0,255]`.

  Each channel of [[to-DIN99c]] is mapped linearly from its range (L 0 to 100, a -38.50 to 45.17, b -45.68 to 44.22) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-DIN99c*]]." :metadoc/categories meta-conv} to-DIN99c* (make-to-DIN99* to-DIN99c :din99c))
(def ^{:doc "Converts an sRGB color to DIN99d with channels mapped to `[0,255]`.

  Each channel of [[to-DIN99d]] is mapped linearly from its range (L 0 to 100, a -37.53 to 44.17, b -45.79 to 43.68) to `[0,255]`. Alpha is kept.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4`. Inverse: [[from-DIN99d*]]." :metadoc/categories meta-conv} to-DIN99d* (make-to-DIN99* to-DIN99d :din99d))

(def ^{:doc "Converts DIN99 to sRGB, the inverse of [[to-DIN99]].

  - `c` DIN99 color, any type accepted by [[to-color]], with channels in the ranges of [[to-DIN99]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-DIN99*]]." :metadoc/categories meta-conv} from-DIN99 (make-from-DIN99 :din99))
(def ^{:doc "Converts DIN99b to sRGB, the inverse of [[to-DIN99b]].

  - `c` DIN99b color, any type accepted by [[to-color]], with channels in the ranges of [[to-DIN99b]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-DIN99b*]]." :metadoc/categories meta-conv} from-DIN99b (make-from-DIN99 :din99b))
(def ^{:doc "Converts DIN99o to sRGB, the inverse of [[to-DIN99o]].

  - `c` DIN99o color, any type accepted by [[to-color]], with channels in the ranges of [[to-DIN99o]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-DIN99o*]]." :metadoc/categories meta-conv} from-DIN99o (make-from-DIN99 :din99o))
(def ^{:doc "Converts DIN99c to sRGB, the inverse of [[to-DIN99c]].

  - `c` DIN99c color, any type accepted by [[to-color]], with channels in the ranges of [[to-DIN99c]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-DIN99c*]]." :metadoc/categories meta-conv} from-DIN99c (make-from-DIN99 :din99c))
(def ^{:doc "Converts DIN99d to sRGB, the inverse of [[to-DIN99d]].

  - `c` DIN99d color, any type accepted by [[to-color]], with channels in the ranges of [[to-DIN99d]]

  Returns an RGB `Vec4`, not clamped: values outside the sRGB gamut give channels outside `[0,255]`. Alpha is kept. See also [[from-DIN99d*]]." :metadoc/categories meta-conv} from-DIN99d (make-from-DIN99 :din99d))
(def ^{:doc "Converts DIN99 with channels in `[0,255]`, as produced by [[to-DIN99*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-DIN99]]." :metadoc/categories meta-conv} from-DIN99* (make-from-DIN99* from-DIN99 :din99))
(def ^{:doc "Converts DIN99b with channels in `[0,255]`, as produced by [[to-DIN99b*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-DIN99b]]." :metadoc/categories meta-conv} from-DIN99b* (make-from-DIN99* from-DIN99b :din99b))
(def ^{:doc "Converts DIN99o with channels in `[0,255]`, as produced by [[to-DIN99o*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-DIN99o]]." :metadoc/categories meta-conv} from-DIN99o* (make-from-DIN99* from-DIN99o :din99o))
(def ^{:doc "Converts DIN99c with channels in `[0,255]`, as produced by [[to-DIN99c*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-DIN99c]]." :metadoc/categories meta-conv} from-DIN99c* (make-from-DIN99* from-DIN99c :din99c))
(def ^{:doc "Converts DIN99d with channels in `[0,255]`, as produced by [[to-DIN99d*]], to sRGB.

  - `c` color with channels in `[0,255]`

  Returns an RGB `Vec4`, not clamped. See also [[from-DIN99d]]." :metadoc/categories meta-conv} from-DIN99d* (make-from-DIN99* from-DIN99d :din99d))

;; Other RGB

(defn ->XYZ-to-RGB
  "Creates a converter from XYZ to an RGB space.

  Parameters:

  - `space`: a key of `clojure2d.color.rgb/rgbs` (for example `:sRGB`, `:Adobe-RGB-1998`, `:ACEScg`) or an RGB space map (see `clojure2d.color.rgb/rgb-data->map`).

  Returns a function of one XYZ color (`Y` 0-100, as [[to-XYZ]], relative to the space's white point; any value accepted by [[to-color]]) returning `Vec4` RGB with channels 0-255, encoded with the space's transfer function; alpha is kept. Values outside the gamut are not clamped. No chromatic adaptation is applied.

  For `:sRGB` the result equals [[from-XYZ]]: both use the matrices of `(rgb/rgbs :sRGB)`.

  Throws `ex-info` with `{:space space}` for an unknown space name.

  See also [[->RGB-to-XYZ]], [[->RGB-to-RGB]], `clojure2d.color.rgb/rgb-space`."
  [space]
  (let [{to :to m :xyz-to-rgb} (rgb/rgb-space space)]
    (fn ^Vec4 [xyz]
      (let [^Vec4 c (XYZ-to-XYZ1 xyz)
            ^Vec3 c3 (v/fmap (mat/mulv m (Vec3. (.x c) (.y c) (.z c))) to)]
        (RGB1-to-RGB (Vec4. (.x c3) (.y c3) (.z c3) (.w c)))))))

(defn ->RGB-to-XYZ
  "Creates a converter from an RGB space to XYZ.

  Parameters:

  - `space`: a key of `clojure2d.color.rgb/rgbs` or an RGB space map (see `clojure2d.color.rgb/rgb-data->map`).

  Returns a function of one RGB color (channels 0-255, encoded with the space's transfer function; any value accepted by [[to-color]]) returning `Vec4` XYZ (`Y` 0-100, as [[to-XYZ]], relative to the space's white point); alpha is kept. No chromatic adaptation is applied.

  For `:sRGB` the result equals [[to-XYZ]]: both use the matrix of `(rgb/rgbs :sRGB)`, derived from the primaries and `CIE-2-D65`.

  Throws `ex-info` with `{:space space}` for an unknown space name.

  See also [[->XYZ-to-RGB]], [[->RGB-to-RGB]], `clojure2d.color.rgb/rgb-space`."
  [space]
  (let [{from :from m :rgb-to-xyz} (rgb/rgb-space space)]
    (fn ^Vec4 [rgb]
      (let [^Vec4 c (RGB-to-RGB1 rgb)
            ^Vec3 c3 (mat/mulv m (v/fmap (Vec3. (.x c) (.y c) (.z c)) from))]
        (XYZ1-to-XYZ (Vec4. (.x c3) (.y c3) (.z c3) (.w c)))))))

(defn ->RGB-to-RGB
  "Creates a converter between two RGB spaces, with chromatic adaptation when their white points differ.

  Parameters:

  - `source-space`, `target-space`: keys of `clojure2d.color.rgb/rgbs` (for example `:sRGB`, `:Adobe-RGB-1998`, `:Adobe-Wide-Gamut-RGB`) or RGB space maps (see `clojure2d.color.rgb/rgb-data->map`).
  - `adaptation-method` (keyword, optional): `:bradford` (default) or another key of `clojure2d.color.whitepoints/chromatic-adaptation-methods`; used only when the white points differ.

  Returns a function of one color in the source space (any value accepted by [[to-color]], channels 0-255, transfer function encoded) returning the color in the target space as `Vec4` (0-255, encoded, alpha kept). Values outside the target gamut are not clamped.

  Throws `ex-info` with `{:space space}` for an unknown space name and `{:method adaptation-method}` for an unknown method when the white points differ.

  See also [[->XYZ-to-XYZ]], [[->RGB-to-XYZ]], [[->XYZ-to-RGB]]."
  ([source-space target-space]
   (->RGB-to-RGB source-space target-space :bradford))
  ([source-space target-space adaptation-method]
   (let [source (rgb/rgb-space source-space)
         target (rgb/rgb-space target-space)
         ->xyz (->RGB-to-XYZ source)
         <-xyz (->XYZ-to-RGB target)
         swp (:whitepoint source)
         twp (:whitepoint target)
         adaptation-matrix (when (not= swp twp)
                             (wp/chromatic-adaptation-matrix adaptation-method swp twp))]
     (fn ^Vec4 [rgb] (if adaptation-matrix
                      (let [^Vec4 xyz (->xyz rgb)
                            ^Vec3 c (mat/mulv adaptation-matrix (Vec3. (.x xyz) (.y xyz) (.z xyz)))]
                        (<-xyz (Vec4. (.x c) (.y c) (.z c) (.w xyz))))
                      (-> rgb ->xyz <-xyz))))))

(defn ->RGB-to-linear-RGB
  "Creates a converter decoding the transfer function of an RGB space: encoded RGB to linear RGB, both with channels 0-255.

  Parameters:

  - `space`: a key of `clojure2d.color.rgb/rgbs` or an RGB space map.

  Returns a function of one color (any value accepted by [[to-color]]) returning `Vec4`, each channel `255 · from(c / 255)` with the space's `:from` function; alpha is kept. For `:sRGB` it equals [[from-sRGB]].

  Throws `ex-info` with `{:space space}` for an unknown space name.

  See also [[->linear-RGB-to-RGB]]."
  [space]
  (let [from (:from (rgb/rgb-space space))]
    (fn ^Vec4 [rgb]
      (let [^Vec4 c (RGB-to-RGB1 rgb)
            ^Vec3 c3 (v/fmap (Vec3. (.x c) (.y c) (.z c)) from)]
        (RGB1-to-RGB (Vec4. (.x c3) (.y c3) (.z c3) (.w c)))))))

(defn ->linear-RGB-to-RGB
  "Creates a converter encoding with the transfer function of an RGB space: linear RGB to encoded RGB, both with channels 0-255.

  Parameters:

  - `space`: a key of `clojure2d.color.rgb/rgbs` or an RGB space map.

  Returns a function of one color (any value accepted by [[to-color]]) returning `Vec4`, each channel `255 · to(c / 255)` with the space's `:to` function; alpha is kept. Inverse of [[->RGB-to-linear-RGB]]; for `:sRGB` it equals [[to-sRGB]].

  Throws `ex-info` with `{:space space}` for an unknown space name.

  See also [[->RGB-to-linear-RGB]]."
  [space]
  (let [to (:to (rgb/rgb-space space))]
    (fn ^Vec4 [rgb]
      (let [^Vec4 c (RGB-to-RGB1 rgb)
            ^Vec3 c3 (v/fmap (Vec3. (.x c) (.y c) (.z c)) to)]
        (RGB1-to-RGB (Vec4. (.x c3) (.y c3) (.z c3) (.w c)))))))


;; ### Grayscale

(defn to-Gray
  "Converts an sRGB color to gray, all three channels set to the [[luma]] of the color.

  The conversion is lossy: `from-Gray` is the same function and cannot restore the color.

  - `c` color, any type accepted by [[to-color]]

  Returns a `Vec4` with the three channels and alpha. Ranges for colors in the sRGB gamut: all channels in `[0,255]`. See also [[from-Gray]], [[to-Gray*]]."
  {:metadoc/categories meta-conv}
  ^Vec4 [c]
  (let [^Vec4 c (pr/to-color c)
        l (luma c)]
    (Vec4. l l l (.w c))))

(def ^{:doc "The same as [[to-Gray]]: its channels are already in `[0,255]`." :metadoc/categories meta-conv} to-Gray* to-Gray)

;; do nothing in reverse
(def ^{:doc "The same function as [[to-Gray]]: the gray conversion cannot be inverted." :metadoc/categories meta-conv} from-Gray to-Gray)
(def ^{:doc "The same function as [[to-Gray]]: the gray conversion cannot be inverted." :metadoc/categories meta-conv} from-Gray* to-Gray)

;; Just for a case "do nothing"
(def ^{:doc "Alias for [[to-color]]" :metadoc/categories meta-conv} to-RGB pr/to-color)
(def ^{:doc "Alias for [[to-color]]" :metadoc/categories meta-conv} from-RGB pr/to-color)
(def ^{:doc "Alias for [[to-color]]" :metadoc/categories meta-conv} to-RGB* pr/to-color)
(def ^{:doc "Alias for [[to-color]]" :metadoc/categories meta-conv} from-RGB* pr/to-color)

(def ^{:doc "Decodes sRGB to linear RGB, the same function as [[from-sRGB]] (the `:linearRGB` entry of [[colorspaces]])." :metadoc/categories meta-conv} to-linearRGB from-sRGB)
(def ^{:doc "Encodes linear RGB to sRGB, the same function as [[to-sRGB]]." :metadoc/categories meta-conv} from-linearRGB to-sRGB)
(def ^{:doc "The same as [[to-linearRGB]]: its channels are already in `[0,255]`." :metadoc/categories meta-conv} to-linearRGB* from-sRGB)
(def ^{:doc "The same as [[from-linearRGB]]: its channels are already in `[0,255]`." :metadoc/categories meta-conv} from-linearRGB* to-sRGB)

;; List of all color spaces with functions
(def ^{:doc "Colorspace conversions by name: keyword to `[to-XXX from-XXX]`, the raw converters.

  The `to-` function converts an sRGB color (any type accepted by [[to-color]]) to the colorspace, the `from-` function converts back; both return `Vec4` and keep alpha. Values are not clamped. Use [[colorspaces*]] for channels mapped to `[0,255]`; names are listed in [[colorspaces-list]].

  Conventions of the raw conversions (input sRGB, channels 0-255, alpha kept):

  - hue in degrees 0-360: `:HSV`, `:HSB`, `:HSL`, `:HSI`, `:HWB`, `:GLHS` (second channel), `:Cubehelix`, `:LCH`, `:LCHuv`, `:JCH`, `:Oklch`, `:PalettonHSV`; hue in 0-1: `:Okhsl`, `:Okhsv`, `:Okhwb`; hue -180 to 180: `:HCL`; for grays the hue is undefined (0 or rounding noise)
  - CIE 1931 2° D65 white: `:XYZ` (0-100), `:XYZ1` (0-1), `:Yxy`, `:LAB`, `:LCH`, `:LUV`, `:LCHuv`, `:HunterLAB`, `:UVW`, `:UCS`, the `:DIN99` family, `:IPT`, `:IgPgTg`, `:OSA`, `:LMS`; `:JAB` and `:JCH` use absolute luminance with white at 100 cd/m2
  - linear transforms of the gamma-encoded channels on the 0-255 scale: `:CMY`, `:OHTA`, `:YCbCr`, `:YCgCo`, `:YDbDr`, `:YIQ`, `:YPbPr`, `:YUV`
  - from linear RGB: `:linearRGB`, `:XYB`, the Oklab family (`:Oklab`, `:Oklch`, `:Okhsl`, `:Okhsv`, `:Okhwb`)
  - other: `:RGB` and `:pass` (identity), `:Gray` (lossy, luma), `:RYB` (Leonard's heuristic), `:sRGB` (encodes linear RGB)

  Ranges and sources are in the docstrings of the `to-` functions."
     :metadoc/categories meta-conv}
  colorspaces {:CMY         [to-CMY from-CMY]
               :OHTA        [to-OHTA from-OHTA]
               :XYZ         [to-XYZ from-XYZ]
               :XYZ1        [to-XYZ1 from-XYZ1]
               :DIN99       [to-DIN99 from-DIN99]
               :DIN99b      [to-DIN99b from-DIN99b]
               :DIN99c      [to-DIN99c from-DIN99c]
               :DIN99d      [to-DIN99d from-DIN99d]
               :DIN99o      [to-DIN99o from-DIN99o]
               :UCS         [to-UCS from-UCS]
               :UVW         [to-UVW from-UVW]
               :XYB         [to-XYB from-XYB]
               :RYB         [to-RYB from-RYB]
               :Yxy         [to-Yxy from-Yxy]
               :LMS         [to-LMS from-LMS]
               :IPT         [to-IPT from-IPT]
               :IgPgTg      [to-IgPgTg from-IgPgTg]
               :LUV         [to-LUV from-LUV]
               :LAB         [to-LAB from-LAB]
               :Oklab       [to-Oklab from-Oklab]
               :Oklch       [to-Oklch from-Oklch]
               :Okhsv       [to-Okhsv from-Okhsv]
               :Okhwb       [to-Okhwb from-Okhwb]
               :Okhsl       [to-Okhsl from-Okhsl]
               :JAB         [to-JAB from-JAB]
               :HunterLAB   [to-HunterLAB from-HunterLAB]
               :LCH         [to-LCH from-LCH]
               :LCHuv       [to-LCHuv from-LCHuv]
               :JCH         [to-JCH from-JCH]
               :HCL         [to-HCL from-HCL]
               :HSB         [to-HSB from-HSB]
               :HSI         [to-HSI from-HSI]
               :HSL         [to-HSL from-HSL]
               :HSV         [to-HSV from-HSV]
               :PalettonHSV [to-PalettonHSV from-PalettonHSV]
               :HWB         [to-HWB from-HWB]
               :GLHS        [to-GLHS from-GLHS]
               :YPbPr       [to-YPbPr from-YPbPr]
               :YDbDr       [to-YDbDr from-YDbDr]
               :YCbCr       [to-YCbCr from-YCbCr]
               :YCgCo       [to-YCgCo from-YCgCo]
               :YUV         [to-YUV from-YUV]
               :YIQ         [to-YIQ from-YIQ]
               :Gray        [to-Gray from-Gray]
               :sRGB        [to-sRGB from-sRGB]
               :linearRGB   [from-sRGB to-sRGB]
               :Cubehelix   [to-Cubehelix from-Cubehelix]
               :OSA         [to-OSA from-OSA]
               :RGB         [to-color to-color]
               :pass        [to-color to-color]})

(def ^{:doc "Colorspace conversions by name with channels mapped to `[0,255]`: keyword to `[to-XXX* from-XXX*]`.

  Each `to-XXX*` maps the channels of the raw `to-XXX` (see [[colorspaces]]) to `[0,255]`, mostly linearly from the ranges of sRGB colors; `from-XXX*` reverses it. Useful for treating any colorspace like RGB, e.g. in pixel filters. Alpha is kept, values are not clamped."
     :metadoc/categories meta-conv}
  colorspaces* {:CMY         [to-CMY* from-CMY*]
                :OHTA        [to-OHTA* from-OHTA*]
                :XYZ         [to-XYZ* from-XYZ*]
                :XYZ1        [to-XYZ1* from-XYZ1*]
                :DIN99       [to-DIN99* from-DIN99*]
                :DIN99b      [to-DIN99b* from-DIN99b*]
                :DIN99c      [to-DIN99c* from-DIN99c*]
                :DIN99d      [to-DIN99d* from-DIN99d*]
                :DIN99o      [to-DIN99o* from-DIN99o*]
                :UCS         [to-UCS* from-UCS*]
                :UVW         [to-UVW* from-UVW*]
                :XYB         [to-XYB* from-XYB*]
                :RYB         [to-RYB* from-RYB*]
                :Yxy         [to-Yxy* from-Yxy*]
                :LMS         [to-LMS* from-LMS*]
                :IPT         [to-IPT* from-IPT*]
                :IgPgTg      [to-IgPgTg* from-IgPgTg*]
                :LUV         [to-LUV* from-LUV*]
                :LAB         [to-LAB* from-LAB*]
                :Oklab       [to-Oklab* from-Oklab*]
                :Oklch       [to-Oklch* from-Oklch*]
                :Okhsv       [to-Okhsv* from-Okhsv*]
                :Okhwb       [to-Okhwb* from-Okhwb*]
                :Okhsl       [to-Okhsl* from-Okhsl*]
                :JAB         [to-JAB* from-JAB*]
                :HunterLAB   [to-HunterLAB* from-HunterLAB*]
                :LCH         [to-LCH* from-LCH*]
                :LCHuv       [to-LCHuv* from-LCHuv*]
                :JCH         [to-JCH* from-JCH*]
                :HCL         [to-HCL* from-HCL*]
                :HSB         [to-HSB* from-HSB*]
                :HSI         [to-HSI* from-HSI*]
                :HSL         [to-HSL* from-HSL*]
                :HSV         [to-HSV* from-HSV*]
                :PalettonHSV [to-PalettonHSV* from-PalettonHSV*]
                :HWB         [to-HWB* from-HWB*]
                :GLHS        [to-GLHS* from-GLHS*]
                :YPbPr       [to-YPbPr* from-YPbPr*]
                :YDbDr       [to-YDbDr* from-YDbDr*]
                :YCbCr       [to-YCbCr* from-YCbCr*]
                :YCgCo       [to-YCgCo* from-YCgCo*]
                :YUV         [to-YUV* from-YUV*]
                :YIQ         [to-YIQ* from-YIQ*]
                :Gray        [to-Gray from-Gray]
                :sRGB        [to-sRGB* from-sRGB*]
                :linearRGB   [from-sRGB* to-sRGB*]
                :Cubehelix   [to-Cubehelix* from-Cubehelix*]
                :OSA         [to-OSA* from-OSA*]
                :RGB         [to-color to-color]
                :pass        [to-color to-color]})

;; List of color spaces names
(def ^{:doc "Sorted list of the colorspace names, the keys of [[colorspaces]] and [[colorspaces*]]." :metadoc/categories meta-conv} colorspaces-list (sort (keys colorspaces)))

(defn- colorspace-converters
  "The `[to from]` pair of `colorspace` in `table` ([[colorspaces]] or [[colorspaces*]]); throws `ex-info` with `{:colorspace colorspace}` for an unknown name."
  [table colorspace]
  (or (table colorspace)
      (throw (ex-info (str "Unknown colorspace: " colorspace) {:colorspace colorspace}))))

(defn- channel-error
  "Throws `ex-info` with `{:channel ch}`: valid channels are 0-3."
  [ch]
  (throw (ex-info (str "Invalid channel: " ch ", expected 0-3") {:channel ch})))

;;

(def ^{:doc "Alias for [[clojure2d.color.whitepoints/spectrum]]: creates a spectrum map `{:range :lambda :value}` from sampled spectral data."}
  spectrum wp/spectrum)

(def ^{:doc "Alias for [[clojure2d.color.whitepoints/->spectrum-to-XYZ1]]: builds a converter of a spectrum to XYZ with `Y` in 0-1."}
  ->spectrum-to-XYZ1 wp/->spectrum-to-XYZ1)

(defn ->spectrum-to-XYZ
  "Builds a converter of a spectrum (reflectance or transmittance) to XYZ under an illuminant and a standard observer, with `Y` in 0-100.

  Same as [[->spectrum-to-XYZ1]] (see `clojure2d.color.whitepoints/->spectrum-to-XYZ1` for the method), with the result scaled to the 0-100 range of [[to-XYZ]].

  Parameters:

  - `observer` (optional, default `:CIE-2`): `:CIE-2`, `:CIE-10`, `:CIE-2015-2`, `:CIE-2015-10` or a color matching functions map.
  - `illuminant` (optional, default `:D65`): a key of `clojure2d.color.whitepoints/illuminants-spectrum-data` or a spectrum map.
  - `options` (map, optional): `:interpolation` of the spectrum (default `:auto`), `:illuminant-interpolation` (default `:linear`), `:extrapolation` (`:constant` default, or `:zero`), `:step` (nm, default `1.0`).

  Returns a function of a spectrum map (`:lambda`, `:value`, optional `:range`, see [[spectrum]]) returning XYZ as `Vec4` (alpha 255); a perfect reflector gives `Y = 100`.

  Throws `ex-info` for an unsupported extrapolation, an unknown observer or illuminant.

  See also [[wavelength-to-XYZ]]."
  ([] (->spectrum-to-XYZ :CIE-2 :D65))
  ([observer illuminant] (->spectrum-to-XYZ observer illuminant nil))
  ([observer illuminant options] (let [->xyz1 (->spectrum-to-XYZ1 observer illuminant options)]
                                   (fn [spectrum]
                                     (-> spectrum ->xyz1 XYZ1-to-XYZ)))))

;;

(defn make-LCH
  "Create LCH conversion functions pair from any luma based color space. "
  [cs]
  (let [[to from] (colorspaces cs)]
    [(partial to-luma-color-hue to)
     (partial from-luma-color-hue from)]))

(defn complementary
  "Create complementary color. Possible colorspaces are:

  * `:PalettonHSV` (default)
  * `:HSB`, `:HSV`, `:HSL` or `:HWB`
  * `:GLHS`"
  (^Vec4 [c] (complementary c :PalettonHSV))
  (^Vec4 [c colorspace]
   (let [[to from] (colorspaces colorspace)
         ^Vec4 c (to c)]
     (from (if (= colorspace :GLHS)
             (Vec4. (+ 180.0 (.x c)) (.y c) (.z c) (.w c))
             (Vec4. (.x c) (+ 180.0 (.y c)) (.z c) (.w c)))))))

(defn set-channel
  "Returns the color with one channel set to `val`, optionally in another colorspace.

  - `col` color, any type accepted by [[to-color]]
  - `colorspace` (optional) a key of [[colorspaces]]: the color is converted with the raw `to-` function, the channel set, and converted back with `from-`
  - `channel` 0, 1, 2 or 3 (alpha)
  - `val` new value, in the colorspace's raw range, not clamped

  Returns a `Vec4`. Throws `ex-info` with `{:colorspace colorspace}` for an unknown colorspace and `{:channel channel}` for a channel outside 0-3. See also [[get-channel]], [[set-alpha]]."
  {:metadoc/categories meta-ops}
  (^Vec4 [col colorspace ^long channel ^double val]
   (let [[to from] (colorspace-converters colorspaces colorspace)
         ^Vec4 c (to col)]
     (from (case channel
             0 (Vec4. val (.y c) (.z c) (.w c))
             1 (Vec4. (.x c) val (.z c) (.w c))
             2 (Vec4. (.x c) (.y c) val (.w c))
             3 (Vec4. (.x c) (.y c) (.z c) val)
             (channel-error channel)))))
  (^Vec4 [col ^long channel ^double val]
   (let [^Vec4 c (pr/to-color col)]
     (case channel
       0 (Vec4. val (.y c) (.z c) (.w c))
       1 (Vec4. (.x c) val (.z c) (.w c))
       2 (Vec4. (.x c) (.y c) val (.w c))
       3 (Vec4. (.x c) (.y c) (.z c) val)
       (channel-error channel)))))

(defn get-channel
  "Returns one channel of a color, optionally in another colorspace.

  - `col` color, any type accepted by [[to-color]]
  - `colorspace` (optional) a key of [[colorspaces]]: the channel of the raw `to-` conversion
  - `channel` 0, 1, 2 or 3 (alpha)

  Returns a double. Throws `ex-info` with `{:colorspace colorspace}` for an unknown colorspace and `{:channel channel}` for a channel outside 0-3. See also [[set-channel]], [[ch0]]."
  {:metadoc/categories meta-ops}
  (^double [col colorspace ^long channel]
   (let [to (first (colorspace-converters colorspaces colorspace))]
     (get-channel (to col) channel)))
  (^double [col ^long channel]
   (case channel
     0 (ch0 col)
     1 (ch1 col)
     2 (ch2 col)
     3 (alpha col)
     (channel-error channel))))

(defn color-converter
  "Creates a function converting colors given in colorspace `cs` with custom channel maxima to RGB, as Processing `colorMode`.

  - `cs` a key of [[colorspaces*]]
  - `ch1-scale` .. `ch4-scale` maxima of the input channels; with one value it is used for all four channels, with three values alpha keeps the `[0,255]` range

  The returned function rescales each channel of its argument to `[0,255]` (`255 * v / max`), clamps, and applies the normalized `from-` function of `cs` (`from-XXX*`). With `cs` alone it returns that `from-XXX*` function. Throws `ex-info` with `{:colorspace cs}` for an unknown colorspace. See also [[colorspaces*]]."
  {:metadoc/categories meta-conv}
  ([cs ch1-scale ch2-scale ch3-scale ch4-scale]
   (let [colorspace-fn (second (colorspace-converters colorspaces* cs))]
     (fn [v] 
       (let [^Vec4 v (pr/to-color v)
             ch1 (* 255.0 (/ (.x v) ^double ch1-scale))
             ch2 (* 255.0 (/ (.y v) ^double ch2-scale))
             ch3 (* 255.0 (/ (.z v) ^double ch3-scale))
             ch4 (* 255.0 (/ (.w v) ^double ch4-scale))]
         (colorspace-fn (v/fmap (Vec4. ch1 ch2 ch3 ch4) clamp255))))))
  ([cs ch1-scale ch2-scale ch3-scale] (color-converter cs ch1-scale ch2-scale ch3-scale 255.0))
  ([cs ch-scale] (color-converter cs ch-scale ch-scale ch-scale ch-scale))
  ([cs] (second (colorspace-converters colorspaces* cs))))

;; black body

(defn- kelvin-red
  ^double [^double k ^double lnk]
  (if (< k 0.65)
    255.0
    (min 255.0 (* 255.0 (rgb/linear-to-srgb (+ 0.32068362618584273
                                               (* 0.19668730877673762 (m/pow (+ -0.21298613432655075 k) -1.5139012907556737))
                                               (* -0.013883432789258415 lnk)))))))

(defn- kelvin-green1
  ^double [^double k ^double lnk]
  (let [eek (+ k -0.44267061967913873)]
    (max 0.0 (* 255.0 (rgb/linear-to-srgb (+ 1.226916242502167
                                             (* -1.3109482654223614 eek eek eek (m/exp (* eek -5.089297600846147)))
                                             (* 0.6453936305542096 lnk)))))))

(defn- kelvin-green2
  ^double [^double k ^double lnk]
  (* 255.0 (rgb/linear-to-srgb (+ 0.4860175851734596
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
            (m/constrain (* 255.0 (rgb/linear-to-srgb (+ 1.677499032830161
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

(defn wavelength-to-XYZ
  "Returns the XYZ color of monochromatic light of a given wavelength: the values of the color matching functions.

  Parameters:

  - `lambda` (double): wavelength in nm.
  - `observer` (keyword, optional): `:CIE-2` (default), `:CIE-10`, `:CIE-2015-2` or `:CIE-2015-10`.

  Returns XYZ as `Vec4` scaled as [[to-XYZ]] (the peak of `ȳ` gives `Y = 100`), alpha 255. Wavelengths between the 1 nm data samples are interpolated (Sprague); outside the observer's range (360-830 nm, 390-830 nm for 2015 observers) the result is black.

  Throws `ex-info` with `{:observer observer}` for an unknown observer.

  See also [[wavelength]], `clojure2d.color.whitepoints/cmf`."
  (^Vec4 [^double lambda] (wavelength-to-XYZ lambda :CIE-2))
  (^Vec4 [^double lambda observer]
   (let [[x y z] (wp/cmf observer)]
     (XYZ1-to-XYZ (Vec3. (x lambda) (y lambda) (z lambda))))))

(defn wavelength
  "Returns color from given wavelength in nm"
  (^Vec4 [^double lambda] (wavelength lambda :CIE-2))
  (^Vec4 [^double lambda observer]
   (-> lambda (wavelength-to-XYZ observer) from-XYZ clamp)))

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
              :rgb (fn [e n r a] (Vec4. e n r a))}
        d180 {:a [1.0 1.0]
              :b [1.0 0.8]
              :f (fn ^double [^double e]
                   (if (== e 180.0) -1.0
                       (* 0.5 (m/tan (* m/HALF_PI (/ (- e 120.0) 60.0))))))
              :fi (fn ^double [^double e]
                    (if (== e -1.0) 180.0
                        (+ 120.0 (* 2.0 (/ (* (m/atan (/ e 0.5)) 60.0) m/PI)))))
              :g i
              :rgb (fn [e n r a] (Vec4. n e r a))}
        d210 {:a [1.0 0.8]
              :b [1.0 0.6]
              :f (fn ^double [^double e]
                   (if (== e 180.0) -1.0
                       (* 0.75 (m/tan (* m/HALF_PI (/ (- 210.0 e) 30.0))))))
              :fi (fn ^double [^double e]
                    (if (== e -1.0) 180.0
                        (- 210.0 (* 2.0 (/ (* (m/atan (/ e 0.75)) 30.0) m/PI)))))
              :g s
              :rgb (fn [e n r a] (Vec4. r e n a))}
        d255 {:a [1.0 0.6]
              :b [0.85 0.7]
              :f (fn ^double [^double e]
                   (if (== e 255.0) -1.0
                       (* 1.33 (m/tan (* m/HALF_PI (/ (- e 210.0) 45.0))))))
              :fi (fn ^double [^double e]
                    (if (== e -1.0) 255.0
                        (+ 210.0 (* 2.0 (/ (* (m/atan (/ e 1.33)) 45.0) m/PI)))))
              :g i
              :rgb (fn [e n r a] (Vec4. r n e a))}
        d315 {:a [0.85 0.7]
              :b [1.0 0.65]
              :f (fn ^double [^double e]
                   (if (== e 255.0) -1.0
                       (* 1.33 (m/tan (* m/HALF_PI (/ (- 315.0 e) 60.0))))))
              :fi (fn ^double [^double e]
                    (if (== e -1.0) 255.0
                        (- 315.0 (* 2.0 (/ (* (m/atan (/ e 1.33)) 60.0) m/PI)))))
              :g s
              :rgb (fn [e n r a] (Vec4. n r e a))}
        d360 {:a [1.0 0.65]
              :b [1.0 1.0]
              :f (fn ^double [^double e]
                   (if (zero? e) -1.0
                       (* 1.33 (m/tan (* m/HALF_PI (/ (- e 315.0) 45.0))))))
              :fi (fn ^double [^double e]
                    (if (== e -1.0) 0.0
                        (+ 315.0 (* 2.0 (/ (* (m/atan (/ e 1.33)) 45.0) m/PI)))))
              :g i
              :rgb (fn [e n r a] (Vec4. e r n a))}]
    (fn [^double h]
      (condp clojure.core/> h
        120.0 d120
        180.0 d180
        210.0 d210
        255.0 d255
        315.0 d315
        360.0 d360))))

(defn- adjust-s-v-rev
  ^double [^double e ^double t] (if (<= t 1.0)
                                  (* e t)
                                  (+ e (* (- 1.0 e) (dec t)))))

(defn- paletton-hsv-to-rgb
  "Paletton version of HSV to RGB converter"
  (^Vec4 [^double hue ^double ks ^double kv ^double alpha]
   (let [ks (m/constrain ks 0.0 2.0)
         kv (m/constrain kv 0.0 2.0)
         h (wrap-hue hue)
         {:keys [a b f g rgb]} (paletton-base-data h)
         av (second a)
         bv (second b)
         as (first a)
         bs (first b)
         ^double n (f h)
         v (adjust-s-v-rev (g av bv n) kv)
         s (adjust-s-v-rev (g as bs n) ks)
         r (* 255.0 v)
         b (* r (- 1.0 s))
         g (if (== n -1.0) b
               (/ (+ r (* n b)) (inc n)))]
     (rgb r g b alpha)))
  (^Vec4 [^double hue ^double ks ^double kv]
   (paletton-hsv-to-rgb hue ks kv 255.0)))

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
           s (i (if (== l p) -1.0
                    (/ (- f l) (- l p))))]
       s)))
  ([c] (let [cc (to-color c)]
         (hue-paletton (.x cc) (.y cc) (.z cc)))))

(defn- get-s-v
  [^double h]
  (let [{:keys [a b f g]} (paletton-base-data h)
        av (second a)
        bv (second b)
        as (first a)
        bs (first b)
        r (f h)
        s (g av bv r)
        i (g as bs r)]
    [i s]))

(defn- adjust-s-v
  ^double [^double e ^double t]
  (if (zero? e)
    0.0
    (if (<= t e)
      (/ t e)
      (inc (/ (- t e) (- 1.0 e))))))

(defn- paletton-rgb-to-hsv
  (^Vec4 [^double r ^double g ^double b ^double alpha]
   (if (== r g b)
     (Vec4. 0.0 0.0 (/ (+ (* 0.299 r) (* 0.587 g) (* 0.114 b)) 255.0) alpha)
     (let [f (max r g b)
           p (min r g b)
           h (hue-paletton r g b)
           [s v] (get-s-v h)
           n (adjust-s-v s (/ (- f p) f))
           r (adjust-s-v v (/ f 255.0))]
       (Vec4. h n r alpha))))
  (^Vec4 [^double r ^double g ^double b]
   (paletton-rgb-to-hsv r g b 255.0)))

;; List of paletton presets
(defonce ^:private paletton-presets
  {:pale-light          [[0.24649 1.78676] [0.09956 1.95603] [0.17209 1.88583] [0.32122 1.65929] [0.39549 1.50186]]
   :pastels-bright      [[0.65667 1.86024] [0.04738 1.99142] [0.39536 1.89478] [0.90297 1.85419] [1.86422 1.8314]]
   :shiny               [[1.00926 2]       [0.3587 2]        [0.5609 2]        [2 0.8502] [2 0.65438]]
   :pastels-lightest    [[0.34088 1.09786] [0.13417 1.62645] [0.23137 1.38072] [0.45993 0.92696] [0.58431 0.81098]]
   :pastels-very-light  [[0.58181 1.32382] [0.27125 1.81913] [0.44103 1.59111] [0.70192 1.02722] [0.84207 0.91425]]
   :full                [[1 1]             [0.61056 1.24992] [0.77653 1.05996] [1.06489 0.77234] [1.25783 0.60685]]
   :pastels-light       [[0.37045 0.90707] [0.15557 1.28367] [0.25644 1.00735] [0.49686 0.809] [0.64701 0.69855]]
   :pastels-med         [[0.66333 0.8267]  [0.36107 1.30435] [0.52846 0.95991] [0.78722 0.70882] [0.91265 0.5616]]
   :darker              [[0.93741 0.68672] [0.68147 0.88956] [0.86714 0.82989] [1.12072 0.5673] [1.44641 0.42034]]
   :pastels-mid-pale    [[0.38302 0.68001] [0.15521 0.98457] [0.26994 0.81586] [0.46705 0.54194] [0.64065 0.44875]]
   :pastels             [[0.66667 0.66667] [0.33333 1]       [0.5 0.83333]     [0.83333 0.5] [1 0.33333]]
   :dark-neon           [[0.94645 0.59068] [0.99347 0.91968] [0.93954 0.7292]  [1.01481 0.41313] [1.04535 0.24368]]
   :pastels-dark        [[0.36687 0.39819] [0.25044 0.65561] [0.319 0.54623]   [0.55984 0.37953] [0.70913 0.3436]]
   :pastels-very-dark   [[0.60117 0.41845] [0.36899 0.59144] [0.42329 0.44436] [0.72826 0.35958] [0.88393 0.27004]]
   :dark                [[1.31883 0.40212] [0.9768 0.25402]  [1.27265 0.30941] [1.21289 0.60821] [1.29837 0.82751]]
   :pastels-mid-dark    [[0.26952 0.22044] [0.23405 0.52735] [0.23104 0.37616] [0.42324 0.20502] [0.54424 0.18483]]
   :pastels-darkest     [[0.53019 0.23973] [0.48102 0.50306] [0.50001 0.36755] [0.6643 0.32778] [0.77714 0.3761]]
   :darkest             [[1.46455 0.21042] [0.99797 0.16373] [0.96326 0.274]   [1.56924 0.45022] [1.23016 0.66]]
   :almost-black        [[0.12194 0.15399] [0.34224 0.50742] [0.24211 0.34429] [0.31846 0.24986] [0.52251 0.33869]]
   :almost-gray-dark    [[0.10266 0.24053] [0.13577 0.39387] [0.11716 0.30603] [0.14993 0.22462] [0.29809 0.19255]]
   :almost-gray-darker  [[0.07336 0.36815] [0.18061 0.50026] [0.09777 0.314]   [0.12238 0.25831] [0.14388 0.1883]]
   :almost-gray-mid     [[0.07291 0.59958] [0.19602 0.74092] [0.10876 0.5366]  [0.15632 0.48229] [0.20323 0.42268]]
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
;; https://www.imatest.com/docs/colorcheck_ref/

(defn delta-E*
  "ΔE*_ab difference, CIE 1976"
  (^double [c1 c2] (delta-E* c1 c2 nil))
  (^double [c1 c2 {:keys [colorspace]
                   :or {colorspace :LAB}}]
   (let [to (first (colorspaces colorspace))
         ^Vec4 c1 (to c1)
         ^Vec4 c2 (to c2)]
     (m/hypot-sqrt (- (.x c2) (.x c1))
                   (- (.y c2) (.y c1))
                   (- (.z c2) (.z c1))))))

(defn- delta-ab
  ^double [^Vec4 c1 ^Vec4 c2]
  (- (m/hypot-sqrt (.y c1) (.z c1))
     (m/hypot-sqrt (.y c2) (.z c2))))

(defn delta-C*
  "ΔC*_ab difference, chroma difference in LAB color space, CIE 1976"
  {:metadoc/categories #{:dist}}
  (^double [c1 c2] (delta-C* c1 c2 nil))
  (^double [c1 c2 {:keys [colorspace]
                   :or {colorspace :LAB}}]
   (let [to (first (colorspaces colorspace))]
     (delta-ab (to c1) (to c2)))))

(def ^{:deprecated "Use delta-C*"} delta-c delta-C*)

(defn delta-H*
  "ΔH* difference, hue difference in LAB, CIE 1976"
  {:metadoc/categories #{:dist}}
  (^double [c1 c2] (delta-H* c1 c2 nil))
  (^double [c1 c2 {:keys [colorspace]
                   :or {colorspace :LAB}}]
   (let [to (first (colorspaces colorspace))
         ^Vec4 c1 (to c1)
         ^Vec4 c2 (to c2)]
     (m/safe-sqrt (- (+ (m/sq (- (.y c2) (.y c1)))
                        (m/sq (- (.z c2) (.z c1))))
                     (m/sq (delta-ab c1 c2)))))))

(def ^{:deprecated "Use delta-H*"} delta-h delta-H*)

(defn delta-E-HyAB
  "ΔE_HyAB difference"
  (^double [c1 c2] (delta-E-HyAB c1 c2 nil))
  (^double [c1 c2 {:keys [colorspace]
                   :or {colorspace :LAB}}]
   (let [to (first (colorspaces colorspace))
         ^Vec4 c1 (to c1)
         ^Vec4 c2 (to c2)]
     (+ (m/hypot-sqrt (- (.y c2) (.y c1))
                      (- (.z c2) (.z c1)))
        (m/abs (- (.x c2) (.x c1)))))))

(defn delta-E*-94
  "ΔE* difference, CIE 1994"
  {:metadoc/categories #{:dist}}
  (^double [c1 c2] (delta-E*-94 c1 c2 nil))
  (^double [c1 c2 {:keys [textiles? colorspace]
                   :or {textiles? false colorspace :LAB}}]
   (let [k1 (if textiles? 0.048 0.045)
         k2 (if textiles? 0.014 0.015)
         to (first (colorspaces colorspace))
         ^Vec4 c1 (to c1)
         ^Vec4 c2 (to c2)
         C1* (m/hypot-sqrt (.y c1) (.z c1))
         C2* (m/hypot-sqrt (.y c2) (.z c2))
         Sc (inc (* k1 C1*))
         Sh (inc (* k2 C1*))
         SL (if textiles? 2.0 1.0)
         dab (- C1* C2*)]
     (m/sqrt (+ (m/sq (/ (- (.x c2) (.x c1)) SL))
                (m/sq (/ dab Sc))
                (m/sq (/ (m/safe-sqrt (- (+ (m/sq (- (.y c2) (.y c1)))
                                            (m/sq (- (.z c2) (.z c1))))
                                         (m/sq dab))) Sh)))))))

(def ^{:metadoc/categories #{:dist} :deprecated "Use delta-E*"
     :doc "Delta E CIE distance (euclidean in LAB colorspace."}
  delta-e-cie delta-E*)

(defn delta-E*-euclidean
  ^{:metadoc/categories #{:dist}
    :doc "Euclidean distance in given colorspace (default Oklab)."}
  (^double [c1 c2] (delta-E*-euclidean c1 c2 nil))
  (^double [c1 c2 {:keys [colorspace]
                   :or {colorspace :Oklab}}]
   (let [to (first (colorspaces colorspace))
         ^Vec4 c1 (to c1)
         ^Vec4 c2 (to c2)]
     (m/hypot-sqrt (- (.x c2) (.x c1))
                   (- (.y c2) (.y c1))
                   (- (.z c2) (.z c1))))))

(defn delta-C-RGB
  "ΔC in RGB color space"
  ^double [c1 c2]
  (let [^Vec4 c1 (pr/to-color c1)
        ^Vec4 c2 (pr/to-color c2)
        dR (- (.x c2) (.x c1))
        dG (- (.y c2) (.y c1))
        dB (- (.z c2) (.z c1))
        r (* 0.5 (+ (.x c1) (.x c2)))]
    (m/sqrt (+ (* (+ 2.0 (/ r 256.0)) dR dR)
               (* 4.0 dG dG)
               (* (+ 2.0 (/ (- 255.0 r) 256.0)) dB dB)))))

(defn- diff-H
  ^double [^double h1 ^double h2]
  (if (pos? (* h1 h2))
    (m/abs (- h1 h2))
    (min (+ (m/abs h1) (m/abs h2))
         (+ (- 180.0 (m/abs h1)) (- 180.0 (m/abs h2))))))

;; http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.125.3833&rep=rep1&type=pdf
(defn delta-D-HCL
  "Color difference in HCL (Sarifuddin and Missaou) color space"
  (^double [c1 c2] (delta-D-HCL c1 c2 nil))
  (^double [c1 c2 {:keys [colorspace]
                   :or {colorspace :HCL}}]
   (let [to (first (colorspaces colorspace))
         ^Vec4 c1 (to c1)
         ^Vec4 c2 (to c2)
         dH (diff-H (.x c2) (.x c1))
         dL (- (.z c2) (.z c1))]
     (m/sqrt (+ (m/sq (* 1.4456 dL))
                (* (+ dH 0.16)
                   (+ (m/sq (.y c1))
                      (m/sq (.y c2))
                      (* -2.0 (.y c1) (.y c2) (m/cos (m/radians dH))))))))))

(defn delta-E*-CMC
  "ΔE* CMC difference

  Parameters `l` and `c` defaults to 1.0. Other common settings is `l=2.0` and `c=1.0`."
  {:metadoc/categories #{:dist}}
  (^double [c1 c2] (delta-E*-CMC c1 c2 nil))
  (^double [c1 c2 {:keys [^double l ^double c colorspace]
                   :or {l 1.0 c 1.0 colorspace :LAB}}]
   (let [to (first (colorspaces colorspace))
         ^Vec4 c1 (to c1)
         ^Vec4 c2 (to c2)
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

(def ^{:deprecated "Use delta-E*-CMC"} delta-e-cmc delta-E*-CMC)

(defn delta-E-z
  "ΔE* calculated in JAB color space."
  {:metadoc/categories #{:dist}}
  (^double [c1 c2] (delta-E-z c1 c2 nil))
  (^double [c1 c2 {:keys [colorspace]
                   :or {colorspace :JAB}}]
   (let [to (first (colorspaces colorspace))
         ^Vec4 c1 (to c1)
         ^Vec4 c2 (to c2)
         J1 (.x c1)
         J2 (.x c2)
         a1 (.y c1)
         a2 (.y c2)
         b1 (.z c1)
         b2 (.z c2)
         C1 (m/hypot-sqrt a1 b1)
         C2 (m/hypot-sqrt a2 b2)
         h1 (m/atan2 b1 a1)
         h2 (m/atan2 b2 a2)]
     (m/hypot-sqrt (- J2 J1)
                   (- C2 C1)
                   (* 2.0 (m/sqrt (* C1 C2)) (m/sin (* 0.5 (- h2 h1))))))))

(def ^{:deprecated "Use delta-e-jab"} delta-e-jab delta-E-z)

(def ^{:private true :const true :tag 'long} p25_7 (* 25 25 25 25 25 25 25))
(def ^{:private true :const true :tag 'double} r30 (m/radians 30.0))
(def ^{:private true :const true :tag 'double} r6 (m/radians 6.0))
(def ^{:private true :const true :tag 'double} r63 (m/radians 63.0))

(defn- de2000p7
  ^double [^double v]
  (let [v2 (* v v)
        v4 (* v2 v2)
        v7 (* v4 v2 v)]
    (m/sqrt (/ v7 (+ v7 p25_7)))))

(defn delta-E*-2000
  "ΔE* color difference, CIE 2000

  http://www2.ece.rochester.edu/~gsharma/ciede2000/ciede2000noteCRNA.pdf"
  (^double [c1 c2] (delta-E*-2000 c1 c2 nil))
  (^double [c1 c2 {:keys [^double l ^double c ^double h colorspace]
                   :or {l 1.0 c 1.0 h 1.0 colorspace :LAB}}]
   (let [to (first (colorspaces colorspace))
         ^Vec4 c1 (to c1)
         ^Vec4 c2 (to c2)
         C1* (m/hypot-sqrt (.y c1) (.z c1))
         C2* (m/hypot-sqrt (.y c2) (.z c2))
         Cm* (* 0.5 (+ C1* C2*))
         G+ (inc (* 0.5 (- 1.0 (de2000p7 Cm*))))
         a1' (* G+ (.y c1))
         a2' (* G+ (.y c2))
         C1' (m/hypot-sqrt a1' (.z c1))
         C2' (m/hypot-sqrt a2' (.z c2))
         h1' (m/degrees (m/atan2 (.z c1) a1'))
         h1' (if (neg? h1') (+ h1' 360.0) h1')
         h2' (m/degrees (m/atan2 (.z c2) a2'))
         h2' (if (neg? h2') (+ h2' 360.0) h2')
         dL' (- (.x c2) (.x c1))
         dC' (- C2' C1')
         diffh' (- h2' h1')
         adiffh' (m/abs diffh')
         C1'C2' (* C1' C2')
         dh' (m/radians (double (cond
                                  (zero? C1'C2') 0.0
                                  (<= adiffh' 180.0) diffh'
                                  (> diffh' 180.0) (- diffh' 360.0)
                                  :else (+ diffh' 360.0))))
         dH' (* 2.0 (m/sqrt C1'C2') (m/sin (* 0.5 dh')))
         Lm' (* 0.5 (+ (.x c1) (.x c2)))
         Cm' (* 0.5 (+ C1' C2'))
         h1'+h2' (+ h1' h2')
         ^double hm' (cond
                       (zero? C1'C2') h1'+h2'
                       (<= adiffh' 180.0) (* 0.5 h1'+h2')
                       (< h1'+h2' 360.0) (* 0.5 (+ h1'+h2' 360.0))
                       :else (* 0.5 (- h1'+h2' 360.0)))
         hm'r (m/radians hm')
         T (+ 1.0
              (* -0.17 (m/cos (- hm'r r30)))
              (* 0.24 (m/cos (* 2.0 hm'r)))
              (* 0.32 (m/cos (+ (* 3.0 hm'r) r6)))
              (* -0.2 (m/cos (- (* 4.0 hm'r) r63))))
         dtheta (* r30 (m/exp (- (m/sq (/ (- hm' 275.0) 25.0)))))
         Rc (* 2.0 (de2000p7 Cm'))
         Lm'-50sq (m/sq (- Lm' 50.0))
         Sl (inc (/ (* 0.015 Lm'-50sq)
                    (m/sqrt (+ 20.0 Lm'-50sq))))
         Sc (inc (* 0.045 Cm'))
         Sh (inc (* 0.015 Cm' T))
         Rt (- (* (m/sin (* 2.0 dtheta)) Rc))
         l' (/ dL' (* l Sl))
         c' (/ dC' (* c Sc))
         h' (/ dH' (* h Sh))]
     (m/sqrt (+ (m/sq l') (m/sq c') (m/sq h')
                (* Rt c' h'))))))

(defn contrast-ratio
  "WCAG contrast ratio.

  Based on YUV luma."
  {:metadoc/categories #{:dist}}
  ^double [c1 c2]
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
  ([c1 c2] (noticable-different? c1 c2 nil))
  ([c1 c2 {:keys [^double s ^double p colorspace]
           :or {s 0.1 p 0.5 colorspace :LAB}} ]
   (let [to (first (colorspaces colorspace))
         c1 (to c1)
         c2 (to c2)
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
               dist (m/abs (double (dist-fn c c1)))]
           (recur (unchecked-inc i)
                  (if (< dist currdist) c1 currc)
                  (if (< dist currdist) dist currdist)))
         currc))))
  ([pal c] (nearest-color pal c v/dist))
  ([pal] (partial nearest-color pal)))

(defn average
  "Average colors in given `colorspace` (default: `:sRGB`)"
  {:metadoc/categories #{:interp}}
  ([cs colorspace]
   (let [[to from] (colorspaces colorspace)]
     (from (v/average-vectors (map to cs)))))
  ([cs]
   (v/average-vectors (map pr/to-color cs))))

(defn weighted-average
  "Average colors with weights in given `colorspace` (default: `:sRGB`)"
  {:metadoc/categories #{:interp}}
  ([cs weights colorspace]
   (let [[to from] (colorspaces colorspace)]
     (from (v/div (reduce v/add (map #(v/mult (to %1) %2) cs weights)) (reduce m/+ weights)))))
  ([cs weights]
   (v/div (reduce v/add (map #(v/mult (pr/to-color %1) %2) cs weights)) (reduce m/+ weights))))

(defn lerp
  "Linear interpolation of two colors.

  See also [[lerp+]], [[lerp-]], [[gradient]] and [[mix]]"
  {:metadoc/categories #{:interp}}
  ([c1 c2 colorspace ^double t]
   (let [[to from] (colorspaces colorspace)]
     (from (lerp (to c1) (to c2) t))))
  ([c1 c2] (lerp c1 c2 0.5))
  ([c1 c2 ^double t]
   (v/interpolate (pr/to-color c1) (pr/to-color c2) t)))

(defn lerp-
  "Linear interpolation of two colors conserving luma of the first color.

  Amount: strength of the blend (defaults to 0.5)

  See also [[lerp+]]."
  {:metadoc/categories #{:interp}}
  ([c1 c2 colorspace ^double amount]
   (let [[to from] (colorspaces colorspace)
         cc1 (to c1)
         cc2 (to c2)
         res (from (lerp cc1 cc2 amount))]
     (set-channel res :LAB 0 (ch0 (to-LAB c1)))))
  ([c1 c2] (lerp- c1 c2 0.5))
  ([c1 c2 ^double amount]
   (let [res (lerp c1 c2 amount)]
     (set-channel res :LAB 0 (ch0 (to-LAB c1))))))

(defn lerp+
  "Linear interpolation of two colors conserving luma of the second color.

  Amount: strength of the blend (defaults to 0.5).

  See also [[lerp-]]."
  ([c1 c2 colorspace ^double amount]
   (lerp- c2 c1 colorspace (- 1.0 amount)))
  ([c1 c2] (lerp- c1 c2 0.5))
  ([c1 c2 ^double amount]
   (lerp- c2 c1 (- 1.0 amount))))

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

(defn mixbox
  "Pigment based color mixing.

  https://github.com/scrtwpns/mixbox"
  (^Vec4 [col1 col2] (mixbox col1 col2 0.5))
  (^Vec4 [col1 col2 ^double t]
   (let [^int c1 (pack col1)
         ^int c2 (pack col2)]
     (pr/to-color (Mixbox/lerp c1 c2 (float t))))))

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

  `tint-color`"
  {:metadoc/categories #{:pal}}
  ([tint-color]
   (let [tint (v/add (pr/to-color tint-color) vec4-one)]
     (fn [c]
       (let [c (v/add (pr/to-color c) vec4-one)]
         (clamp (v/mult (v/sqrt (v/div (v/emult c tint) 65536.0)) 255.0)))))))

(defn reduce-colors
  "Reduce colors using k-means++ (or fuzzy-kmeans) clustering in given `colorspace` (default `:RGB`).

  Use for long sequences (for example to generate palette from image)."
  {:metadoc/categories #{:pal}}
  ([xs number-of-colors] (reduce-colors xs number-of-colors nil))
  ([xs number-of-colors {:keys [fuzzy? colorspace]
                         :or {fuzzy? true}
                         :as options}]
   (let [[to from] (if colorspace (colorspaces* colorspace) [identity identity])
         clustering-fn (if fuzzy? cl/fuzzy-kmeans cl/kmeans++)]     
     (sort-by luma ;; sort by brightness
              (for [{:keys [data]} (clustering-fn (map to xs) (assoc options :clusters number-of-colors))]
                (->> (map pack data) ;; pack colors into integers
                     (stat/modes) ;; find colors which appears most often
                     (map (comp pr/to-color unchecked-long)) ;; convert back to colors
                     (v/average-vectors) ;; average vectors if necessary
                     (from))))))) ;; convert back to RGB

;; colors

(defn whiten
  "Change color towards white.

  Works in HSB color space. Default amount is set to 0.2 and changes W channel by this amount.

  See [[blacken]]."
  {:metadoc/categories meta-ops}
  (^Vec4 [col] (whiten col 0.2))
  (^Vec4 [col ^double amt]
   (let [c (to-HWB col)]
     (clamp (from-HWB (Vec4. (.x c) (m/constrain (+ (.y c) amt) 0.0 1.0) (.z c) (.w c)))))))

(defn blacken
  "Change color towards black.

  Works in HSB color space. Default amount is set to 0.2 and changes B channel by this amount."
  {:metadoc/categories meta-ops}
  (^Vec4 [col] (blacken col 0.2))
  (^Vec4 [col ^double amt]
   (let [c (to-HWB col)]
     (clamp (from-HWB (Vec4. (.x c) (.y c) (m/constrain (+ (.z c) amt) 0.0 1.0) (.w c)))))))

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
     (from (case channel
             0 (Vec4. (+ value (.x c)) (.y c) (.z c) (.w c))
             1 (Vec4. (.x c) (+ value (.y c)) (.z c) (.w c))
             2 (Vec4. (.x c) (.y c) (+ value (.z c)) (.w c))
             3 (Vec4. (.x c) (.y c) (.z c) (+ value (.w c)))
             c))))
  (^Vec4 [col ^long channel ^double value]
   (let [^Vec4 c (pr/to-color col)]
     (case channel
       0 (Vec4. (+ value (.x c)) (.y c) (.z c) (.w c))
       1 (Vec4. (.x c) (+ value (.y c)) (.z c) (.w c))
       2 (Vec4. (.x c) (.y c) (+ value (.z c)) (.w c))
       3 (Vec4. (.x c) (.y c) (.z c) (+ value (.w c)))
       c))))

(defn modulate
  "Modulate (multiply) chosen channel by given amount. Works with any color space."
  {:metadoc/categories meta-ops}
  (^Vec4 [col colorspace ^long channel ^double amount]
   (let [[to from] (colorspaces colorspace)
         ^Vec4 c (to col)]
     (from (case channel
             0 (Vec4. (* amount (.x c)) (.y c) (.z c) (.w c))
             1 (Vec4. (.x c) (* amount (.y c)) (.z c) (.w c))
             2 (Vec4. (.x c) (.y c) (* amount (.z c)) (.w c))
             3 (Vec4. (.x c) (.y c) (.z c) (* amount (.w c)))
             c))))
  (^Vec4 [col ^long channel ^double amount]
   (let [^Vec4 c (pr/to-color col)]
     (case channel
       0 (Vec4. (* amount (.x c)) (.y c) (.z c) (.w c))
       1 (Vec4. (.x c) (* amount (.y c)) (.z c) (.w c))
       2 (Vec4. (.x c) (.y c) (* amount (.z c)) (.w c))
       3 (Vec4. (.x c) (.y c) (.z c) (* amount (.w c)))
       c))))

(defn adjust-temperature
  "Adjust temperature of color.

  Default amount: 0.35
  
  See [[temperature]] and [[lerp+]]."
  {:metadoc/categories meta-ops}
  ([c temp] (adjust-temperature c temp 0.35))
  ([c temp amount]
   (let [t (temperature temp)]
     (lerp+ (pr/to-color c) t amount))))

;;

(defn- interpolated-gradient
  [palette-or-gradient-name {:keys [colorspace interpolation domain to? from?]
                             :or {colorspace :RGB
                                  to? true from? true
                                  interpolation :linear}}]
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
            ifn (if (keyword? interpolation) (partial i/interpolation interpolation) interpolation)
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
                                           (v/fmap (from-Cubehelix (Vec4. (- (* t 360.0) 100.0)
                                                                          (- 1.5 (* 1.5 ts))
                                                                          (- 0.8 (* 0.9 ts))
                                                                          255.0))
                                                   clamp255)))
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
      * `:interpolation` - interpolation name or interpolation function. Interpolation can be one of [[interpolators-1d-list]] or [[easings-list]]. When easings is used, only first two colors are interpolated. Default: `:linear`.
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
              (seqable? palette-or-gradient-name)
              (every? valid-color? palette-or-gradient-name)) (apply iq-gradient palette-or-gradient-name)       
         :else (interpolated-gradient palette-or-gradient-name options))))

(defn merge-gradients
  "Combine two gradients, optionally select `midpoint` (0.5 by default).

  Resulting gradient has colors from `g1` for values lower than `midpoint` and from `g2` for values higher than `midpoint`"
  {:metadoc/categories #{:gr}}
  ([g1 g2 ^double midpoint]
   (fn [^double t]
     (if (< t midpoint)
       (g1 (m/norm t 0.0 midpoint))
       (g2 (m/norm t midpoint 1.0)))))
  ([g1 g2] (merge-gradients g1 g2 0.5)))

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
  ([p ^long number-of-colors gradient-params]
   (vec (if (fn? p)
          (if (m/one? number-of-colors)
            [(if-let [cs (:colorspace gradient-params)]
               (average (m/sample p 100) cs)
               (average (m/sample p 100)))]
            (m/sample p number-of-colors))
          (if (m/one? number-of-colors)
            [(if-let [cs (:colorspace gradient-params)]
               (average (palette p) cs)
               (average (palette p)))]
            (palette (gradient (palette p) gradient-params) number-of-colors))))))

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
           xs (m/slice-range 0.0 1.0 200)
           ls (map (fn [^double v] (ch0 (to-LAB (palette-or-gradient v)))) xs)
           i (li/linear ls xs)]
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
     (iq-gradient (first pal) (first (drop 5 pal))))))

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
               :interpolation (r/randval :linear :cubic)}]
    (condp clojure.core/> (r/drand)
      0.1 (iq-random-gradient)
      0.6 (gradient (palette) gpars)
      0.7 (gradient (sort-by luma (paletton :monochromatic (r/drand 360)
                                            {:preset (rand-nth paletton-presets-list)})) gpars)
      (gradient))))

(defonce ^:private thing-presets
  ;; L range and C range
  {:intense-light [[204.0 255.0] [229.5 255.0]]
   :intense-dark  [[51.0 89.25]  [229.5 255.0]]
   :light         [[229.5 255.0] [76.5 178.5]]
   :dark          [[38.25 102.0] [178.5 255.0]]
   :bright        [[204.0 255.0] [191.25 242.25]]
   :weak          [[178.5 255.0] [38.25 76.5]]
   :neutral       [[76.5 178.5]  [63.75 89.25]]
   :fresh         [[204.0 255.0] [102.0 204.0]]
   :soft          [[153.0 229.5] [51.0 76.5]]
   :hard          [[102.0 255.0] [216.75 242.25]]
   :warm          [[102.0 229.5] [153.0 229.5]]
   :cool          [[229.5 255.0] [12.75 51.0]]
   :all           [[0.0 255.0]   [0.0 255.0]]
   :gray-light    [[178.5 255.0] [0.0 0.0]]
   :gray-dark     [[0.0 76.5]    [0.0 0.0]]
   :gray          [[76.5 178.5]  [0.0 0.0]]})

(defonce thing-presets-list (set (keys thing-presets)))
(defonce color-themes (sort (concat thing-presets-list paletton-presets-list)))

(defn- random-channel-value
  ^double [scheme ^double value]
  (cond
    (number? scheme) (let [v (double scheme)]
                       (r/drand (- value v)
                                (+ value v)))
    (sequential? scheme) (let [[mn mx] scheme]
                           (r/drand mn mx))
    :else value))

(defn- apply-LCH-theme
  [color [L C H]]
  (let [^Vec4 c (to-LCH* color)]
    (set-alpha (from-LCH* (map random-channel-value [L C H] c)) (.w c))))

(defn apply-theme
  "Apply theme to the color, see [[color-themes]] for names.

  Generates random color similar to provided one.
  All operations are done in LSH* color space (normalized to have values between 0 and 255).

  Color theme can be one of:

  * keyword - predefined color theme is used
  * [L C H] - randomization scheme of given channel, which can be:
      - [min-value max-value] - uniform random value from given range
      - a number - [channel-value, channel+value] range is used for random selection
      - nil, or anything else - keep original channel value"
  [color color-theme]
  (clamp (if (keyword color-theme)
           (if (thing-presets-list color-theme)
             (let [[[l1 l2] [c1 c2]] (thing-presets color-theme)
                   ^Vec4 c (to-LCH* color)]
               (from-LCH* (Vec4. (r/drand l1 l2) (r/drand c1 c2) (.z c) (.w c))))
             (let [g (->> {:preset color-theme}
                          (paletton :monochromatic (hue-paletton color))
                          (sort-by luma)
                          (gradient))]
               (set-alpha (g (r/drand)) (alpha color))))
           (apply-LCH-theme color color-theme))))

(defn random-color
  "Generate random color.

  Optionally color theme or alpha can be provided.

  List of possible color themes is stored in `color-themes` var. These are taken from thi.ng and paletton."
  {:metadoc/categories #{:pal}}
  ([color-theme alpha]
   (clamp (if (thing-presets-list color-theme)
            (let [[[l1 l2] [c1 c2]] (thing-presets color-theme)]
              (from-LCH* [(r/drand l1 l2) (r/drand c1 c2) (r/drand 255.0) alpha]))
            (set-alpha (rand-nth (paletton :monochromatic (r/drand 360.0) {:preset color-theme})) alpha))))
  ([alpha-or-color-theme]
   (if (keyword? alpha-or-color-theme)
     (random-color alpha-or-color-theme 255.0)
     (set-alpha (random-color) alpha-or-color-theme)))
  ([] (r/randval 0.2 (rand-nth (named-colors-list))
                 (r/randval 0.5
                            (rand-nth (random-palette))
                            (Vec4. (r/drand 255.0) (r/drand 255.0) (r/drand 255.0) 255.0)))))

(defn fe-color-matrix
  "Create a feColorMatrix operator."
  [[^double r1 ^double r2 ^double r3 ^double r4 ^double r5
    ^double g1 ^double g2 ^double g3 ^double g4 ^double g5
    ^double b1 ^double b2 ^double b3 ^double b4 ^double b5
    ^double a1 ^double a2 ^double a3 ^double a4 ^double a5]]
  (fn [c]
    (let [^Vec4 c (pr/to-color c)]
      (clamp (Vec4. (+ (* r1 (.x c)) (* r2 (.y c)) (* r3 (.z c)) (* r4 (.w c)) r5)
                    (+ (* g1 (.x c)) (* g2 (.y c)) (* g3 (.z c)) (* g4 (.w c)) g5)
                    (+ (* b1 (.x c)) (* b2 (.y c)) (* b3 (.z c)) (* b4 (.w c)) b5)
                    (+ (* a1 (.x c)) (* a2 (.y c)) (* a3 (.z c)) (* a4 (.w c)) a5))))))

(defn sepia
  ([^double amount]
   (let [amount (- 1.0 amount)
         a (+ 0.393 (* 0.607 amount)) b (- 0.769 (* 0.769 amount)) c (- 0.189 (* 0.189 amount))
         d (- 0.349 (* 0.349 amount)) e (+ 0.686 (* 0.314 amount)) f (- 0.168 (* 0.168 amount))
         g (- 0.272 (* 0.272 amount)) h (- 0.534 (* 0.534 amount)) i (+ 0.131 (* 0.869 amount))]
     (fe-color-matrix [a b c 0.0 0.0
                       d e f 0.0 0.0
                       g h i 0.0 0.0
                       0.0 0.0 0.0 1.0 0,0]))))

(defn contrast
  ([^double amount]
   (let [intercept (* 127.5 (- 1.0 amount))]
     (fe-color-matrix [amount 0.0 0.0 0.0 intercept
                       0.0 amount 0.0 0.0 intercept
                       0.0 0.0 amount 0.0 intercept
                       0.0 0.0 0.0 1.0 0.0]))))

(defn exposure
  ([^double amount]
   (fe-color-matrix [amount 0.0 0.0 0.0 0.0
                     0.0 amount 0.0 0.0 0.0
                     0.0 0.0 amount 0.0 0.0
                     0.0 0.0 0.0 1.0 0.0])))

(defn brightness
  ([^double amount]
   (let [amount (* 255.0 amount)]
     (fe-color-matrix [1.0 0.0 0.0 0.0 amount
                       0.0 1.0 0.0 0.0 amount
                       0.0 0.0 1.0 0.0 amount
                       0.0 0.0 0.0 1.0 0.0]))))


(defn saturation
  ([^double amount]
   (let [r+ (+ 0.2126 (* 0.7874 amount))
         r- (- 0.2126 (* 0.2126 amount))
         g+ (+ 0.7152 (* 0.2848 amount))
         g- (- 0.7152 (* 0.7152 amount))
         b+ (+ 0.0722 (* 0.9278 amount))
         b- (- 0.0722 (* 0.0722 amount))]
     (fe-color-matrix [r+ g- b- 0.0 0.0
                       r- g+ b- 0.0 0.0
                       r- g- b+ 0.0 0.0
                       0.0 0.0 0.0 1.0 0.0]))))

(defn hue-rotate
  ([^double angle-degrees]
   (let [a (m/radians angle-degrees)
         sa (m/sin a)
         ca (m/cos a)
         cr+ (+ 0.213 (* 0.787 ca))
         cr- (+ 0.213 (* -0.213 ca))
         cg+ (+ 0.715 (* 0.285 ca))
         cg- (+ 0.715 (* -0.715 ca))
         cb+ (+ 0.072 (* 0.928 ca))
         cb- (+ 0.072 (* -0.072 ca))]
     (fe-color-matrix [(+ cr+ (* -0.213 sa)) (+ cg- (* -0.715 sa)) (+ cb- (* 0.928 sa)) 0.0 0.0
                       (+ cr- (* 0.143 sa)) (+ cg+ (* 0.140 sa)) (+ cb- (* -0.283 sa)) 0.0 0.0
                       (+ cr- (* -0.787 sa)) (+ cg- (* 0.715 sa)) (+ cb+ (* 0.071 sa)) 0.0 0.0
                       0.0 0.0 0.0 1.0 0.0]))))

(defn grayscale
  ([^double amount] (saturation (- 1.0 amount))))

;; Color Vision Deficiency
;; https://github.com/chromelens/chromelens/blob/master/lenses/filters/

(def achromatomaly (fe-color-matrix [0.618,0.320,0.062,0.0,0.0,
                                   0.163,0.775,0.062,0.0,0.0,
                                   0.163,0.320,0.516,0.0,0.0,
                                   0.0,0.0,0.0,1.0,0.0]))
(def achromatopsia (fe-color-matrix [0.299,0.587,0.114,0.0,0.0,
                                   0.299,0.587,0.114,0.0,0.0,
                                   0.299,0.587,0.114,0.0,0.0,
                                   0.0,0.0,0.0,1.0,0.0]))
(def deuteranomaly (fe-color-matrix [0.8,0.2,0.0,0.0,0.0,
                                   0.258,0.742,0.0,0.0,0.0,
                                   0.0,0.142,0.858,0.0,0.0,
                                   0.0,0.0,0.0,1.0,0.0]))
(def deuteranopia  (fe-color-matrix [0.625,0.375,0.0,0.0,0.0,
                                   0.7,0.3,0.0,0.0,0.0,
                                   0.0,0.3,0.7,0.0,0.0,
                                   0.0,0.0,0.0,1.0,0.0]))
(def protanomaly   (fe-color-matrix [0.817,0.183,0.0,0.0,0.0,
                                   0.333,0.667,0.0,0.0,0.0,
                                   0.0,0.125,0.875,0.0,0.0,
                                   0.0,0.0,0.0,1.0,0.0]))
(def protanopia    (fe-color-matrix [0.567,0.433,0.0,0.0,0.0,
                                   0.558,0.442,0.0,0.0,0.0,
                                   0.0,0.242,0.758,0.0,0.0,
                                   0.0,0.0,0.0,1.0,0.0]))
(def tritanomaly   (fe-color-matrix [0.967,0.033,0.0,0.0,0.0,
                                   0.0,0.733,0.267,0.0,0.0,
                                   0.0,0.183,0.817,0.0,0.0,
                                   0.0,0.0,0.0,1.0,0.0]))
(def tritanopia    (fe-color-matrix [0.95,0.05,0.0,0.0,0.0,
                                   0.0,0.433,0.567,0.0,0.0,
                                   0.0,0.475,0.525,0.0,0.0,
                                   0.0,0.0,0.0,1.0,0.0]))

;; some utils

(defn- make-line
  [n l]
  (apply list '+
         (remove nil? (map (fn [^double a p]
                             (cond
                               (zero? a) nil
                               (== 1.0 a) (list p n)
                               :else (list '* a (list p n)))) l '[.x .y .z]))))

(defn- matrix-inverse
  ([matrix] (matrix-inverse 'c matrix))
  ([n matrix]
   (let [m (->> matrix
                m/seq->double-double-array
                org.apache.commons.math3.linear.Array2DRowRealMatrix.
                org.apache.commons.math3.linear.MatrixUtils/inverse
                .getData)]
     (for [l m]
       (make-line n l)))))

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
      (when (or (m/invalid-double? (.x res))
                (m/invalid-double? (.y res))
                (m/invalid-double? (.z res)))
        (println "Warning: invalid numbers in " res " for " [r g b]))
      (if (< cc 0x1000000)
        (recur (inc cc) (double nmnr) (double nmxr) (double nmng) (double nmxg) (double nmnb) (double nmxb))
        {:r [nmnr nmxr] :g [nmng nmxg] :b [nmnb nmxb]}))))

(defn- test-reversibility
  ([colorspace]
   (let [[to from] (colorspaces colorspace)]
     (test-reversibility to from)))
  ([to from]
   (for [r (range 256)
         g (range 256)
         b (range 256)
         :let [in (Vec4. r g b 255.0)
               cs (to in)
               rgb (from cs)]
         :when (not= in (lclamp rgb))]
     [r g b cs (lclamp rgb) (v/dist in rgb)])))

(m/unuse-primitive-operators)

(comment (run! (fn [[k [to from]]]
                 (println "---")
                 (println k)
                 (let [f (time (doall (test-reversibility to from)))]
                   (println (take 3 f))))
               (remove (comp #{:Gray} first) colorspaces))
         
         
         )
