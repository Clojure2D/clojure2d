(ns clojure2d.color.cssgram
  "Reimplementation of selection of instagram color filters.

  * Webpage: https://una.im/CSSgram/
  * Info: https://alistapart.com/article/finessing-fecolormatrix/
  * Source: https://github.com/una/CSSgram
  * Licence: MIT

  Additionally there is an possibility to create custom color filter with [[custom-filter]]."
  (:require [clojure2d.color :as c]
            [clojure2d.color.blend :as cb]))

(defonce ^:private c075 (c/contrast 0.75))
(defonce ^:private c085 (c/contrast 0.85))
(defonce ^:private c09 (c/contrast 0.9))
(defonce ^:private c095 (c/contrast 0.95))
(defonce ^:private c108 (c/contrast 1.08))
(defonce ^:private c11 (c/contrast 1.1))
(defonce ^:private c12 (c/contrast 1.2))
(defonce ^:private c15 (c/contrast 1.5))

(defonce ^:private e09 (c/exposure 0.9))
(defonce ^:private e095 (c/exposure 0.95))
(defonce ^:private e105 (c/exposure 1.05))
(defonce ^:private e108 (c/exposure 1.08))
(defonce ^:private e11 (c/exposure 1.1))
(defonce ^:private e115 (c/exposure 1.15))
(defonce ^:private e12 (c/exposure 1.2))

(defonce ^:private s066 (c/saturation 0.66))
(defonce ^:private s075 (c/saturation 0.75))
(defonce ^:private s085 (c/saturation 0.85))
(defonce ^:private s09 (c/saturation 0.9))
(defonce ^:private s11 (c/saturation 1.1))
(defonce ^:private s12 (c/saturation 1.2))
(defonce ^:private s13 (c/saturation 1.3))
(defonce ^:private s135 (c/saturation 1.35))
(defonce ^:private s15 (c/saturation 1.5))
(defonce ^:private s16 (c/saturation 1.6))

(defonce ^:private sp008 (c/sepia 0.08))
(defonce ^:private sp02 (c/sepia 0.2))
(defonce ^:private sp022 (c/sepia 0.22))
(defonce ^:private sp025 (c/sepia 0.25))
(defonce ^:private sp03 (c/sepia 0.3))

(defonce ^:private g1 (c/grayscale 1.0))
(defonce ^:private g05 (c/grayscale 0.5))

(defonce ^:private hr-20 (c/hue-rotate -20.0))
(defonce ^:private hr-10 (c/hue-rotate -10.0))

(defonce ^:private c-1977 (c/color 243.0 106.0 188.0 (* 0.3 255.0)))

(defn y1977
  [c]
  (-> (cb/blend-colors cb/screen c c-1977) c11 e11 s13))

(defonce ^:private c-aden (c/color 66.0 10.0 14.0 (* 0.2 255.0)))

(defn aden
  [c]
  (-> (cb/blend-colors cb/darken c c-aden) hr-20 c09 s085 e12))

(defonce ^:private c-brooklyn (c/mix (c/color 168.0 223.0 193.0 (* 0.4 255.0)) "#c4b7c8" 0.3))

(defn brooklyn
  [c]
  (-> (cb/blend-colors cb/overlay c c-brooklyn) c09 e11))

(defonce ^:private c-clarendon (c/color 127.0 187.0 227.0 (* 0.2 255.0)))

(defn clarendon
  [c]
  (-> (cb/blend-colors cb/overlay c c-clarendon) c12 s135))

(defonce ^:private c-earlybird (c/set-alpha (c/weighted-average ["#d0ba8e" "#360309" "#1d0210"] [0.2 0.65 0.15]) (* 0.15 255.0)))

(defn earlybird
  [c]
  (-> (cb/blend-colors cb/overlay c c-earlybird) c09 sp02))

(defonce ^:private c-gingham (c/color :lavender))

(defn gingham
  [c]
  (-> (cb/blend-colors cb/softlight c c-gingham) e105 hr-10))

(defonce ^:private c-hudson (c/set-alpha (c/mix "#a6b1ff" "#342134" 0.5) 127.5))

(defn hudson
  [c]
  (-> (cb/blend-colors cb/multiply c c-hudson) e12 c09 s11))

(defn inkwell
  [c]
  (-> c sp03 c11 e11 g1))

(defonce ^:private c-kelvin-1 (c/color "#382c34"))
(defonce ^:private c-kelvin-2 (c/color "#b77d21"))

(defn kelvin
  [c]
  (cb/blend-colors cb/overlay (cb/blend-colors cb/dodge c c-kelvin-1) c-kelvin-2))

(defonce ^:private c-lark-1 (c/color "#22253f"))
(defonce ^:private c-lark-2 (c/gray 248.0 (* 0.8 255.0)))

(defn lark
  [c]
  (-> (cb/blend-colors cb/darken (cb/blend-colors cb/dodge c c-lark-1) c-lark-2) c09))

(defonce ^:private c-lofi (c/gray 0x22 (* 0.3 255.0)))

(defn lofi
  [c]
  (-> (cb/blend-colors cb/multiply c c-lofi) s11 c15))

(defonce ^:private c-maven [3 230 26 (* 0.2 255.0)])

(defn maven
  [c]
  (-> c (cb/hue c-maven) sp025 e095 c095 s15))

(defonce ^:private c-mayfair (-> (c/weighted-average [(c/color :white (* 0.8 255.0)) [255.0 200 200 (* 0.6 255.0)] 0x111111] [0.2 0.2 0.6])
                                 (c/modulate 3 0.4)))

(defn mayfair
  [c]
  (-> (cb/blend-colors cb/overlay c c-mayfair) c11 s11))


(defonce ^:private c-moon-1 (c/gray 0xa0))
(defonce ^:private c-moon-2 (c/gray 0x38))

(defn moon
  [c]
  (-> (cb/blend-colors cb/lighten (cb/blend-colors cb/softlight c c-moon-1) c-moon-2) g1 c11 e11))

(defonce ^:private c-nashville-1 (c/color 247.0 176.0 153.0 (* 0.56 255.0)))
(defonce ^:private c-nashville-2 (c/color 0.0 70.0 150.0 (* 0.4 255.0)))

(defn nashville
  [c]
  (-> (cb/blend-colors cb/lighten (cb/blend-colors cb/darken c c-nashville-1) c-nashville-2)
      sp02 c12 e105 s12))

;; perpetua is skipped: it's only linear gradient from top to bottom, linear-gradient(to bottom, #005b9a, #e6c13d);

(defonce ^:private c-reyes (c/color "#efcdad" 127.5))

(defn reyes
  [c]
  (-> (cb/blend-colors cb/softlight c c-reyes) sp022 e11 c085 s075))

(defonce ^:private c-rise-1 (c/lerp (c/color 236.0 205.0 169.0 (* 0.15 255.0))
                                    (c/color 50.0 30.0 7.0 (* 0.4 255.0)) 0.2))
(defonce ^:private c-rise-2 (c/color 232.0 197.0 152.0 (* 0.8 0.6 255.0)))

(defn rise
  [c]
  (-> (cb/blend-colors cb/overlay (cb/blend-colors cb/multiply c c-rise-1) c-rise-2)
      e105 sp02 c09 s09))

(defonce ^:private c-slumber-1 (c/color 69.0 41.0 12.0 (* 0.4 255.0)))
(defonce ^:private c-slumber-2 (c/color 125.0 105.0 24.0 (* 0.5 255.0)))

(defn slumber
  [c]
  (-> (cb/blend-colors cb/softlight (cb/blend-colors cb/lighten c c-slumber-1) c-slumber-2)
      s066 e105))

(defonce ^:private c-stinson (c/color 240.0 149.0 128.0 (* 0.2 255.0)))

(defn stinson
  [c]
  (-> (cb/blend-colors cb/softlight c c-stinson) c075 s085 e115))

(defonce ^:private c-toaster (c/lerp "#804e0f" "#3b003b" 0.4))

(defn toaster
  [c]
  (-> (cb/blend-colors cb/screen c c-toaster) c15 e09))

(defonce ^:private c-valencia (c/color "#3a0339" 127.5))

(defn valencia
  [c]
  (-> (cb/blend-colors cb/exclusion c c-valencia) c108 e108 sp008))

(defonce ^:private c-walden (c/color "#0044cc" (* 0.3 255.0)))

(defn walden
  [c]
  (-> (cb/blend-colors cb/screen c c-walden) e11 hr-10 sp03 s16))

(defonce ^:private c-willow (c/color 212.0 169.0 175.0))

(defn willow
  [c]
  (-> (cb/blend-colors cb/overlay c c-willow) g05 c085 e09))

(defonce ^:private c-x-pro2 (c/lerp "#e6e7e0" (c/color 43.0 42.0 161.0 (* 0.6 255.0)) 0.3))

(defn x-pro2
  [c]
  (-> (cb/blend-colors cb/burn c c-x-pro2) sp03))

(keys (ns-publics *ns*))
;; => (maven nashville mayfair hudson y1977 valencia slumber brooklyn moon lofi reyes stinson earlybird toaster gingham aden kelvin willow lark custom-filter inkwell x-pro2 clarendon walden rise)

(defmacro ^:private make-filters-list
  []
  `[~@(map #(vector (keyword %) %) (remove #{'custom-filter 'filters 'filters-list} (keys (ns-publics *ns*))))])

(def filters (into {} (make-filters-list)))

(def filters-list (sort (keys filters)))

;; custom filters

(def ^:private color-operators {:sepia c/sepia
                              :contrast c/contrast
                              :exposure c/exposure
                              :brightness c/brightness
                              :grayscale c/grayscale
                              :hue-rotate c/hue-rotate
                              :saturation c/saturation
                              :matrix c/fe-color-matrix
                              :temperature (fn [temp amount] (fn [c] (c/adjust-temperature c temp amount)))
                              :tint c/tinter})

(def ^:private ch-operators {:hue cb/hue
                           :saturation cb/saturation
                           :luminocity cb/luminocity
                           :color cb/color
                           :ch0 cb/ch0
                           :ch1 cb/ch1
                           :ch2 cb/ch2})

(defn- find-blend
  [[op c2 c3]]
  (if-let [opf (ch-operators op)]
    (if c3
      (fn [c] (opf c2 c c3))
      (fn [c] (opf c c2)))
    (let [opf (get cb/blends op (if (fn? op) op cb/normal))]
      (fn [c] (cb/blend-colors opf c c2)))))

(defn custom-filter
  "Create a custom filter out of list of step definitions. A filter is a function wich returns new color for a given one.

  Each step can be one of:

  * a function which takes color and returs new color
  * a vector with an operator name as a keyword and arguments
      - one of the: `:sepia`, `:contrast`, `:exposure`, `:brightness`, `:grayscale`, `:hue-rotate`, `:saturation`, `:tint` - with an amount as the argument (see below)
      - `:temperature` with temperature in Kelvins and amount (0.0-1.0)
      - `:matrix` - feColorMatrix definition, [see here](https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feColorMatrix)
      - `:blend` for blending two colors, parameters are blednding operator and a color (see below)

  Bleding operators are defined in `clojure2d.color.blend/blends-list` plus `:hue`, `:saturation`, `:luminocity` and `:color` which copies one/two channels between colors in HSB color space. [see here](https://www.w3.org/TR/compositing-1/#blending).

  Note: CSS `brightness` is defined as `:exposure` which multiplies channels by given amount. `:brightness` changes channels by adding a percentage of the original value.

  Example: to construct [[moon]] filter, our definition will look like below:

  `(custom-filter [:blend :softlight (c/gray 0xa0)] [:blend :lighten (c/gray 0x38)]
                [:grayscale 1.0] [:contrast 1.1] [:exposure 1.1])`"
  [& defs]
  (reduce (fn [f opdef]
            (if (fn? opdef)
              opdef
              (let [[op & args] opdef
                    opf (if (= op :blend)
                          (find-blend args)
                          (if-let [cof (get color-operators op)]
                            (apply cof args)
                            (if (fn? op) op identity)))]
                (comp opf f)))) identity defs))
