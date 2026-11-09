(ns clojure2d.color.cssgram
  "Instagram-like color filters, a per-color reimplementation of CSSgram.

  CSSgram builds each filter from up to two overlay layers (`::before`, then `::after`), each with a color, a CSS `mix-blend-mode` and an opacity, followed by a CSS `filter` list applied to the blended result. Here every filter is a function from a color to a color, defined as data in [[filter-definitions]] and built with [[custom-filter]]:

  - overlays are blended in stacking order with `clojure2d.color.blend` modes; the W3C modes match CSS: `soft-light` is `w3c-softlight`, `hue` is `w3c-hue`, `color` is `w3c-color`, `color-dodge` is `dodge`, `color-burn` is `burn`
  - CSS opacity is folded into the overlay color's alpha
  - filter steps map CSS `brightness` to `:exposure` and `saturate` to `:saturation`

  A per-color filter has no position, so CSS gradients are approximated by a single color (a mix or weighted average of the stops): aden, brooklyn, earlybird, hudson, lofi, mayfair, perpetua, rise, toaster and x-pro2. The other filters follow the CSSgram source exactly.

  Filters: [[y1977]], [[aden]], [[brannan]], [[brooklyn]], [[clarendon]], [[earlybird]], [[gingham]], [[hudson]], [[inkwell]], [[kelvin]], [[lark]], [[lofi]], [[maven]], [[mayfair]], [[moon]], [[nashville]], [[perpetua]], [[reyes]], [[rise]], [[slumber]], [[stinson]], [[toaster]], [[valencia]], [[walden]], [[willow]], [[x-pro2]]; by name in [[filters]] and [[filters-list]].

  * Webpage: https://una.im/CSSgram/
  * Info: https://alistapart.com/article/finessing-fecolormatrix/
  * Source: https://github.com/una/CSSgram
  * Licence: MIT"
  (:require [clojure2d.color :as c]
            [clojure2d.color.blend :as cb]))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

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

(def ^:private whole-color-blends {:hue cb/hue
                                   :saturation cb/saturation
                                   :luminosity cb/luminosity
                                   :color cb/color
                                   :w3c-hue cb/w3c-hue
                                   :w3c-saturation cb/w3c-saturation
                                   :w3c-color cb/w3c-color
                                   :w3c-luminosity cb/w3c-luminosity
                                   :darker-color cb/darker-color
                                   :lighter-color cb/lighter-color
                                   :ch0 cb/ch0
                                   :ch1 cb/ch1
                                   :ch2 cb/ch2})

(defn- blend-step
  "Filter step blending the filtered color (backdrop) with an overlay color (source)."
  [[mode c2 c3]]
  (if-let [f (whole-color-blends mode)]
    (if c3
      (fn [c] (f c2 c c3))
      (fn [c] (f c c2)))
    (let [f (cb/blend-fn mode)]
      (fn [c] (cb/blend-colors f c c2)))))

(defn- operator-step
  "Filter step for a `[operator & args]` definition."
  [[op & args]]
  (cond
    (= op :blend) (blend-step args)
    (contains? color-operators op) (apply (color-operators op) args)
    (fn? op) op
    :else (throw (ex-info (str "Unknown filter operator: " op) {:operator op}))))

(defn custom-filter
  "Creates a color filter from step definitions, applied left to right.

  A filter is a function from a color to a color. Each step is one of:

  - a function from a color to a color
  - `[:sepia amount]`, `[:contrast amount]`, `[:exposure amount]`, `[:brightness amount]`, `[:grayscale amount]`, `[:hue-rotate degrees]`, `[:saturation amount]`, `[:tint color]`: color operations of `clojure2d.color`
  - `[:temperature kelvins amount]`: shifts towards the color of a light source, amount in `[0,1]`
  - `[:matrix m]`: an SVG `feColorMatrix` definition, [see here](https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feColorMatrix)
  - `[:blend mode color]`: blends the filtered color (backdrop) with `color` (source, its alpha acts as the CSS opacity); `mode` is a keyword of `clojure2d.color.blend/blends` or a per-channel function, or a whole-color mode `:hue`, `:saturation`, `:luminosity`, `:color` (HSB), `:w3c-hue`, `:w3c-saturation`, `:w3c-color`, `:w3c-luminosity`, `:darker-color`, `:lighter-color`
  - `[:blend :ch0 colorspace color]` (also `:ch1`, `:ch2`): takes that channel of `color` in `colorspace`; without `colorspace` RGB is used

  CSS `brightness` corresponds to `:exposure`, which multiplies the channels; `:brightness` adds a percentage of the original value.

  Returns the filter function. Throws `ex-info` with `{:operator op}` for an unknown operator and with `{:blend mode}` for an unknown blend mode.

  Example, the [[moon]] filter: `(custom-filter [:blend :w3c-softlight (c/gray 160)] [:blend :lighten (c/gray 56)] [:grayscale 1.0] [:contrast 1.1] [:exposure 1.1])`.

  See also [[filter-definitions]]."
  [& defs]
  (reduce (fn [f step]
            (comp (if (fn? step) step (operator-step step)) f)) identity defs))

;; filters as data

(defn- css-color
  "CSS `rgba` color with an extra CSS opacity folded into the alpha."
  ([r g b] (c/color r g b))
  ([r g b ^double alpha] (c/color r g b (* 255.0 alpha))))

(def ^{:doc "CSSgram filters as data: filter name to a map with:

  - `:overlays` blend steps in stacking order (`::before`, then `::after`), each `[mode color]` with the CSS opacity in the color's alpha
  - `:filters` CSS filter list as [[custom-filter]] steps
  - `:approximation` (optional) why the filter is not exact, e.g. a gradient replaced by one color

  Each public filter function is `(apply custom-filter (concat overlays filters))` of its entry."}
  filter-definitions
  {:y1977 {:overlays [[:screen (css-color 243 106 188 0.3)]]
           :filters [[:contrast 1.1] [:exposure 1.1] [:saturation 1.3]]}
   :aden {:overlays [[:darken (css-color 66 10 14 0.2)]]
          :filters [[:hue-rotate -20.0] [:contrast 0.9] [:saturation 0.85] [:exposure 1.2]]
          :approximation "linear gradient to transparent replaced by its first stop"}
   :brannan {:overlays [[:lighten (css-color 161 44 199 0.31)]]
             :filters [[:sepia 0.5] [:contrast 1.4]]}
   :brooklyn {:overlays [[:overlay (c/mix (css-color 168 223 193 0.4) "#c4b7c8" 0.3)]]
              :filters [[:contrast 0.9] [:exposure 1.1]]
              :approximation "radial gradient replaced by a 70/30 mix of the stops"}
   :clarendon {:overlays [[:overlay (css-color 127 187 227 0.2)]]
               :filters [[:contrast 1.2] [:saturation 1.35]]}
   :earlybird {:overlays [[:overlay (c/set-alpha (c/weighted-average ["#d0ba8e" "#360309" "#1d0210"] [0.2 0.65 0.15]) (* 0.15 255.0))]]
               :filters [[:contrast 0.9] [:sepia 0.2]]
               :approximation "opaque radial gradient replaced by a weighted average of the stops with alpha 0.15"}
   :gingham {:overlays [[:w3c-softlight (css-color 230 230 250)]]
             :filters [[:exposure 1.05] [:hue-rotate -10.0]]}
   :hudson {:overlays [[:multiply (c/set-alpha (c/mix "#a6b1ff" "#342134" 0.5) 127.5)]]
            :filters [[:exposure 1.2] [:contrast 0.9] [:saturation 1.1]]
            :approximation "radial gradient replaced by a 50/50 mix of the stops"}
   :inkwell {:overlays []
             :filters [[:sepia 0.3] [:contrast 1.1] [:exposure 1.1] [:grayscale 1.0]]}
   :kelvin {:overlays [[:dodge (css-color 56 44 52)] [:overlay (css-color 183 125 33)]]
            :filters []}
   :lark {:overlays [[:dodge (css-color 34 37 63)] [:darken (css-color 242 242 242 0.8)]]
          :filters [[:contrast 0.9]]}
   :lofi {:overlays [[:multiply (c/gray 0x22 (* 0.3 255.0))]]
          :filters [[:saturation 1.1] [:contrast 1.5]]
          :approximation "radial gradient from transparent replaced by its last stop with alpha 0.3"}
   :maven {:overlays [[:w3c-hue (css-color 3 230 26 0.2)]]
           :filters [[:sepia 0.25] [:exposure 0.95] [:contrast 0.95] [:saturation 1.5]]}
   :mayfair {:overlays [[:overlay (-> (c/weighted-average [(c/color :white (* 0.8 255.0)) [255.0 200 200 (* 0.6 255.0)] 0x111111] [0.2 0.2 0.6])
                                      (c/modulate 3 0.4))]]
             :filters [[:contrast 1.1] [:saturation 1.1]]
             :approximation "radial gradient replaced by a weighted average of the stops with opacity 0.4"}
   :moon {:overlays [[:w3c-softlight (css-color 160 160 160)] [:lighten (css-color 56 56 56)]]
          :filters [[:grayscale 1.0] [:contrast 1.1] [:exposure 1.1]]}
   :nashville {:overlays [[:darken (css-color 247 176 153 0.56)] [:lighten (css-color 0 70 150 0.4)]]
               :filters [[:sepia 0.2] [:contrast 1.2] [:exposure 1.05] [:saturation 1.2]]}
   :perpetua {:overlays [[:w3c-softlight (c/set-alpha (c/lerp "#005b9a" "#e6c13d" 0.5) 127.5)]]
              :filters []
              :approximation "linear gradient replaced by a 50/50 mix of the stops"}
   :reyes {:overlays [[:w3c-softlight (css-color 239 205 173 0.5)]]
           :filters [[:sepia 0.22] [:exposure 1.1] [:contrast 0.85] [:saturation 0.75]]}
   :rise {:overlays [[:multiply (c/lerp (c/color 236.0 205.0 169.0 (* 0.15 255.0)) (c/color 50.0 30.0 7.0 (* 0.4 255.0)) 0.2)]
                     [:overlay (css-color 232 197 152 (* 0.8 0.6))]]
          :filters [[:exposure 1.05] [:sepia 0.2] [:contrast 0.9] [:saturation 0.9]]
          :approximation "radial gradients replaced by a mix of the stops and the first stop"}
   :slumber {:overlays [[:lighten (css-color 69 41 12 0.4)] [:w3c-softlight (css-color 125 105 24 0.5)]]
             :filters [[:saturation 0.66] [:exposure 1.05]]}
   :stinson {:overlays [[:w3c-softlight (css-color 240 149 128 0.2)]]
             :filters [[:contrast 0.75] [:saturation 0.85] [:exposure 1.15]]}
   :toaster {:overlays [[:screen (c/lerp "#804e0f" "#3b003b" 0.4)]]
             :filters [[:contrast 1.5] [:exposure 0.9]]
             :approximation "radial gradient replaced by a 60/40 mix of the stops"}
   :valencia {:overlays [[:exclusion (css-color 58 3 57 0.5)]]
              :filters [[:contrast 1.08] [:exposure 1.08] [:sepia 0.08]]}
   :walden {:overlays [[:screen (css-color 0 68 204 0.3)]]
            :filters [[:exposure 1.1] [:hue-rotate -10.0] [:sepia 0.3] [:saturation 1.6]]}
   :willow {:overlays [[:w3c-color (css-color 216 205 203)]]
            :filters [[:grayscale 0.5] [:contrast 0.95] [:exposure 0.9]]}
   :x-pro2 {:overlays [[:burn (c/lerp "#e6e7e0" (c/color 43.0 42.0 161.0 (* 0.6 255.0)) 0.3)]]
            :filters [[:sepia 0.3]]
            :approximation "radial gradient replaced by a 70/30 mix of the stops"}})

(defn- definition->filter
  [{:keys [overlays filters]}]
  (apply custom-filter (concat (map (partial into [:blend]) overlays) filters)))

(def ^{:doc "CSSgram filters by name: keyword of [[filter-definitions]] to the filter function."}
  filters (into {} (map (fn [[k d]] [k (definition->filter d)])) filter-definitions))

(def ^{:doc "Sorted list of the filter names (keywords of [[filters]])."}
  filters-list (sort (keys filters)))

(def ^{:doc "CSSgram 1977: pink screen overlay, then contrast 1.1, brightness 1.1, saturate 1.3.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:y1977` in [[filter-definitions]]."}
  y1977 (filters :y1977))
(def ^{:doc "CSSgram Aden: dark red darken overlay, then hue-rotate -20, contrast 0.9, saturate 0.85, brightness 1.2. The gradient overlay is approximated by one color.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:aden` in [[filter-definitions]]."}
  aden (filters :aden))
(def ^{:doc "CSSgram Brannan: sepia 0.5, contrast 1.4 after a purple lighten overlay.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:brannan` in [[filter-definitions]]."}
  brannan (filters :brannan))
(def ^{:doc "CSSgram Brooklyn: green-gray overlay, then contrast 0.9, brightness 1.1. The gradient overlay is approximated by one color.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:brooklyn` in [[filter-definitions]]."}
  brooklyn (filters :brooklyn))
(def ^{:doc "CSSgram Clarendon: light blue overlay, then contrast 1.2, saturate 1.35.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:clarendon` in [[filter-definitions]]."}
  clarendon (filters :clarendon))
(def ^{:doc "CSSgram Earlybird: warm overlay, then contrast 0.9, sepia 0.2. Approximation: the opaque gradient overlay is replaced by its weighted average with alpha 0.15.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:earlybird` in [[filter-definitions]]."}
  earlybird (filters :earlybird))
(def ^{:doc "CSSgram Gingham: lavender soft light (W3C) overlay, then brightness 1.05, hue-rotate -10.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:gingham` in [[filter-definitions]]."}
  gingham (filters :gingham))
(def ^{:doc "CSSgram Hudson: blue-purple multiply overlay at opacity 0.5, then brightness 1.2, contrast 0.9, saturate 1.1. The gradient overlay is approximated by one color.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:hudson` in [[filter-definitions]]."}
  hudson (filters :hudson))
(def ^{:doc "CSSgram Inkwell: sepia 0.3, contrast 1.1, brightness 1.1, grayscale; no overlay.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:inkwell` in [[filter-definitions]]."}
  inkwell (filters :inkwell))
(def ^{:doc "CSSgram Kelvin: dark color dodge overlay, then orange overlay; no filter list.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:kelvin` in [[filter-definitions]]."}
  kelvin (filters :kelvin))
(def ^{:doc "CSSgram Lark: dark blue color dodge overlay, light gray darken overlay, then contrast 0.9.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:lark` in [[filter-definitions]]."}
  lark (filters :lark))
(def ^{:doc "CSSgram Lo-Fi: dark gray multiply overlay, then saturate 1.1, contrast 1.5. The vignette gradient is approximated by one color.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:lofi` in [[filter-definitions]]."}
  lofi (filters :lofi))
(def ^{:doc "CSSgram Maven: green hue (W3C) overlay, then sepia 0.25, brightness 0.95, contrast 0.95, saturate 1.5.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:maven` in [[filter-definitions]]."}
  maven (filters :maven))
(def ^{:doc "CSSgram Mayfair: pinkish overlay at opacity 0.4, then contrast 1.1, saturate 1.1. The gradient overlay is approximated by one color.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:mayfair` in [[filter-definitions]]."}
  mayfair (filters :mayfair))
(def ^{:doc "CSSgram Moon: gray soft light (W3C) and dark gray lighten overlays, then grayscale, contrast 1.1, brightness 1.1.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:moon` in [[filter-definitions]]."}
  moon (filters :moon))
(def ^{:doc "CSSgram Nashville: peach darken and blue lighten overlays, then sepia 0.2, contrast 1.2, brightness 1.05, saturate 1.2.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:nashville` in [[filter-definitions]]."}
  nashville (filters :nashville))
(def ^{:doc "CSSgram Perpetua: blue-to-yellow soft light (W3C) overlay at opacity 0.5; no filter list. The gradient overlay is approximated by the mix of its stops.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:perpetua` in [[filter-definitions]]."}
  perpetua (filters :perpetua))
(def ^{:doc "CSSgram Reyes: beige soft light (W3C) overlay at opacity 0.5, then sepia 0.22, brightness 1.1, contrast 0.85, saturate 0.75.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:reyes` in [[filter-definitions]]."}
  reyes (filters :reyes))
(def ^{:doc "CSSgram Rise: warm multiply and overlay overlays, then brightness 1.05, sepia 0.2, contrast 0.9, saturate 0.9. The gradient overlays are approximated by one color each.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:rise` in [[filter-definitions]]."}
  rise (filters :rise))
(def ^{:doc "CSSgram Slumber: brown lighten and olive soft light (W3C) overlays, then saturate 0.66, brightness 1.05.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:slumber` in [[filter-definitions]]."}
  slumber (filters :slumber))
(def ^{:doc "CSSgram Stinson: salmon soft light (W3C) overlay, then contrast 0.75, saturate 0.85, brightness 1.15.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:stinson` in [[filter-definitions]]."}
  stinson (filters :stinson))
(def ^{:doc "CSSgram Toaster: brown-purple screen overlay, then contrast 1.5, brightness 0.9. The gradient overlay is approximated by one color.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:toaster` in [[filter-definitions]]."}
  toaster (filters :toaster))
(def ^{:doc "CSSgram Valencia: dark purple exclusion overlay at opacity 0.5, then contrast 1.08, brightness 1.08, sepia 0.08.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:valencia` in [[filter-definitions]]."}
  valencia (filters :valencia))
(def ^{:doc "CSSgram Walden: blue screen overlay at opacity 0.3, then brightness 1.1, hue-rotate -10, sepia 0.3, saturate 1.6.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:walden` in [[filter-definitions]]."}
  walden (filters :walden))
(def ^{:doc "CSSgram Willow: pinkish gray color (W3C) overlay, then grayscale 0.5, contrast 0.95, brightness 0.9. The CSS `::before` overlay is invalid CSS (ignored by browsers) and is left out.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:willow` in [[filter-definitions]]."}
  willow (filters :willow))
(def ^{:doc "CSSgram X-Pro II: light color burn overlay, then sepia 0.3. The gradient overlay is approximated by one color.

  - `c` color, anything accepted by `clojure2d.color/to-color`

  Returns a `Vec4`. Definition: `:x-pro2` in [[filter-definitions]]."}
  x-pro2 (filters :x-pro2))
