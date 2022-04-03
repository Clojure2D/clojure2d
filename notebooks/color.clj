^{:nextjournal.clerk/visibility :hide-ns
  :nextjournal.clerk/toc :pin}
(ns notebooks.color
  (:require
   [clerk.styles :refer [color-styles 🎨]]
   [clojure2d.color :as c]
   [clojure2d.color.blend :as bl]
   [clojure2d.color.cssgram :as cssgram]
   [clojure2d.core :as c2d]
   [clojure2d.extra.utils :as utils]
   [clojure2d.pixels :as pixels]
   [fastmath.core :as m]
   [fastmath.random :as r]
   [fastmath.vector :as v]
   [nextjournal.clerk :as clerk]
   [nextjournal.clerk.viewer :as viewer]))

^{::clerk/viewer :html ::clerk/visibility :hide ::clerk/no-cache true} color-styles
^{::clerk/visibility :hide
  ::clerk/viewer :hide-result}
(clerk/set-viewers!
 (conj
  (mapv #(cond-> % (contains? % :fetch-opts) (assoc-in [:fetch-opts :n] 500)) viewer/default-viewers)
  {:name :block
   :render-fn '#(v/html (into [:div.flex.flex-col] (v/inspect-children %2) %1))}))

;; # clojure2d.color namespace

;; `clojure2d.color` namespace for Clojure is a rich collection of functions for various color, palettes and grandents manipulations. Including:
;;
;; * various color representations
;; * color spaces coversions
;; * palettes and gradients
;; * distances
;; * blending

;; Note: Only CIE standard illuminant D65, 2° observer is supported

;; I use `🎨` internal function to render given color, palette or gradient.

;; # Intro

;; There are three entities we will deal with:

;; * `color` - representation of the single color with 3 color information channels and additional alpha channel (4 in total).

(🎨 :darkcyan)
(🎨 (c/color :cyan))

;; * `palette` - vector of colors

(🎨 [:docc/dark-tyrian-blue :blue [200 200 255] "#f" (c/set-alpha "#a" 100)])
(🎨 (c/palette [:green :lime :yellow]))

;; * `gradient` - continuous interpolation between colors, returning a color for given value from `0.0` to `1.0`

(def yb-gradient (c/gradient [:yellow :blue]))
(yb-gradient 0.3)
(🎨 yb-gradient)

;; # Color

;; At the beginning let's start with a `color`. A color is always a vector containing 3 channels from given color space plus one additional to describe opacity, ie. alpha channel.
;;
;; By default Java and WWW apps interpret a color as a *sRGB*. In `clojure2d.color` value of each channel in *sRGB* color space is from `[0.0-255.0]` range. Other color spaces defined here can operate on different ranges. More about this later.

;; ## Color representation

;; Internally a color is represented as `fastmath.vector.Vec4` type but we can use any other representation which eventually is coerced to a `Vec4`. Let's look at all of the possible options. 

;; ### Named color

;; The quickest option is to use a keyword as a color name, there are two groups of names.
;;
;; The full list of names can be found [here](https://clojure2d.github.io/clojure2d/docs/static/colors.html) or retrieved by calling:

(sort (c/named-colors-list))

;; #### HTML color names

;; All basic and extended colors used by modern browsers as a lower-case keyword. Note that some colors can have two similar names. For example `:darkblue` and `:dark-blue` represent the same color.

(🎨 :darkblue :dark-blue)

;; #### Dictionary of Colour Combinations

;; This is the list of colors compiled in the book ["A Dictionary of Colour Combinations"](https://en.seigensha.com/books/978-4-86152-247-5/) by Sanzo Wada, digitalized by [Dain M. Blodorn Kim](https://github.com/dblodorn/sanzo-wada/) and eventually corrected and published by [Matt DesLauriers](https://github.com/mattdesl/dictionary-of-colour-combinations).
;;
;; Colors from this resource are prefixed by a `:docc/` namespace.

(🎨 :docc/burnt-sienna :docc/peacock-blue :docc/light-pinkish-cinnamon)

;; ### CSS string

;; The other representation is based on CSS. A hexadecimal string in one of the following forms: `#X`, `#XX`, `#RGB`, `#RRGGBB` or `#RRGGBBAA`. Where `X` (`[0-F]`) or `XX` (`[00-FF]`) are the grey values. The hash character is optional, lower case also can be used.

(🎨 "#7" "#4a" "#FA3" "#FFAA33AA" "f1a130")

;; To convert any color to CSS representation call:

(clerk/table
 (map (juxt identity c/format-hex) [:red 0xaa112299 [123 44 55]
                                    java.awt.Color/YELLOW
                                    "ab"]))

;; ### Integer

;; Any integer (32bit number) is interpreted as a color, where 8 most significant bits represent alpha channel, next 8 bits, first channel, and so on: `0xAARRGGBB`

(🎨 0x91aa0000 ; with alpha set to 0x91
    255
    -256)

;; Note that 0x00rrggbb is the same as 0xffrrggbb, so you can't represent fully transparent color with an integer.

[(c/color 0x123456)
 (c/color 0x00123456)
 (c/color 0xff123456)]

;; conversion to an integer is possible with `pack` function

(c/pack [22 33 44 55])

;; ### Any `Sequable`

;; Any Clojure `Sequable` is coerced to a color with the following rules. Color channel value can be any number type (int or double). Numbers are rounded to the nearest integer and clamped to have [0-255] range.
;;
;; Rules are:
;;
;; * 1-element sequence is treated as a `grey`
;; * 2-element sequence is a `gray` with `alpha`
;; * 3-element sequence is `red`, `green` and `blue`
;; Finally, 4+-element sequence is treated as *RGBA*, any additional sequence elements are ignored

(🎨 [129]
    (list 140.11 255/3)
    (seq (java.util.ArrayList. [240 220 20]))
    [129 190 222 230 1 2 3 4])

;; ### Fastmath vectors

;; Fastmath vectors `Vec2`, `Vec3` and `Vec4` are treated as a `Sequable`

(🎨 (v/vec2 12 200)
    (v/vec3 12 99 200)
    (v/vec4 12 99 122 200))

;; ### AWT Color

;; For interoperability we can use also AWT color (`java.awt.Color` class).

(🎨 (java.awt.Color. 120 33 99)
    (java.awt.Color. 0.9 0.2 0.4)
    (java.awt.Color/PINK))

;; ### Coercions

;; Any color representation is implicitely coerced to a `fastmath.vector.Vec4` type. You can make explicit coercion with `to-color` and `color` function. The latter can construct a color from channel values. Additionally `gray` function returns gray color (with optional alpha)

(class (c/to-color :green))
(map c/to-color [:red 0xffaa33 "fa4" [1 2 3 4]])
(🎨 (c/color :red 200)
    (c/color 12 33 99)
    (c/color 12 33 99 200)
    (c/gray 99))

;; You can also convert to AWT Color, integer and - as previously mentioned - to a CSS Color

^{::clerk/viewer :block}
[(c/awt-color :pink)
 (c/awt-gray 123)
 (c/format-hex :pink)
 (c/pack :pink)]

;; For interop with `quil` just call `c/quil` (which is the same to `pack` and converts a color to a ARGB integer. RGB Color defined by `quil/color` will work in `Clojure2d` without any conversion.

(c/quil :pink)

;; ### All color representations in one place

^{::clerk/visibility :hide}
(clerk/table {:head [:input :output]
              :rows [[:darkgreen (🎨 :darkgreen)]
                     [:docc/green-blue (🎨 :docc/green-blue)]
                     ['(c/color 127) (🎨 (c/color 127))]
                     ['(c/color 127 100) (🎨 (c/color 127 100))]
                     ['(c/color 33 44 55) (🎨 (c/color 33.0 44.0 55.0))]
                     ['(c/color 33 44 55 200) (🎨 (c/color 33.0 44.0 55.0 200.0))]
                     ['(c/gray 100) (🎨 (c/gray 100.0))]
                     ['(c/gray 100 200) (🎨 (c/gray 100.0 200.0))]
                     ["\"#a\"" (🎨 "#a")]
                     ["\"ae\"" (🎨 "ae")]
                     ["\"#3AF\"" (🎨 "#3AF")]
                     ["\"34e9a3\"" (🎨 "34e9a3")]
                     ["\"34A3e999\"" (🎨 "34A3e999")]
                     ["0xa34556" (🎨 0xa34556)]
                     ["0xAAa35645" (🎨 0xAAa35645)]
                     [[127] (🎨 [127])]
                     ['(127 200) (🎨 '(127 200))]
                     ['(double-array [33 44 122]) (🎨 (double-array [33 44 122]))]
                     ['(int-array [33 44 99 122]) (🎨 (int-array [33 44 99 122]))]
                     ['(v/vec2 33 144) (🎨 (v/vec2 33 144))]
                     ['(v/vec3 22 33 144) (🎨 (v/vec3 22 33 144))]
                     ['(v/vec4 11 22 33 144) (🎨 (v/vec4 11 22 33 144))]
                     ['java.awt.Color/PINK (🎨 java.awt.Color/PINK)]
                     ['(c/format-hex :pink) (🎨 (c/format-hex :pink))]
                     ['(c/pack :pink) (🎨 (c/pack :pink))]
                     ['(c/from-HSB [300 0.5 0.5]) (🎨 (c/from-HSB [300 0.5 0.5]))]
                     ['(c/from-LAB [50 50 50]) (🎨 (c/from-LAB [50 50 50]))]]})

;; ### Color validation

;; To check if color is valid or invalid. You can use two functions: `possible-color?` and `valid-color?`. Former function uses some logic to determine if the imput is color or not, Latter one, just tries to coerce to a color and catches exception when it's not (returning `false` in such case).

[(c/possible-color? [11 22 33]) (c/valid-color? [11 22 33])]
[(c/possible-color? [:a]) (c/valid-color? [:a])]

;; There are also tests if color is black or not

(c/black? :black)
(c/not-black? :black)

;; ## Getting and setting channels

;; ### Setting channels

;; To modify given channel of the color use one of the following:
;;
;; * `set-red` or `set-ch0` - to modify first channel
;; * `set-green` or `set-ch1`- to modify second channel
;; * `set-blue` or `set-ch2` - to modify third one
;; * `set-alpha` or call `(color any-color alpha)` - to modify alpha channel
;; * `set-channel` - sets selected channel (optionally for given color space)

(🎨 (c/set-green :red 200)
    (c/set-alpha :red 200)
    (c/color :red 200)
    (c/set-channel :red :HSL 0 300.0))

;; ### Getting channels

;; To access given channel call:

;; * `red` or `ch0` - to get first channel
;; * `green` or `ch1` - to get second channel
;; * `blue` or `ch2` - to get third channel
;; * `alpha` - to get alpha channel
;; * `get-channel` - to get selected channel (optionally for given color space)

(🎨 :docc/fawn)
((juxt c/red c/green c/blue c/alpha) :docc/fawn)
(c/get-channel :red :HSL 2)

;; #### Getting Luma and Hue

;; There is a set of functions returning additional color information, these are:

;; * `luma` - information about luma (brightness) of the color, `[0.0-255.0]`
;; * `relative-luma` - luma from linear RGB, `[0.0-255.0]`
;; * `hue` - hue value, hexagon projection, `[0.0-360.0]`
;; * `hue-polar` - hue value, polar projection, `[0.0-360.0]`,
;; * `hue-paletton` - hue value, as it is used in [paletton](https://paletton.com/), `[0.0-360.0]`

(🎨 :docc/buffy-citrine)
((juxt c/luma c/relative-luma c/hue c/hue-polar c/hue-paletton) :docc/buffy-citrine)

;; #### Clamping

;; To ensure our *sRGB* color is within the range we can use `clamp` which constrains values to a `[0.0-255.0]` range and `lclamp` which also round channel values to the nearest integer.

(🎨 (c/clamp [-23.3 123.033 293.33])
    (c/lclamp [-23.3 123.033 293.33]))

;; ## Color comparison and distances

;; This part describes a number of color comparison and distance/difference functions. Most of them are based mainly on CIE delta-E* definitions:

;; * `delta-E*` - ${\Delta}E^*_{ab}$, CIE76 difference, euclidean distance in LAB color space
;; * `delta-C*` - ${\Delta}C^*_{ab}$, chroma (color) difference in LAB color space, CIE76
;; * `delta-H*` - ${\Delta}H^*_{ab}$, hue difference in LAB color space, CIE76
;; * `delta-E*-94` - ${\Delta}E^*_{94}$, CIE94 difference
;; * `delta-E*-2000` - ${\Delta}E^*_{00}$, CIEDE2000 difference, three optional weights can be provided, `l` for luma, `c` for color intensity and `h` for hue. By default they are set to `1.0`.
;; * `delta-E*-CMC` - ${\Delta}E^*_{CMC}$, CMC l:c 1984 difference, two optional weights can be provided, `l` for luma and `c` for color intensity. By default they are set to `1.0`.
;; * `delta-E-z` - ${\Delta}E_{z}$, difference in JzAzBz color space
;; * `delta-E-HyAB` - ${\Delta}E_{HyAB}$, hybrid difference in LAB color space
;; * `delta-E*-euclidean` - general, euclidean, distance in selected color space (`Oklab` by default)
;; * `delta-C-RGB` - ${\Delta}C$, difference in RGB color space ("redmean")
;; * `delta-D-HCL` - ${\Delta}D_{HCL}$, difference in HCL (Sarifuddin and Missaou) color space

;; Difference between `:yellow` :and `:blue`

^{::clerk/visibility :hide}
(clerk/table
 {:head [:delta :value]
  :rows [['(c/delta-E* :yellow :blue)                      (c/delta-E* :yellow :blue)]
         ['(c/delta-C* :yellow :blue)                      (c/delta-C* :yellow :blue)]
         ['(c/delta-H* :yellow :blue)                      (c/delta-H* :yellow :blue)]
         ['(c/delta-E*-94 :yellow :blue)                   (c/delta-E*-94 :yellow :blue)]
         ['(c/delta-E*-2000 :yellow :blue)                 (c/delta-E*-2000 :yellow :blue)]
         ['(c/delta-E*-2000 :yellow :blue 2.0 1.0 1.0)     (c/delta-E*-2000 :yellow :blue 2.0 1.0 1.0)]
         ['(c/delta-E*-CMC :yellow :blue)                  (c/delta-E*-CMC :yellow :blue)]
         ['(c/delta-E*-CMC :yellow :blue 2.0 1.0)          (c/delta-E*-CMC :yellow :blue 2.0 1.0)]
         ['(c/delta-E-z :yellow :blue)                     (c/delta-E-z :yellow :blue)]
         ['(c/delta-E-HyAB :yellow :blue)                  (c/delta-E-HyAB :yellow :blue)]
         ['(c/delta-E*-euclidean :yellow :blue)            (c/delta-E*-euclidean :yellow :blue)]
         ['(c/delta-E*-euclidean :yellow :blue :YUV)       (c/delta-E*-euclidean :yellow :blue :YUV)]
         ['(c/delta-C-RGB :yellow :blue)                   (c/delta-C-RGB :yellow :blue)]
         ['(c/delta-D-HCL :yellow :blue)                   (c/delta-D-HCL :yellow :blue)]]})

;; There are two more measures: `contrast-ratio` from WCAG (the value below 3.0 means that contrast is too low) and `noticable-different?` to check if colors are visually different, as described in this [paper](https://research.tableau.com/sites/default/files/2014CIC_48_Stone_v3.pdf).

[(c/contrast-ratio :yellow :blue)
 (c/contrast-ratio :yellow :lightyellow)]

;; Let's draw a chart showing how `contrast-ratio` values change when we compare given color to grays.

(defn get-series
  [reference]
  (map (fn [g]
         {:gray g
          :contrast-ratio (c/contrast-ratio reference (c/gray g))
          :reference reference}) (range 256)))

(clerk/vl {:data {:values (concat (get-series :black)
                                  (get-series :white)
                                  (get-series :red)
                                  (get-series :green)
                                  (get-series :blue)
                                  (get-series :yellowgreen))}
           :width 600
           :height 300
           :usermeta {:embedOptions {:renderer "svg"}}
           :mark {:type :line
                  :tooltip {:field :contrast-ratio}}
           :encoding {:x {:field :gray :type :quantitative}
                      :y {:field :contrast-ratio :type :quantitative}
                      :color {:field :reference :type :nominal
                              :scale {:range [:black :blue :green
                                              :red :lightgray :yellowgreen]}}}})

;; To check if two colors are visually different we can call `noticable-different?` function.

[(c/noticable-different? :yellow :blue)
 (c/noticable-different? :yellow :lightyellow)
 (c/noticable-different? (c/gray 245.0) (c/gray 235.0))]

;; ## Color space conversions

;; Color can be converted to and from various of different than `sRGB` color spaces. Full list of possible color spaces is defined under `colorspaces-list` variable.

c/colorspaces-list

;; All color space converters are defined as a functions with names `to-XXX` and `from-XXX`, where `XXX` is the name of the color space. All color spaces a fully reversible, which means that converting to and from given color space yields approximately initial color.

(c/from-YDbDr (c/to-YDbDr [100 200 43]))

;; Due to the fact, that each color space has its own ranges for each channel, there are normalized versions of converters. The names of the functions end with the `*`. Normalization is linear in most cases (exception: `OSA`)

(c/to-LAB :yellow)
(c/to-LAB* :yellow)

;; Normalized color spaces are also reversible:

(c/from-LUV* (c/to-LUV* [100 200 43]))

;; Let's visualize how set of random `sRGB` colors maps to `JCH` and `LAB` color spaces.

(defn get-cs-points
  [cs labels]
  (let [[lx ly lz] labels
        [to] (c/colorspaces cs)
        colors (repeatedly 2000 #(c/color (r/drand 255.0) (r/drand 255.0) (r/drand 255.0)))
        [x y z] (apply map vector (map to colors))
        f #(m/approx % 3)]
    {:data [{:x (map f x) :y (map f y) :z (map f z)
             :type :scatter3d
             :mode :markers
             :marker {:color (map c/format-hex colors)}}]
     :layout {:scene {:xaxis {:title {:text lx}}
                      :yaxis {:title {:text ly}}
                      :zaxis {:title {:text lz}}}
              :height 700
              :width 700}}))

(clerk/plotly (get-cs-points :JCH "JCH"))
(clerk/plotly (get-cs-points :LAB "LAB"))

;; ### Color space ranges

^{::clerk/visibility :hide}
(clerk/table
 {:head ["color space" "channel 1" "channel 2" "channel 3" "comment"]
  :rows (sort-by first [[:CMY [0.0 255.0] [0.0 255.0] [0.0 255.0] ""]
                        [:Cubehelix [0.0 360.0] [0.0 4.61] [0.0 1.0] ""]
                        [:GLHS [0.0 1.0] [0.0 360.0] [0.0 1.0] "min=0.2, mid=0.1, max=0.7"]
                        [:Gray [0.0 255.0] [0.0 255.0] [0.0 255.0] ""]
                        [:HCL [-180.0 180.0] [0.0 170.0] [0.0 135.27] "Sarifuddin/Missaou, λ=3"]
                        [:HSB [0.0 360.0] [0.0 1.0] [0.0 1.0] "same as HSV"]
                        [:HSI [0.0 360.0] [0.0 1.0] [0.0 1.0] ""]
                        [:HSL [0.0 360.0] [0.0 1.0] [0.0 1.0] ""]
                        [:HSV [0.0 360.0] [0.0 1.0] [0.0 1.0] "same as HSB"]
                        [:HWB [0.0 360.0] [0.0 1.0] [0.0 1.0] ""]
                        [:HunterLAB [0.0 100.0] [-69.08 109.46] [-199.78 55.72] ""]
                        [:IPT [0.0 1.0] [-0.45 0.66] [-0.75 0.65] ""]
                        [:IgPgTg [0.0 0.97] [-0.35 0.39] [-0.41 0.44] ""]
                        [:JAB [0.0 0.17] [-0.09 0.11] [-0.16 0.12] "JzAzBz, white point=100"]
                        [:JCH [0.0 0.17] [0.0 0.16] [0.0 360.0] "polar JzAzBz"]
                        [:LAB [0.0 100.0] [-86.18 98.23] [-107.86 94.48] "D65"]
                        [:LCH [0.0 100.0] [0.0 133.81] [0.0 360.0] "polar LAB"]
                        [:LMS [0.0 100.0] [0.0 100.0] [0.0 100.0] ""]
                        [:LUV [0.0 100.0] [-83.08 175.02] [-134.1 107.4] ""]
                        [:OHTA [0.0 255.0] [-127.5 127.5] [-127.5 127.5] ""]
                        [:OSA [-13.51 7.14] [-20.0 20.0] [-23.0 23.0] "excluded extreme values"]
                        [:Oklab [0.0 1.0] [-0.23 0.28] [-0.31 0.2] ""]
                        [:Oklch [0.0 1.0] [0.0 0.32] [0.0 360.0] "polar Oklab"]
                        [:PalettonHSV [0.0 360.0] [0.0 2.0] [0.0 2.0] ""]
                        [:RYB [0.0 255.0] [0.0 255.0] [0.0 255.0] "Sugita/Takahashi"]
                        [:XYB [-0.02 0.03] [0.0 0.85] [0.0 0.85] "from JPEG XL"]
                        [:XYZ [0.0 95.47] [0.0 100.0] [0.0 108.88] "D65"]
                        [:XYZ1 [0.0 0.9547] [0.0 1.0] [0.0 1.0888] "D65"]
                        [:UCS [0.0 0.63] [0.0 1.0] [0.0 1.57] ""]
                        [:UVW [-82.15 171.81] [-87.16 70.82] [-17.0 99.0] ""]
                        [:YCbCr [0.0 255.0] [-127.5 127.5] [-127.5 127.5] ""]
                        [:YCgCo [0.0 255.0] [-127.5 127.5] [-127.5 127.5] ""]
                        [:YDbDr [0.0 255.0] [-340.0 340.0] [-340.0 340.0] ""]
                        [:YIQ [0.0 255.0] [-151.9 151.9] [-133.26 133.26] ""]
                        [:YPbPr [0.0 255.0] [-236.59 236.59] [-200.79 200.79] ""]
                        [:YUV [0.0 255.0] [-111.18 111.18] [-156.83 156.83] ""]
                        [:Yxy [0.0 100.0] [0.15 0.64] [0.06 0.6] "CIE xyY"]])})

;; ### sRGB vs linear RGB

;; By default all colors are interpreted as `sRGB`. To convert to `linear RGB` call `to-linearRGB` or `from-sRGB`.

(c/to-linearRGB [124.0 125.0 254.0])
(c/from-linearRGB [51.4 52.3 252.7])

;; ### Color converter

;; You can build a color converter similar to `colorMode` in Processing/Quil. `color-converter` function returns a function which converts from given color space to `sRGB` using a channel values ranging from `0.0` to a scale (`255.0` by default) parameter. Possible options are:

;; * `(color-converter :HSB)` - returns a function converting from `HSB` color space with all channels are set to range from `0.0` to `255.0`
;; * `(color-converter :HSB 100.0)` - as above with input channels scaled between `0.0` to `100.0`
;; * `(color-converter :HSB 10.0 20.0 30.0)` - each color channel is scaled respectively to a maximum: `10`, `20` and `30`. Alpha channel is scaled from `0.0` to `255.0`
;; * `(color-converter :HSB 10 20 30 100)` - as above, with alpha scaled to a maximum `100`.

;; Let's create converter from `HSB` color space which accepts all parameters from the range `[0.0 100.0]`.

(def from-HSB-100 (c/color-converter :HSB 100.0))

(🎨 (c/from-HSB [180.0 0.5 0.5])
    (c/from-HSB* [127.5 127.5 127.5])
    (from-HSB-100 [50.0 50.0 50.0]))

;; Similarly, let's create converter from `LCH` where `L` channel is from the range `[0.0 1.0]`,`C` from the range `[0.0 2.0]`, `H` from the range `[0.0 3.0]` and alpha is from `0.0` to `100.0`.

(def from-LCH-123 (c/color-converter :LCH 1 2 3 100))

(🎨 (c/from-LCH [55 90 40 205])
    (from-LCH-123 [0.55 1.345 1/3 80.4]))

;; ## Random color generation

;; To generate random color, call `random-color`. You can optionally select a color theme (thi.ng or paletton presets). List of color schemes: 

c/color-themes

(🎨 (sort-by c/hue (repeatedly 15 c/random-color))
    (sort-by c/hue (repeatedly 15 #(c/random-color :warm (r/drand 100 255))))
    (sort-by c/hue (repeatedly 15 #(c/random-color :pastels-dark))))

;; ### Applying theme

;; Sometimes you may want to generate random color similar to a given one. For that you can call `apply-theme` functions. Function accepts source color and color theme as a name or scheme vector.

;; #### Color theme by name

(🎨 :docc/green-blue
    (repeatedly 15 #(c/apply-theme :docc/green-blue :warm))
    (repeatedly 15 #(c/apply-theme :docc/green-blue :pastels-bright))
    (repeatedly 15 #(c/apply-theme :docc/green-blue :fresh)))

;; #### Color theme by scheme

;; Scheme vector consists 3 values, one for each LCH channel. If a value is:

;; * a 2-element vector, channel value is selected randomly from a given range
;; * a number, channel value is selected randomly from [channel-number, channe+number] range
;; * other values just leave original channel value

;; Operations are done in LCH* (0-255) color space

(🎨 :docc/yellow-ocher
    ;; hue is selectod randomly from a values +-30 around source one
    (repeatedly 15 #(c/apply-theme :docc/yellow-ocher [:keep :keep 30]))
    ;; hue is kept but saturation (C) is selected from 220 to 255 range
    (repeatedly 15 #(c/apply-theme :docc/yellow-ocher [:keep [10 255] :keep]))
    ;; additionally let's vary luma
    (repeatedly 15 #(c/apply-theme :docc/yellow-ocher [50.0 [200 255] :keep])))

;; # Palettes

;; Palette is a vector of colors. It can be constructed by hand, by name, created from hue, gradient, image or from other palette.
(🎨 [:maroon 0xbba3b4f5 :docc/olive [112 33 200]])

;; ## Names

;; `clojure2d.color` defines plenty of ready to use palettes gathered from many sources and other libraries, Colourlovers palettes, thi.ng, dictionary of colour combinations, R Paletteer package (collection), cpt-city - an archive of colour gradients, ColorBrewer and so on.

;; The full list of names can be found [here](https://clojure2d.github.io/clojure2d/docs/static/palettes/index.html). 

;; To create palette from name, call `palette` function with a number or keyword as a name. Call without an argument to select random name and hence the palette.

(🎨 (c/palette))

;; Simple keywords as a names consist ColorBrewer, Wes Anderson, common charting, and some other basic palettes

^{::clerk/visibility :hide}
(clerk/table
 [[:accent (🎨 (c/palette :accent))]
  [:category10 (🎨 (c/palette :category10))]
  [:microsoft-2 (🎨 (c/palette :microsoft-2))]
  [:grand-budapest-1 (🎨 (c/palette :grand-budapest-1))]
  [:oranges-5 (🎨 (c/palette :oranges-5))]])

;; ### [Colourlovers](https://www.colourlovers.com/palettes/most-loved/all-time/meta)

;; The best 500 palettes. The name is an integer from `0` to `499`

(🎨 (c/palette 0)
    (c/palette 499))

^{::clerk/visibility :hide}
(clerk/table
 [[1 (🎨 (c/palette 1))]
  [10 (🎨 (c/palette 10))]
  [50 (🎨 (c/palette 50))]
  [100 (🎨 (c/palette 100))]
  [200 (🎨 (c/palette 200))]
  [400 (🎨 (c/palette 400))]])

;; ### [thi.ng](https://github.com/thi-ng/umbrella/tree/develop/packages/color-palettes)

;; Set of palettes defined in thi.ng umbrella, in `color-palettes` package.

(🎨 (c/palette :thi.ng/ORLwKeosxtEeZxq))

^{::clerk/visibility :hide}
(clerk/table
 [[:thi.ng/OkEXVdMQmQ1oQTp (🎨 (c/palette :thi.ng/OkEXVdMQmQ1oQTp))]
  [:thi.ng/QLj3F8heV6QT4YG (🎨 (c/palette :thi.ng/QLj3F8heV6QT4YG))]
  [:thi.ng/qAPJgQvoDkRkQTN (🎨 (c/palette :thi.ng/qAPJgQvoDkRkQTN))]
  [:thi.ng/bYcivY8Jqx8nsiR (🎨 (c/palette :thi.ng/bYcivY8Jqx8nsiR))]
  [:thi.ng/sz5Uxo4ByGDH6tQ (🎨 (c/palette :thi.ng/sz5Uxo4ByGDH6tQ))]])

;; ### [DOCC](https://github.com/mattdesl/dictionary-of-colour-combinations)

;; 348 palettes from dictionary of color combinations.

(🎨 (c/palette :docc/docc-1)
    (c/palette :docc/docc-348))

^{::clerk/visibility :hide}
(clerk/table
 [[:docc/docc-1 (🎨 (c/palette :docc/docc-2))]
  [:docc/docc-10 (🎨 (c/palette :docc/docc-10))]
  [:docc/docc-100 (🎨 (c/palette :docc/docc-100))]
  [:docc/docc-200 (🎨 (c/palette :docc/docc-200))]
  [:docc/docc-300 (🎨 (c/palette :docc/docc-300))]])

;; ### [Paletteer](https://github.com/EmilHvitfeldt/paletteer)

;; Collection of palettes defined in various R packages.

^{::clerk/visibility :hide}
(clerk/table
 [[:nord/frost            (🎨 (c/palette :nord/frost))]
  [:ghibli/TotoroLight    (🎨 (c/palette :ghibli/TotoroLight))]
  [:cartography/red.pal-9 (🎨 (c/palette :cartography/red.pal-9))]
  [:dutchmasters/anatomy  (🎨 (c/palette :dutchmasters/anatomy))]
  [:palettetown/abra      (🎨 (c/palette :palettetown/abra))]])

;; ### [CPT City](http://soliton.vm.bytemark.co.uk/pub/cpt-city/index.html)

;; Collection of palettes from CPT-City, c3g discrete files.

^{::clerk/visibility :hide}
(clerk/table
 [[:arendal/temperature    (🎨 (c/palette :arendal/temperature))]
  [:cl/fs2010              (🎨 (c/palette :cl/fs2010))]
  [:os/os250k-metres       (🎨 (c/palette :os/os250k-metres))]
  [:jjg_neo10_liht/frozen-in-time (🎨 (c/palette :jjg_neo10_liht/frozen-in-time))]
  [:heine/Exxon88  (🎨 (c/palette :heine/Exxon88))]])

;; ## [Paletton](https://paletton.com/)

;; Paletton is a great web tool to generate palettes based on hue. `paletton` function recreates the method used on the web and returns a palette. Possible options are:

;; * type of the palette: `:monochromatic`, `:triad` and `:tetrad`
;; * hue, an angle from 0 to 360 degrees.
;; * complementary color
;; * adjacent colors angle
;; * a color scheme

;; To generate [these colors](https://paletton.com/#uid=60k0u0kwi++bu++hX++++rd++kX), you should call:

(🎨 (c/paletton :triad 20 {:preset :shiny :compl true :adj true :angle 30}))

;; Other examples

(🎨 (c/paletton :monochromatic 200 {:preset :pale-light :compl false})
    (c/paletton :monochromatic 200 {:preset :pale-light :compl true})
    ;; adjacent colors are taken from complementary color
    (c/paletton :triad 200 {:preset :pale-light :compl false :adj false})
    (c/paletton :triad 200 {:preset :pale-light :compl true :adj false})
    ;; adjacent colors are taken from base color
    (c/paletton :triad 200 {:preset :pale-light :compl false :adj true})
    (c/paletton :triad 200 {:preset :pale-light :compl true :adj true})
    (c/paletton :tetrad 200 {:preset :pale-light})
    (c/paletton :tetrad 200 {:preset :pale-light :angle 120}))

;; ## Resampling

;; Palette can be resampled to get less or more colors. There are two methods one based on sampling gradient created out of palette, second based on k-means clustering.

;; ### Gradient resampling

;; For given palette provide requested number of colors and optionally gradient creation arguments.

(🎨 (c/palette :moonrise-3))

;; Resampled `:moonrise-3`

(apply 🎨 (map (partial c/palette :moonrise-3)
               [1 2 3 4 5 6 8 10 12 15]))

;; We can resample in different color space and with monotonic interpolation

(apply 🎨 (map #(c/palette :moonrise-3 %
                           {:interpolation :monotone :colorspace :JCH})
               [1 2 3 4 5 6 8 10 12 15]))

;; We can also resample a gradient

(🎨 (c/gradient :pals/kovesi.cyclic_mrybm_35_75_c68))

(apply 🎨 (map #(c/palette (c/gradient :pals/kovesi.cyclic_mrybm_35_75_c68) %) [1 2 3 5 7 10]))

;; ### k-Means

;; In case you have very big palette or a sequence of colors (possibly an image), you can use `reduce-colors` to construct a smaller palette. The algorithm uses `x-means` and number of colors returned may be lower than requested.

;; Let's start with very long palette

(count (c/palette :viridis))

(apply 🎨 (map (partial c/reduce-colors (c/palette :viridis)) [2 3 5 10 30]))

;; Now let's load image

(def cat-image
  (c2d/load-image "https://live.staticflickr.com/3819/11717823153_7be7b26ede_w_d.jpg"))

;; And let's get pixels as a sequence of colors

(def cat-pixels (pixels/to-pixels cat-image))

(apply 🎨 (map (partial c/reduce-colors cat-pixels) [2 3 5 10]))

;; Reduce in `OSA` color space

(apply 🎨 (map #(c/reduce-colors cat-pixels % :OSA) [2 3 5 10]))

;; ### Clarans

;; Instead of providing a color space you can also provide distance function (either own function or one for `fastmath.distance`). For a reduction `clarans` algorithm is used.

;; Warning: it can be very slow.

(require '[fastmath.distance])

(apply 🎨 (map (partial c/reduce-colors (c/palette :viridis) 10)
               [c/contrast-ratio
                c/delta-E*
                c/delta-E-z
                fastmath.distance/chebyshev
                fastmath.distance/manhattan
                fastmath.distance/canberra]))

;; ## Random palette.

;; When you call `random-palette` function you'll get random set of colors. These are selected randomly from preset palettes, gradients and paletton. There are no parameters.

(apply 🎨 (repeatedly 5 c/random-palette))

;; # Gradients

;; Gradient is a continuous function which returns interpolated color, a parameter should be a number from `0.0` to `1.0`. Gradient can be created by name or from palette. Additionally there is a special case for cosinus method of creating gradients (described by Inigo Quilez ,see below).

;; ## Presets

;; There is a big collection of ready to use gradients gathered (similarly to palettes) from other libraries or sites. Full list is available [here](https://clojure2d.github.io/clojure2d/docs/static/gradients/index.html). Some examples:

(🎨 (c/gradient :pals/kovesi.cyclic_mrybm_35_75_c68))

^{::clerk/visibility :hide}
(clerk/table
 {:head [:name :gradient]
  :rows [[:pals/ocean.thermal (🎨 (c/gradient :pals/ocean.thermal))]
         [:grDevices/Inferno (🎨 (c/gradient :grDevices/Inferno))]
         [:xkcd/xkcd-bath (🎨 (c/gradient :xkcd/xkcd-bath))]
         [:ma_retro2/retro2_01 (🎨 (c/gradient :ma_retro2/retro2_01))]
         [:neota_food/carrot (🎨 (c/gradient :neota_food/carrot))]
         [:pd_art/art-nouveau-01 (🎨 (c/gradient :pd_art/art-nouveau-01))]
         [:es_emerald_dragon/es_emerald_dragon_08 (🎨 (c/gradient :es_emerald_dragon/es_emerald_dragon_08))]]})

;; ## Gradient from palette

;; In case you want to create own gradient out of custom palette, you can use several options how to interpolate between colors.
;; You can select color space conversion, interpolation method and color distribution.

(🎨 (c/gradient (c/palette :prl-6)))

;; Let's play with the following gradient. By default interpolation is linear with evenly distributed colors in RGB color space. List of possible interpolations with documentation is [here](https://generateme.github.io/fastmath/fastmath.interpolation.html)

(🎨 (c/gradient [:white :red :yellow (c/color :blue 160) :lime :black]
                {:domain [0.0 0.2 0.4 0.6 0.8 1.0]
                 :interpolation :linear-smile
                 :colorspace :RGB}))

;; Let's create custom `loess` interpolator
(defn loess [xs ys] (fastmath.interpolation/loess 0.7 2 xs ys))

^{::clerk/visibility :hide}
(clerk/table
 {:head [:options :result]
  :rows (map (juxt identity #(🎨 (c/gradient [:white :red :yellow (c/color :blue 160) :lime :black] %)))
             [{} {:interpolation :monotone}
              {:interpolation :cubic-spline}
              {:interpolation :shepard}
              {:interpolation loess}
              {:colorspace :LAB}
              {:colorspace :Oklab}
              {:colorspace :JCH}
              {:domain [0.0 0.1 0.2 0.7 0.8 1.0]}
              {:domain [0.0 0.45 0.47 0.53 0.55 1.0]}])})

;; ## Inigo Quilez gradient generator

;; [Inigo Quilez cosinus method](https://iquilezles.org/www/articles/palettes/palettes.htm) is the way of creating gradients from the following formula:

;; $$c_i(t)=a_i+b_i\cos{[2\pi(c_it+d_i)]}$$
;; $$t\in[0.0,1.0], i\in\{R,G,B\}$$

;; To create a gradient, call `gradient` with `:iq` interpolation. As the first argument you have to provide a,b,c and d coefficient triplets.

(🎨 (let [a-coeffs [0.5 0.5 0.5]
          b-coeffs [0.5 0.5 0.5]
          c-coeffs [1 0.5 -1]
          d-coeffs [0.2 0.3 0.4]]
      (c/gradient [a-coeffs b-coeffs c-coeffs d-coeffs] {:interpolation :iq})))

;; All gradients from the aforementioned article are predefined:

^{::clerk/visibility :hide}
(clerk/table
 {:head [:name :gradient]
  :rows (map (juxt identity #(🎨 (c/gradient %))) (map #(keyword (str "iq-" %)) (range 1 8)))})

;; There is also an option to create gradient from two colors using cosine interpolation. Coefficients are calculated automatically basing on [thi.ng algorithm](https://github.com/thi-ng/color/blob/master/src/gradients.org#gradient-coefficient-calculation).

(🎨
 ;; compare two interpolation methods, default and cosine
 (c/gradient [:red :green])
 (c/gradient [:red :green] {:interpolation :iq})
 ;; yellow-blue
 (c/gradient [:yellow :blue])
 (c/gradient [:yellow :blue] {:interpolation :iq}))

;; ## Easings

;; Another way of interpolating between two colors is by using easing functions. They are defined and documented [here](https://generateme.github.io/fastmath/fastmath.easings.html).

(🎨 (c/gradient [:red :green] {:interpolation :back-in-out})
    (c/gradient [:red :green] {:interpolation :bounce-in-out})
    (c/gradient [:red :green] {:interpolation :poly-in-out})
    (c/gradient [:red :green] {:interpolation :circle-in})
    (c/gradient [:red :green] {:interpolation :circle-out}))

;; ## Merging gradients

;; Two gradients can be merged. The midpoint argument selects the point (default 0.5) where two gradients joins.

(let [g1 (c/gradient [:darkred :deeppink :light-yellow])
      g2 (c/gradient [:light-yellow :lightgreen :teal])]
  (🎨 g1 g2
      (c/merge-gradients g1 g2)
      (c/merge-gradients g1 g2 0.7)))

;; ## Random gradient

;; Random gradient can be generated by calling `random-gradient`.

(apply 🎨 (repeatedly 5 c/random-gradient))

;; # Fixing luma

;; If you want to correct luma to be linear, call `correct-luma`. Please be sure you palette is sorted by luma (ascending or descending) before calling.

;; The method is similar to one used in `chroma.js` and described [here](https://www.vis4.net/blog/2013/09/mastering-multi-hued-color-scales/#combining-bezier-interpolation-and-lightness-correction). The example showin in article is recreated below:

(let [pal [:lightyellow :orangered :deeppink :darkred]]
  (🎨 (-> (c/gradient pal {:colorspace :LAB}) (c/palette 9))
      (-> (c/gradient pal {:colorspace :LAB
                           :interpolation :b-spline}) (c/palette 9))
      (-> (c/gradient pal {:colorspace :LAB}) c/correct-luma (c/palette 9))
      (-> (c/gradient pal {:colorspace :LAB
                           :interpolation :b-spline}) c/correct-luma (c/palette 9))))

;; Let's see other examples with luma profiles.

(🎨 (sort-by c/luma (c/palette :guell))
    (c/correct-luma (sort-by c/luma (c/palette :guell))))

;; Since it's done in `LAB` color space, some colors can vanish.

;; `correct-luma` works also for gradients (should be monotonic).

(🎨 (c/gradient [:black :docc/deep-slate-green :orangered :aquamarine :white])
    (c/correct-luma (c/gradient [:black :docc/deep-slate-green :orangered :aquamarine :white])))

(defn get-luma-from-gradient
  [gradient]
  (let [cgradient (c/correct-luma gradient)
        xs (m/slice-range 0.0 1.0 50)]
    (map (partial zipmap [:t :luma :corrected?])
         (concat (map (juxt identity
                            (comp first c/to-LAB gradient)
                            (constantly "no")) xs)
                 (map (juxt identity
                            (comp first c/to-LAB cgradient)
                            (constantly "yes")) xs)))))

(clerk/vl {:data {:values (get-luma-from-gradient (c/gradient [:black :docc/deep-slate-green :orangered :aquamarine :white]))}
           :width 600
           :height 300
           :title "Luma from LAB color space"
           :usermeta {:embedOptions {:renderer "svg"}}
           :mark {:type :line}
           :encoding {:x {:field :t :type :quantitative}
                      :y {:field :luma :type :quantitative}
                      :color {:field :corrected? :type :nominal}}})

(🎨 (c/gradient [:white :lightcyan :palegreen :navy :black])
    (c/correct-luma (c/gradient [:white :lightcyan :palegreen :navy :black])))

(clerk/vl {:data {:values (get-luma-from-gradient (c/gradient [:white :lightcyan :palegreen :navy :black]))}
           :width 600
           :height 300
           :title "Luma from LAB color space"
           :usermeta {:embedOptions {:renderer "svg"}}
           :mark {:type :line}
           :encoding {:x {:field :t :type :quantitative}
                      :y {:field :luma :type :quantitative}
                      :color {:field :corrected? :type :nominal}}})

;; Let's create diverging multi-hue color palette as desribed in [this chapter](https://www.vis4.net/blog/2013/09/mastering-multi-hued-color-scales/#update-diverging-multi-hue-color-palettes).

(let [opts {:interpolation :b-spline :colorspace :LAB}
      g1 (-> (c/gradient [:darkred :deeppink :light-yellow] opts) c/correct-luma)
      g2 (-> (c/gradient [:light-yellow :lightgreen :teal] opts) c/correct-luma)]
  (🎨 (c/palette (c/merge-gradients g1 g2) 13)))

^{::clerk/visibility :hide}
(let [opts {:interpolation :b-spline :colorspace :LAB}
      g (c/merge-gradients
         (-> (c/gradient [:darkred :deeppink :light-yellow] opts) c/correct-luma)
         (-> (c/gradient [:light-yellow :lightgreen :teal] opts) c/correct-luma))]
  (clerk/vl {:data {:values (map #(zipmap [:t :luma]
                                          [% (first (c/to-LAB (g %)))]) (m/slice-range 0.0 1.0 100))}
             :width 600
             :height 300
             :usermeta {:embedOptions {:renderer "svg"}}
             :title "Luma from LAB color space"
             :mark {:type :line}
             :encoding {:x {:field :t :type :quantitative}
                        :y {:field :luma :type :quantitative}}}))

;; # Mixing and blending

;; ## Mixing

;; Colors can be mixed and blended in various ways. Let's start with linear interpoation. By default `lerp` finds mid colour. 

(🎨 (c/lerp :orange :blue))

;; But we can decide about the amount of interpolation

(🎨 (c/lerp :orange :blue 0.25)
    (c/lerp :orange :blue 0.75))

;; Another two functions `lerp+` and `lerp-` are trying to conserve brightness of the right or left color.

(🎨 (c/lerp- :orange :blue 0.3)
    (c/lerp+ :orange :blue 0.7))

;; Brightness (Luma) is taken from the first channel of *LAB* color space.

^{::clerk/viewer :block}
[(c/get-channel :blue :LAB 0)
 (c/get-channel (c/lerp+ :orange :blue 0.7) :LAB 0)
 (c/get-channel :orange :LAB 0)
 (c/get-channel (c/lerp- :orange :blue 0.3) :LAB 0)]

;; There also two other methods of mixing: `mix` and `mixsub`. The former one mixes squared values (and takes square root after all), the latter perform subtractive mixing.

(🎨 (c/mix :orange :blue)
    (c/mixsub :orange :blue))

;; We can also lerp and mix in different color spaces
(🎨 (c/lerp :orange :blue :Oklab 0.5)
    (c/mix :orange :blue :Oklab 0.5))

;; To compare above methods let's check gradients generated by them:

(🎨 (partial c/lerp :orange :blue)
    (partial c/lerp :orange :blue :Oklab)
    (partial c/lerp- :orange :blue)
    (partial c/lerp+ :orange :blue)
    (partial c/mix :orange :blue)
    (partial c/mix :orange :blue :Oklab)
    (partial c/mixsub :orange :blue)
    (partial c/mixsub :orange :blue :Oklab))


;; Last method calculates average colors.

(🎨 (c/average [:orange :blue :green :yellow]))

;; We can also average with weights

(🎨 (c/weighted-average [:orange :blue :green :yellow] [1 0.5 1 5]))

;; Additionally we can average in different color spaces:

(🎨 (c/average [:orange :blue :green :yellow] :LAB)
    (c/weighted-average [:orange :blue :green :yellow] [1 0.5 1 5] :LUV))

;; ## Blending

;; Two colors/palettes/gradients can be combined in various ways known from graphics editors. Bleding functions are defined in `clojure2d.color.blend`.

;; You can select one blending method for all channels or for each channel separately. Algorithms and alpha blending are described [here](https://www.w3.org/TR/compositing-1/#blending) (simple alpha compositing is used in the library).

(🎨 (bl/blend-colors bl/add [12 33 55] [55 66 77])
    (bl/blend-colors bl/add [12 33 55 120] [55 66 77 200])
    (bl/blend-colors bl/subtract bl/multiply bl/screen [12 33 55] [55 66 77]))

;; List of all blending methods

bl/blends-list

;; Below table shows the result after blending two following colors.

(🎨 :docc/yellow-ocher :docc/rosolanc-purple)

^{::clerk/visibility :hide}
(clerk/table
 {:head [:method :result]
  :rows (mapv (juxt identity #(🎨 (bl/blend-colors (bl/blends %) :docc/yellow-ocher :docc/rosolanc-purple))) bl/blends-list)})

;; Above methods work also for palettes and gradients

(🎨 (c/palette :spectral-9)
    (c/palette :reds-9)
    (bl/blend-palettes bl/subtract (c/palette :spectral-9) (c/palette :reds-9))
    (bl/blend-palettes bl/darken (c/palette :spectral-9) (c/palette :reds-9))
    (bl/blend-palettes bl/screen (c/palette :spectral-9) (c/palette :reds-9)))

(🎨 (c/gradient :yellow-purple-magenta)
    (c/gradient :cubehelix)
    (bl/blend-gradients bl/subtract
                        (c/gradient :yellow-purple-magenta) (c/gradient :cubehelix))
    (bl/blend-gradients bl/darken
                        (c/gradient :yellow-purple-magenta) (c/gradient :cubehelix))
    (bl/blend-gradients bl/screen
                        (c/gradient :yellow-purple-magenta) (c/gradient :cubehelix)))

;; ### Channel copying

;; You can also copy channels between colors in HSB or any color space.

(🎨 (bl/hue :docc/yellow-ocher :docc/rosolanc-purple)
    (bl/hue :docc/rosolanc-purple :docc/yellow-ocher)
    (bl/saturation :docc/yellow-ocher :docc/rosolanc-purple)
    (bl/saturation :docc/rosolanc-purple :docc/yellow-ocher)
    (bl/luminocity :docc/yellow-ocher :docc/rosolanc-purple)
    (bl/luminocity :docc/rosolanc-purple :docc/yellow-ocher)
    (bl/color :docc/yellow-ocher :docc/rosolanc-purple)
    (bl/color :docc/rosolanc-purple :docc/yellow-ocher)
    (bl/ch0 :RGB :docc/yellow-ocher :docc/rosolanc-purple)
    (bl/ch0 :RGB :docc/rosolanc-purple :docc/yellow-ocher)
    (bl/ch1 :RGB :docc/yellow-ocher :docc/rosolanc-purple)
    (bl/ch1 :RGB :docc/rosolanc-purple :docc/yellow-ocher)
    (bl/ch2 :RGB :docc/yellow-ocher :docc/rosolanc-purple)
    (bl/ch2 :RGB :docc/rosolanc-purple :docc/yellow-ocher))

;; # Alterations

;; There is a collection of functions which help to alter color characteristics like brightness, saturation, hue, etc. We can also change temperature or tint with other color. We can also build and use `feColorMatrix`.

;; Alterations can be used to alter palettes and gradients.

;; ## Modulation / adjustment

;; Multiplicative or additive operations on channels

;; ### Modulate

;; First option is to use `modulate` function, which multiplies selected channel by a value. Additionally you can choose color space to operate in.

(🎨 (c/modulate :red 0 0.5) ;; Divide red channel by 2
    :maroon ;; Make `:maroon` brighter and less saturated
    (-> :maroon
        (c/modulate :HSL 2 2.0) ; change L channel
        (c/modulate :HSL 1 0.5) ; change S channel
        ))

;; Let's modulate a palette and gradient

(🎨 (c/palette :category10)
    ;; darken by changing L channel in LAB color space
    (mapv #(c/modulate % :LAB 0 0.5) (c/palette :category10)))

(🎨 (c/gradient :warm)
    ;; saturate in LCH color space
    (comp #(c/modulate % :LCH 1 2.0) (c/gradient :warm)))

;; ### Adjust

;; Similarly works `adjust`, the only difference is that it adds a value to a channel.

(🎨 (c/adjust :red 1 127.0)
    ;; Rotate hue 60deg in *HSV* color space
    (c/adjust :red :HSV 0 60.0))

;; The same for palette and gradient

(🎨 (c/palette :category10)
    ;; darken by changing a blackness in HWB color space
    (mapv #(c/adjust % :HWB 2 0.5) (c/palette :category10)))

(🎨 (c/gradient :warm)
    ;; rotate hue 60deg
    (comp #(c/adjust % :HSB 0 60.0) (c/gradient :warm)))

;; ### Saturation and brightness

;; Another group of function changes saturation and brightness directly, use:

;; * `saturate` and `desaturate` to change color saturation (it's done in *LCH* color space)
;; * `brighten` and `darken` to change luma (it's done in *LAB* color space)

;; #### Saturation / desaturation

(🎨 :lightblue
    (c/saturate :lightblue)
    (c/saturate :lightblue 2.0)
    (c/desaturate :lightblue 0.5)
    (c/desaturate :lightblue))

(🎨 (c/palette :purple-gray-6)
    (mapv c/saturate (c/palette :purple-gray-6))
    (mapv c/desaturate (c/palette :purple-gray-6)))

(🎨 (c/gradient :cool)
    (comp c/saturate (c/gradient :cool))
    (comp c/desaturate (c/gradient :cool)))

;; #### Brighten / darken

(🎨 :lightblue
    (c/brighten :lightblue)
    (c/brighten :lightblue 2.0)
    (c/darken :lightblue)
    (c/darken :lightblue 2.0))

(🎨 (c/palette :purple-gray-6)
    (mapv c/brighten (c/palette :purple-gray-6))
    (mapv c/darken (c/palette :purple-gray-6)))

(🎨 (c/gradient :cool)
    (comp c/brighten (c/gradient :cool))
    (comp c/darken (c/gradient :cool)))

;; #### Whiten / blacken

(🎨 :lightblue
    (c/whiten :lightblue)
    (c/whiten :lightblue -0.2) ;; less whiteness
    (c/whiten :lightblue 1.0)
    (c/blacken :lightblue)
    (c/blacken :lightblue -0.2) ;; less blackness
    (c/blacken :lightblue 1.0))

(🎨 (c/palette :purple-gray-6)
    (mapv c/whiten (c/palette :purple-gray-6))
    (mapv c/blacken (c/palette :purple-gray-6)))

(🎨 (c/gradient :cool)
    (comp c/whiten (c/gradient :cool))
    (comp c/blacken (c/gradient :cool)))

;; ### Temperature

;; Adjusting a temperature can make a color warmer or cooler. You can use a temperature name or actual temperature in Kelvins. Additionally we can specify an amount (default: 0.35) of adjustment.
;; Named temperatures are:

c/temperature-names

;; `temperature` function returns a color connected to a given temperature.
(🎨 (c/temperature 10000))

;; Let's see how temperature gradient looks like (from 0K to 30000K):

(🎨 (comp c/temperature (partial * 30000.0)))

;; and finally let's adjust color,

(🎨 :pink
    (c/adjust-temperature :pink :candle)
    (c/adjust-temperature :pink :candle 0.75)
    (c/adjust-temperature :pink :cool)
    (c/adjust-temperature :pink :cool 0.75)
    (c/adjust-temperature :pink 3800))

;; palette,

(🎨 (c/palette :summer)
    (mapv #(c/adjust-temperature % 1000) (c/palette :summer))
    (mapv #(c/adjust-temperature % 30000) (c/palette :summer)))

;; and gradient

(🎨 (c/gradient :cw_2/cw2-079)
    (comp #(c/adjust-temperature % 1000) (c/gradient :cw_2/cw2-079))
    (comp #(c/adjust-temperature % 30000) (c/gradient :cw_2/cw2-079)))

;; ### Tinting

;; We can also change a color towards the value of other color (or palette/gradient). To make it create a `tinter` which is a function changing color to a selected one. It works by multiplying color values, so works well for lighter colors. Let's make tinter which makes colors greener and more yellowish.

(def greener (c/tinter [50 255 50]))
(def yellowish (c/tinter [255 255 50]))
(def reddish (c/tinter [200 50 60]))

(🎨 (c/color :lightblue)
    (greener :lightblue)
    (yellowish :lightblue)
    (reddish :lightblue)
    (greener :gray)
    (yellowish :gray)
    (reddish :gray))

;; Dark, or colors with channels near to zero, are more problematic. Next chapter (about mixing) can help to find better way for tinting.

(🎨 (greener :red)
    (yellowish :red)
    (reddish :red))

(🎨 (c/palette :beyonce/X47)
    (mapv yellowish (c/palette :beyonce/X47))
    (mapv greener (c/palette :beyonce/X47))
    (mapv reddish (c/palette :beyonce/X47)))

(🎨 (c/gradient :ma_xmas/xmas_01)
    (comp yellowish (c/gradient :ma_xmas/xmas_01))
    (comp greener (c/gradient :ma_xmas/xmas_01))
    (comp reddish (c/gradient :ma_xmas/xmas_01)))

;; ## Matrix operations

;; SVG's [feColorMatrix](https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feColorMatrix) allows another way to alter a color. We can use several predefined and commonly used operators. Matrices operate on `sRGB` color space.

;; ### Contrast

(def contrast-15 (c/contrast 1.5))
(def contrast-075 (c/contrast 0.75))

(🎨 :orange
    (contrast-15 :orange)
    (contrast-075 :orange))

(🎨 (c/palette :njgs/njaquif)
    (mapv contrast-15 (c/palette :njgs/njaquif))
    (mapv contrast-075 (c/palette :njgs/njaquif)))

(🎨 (c/gradient :ggthemes/Green-Gold)
    (comp contrast-15 (c/gradient :ggthemes/Green-Gold))
    (comp contrast-075 (c/gradient :ggthemes/Green-Gold)))

;; ### Exposure

;; Multiplicative adjustment of the channel values. This is the same as CSS `brightness`.

(def exposure-125 (c/exposure 1.25))
(def exposure-075 (c/exposure 0.75))

(🎨 :orange
    (exposure-125 :orange)
    (exposure-075 :orange))

(🎨 (c/palette :njgs/njaquif)
    (mapv exposure-125 (c/palette :njgs/njaquif))
    (mapv exposure-075 (c/palette :njgs/njaquif)))

(🎨 (c/gradient :ggthemes/Green-Gold)
    (comp exposure-125 (c/gradient :ggthemes/Green-Gold))
    (comp exposure-075 (c/gradient :ggthemes/Green-Gold)))

;; ### Brightness

;; Additive adjustment of the channel values. Argument `0` means no change. `1.0` adds `255`, `-1.0` subracts `255`.

(def brightness-+02 (c/brightness 0.2))
(def brightness--02 (c/brightness -0.2))

(🎨 :orange
    (brightness-+02 :orange)
    (brightness--02 :orange))

(🎨 (c/palette :njgs/njaquif)
    (mapv brightness-+02 (c/palette :njgs/njaquif))
    (mapv brightness--02 (c/palette :njgs/njaquif)))

(🎨 (c/gradient :ggthemes/Green-Gold)
    (comp brightness-+02 (c/gradient :ggthemes/Green-Gold))
    (comp brightness--02 (c/gradient :ggthemes/Green-Gold)))

;; ### Saturation

(def saturation-15 (c/saturation 1.5))
(def saturation-075 (c/saturation 0.75))

(🎨 :orange
    (saturation-15 :orange)
    (saturation-075 :orange))

(🎨 (c/palette :njgs/njaquif)
    (mapv saturation-15 (c/palette :njgs/njaquif))
    (mapv saturation-075 (c/palette :njgs/njaquif)))

(🎨 (c/gradient :ggthemes/Green-Gold)
    (comp saturation-15 (c/gradient :ggthemes/Green-Gold))
    (comp saturation-075 (c/gradient :ggthemes/Green-Gold)))

;; ### Grayscale

;; Desaturates a color, `(grayscale 1.0)` creates grays

(def grayscale-05 (c/grayscale 0.5))
(def grayscale-1 (c/grayscale 1.0))

(🎨 :orange
    (grayscale-05 :orange)
    (grayscale-1 :orange))

(🎨 (c/palette :njgs/njaquif)
    (mapv grayscale-05 (c/palette :njgs/njaquif))
    (mapv grayscale-1 (c/palette :njgs/njaquif)))

(🎨 (c/gradient :ggthemes/Green-Gold)
    (comp grayscale-05 (c/gradient :ggthemes/Green-Gold))
    (comp grayscale-1 (c/gradient :ggthemes/Green-Gold)))

;; ### Sepia

(def sepia-05 (c/sepia 0.5))
(def sepia-1 (c/sepia 1.0))

(🎨 :orange
    (sepia-05 :orange)
    (sepia-1 :orange))

(🎨 (c/palette :njgs/njaquif)
    (mapv sepia-05 (c/palette :njgs/njaquif))
    (mapv sepia-1 (c/palette :njgs/njaquif)))

(🎨 (c/gradient :ggthemes/Green-Gold)
    (comp sepia-05 (c/gradient :ggthemes/Green-Gold))
    (comp sepia-1 (c/gradient :ggthemes/Green-Gold)))

;; ### Rotate hue

;; Rotates hue with given argument in degrees. Argument can be negative.

(def hue-rotate--60 (c/hue-rotate -60.0))
(def hue-rotate-+60 (c/hue-rotate +60.0))

(🎨 :orange
    (hue-rotate--60 :orange)
    (hue-rotate-+60 :orange))

(🎨 (c/palette :njgs/njaquif)
    (mapv hue-rotate--60 (c/palette :njgs/njaquif))
    (mapv hue-rotate-+60 (c/palette :njgs/njaquif)))

(🎨 (c/gradient :ggthemes/Green-Gold)
    (comp hue-rotate--60 (c/gradient :ggthemes/Green-Gold))
    (comp hue-rotate-+60 (c/gradient :ggthemes/Green-Gold)))

;; ### Custom matrix

;; We can create a custom color matrix as described in [this article](https://alistapart.com/article/finessing-fecolormatrix/).
;; To construct a matrix operator, call `fe-color-matrix` with a vector of 20 values (row-wise).

(def blue-magenta (c/fe-color-matrix [1 0 0 0 0 0 0 0 0 0 0 0 1 0.5 0 0 0 0 1 0]))

(🎨 :orange
    (blue-magenta :orange))

(🎨 (c/palette :njgs/njaquif)
    (mapv blue-magenta (c/palette :njgs/njaquif)))

(🎨 (c/gradient :ggthemes/Green-Gold)
    (comp blue-magenta (c/gradient :ggthemes/Green-Gold)))

;; # Color Vision Deficiency 

;; To simulate how color, palette or gradient looks with given color blidness you can call `cvd-lens` function. There are 8 defined CVDs. Let's simulate CVD with a `rainbow` grandient.

(def rainbow (c/gradient :grDevices/rainbow))

(🎨 rainbow
    (comp c/achromatomaly rainbow)
    (comp c/achromatopsia rainbow)
    (comp c/deuteranomaly rainbow)
    (comp c/deuteranopia rainbow)
    (comp c/protanomaly rainbow)
    (comp c/protanopia rainbow)
    (comp c/tritanomaly rainbow)
    (comp c/tritanopia rainbow))

;; # CSSGram

;; [CSSGram](https://github.com/una/CSSgram) filters

^{::clerk/visibility :hide
  ::clerk/viewer :hide-result}
(def css-jewel-p #(🎨 (map % (c/palette :rdylgn-7))))

^{::clerk/visibility :hide
  ::clerk/viewer :hide-result}
(def css-jewel-g #(🎨 (comp % (c/gradient :rainbow))))

^{::clerk/visibility :hide}
(clerk/table
 {:head [:filter :palette :gradient]
  :rows (conj
         (map (juxt name #(css-jewel-p (cssgram/filters %)) #(css-jewel-g (cssgram/filters %))) cssgram/filters-list)
         ["input palette and gradient" (🎨 (c/palette :rdylgn-7)) (🎨 (c/gradient :rainbow))])})

;; Let's create a helper function which allows us to filter image

(def filter-cat #(->> cat-pixels (pixels/filter-colors %) c2d/to-image))

(clerk/table
 [["1997" "aden" "brooklyn"]
  [(filter-cat cssgram/y1977) (filter-cat cssgram/aden) (filter-cat cssgram/brooklyn)]
  ["moon" "walden" "x-pro2"]
  [(filter-cat cssgram/moon) (filter-cat cssgram/walden) (filter-cat cssgram/x-pro2)]
  ["lofi" "hudson" "reyes"]
  [(filter-cat cssgram/lofi) (filter-cat cssgram/hudson) (filter-cat cssgram/reyes)]])

;; You can create your own filter using small DSL 

(def more-blue-filter (cssgram/custom-filter [:blend :add 0x221122aa]
                                           [:blend :divide 0x33aaaa22]
                                           [:saturation 1.4]
                                           [:contrast 1.3]
                                           [:blend :hue 0x880044ff]
                                           [:exposure 0.9]))

(filter-cat more-blue-filter)

;; # Utils

;; `clojure2d.extra.utils` defines some visualization functions: `show-color`, `show-palette` and `show-gradient`. Functions display a window showing color, palette and gradient. Alternatively, you can use `color->image`, `palette->image` and `gradient->image` to get an image without showing a window.

(utils/color->image :docc/light-glaucous-blue)
(utils/palette->image :ylgnbu-9)
(utils/gradient->image :rainbow)

;; Palette and gradient images can be rendered with luma line as well:

(utils/palette->image :ylgnbu-9 true)
(utils/gradient->image :rainbow true)

^{::clerk/visibility :hide
  ::clerk/viewer :hide-result}
(comment
  (clerk/serve! {:browse? false :watch-paths ["notebooks"]})
  (clerk/show! "notebooks/color.clj")
  (clerk/build-static-app! {:browse? false :paths ["notebooks/color.clj"] :out-path "docs/notebooks/"})
  (clerk/clear-cache!)
  (clerk/halt!))

