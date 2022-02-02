^{:nextjournal.clerk/visibility :hide-ns}
(ns ^:nextjournal.clerk/no-cache notebooks.color
  (:require [nextjournal.clerk :as clerk]
            [clojure2d.color :as c]
            [clerk.styles :refer [🎨 color-styles]]
            [fastmath.vector :as v]))

^{::clerk/viewer :html ::clerk/visibility :hide} color-styles

;; # clojure2d.color namespace

;; `clojure2d.color` namespace for Clojure is a rich collection of functions for various color, palettes and grandents manipulations. Including:
;;
;; * various color representations
;; * color spaces coversions
;; * palettes and gradients
;; * distances
;; * blending

;; I use `🎨` internal function to render given color, palette or gradient.

;; # Color

;; At the beginning let's start with a `color`. A color is always a vector containing 3 coordinates from given color space plus one additional to describe opacity, ie. alpha channel. So we need 4 numbers to describe a color.
;;
;; By default Java and WWW apps interpret a color as a *sRGB*. In `clojure2d.color` value of each channel in *sRGB* color space is from `[0.0-255.0]` range. Other color spaces defined here can operate on different ranges. More about this later.

;; ## Color representation

;; Internally a color is represented as `fastmath.vector.Vec4` type but we can use any other representation which eventually is coerced to a `Vec4`. Let's look at all of the possible options. 

;; ### Named color

;; The quickest option is to use a keyword as a color name, there are two groups of colors.
;;
;; Full list of names can be seen [here](https://clojure2d.github.io/clojure2d/docs/static/colors.html) or retrieved by calling:

(sort (c/named-colors-list))

;; #### HTML color names

;; All basic and extended colors used by modern browsers as a lower-case keyword. Note that some colors can have two similar names. For example `:darkblue` and `:dark-blue` represent the same color.

(🎨 :darkblue)
(🎨 :dark-blue)

;; #### Dictionary of Colour Combinations

;; This is the list of colors compiled in the book ["A Dictionary of Colour Combinations"](https://en.seigensha.com/books/978-4-86152-247-5/) by Sanzo Wada, digitalized by [Dain M. Blodorn Kim](https://github.com/dblodorn/sanzo-wada/) and eventually correctend and published by [Matt DesLauriers](https://github.com/mattdesl/dictionary-of-colour-combinations).
;;
;; Colors from this resource are prefixed by a `:docc/` namespace.

(🎨 :docc/burnt-sienna)
(🎨 :docc/peacock-blue)

;; ### CSS string

;; The other representation can be based on CSS. A hexadecimal string in one of the following forms: `#X`, `#XX`, `#RGB`, `#RRGGBB` or `#RRGGBBAA`. Where `X` (`[0-F]`) or `XX` (`[00-FF]`) are the grey values. The hash character is optional, lower case also can be used.

(🎨 "#7")
(🎨 "#4a")
(🎨 "#FA3")
(🎨 "#FFAA33AA")
(🎨 "f1a130")

;; To convert any color to CSS representation we can call

(c/format-hex :red)
(c/format-hex 0xaa112299)

;; ### Integer

;; Any integer (32bit number) is interpreted as a color, where 8 most significant bits represent alpha channel, next 8 bits, first channel, and so on: `0xAARRGGBB`

(🎨 0x91aa0000)
(🎨 255)

;; ### Any `Sequable`

;; Any Clojure `Sequable` is coerced to a color with the following rules. Color channel value can be any number type (int or double). Numbers are rounded to the nearest integer and clamped to have [0-255] range.
;;
;; 1-element sequence is a `grey`:

(🎨 [129])

;; 2-element sequence is a `gray` with `alpha`:

(🎨 (list 140.11 255/3))

;; 3-element sequence is read as *RGB* values:

(🎨 (seq (java.util.ArrayList. [240 220 20])))

;; Finally, 4-element sequence is treated as *RGBA*, any additional sequence elements are ignored:

(🎨 [129 190 222 230 1 2 3 4])

;; ### Fastmath vectors

;; Fastmath vectors `Vec2`, `Vec3` and `Vec4` are treated as a `Sequable`

(🎨 (v/vec2 12 200))
(🎨 (v/vec3 12 99 200))
(🎨 (v/vec4 12 99 122 200))

;; ### AWT Color

;; For interoperability we can use also AWT color (`java.awt.Color` class).

(🎨 (java.awt.Color. 120 33 99))
(🎨 (java.awt.Color. 0.9 0.2 0.4))
(🎨 (java.awt.Color/PINK))

;; ### Coercions

;; Any color representation is implicitely coerced to a `fastmath.vector.Vec4` type. You can make explicit coercion with `to-color` and `color` function. The latter can construct a color from channel values. Additionally `gray` function returns gray color (with optional alpha)

(class (c/to-color :green))
(map c/to-color [:red 0xffaa33 "fa4" [1 2 3 4]])
(🎨 (c/color :red 200))
(🎨 (c/color 12 33 99))
(🎨 (c/gray 99))

;; You can also convert to AWT Color, integer and - as previously mentioned - to a CSS Color

(c/awt-color :pink)
(c/awt-gray 123)
(c/format-hex :pink)
(c/pack :pink)

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

(🎨 (c/set-green :red 200))
(🎨 (c/set-alpha :red 200))
(🎨 (c/color :red 200))
(🎨 (c/set-channel :red :HSL 0 300))

;; Additionally you can multiply selected channel by given value with `modulate` function. It can also operate within selected colorspace (more about this later).

(🎨 (c/modulate :red 0 0.5))
;; Divide saturation by 2
(🎨 (c/modulate :red :HSL 1 0.5))

;; ### Getting channels

;; To access given channel call:

;; * `red` or `ch0` - to get first channel
;; * `green` or `ch1` - to get second channel
;; * `blue` or `ch2` - to get third channel
;; * `alpha` - to get alpha channel
;; * `get-channel` - to get selected channel (optionally for given color space)

(🎨 :docc/fawn)
((juxt c/red c/green c/blue c/alpha) :docc/fawn)
;; Get luma channel from HSL color space for `:red` color.
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

(🎨 (c/clamp [-23.3 123.033 293.33]))
(🎨 (c/lclamp [-23.3 123.033 293.33]))

;; ## Alterations

;; ## Mixing and blending

;; ## Color comparison and distances

;; ## Color space conversions

;; ## Palettes

;; ### Paletton

;; ## Gradients

;; ### Inigo Quilez gradient generator

;; ## Color Vision Deficiency 

;; To simulate how color, palette or gradient looks with given color blidness you can call `cvd-lens` function. There are 8 defined CVDs. Let's simulate CVD with a `rainbow` grandient.

(def rainbow (c/gradient :grDevices/rainbow))

(🎨 rainbow)
(🎨 (c/cvd-lens rainbow :achromatomaly))
(🎨 (c/cvd-lens rainbow :achromatopsia))
(🎨 (c/cvd-lens rainbow :deuteranomaly))
(🎨 (c/cvd-lens rainbow :deuteranopia))
(🎨 (c/cvd-lens rainbow :protanomaly))
(🎨 (c/cvd-lens rainbow :protanopia))
(🎨 (c/cvd-lens rainbow :tritanomaly))
(🎨 (c/cvd-lens rainbow :tritanopia))

;; ## Random generators

(🎨 (c/random-color))
(🎨 (c/random-palette))
(🎨 (c/random-gradient))

^{:nextjournal.clerk/visibility :hide
  :nextjournal.clerk/viewer :hide-result}
(comment
  (require '[nextjournal.clerk :as clerk])
  (clerk/serve! {:browse? false :watch-paths ["notebooks"]})
  (clerk/show! "notebooks/color.clj")
  (clerk/build-static-app! {:browse? false :paths ["notebooks/color.clj"] :out-path "docs/notebooks/"})
  (clerk/halt!))
