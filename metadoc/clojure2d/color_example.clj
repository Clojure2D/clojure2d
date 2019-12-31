(ns clojure2d.color-example
  (:require [hiccup.core :refer :all]
            [metadoc.examples :refer :all]
            [clojure2d.color :refer :all]
            [clojure2d.color.blend :as b]
            [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.easings :as e]
            [fastmath.interpolation :as i]
            [fastmath.kernel :as rbf]))

(r/set-seed! r/default-rng 1)

(defmacro example-color [description ex]
  `(assoc (example ~description ~ex) :type :color))

(defmacro example-palette [description ex]
  `(assoc (example ~description ~ex) :type :palette))

(defmacro example-gradient [description ex]
  `(assoc (example ~description ~ex) :type :gradient))

(defn color->html [c]
  (let [str-color (if (< (luma c) 128.0) "white" "black")
        o (/ (alpha c) 255.0)
        n (format-hex c)]
    (html [:span {:style (str "display:block;opacity:" o ";padding:5px;background-color:" n ";color:" str-color ";")} (str "[" n "] " c)])))

(defn palette->html [c]
  (html (for [col c]
          [:span {:style (str "border:1px solid;margin-right:1px;padding:6px;background-color:" (format-hex col) ";border-radius:5px;")} "  "])))

(defn gradient->html [f]
  (html (for [^double t (range 0.0 1.0 0.015)]
          [:span {:style (str "background-color:" (format-hex (f t)) ";width:1%;display:inline-block;height:20px;")}])))

(defmethod format-html :color [r]
  (html [:div
         [:blockquote (:doc r)]
         [:pre [:code {:class "hljs clojure"} (:example r)]
          [:div {:style "z-index:-10;background-image:url(../bg.png);top:0;bottom:0;left:0;right:0"} (color->html (:result r))]]]))

(defmethod evaluate :color [ex]
  (assoc (evaluate (assoc ex :type :simple)) :type :color))

(defmethod format-html :palette [r]
  (html [:div
         [:blockquote (:doc r)]
         [:pre [:code {:class "hljs clojure"} (:example r)]
          (palette->html (:result r))]]))

(defmethod evaluate :palette [ex]
  (assoc (evaluate (assoc ex :type :simple)) :type :palette))

(defmethod format-html :gradient [r]
  (html [:div
         [:blockquote (:doc r)]
         [:pre [:code {:class "hljs clojure"} (:example r)]]
         [:div {:style "width:100%"} (gradient->html (:result r))]]))

(defmethod evaluate :gradient [ex]
  (assoc (evaluate (assoc ex :type :simple)) :type :gradient))

;; --------------

(add-examples to-color
  (example-session "Various conversions"
    (to-color :khaki)
    (to-color "abcc12")
    (to-color "#1234af")
    (to-color "#1234af80")
    (to-color "a")
    (to-color "4a")
    (to-color "fac")
    (to-color (v/vec2 120 120))
    (to-color (v/vec3 144 133.3 122))
    (to-color (v/vec4 111 122 133 44))
    (to-color [30 40 50])
    (to-color (range 200 220 5))
    (to-color 0xaafffeeb)))

(add-examples to-awt-color
  (example-session "Various conversions"
    (to-awt-color :khaki)
    (to-awt-color 0xaa112233)))

(add-examples luma (example-session "Usage" (luma 0x152535) (luma :red)))
(add-examples relative-luma (example-session "Usage" (relative-luma 0x152535) (relative-luma :red)))
(add-examples red (example-session "Usage" (red 0x152535) (red :khaki)))
(add-examples green (example-session "Usage" (green 0x152535) (green :khaki)))
(add-examples blue (example-session "Usage" (blue 0x152535) (blue :khaki)))
(add-examples alpha (example-session "Usage" (alpha 0x05152535) (alpha :khaki)))
(add-examples ch0 (example-session "Usage" (ch0 0x152535) (ch0 :khaki)))
(add-examples ch1 (example-session "Usage" (ch0 0x152535) (ch1 :khaki)))
(add-examples ch2 (example-session "Usage" (ch0 0x152535) (ch2 :khaki)))

(add-examples get-channel
  (example (get-channel :green 1))
  (example "Get first channel from LAB representation of green." (get-channel :green :LAB 0)))

(add-examples hue (example-session "Usage" (hue :red) (hue :green) (hue :blue) (hue "#12f5e6") (hue-polar "#12f5e6") (hue-paletton "#4455f6")))
(add-examples hue-polar (example-session "Usage" (hue-polar "#4455f6") (hue "#4455f6") (hue-paletton "#4455f6")))

(add-examples lerp
  (example-palette "Usage" [(lerp :red :blue 0) (lerp :red :blue 0.5) (lerp :red :blue 1.0)])
  (example-gradient "As gradient" (partial lerp :red :blue)))
(add-examples lerp+
  (example-palette "Usage" [(lerp+ :red :blue 0) (lerp+ :red :blue 0.5) (lerp+ :red :blue 1.0)])
  (example-gradient "As gradient" (partial lerp+ :red :blue)))
(add-examples lerp-
  (example-palette "Usage" [(lerp- :red :blue 0) (lerp- :red :blue 0.5) (lerp- :red :blue 1.0)])
  (example-gradient "As gradient" (partial lerp- :red :blue)))

(add-examples set-alpha (example-color "Usage" (set-alpha :khaki 200)))
(add-examples set-ch0 (example-color "Usage" (set-ch0 :khaki 11)))
(add-examples set-ch1 (example-color "Usage" (set-ch1 :khaki 11)))
(add-examples set-ch2 (example-color "Usage" (set-ch2 :khaki 11)))
(add-examples set-red (example-color "Usage" (set-red :khaki 11)))
(add-examples set-green (example-color "Usage" (set-green :khaki 11)))
(add-examples set-blue (example-color "Usage" (set-blue :khaki 11)))
(add-examples set-awt-alpha (example-color "Usage" (set-awt-alpha :khaki 200)))

(add-examples gray (example-color "Usage" (gray 123)))
(add-examples awt-gray (example-color "Usage" (awt-gray 23)))

(add-examples color
  (example-session "Usage" (color :yellow) (color :yellow 112) (color 1 2 3) (color 6 5 4 11))
  (example-color "Usage" (color 88 55 44 200)))

(add-examples awt-color
  (example-session "Usage" (awt-color :yellow) (awt-color 6 5 4 11))
  (example-color "Usage" (awt-color 200 200 112 200)))

(add-examples clamp
  (example "Usage" (clamp (color 304 -123 3.4 22.9))))

(add-examples lclamp
  (example "Usage" (lclamp (color 304 -123 3.4 22.9))))

(add-examples format-hex
  (example-session "Usage"
    (format-hex :maroon)
    (format-hex (color 4 55 222))
    (format-hex (color 4 55 222 127.5))))

(add-examples pack
  (example-session "Pack colors into 32 bit int."
    (pack :green)
    (pack (color 12 33 255)))
  (example-session "See the difference about treating alpha when packing raw integer."
    (pack 0x1122ff)
    (pack (to-color 0x1122ff))))

(add-examples modulate
  (example-color "More red" (modulate [123 22 233] 0 1.2))
  (example-color "More saturation" (modulate [123 22 233] :HSL 1 1.2))
  (example-palette "Decrease luma" (mapv #(modulate % :LAB 0 0.8) (palette 0))))

;;

(add-examples possible-color?
  (example-session "Usage"
    (possible-color? :blue)
    (possible-color? :not-a-color)
    (possible-color? [1 2 3 255])
    (possible-color? [:red :green :blue])
    (possible-color? (gradient [:red :blue]))))

(add-examples valid-color?
  (example-session "Usage"
    (valid-color? :blue)
    (valid-color? :not-a-color)
    (valid-color? [1 2 3 255])
    (valid-color? [:red :green :blue])
    (valid-color? (gradient [:red :blue]))))


(add-examples possible-palette?
  (example-session "Usage"
    (possible-palette? :blue)
    (possible-palette? [1 2 3 255])
    (possible-palette? [:red :green :blue])
    (possible-palette? (gradient [:red :blue]))))

(add-examples temperature
  (example-color "Warm" (temperature :warm))
  (example-color "Blue sky" (temperature :blue-sky))
  (example-color "3000K" (temperature 3000)))

(add-examples tinter
  (example-palette "Input palette" (palette [:red :yellow] 10))
  (example-palette "Make any color red" (map (tinter [255 55 55]) (palette [:red :yellow] 10)))
  (example-palette "Tint using gradient" (map (tinter (gradient [:red :yellow :blue])) (palette [:red :yellow] 10))))

;; --

(add-examples to-linear
  (example "Gamma correction" (to-linear 0.5)))

(add-examples from-linear
  (example "Gamma correction" (from-linear 0.5)))

(add-examples colorspaces
  (example-color "Example conversion" (let [[to from] (colorspaces :Cubehelix)]
                                        (mapv m/approx (from (to [100 200 50]))))))

(add-examples colorspaces*
  (example-color "Example conversion" (let [[to from] (colorspaces* :Cubehelix)]
                                        (mapv m/approx (from (to [100 200 50]))))))

;;

(add-examples b/blend-colors
  (example-color "Multiply colors" (b/blend-colors b/multiply :brown :khaki))
  (example-color "Modulo add colors" (b/blend-colors b/madd :brown :khaki))
  (example-color "Hardmix colors" (b/blend-colors b/hardmix :brown :khaki)))

(def i1 (p/load-pixels "docs/cockatoo.jpg"))
(def i2 (p/load-pixels "docs/cockatoo1.jpg"))

(defsnippet clojure2d.color blend-images
  "Compose two cockatoo images (one is rotated) with given blend method." true
  (let [n (str "images/color/" (first opts) ".jpg")]
    (save (p/compose-channels f false i1 i2) (str "docs/" n))
    (str "../" n)))

(defmacro add-blends-examples
  []
  `(do ~@(for [x b/blends-list]
           (let [n (symbol (str "b/" (name x)))]
             `(add-examples ~n
                (example-snippet "Composed images" blend-images :image ~x)
                (example-session "Simple calls" (~n 50 200) (~n 80 120) (~n 170 220))
                (example-palette "Blend two colors" [:salmon :mediumturquoise (b/blend-colors ~n :salmon :mediumturquoise)])
                (example-palette "Blend two palettes" (b/blend-palettes ~n (palette 5) (palette 35)))
                (example-gradient "Blend two gradients" (b/blend-gradients ~n (gradient (palette 5)) (gradient (palette 35)))))))))

(add-blends-examples)

(add-examples b/blends
  (example-session "Access" (b/blends :mdodge) ((b/blends :mdodge) 0.4 0.3)))

(add-examples b/blends-list
  (example "List of blends" b/blends-list))

(add-examples b/blend-palettes
  (example-palette "First palette" (palette 5))
  (example-palette "Second palette" (palette 35))
  (example-palette "Blend two palettes using burn mode" (b/blend-palettes b/burn (palette 5) (palette 35)))
  (example-palette "Blend using different modes for each channel" (b/blend-palettes b/burn b/divide b/difference (palette 5) (palette 35))))

(add-examples b/blend-gradients
  (example-gradient "First gradient" (gradient (palette 5)))
  (example-gradient "Second gradient" (gradient (palette 35)))
  (example-gradient "Blend two gradients using burn mode" (b/blend-gradients b/burn (gradient (palette 5)) (gradient (palette 35))))
  (example-gradient "Blend using different modes for each channel" (b/blend-gradients b/burn b/divide b/difference (gradient (palette 5)) (gradient (palette 35)))))

(defmacro add-colorspace-examples
  []
  `(do ~@(for [n colorspaces-list]
           (let [n-to (symbol (str "to-" (name n)))]
             `(do (add-examples ~n-to
                    (example-gradient "Gradient between four colors using given color space." (gradient [:maroon :white :black :lightcyan] {:colorspace ~n}))
                    (example "Convert into color space value" (~n-to :peru))))))))

(defmacro add-colorspace*-examples
  []
  `(do ~@(for [n colorspaces-list]
           (let [n-to (symbol (str "to-" (name n) "*"))]
             `(do (add-examples ~n-to
                    (example "Convert into color space value" (~n-to :peru))))))))

(add-colorspace-examples)
(add-colorspace*-examples)

(defmacro add-colorspace*-from-examples
  []
  `(do ~@(for [n colorspaces-list]
           (let [n-from (symbol (str "from-" (name n) "*"))
                 v (symbol "v")]
             `(do (add-examples ~n-from
                    (example-gradient "Gradient generated from given color space." (fn [~v] (~n-from (lerp [255 255 255] [0 0 0] ~v))))))))))

(add-colorspace*-from-examples)

(defmacro add-colorspace-from-examples
  []
  `(do ~@(for [n colorspaces-list]
           (let [n-from (symbol (str "from-" (name n)))
                 n-to (symbol (str "to-" (name n)))]
             `(do (add-examples ~n-from
                    (example-color "Converting to and from given color space should yield almost the same color." (lclamp (~n-from (~n-to  "#7a03fe"))))))))))

(add-colorspace-from-examples)

(add-examples colorspaces-list
  (example "List of color spaces" colorspaces-list))

;;

(add-examples color-converter
  (example-session "When you pass only color space, returns normalized from-XXX* function."
    (color-converter :YUV)
    ((color-converter :YUV) [158 94 85]))
  (example-session "You can set maximum value for all channels or individually."
    ((color-converter :HCL 1.0) [0.5 0.5 0.5 0.5])
    ((color-converter :HCL 100.0) [50 50 50 50])
    ((color-converter :HCL 10 20 30) [5 10 15 127.5])
    ((color-converter :HCL 10 20 30 40) [5 10 15 20]))
  (example-color "You can set maximum value for each channel separately. Here makes conversion from HSL color space where hue is from range 0-360, saturation and lightness from 0-100. Alpha is from range 0-255."
                 ((color-converter :HCL 360.0 100.0 100.0) [240 50 50])))

;;

(add-examples palette
  (example-palette "Named palette" (palette :set3))
  (example-palette "Colourlovers palette" (palette 0))
  (example-palette "Resampled palette" (palette 0 10))
  (example-palette "Resampled palette with interpolation and color space" (palette 0 10 {:colorspace :LUV
                                                                                         :interpolation :loess}))
  (example-palette "Sampled from gradient" (palette (gradient :cyan-magenta)))
  (example-palette "Sampled from gradient with number of colors" (palette (gradient :cyan-magenta) 10))
  (example "List of named palettes" (list (palette))))

(add-examples iq-gradient
  (example-gradient "Create gradient from 4 coeffs"
                    (iq-gradient
                     [0.5 0.5 0.5]
                     [0.4 0.5 0.6]
                     [0.2 0.2 1.0]
                     [1.0 0.1 1.0]))
  (example-gradient "Create gradient from two colors"
                    (iq-gradient :red :blue)))

;; ----

(add-examples paletton-presets-list
  (example "List of all paletton presets" paletton-presets-list))

(add-examples hue-paletton
  (example-session "Convert RGB to paletton hue."
    (hue :amber)
    (hue-polar :amber)
    (hue-paletton :amber)
    (hue-paletton 22 33 123)))

(add-examples paletton
  (example-palette "Monochromatic dark-neon palette" (paletton :monochromatic 140 {:preset :dark-neon}))
  (example-palette "Monochromatic full (default) palette" (paletton :monochromatic 140))
  (example-palette "Monochromatic shiny palette with complementary color" (paletton :monochromatic 300 {:preset :shiny :compl true}))
  (example-palette "Triad full palette, angle 30" (paletton :triad 120))
  (example-palette "Triad palette, angle 10 with complementary" (paletton :triad 120 {:preset :pastels-dark :angle 10 :compl true}))
  (example-palette "Triad palette, angle 10 with complementary, not adjacent version" (paletton :triad 120 {:adj false :preset :pastels-dark :angle 10 :compl true}))
  (example-palette "Tetrad palette" (paletton :tetrad 20 {:preset :pastels-darkest}))
  (example-palette "Tetrad palette, bigger angle " (paletton :tetrad 20 {:angle 100 :preset :pastels-darkest})))

;; ----

(add-examples delta-c
  (example "Distance between colors" (delta-c :maroon :amber)))

(add-examples delta-h
  (example "Distance between colors" (delta-h :maroon :amber)))

(add-examples delta-e-cie
  (example "Distance between colors" (delta-e-cie :maroon :amber)))

(add-examples delta-e-jab
  (example "Distance between colors" (delta-e-jab :maroon :amber)))

(add-examples delta-e-cmc
  (example "Distance between colors" (delta-e-cmc :maroon :amber)))

(add-examples euclidean
  (example "Distance between colors" (euclidean :maroon :amber)))

(add-examples contrast-ratio
  (example "Contrast ratio" (contrast-ratio :pink :hotpink))
  (example "Contrast ratio" (contrast-ratio :pink :purple)))

(add-examples noticable-different?
  (example "Contrast ratio" (noticable-different? :pink :hotpink))
  (example "Contrast ratio" (noticable-different? "00aabb" "00baba")))

(def some-palette (paletton :triad 210 {:angle 40}))

(add-examples nearest-color
  (example-color "Find nearest color to given color from below palette." [120 0 80])
  (example-palette "All below examples are using this palette" some-palette)
  (example-color "With Delta C" (nearest-color some-palette [120 0 80] delta-c))
  (example-color "With Delta H" (nearest-color some-palette [120 0 80] delta-h))
  (example-color "With Delta E CIE" (nearest-color some-palette [120 0 80] delta-e-cie))
  (example-color "With Delta E CMC" (nearest-color some-palette [120 0 80] delta-e-cmc))
  (example-color "With euclidean" (nearest-color some-palette [120 0 80])))

(add-examples reduce-colors
  (example-palette "Reduce cockatoo image palette (2 colors)" (reduce-colors i1 2))
  (example-palette "Reduce cockatoo image palette in LAB (2 colors)" (reduce-colors i1 2 :LAB))
  (example-palette "Reduce cockatoo image palette (6 colors)" (reduce-colors i1 6))
  (example-palette "Reduce cockatoo image palette in LAB (6 colors)" (reduce-colors i1 6 :LAB))
  (example-palette "Reduce cockatoo image palette (15 colors)" (reduce-colors i1 15))
  (example-palette "Reduce cockatoo image palette in LAB (15 colors)" (reduce-colors i1 15 :LAB)))

(add-examples darken
  (example-palette "Make palette" (take 10 (iterate (fn [c] (darken c 0.5)) :amber)))
  (example-color "Given color" :khaki)
  (example-color "Darken" (darken :khaki 2.0)))

(add-examples brighten
  (example-palette "Make palette" (take 10 (iterate (fn [c] (brighten c 0.5)) "03100f")))
  (example-color "Given color" :khaki)
  (example-color "Brighten" (brighten :khaki 0.5)))

(add-examples saturate
  (example-palette "Make palette" (take 10 (iterate saturate (from-HSL (color 300 0.0 0.5))))))

(add-examples desaturate
  (example-palette "Make palette" (take 10 (iterate desaturate (from-HSL (color 300 1.0 0.5))))))

;; ----

(add-examples average
  (example-color "Average in RGB" (average [:yellow :red :teal "#dddddd"]))
  (example-color "Average in LAB" (average [:yellow :red :teal "#dddddd"] :LAB))
  (example-color "Average in HSL" (average [:yellow :red :teal "#dddddd"] :LCH)))

(add-examples mix
  (example-color "Mix" (mix :red :blue))
  (example-gradient "As gradient" (partial mix :red :blue))
  (example-color "Mix, different ratio" (mix :red :blue 0.25))
  (example-color "Mix in LAB" (mix :red :blue :LAB 0.5))
  (example-color "Mix in HSI" (mix :red :blue :HSI 0.5)))

;; ----

(add-examples gradient
  (example-gradient "Named gradient" (gradient :rainbow-m))
  (example-gradient "Linear, RGB" (gradient (palette 5)))
  (example-gradient "Linear, HSL" (gradient (palette 5) {:colorspace :HSL}))
  (example-gradient "Linear, Yxy" (gradient (palette 5) {:colorspace :Yxy}))
  (example-gradient "Cubic, Yxy" (gradient (palette 5) {:colorspace :Yxy :interpolation :cubic-spline}))
  (example-gradient "Loess, Yxy" (gradient (palette 5) {:colorspace :Yxy :interpolation :loess}))
  (example-gradient "RBF, Yxy" (gradient (palette 5) {:colorspace :Yxy :interpolation (partial i/rbf (rbf/rbf :thin-plate))}))
  (example-gradient "B-Spline for smoother colors in LAB" (gradient (palette 5) {:colorspace :LAB :interpolation :b-spline}))
  (example-gradient "Shepard, Yxy, irregular spacing" (gradient (palette 5) {:colorspace :Yxy :interpolation :shepard :domain [0 0.1 0.15 0.8 1.0]}))
  (example-gradient "Easing functions as interpolator" (gradient [[-120 0.1 0.2] [120 0.3 1.0]] {:interpolation :bounce-in-out
                                                                                                 :to? false
                                                                                                 :colorspace :HSL}))
  (example-palette "Easy way to create palette from gradient" (palette (gradient [:blue :green] {:colorspace :HSL
                                                                                                 :interpolation :cubic-spline}) 10))
  (example "List of ready to use gradients" (sort (gradient))))

(add-examples gradient-cubehelix
  (example-gradient "Cubehelix gradient" (gradient-cubehelix [[300 0.2 0.2] [200 0.8 0.9]])))

(add-examples resample
  (example-palette "Input palette" (palette 12))
  (example-palette "Resample one of the colourlovers palette." (resample (palette 12) 16))
  (example-palette "Resample one of the colourlovers palette. Different gradient settings."
                   (resample (palette 12) 16 {:colorspace :LUV
                                              :interpolation :cubic-spline})))

;; ----

(add-examples gradient
  (example-gradient "Cool" (gradient :cool))
  (example-gradient "Warm" (gradient :warm))
  (example-gradient "Cubehelix" (gradient :cubehelix))
  (example-gradient "IQ 1" (gradient :iq-1)))

(add-examples named-colors-list
  (example "List of color names" (named-colors-list)))

(add-examples correct-luma
  (example-gradient "Before correction" (gradient [:black :red :yellow :white]))
  (example-gradient "After correction" (correct-luma (gradient [:black :red :yellow :white])))
  (example-palette "No correction" (palette [:lightyellow :orangered :deeppink :darkred] 9))
  (example-palette "B-Spline interpolation" (palette [:lightyellow :orangered :deeppink :darkred] 9 {:colorspace :LAB
                                                                                                     :interpolation :b-spline}))
  (example-palette "Lightness correction" (correct-luma (palette [:lightyellow :orangered :deeppink :darkred] 9)))
  (example-palette "Both" (correct-luma (palette [:lightyellow :orangered :deeppink :darkred] 9 {:colorspace :LAB
                                                                                                 :interpolation :b-spline}))))

(add-examples adjust-temperature
  (example-palette "Without adjustment" (palette 0))
  (example-palette "Cool" (adjust-temperature (palette 0) :cool 0.5))
  (example-palette "Warm" (adjust-temperature (palette 0) :warm 0.5))
  (example-gradient "Without adjustment" (gradient [:white :black]))
  (example-gradient "Cold" (adjust-temperature (gradient [:white :black]) 10000))
  (example-gradient "Hot" (adjust-temperature (gradient [:white :black]) 1000))
  (example-color "Adjust color" (adjust-temperature :red 30000)))

;;

(add-examples random-gradient
  (example-gradient "Randomly generated gradient" (random-gradient))
  (example-gradient "Randomly generated gradient" (random-gradient))
  (example-gradient "Randomly generated gradient" (random-gradient)))

(add-examples random-palette
  (example-palette "Randomly generated palette" (random-palette))
  (example-palette "Randomly generated palette" (random-palette))
  (example-palette "Randomly generated palette" (random-palette)))

(add-examples random-color
  (example-color "Random color" (random-color))
  (example-color "Random color (with alpha set)" (random-color 180)))
