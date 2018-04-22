(ns clojure2d.color-example
  (:require [hiccup.core :refer :all]
            [metadoc.examples :refer :all]
            [clojure2d.color :refer :all]
            [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [fastmath.easings :as e]
            [fastmath.interpolation :as i]
            [fastmath.rbf :as rbf]))

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

(add-examples clamp255
  (example-session "Usage" (clamp255 -1) (clamp255 255.1) (clamp255 123.5)))

(add-examples lclamp255
  (example-session "Usage" (lclamp255 -1) (lclamp255 255.1) (lclamp255 123.5)))

(add-examples to-color
  (example-session "Various conversions"
    (to-color :khaki)
    (to-color "abcc12")
    (to-color "#1234af")
    (to-color (v/vec3 44 33.3 22))
    (to-color (v/vec4 11 22 33 44))
    (to-color [3 4 5])
    (to-color (range 4))
    (to-color 0xaafffeeb)))

(add-examples to-awt-color
  (example-session "Various conversions"
    (to-awt-color :khaki)
    (to-awt-color 0xaa112233)))

(add-examples luma (example-session "Usage" (luma 0x152535) (luma :red)))
(add-examples red (example-session "Usage" (red 0x152535) (red :khaki)))
(add-examples green (example-session "Usage" (green 0x152535) (green :khaki)))
(add-examples blue (example-session "Usage" (blue 0x152535) (blue :khaki)))
(add-examples alpha (example-session "Usage" (alpha 0x05152535) (alpha :khaki)))
(add-examples ch0 (example-session "Usage" (ch0 0x152535) (ch0 :khaki)))
(add-examples ch1 (example-session "Usage" (ch0 0x152535) (ch1 :khaki)))
(add-examples ch2 (example-session "Usage" (ch0 0x152535) (ch2 :khaki)))

(add-examples hue (example-session "Usage" (hue :red) (hue :green) (hue :blue) (hue "#12f5e6") (hue-polar "#12f5e6")))
(add-examples hue-polar (example-session "Usage" (hue-polar "#4455f6") (hue "#4455f6")))

(add-examples lerp (example-palette "Usage" [(lerp :red :blue 0) (lerp :red :blue 0.5) (lerp :red :blue 1.0)]))

(add-examples set-alpha (example-color "Usage" (set-alpha :khaki 200)))
(add-examples set-ch0 (example-color "Usage" (set-ch0 :khaki 11)))
(add-examples set-ch1 (example-color "Usage" (set-ch1 :khaki 11)))
(add-examples set-ch2 (example-color "Usage" (set-ch2 :khaki 11)))
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
  (example "Usage" (clamp (v/vec4 304 -123 3.4 22.9))))

(add-examples lclamp
  (example "Usage" (lclamp (v/vec4 304 -123 3.4 22.9))))

(add-examples format-hex
  (example-session "Usage" (format-hex :maroon) (format-hex (color 4 55 222))))

;; --

(add-examples to-linear
  (example "Gamma correction" (to-linear 0.5)))

(add-examples from-linear
  (example "Gamma correction" (from-linear 0.5)))

(add-examples colorspaces
  (example-color "Example conversion" (let [[to from] (colorspaces :Cubehelix)]
                                        (map m/approx (from (to [100 200 50]))))))

(add-examples colorspaces*
  (example-color "Example conversion" (let [[to from] (colorspaces* :Cubehelix)]
                                        (map m/approx (from (to [100 200 50]))))))

;;

(add-examples *blend-threshold*
  (example-session "Usage"
    (blend-opacity 0.23 0.88)
    (binding [*blend-threshold* 0.9]
      (blend-opacity 0.23 0.88))
    (blend-opacity 0.23 0.88 0.9)))

(add-examples blend-values
  (example-session "Usage" (blend-values blend-multiply 123 44) (blend-values blend-add 123 44)))

(add-examples blend-colors
  (example-color "Multiply colors" (blend-colors blend-multiply :brown :khaki))
  (example-color "Modulo add colors" (blend-colors blend-madd :brown :khaki)))

(def i1 (p/load-pixels "docs/cockatoo.jpg"))
(def i2 (p/load-pixels "docs/cockatoo1.jpg"))

(defsnippet clojure2d.color blend-images
  "Compose two cockatoo images (one is rotated) with given blend method."
  (let [n (str "images/color/" (first opts) ".jpg")]
    (save (p/compose-channels f false i1 i2) (str "docs/" n))
    (str "../" n)))

(defmacro add-blends-examples
  []
  `(do ~@(for [x blends-list]
           (let [n (symbol (str "blend-" (name x)))]
             `(add-examples ~n
                (example-snippet "Composed images" blend-images :image ~x)
                (example-session "Simple calls" (~n 0.43 0.68) (~n 0.22 0.44))
                (example-palette "Blend two colors" [:salmon :mediumturquoise (blend-colors ~n :salmon :mediumturquoise)]))))))

(add-blends-examples)

(add-examples blends
  (example-session "Access" (blends :mdodge) ((blends :mdodge) 0.4 0.3)))

(add-examples blends-list
  (example "List of blends" blends-list))

(defmacro add-colorspace-examples
  []
  `(do ~@(for [n colorspaces-list]
           (let [n-to (symbol (str "to-" (name n)))]
             `(do (add-examples ~n-to
                    (example-gradient "Gradient between four colors using given color space." (gradient ~n [:maroon :white :black :lightcyan]))
                    (example "Convert into colorspace value" (~n-to :peru))))))))

(defmacro add-colorspace*-examples
  []
  `(do ~@(for [n colorspaces-list]
           (let [n-to (symbol (str "to-" (name n) "*"))]
             `(do (add-examples ~n-to
                    (example "Convert into colorspace value" (~n-to :peru))))))))

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
                    (example-color "Converting to and from given colorspace should yield almost the same color." (map m/approx ((juxt red green blue) (~n-from (~n-to (color 122 3 254))))))))))))

(add-colorspace-from-examples)

(add-examples colorspaces
  (example "List of colorspaces" colorspaces-list))

;;

(add-examples color-converter
  (example-session "When you pass only color space, returns normalized from-XXX* function."
    (color-converter :YUV)
    ((color-converter :YUV) [158 94 85]))
  (example-session "You can maximum value for all channels."
    ((color-converter :HCL 1.0) [0.5 0.5 0.5 0.2])
    ((color-converter :HCL 100.0) [50 50 50 100]))
  (example-color "You can set maximum value for each channel separately. Here makes conversion from HSL color space where hue is from range 0-360, saturation and lightness from 0-100. Alpha is from range 0-255."
                 ((color-converter :HCL 360.0 100.0 100.0) [240 50 50])))

;;

(add-examples colourlovers-palettes
  (example "Number of palettes" (count colourlovers-palettes))
  (example-palette "Palette 0" (colourlovers-palettes 0))
  (example-palette "Palette 40" (colourlovers-palettes 40))
  (example-palette "Palette 101" (colourlovers-palettes 101))
  (example-palette "Palette 201" (colourlovers-palettes 201))
  (example-palette "Palette 499" (colourlovers-palettes 499)))

(add-examples iq-palette-gradient
  (example-gradient "Create gradient"
                    (iq-palette-gradient
                     (v/vec3 0.5 0.5 0.5)
                     (v/vec3 0.4 0.5 0.6)
                     (v/vec3 0.2 0.2 1.0)
                     (v/vec3 1.0 0.1 1.0))))

(add-examples iq-palette-random-gradient
  (example-gradient "Create gradient" (iq-palette-random-gradient))
  (example-gradient "Create another gradient" (iq-palette-random-gradient)))

;; ----

(add-examples paletton-presets-list
  (example "List of all paletton presets" paletton-presets-list))

(add-examples paletton-rgb-to-hue
  (example-session "Convert RGB to paletton hue."
    (hue :amber)
    (paletton-rgb-to-hue :amber)
    (paletton-rgb-to-hue 22 33 123)))

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
  (example-color "With Delta C" (nearest-color delta-c some-palette [120 0 80]))
  (example-color "With Delta H" (nearest-color delta-h some-palette [120 0 80]))
  (example-color "With Delta E CIE" (nearest-color delta-e-cie some-palette [120 0 80]))
  (example-color "With Delta E CMC" (nearest-color delta-e-cmc some-palette [120 0 80]))
  (example-color "With euclidean" (nearest-color euclidean some-palette [120 0 80])))

;; ----


(add-examples change-lab-luma
  (example-color "Given color" :khaki)
  (example-color "Lighten by 30" (change-lab-luma 30 :khaki))
  (example-color "Darken by 50" (change-lab-luma -50 :khaki)))

(add-examples darken
  (example-palette "Make palette" (take 10 (iterate darken :amber))))

(add-examples brighten
  (example-palette "Make palette" (take 10 (iterate brighten "03100f"))))

(add-examples change-saturation
  (example-color "Given color" :khaki)
  (example-color "Saturate by 30" (change-saturation 30 :khaki))
  (example-color "Desaturate by 30" (change-saturation -30 :khaki)))

(add-examples saturate
  (example-palette "Make palette" (take 10 (iterate saturate (from-HSL (color 300 0.0 0.5))))))

(add-examples desaturate
  (example-palette "Make palette" (take 10 (iterate desaturate (from-HSL (color 300 1.0 0.5))))))

;; ----

(add-examples average
  (example-color "Average in RGB" (average [:yellow :red :teal "#dddddd"]))
  (example-color "Average in LAB" (average :LAB [:yellow :red :teal "#dddddd"]))
  (example-color "Average in HSL" (average :LCH [:yellow :red :teal "#dddddd"])))

(add-examples mix
  (example-color "Mix" (mix :red :blue))
  (example-color "Mix, different ratio" (mix :red :blue 0.25))
  (example-color "Mix in LAB" (mix :LAB :red :blue 0.5))
  (example-color "Mix in HSI" (mix :HSI :red :blue 0.5)))

;; ----

(add-examples gradient
  (example-gradient "Linear, RGB" (gradient (colourlovers-palettes 5)))
  (example-gradient "Linear, HSL" (gradient :HSL (colourlovers-palettes 5)))
  (example-gradient "Linear, Yxy" (gradient :Yxy (colourlovers-palettes 5)))
  (example-gradient "Cubic, Yxy" (gradient :Yxy :cubic-spline (colourlovers-palettes 5)))
  (example-gradient "Loess, Yxy" (gradient :Yxy :loess (colourlovers-palettes 5)))
  (example-gradient "RBF, Yxy" (gradient :Yxy (partial i/rbf (rbf/rbf :thinplate)) (colourlovers-palettes 5)))
  (example-gradient "Shepard, Yxy, irregular spacing" (gradient :Yxy :shepard [0 0.1 0.15 0.8 1.0] (colourlovers-palettes 5)))
  (example-palette "Easy way to create palette from gradient" (m/sample (gradient :HSL :cubic-spline [:blue :green]) 10)))

(add-examples gradient-easing
  (example-gradient "Linear, HCL" (gradient-easing :HCL [300 0.2 0.2] [200 0.8 0.9]))
  (example-gradient "Bounce in-out, HCL" (gradient-easing :HCL e/bounce-in-out [300 0.2 0.2] [200 0.8 0.9])))

(add-examples gradient-cubehelix
  (example-gradient "Cubehelix gradient" (gradient-cubehelix [300 0.2 0.2] [200 0.8 0.9])))

(add-examples resample
  (example-palette "Input palette" (colourlovers-palettes 12))
  (example-palette "Resample one of the colourlovers palette." (resample 16 (colourlovers-palettes 12)))
  (example-palette "Resample one of the colourlovers palette. Different gradient settings."
                   (resample 16 (colourlovers-palettes 12) :LUV :cubic-spline)))
;; ----

(add-examples gradient-presets-list  (example "List of ready to use gradients" gradient-presets-list))

(add-examples gradient-presets
  (example-gradient "Cool" (:cool gradient-presets))
  (example-gradient "Warm" (:warm gradient-presets))
  (example-gradient "Cubehelix" (:cubehelix gradient-presets))
  (example-gradient "Rainbow" (:rainbow gradient-presets))
  (example-gradient "IQ 1" (:iq-1 gradient-presets))
  (example-gradient "IQ 2" (:iq-2 gradient-presets))
  (example-gradient "IQ 3" (:iq-3 gradient-presets))
  (example-gradient "IQ 4" (:iq-4 gradient-presets))
  (example-gradient "IQ 5" (:iq-5 gradient-presets))
  (example-gradient "IQ 6" (:iq-6 gradient-presets))
  (example-gradient "IQ 7" (:iq-7 gradient-presets)))

(add-examples palette-presets
  (example-palette "`:prgn-10`" (palette-presets :prgn-10))
  (example-palette "`:rdylbu-9`" (palette-presets :rdylbu-9))
  (example-palette "`:spectral-11`" (palette-presets :spectral-11))
  (example-palette "`:oranges-9`" (palette-presets :oranges-9))
  (example-palette "`:accent`" (palette-presets :accent))
  (example-palette "`:dark2`" (palette-presets :dark2))
  (example-palette "`:set3`" (palette-presets :set3))
  (example-palette "`:category-20c`" (palette-presets :category20c))
  (example-palette "`:viridis`" (resample 16 (palette-presets :viridis)))
  (example-palette "`:microsoft-3`" (resample 16 (palette-presets :microsoft-3)))
  (example-palette "`:tableau-20-2`" (resample 16 (palette-presets :tableau-20-2))))

(add-examples palette-presets-list
  (example "List of palettes" palette-presets-list))

(add-examples html-colors-list
  (example "List of html color names" html-colors-list))


;;
