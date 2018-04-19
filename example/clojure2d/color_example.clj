(ns clojure2d.color-example
  (:require [hiccup.core :refer :all]
            [metadoc.examples :refer :all]
            [clojure2d.color :refer :all]
            [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [fastmath.vector :as v]))

(defmacro example-color [description ex]
  `(assoc (example ~description ~ex) :type :color))

(defmacro example-palette [description ex]
  `(assoc (example ~description ~ex) :type :palette))

(defmacro example-gradient [description ex]
  `(assoc (example ~description ~ex) :type :gradient))

(defn color->html [c]
  (let [str-color (if (< (luma c) 128.0) "white" "black")
        o (/ (alpha c) 255.0)
        n (format-css c)]
    (html [:span {:style (str "display:block;opacity:" o ";padding:5px;background-color:" n ";color:" str-color ";")} (str "[" n "] " c)])))

(defn palette->html [c]
  (html (for [col c]
          [:span {:style (str "border:1px solid;margin-right:1px;padding:6px;background-color:" (format-css col) ";border-radius:5px;")} "  "])))

(defn gradient->html [f] 
  (html (for [^double t (range 0.0 1.0 0.015)]
          [:span {:style (str "background-color:" (format-css (f t)) ";width:1%;display:inline-block;height:20px;")}])))

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

(add-examples format-css
  (example-session "Usage" (format-css :maroon) (format-css (color 4 55 222))))

;; --

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
             `(add-examples ~n (example-snippet "Composed images" blend-images :image ~x))))))

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
                    (example-gradient "Gradient between four colors using given colorspace." (gradient [:maroon :white :black :lightcyan] ~n))
                    (example "Convert into colorspace value" (~n-to :peru))))))))

(defmacro add-colorspace*-examples
  []
  `(do ~@(for [n colorspaces-list]
           (let [n-to (symbol (str "to-" (name n) "*"))]              
             `(do (add-examples ~n-to
                    (example "Convert into colorspace value" (~n-to :peru))))))))

(add-colorspace-examples)
(add-colorspace*-examples)


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


