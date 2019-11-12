(ns clojure2d.colors-gen-example
  (:require [hiccup.core :refer :all]
            [hiccup.page :as p]
            [clojure2d.color :refer :all]))

(defn palette-spans
  [c]
  (html (for [col c]
          [:span.pal {:style (str "background-color:" (format-hex col) ";")} "  "])))

(defn gradient-spans [f]
  (html (for [^double t (range 0.0 1.0 0.015)]
          [:span.grad {:style (str "background-color:" (format-hex (f t)) ";")}])))

(defn color-spans [c]
  (let [str-color (if (< (luma c) 128.0) "white" "black")
        n (format-hex c)]
    (html [:span.color {:style (str "background-color:" n ";color:" str-color ";")} (str "[" n "] " c)])))


(spit "docs/static/palettes.html"
      (p/html5
       (p/include-css "static.css")
       (for [k (sort (keys palette-presets))]
         [:pre.pal
          [:div [:span.name (name k)] (palette-spans (palette-presets k))]])))

(spit "docs/static/colourlovers.html"
      (p/html5
       (p/include-css "static.css")
       (for [[i p] (map-indexed vector colourlovers-palettes)]
         [:pre.pal
          [:div [:span.name i] (palette-spans p)]])))

(spit "docs/static/gradients.html"
      (p/html5
       (p/include-css "static.css")
       (for [k (sort (keys gradient-presets))]
         [:pre.grad
          [:div [:span.name (name k)] (gradient-spans (gradient-presets k))]])))

(spit "docs/static/colors.html"
      (p/html5
       (p/include-css "static.css")
       (for [k named-colors-list]
         [:pre.color
          [:div (color-spans k)]])))
