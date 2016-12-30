(ns examples.ex20-colors
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [clojure2d.math.vector :as v]
            [clojure2d.math :as m])
  (:import [clojure2d.math.vector Vec4]))

;; reduce colors to random palette

(def img (p/load-pixels "results/test.jpg"))

;; colourlovers
(p/save-pixels (p/filter-colors (c/make-reduce-color-filter) img) "results/ex20/colourlovers.jpg")

;; generated palette with 8 colors
(def random-palette-8 (c/make-random-palette 8))
(p/save-pixels (p/filter-colors (c/make-reduce-color-filter random-palette-8) img) "results/ex20/generated.jpg")

;; different distance function
(def random-palette-6 (conj (c/make-random-palette 4) (Vec4. 0 0 0 255.0) (Vec4. 255 255 255 255)))
(p/save-pixels (p/filter-colors (c/make-reduce-color-filter v/dist random-palette-6) img) "results/ex20/euclid.jpg")
(p/save-pixels (p/filter-colors (c/make-reduce-color-filter v/dist-abs random-palette-6) img) "results/ex20/abs.jpg")
(p/save-pixels (p/filter-colors (c/make-reduce-color-filter v/dist-cheb random-palette-6) img) "results/ex20/cheb.jpg")

(do
  (def paletton-palette (c/paletton-palette :triad 0 {:compl true :angle 20 :preset :shiny}))
  (p/save-pixels (p/filter-colors (c/make-reduce-color-filter paletton-palette) img) "results/ex20/paletton.jpg"))
