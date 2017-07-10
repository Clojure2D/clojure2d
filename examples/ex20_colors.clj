(ns examples.ex20-colors
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [clojure2d.math.vector :as v]
            [clojure2d.math :as m])
  (:import [clojure2d.math.vector Vec4]
           [clojure2d.pixels Pixels]))

;; reduce colors to random palette

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^Pixels img (p/load-pixels "results/test.jpg"))

(def canvas (make-canvas (width img) (height img)))
(def window (show-window canvas "Colors" (width img) (height img) 15))

(defmethod key-pressed ["Colors" \space] [_]
  (save-canvas canvas (next-filename "results/ex20/" ".jpg")))

;; colourlovers
(p/set-canvas-pixels! canvas (p/filter-colors (c/make-reduce-color-filter) img))

;; generated palette with 8 colors
(def random-palette-8 (c/make-random-palette 8))
(p/set-canvas-pixels! canvas (p/filter-colors (c/make-reduce-color-filter random-palette-8) img))

;; different distance function
(def random-palette-6 (conj (c/make-random-palette 4) (Vec4. 0 0 0 255.0) (Vec4. 255 255 255 255)))
(p/set-canvas-pixels! canvas (p/filter-colors (c/make-reduce-color-filter v/dist random-palette-6) img))
(p/set-canvas-pixels! canvas  (p/filter-colors (c/make-reduce-color-filter v/dist-abs random-palette-6) img))
(p/set-canvas-pixels! canvas  (p/filter-colors (c/make-reduce-color-filter v/dist-cheb random-palette-6) img))

(do
  (def paletton-palette (c/paletton-palette :triad 0 {:compl true :angle 20 :preset (rand-nth c/paletton-presets-names)}))
  (p/set-canvas-pixels! canvas  (p/filter-colors (c/make-reduce-color-filter paletton-palette) img)))
