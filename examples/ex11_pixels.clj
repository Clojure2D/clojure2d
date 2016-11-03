;; filtering and blending pixels

(ns examples.ex11-pixels
  (:require [clojure2d.core :as core]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; first let's load an image into ^Pixels type
;; pixels are layout in planar mode (first red channel, then green, blue and alpha)
(def img (core/load-pixels "results/test.jpg"))

;; now let's process pixels with defined pixels

;; dilate three times on 3 channels, skip alpha
(core/save-pixels (->> img 
                      (p/filter-channels p/dilate-filter false)
                      (p/filter-channels p/dilate-filter false)
                      (p/filter-channels p/dilate-filter false))
                  "results/ex11/dilate.jpg")

;; erode three times on 3 channels, skip alpha
(core/save-pixels (->> img 
                      (p/filter-channels p/erode-filter false)
                      (p/filter-channels p/erode-filter false)
                      (p/filter-channels p/erode-filter false))
                  "results/ex11/erode.jpg")

;; box blur with radius 5
(core/save-pixels (p/filter-channels p/box-blur-5 img)
                  "results/ex11/boxblur5.jpg")

;; box blur with radius 23
(core/save-pixels (p/filter-channels (p/make-box-blur 23) img)
                  "results/ex11/boxblur23.jpg")

;; gaussian blur with radius 5
(core/save-pixels (p/filter-channels p/gaussian-blur-5 img)
                  "results/ex11/gaussianblur5.jpg")

;; posterize 4, to give number of levels call (p/make-posterize 2)
(core/save-pixels (p/filter-channels p/posterize-4 img)
                  "results/ex11/posterize2.jpg")

;; threshold all channels but alpha (50%), custom threshold (p/make-threshold 44)
(core/save-pixels (p/filter-channels p/threshold-50 false img)
                  "results/ex11/threshold50.jpg")

;; median filter five times (slow!)
(core/save-pixels (->> img 
                      (p/filter-channels p/median-filter false)
                      (p/filter-channels p/median-filter false)
                      (p/filter-channels p/median-filter false)
                      (p/filter-channels p/median-filter false)
                      (p/filter-channels p/median-filter false))
                  "results/ex11/median.jpg")

;; quntile filter 10 times (slow!!!), you can select quantile 0-8, 4 = median filter
(def quantile-filter (p/make-quantile-filter 3))
(core/save-pixels (reduce (fn [res _] (p/filter-channels quantile-filter false res)) img (range 10))
                  "results/ex11/quantile3.jpg")

;; tint image
(core/save-pixels (p/filter-channels (p/make-tint-filter 10 30 200) img) "results/ex11/tint.jpg")

;; let's compose some images
;; list of all composite methods is here
(def all-blends (keys c/blends))

;; let's modulo add two images
(core/save-pixels (p/compose-channels :madd false 
                                      (core/load-pixels "results/ex11/erode.jpg")
                                      (core/load-pixels "results/ex11/boxblur23.jpg"))
                  "results/ex11/divide.jpg")

;; let's make random blends
(core/save-pixels (p/compose-channels (rand-nth all-blends) (rand-nth all-blends) (rand-nth all-blends) nil 
                                      (core/load-pixels "results/ex11/erode.jpg")
                                      (core/load-pixels "results/ex11/boxblur23.jpg"))
                  "results/ex11/random_blend.jpg")
