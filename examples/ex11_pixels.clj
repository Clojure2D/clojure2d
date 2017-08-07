;; filtering and blending pixels

(ns examples.ex11-pixels
  (:require [clojure2d.core :as core]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c])
  (:import [clojure2d.pixels Pixels]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; first let's load an image into ^Pixels type
;; pixels are layout in planar mode (first red channel, then green, blue and alpha)
(def ^Pixels img (p/load-pixels "results/test.jpg"))

(def canvas (core/make-canvas (core/width img) (core/height img)))
(def window (core/show-window canvas "Pixels" 15 nil))

(defmethod core/key-pressed ["Pixels" \space] [_ _]
  (core/save-canvas canvas (core/next-filename "results/ex11/" ".jpg")))

;; now let's process pixels with defined pixels

;; dilate three times on 3 channels, skip alpha
(p/set-canvas-pixels! canvas (->> img 
                                  (p/filter-channels p/dilate-filter false)
                                  (p/filter-channels p/dilate-filter false)
                                  (p/filter-channels p/dilate-filter false)))

;; erode three times on 3 channels, skip alpha
(p/set-canvas-pixels! canvas (->> img 
                                  (p/filter-channels p/erode-filter false)
                                  (p/filter-channels p/erode-filter false)
                                  (p/filter-channels p/erode-filter false)))

;; box blur with radius 5
(p/set-canvas-pixels! canvas (p/filter-channels p/box-blur-5 img))

;; box blur with radius 23
(p/set-canvas-pixels! canvas (p/filter-channels (p/make-box-blur 23) img))

;; gaussian blur with radius 5
(p/set-canvas-pixels! canvas (p/filter-channels p/gaussian-blur-5 img))

;; posterize 4, to give number of levels call (p/make-posterize 2)
(p/set-canvas-pixels! canvas (p/filter-channels p/posterize-4 img))

;; threshold all channels but alpha (50%), custom threshold (p/make-threshold 44)
(p/set-canvas-pixels! canvas  (p/filter-channels p/threshold-50 false img))

;; median filter five times (slow!)
(p/set-canvas-pixels! canvas (->> img 
                                  (p/filter-channels p/median-filter false)
                                  (p/filter-channels p/median-filter false)
                                  (p/filter-channels p/median-filter false)
                                  (p/filter-channels p/median-filter false)
                                  (p/filter-channels p/median-filter false)))

;; quntile filter 10 times (slow!!!), you can select quantile 0-8, 4 = median filter
(def quantile-filter (p/make-quantile-filter 3))
(p/set-canvas-pixels! canvas (reduce (fn [res _] (p/filter-channels quantile-filter false res)) img (range 10)))

;; tint image
(p/set-canvas-pixels! canvas (p/filter-channels (p/make-tint-filter 10 30 200) img))

;; let's compose some images

;; let's modulo add two images
(p/set-canvas-pixels! canvas (p/compose-channels :madd false
                                                 (p/load-pixels "results/ex11/erode.jpg")
                                                 (p/load-pixels "results/ex11/boxblur23.jpg")))

;; let's make random blends
(p/set-canvas-pixels! canvas (p/compose-channels (rand-nth c/blends-names) (rand-nth c/blends-names) (rand-nth c/blends-names) nil 
                                                 (p/load-pixels "results/ex11/erode.jpg")
                                                 (p/load-pixels "results/ex11/boxblur23.jpg")))
