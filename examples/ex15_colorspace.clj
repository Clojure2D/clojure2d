(ns examples.ex15-colorspace
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c])
  (:import [clojure2d.pixels Pixels]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; convert image into some colorspaces

(def ^Pixels img (p/load-pixels "results/test.jpg"))

(def canvas (make-canvas (width img) (height img)))
(def window (show-window canvas "Colorspace" 15 nil))

(defmethod key-pressed ["Colorspace" \space] [_]
  (save-canvas canvas (next-filename "results/ex15/" ".jpg")))


(p/set-canvas-pixels! canvas (p/filter-colors c/to-OHTA img))
(p/set-canvas-pixels! canvas (p/filter-colors c/to-CMY img))
(p/set-canvas-pixels! canvas (p/filter-colors c/to-YPbPr img))
(p/set-canvas-pixels! canvas (p/filter-colors c/from-OHTA img))
(p/set-canvas-pixels! canvas (p/filter-colors c/from-YPbPr img))

(p/set-canvas-pixels! canvas (p/filter-colors (comp c/to-YPbPr c/from-OHTA c/from-YPbPr) img))

;; equalize histogram in YPbPr colorspace
(p/set-canvas-pixels! canvas (->> img
                                  (p/filter-colors c/to-YPbPr)
                                  (p/filter-channels p/equalize-filter false)
                                  (p/filter-colors c/from-YPbPr)))


;; random conversion
(let [cs1 (rand-nth c/colorspaces-names)
      cs2 (rand-nth c/colorspaces-names)]
  (println (str ":RGB -> " cs1 " -> " cs2 " -> :RGB"))
  (p/set-canvas-pixels! canvas (->> img
                                    (p/filter-colors (first (c/colorspaces cs1)))
                                    (p/filter-channels p/normalize-filter false)
                                    (p/filter-colors (second (c/colorspaces cs2)))
                                    (p/filter-channels p/normalize-filter false))))

