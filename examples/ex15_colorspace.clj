(ns examples.ex15-colorspace
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]))

;; convert image into some colorspaces

(def img (p/load-pixels "results/test.jpg"))

(p/save-pixels (p/filter-colors c/to-OHTA img) "results/ex15/ohta.jpg")
(p/save-pixels (p/filter-colors c/to-CMY img) "results/ex15/cmy.jpg")
(p/save-pixels (p/filter-colors c/to-YPbPr img) "results/ex15/ypbpr.jpg")
(p/save-pixels (p/filter-colors c/from-OHTA img) "results/ex15/revohta.jpg")
(p/save-pixels (p/filter-colors c/from-YPbPr img) "results/ex15/revypbpr.jpg")

(p/save-pixels (p/filter-colors (comp c/to-YPbPr c/from-OHTA c/from-YPbPr) img) "results/ex15/mess.jpg")

;; equalize histogram in YPbPr colorspace
(p/save-pixels (->> img
                    (p/filter-colors c/to-YPbPr)
                    (p/filter-channels p/equalize-filter false)
                    (p/filter-colors c/from-YPbPr))
             "results/ex15/equalize.jpg")
