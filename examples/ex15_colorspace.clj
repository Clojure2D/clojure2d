(ns examples.ex15-colorspace
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]))

;; convert image into some colorspaces

(def img (load-pixels "results/test.jpg"))

(save-pixels (p/filter-colors c/to-OHTA img) "results/ex15/ohta.jpg")
(save-pixels (p/filter-colors c/to-CMY img) "results/ex15/cmy.jpg")
(save-pixels (p/filter-colors c/to-YPbPr img) "results/ex15/ypbpr.jpg")
(save-pixels (p/filter-colors c/from-OHTA img) "results/ex15/revohta.jpg")
(save-pixels (p/filter-colors c/from-YPbPr img) "results/ex15/revypbpr.jpg")

(save-pixels (p/filter-colors (comp c/to-YPbPr c/from-OHTA c/from-YPbPr) img) "results/ex15/mess.jpg")

;; equalize histogram in YPbPr colorspace
(save-pixels (p/filter-colors c/from-YPbPr 
                              (p/filter-channels p/equalize-filter false 
                                                 (p/filter-colors c/to-YPbPr img)))
             "results/ex15/equalize.jpg")
