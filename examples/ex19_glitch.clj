;; Several glitch examples

(ns examples.ex19-glitch
  (:require [clojure2d.math :as m]
            [clojure2d.pixels :as p]
            [clojure2d.core :refer :all]
            [clojure2d.extra.glitch :as g])
  (:import [clojure2d.pixels Pixels]))


(def ^Pixels img (load-pixels "results/test.jpg"))

;; slitscan

(save-pixels (p/filter-channels (g/make-slitscan-filter) nil img) "results/ex19/slit.jpg")
(save-pixels (p/filter-channels (g/make-slitscan-filter) (g/make-slitscan-filter) (g/make-slitscan-filter) nil img) "results/ex19/slit_s.jpg")
