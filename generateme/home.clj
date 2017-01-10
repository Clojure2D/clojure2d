(ns examples.ex11-pixels
  (:require [clojure2d.core :as core]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [clojure2d.math :as m]
            [clojure2d.extra.glitch :as g]))


(def p1 (p/load-pixels "generateme/tst/eggs.jpg"))

(def p2 (p/load-pixels "generateme/tst/eggst.jpg"))

(p/save-pixels (p/filter-channels p/equalize-filter (p/filter-channels p/normalize-filter (g/random-blend p1 p2))) "generateme/tst/eggsres.jpg")

(def p3 (p/load-pixels "generateme/tst/mono.jpg"))

(do
  (def palette (g/color-reducer-machine))
  (println palette)
  (p/save-pixels (g/color-reducer-machine p3 palette) "generateme/tst/paletton5.jpg"))
