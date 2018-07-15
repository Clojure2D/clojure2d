(ns colors
  (:require [clojure2d.core :refer :all]
            [clojure2d.color :as c]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [clojure2d.extra.utils :as u]
            [fastmath.clustering :as cl]
            [clojure2d.pixels :as p]
            [fastmath.stats :as stat]))

(def canv (canvas 800 300))

(def window (show-window {:canvas canv}))

(with-canvas [c canv] 
  (let [col (c/resample 112 (c/colourlovers-palettes 5) :LAB :cubic-spline)
        p0 (c/gradient col)
        p1 (c/gradient (c/colourlovers-palettes 5))]
    (dotimes [x (width c)]
      (let [t (m/norm x 0 (width c) 0 1)]
        (set-color c (p0 t))
        (line c x 0 x 150)
        (set-color c (p1 t))
        (line c x 150 x 300)))))

;;

(def img (p/load-pixels "docs/samurai.jpg"))

(u/show-palette (c/reduce-colors :LUV (resize (p/load-pixels "bbb1.jpg") 600 600) 5) )  

;;

(save (:buffer (u/show-gradient (c/gradient :LAB :cubic-spline (c/reduce-colors :LAB (resize (p/load-pixels "b5.jpg") 600 600) 5)))) "b5grad.jpg")

(u/show-gradient (c/gradient :LAB :cubic-spline (c/reduce-colors :LUV (resize (p/load-pixels "bbb1.jpg") 600 600) 4)))

;;
