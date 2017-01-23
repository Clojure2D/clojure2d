(ns examples.ex11-pixels
  (:require [clojure2d.core :as core]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [clojure2d.math :as m]
            [clojure2d.extra.glitch :as g]
            [clojure2d.extra.variations :as v]
            [clojure2d.extra.overlays :as o]))


(def p1 (p/load-pixels "generateme/ooo/ooo2.jpg"))

(def p2 (p/load-pixels "generateme/ooo/res_07E234_ooo.jpg"))

(def p3 (p/load-pixels "generateme/ooo/ooo.jpg"))

(def noise-overlay (o/make-noise 80 (.w p1) (.h p1)))
(def spots-overlay (o/make-spots 80 [30 60 120 180] (.w p1) (.h p1)))


(def canvas (core/create-canvas (.w p1) (.h p1)))

(def scale 0.6)


(def windows (core/show-window canvas "glitch" (* scale (.w p1)) (* scale (.h p1)) 10))

(let [b (g/blend-machine)
      b2 (g/blend-machine)]
  (println b)
  (println b2)
  (p/set-canvas-pixels canvas (p/filter-channels p/equalize-filter false 
                                                 (p/filter-channels p/normalize-filter false
                                                                    (g/blend-machine p3
                                                                                     (g/blend-machine p2 p1 b)
                                                                                     b2)))))

(core/with-canvas canvas
  (core/image (o/render-rgb-scanlines (@canvas 1))))

(core/with-canvas canvas
  (core/image (->> (@canvas 1)
                   (o/render-noise noise-overlay)
                   (o/render-spots spots-overlay))))

(core/save-canvas canvas (core/next-filename "generateme/ooo/res" ".jpg"))

(def p4 (p/get-canvas-pixels canvas))

(do
  (def palette (g/color-reducer-machine))
  (println palette)
  (p/set-canvas-pixels canvas (g/color-reducer-machine p4 palette)))


;; slitscan

(let [v1 (v/make-variation (rand-nth v/variation-list-not-random) 1.0 {})
      v2 (v/make-variation (rand-nth v/variation-list-not-random) 1.0 {})
      f (comp v1 v2)]

  (binding [p/*pixels-edge* :wrap]
    (p/set-canvas-pixels canvas (p/filter-channels (g/make-slitscan2-filter f 2.0)
                                                   (g/make-slitscan2-filter f 1.9)
                                                   (g/make-slitscan2-filter f 2.1) nil p1))))
