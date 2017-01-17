(ns examples.ex11-pixels
  (:require [clojure2d.core :as core]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [clojure2d.math :as m]
            [clojure2d.extra.glitch :as g]))


(def p1 (p/load-pixels "generateme/tst/gg1.jpg"))

(def p2 (p/load-pixels "generateme/tst/gg3.jpg"))

(def p3 (p/load-pixels "generateme/tst/res_9F2D998D_krew.jpg"))


(def canvas (core/create-canvas (.w p1) (.h p1)))

(def windows (core/show-window canvas "glitch" (* 0.5 (.w p1)) (* 0.5 (.h p1)) 10))

(let [b (g/blend-machine)]
  (println b)
  (p/set-canvas-pixels canvas (p/filter-channels p/equalize-filter false 
                                                 (p/filter-channels p/normalize-filter false 
                                                                    (g/blend-machine p2 p1 b)))))

(core/save-canvas canvas (core/next-filename "generateme/tst/dance/res" ".jpg"))

(def p4 (p/get-canvas-pixels canvas))

(do
  (def palette (g/color-reducer-machine))
  (println palette)
  (p/set-canvas-pixels canvas (g/color-reducer-machine p4 palette)))
