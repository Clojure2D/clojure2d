(ns examples.ex11-pixels
  (:require [clojure2d.core :as core]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [clojure2d.math :as m]
            [clojure2d.extra.glitch :as g]
            [clojure2d.extra.variations :as v]
            [clojure2d.extra.overlays :as o]
            [clojure2d.extra.signal :refer :all]
            [criterium.core :refer :all])
  (:import [net.jafama FastMath]))



(def p1 (p/load-pixels "generateme/lucas/lucas.jpg"))

(def p2 (p/load-pixels "generateme/lucas/lucas1.jpg"))

(def p3 (p/load-pixels "generateme/ooo/ooo.jpg"))

(def noise-overlay (o/make-noise 80 (.w p1) (.h p1)))
(def spots-overlay (o/make-spots 80 [30 60 120 180] (.w p1) (.h p1)))


(def canvas (core/create-canvas (.w p1) (.h p1)))

(def scale 0.5)


(def windows (core/show-window canvas "glitch" (* scale (.w p1)) (* scale (.h p1)) 10))

(let [b (g/blend-machine)
      b2 (g/blend-machine)]
  (println b)
  (println b2)
  (p/set-canvas-pixels canvas (p/filter-channels p/equalize-filter false 
                                                 (p/filter-channels p/normalize-filter false
                                                                    (g/blend-machine p1 p4 b)))))

(quick-bench (p/filter-channels p/dilate-filter false p1))

(core/with-canvas canvas
  (core/image (o/render-rgb-scanlines (@canvas 1))))

(core/with-canvas canvas
  (core/image (->> (@canvas 1)
                   (o/render-noise noise-overlay)
                   (o/render-spots spots-overlay))))

(core/save-canvas canvas (core/next-filename "generateme/lukas/res" ".jpg"))

(p/set-canvas-pixels canvas p4)

(def p4 (p/get-canvas-pixels canvas))

(def p5 (p/get-canvas-pixels canvas))

(do
  (def palette (g/color-reducer-machine))
  (println palette)
  (p/set-canvas-pixels canvas (g/color-reducer-machine p5 palette)))


;; slitscan

(let [v1 (v/make-variation (rand-nth v/variation-list-not-random) 1.0 {})
      v2 (v/make-variation (rand-nth v/variation-list-not-random) 1.0 {})
      f (comp v1 v2)]

  (binding [p/*pixels-edge* :wrap]
    (p/set-canvas-pixels canvas (p/filter-channels (g/make-slitscan2-filter f 1.0)
                                                   (g/make-slitscan2-filter f 0.95)
                                                   (g/make-slitscan2-filter f 1.05) nil p4))))

;; full process without use of filter-channels
(time (let [effect (make-effect :dj-eq {:lo (m/drand -20 20) :mid (m/drand -20 20) :hi (m/drand -20 20) :peak_bw 1.3 :shelf_slope 1.5 :rate (m/irand 4000 100000)})
            in (signal-from-pixels p4 {:layout :planar
                                      :coding :alaw
                                      :signed true
                                      :channels [2 0 1]
                                      :bits 8})
            res (apply-effect effect in)
            resp (signal-to-pixels (p/clone-pixels p1) res {:layout :planar
                                                           :coding :alaw-rev
                                                           :signed true
                                                           :channels [2 0 1]
                                                           :bits 8})]
        (p/set-canvas-pixels canvas (p/filter-channels p/normalize nil resp))))


;;; some speed tests

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def arr (vec (repeatedly 1000 #(m/drand))))

(defn lsin
  ""
  ^double [^double v]
  (FastMath/sin v))

(quick-bench (mapv #(+ (lsin %) (lsin %)) arr))
