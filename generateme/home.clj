(ns examples.ex11-pixels
  (:require [clojure2d.core :as core]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.extra.glitch :as g]
            [clojure2d.extra.variations :as v]
            [clojure2d.extra.overlays :as o]
            [clojure2d.extra.signal :refer :all]
            [criterium.core :refer :all]
            [clojure.pprint :refer [pprint]])
  (:import [net.jafama FastMath]))

(def p1 (p/load-pixels "generateme/oo/a2.png"))

(def p2 (p/load-pixels "generateme/oo/a1.jpg"))

(def p3 (p/load-pixels "generateme/ooo/ooo.jpg"))

(def noise-overlay (o/make-noise 40 (.w p1) (.h p1)))
(def spots-overlay (o/make-spots 80 [30 60 120 180] (.w p1) (.h p1)))

(def canvas (core/create-canvas (.w p1) (.h p1)))

(def scale 0.83)

(def windows (core/show-window canvas "glitch" (* scale (.w p1)) (* scale (.h p1)) 10))

(let [b (g/blend-machine)
      b2 (g/blend-machine)]
  (println b)
  (println b2)
  (p/set-canvas-pixels! canvas (p/filter-channels p/normalize-filter false 
                                                  (p/filter-channels p/normalize-filter false
                                                                     (g/blend-machine p2 p1 b)))))

(quick-bench (p/filter-channels p/dilate-filter false p1))

(core/with-canvas canvas
  (core/image (o/render-rgb-scanlines (@canvas 1))))

(core/with-canvas canvas
  (core/image (->> (core/get-image canvas)
                   (o/render-noise noise-overlay)
                   (o/render-spots spots-overlay))))

(core/close-session)

(core/save-canvas canvas (core/next-filename "generateme/oo/aaa" ".png"))

(p/set-canvas-pixels canvas p5)

(def p4 (p/get-canvas-pixels canvas))

(def p5 (p/get-canvas-pixels canvas))

(def p6 (p/get-canvas-pixels canvas))


(do
  (def palette (g/color-reducer-machine))
  (println palette)
  (p/set-canvas-pixels! canvas (g/color-reducer-machine palette p4)))


;; slitscan
(binding [v/*skip-random-variations* true]
  (let [;v1name (rand-nth v/variation-list-not-random)
                                        ;v2name (rand-nth v/variation-list-not-random)
                                        ;v1 (v/make-variation v1name 1.0 {})
                                        ;v2 (v/make-variation v2name 1.0 {})
        field-config (v/make-random-configuration)
        field (v/make-combination field-config)
                                        ;f (comp v1 v2)
        f field]

    (binding [p/*pixels-edge* :wrap]
      (pprint field-config)
                                        ;    (println (str v2name " o " v1name))
      (p/set-canvas-pixels canvas (p/filter-channels (g/make-slitscan2-filter f 2.0)
                                                     (g/make-slitscan2-filter f 1.98)
                                                     (g/make-slitscan2-filter f 2.02) nil p1)))))

;; full process without use of filter-channels
(time (let [effect (make-effect :dj-eq {:lo (r/drand -5 5) :mid (r/drand -5 5) :hi (r/drand -5 5) :peak_bw 1.3 :shelf_slope 1.5 :rate (r/irand 4000 100000)})
            effect2 (make-effect :distort {:factor 1.0})
            inluv (p/filter-colors c/to-LUV p5)
            in (signal-from-pixels inluv {:layout :planar
                                          :channels [0 1 2]
                                          :bits 8
                                          :coding :none
                                          :signed true})
            res (apply-effect effect in)
            resp (signal-to-pixels (p/clone-pixels p1) res {:channels [0 1 2]
                                                            :layout :planar
                                                            :bits 8
                                                            :coding :none
                                                            :signed true})]
        (p/set-canvas-pixels! canvas (p/filter-channels p/normalize nil (p/filter-colors c/from-LUV resp)))))

;; fold

(binding [v/*skip-random-variations* true]
  (let [;v1name (rand-nth v/variation-list-not-random)
                                        ;v2name (rand-nth v/variation-list-not-random)
                                        ;v1 (v/make-variation v1name 1.0 {})
                                        ;v2 (v/make-variation v2name 1.0 {})
        field-config (v/make-random-configuration)
        field (v/make-combination field-config)
        f field
                                        ;        f (comp v1 v2)
        ]

    (binding [p/*pixels-edge* :wrap]
                                        ;(println (str v2name " o " v1name))
      (pprint field-config)
      (p/set-canvas-pixels canvas (p/filter-channels (g/make-fold-filter f 2.01)
                                                     (g/make-fold-filter f 2.0)
                                                     (g/make-fold-filter f 1.99) nil p1)))))


;;; some speed tests

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def arr (vec (repeatedly 1000 #(m/drand))))

(defn lsin
  ""
  ^double [^double v]
  (FastMath/sin v))

(quick-bench (mapv #(+ (lsin %) (lsin %)) arr))
