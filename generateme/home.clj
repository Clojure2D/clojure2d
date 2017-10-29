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
            [clojure2d.math.vector :as vv]
            [clojure.pprint :refer [pprint]])
  (:import [net.jafama FastMath]
           [clojure2d.math.vector Vec4 Vec2]
           [clojure2d.java PrimitiveMath]))

(def p1 (p/filter-colors c/to-HSB (p/load-pixels "generateme/pic1.jpg")))

(def oa (object-array (map #(p/get-color p1 %) (range (count p1)))))

(def oasorter (sort #(< (c/red %1) (c/red %2)) oa))

(dorun (map-indexed #(p/set-color p1 %1 %2) oasorter))

(aget oa 3000)

(nth oasorter 1)

(def p2 (p/load-pixels "generateme/gface/2.jpg"))

(def p3 (p/load-pixels "generateme/ooo/ooo.jpg"))

(def noise-overlay (o/make-noise (core/width p1) (core/height p1) {:alpha 40}))
(def spots-overlay (o/make-spots (core/width p1) (core/height p1) {:alpha 80 :intensities [30 60 120 180]}))

(def canvas (core/create-canvas (core/width p1) (core/height p1)))

(def scale (double 0.5))

(def windows (core/show-window canvas "glitch" (* scale (core/width p1)) (* scale (core/height p1)) 10))

(let [b (g/blend-machine)
      b2 (g/blend-machine)]
  (println b)
  (comment println b2)
  (p/set-canvas-pixels! canvas (p/filter-channels p/equalize-filter false 
                                                  (p/filter-channels p/normalize-filter false
                                                                     (g/blend-machine b p1 p3)))))

(core/with-canvas canvas
  (core/image (o/render-rgb-scanlines p1)))

(core/with-canvas canvas
  (core/image (-> (p/image-from-pixels p5)
                  (o/render-noise noise-overlay)
                  (o/render-spots spots-overlay))))

(core/with-canvas canvas
  (core/image (o/render-crt-scanlines (p/image-from-pixels p3) {:resolution 3})))

(core/close-session)

(core/save canvas (core/next-filename "generateme/hedge/aaa" ".png"))

(p/set-canvas-pixels! canvas (p/filter-colors c/from-HSB p1))

(def p2 (p/get-canvas-pixels canvas))

(def p3 (p/get-canvas-pixels canvas))

(def p4 (p/get-canvas-pixels canvas))

(def p5 (p/get-canvas-pixels canvas))


(defn make-more-colors
  [palette]
  (let [p (:palette palette) 
        np (if (<= (count p) 10)
             (vec (concat p (for [c1 p
                                  c2 p
                                  :when (not= c1 c2)]
                              (vv/interpolate c1 c2 0.5))))
             p)]
    (println (str "Palette size: " (count np)))
    (assoc palette :palette np)))

(do
  (def palette (g/color-reducer-machine))
  (comment println palette)
  (p/set-canvas-pixels! canvas (p/filter-channels p/normalize-filter nil (g/color-reducer-machine palette p2))))

;;mirror
(defn make-random-mirror
  ""
  []
  (partial p/filter-channels 
           (g/make-mirror-filter (rand-nth (keys g/mirror-types)))
           (g/make-mirror-filter (rand-nth (keys g/mirror-types)))
           (g/make-mirror-filter (rand-nth (keys g/mirror-types)))
           nil))

(p/set-canvas-pixels! canvas (->> p5
                                  ((make-random-mirror))
                                  ((make-random-mirror))))


;; slitscan
(binding [v/*skip-random-variations* true]
  (let [;v1name (rand-nth v/variation-list-not-random)
                                        ;v2name (rand-nth v/variation-list-not-random)
                                        ;v1 (v/make-variation v1name 1.0 {})
                                        ;v2 (v/make-variation v2name 1.0 {})
        field-config (v/make-random-configuration 2)
        field (v/make-combination field-config)
                                        ;f (comp v1 v2)
        f field]

    (binding [p/*pixels-edge* :wrap]
      (pprint field-config)
                                        ;    (println (str v2name " o " v1name))
      (p/set-canvas-pixels! canvas (p/filter-channels (g/make-slitscan2-filter f 2.0)
                                                      (g/make-slitscan2-filter f 1.98)
                                                      (g/make-slitscan2-filter f 2.02) nil p1)))))




(defn sonify
  [^long frame]
  ;; full process without use of filter-channels
  (time (let [t (m/norm frame 0 (* 60 25) 0 m/TWO_PI)
              st (- (* 10.0 (m/sin t)) 3)
              ct (- (* 10.0 (m/cos t)) 3)
              ;; effect (make-effect :simple-lowpass {:cutoff st})
              effect (make-effect :dj-eq {:lo st :mid ct :hi 0 :peak-bw 1.3 :shelf-slope 1.5 :rate 44100})
              ;; effect2 (make-effect :divider {:denom 2})
              ;; effect3 (make-effect :slew-limit {:maxrise 500 :maxfall 1000})
              ;; inluv (p/filter-colors c/to-LUV p1)

              resp (apply-effects-to-pixels effect
                                            {:layout :planar
                                             :channels [0 1 2]
                                             :bits 8
                                             :coding :none
                                             :signed false}
                                            {:channels [0 1 2]
                                             :layout :planar
                                             :bits 8
                                             :coding :none
                                             :signed false} p1)]
          (p/set-canvas-pixels! canvas resp;
                                ;; (p/filter-channels p/equalize-filter nil resp)
                                ))))

(do
  (core/close-session)
  (dotimes [x (* 60 25)]
    (sonify x)
    (core/save canvas (core/next-filename "generateme/fubar/fr" ".jpg"))))

;; fold

(binding [v/*skip-random-variations* true]
  (let [;v1name (rand-nth v/variation-list-not-random)
                                        ;v2name (rand-nth v/variation-list-not-random)
                                        ;v1 (v/make-variation v1name 1.0 {})
                                        ;v2 (v/make-variation v2name 1.0 {})
        field-config (v/make-random-configuration 2)
        field (v/make-combination field-config)
        f field
                                        ;        f (comp v1 v2)
        ]

    (binding [p/*pixels-edge* :wrap]
                                        ;(println (str v2name " o " v1name))
      (pprint field-config)
      (p/set-canvas-pixels! canvas (p/filter-channels (g/make-fold-filter f 2.01)
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

(p/set-canvas-pixels! canvas (g/blend-images-filter {:nasmes (take 1 (drop 2 (map #(.getPath ^java.io.File %) (filter #(.isFile ^java.io.File %) (file-seq (clojure.java.io/file "generateme/bl"))))))
                                                     :pixels [p2 p3] 
                                                     :distance :abs
                                                     :mode :colord} p1))

(keys vv/distances)
;; => (:euclid :euclid-sq :abs :cheb :canberra :emd :discrete)


;;;; sonification loop

(let [frames (* 60 24)
      scale (/ m/TWO_PI frames)]
  (core/close-session)
  (dotimes [x frames]
    (let [cutoff (m/norm (m/sin (* x scale)) -1 1 0.01 0.99)
          resonance (m/norm (m/sin (inc (* 2 x scale))) -1 1 0.01 0.99)
          env-mod (m/norm (m/sin (+ 2 (* x scale))) -1 1 0.01 0.99)
          fs (m/norm (m/cos (* x scale)) -1 1 250 100000)
          
          effect (make-effect :vcf303 {:rate fs :cutoff cutoff :resonance resonance :env-mod env-mod})
          res (p/filter-channels p/normalize-filter nil (apply-effects-to-pixels effect {:signed true} {:signed true} p1))]
      (p/set-canvas-pixels! canvas res)
      (binding [core/*jpeg-image-quality* 0.9]
        (core/save-canvas canvas (core/next-filename "generateme/painter/vcf303/" ".jpg"))))))


;;; path test

(def pcanvas (core/make-canvas 500 500))
(def pwindow (core/show-window pcanvas "Path test"))

(core/with-canvas pcanvas
  (core/set-background :black)
  (core/path-quad [(Vec2. 200 200) (Vec2. 200 300) (Vec2. 300 400) (Vec2. 300 300) (Vec2. 400 300)]))

;;
