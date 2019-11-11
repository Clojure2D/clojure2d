(ns clojure2d.extra.overlays-example
  (:require [clojure2d.core :refer :all]
            [clojure2d.extra.overlays :refer :all]
            [metadoc.examples :refer :all]
            [fastmath.random :as r]
            [clojure2d.color :as c]))

(r/set-seed! r/default-rng 42)

(defsnippet clojure2d.extra.overlays save-result "Save overlays"
  (let [n (str "images/overlays/" (first opts) ".jpg")
        c (canvas 600 800)
        i (load-image "docs/samurai.jpg")]
    (with-canvas-> c
      (set-color (c/gray 50))
      (rect 0 0 600 400)
      (set-color (c/gray 205))
      (rect 0 400 600 400)
      (image i 50 100))
    (save (apply f c params) (str "docs/" n))
    (str "../" n)))

(add-examples render-rgb-scanlines
  (example-snippet "Render RGB scanlines" save-result :image render-rgb-scanlines {:scale 1.8}))

(add-examples render-crt-scanlines
  (example-snippet "Render CRT scanlines" save-result :image render-crt-scanlines {:resolution 24.0 :mask-dark 0.7 :mask-light 1.3 :hardscan -8.0}))

(add-examples render-noise
  (example-snippet "Render noise overlay" save-result :image render-noise)
  (example-snippet "Render noise overlay, dark" save-result :image render-noise (noise-overlay 600 800 {:alpha 200})))

(save (noise-overlay 300 300 {:alpha 200}) "docs/images/overlays/noise.png")

(add-examples noise-overlay
  (example-image "Overlay image" "../images/overlays/noise.png"))

(add-examples spots-overlay
  (example "Generate spots overlay (it's a list of images)" (count (spots-overlay 300 300 {:alpha 80 :intensities [10 100 200]}))))

(add-examples render-spots
  (example-snippet "Render spots overlay" save-result :image render-spots (spots-overlay 600 800  {:alpha 128 :intensities [10 50 100 150 200 250]})))

