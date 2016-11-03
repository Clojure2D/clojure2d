;; three overlays I defined (used in my works)

(ns examples.ex12-overlays
  (:require [clojure2d.core :as core]
            [clojure2d.extra.overlays :as o])
  (:import [java.awt.image BufferedImage]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; load image and store
(def ^BufferedImage img (core/load-image "results/test.jpg"))

;; tv/rgb skanning lines
(core/save-image (o/render-rgb-scanlines img) "results/ex12/rgb.jpg")

;; noise
(def noise-overlay (o/make-noise 80 (.getWidth img) (.getHeight img)))
(core/save-image (o/render-noise noise-overlay img) "results/ex12/noise.jpg")

;; spots, it's good to prepare overlay first, than apply onto the image
(def spots-overlay (o/make-spots 80 [30 60 120 180] (.getWidth img) (.getHeight img)))
(core/save-image (o/render-spots spots-overlay img) "results/ex12/spots.jpg")

;; apply all
(core/save-image (->> img
                      (o/render-noise noise-overlay)
                      (o/render-spots spots-overlay)
                      (o/render-rgb-scanlines)) "results/ex12/all.jpg")
