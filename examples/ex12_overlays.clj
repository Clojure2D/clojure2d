;; three overlays I defined (used in my works)

(ns examples.ex12-overlays
  (:require [clojure2d.core :as core]
            [clojure2d.extra.overlays :as o])
  (:import [java.awt.image BufferedImage]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; load image and store
(def ^BufferedImage img (core/load-image "results/test.jpg"))

(def canvas (core/make-canvas (core/width img) (core/height img)))
(def window (core/show-window canvas "Overlays" 15 nil))

(defmethod core/key-pressed ["Overlays" \space] [_]
  (core/save-canvas canvas (core/next-filename "results/ex12/" ".jpg")))

;; tv/rgb skanning lines
(core/with-canvas canvas
  (core/image (o/render-rgb-scanlines img)))

;; noise
(def noise-overlay (o/make-noise 80 (core/width img) (core/height img)))

(core/with-canvas canvas
  (core/image (o/render-noise noise-overlay img)))

;; spots, it's good to prepare overlay first, than apply onto the image
(def spots-overlay (o/make-spots 80 [30 60 120 180] (core/width img) (core/height img)))

(core/with-canvas canvas
  (core/image (o/render-spots spots-overlay img)))

;; apply all
(core/with-canvas canvas
  (core/image (->> img
                   (o/render-noise noise-overlay)
                   (o/render-spots spots-overlay)
                   (o/render-rgb-scanlines))))
