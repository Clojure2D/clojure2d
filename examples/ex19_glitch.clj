;; Several glitch examples

(ns examples.ex19-glitch
  (:require [clojure2d.math :as m]
            [clojure2d.pixels :as p]
            [clojure2d.core :refer :all]
            [clojure2d.color :as c]
            [clojure2d.extra.variations :as v]
            [clojure2d.extra.glitch :as g])
  (:import [clojure2d.pixels Pixels]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^Pixels img (p/load-pixels "results/test.jpg"))

(def canvas (make-canvas (width img) (height img)))
(def window (show-window canvas "Glitch"))

(defmethod key-pressed ["Glitch" \space] [_ _]
  (save canvas (next-filename "results/ex19/" ".jpg")))

;; slitscan

(def sc-config-x
  (let [s (g/slitscan-random-setup)]
    (println s)
    (g/make-slitscan-waves s)))

(def sc-config-y
  (let [s (g/slitscan-random-setup)]
    (println s)
    (g/make-slitscan-waves s)))

(p/set-canvas-pixels! canvas (p/filter-channels (g/make-slitscan-filter sc-config-x sc-config-y) nil img))

;; channel shift

(p/set-canvas-pixels! canvas (p/filter-channels (g/make-shift-channels-filter 0.1 true true)
                                                nil
                                                (g/make-shift-channels-filter -0.1 true true)
                                                nil img))

(p/set-canvas-pixels! canvas (->> img
                                  (p/filter-colors c/to-HWB)
                                  (p/filter-channels (g/make-shift-channels-filter 0.1 true false)
                                                     nil
                                                     (g/make-shift-channels-filter -0.1 true false)
                                                     nil)
                                  (p/filter-colors c/from-HWB)))

;; mirror image

(defn make-random-mirror
  ""
  []
  (partial p/filter-channels 
           (g/make-mirror-filter (rand-nth (keys g/mirror-types)))
           (g/make-mirror-filter (rand-nth (keys g/mirror-types)))
           (g/make-mirror-filter (rand-nth (keys g/mirror-types)))
           nil))

(p/set-canvas-pixels! canvas (->> img
                                  ((make-random-mirror))
                                  ((make-random-mirror))))


;; slitscan 2

(binding [v/*skip-random-variations* true]
  (let [field-config (v/make-random-configuration)
        f (v/make-combination field-config)]

    (binding [p/*pixels-edge* :wrap]
      (println field-config)
      (p/set-canvas-pixels! canvas (p/filter-channels (g/make-slitscan2-filter f 2.03)
                                                      (g/make-slitscan2-filter f 2.0)
                                                      (g/make-slitscan2-filter f 1.97) nil img)))))


;; fold

(binding [v/*skip-random-variations* true]
  (let [field-config (v/make-random-configuration)
        f (v/make-combination field-config)]

    (binding [p/*pixels-edge* :wrap]
      (println field-config)
      (p/set-canvas-pixels! canvas (p/filter-channels (g/make-fold-filter f 2.03)
                                                      (g/make-fold-filter f 2.0)
                                                      (g/make-fold-filter f 1.97) nil img)))))

;; pix2line

(p/set-canvas-pixels! canvas (p/filter-channels (g/make-pix2line (g/make-pix2line-config))
                                                (g/make-pix2line (g/make-pix2line-config))
                                                (g/make-pix2line (g/make-pix2line-config)) nil img))
