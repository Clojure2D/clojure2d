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

(let [s (g/slitscan-random-config)]
  (println s)
  (p/set-canvas-pixels! canvas (p/filter-channels (g/make-slitscan s) nil img)))

;; channel shift

(p/set-canvas-pixels! canvas (p/filter-channels (g/make-shift-channels {:horizontal-shift 0.1
                                                                        :vertical-shift 0.1})
                                                nil
                                                (g/make-shift-channels {:horizontal-shift -0.1
                                                                        :vertical-shift -0.1})
                                                nil img))

(p/set-canvas-pixels! canvas (->> img
                                  (p/filter-colors c/to-HWB)
                                  (p/filter-channels (g/make-shift-channels {:horizontal-shift 0.1
                                                                             :vertical-shift 0.0})
                                                     nil
                                                     (g/make-shift-channels {:horizontal-shift -0.1
                                                                             :vertical-shift 0.0})
                                                     nil)
                                  (p/filter-colors c/from-HWB)))

;; random shift
(p/set-canvas-pixels! canvas (p/filter-channels (g/make-shift-channels)
                                                (g/make-shift-channels)
                                                (g/make-shift-channels)
                                                nil img))

;; mirror image

(defn make-random-mirror
  ""
  []
  (partial p/filter-channels 
           (g/make-mirror (g/mirror-random-config))
           (g/make-mirror (g/mirror-random-config))
           (g/make-mirror (g/mirror-random-config))
           nil))

(p/set-canvas-pixels! canvas (->> img
                                  ((make-random-mirror))
                                  ((make-random-mirror))))


;; slitscan 2

(binding [v/*skip-random-variations* true]
  (let [field-config (v/make-random-configuration)]
    (binding [p/*pixels-edge* :wrap]
      (println field-config)
      (p/set-canvas-pixels! canvas (p/filter-channels (g/make-slitscan2 {:variation field-config :r 2.03})
                                                      (g/make-slitscan2 {:variation field-config :r 2.0})
                                                      (g/make-slitscan2 {:variation field-config :r 1.97}) nil img)))))


;; fold

(binding [v/*skip-random-variations* true]
  (let [field-config (v/make-random-configuration)]
    (binding [p/*pixels-edge* :wrap]
      (println field-config)
      (p/set-canvas-pixels! canvas (p/filter-channels (g/make-fold {:variation field-config :r 2.03})
                                                      (g/make-fold {:variation field-config :r 2.0})
                                                      (g/make-fold {:variation field-config :r 1.97}) nil img)))))

;; pix2line

(p/set-canvas-pixels! canvas (p/filter-channels (g/make-pix2line (g/pix2line-random-config))
                                                (g/make-pix2line (g/pix2line-random-config))
                                                (g/make-pix2line (g/pix2line-random-config)) nil img))
