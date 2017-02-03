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

(def canvas (make-canvas (.w img) (.h img)))
(def window (show-window canvas "Glitch" (.w img) (.h img) 15))

(defmethod key-pressed ["Glitch" \space] [_]
  (save-canvas canvas (next-filename "results/ex19/" ".jpg")))

;; slitscan

(p/set-canvas-pixels canvas (p/filter-channels (g/make-slitscan-filter) nil img))

(p/set-canvas-pixels canvas (p/filter-channels (g/make-slitscan-filter) (g/make-slitscan-filter) (g/make-slitscan-filter) nil img))

;; channel shift

(p/set-canvas-pixels canvas (p/filter-channels (g/make-shift-channels-filter 0.1 true true)
                                  nil
                                  (g/make-shift-channels-filter -0.1 true true)
                                  nil img))

(p/set-canvas-pixels canvas (->> img
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

(p/set-canvas-pixels canvas (->> img
                                 ((make-random-mirror))
                                 ((make-random-mirror))))


;; slitscan 2

(let [v1name (rand-nth v/variation-list-not-random)
      v2name (rand-nth v/variation-list-not-random)
      v1 (v/make-variation v1name 1.0 {})
      v2 (v/make-variation v2name 1.0 {})
      f (comp v1 v2)]

  (binding [p/*pixels-edge* :wrap]
    (println (str v1name " o " v2name))
    (p/set-canvas-pixels canvas (p/filter-channels (g/make-slitscan2-filter f 2.03)
                                                   (g/make-slitscan2-filter f 2.0)
                                                   (g/make-slitscan2-filter f 1.97) nil img))))


;; fold

(let [v1name (rand-nth v/variation-list-not-random)
      v2name (rand-nth v/variation-list-not-random)
      v1 (v/make-variation v1name 1.0 {})
      v2 (v/make-variation v2name 1.0 {})
      f (comp v1 v2)]

  (binding [p/*pixels-edge* :wrap]
    (println (str v2name " o " v1name))
    (p/set-canvas-pixels canvas (p/filter-channels (g/make-fold-filter f 2.03)
                                                   (g/make-fold-filter f 2.0)
                                                   (g/make-fold-filter f 1.97) nil img))))
