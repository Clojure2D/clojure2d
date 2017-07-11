(ns example.NOC.introduction.figure-I-6-randomgraph
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; (def random-fn r/drand)

;; (println r/randomizers)
;; (def random-fn (partial r/drandom (r/make-randomizer :mersenne)))

(def canvas (make-canvas 400 200))
(def window (show-window canvas "Random graph"))

(let [p (map #(Vec2. % (random-fn (height canvas))) (range 0 (width canvas) 2))]
  (with-canvas canvas
    (set-background 255 255 255)
    (set-color 0 0 0)
    (set-stroke 2.0)
    (path p)))

