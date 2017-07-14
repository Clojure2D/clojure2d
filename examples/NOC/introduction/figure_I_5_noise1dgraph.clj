(ns example.NOC.introduction.figure-I-5-noise1dgraph
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.joise :as j]
            [clojure2d.math.random :as r])
  (:import clojure2d.math.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; various types of noise defined in clojure2d library
(def noise-fn r/noise)
;; (def noise-fn (r/make-perlin-noise (r/irand) 2)) ;; two octaves only
;; (def noise-fn j/perlin-noise)
;; (def noise-fn (j/make-random-basis-noise))
;; (def noise-fn (j/make-random-cell-noise))
;; (def noise-fn (j/make-random-fractal-noise))

(defn draw
  "Draw on canvas."
  [canvas window framecount state]
  (let [^double t (or state 0.0)
        p (map #(Vec2. % (* ^double (height window) ^double (noise-fn (+ t (* 0.01 ^double %))))) (range 0 (width canvas) 2))]

    (-> canvas
        (set-background :white)
        (set-color :black)
        (set-stroke 2.0)
        (path p))

    (+ t 0.01)))

(def window (show-window (make-canvas 400 200) "Noise 1d graph" draw))
