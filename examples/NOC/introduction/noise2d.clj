(ns examples.NOC.introduction.noise2d
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]
            [clojure2d.math :as m]
            [clojure2d.pixels :as p]
            [clojure2d.math.joise :as j]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; various types of noise defined in clojure2d library
(def noise-fn r/noise)
;; (def noise-fn (r/make-perlin-noise (r/irand) 2)) ;; two octaves only
;; (def noise-fn j/perlin-noise)
;; (def noise-fn (j/make-random-basis-noise))
;; (def noise-fn (j/make-random-cell-noise))
;; (def noise-fn (j/make-random-fractal-noise))

(def canvas (make-canvas 640 360))
(def window (show-window canvas "Noise 2d"))

(let [pixels (p/get-canvas-pixels canvas)]

  (dotimes [x (width canvas)]
    (dotimes [y (height canvas)]
      (let [xoff (m/norm x 0 (width canvas) 0 (* 0.02 ^int (width canvas)))
            yoff (m/norm y 0 (height canvas) 0 (* 0.02 ^int (height canvas)))
            b (* 255.0 ^double (noise-fn xoff yoff))]

        (p/set-color pixels x y (c/make-color b b b)))))

  (p/set-canvas-pixels! canvas pixels))
