;; Random or uniform generators
;; See section "Random Vectors": http://commons.apache.org/proper/commons-math/userguide/random.html

(ns examples.ex30-generators
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def canvas (create-canvas 600 900))
(def window (show-window canvas "Generators" 5 nil))

(defmethod key-pressed ["Generators" \space] [_ _]
  (binding [*jpeg-image-quality* 0.9]
    (save canvas "results/ex30/result.jpg")))

(defn draw-random-rect
  "Draw random rectangle from dots from random sequence"
  [canvas s posx posy scale num]
  (let [generator (r/make-sequence-generator s 2)
        transformed (map #(v/mult % scale) (generator))]
    (doseq [^Vec2 v (take num transformed)]
      (ellipse canvas (+ ^double posx (.x v)) (+ ^double posy (.y v)) 3 3)))
  canvas)

(with-canvas canvas
  (set-background 10 10 10)
  (set-color 255 0 0 150)
  (draw-random-rect :halton 50 50 225 1000) ;; upper left
  (draw-random-rect :sobol 325 50 225 1000) ;; upper right
  (set-color 0 255 0 100)
  (draw-random-rect :gaussian 300 450 (/ 112.5 2) 2000) ;; middle
  (set-color 200 200 200 150)
  (draw-random-rect :sphere 162.5 737.5 112.5 150) ;; bottom left
  (draw-random-rect :default 325 625 225 1000)) ;; bottom right
