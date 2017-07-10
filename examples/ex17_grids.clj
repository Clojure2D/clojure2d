;; grids based on noise, result used in pix2line glitch sketch

(ns examples.ex17-grids
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.joise :as j]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c])
  (:import [clojure2d.pixels Pixels]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def canvas (create-canvas 600 600))

(def window (show-window canvas "grid" 10 nil))

(defmethod key-pressed ["grid" \space] [_]
  (save-canvas canvas (next-filename "results/ex17/" ".jpg")))

(def dark (c/make-awt-color 2 22 52))
(def light (c/make-awt-color 232 221 203))

(defn draw-grid
  ""
  [canvas]
  (let [^int nx (r/irand 2 200)
        ^int ny (r/irand 2 200)
        noise (r/make-perlin-noise)
        ^int scale (r/irand 5)
        nnx (inc (int (* scale nx)))
        nny (inc (int (* scale ny)))
        div (* (- 201 nx) (- 201 ny))
        ^double shift (r/drand -10 10)]
    (println (str "nx=" nx))
    (println (str "ny=" ny))
    (dotimes [y 600]
      (dotimes [x 600]
        (let [time (+ shift (int (/ (+ x (* y 600)) div)))
              yy (* (quot x nnx) (int (* nnx (inc ^double (noise (quot x nnx) time )))))
              xx (* (quot y nny) (int (* nny (inc ^double (noise (quot y nny) (- time))))))
              n (< ^double (noise (quot (+ x xx) nx) (quot (+ y yy) ny)) 0.5)]
          (set-awt-color canvas (if n dark light))
          (rect canvas x y 1 1))))))

(do
  (with-canvas canvas
    (draw-grid))
  :done)
