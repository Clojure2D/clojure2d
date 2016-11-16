;; grids based on noise, result used in pix2line glitch sketch

(ns examples.ex17-grids
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.joise :as j]
            [clojure2d.pixels :as p])
  (:import  [java.awt Color]
            [clojure2d.pixels Pixels]))


(def canvas (create-canvas 600 600))

(show-window canvas "grid" 600 600 10)

(defmethod key-pressed ["grid" \space] [_]
  (let [r (to-hex (m/irand) 8)]
    (save-canvas canvas (str "results/ex17/" r ".jpg"))))

(def dark (Color. 2 22 52))
(def light (Color. 232 221 203))

(defn draw-grid
  ""
  [canvas]
  (let [nx (m/irand 2 200)
        ny (m/irand 2 200)
        noise (m/make-perlin-noise)
        scale (m/irand 5)
        nnx (inc (int (* scale nx)))
        nny (inc (int (* scale ny)))
        div (* (- 201 nx) (- 201 ny))
        shift (m/drand -10 10)]
    (println (str "nx=" nx))
    (println (str "ny=" ny))
    (dotimes [y 600]
      (dotimes [x 600]
        (let [time (+ shift (int (/ (+ x (* y 600)) div)))
              yy (* (quot x nnx) (int (* nnx (inc (m/noise (quot x nnx) time )))))
              xx (* (quot y nny) (int (* nny (inc (m/noise (quot y nny) (- time))))))
              n (< (m/noise (quot (+ x xx) nx) (quot (+ y yy) ny)) 0.5)]
          (set-color canvas (if n dark light))
          (rect canvas x y 1 1))))))

(do
  (with-canvas canvas
    (draw-grid))
  :done)
