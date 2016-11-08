;; grids based on noise, result used in pix2line glitch sketch

(ns examples.ex17-grids
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.joise :as j]
            [clojure2d.utils :as u]
            [clojure2d.pixels :as p])
  (:import  [java.awt Color]
            [clojure2d.pixels Pixels]))


(def canvas (create-canvas 600 600))

(show-window canvas "grid" 600 600 10)

(defmethod key-pressed ["grid" \space] [_]
  (let [r (u/to-hex (m/irand) 8)]
    (save-canvas canvas (str "results/ex17/" r ".jpg"))))

(def dark (Color. 2 22 52))
(def light (Color. 232 221 203))

(defn draw-grid
  ""
  [canvas]
  (let [nx (m/irand 1 200)
        ny (m/irand 1 200)
        noise (j/make-random-fractal)
        scale (m/drand 5)
        nnx (inc (int (* scale nx)))
        nny (inc (int (* scale ny)))]
    (println (str "nx=" nx))
    (println (str "ny=" ny))
    (dotimes [y 600]
      (dotimes [x 600]
        (let [time (int (/ (+ x (* y 600)) (* (- 200 nx) (- 200 ny))))
              yy (* (quot x nnx) (int (* nnx (inc (noise (quot x nnx) time)))))
              xx (* (quot y nny) (int (* nny (inc (noise (quot y nny) time)))))
              n (< (noise (quot (+ x xx) nx) (quot (+ y yy) ny)) 0.5)]
          (set-color canvas (if n dark light))
          (point canvas x y))))))

(do
  (with-canvas canvas
    (draw-grid))
  :done)
