;; http://math.mit.edu/~sheffield/spokes.html
;;
;; Animate kappa value from `0` to `inf`

(ns examples.ex25-gff
  "Gaussian Free Field" 
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m])
  (:import [java.awt Color]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; size of discrete gaussian free field grid (grid of size gff-size x gff-size)
(def ^:const ^int gff-size (m/irand 5 100))
(def ^:const ^int rays 75)
(def ^:const ^double rsteps (/ m/TWO_PI rays))

(defn gff-rows [] (vec (repeatedly gff-size m/grand)))
(def gff (vec (repeatedly gff-size gff-rows)))

(def ^:const ^int width 800)
(def ^:const ^int height 800)

(defn get-field-value
  "Get bilineary interpolated gaussian field value"
  ^double [^double x ^double y]
  (let [^double px (m/norm x 0 width 0 gff-size)
        ^double py (m/norm y 0 height 0 gff-size)
        ix (m/floor px)
        iy (m/floor py)
        rx (- px ix)
        ry (- py iy)
        ix (int (mod ix gff-size))
        iy (int (mod iy gff-size))
        ix+ (int (mod (inc ix) gff-size))
        iy+ (int (mod (inc iy) gff-size))
        vy1 (m/lerp (get-in gff [ix iy]) (get-in gff [ix+ iy]) rx)
        vy2 (m/lerp (get-in gff [ix iy+]) (get-in gff [ix+ iy+]) rx)]
    (m/lerp vy1 vy2 ry)))

(defn my-draw
  ""
  [canvas hf]
  (do
    (set-background canvas 21 20 19)
    (dotimes [t rays]
      (let [theta (* t rsteps)]
        (loop [x (double (* 0.5 width))
               y (double (* 0.5 height))
               iter (int 0)]
          (when (< iter 350)
            (let [v (* hf (get-field-value x y))
                  sx (m/sin (+ v theta))
                  sy (m/cos (+ v theta))]
              (rect canvas x y 0.8 0.8)
              (recur (+ x sx)
                     (+ y sy)
                     (unchecked-inc-int iter)))))))))

(defn draw
  ""
  [canvas frame _]
  (let [kappa (* (double frame) 0.0041)
        hf (/ (m/sqrt (* 8.0 (/ kappa m/PI))) 
              (- 4.0 kappa))]
    (with-canvas canvas
      (my-draw hf))))

(do

  (def canvas (create-canvas width height))

  (with-canvas canvas
    (set-color 220 220 210 200))

  (def window (show-window canvas "GFF" width height 25 draw)))
