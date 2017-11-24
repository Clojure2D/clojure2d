(ns examples.GG.P.P-1-2-3-04
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v]
            [clojure2d.math :as m]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn make-parts
  "Create fragments"
  [^long part-count]
  (loop [ii (int 0)
         mx part-count
         coll []] 
    (if (< ii mx)
      (if (r/brand 0.075)
        (let [fragments (r/irand 2 20)]
          (recur (inc ii) (+ mx fragments) (vec (concat coll (repeatedly fragments #(r/drand 2))))))
        (recur (inc ii) mx (conj coll (r/drand 2 20))))
      coll)))

(defn draw
  "Draw stripes"
  [canvas]
  (let [iter (make-counter)
        colors (mapv #(c/from-HSB (if (even? %)
                                    (v/vec4 (r/drand 256) 255 (r/drand 255) 255)
                                    (v/vec4 138 (r/drand 255) 255 255))) (range 20))
        row-count (r/irand 5 30) 
        row-height (/ ^int (height canvas) (double row-count))
        rows (map #(vector % (make-parts (inc ^long %))) (range (dec row-count) -1 -1))]
    (set-background canvas :black)
    (doseq [[^long i parts] rows]
      (let [sum-parts-total (reduce clojure.core/+ parts)
            cumulative (reductions clojure.core/+ 0 parts)]
        (mapv (fn [cum p]
                (when (r/brand 0.45)
                  (let [x (+ (r/drand -10 10) ^double (m/norm cum 0 sum-parts-total 0 (width canvas)))
                        y (+ (r/drand -10 10) (* i row-height))
                        w (+ (r/drand -10 10) ^double (m/norm p 0 sum-parts-total 0 (width canvas)))
                        h (* 1.5 row-height)]
                    (set-gradient canvas x y (c/make-awt-color 0 0 0 180) x (+ y h) (c/set-alpha (colors (mod ^long (iter) 20)) 100))
                    (rect canvas x y w h)))) cumulative parts)))))

(def canvas (make-canvas 800 800))
(def window (show-window canvas "P_1_2_3_04"))

(defmethod mouse-event [(:window-name window) :mouse-released] [_ _] 
  (with-canvas-> canvas
    draw))

(with-canvas-> canvas
  draw)
