;; http://lodev.org/cgtutor/fire.html

(ns examples.ex40-old-school-fire
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^int bw 400)
(def ^:const ^int bh 200)
(def ^:const ^int bh- (dec bh))

(def palette (mapv #(c/from-HSB (c/make-color (/ ^long % 3.0) 255 (min 255 (* 2.0 ^long %)))) (range 256)))

(defn make-2d-getter-setter
  "2d `ints` array getter"
  [^long w ^long h]
  (let [^ints array (int-array (* bw bh))]
    [(fn ^long [^long x ^long y] (aget array (+ (m/wrap 0 w x) (* (m/wrap 0 h y) w))))
     (fn ^long [^long x ^long y ^long v] (aset array (+ (m/wrap 0 w x) (* (m/wrap 0 h y) w)) (unchecked-int v)))]))

(defn process-fire
  "Calculate next step"
  [[get2d set2d]]
  (dotimes [x bw]
    (set2d x bh- (r/randval 0.8 (r/irand 256) 0))) 
  (dotimes [y bh-]
    (let [y+ (inc y)]
      (dotimes [x bw]
        (set2d x y (/ (bit-shift-left (+ ^long (get2d (dec x) y+)
                                         ^long (get2d x y+)
                                         ^long (get2d (inc x) y+)
                                         ^long (get2d x (inc y+))) 6) 257))))))

(defn set-pixels
  "buffer -> pixels"
  [pixels [get2d _]] 
  (dotimes [x bw]
    (dotimes [y bh]
      (p/set-color pixels x y (palette (get2d x y)))))
  pixels)

(defn draw
  "Frames"
  [canvas _ _ state]
  (let [[pixels fns :as all] (or state [(p/make-pixels bw bh) (make-2d-getter-setter bw bh)])]
    (process-fire fns)
    (p/set-canvas-pixels! canvas (set-pixels pixels fns))
    all))

(def canvas (make-canvas bw bh :mid))
(def window (show-window canvas "Oldschool fire" (* 2 bw) (* 2 bh) 60 draw))
