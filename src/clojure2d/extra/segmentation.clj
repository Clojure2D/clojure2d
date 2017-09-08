;; ## Namespace scope
;;
;; Segment pixels into squares. Segmentation is based on similarity of channel values.
;;
;; See example 13

(ns clojure2d.extra.segmentation
  (:require [clojure2d.core :as core]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.pixels :as p])
  (:import [clojure2d.pixels Pixels]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- calc-stdev
  "Calculate standard deviation of selection 10% of random channel values"
  ([^Pixels pixels ch sx sy w h]
   (let [^int sx sx
         ^int sy sy
         ^int w w
         ^int h h
         limit (max (int (* 0.1 w h)) 4)]
     (loop [A 0.0
            Q 0.0
            k 1]
       (let [posx (r/irand w)
             posy (r/irand h)
             xk (double (p/get-value pixels ch (+ sx posx) (+ sy posy)))
             newA (+ A (/ (- xk A) k))
             newQ (+ Q (* (- xk A) (- xk newA)) )]
         (if (< k limit)
           (recur newA newQ (inc k))
           (m/sqrt (/ Q (dec limit)))))))))

(defn segment-pixels
  "Decompose channel into segments where mins is minimum size of segment, maxs is maximum size, thr is accuracy (minimum std dev of pixel values to make decision about subdivision."
  [^Pixels p ch {:keys [^long min-size ^long max-size ^double threshold]
                 :or {mins 4 maxs 256 thr 15.0}}]
  (let [ww (bit-shift-left 1 (m/high-2-exp (.w p)))
        hh (bit-shift-left 1 (m/high-2-exp (.h p)))
        mins (max 2 min-size)

        segmf (fn local-segmentation
                [^long x ^long y ^long size res]
                (if (or (>= x (.w p)) (>= y (.h p)))
                  res
                  (lazy-seq
                   (let [^double stdev (calc-stdev p ch x y size size)]
                     (if (or (> size max-size)
                             (and (> size mins)
                                  (> stdev threshold)))
                       (let [mid (long (/ size 2))]
                         (->> res
                              (local-segmentation (+ x mid) (+ y mid) mid)
                              (local-segmentation x (+ y mid) mid)
                              (local-segmentation (+ x mid) y mid)
                              (local-segmentation x y mid)))
                       (cons [x y size] res))))))]
    (segmf 0 0 (max ww hh) nil)))
