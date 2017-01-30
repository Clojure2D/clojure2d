;; segment pixels into squares of similar values

(ns clojure2d.extra.segmentation
  (:require [clojure2d.core :as core]
            [clojure2d.math :as m]
            [clojure2d.pixels :as p])
  (:import [clojure2d.pixels Pixels]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn calc-stdev
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
       (let [^int posx (m/irand w)
             ^int posy (m/irand h)
             xk (double (p/get-value pixels ch (+ sx posx) (+ sy posy)))
             newA (+ A (/ (- xk A) k))
             newQ (+ Q (* (- xk A) (- xk newA)) )]
         (if (< k limit)
           (recur newA newQ (inc k))
           (m/sqrt (/ Q (dec limit)))))))))

(defn segment-pixels-divide
  ""
  [^Pixels p ch mins maxs thr]
  (let [ww (bit-shift-left 1 ^long (m/high-2-exp (.w p)))
        hh (bit-shift-left 1 ^long (m/high-2-exp (.h p)))
        mins (max 2 ^long mins)
        ^long maxs maxs
        ^double thr thr

        segmf (fn local-segmentation
                [^long x ^long y ^long size res]
                (if (or (>= x (.w p)) (>= y (.h p)))
                  res
                  (lazy-seq
                   (let [^double stdev (calc-stdev p ch x y size size)]
                     (if (or (> size maxs)
                             (and (> size mins)
                                  (> stdev thr)))
                       (let [mid (long (/ size 2))]
                         (->> res
                              (local-segmentation (+ x mid) (+ y mid) mid)
                              (local-segmentation x (+ y mid) mid)
                              (local-segmentation (+ x mid) y mid)
                              (local-segmentation x y mid)))
                       (cons [x y size] res))))))]
    (segmf 0 0 (max ww hh) nil)))
