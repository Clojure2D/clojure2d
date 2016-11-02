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
   (let [limit (max (int (* 0.1 w h)) 4)]
     (loop [A 0.0
            Q 0.0
            k 1]
       (let [posx (m/irand w)
             posy (m/irand h)
             xk (p/get-value pixels ch (+ sx posx) (+ sy posy))
             newA (+ A (/ (- xk A) k))
             newQ (+ Q (* (- xk A) (- xk newA)) )]
         (if (< k limit)
           (recur newA newQ (inc k))
           (m/sqrt (/ Q (dec limit)))))))))

(defn segment-pixels-divide
  ""
  [^Pixels p ch mins maxs thr]
  (let [ww (bit-shift-left 1 (m/high-2-exp (.w p)))
        hh (bit-shift-left 1 (m/high-2-exp (.h p)))
        mins (max 2 mins)

        segmf (fn local-segmentation
                [x y size res]
                (if (or (>= x (.w p)) (>= y (.h p)))
                  res
                  (lazy-seq
                   (let [stdev (calc-stdev p ch x y size size)]
                     (if (or (> size maxs)
                             (and (> size mins)
                                  (> stdev thr)))
                       (let [mid (/ size 2)]
                         (->> res
                              (local-segmentation (+ x mid) (+ y mid) mid)
                              (local-segmentation x (+ y mid) mid)
                              (local-segmentation (+ x mid) y mid)
                              (local-segmentation x y mid)))
                       (cons [x y size] res))))))]
    (segmf 0 0 (max ww hh) nil)))
