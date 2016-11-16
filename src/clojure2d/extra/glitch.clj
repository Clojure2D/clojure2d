(ns clojure2d.extra.glitch
  (:require [clojure2d.math :as m]
            [clojure2d.pixels :as p]
            [clojure2d.core :refer :all]
            [clojure2d.extra.signal :as s])
  (:import [clojure2d.pixels Pixels]))

;; Simple 2d SLITSCAN

(def freqs (vec (map #(bit-shift-left 1 %) (range 16))))
(def amps (vec (map #(/ 1.0 %) freqs)))

(defn make-random-waves
  ""
  []
  (s/make-sum-wave (->> (m/irand 2 6)
                        (range)
                        (map #(s/make-wave (rand-nth s/waves) (freqs %) (amps %) (m/drand 1)))
                        (filterv (fn [_] (m/brand 0.8))))))

(defn slitscan
  ""
  [fx fy ch ^Pixels p x y]
  (let [sx (/ 1.0 (.w p))
        sy (/ 1.0 (.h p))
        shiftx (* 0.5 (.w p) (fx (* x sx)))
        shifty (* 0.5 (.h p) (fy (* y sy)))
        xx (mod (int (+ x (.w p) shiftx)) (.w p))
        yy (mod (int (+ y (.h p) shifty)) (.h p))]
      (p/get-value p ch xx yy)))

(defn make-slitscan-filter
  ""
  ([]
   (partial p/filter-channel-xy (partial slitscan (make-random-waves) (make-random-waves))))
  ([fx fy]
   (partial p/filter-channel-xy (partial slitscan fx fy))))


