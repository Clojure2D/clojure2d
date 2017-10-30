(ns examples.GG.P.P-1-2-2-01
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]))

(defn make-sort-comparator
  "Compare colors"
  [f]
  (fn [c1 c2]
    (> ^double (f c1) ^double (f c2))))

(def hue-comparator (make-sort-comparator c/red))
(def saturation-comparator (make-sort-comparator c/green))
(def brightness-comparator (make-sort-comparator c/blue))
(def luma-comparator (make-sort-comparator (comp c/to-luma c/from-HSB)))

(defn draw
  "Draw tiles"
  [canvas window _ _]
  (let [img (:image (get-state window))
        tile-count (/ (width canvas) (max (mouse-x window) 5))
        rect-size (/ (width canvas) (double tile-count))
        grid-seq (for [grid-y (range tile-count)
                       grid-x (range tile-count)]
                   [(* grid-x rect-size) (* grid-y rect-size)])
        grid (map #(let [[px py] %] (p/get-color img px py)) grid-seq)
        sgrid (case (:sort-mode (get-state window))
                :hue (sort hue-comparator grid)
                :saturation (sort saturation-comparator grid)
                :brightness (sort brightness-comparator grid)
                :grayscale (sort luma-comparator grid)
                grid)]
    (dorun (map #(let [[px py] %1]
                   (set-color canvas (c/from-HSB %2))
                   (rect canvas px py rect-size rect-size)) grid-seq sgrid))))

(def window (show-window {:canvas (make-canvas 600 600)
                          :window-name "P_1_2_2_01"
                          :draw-fn draw
                          :state {:image (p/filter-colors c/to-HSB (p/load-pixels "examples/GG/data/pic1.jpg"))}}))

(defmethod key-released [(:window-name window) \1] [_ s] (assoc s :image (p/filter-colors c/to-HSB (p/load-pixels "examples/GG/data/pic1.jpg"))))
(defmethod key-released [(:window-name window) \2] [_ s] (assoc s :image (p/filter-colors c/to-HSB (p/load-pixels "examples/GG/data/pic2.jpg"))))
(defmethod key-released [(:window-name window) \3] [_ s] (assoc s :image (p/filter-colors c/to-HSB (p/load-pixels "examples/GG/data/pic3.jpg"))))

(defmethod key-released [(:window-name window) \4] [_ s] (assoc s :sort-mode nil))
(defmethod key-released [(:window-name window) \5] [_ s] (assoc s :sort-mode :hue))
(defmethod key-released [(:window-name window) \6] [_ s] (assoc s :sort-mode :saturation))
(defmethod key-released [(:window-name window) \7] [_ s] (assoc s :sort-mode :brightness))
(defmethod key-released [(:window-name window) \8] [_ s] (assoc s :sort-mode :grayscale))
