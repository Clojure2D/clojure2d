(ns examples.GG.P.P-1-2-3-01
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.color :as c]
            [clojure2d.math.vector :as v]
            [clojure2d.math.random :as r]))

(def ^:const ^int tile-count-x 50)
(def ^:const ^int tile-count-y 10)

(defn draw
  "Draw rectangles"
  [canvas window _ _]
  (let [colors (get-state window)
        current-tile-count-x (int (m/cnorm (mouse-x window) 0 (width canvas) 1 tile-count-x))
        current-tile-count-y (int (m/cnorm (mouse-y window) 0 (height canvas) 1 tile-count-y))
        tile-width (/ (width canvas) (double current-tile-count-x))
        tile-height (/ (height canvas) (double current-tile-count-y))]
    (dorun (map #(let [[pos-x pos-y] %1]
                   (set-color canvas (c/from-HSB %2))
                   (rect canvas pos-x pos-y tile-width tile-height))
                (for [grid-y (range tile-count-y)
                      grid-x (range tile-count-x)]
                  [(* tile-width grid-x) (* tile-height grid-y)])
                (cycle (take current-tile-count-x colors))))))

(def window (show-window {:canvas (make-canvas 800 800)
                          :window-name "P_1_2_3_01"
                          :draw-fn draw
                          :state (repeatedly tile-count-x #(v/vec4 (r/drand 255) (r/drand 255) (r/drand 255) 255))}))

(defmethod key-released [(:window-name window) \1] [_ _] (repeatedly tile-count-x #(v/vec4 (r/drand 256) (r/drand 256) (r/drand 256) 255)))
(defmethod key-released [(:window-name window) \2] [_ _] (repeatedly tile-count-x #(v/vec4 (r/drand 256) (r/drand 256) 255 255)))
(defmethod key-released [(:window-name window) \3] [_ _] (repeatedly tile-count-x #(v/vec4 (r/drand 256) 255 (r/drand 256) 255)))
(defmethod key-released [(:window-name window) \4] [_ _] (repeatedly tile-count-x #(v/vec4 0 0 (r/drand 256) 255)))
(defmethod key-released [(:window-name window) \5] [_ _] (repeatedly tile-count-x #(v/vec4 138 255 (r/drand 256) 255)))
(defmethod key-released [(:window-name window) \6] [_ _] (repeatedly tile-count-x #(v/vec4 138 (r/drand 256) 255 255)))
(defmethod key-released [(:window-name window) \7] [_ _] (repeatedly tile-count-x #(v/vec4 (r/drand 128) (r/drand 204 256) (r/drand 128 230) 255)))
(defmethod key-released [(:window-name window) \8] [_ _] (repeatedly tile-count-x #(v/vec4 (r/drand 128 256) (r/drand 204 256) (r/drand 128 230) 255)))
(defmethod key-released [(:window-name window) \9] [_ _] (map #(if (even? %)
                                                                 (v/vec4 (r/drand 256) 255 (r/drand 255) 255)
                                                                 (v/vec4 138 (r/drand 255) 255 255)) (range tile-count-x)))
(defmethod key-released [(:window-name window) \0] [_ _] (map #(if (even? %)
                                                                 (v/vec4 136 (r/drand 256) (r/drand 25 255) 255)
                                                                 (v/vec4 193 (r/drand 255) (r/drand 25 230) 255)) (range tile-count-x)))

