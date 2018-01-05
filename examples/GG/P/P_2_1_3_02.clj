(ns examples.GG.P.P-2-1-3-02
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]))

(def ^:const ^int cnt 10)

(defn next-pair
  "Looping helper"
  [[side i]]
  (let [ni (inc i)]
    (if (< ni cnt)
      [side ni]
      (let [nside (inc side)]
        (if (< nside 4)
          [nside 0]
          nil)))))

(defn draw
  "Draw lines"
  [canvas window _ _]
  (let [draw-mode (get-state window)
        mx (m/abs (mouse-x window))
        tile-count-x (inc (int (/ mx 30)))
        tile-count-y (inc (int (/ (m/abs (mouse-y window)) 30)))]
    (set-background canvas (if (< draw-mode 3) :white :black))
    (set-stroke canvas 0.5 )
    (dotimes [grid-y (inc tile-count-y)]
      (dotimes [grid-x (inc tile-count-x)]
        (let [tile-width (/ (width canvas) tile-count-x)
              tile-height (/ (height canvas) tile-count-y)
              pos-x (* tile-width grid-x)
              pos-y (* tile-height grid-y)
              x1 (* 0.5 tile-width)
              y1 (* 0.5 tile-height)]
          
          (push-matrix canvas)
          (translate canvas pos-x pos-y)
          (loop [[side i] [0 0]
                 x2 0
                 y2 0
                 line-weight 0
                 stroke-color 0]
            (let [[nx2 ny2] (case (int side)
                              0 [(+ x2 (/ tile-width cnt)) 0.0]
                              1 [tile-width (+ y2 (/ tile-height cnt))]
                              2 [(- x2 (/ tile-width cnt)) tile-height]
                              3 [0 (- y2 (/ tile-height cnt))])
                  [nline-weight nstroke-color] (if (< i (/ cnt 2))
                                                 [(inc line-weight) (+ stroke-color 42.666666)]
                                                 [(dec line-weight) (- stroke-color 42.666666)])]
              (case (int draw-mode)
                1 (set-color canvas :black)
                2 (do
                    (set-color canvas :black)
                    (set-stroke canvas line-weight))
                3 (do
                    (set-color canvas stroke-color stroke-color stroke-color)
                    (set-stroke canvas (/ mx 100))))

              (line canvas x1 y1 x2 y2)
              
              (when-let [pair (next-pair [side i])]
                (recur pair nx2 ny2 nline-weight nstroke-color))))
          
          (pop-matrix canvas))))))


(def window (show-window {:canvas (make-canvas 600 600 :highest)
                          :draw-fn draw
                          :window-name "P_2_1_3_02"
                          :state 1}))

(defmethod key-pressed [(:window-name window) \1] [_ _] 1)
(defmethod key-pressed [(:window-name window) \2] [_ _] 2)
(defmethod key-pressed [(:window-name window) \3] [_ _] 3)

