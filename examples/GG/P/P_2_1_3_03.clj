(ns examples.GG.P.P-2-1-3-03
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]))

(defn draw
  "Draw lines"
  [canvas window _ _]
  (let [[draw-mode tile-count-x tile-count-y] (get-state window)
        mx (m/abs (mouse-x window))
        my (m/abs (mouse-y window))
        cnt (+ 5 (int (/ mx 20)))
        para (- (/ my (float (height canvas))) 0.5)]
    (set-background canvas :white)
    (set-color canvas :black)
    (dotimes [grid-y (inc tile-count-y)]
      (dotimes [grid-x (inc tile-count-x)]
        (let [tile-width (/ (width canvas) tile-count-x)
              tile-height (/ (height canvas) tile-count-y)
              pos-x (+ (* 0.5 tile-width) (* tile-width grid-x))
              pos-y (+ (* 0.5 tile-height) (* tile-height grid-y))]
          
          (push-matrix canvas)
          (translate canvas pos-x pos-y)
          
          (case draw-mode
            1 (do
                (translate canvas (* -0.5 tile-width) (* -0.5 tile-height))
                (dotimes [i cnt]
                  (line canvas 0 (* (+ para 0.5) tile-height) tile-width (/ (* i tile-height) cnt))
                  (line canvas 0 (/ (* i tile-height) cnt) tile-width (- tile-height (* (+ para 0.5) tile-height)))))
            2 (dotimes [i (inc cnt)]
                (-> canvas
                    (line (* para tile-width) (* para tile-height) (/ tile-width 2) (* (- (/ i cnt) 0.5) tile-height))
                    (line (* para tile-width) (* para tile-height) (/ tile-width -2) (* (- (/ i cnt) 0.5) tile-height))
                    (line (* para tile-width) (* para tile-height) (* (- (/ i cnt) 0.5) tile-width) (/ tile-height 2))
                    (line (* para tile-width) (* para tile-height) (* (- (/ i cnt) 0.5) tile-width) (/ tile-height -2))))
            3 (dotimes [i (inc cnt)]
                (-> canvas
                    (line 0 (* para tile-height) (/ tile-width 2) (* (- (/ i cnt) 0.5) tile-height))
                    (line 0 (* para tile-height) (/ tile-width -2) (* (- (/ i cnt) 0.5) tile-height))
                    (line 0 (* para tile-height) (* (- (/ i cnt) 0.5) tile-width) (/ tile-height 2))
                    (line 0 (* para tile-height) (* (- (/ i cnt) 0.5) tile-width) (/ tile-height -2)))))
          
          (pop-matrix canvas))))))

(def window (show-window {:canvas (make-canvas 600 600)
                          :draw-fn draw
                          :window-name "P_2_1_3_03"
                          :state [1 6 6]}))

(defmethod key-pressed [(:window-name window) \1] [_ [_ a b]] [1 a b])
(defmethod key-pressed [(:window-name window) \2] [_ [_ a b]] [2 a b])
(defmethod key-pressed [(:window-name window) \3] [_ [_ a b]] [3 a b])
(defmethod key-pressed [(:window-name window) virtual-key] [e [dm tcx tcy]]
  (case (key-code e)
    :down [dm tcx (max (dec tcy) 1)]
    :up [dm tcx (inc tcy)]
    :left [dm (max (dec tcx) 1) tcy]
    :right [dm (inc tcx) tcy]
    [dm tcx tcy]))

(with-canvas (make-canvas 600 600)
  (draw window 0 0))
