(ns examples.GG.P.P-2-1-3-04
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.color :as c]
            [clojure2d.math.vector :as v]))

(def color-mode-fn (c/make-color-converter c/from-HSB 360 100 100))

(defn draw
  "Draw lines"
  [canvas window _ _]
  (let [[draw-mode tile-count-x tile-count-y] (get-state window)
        mx (m/abs (mouse-x window))
        my (m/abs (mouse-y window))
        cnt (+ 10 (int (/ mx 10)))
        para (/ my (float (height canvas)))]
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
            1 (dotimes [i cnt]
                (-> canvas
                    (crect 0 0 tile-width tile-height true)
                    (scale (- 1.0 (/ 3.0 cnt)))
                    (rotate (* 0.1 para))))
            2 (dotimes [i (inc cnt)]
                (let [gradient (v/interpolate (c/make-color :black) (c/make-color 52 100 71) (/ i cnt))]
                  (-> canvas
                      (set-color (color-mode-fn gradient) (* 200.0 (/ i cnt)))
                      (rotate m/QUARTER_PI)
                      (crect 0 0 tile-width tile-height)
                      (scale (- 1.0 (/ 3.0 cnt)))
                      (rotate (* 1.5 para)))))
            3 (dotimes [i (inc cnt)]
                (let [gradient (v/interpolate (c/make-color 0 130 164) (c/make-color :white) (/ i cnt))]
                  (-> canvas
                      (set-color gradient 170)

                      (push-matrix)
                      (translate (* 4 i) 0)
                      (ellipse 0 0 (/ tile-width 4) (/ tile-height 4))
                      (pop-matrix)

                      (push-matrix)
                      (translate (* -4 i) 0)
                      (ellipse 0 0 (/ tile-width 4) (/ tile-height 4))
                      (pop-matrix)

                      (scale (- 1.0 (/ 1.5 cnt)))
                      (rotate (* 1.5 para))))))
          
          (pop-matrix canvas))))))

(def window (show-window {:canvas (make-canvas 550 550)
                          :draw-fn draw
                          :window-name "P_2_1_3_04"
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
