(ns exampes.GG.P.P-2-0-01
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]))

(defn draw
  "Draw lines"
  [canvas window _ _]
  (let [circle-res (int (m/norm (mouse-y window) 0 (height canvas) 2 80))
        radius (+ 0.5 (- (mouse-x window) (* 0.5 (width canvas))))
        angle (/ m/TWO_PI circle-res)]
    (-> canvas
        (set-stroke (/ (max 1.0 (mouse-y window)) 20.0) java.awt.BasicStroke/CAP_SQUARE)
        (set-background :white)
        (set-color :black)
        (translate (* 0.5 (width window)) (* 0.5 (height window))))
    (dotimes [i circle-res]
      (let [x (* radius (m/cos (* i angle)))
            y (* radius (m/sin (* i angle)))]
        (line canvas 0 0 x y)))))

(def window (show-window (make-canvas 550 550) "P_2_0_01" draw))
