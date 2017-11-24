(ns GG.P.P-2-0-02
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.vector :as v]
            [clojure2d.math :as m]))

(defn draw
  "Draw figures"
  [canvas window _ _]
  (when (and (get-state window) (pos? (mouse-x window)))
    (let [circle-res (int (m/norm (+ 100 (mouse-y window)) 0 (height canvas) 2 10))
          radius (+ 0.5 (- (mouse-x window) (* 0.5 (width canvas))))
          angle (/ m/TWO_PI circle-res)
          p (map #(v/vec2 (* radius (m/cos (* % angle)))
                          (* radius (m/sin (* % angle)))) (range circle-res))]
      (-> canvas
          (set-stroke 2.0)
          (set-color :black 25)
          (translate (* 0.5 (width window)) (* 0.5 (height window)))
          (path p true)))))

(def canvas (make-canvas 720 720))
(def window
  (do
    (with-canvas-> canvas (set-background :white))
    (show-window {:canvas canvas
                  :window-name "P_2_0_02"
                  :draw-fn draw
                  :state false})))

(defmethod mouse-event [(:window-name window) :mouse-pressed] [_ _] true)
(defmethod mouse-event [(:window-name window) :mouse-released] [_ _] false)

(defmethod key-pressed [(:window-name window) \backspace] [_ state]
  (with-canvas-> canvas (set-background :white))
  state)
