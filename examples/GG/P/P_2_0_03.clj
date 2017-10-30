(ns GG.P.P-2-0-03
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.vector :as v]
            [clojure2d.math :as m]
            [clojure2d.color :as c]))

(defn draw
  "Draw figures"
  [canvas window _ _]
  (when (and (:mouse-pressed (get-state window)) (pos? (mouse-x window)))
    (let [circle-res (int (m/norm (+ 100 (mouse-y window)) 0 (height canvas) 2 10))
          radius (+ 0.5 (- (mouse-x window) (* 0.5 (width canvas))))
          angle (/ m/TWO_PI circle-res)
          p (map #(v/vec2 (* radius (m/cos (* % angle)))
                          (* radius (m/sin (* % angle)))) (range circle-res))]
      (-> canvas
          (set-stroke 2.0)
          (set-color (:stroke-color (get-state window)))
          (translate (* 0.5 (width window)) (* 0.5 (height window)))
          (path p true)))))

(def canvas (make-canvas 720 720))
(def window
  (do
    (with-canvas canvas (set-background :white))
    (show-window {:canvas canvas
                  :window-name "P_2_0_02"
                  :draw-fn draw
                  :fps 30
                  :state {:mouse-pressed false
                          :stroke-color (c/make-color 0 0 0 25)}})))

(defmethod mouse-event [(:window-name window) :mouse-pressed] [_ s] (assoc s :mouse-pressed true))
(defmethod mouse-event [(:window-name window) :mouse-released] [_ s] (assoc s :mouse-pressed false))

(defmethod key-pressed [(:window-name window) \backspace] [_ state]
  (with-canvas canvas (set-background :white))
  state)

(defmethod key-pressed [(:window-name window) \1] [_ s] (assoc s :stroke-color (c/make-color 0 0 0 25)))
(defmethod key-pressed [(:window-name window) \2] [_ s] (assoc s :stroke-color (c/from-HSB (c/make-color 136 255 164 25))))
(defmethod key-pressed [(:window-name window) \3] [_ s] (assoc s :stroke-color (c/from-HSB (c/make-color 37 255 182 25))))
