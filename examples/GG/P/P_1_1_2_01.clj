(ns GG.P.P-1-1-2-01
  (:require [clojure2d.core :refer :all]
            [clojure2d.color :as c]
            [clojure2d.math :as m]))

(def ^:const hue-scale (/ 255.0 360.0))

(defn draw
  "Draw colorful triangle fan"
  [canvas window _ _]
  (when (mouse-in-window? window)
    (let [s (m/norm (mouse-x window) 0 (width window) 0.0 255.0)
          b (m/norm (mouse-y window) 0 (height window) 0.0 255.0)
          angle-step (/ 360 (get-state window))
          midx (* 0.5 (width canvas))
          midy (* 0.5 (height canvas))
          angles (map #(vector % (+ midx (* 300.0 (m/cos (m/radians %))))
                               (+ midy (* 300.0 (m/sin (m/radians %))))) (range 0.0 (+ 360.0 angle-step) angle-step))]
      (set-background canvas :white)
      (doseq [[[angle v1 v2] [_ v3 v4]] (map #(vector %1 %2) angles (next angles))]
        (-> canvas
            (set-color (c/from-HSB (c/make-color (* hue-scale angle) s b)))
            (triangle midx midy v1 v2 v3 v4))))))

(def window (show-window {:canvas (make-canvas 800 800 :low)
                          :window-name "P_1_1_2_01"
                          :draw-fn draw
                          :state 360.0}))

(defmethod key-pressed [(:window-name window) \1] [_ _] 360.0)
(defmethod key-pressed [(:window-name window) \2] [_ _] 45.0)
(defmethod key-pressed [(:window-name window) \3] [_ _] 24.0)
(defmethod key-pressed [(:window-name window) \4] [_ _] 12.0)
(defmethod key-pressed [(:window-name window) \5] [_ _] 6.0)
