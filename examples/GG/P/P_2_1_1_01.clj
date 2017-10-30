(ns examples.GG.P.P-2-1-1-01
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]))

(def ^:const ^double scl (/ 600.0 20.0))

(defn draw
  "Draw lines"
  [canvas window _ _]
  (let [cap (:cap (get-state window))
        wx (max 0.1 (/ (mouse-x window) 20.0))
        wy (max 0.1 (/ (mouse-y window) 20.0))
        rng (r/make-randomizer :default (:seed (get-state window)))]
    (set-background canvas :white)
    (set-color canvas :black)
    (doseq [grid-x (range 20)
            grid-y (range 20)]
      (let [px (* grid-x scl)
            py (* grid-y scl)]
        (if (r/brandom rng 0.5)
          (-> canvas
              (set-stroke wx cap)
              (line px py (+ px scl) (+ py scl)))
          (-> canvas
              (set-stroke wy cap)
              (line px (+ py scl) (+ px scl) py)))))))


(def window (show-window {:canvas (make-canvas 600 600)
                          :draw-fn draw
                          :window-name "P_2_1_1_01"
                          :state {:cap java.awt.BasicStroke/CAP_ROUND
                                  :seed 0}}))

(defmethod key-pressed [(:window-name window) \1] [_ s] (assoc s :cap java.awt.BasicStroke/CAP_ROUND))
(defmethod key-pressed [(:window-name window) \2] [_ s] (assoc s :cap java.awt.BasicStroke/CAP_SQUARE))
(defmethod key-pressed [(:window-name window) \3] [_ s] (assoc s :cap java.awt.BasicStroke/CAP_BUTT))

(defmethod mouse-event [(:window-name window) :mouse-pressed] [_ s] (assoc s :seed (r/irand)))
