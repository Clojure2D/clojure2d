(ns examples.GG.P.P-2-1-1-02
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]
            [clojure2d.color :as c]))

(def ^:const ^double scl (/ 600.0 20.0))

(defn draw
  "Draw lines"
  [canvas window _ _]
  (let [cap (:cap (get-state window))
        wx (max 0.1 (/ (mouse-x window) 10.0))
        wy (max 0.1 (/ (mouse-y window) 10.0))
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
              (set-color (c/set-alpha (:color-left (get-state window)) (:alpha-left (get-state window))))
              (line px py (+ px scl) (+ py scl)))
          (-> canvas
              (set-stroke wy cap)
              (set-color (c/set-alpha (:color-right (get-state window)) (:alpha-right (get-state window))))
              (line px (+ py scl) (+ px scl) py)))))))


(def window (show-window {:canvas (make-canvas 600 600)
                          :draw-fn draw
                          :window-name "P_2_1_1_02"
                          :state {:cap java.awt.BasicStroke/CAP_ROUND
                                  :seed 0
                                  :alpha-left 255
                                  :alpha-right 255
                                  :color-left (c/make-color 197 0 123)
                                  :color-right (c/make-color 87 35 129)}}))

(defmethod key-pressed [(:window-name window) \1] [_ s] (assoc s :cap java.awt.BasicStroke/CAP_ROUND))
(defmethod key-pressed [(:window-name window) \2] [_ s] (assoc s :cap java.awt.BasicStroke/CAP_SQUARE))
(defmethod key-pressed [(:window-name window) \3] [_ s] (assoc s :cap java.awt.BasicStroke/CAP_BUTT))

(defmethod key-pressed [(:window-name window) \4] [_ s]
  (if (= (c/to-color :black) (:color-left s))
    (assoc s :color-left (c/from-HSB (c/make-color 230 255 197)))
    (assoc s :color-left (c/to-color :black))))

(defmethod key-pressed [(:window-name window) \5] [_ s]
  (if (= (c/to-color :black) (:color-right s))
    (assoc s :color-right (c/from-HSB (c/make-color 194 187 130)))
    (assoc s :color-right (c/to-color :black))))

(defmethod key-pressed [(:window-name window) \6] [_ s]
  (if (= (:alpha-left s) 255)
    (assoc s :alpha-left 128)
    (assoc s :alpha-left 255)))

(defmethod key-pressed [(:window-name window) \7] [_ s]
  (if (= (:alpha-right s) 255)
    (assoc s :alpha-right 128)
    (assoc s :alpha-right 255)))

(defmethod key-pressed [(:window-name window) \0] [_ s]
  (merge s {:cap java.awt.BasicStroke/CAP_ROUND
            :alpha-left 255
            :alpha-right 255
            :color-left (c/to-color :black)
            :color-right (c/to-color :black)}))

(defmethod mouse-event [(:window-name window) :mouse-pressed] [_ s] (assoc s :seed (r/irand)))
