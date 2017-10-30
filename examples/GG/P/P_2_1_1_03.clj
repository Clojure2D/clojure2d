(ns examples.GG.P.P-2-1-1-03
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]
            [clojure2d.color :as c]))

(defn draw
  "Draw lines"
  [canvas window _ _]
  (let [tile-count (max 0.01 (int (/ (mouse-y window) 15)))
        scl (/ (width canvas) tile-count)
        rng (r/make-randomizer :default (:seed (get-state window)))]
    (set-background canvas :white)
    (set-stroke canvas (max 0.1 (int (/ (mouse-x window) 15))))
    (doseq [grid-x (range tile-count)
            grid-y (range tile-count)]
      (let [px (* grid-x scl)
            py (* grid-y scl)
            alpha-left (if (:transparent-left (get-state window))
                         (* 25.0 grid-y)
                         255)
            alpha-right (if (:transparent-right (get-state window))
                          (- 255 (* 25.0 grid-y))
                          255)]
        (if (r/brandom rng 0.5)
          (-> canvas
              (set-color (c/set-alpha (:color-left (get-state window)) alpha-left))
              (line px py (+ px (* 0.5 scl)) (+ py scl))
              (line (+ px (* 0.5 scl)) py (+ px scl) (+ py scl)))
          (-> canvas
              (set-color (c/set-alpha (:color-right (get-state window)) alpha-right))
              (line px (+ py scl) (+ px (* 0.5 scl)) py)
              (line (+ px (* 0.5 scl)) (+ py scl) (+ px scl) py)))))))

(def window (show-window {:canvas (make-canvas 600 600)
                          :draw-fn draw
                          :window-name "P_2_1_1_03"
                          :state {:seed 0
                                  :transparent-left false
                                  :transparent-right false
                                  :color-left (c/from-HSB (c/make-color 230 255 197))
                                  :color-right (c/to-color :black)}}))

(defmethod key-pressed [(:window-name window) \1] [_ s]
  (if (= (c/from-HSB (c/make-color 194 187 139)) (:color-left s))
    (assoc s :color-left (c/from-HSB (c/make-color 230 255 197)))
    (assoc s :color-left (c/from-HSB (c/make-color 194 187 139)))))

(defmethod key-pressed [(:window-name window) \2] [_ s]
  (if (= (c/to-color :black) (:color-right s))
    (assoc s :color-right (c/from-HSB (c/make-color 136 255 164)))
    (assoc s :color-right (c/to-color :black))))

(defmethod key-pressed [(:window-name window) \3] [_ s]
  (update s :transparent-left not))

(defmethod key-pressed [(:window-name window) \4] [_ s]
  (update s :transparent-right not))

(defmethod key-pressed [(:window-name window) \0] [_ s]
  (merge s {:transparent-left false
            :transparent-right false
            :color-left (c/from-HSB (c/make-color 230 255 197))
            :color-right (c/to-color :black)}))

(defmethod mouse-event [(:window-name window) :mouse-pressed] [_ s] (assoc s :seed (r/irand)))
