(ns examples.GG.P.P-2-1-2-02
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]
            [clojure2d.color :as c]))

(def ^:const ^int scl (/ 600 20))

(defn draw
  "Draw circles"
  [canvas window _ _]
  (let [rng (r/make-randomizer :default (:seed (get-state window)))
        mx (mouse-x window)
        my (mouse-y window)
        radius-b (:radius-b (get-state window))
        radius-f (:radius-f (get-state window))]
    (-> canvas
        (set-background :white)
        (set-color 0 0 0 180)
        (translate (* 0.5 scl) (* 0.5 scl)))
    (doseq [grid-y (range 20)
            grid-x (range 20)]
      (let [px (* scl grid-x)
            py (* scl grid-y) 
            shift-x (/ (r/drandom rng (- mx) mx) 20)
            shift-y (/ (r/drandom rng (- my) my) 20)]
        (-> canvas
            (set-color (c/set-alpha (:color-b (get-state window)) (:alpha-b (get-state window))))
            (ellipse (+ px shift-x) (+ py shift-y) radius-b radius-b)
            (set-color (c/set-alpha (:color-f (get-state window)) (:alpha-f (get-state window))))
            (ellipse px py radius-f radius-f))))))

(def default-cfg
  {:seed 0
   :radius-b 30
   :radius-f 15
   :color-b (c/to-color :black)
   :color-f (c/to-color :white)
   :alpha-b 255
   :alpha-f 255})

(defn from-hsb
  "Convert from hsb to rgb"
  [h s b]
  (c/from-HSB (c/make-color (* 255.0 (/ h 360.0)) (* 255.0 (/ s 100.0)) (* 255.0 (/ b 100.0)))))

(def window (show-window {:canvas (make-canvas 600 600)
                          :window-name "P_2_1_2_02"
                          :draw-fn draw
                          :state default-cfg}))

(defmethod mouse-event [(:window-name window) :mouse-pressed] [_ s] (assoc s :seed (r/irand)))

(defmethod key-pressed [(:window-name window) \0] [_ _] default-cfg)
(defmethod key-pressed [(:window-name window) \3] [_ s]
  (if (= 255 (:alpha-f s))
    (assoc s :alpha-f 128 :alpha-b 128)
    (assoc s :alpha-f 255 :alpha-b 255)))

(defmethod key-pressed [(:window-name window) \1] [_ s]
  (if (= (c/make-color :black) (:color-b s))
    (assoc s :color-b (from-hsb 273 73 51))
    (assoc s :color-b (c/make-color :black))))

(defmethod key-pressed [(:window-name window) \2] [_ s]
  (if (= (c/make-color :white) (:color-f s))
    (assoc s :color-f (from-hsb 323 100 77))
    (assoc s :color-f (c/make-color :white))))

(defmethod key-pressed [(:window-name window) virtual-key] [e s]
  (condp = (key-code e)
    :up (update s :radius-b + 2.0)
    :down (update s :radius-b #(max 10 (- % 2.0)))
    :right (update s :radius-f + 2.0)
    :left (update s :radius-f #(max 5 (- % 2.0)))
    s))

