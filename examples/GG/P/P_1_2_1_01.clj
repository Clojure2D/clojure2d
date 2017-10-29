(ns GG.P.P-1-2-1-01
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.color :as c]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v]))

(def ^:const ^int w 800)
(def ^:const ^int h 800)

(defn tile-count
  "Get number of tiles"
  ^long [v l mx]
  (long (m/cnorm v 0 l 2 mx)))

(defn draw
  "Draw tiles"
  [canvas window _ _]
  (let [tile-count-x (tile-count (mouse-x window) w 100)
        tile-count-y (tile-count (mouse-y window) h 10)
        tile-width (/ (double w) tile-count-x)
        tile-height (/ (double h) tile-count-y)
        state (get-state window)
        colors-left (:colors-left state)
        colors-right (:colors-right state)]
    (dotimes [gridy tile-count-y]
      (let [col1 (colors-left gridy)
            col2 (colors-right gridy)]
        (dotimes [gridx tile-count-x]
          (let [amount (m/norm gridx 0.0 (dec tile-count-x) 0.0 1.0)
                inter-col (if-not (:interpolate-shortest state)
                            (c/from-HSB (v/interpolate col1 col2 amount))
                            (v/interpolate (c/from-HSB col1) (c/from-HSB col2) amount))]
            (-> canvas
                (set-color inter-col)
                (rect (* tile-width gridx) (* tile-height gridy) tile-width tile-height))))))))

(defn shake-colors
  "Generate colors"
  []
  {:colors-left (vec (repeatedly 10 #(c/make-color (r/drand 43) (r/drand 255) 255)))
   :colors-right (vec (repeatedly 10 #(c/make-color (r/drand 114 135) 255 (r/drand 255))))})

(def window (show-window {:canvas (make-canvas w h)
                          :window-name "P_1_2_1_01"
                          :draw-fn draw
                          :state (assoc (shake-colors) :interpolate-shortest true)}))

(defmethod mouse-event [(:window-name window) :mouse-released] [_ s]
  (merge s (shake-colors)))

(defmethod key-released [(:window-name window) \1] [_ s]
  (assoc s :interpolate-shortest true))

(defmethod key-released [(:window-name window) \2] [_ s]
  (assoc s :interpolate-shortest false))
