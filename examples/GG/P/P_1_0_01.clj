(ns examples.GG.P.P-1-0-01
  (:require [clojure2d.core :refer :all]
            [clojure2d.color :as c]))

(def ^:const hue-scale (/ 255.0 360.0))

(defn draw
  "Draw rectangle"
  [canvas window _ _]
  (when (mouse-in-window? window)
    (let [mx (mouse-x window)
          my (mouse-y window)]
      (-> canvas
          (set-background (c/from-HSB (c/make-color (* hue-scale (* 0.5 my)) 255 255)))
          (set-color (c/from-HSB (c/make-color (* hue-scale (- 360.0 (* 0.5 my))) 255 255)))
          (crect 360 360 (inc mx) (inc mx))))))

(show-window (make-canvas 720 720) "P_1_0_01" draw)
