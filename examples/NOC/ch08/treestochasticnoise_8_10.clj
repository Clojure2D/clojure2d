(ns examples.NOC.ch08.treestochasticnoise-8-10
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]))

(def canvas (make-canvas 800 200))

(defn branch
  ""
  [canvas ^double h ^double xoff ^double yoff]
  (let [sw (m/norm h 2.0 120.0 1.0 5.0)
        newh (* 0.66 h)
        maxi (r/irand 5)
        mult (if (even? maxi) 1.0 -1.0)]
    (-> canvas
        (set-stroke sw)
        (line 0 0 0 (- h))
        (translate 0 (- h)))

    (when (> newh 4.0)
      (dotimes [i maxi]
        (let [theta (m/norm (r/noise (+ i xoff) yoff) 0 1 (- m/THIRD_PI) m/THIRD_PI)]
          (-> canvas
              (push-matrix)
              (rotate (* mult theta))
              (branch newh (+ xoff 0.1) yoff)
              (pop-matrix))))))
  canvas)

(defn draw
  ""
  [canvas window ^long fps _]
  (r/seed r/default-random (get-state window))
  (-> canvas
      (set-background :white)
      (set-color :black)
      (text "Click mouse to generate a new tree" 10 (- (height canvas) 10))
      (translate (/ (width canvas) 2.0) (height canvas))
      (branch 60.0 0.0 (* fps 0.0013))))

(def window (show-window {:canvas canvas
                          :window-name "Tree Stochastic - noise 8_10"
                          :draw-fn draw
                          :state (r/irand)}))

(defmethod mouse-event [(:window-name window) :mouse-pressed] [_ _] (r/irand))


