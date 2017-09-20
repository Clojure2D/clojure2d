;; Old sketch reimplementation
;; https://www.openprocessing.org/sketch/151044

(ns examples.ex31-wind
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def canvas (create-canvas 500 500))

(def ^:const ^double s (/ m/TWO_PI 120.0)) ;; speed
(def ^:const ^double phase-scale (/ m/PI 400.0)) 

(defn random-c
  "Distortion and alpha - pencil look"
  ^double []
  (* 0.5 ^double (r/drand) (m/qsin (r/drand m/TWO_PI))))

(defn draw
  "Wind algorithm "
  [canvas window _ state]
  (let [^double a (or state 0.0)]
    (set-background canvas 226 210 184)
    (dotimes [j 16]
      (dotimes [i 400]
        (let [jj (+ 50 (* j 25))
              ii (+ 50 i)
              step (* (m/sin (* m/TWO_PI ^double (r/noise (/ a 40.0)))) (m/sin (* (- 450 ii) phase-scale)))
              swing (->> 50.0
                         (- (* 100.0 ^double (r/noise (+ a (/ ii 200.0))
                                                      (+ a (/ jj 300.0))
                                                      (/ a 10.0))))
                         (* step)
                         (+ jj))
              dx (random-c)
              dy (random-c)
              x (+ ii dx dx)
              y (+ swing dy dy)] 
          (set-color canvas 20 20 20 (- 150 (* 150 (m/hypot dx dy))))
          (ellipse canvas x y 2 2))))
    (+ a s)))

(def window (show-window canvas "Wind blows" draw))
