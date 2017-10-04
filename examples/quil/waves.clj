(ns examples.quil.waves
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^int w 500)
(def ^:const ^int h 500)
(def ^:const ^double mult (m/norm w 700 200 0.01 0.03))

(def ^:const start (v/vec2 0 h))
(def ^:const end (v/vec2 w h))

(defn calc-y [^double tm ^double x ^double mid ^double amp]
  (+ mid (* (m/sin (+ tm x)) amp)))

(defn wave [tm ^double step mid-y amp]
  "Calculate path"
  (conj (cons start
              (mapv #(let [t (* ^long % mult)
                           y (calc-y tm t mid-y amp)]
                       (v/vec2 % y)) (range (- w) (+ step w) step))) end))

(defn draw
  "Draw frames"
  [canvas _ ^long fps _]
  (let [t (* fps 0.05)]
    (set-background canvas 250 250 250)
    (let [move-down (/ h 5)
          amp (/ h 8)]
      (doseq [y (range move-down (+ amp h) 8)]
        (let [x-step (- (* ^long y 0.8) move-down)
              wv (wave t x-step y amp)]
          (-> canvas
              (set-color 50 230 (+ (* 20 (m/sin t)) 230) 40)
              (path wv false false)
              (set-color :white 250)
              (path wv)))))))

(def window (show-window (make-canvas w h) "Waves" draw))

