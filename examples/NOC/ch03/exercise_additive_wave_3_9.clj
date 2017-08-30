(ns NOC.ch03.exercise-additive-wave-3-9
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 640)
(def ^:const ^int h 360)

(def ^:const ^int xspacing 8)
(def ^:const ^int maxwaves 5)

;; number of blobs
(def ^:const ^int ylen (/ (+ w 16) xspacing))

;; generate random wave data (xstep and amplitude)
(def waves-data (map (fn [_] (vector (r/drand 10 30) (* xspacing (/ m/TWO_PI (r/drand 100 300))))) (range maxwaves)))

(defn generate-wave
  "Calculate wave values for given amp, theta, dx and function."
  [len [^double amp ^double dx] f theta]
  (second (reduce (fn [state _] (let [[^double x l] state
                                      v (* amp ^double (f x))]
                                  [(+ x dx) (conj l v)])) [theta []] (range len))))

(defn calc-waves
  "Calculate waves values for each point based on waves data"
  [wdata theta]
  (map-indexed #(generate-wave ylen %2 (if (even? %1) m/sin m/cos) theta) wdata))

(defn draw
  "Sum waves values and draw"
  [canvas _ ^long fc _]
  (let [wave (apply map + (calc-waves waves-data (* fc 0.02)))]

    (-> canvas
        (set-background :black)
        (set-color :white 128))

    (doall (map-indexed #(let [^long x %1
                               ^double y %2]
                           (ellipse canvas (* x xspacing) (+ (* h 0.5) y) 16 16)) wave))))

(def canvas (make-canvas w h))
(def window (show-window canvas "Excercise additive wave 3_9" draw))
