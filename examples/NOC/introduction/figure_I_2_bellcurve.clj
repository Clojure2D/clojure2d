(ns example.NOC.introduction.figure-I-2-bellcurve
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.color :as c])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn calc-bell
  ""
  [i w h ^double sd]
  (let [^double xcoord (m/norm i 0 w -3.0 3.0)
        sdsq (* sd sd)
        xmsq (* -1.0 xcoord xcoord)]
    (m/norm (* (m/pow m/E (/ xmsq sdsq))
               (/ 1.0 (* sd m/SQRT2PI))) 0.0 1.0 (- ^int h 2.0) 2.0)))

(defn draw
  "Draw on canvas."
  [canvas window _ _]
  (let [^double sd (m/norm (mouse-x window) 0 (width window) 0.4 2)
        p (map #(Vec2. % (calc-bell % (width window) (height window) sd)) (range 0 (width canvas) 2))]

    (-> canvas
        (set-background c/:white)
        (set-color c/:black)
        (set-stroke 2.0)
        (path p))))

(def window (show-window (make-canvas 400 200) "Noise 1d graph" draw))
