(ns examples.NOC.introduction.gaussian-I-4
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn draw
  ""
  [canvas _ _ _]
  (let [xloc (r/grand (* 0.5 ^int (width canvas)) 60)]

    (-> canvas
        (set-color :black 10)
        (ellipse xloc (* 0.5 ^int (height canvas)) 16 16))))

(let [canvas (make-canvas 640 360)]

  (with-canvas canvas
    (set-background :white))
  
  (show-window canvas "Random Walk - Gaussian" draw))
