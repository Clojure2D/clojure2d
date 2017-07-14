(ns example.NOC.introduction.gaussian2
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn draw
  "Draw on canvas."
  [canvas window framecount state]
  (let [r (r/grand 100 100) ;; setting mean and stdev is built into the library 
        g (r/grand 200 20)
        b (r/grand 0 50)
        xloc (r/grand (* 0.5 ^double (width canvas)) (* 0.1 ^double (width canvas)))
        yloc (r/grand (* 0.5 ^double (width canvas)) (* 0.1 ^double (width canvas)))]

    (-> canvas
        (set-background 0 0 0 1)
        (set-color r g b)
        (ellipse xloc yloc 8 8))))

(show-window (make-canvas 200 200) "Gaussian2" 120 draw)
