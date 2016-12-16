(ns color-test
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.color :as c])
  (:import [java.awt Color]
           [clojure2d.math.vector Vec2 Vec4]))

(def canvas (create-canvas 500 500))

(def windows (show-window canvas "palettes" 500 500 15))

(defn do-it [canvas]
  (dotimes [x 500]
    (dotimes [y 500]
      
      (let [xx (- x 250)
            yy (- y 250)
            v (Vec2. xx yy)
            len (v/mag v)]
        (when (< len 250)
          (let [h 0
                c (m/norm len 0.0 250.0 255.0 0.0)
                l (m/norm (m/sin (v/heading v)) -1.0 1.0 0.0 255.0)
                col (c/from-HCL (Vec4. h l c 255.0))]
            (set-color canvas (c/to-color col))
            (rect canvas x y 1 1))
          ))
      
      )))

(with-canvas canvas
  (set-background Color/black)
  (do-it))
