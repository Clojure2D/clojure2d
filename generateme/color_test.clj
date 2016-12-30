(ns color-test
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.color :as c])
  (:import [java.awt Color]
           [clojure2d.math.vector Vec2 Vec3 Vec4]))

(def canvas (create-canvas 500 500))

(def windows (show-window canvas "palettes" 500 500 15))

(defn do-it [canvas]
  (dotimes [x 500]
    (dotimes [y 500]
      (let [h 10
            ks (m/norm x 0 500 0.0 2.0)
            kv (m/norm y 0 500 2.0 0.0)
            col (c/paletton-hsv-to-rgb h ks kv)]
        (set-color canvas (c/to-color col))
        (rect canvas x y 1 1))))
  (doseq [[ks kv] (:pastels c/paletton-presets)]
    (let [x (m/norm ks 0.0 2.0 0 500)
          y (m/norm kv 2.0 0.0 0 500)]
      (set-color canvas (Color. 0 0 0 100))
      (ellipse canvas x y 8 8)
      (set-color canvas (Color. 255 255 255 25))
      (ellipse canvas x y 5 5))))

(with-canvas canvas
  (set-background Color/black)
  (do-it))




