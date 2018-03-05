(ns noise
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r])
  (:import [clojure2d.java.noise GradientNoise NoiseConfig ValueNoise SimplexNoise]))

(def canvas (make-canvas 500 500))

(def window (show-window canvas "noise"))

(let [cfg (NoiseConfig. 133 0 5 2.0 0.5 true)]
  (with-canvas [c canvas]
    (dotimes [x 500]
      (dotimes [y 500]
        (let [n (* 255.0 (SimplexNoise/fbm cfg (/ x 50.0) (/ y 50.0)))]
          (set-color c n n n)
          (rect c x y 1 1))))))

(let [cfg (NoiseConfig. (r/irand) 3 6 2.0 0.5 true)
      mn (reduce min (repeatedly 1000000 #(SimplexNoise/value cfg 0 (r/drand -512 512))))
      mx (reduce max (repeatedly 1000000 #(SimplexNoise/value cfg 0 (r/drand -512 512))))]
  [mn mx])

