(ns noise
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]
            [clojure2d.math :as m])
  (:import [clojure2d.java.noise NoiseConfig Noise Billow FBM RidgedMulti]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def canvas (make-canvas 500 500))

(def window (show-window canvas "noise"))

(let [noi (r/make-single-noise {:interpolation :none})]
  (with-canvas [c canvas]
    (dotimes [x 500]
      (dotimes [y 500]
        (let [n (* 255.0 ^double (r/vnoise (/ x 50.0) (/ y 50.0)))]
          (set-color c n n n)
          (rect c x y 1 1))))))

(let [cfg (NoiseConfig. (r/irand) 1 3 6 2.0 0.5 true)
      mn1 (reduce clojure.core/min (repeatedly 1000000 #(r/noise (r/drand -512 512))))
      mx1 (reduce clojure.core/max (repeatedly 1000000 #(r/noise (r/drand -512 512))))
      mn2 (reduce clojure.core/min (repeatedly 1000000 #(r/noise (r/drand -512 512) (r/drand -512 512))))
      mx2 (reduce clojure.core/max (repeatedly 1000000 #(r/noise (r/drand -512 512) (r/drand -512 512))))
      mn3 (reduce clojure.core/min (repeatedly 1000000 #(r/noise (r/drand -512 512) (r/drand -512 512) (r/drand -512 512))))
      mx3 (reduce clojure.core/max (repeatedly 1000000 #(r/noise (r/drand -512 512) (r/drand -512 512) (r/drand -512 512))))]
  [mn1 mx1 mn2 mx2 mn3 mx3])

