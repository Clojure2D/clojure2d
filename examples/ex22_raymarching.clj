(ns examples.ex22-raymarching
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.color :as c]
            [clojure2d.extra.variations :as var])
  (:import [clojure2d.math.vector Vec2 Vec3]
           [java.awt Color]))

(def canvas (create-canvas 800 800))

(def windows (show-window canvas "raymarching" 800 800 25))

(def ^Vec3 ro (Vec3. 0.0 8.0 10.0))

(def ^:const mint 0.0)
(def ^:const maxt 25.0)

(defn cast-ray
  ""
  [^Vec3 rd f]
  (loop [i (int 0)
         t mint]
    (let [
          ^Vec3 p (v/add ro (v/mult rd t))
          diffh (- (.y p) (f (.x p) (.z p)))]
      (if (or (>= t maxt)
              (> i 256)
              (< diffh (* 0.000002 t)))
        t
        (recur (unchecked-inc i) (+ (* 0.5 diffh) t))))))

(def terrain (comp (var/make-variation :sinusoidal 1.0 {})
                   (var/make-variation :auger 1.0 {})
))

(defn terrain-f
  ""
  [x y]
  (let [^Vec2 v (terrain (Vec2. x y))]
    (* 8.0 (m/noise (* 0.08 (.x v)) (* 0.08 (.y v))))))

(def ^:const EPS 0.00001)

(defn normal-v
  ""
  [f ^Vec3 v]
  (v/normalize (Vec3. (- (f (- (.x v) EPS) (.z v))
                         (f (+ (.x v) EPS) (.z v)))
                      (+ EPS EPS)
                      (- (f (.x v) (- (.z v) EPS))
                         (f (.x v) (+ (.z v) EPS))))))

(def light (v/normalize (Vec3. -1.0 1.0 0.0)))

(def fog-color (Vec3. 120 120 120))
(defn calc-fog
  ""
  [t ^Vec3 col]
  (let [fo (- 1.0 (m/exp (* -0.001 t t t)))]
    (v/interpolate col fog-color fo)))

(dotimes [x 800]
  (let [xx (m/norm x 0.0 800.0 -2.0 2.0)]
    (dotimes [y 800]
      (let [yy (m/norm y 800.0 0.0 -2.0 2.0)
            ^Vec3 rd (Vec3. xx yy -2.0)
            t (cast-ray rd terrain-f)
            c (if (< t maxt)
                (let [n (normal-v terrain-f (v/add ro (v/mult rd t)))
                      d (v/dot n light)
                      cv (int (m/cnorm d 0 1.0 100 255))
                      cv2 (int (* 0.5 cv))
                      ^Vec3 col (Vec3. cv2 cv2 cv)]
                  (c/to-color3 (calc-fog t col)))
                Color/black)]
        (with-canvas canvas
          (set-color c)
          (rect x y 1 1))))))
