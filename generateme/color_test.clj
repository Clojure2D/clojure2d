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


(defn xy2polar
  ""
  [x y]
  (let [n (m/hypot x y)
        r (m/atan2 y x)]
    [n (if (< r 0) (+ r m/TWO_PI) r)]))

(defn polar2xy
  ""
  [r theta]
  [(* r (m/cos theta))
   (* r (m/sin theta))])

(defn calc-limited
  ""
  [x y]
  (let [[r theta] (xy2polar x y)
        newr (m/constrain r 0.0 1.0)
        [newx newy] (polar2xy newr theta)]
    [newx newy r theta]))

(calc-limited 0.35 -0.51)

(defn get-sqr-polar
  ""
  [r theta]
  (let [e (max (m/abs (m/sin theta)) (m/abs (m/cos theta)))]
    [(/ r e) theta]))

(defn value-transform
  ""
  [thr x]
  (if (< x thr)
            (/ (inc x) (inc thr))
            (inc (/ (- x thr) (- 1.0 thr)))))


(defn get-value-transform
  ""
  [x y]
  (let [i (value-transform 0.5 x)
        o (value-transform 0.5 (- y))]
    [i o]))

(defn point-to-val
  ""
  [[x y r theta]]
  ())


(xy2polar 0.35 -0.51)



(defn paletton-move-main
  ""
  [x y]
  (let [[x y r theta] (calc-limited x y)
        [sr stheta] (get-sqr-polar r theta)
        [nx ny] (polar2xy sr stheta)]
    [nx ny sr stheta]))

(get-value-transform 0.42449282219350865 -0.6185466837676846)

(paletton-move-main 0.35 -0.51)
;; => [0.42449282219350865 -0.6185466837676846 0.7501960784313724 5.313843687244963]

(def paletton-base-values
  {:r  [[255.0,0.0,0.0]   [0.0 1.0 1.0]]
   :rg [[255.0,255.0,0.0] [120.0 1.0 1.0]]
   :g  [[0.0 255.0 0.0]   [180.0 1.0 0.8]]
   :gb [[0.0 255.0 255.0] [210.0 1.0 0.6]]
   :b  [[0.0 0.0 255.0]   [255.0 0.85 0.7]]
   :br [[255.0 0.0 255.0] [315.0 1.0 0.65]]})

(def paletton-base-hsv-data
  (let [s (fn [e t n] (if (== n -1.0) e
                          (+ e (/ (- t e) (inc n)))))
        i (fn [e t n] (if (== n -1.0) t
                          (+ t (/ (- e t) (inc n)))))]
    {120.0 {:a (:r paletton-base-values)
            :b (:rg paletton-base-values)
            :f (fn [e]
                 (if (== e 0.0) -1.0
                       (* 0.5 (m/tan (* m/HALF_PI (/ (- 120.0 e) 120.0))))))
            :g s
            :rgb (fn [e n r] (Vec4. e n r 255.0))}
     180.0 {:a (:rg paletton-base-values)
            :b (:g paletton-base-values)
            :f (fn [e]
                 (if (== e 180.0) -1.0
                     (* 0.5 (m/tan (* m/HALF_PI (/ (- e 120.0) 60.0))))))
            :g i
            :rgb (fn [e n r] (Vec4. n e r 255.0))}
     
     210.0 {:a (:g paletton-base-values)
            :b (:gb paletton-base-values)
            :f (fn [e]
                 (if (== e 180.0) -1.0
                     (* 0.75 (m/tan (* m/HALF_PI (/ (- 210.0 e) 30.0))))))
            :g s
            :rgb (fn [e n r] (Vec4. r e n 255.0))}
     255.0 {:a (:gb paletton-base-values)
            :b (:b paletton-base-values)
            :f (fn [e]
                 (if (== e 255.0) -1.0
                     (* 1.33 (m/tan (* m/HALF_PI (/ (- e 210.0) 45.0))))))
            :g i
            :rgb (fn [e n r] (Vec4. r n e 255.0))}
     
     315.0 {:a (:b paletton-base-values)
            :b (:br paletton-base-values)
            :f (fn [e]
                 (if (== e 255.0) -1.0
                     (* 1.33 (m/tan (* m/HALF_PI (/ (- 315.0 e) 60.0))))))
            :g s
            :rgb (fn [e n r] (Vec4. n r e 255.0))}
     360.0 {:a (:br paletton-base-values)
            :b (:r paletton-base-values)
            :f (fn [e]
                 (if (== e 0.0) -1.0
                     (* 1.33 (m/tan (* m/HALF_PI (/ (- e 315.0) 45.0))))))
            :g i
            :rgb (fn [e n r] (Vec4. r n e 255.0))}}))

(defn paletton-select-base-hsv 
  ""
  [hue]
  (second (first (filter #(< (mod hue 360.0) (% 0)) paletton-base-hsv-data))))

(defn paletton-base-color-by-hue
  ""
  [hue s v]
  (let [upd (fn [e t] (if (<= t 1.0)
                        (* e t)
                        (+ e (* (- 1.0 e) (dec t)))))
        {:keys [a b f g rgb]} (paletton-select-base-hsv hue)
        av ((a 1) 2)
        bv ((b 1) 2)
        as ((a 1) 1)
        bs ((b 1) 1)
        n (f hue)
        vv (upd (g av bv n) v)
        ss (upd (g as bs n) s)
        r (* 255.0 vv)
        b (* r (- 1.0 ss))
        g (if (== n -1.0)
            b
            (/ (+ r (* n b)) (inc n)))]
    [hue ss vv (rgb r g b)]))

(paletton-base-color-by-hue 172 0.66333 0.8267)
