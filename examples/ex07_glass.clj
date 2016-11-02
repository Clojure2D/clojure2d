(ns examples.ex07-glass
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.extra.variations :as vr])
  (:import [clojure2d.math.vector Vec2]
           [java.awt Color]))


(defn create-field
  ""
  []
  (let [one-field? (m/brand 0.5)
        field-name1 (rand-nth vr/variation-list)
        field-name2 (rand-nth vr/variation-list)
        field (if one-field?
                (vr/make-variation field-name1 1.0 {})
                (comp (vr/make-variation field-name2 1.0 {}) (vr/make-variation field-name1 1.0 {})))]
    (if one-field?
      (println field-name1)
      (println (str field-name1 " " field-name2)))
    field))

(defn draw-glass
  ""
  [canvas disp width height]
  (let [hw (/ height 2)
        ww (/ width 2)
        field (create-field)]
      (loop [x 0]
        (loop [y 0]
          (let [xt (/ (- x ww) 120)
                yt (/ (- y hw) 120)
                ^Vec2 n (field (Vec2. xt yt))
                n1 (m/noise (.x n) (.y n))
                n2 (m/noise (.y n) (.x n) 0.3)
                v1 (float (m/constrain n1 0 1))
                v2 (float (m/constrain n2 0 1))]
            (set-color canvas (Color. (* v1 v1) (* v1 v2) v2))
            (rect canvas x y 1 1))

          (when (and @disp (< y height)) (recur (inc y))))
        (when (and @disp (< x width)) (recur (inc x))))))

(defn example-07
  ""
  []
  (let [canvas (create-canvas 800 800)
        [_ disp] (show-window canvas "glass" 800 800 25)]

    (defmethod key-pressed ["glass" \space] [_]
      (let [r (clojure2d.utils/to-hex (m/irand) 8)]
        (save-canvas canvas (str "results/ex07/" r ".jpg"))))


    (with-canvas canvas
      (set-background (Color. 200 200 210))
      (draw-glass disp 800 800)))
  :done)

(example-07)
