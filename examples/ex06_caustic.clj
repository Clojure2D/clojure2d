(ns examples.ex06-caustic
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.extra.variations :as vr])
  (:import [clojure2d.math.vector Vec2]
           [java.awt Color]))

(def ^:const min-range -2.0)
(def ^:const max-range 2.0)
(def ^:const tilt-scale 4)
(def ^:const delta-scale 0.5)
(def ^:const shift-scale 30.0)

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

(defn draw-caustic
  ""
  [canvas disp width height]
  (let [hw (/ width 2)
        hh (/ height 2)
        d shift-scale
        d2 (* d 2)
        field (create-field)]
    (loop [x (- d2)]
      (loop [y (- d2)]
        (let [hx (m/norm (- x hw) (- hw) hw min-range max-range)
              hy (m/norm (- y hh) (- hh) hh min-range max-range)
              hhx (* tilt-scale hx)
              hhy (* tilt-scale hy)
              delta (* delta-scale (m/norm (m/noise hx hy) 0 1 -1 1))
              ^Vec2 v1 (field (Vec2. (- hhx delta) (- hhy delta)))
              ^Vec2 v2 (field (Vec2. (+ hhx delta) (+ hhy delta)))
              dx (* d (- (.x v1) (.x v2)))
              dy (* d (- (.y v1) (.y v2)))]
          (rect canvas (+ dx x) (+ dy y) 1 1))
        (when (and @disp (< y (+ d2 height))) (recur (+ y 0.3))))
      (when (and @disp (< x (+ d2 width))) (recur (+ x 0.3)))))
  canvas)

(defn example-06
  ""
  []
  (let [canvas (create-canvas 800 800)
        [_ disp] (show-window canvas "caustic" 800 800 25)]

    (defmethod key-pressed ["caustic" \space] [_]
      (let [r (clojure2d.utils/to-hex (m/irand) 8)]
        (save-canvas canvas (str "results/ex06/" r ".jpg"))))


    (with-canvas canvas
      (set-color (Color. 10 20 40 30))
      (set-background (Color. 200 200 210))
      (draw-caustic disp 800 800)))
  :done)

(example-06)
