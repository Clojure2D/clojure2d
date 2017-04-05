(ns examples.ex06-caustic
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v]
            [clojure2d.extra.variations :as vr])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:const ^double min-range -2.0)
(def ^:const ^double max-range 2.0)
(def ^:const ^long tilt-scale 4)
(def ^:const ^double delta-scale 0.5)
(def ^:const ^long shift-scale 30)

(defn create-field
  ""
  []
  (let [one-field? (r/brand 0.5)
        field-name1 (rand-nth vr/variation-list-not-random)
        field-name2 (rand-nth vr/variation-list-not-random)
        field (if one-field?
                (vr/make-variation field-name1 1.0 {})
                (comp (vr/make-variation field-name2 1.0 {}) (vr/make-variation field-name1 1.0 {})))]
    (if one-field?
      (println field-name1)
      (println (str field-name1 " " field-name2)))
    field))

(defn draw-caustic
  ""
  [canvas disp ^long width ^long height]
  (let [hw (long (/ width 2))
        hh (long (/ height 2))
        d shift-scale
        d2 (* d 2)
        d2- (- d2)
        field (create-field)]
    (loop [x (double d2-)]
      (loop [y (double d2-)]
        (let [^double hx (m/norm (- x hw) (- hw) hw min-range max-range)
              ^double hy (m/norm (- y hh) (- hh) hh min-range max-range)
              hhx (* tilt-scale hx)
              hhy (* tilt-scale hy)
              delta (* delta-scale ^double (m/norm (r/noise hx hy) 0 1 -1 1))
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
      (save-canvas canvas (next-filename "results/ex06/" ".jpg")))

    (with-canvas canvas
      (set-color 10 20 40 30)
      (set-background 200 200 210)
      (draw-caustic disp 800 800)))
  :done)

(example-06)
