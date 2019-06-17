(ns clojure2d.extra.utils
  "Set of various utilities which can be used to display various objects."
  (:require [clojure2d.core :refer :all]
            [clojure2d.color :as c]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [clojure2d.pixels :as p]
            [fastmath.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn show-palette
  "Display palette.

  Input: list of colors."
  [palette]
  (let [c (canvas 1000 300)
        h2 (/ ^int (height c) 2)
        bottom (- ^int (height c) 100)
        step (/ (- ^int (width c) 100.0) (count palette))
        cstep (inc (m/ceil step))]
    (with-canvas [c c]
      (-> c
          (set-background 30 30 30)
          (set-color 225 225 225)
          (rect 0 h2 (width c) h2))
      (doseq [col-no (range (count palette))]
        (set-color c (nth palette col-no))
        (rect c (+ 50 (* ^int col-no step)) 50 cstep bottom)))
    (show-window {:canvas c})
    c))

(defn show-gradient
  "Display gradient.

  Input: gradient function (see [[gradient]])."
  [gradient]
  (show-palette (map gradient (range 0.0 1.0 (/ 1.0 700.0)))))

(defn show-color
  "Display color.

  Input: color"
  [col]
  (let [c (show-palette [col])]
    (with-canvas-> c
      (set-color :white)
      (set-font-attributes 14)
      (text (str col " (" (c/format-hex col) ")") 10 20))))

(defn show-scalar-field
  "Show scalar field R^2->R"
  [f norm-in norm-out]
  (let [p (p/pixels 800 800)
        g (c/gradient [:black :white])
        c (canvas 800 800)]
    (dotimes [x 800]
      (dotimes [y 800]
        (let [col (g (norm-out (f (v/vec2 (norm-in x) (norm-in y)))))]
          (p/set-color p x y col))))
    (p/set-canvas-pixels! c p)
    (show-window {:canvas c})))

(defn show-vector-field
  "Show scalar field R^2->R^2"
  [f norm-out]
  (let [c (canvas 800 800)]
    (with-canvas [c c]
      (set-color c :white 8)
      (dotimes [_ 2500000]
        (let [x (r/drand (- m/PI) m/PI)
              y (r/drand (- m/PI) m/PI)
              v (f (v/vec2 x y))]
          (point c (norm-out (v 0)) (norm-out (v 1))))))
    (show-window {:canvas c})))

(defn show-image
  "Show image"
  [img]
  (let [c (canvas (width img) (height img))]
    (with-canvas-> c
      (image (get-image img)))
    (show-window {:canvas c})))
