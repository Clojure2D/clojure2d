(ns clojure2d.extra.utils
  "Set of various utilities which can be used to display various objects."
  (:require [clojure2d.core :refer [canvas height width with-canvas set-background set-color set-stroke rect
                                    show-window with-canvas-> set-font-attributes text point image get-image path black-canvas]]
            [clojure2d.color :as c]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [clojure2d.pixels :as p]
            [fastmath.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn show-image
  "Show image"
  [img]
  (let [img (get-image img)
        c (canvas (width img) (height img))]
    (with-canvas-> c
      (image img))
    (show-window {:canvas c})))

(defn palette->image
  "Create image with rendered palette.

  Input: list of colors."
  ([palette] (palette->image palette false))
  ([palette luma?]
   (let [palette (c/palette palette)
         c (canvas 1000 300 :highest)
         h2 (/ (height c) 2)
         h (- (height c) 100)
         bottom (+ h 50)
         step (/ (- (width c) 100.0) (count palette))
         hstep (* 0.5 step)
         cstep (inc (m/ceil step))]
     (with-canvas [c c]
       (-> c
           (set-background 30 30 30)
           (set-color 225 225 225)
           (rect 0 h2 (width c) h2))
       (doseq [^long col-no (range (count palette))]
         (set-color c (nth palette col-no))
         (rect c (+ 50 (* col-no step)) 50 cstep h))
       (when luma?
         (let [p (map #(vector (+ 50 hstep (* ^long % step))
                               (m/norm (c/ch0 (c/to-LAB (nth palette %))) 0.0 100.0 bottom 50)) (range (count palette)))]
           (-> c
               (set-color (c/gray 220.0 200.0))
               (set-stroke 3.0)
               (path p)
               (set-color (c/gray 35.0))
               (set-stroke 1.0)
               (path p)))))
     (get-image c))))

(defn show-palette
  "Display palette.

  Input: list of colors."
  ([palette] (show-palette palette false))
  ([palette luma?] (show-image (palette->image palette luma?))))

(defn gradient->image
  "Create image with rendered gradient.

  Input: gradient function (see [[gradient]])."
  ([gradient] (gradient->image gradient false))
  ([gradient luma?]
   (let [gradient (if (fn? gradient) gradient (c/gradient gradient))]
     (palette->image (map gradient (m/slice-range 0.0 1.0 700)) luma?))))

(defn show-gradient
  "Display gradient.

  Input: gradient function (see [[gradient]])."
  ([gradient] (show-gradient gradient false))
  ([gradient luma?]
   (show-image (gradient->image gradient luma?))))

(defn color->image
  "Render color and display L from LAB color space.

  Input: color"
  [col]
  (let [img (palette->image [col])]
    (with-canvas-> (canvas (width img) (height img))
      (image img)
      (set-color :white)
      (set-font-attributes 14)
      (text (str col " (" (c/format-hex col) "); L=" (format "%.2f%%" (first (c/to-LAB col)))) 10 20)
      (get-image))))

(defn show-color
  "Display color and display L from LAB color space.

  Input: color"
  [col]
  (show-image (color->image col)))

(defn show-scalar-field
  "Show scalar field R^2->R"
  [f norm-in norm-out]
  (let [p (p/pixels 800 800)
        g (c/gradient [:black :white])
        c (canvas 800 800)]
    (dotimes [x 800]
      (dotimes [y 800]
        (let [col (g (norm-out (f (v/vec2 (norm-in x) (norm-in y)))))]
          (p/set-color! p x y col))))
    (p/set-canvas-pixels! c p)
    (show-window {:canvas c})))

(defn show-vector-field
  "Show scalar field R^2->R^2"
  [f norm-out]
  (let [c (black-canvas 800 800)]
    (with-canvas [c c]
      (set-color c :white 8)
      (dotimes [_ 2500000]
        (let [x (r/drand (- m/PI) m/PI)
              y (r/drand (- m/PI) m/PI)
              v (f (v/vec2 x y))]
          (point c (norm-out (v 0)) (norm-out (v 1))))))
    (show-window {:canvas c})))

