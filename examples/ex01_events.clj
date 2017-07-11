;; ## Example 01
;;
;; Show how to create mouse and key events + replace-canvas function

(ns examples.ex01-events
  "Process window events"
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; In this example you can see how to create event methods for different windows
;;
;; Let's create two windows and define following actions:
;;
;; * For pressed `space` on both windows display window name
;; * For released mouse on first window do the following:
;;     * create new canvas
;;     * get mouse position and change to the color
;;     * set canvas color to newly calculated
;;     * replace canvas
;;
;; Note: this is just ilustrations how to replace canvas, simpler is just change color on canvas attached to window (without replacing it).

(defn example-01
  "Create 2 windows and attach event methods"
  []
  (let [name1 "first window"
        name2 "second window"
        frame1 (show-window (make-canvas 1 1) name1 400 400 25)
        frame2 (show-window (make-canvas 100 100) name2 400 200 10)]
    
    (defmethod key-pressed [name1 \space] [_]
      (println (str "Window: " name1)))

    (defmethod key-pressed [name2 \space] [_]
      (println (str "Window: " name2)))

    (defmethod mouse-event [name1 :mouse-pressed] [e]
      (let [canvas (make-canvas 1 1)
            x (mouse-x e)
            y (mouse-y e)
            cr (m/cnorm x 0 399 0 255)
            cg (m/cnorm y 0 399 0 255)]
        (with-canvas canvas
          (set-background cr cg 128))
        (replace-canvas frame1 canvas)))
    nil))

(example-01)
