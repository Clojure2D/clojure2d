(ns examples.GG.P.P-2-2-1-01
  (:require [clojure2d.core :refer :all]))

(def ^:const ^double step-size 1.0)
(def ^:const ^double diameter 1.0)

(defn draw
  "Draw agent"
  [canvas window _ state]
  (let [mx (max 1 (mouse-x window))]
    (loop [[px py] state
           i 0]
      (let [direction (rand-nth [:n :ne :e :se :s :sw :w :nw])
            [npx npy] (case direction
                        :n  [px (- py step-size)]
                        :ne [(+ px step-size) (- py step-size)]
                        :e  [(+ px step-size) py]
                        :se [(+ px step-size) (+ py step-size)]
                        :s  [px (+ py step-size)]
                        :sw [(- px step-size) (+ py step-size)]
                        :w  [(- px step-size) py]
                        :nw [(- px step-size) (- py step-size)])
            npx (if (> npx (width canvas)) 0 (if (neg? npx) (width canvas) npx))
            npy (if (> npy (height canvas)) 0 (if (neg? npy) (height canvas) npy))]
        (-> canvas
            (set-color :black 40)
            (ellipse (+ px (* 0.5 step-size))
                     (+ py (* 0.5 step-size))
                     diameter, diameter))
        (if (< i mx)
          (recur [npx npy] (inc i))
          [npx npy])))))

(let [canvas (make-canvas 800 800)]
  (with-canvas-> canvas
    (set-background :white))
  (def window (show-window {:canvas canvas
                            :draw-fn draw
                            :window-name "P_2_2_1_01"
                            :draw-state [(/ (width canvas) 2)
                                         (/ (height canvas) 2)]})))

(with-canvas-> (make-canvas 800 800)
  (draw window 0 [400 400]))
