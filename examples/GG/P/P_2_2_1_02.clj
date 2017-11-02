(ns examples.GG.P.P-2-2-1-02
  (:require [clojure2d.core :refer :all]
            [clojure2d.color :as c]))

(def ^:const ^double step-size 1.0)
(def ^:const ^double diameter 1.0)

(def hsb-mode (c/make-color-converter c/from-HSB 360 100 100 100))

(defn draw
  "Draw agent"
  [canvas window _ state]
  (let [mx (max 1 (mouse-x window))
        {:keys [step-size diameter draw-mode]} (get-state window)]
    (loop [[px py counter] state
           i 0]
      (let [direction (if (== draw-mode 2)
                        (rand-nth [:n :ne :e :se])
                        (rand-nth [:n :ne :e :se :s :sw :w]))
            [npx npy] (case direction
                        :n  [px (- py step-size)]
                        :ne [(+ px step-size) (- py step-size)]
                        :e  [(+ px step-size) py]
                        :se [(+ px step-size) (+ py step-size)]
                        :s  [px (+ py step-size)]
                        :sw [(- px step-size) (+ py step-size)]
                        :w  [(- px step-size) py])
            npx (if (> npx (width canvas)) 0 (if (neg? npx) (width canvas) npx))
            npy (if (> npy (height canvas)) 0 (if (neg? npy) (height canvas) npy))
            ncounter (if (>= counter 100) 0 (inc counter))]

        (when (and (== draw-mode 3) (>= counter 100))
          (-> canvas
              (set-color (hsb-mode (c/make-color 192 100 64 80)))
              (ellipse (+ px (* 0.5 step-size))
                       (+ py (* 0.5 step-size))
                       (+ 7 diameter) (+ 7 diameter))))
        
        (-> canvas
            (set-color :black 100)
            (ellipse (+ px (* 0.5 step-size))
                     (+ py (* 0.5 step-size))
                     diameter diameter))
        (if (< i mx)
          (recur [npx npy ncounter] (inc i))
          [npx npy ncounter])))))

(let [canvas (make-canvas 550 550)]
  (with-canvas canvas
    (set-background :white))
  (def window (show-window {:canvas canvas
                            :draw-fn draw
                            :window-name "P_2_2_1_02"
                            :state {:step-size 1.0
                                    :diameter 1.0
                                    :draw-mode 1}
                            :draw-state [(/ (width canvas) 2)
                                         (/ (height canvas) 2)
                                         0]})))

(defmethod key-pressed [(:window-name window) \1] [_ _] {:step-size 1.0
                                                         :diameter 1.0
                                                         :draw-mode 1})

(defmethod key-pressed [(:window-name window) \2] [_ _] {:step-size 1.0
                                                         :diameter 1.0
                                                         :draw-mode 2})

(defmethod key-pressed [(:window-name window) \3] [_ _] {:step-size 10.0
                                                         :diameter 5.0
                                                         :draw-mode 3})

(defmethod key-pressed [(:window-name window) \backspace] [_ s]
  (with-canvas @(:buffer window)
    (set-background :white))
  s)

