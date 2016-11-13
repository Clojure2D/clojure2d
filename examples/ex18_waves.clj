;; visualize wave generators

(ns examples.ex16-analog
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.extra.signal :as s]))

(def canvas (create-canvas 600 600))

(def display (show-window canvas "waves" 600 600 25))

(defmethod key-pressed ["waves" \space] [_]
  (let [r (clojure2d.utils/to-hex (m/irand) 8)]
    (save-canvas canvas (str "results/ex18/" r ".jpg"))))

;; frequencies and amplitudes
(def f (into [] (map #(bit-shift-left 1 %) (range 16))))
(def a (into [] (map #(/ 1.0 %) f)))

(defn draw-fun
  ""
  [canvas f]
  (dotimes [x 600]
    (rect canvas x (+ 300 (* 300 (f (m/norm x 0 600 0 1)))) 1 1)))

;; run several times
(let [lst (into [] (map #(s/make-wave (rand-nth s/waves) (f %) (a %) (m/drand 1)) (range 1 5)))]
    (with-canvas canvas
      (set-color (java.awt.Color/white))
      (set-background java.awt.Color/black)
      (draw-fun (s/make-sum-wave lst)))
    :done)

(defn draw-fun2
  ""
  [canvas y f]
  (dotimes [x 600]
    (let [v (-> (f (m/norm x 0 600 0.0 1.0))
                (m/cnorm -1.0 1.0 0.0 0.999)
                (float))
          c (java.awt.Color. v (float (m/sq v)) (float (m/sqrt v)))]
      (set-color canvas c)
      (rect canvas x y 1 1))))

;; try several times
(let [num 7
      wvs (repeatedly num #(rand-nth s/waves))
      phases (repeatedly num #(m/drand 1.0))
      phasemult (repeatedly num #(m/drand -3.0 3.0))
      octaves (repeatedly num #(m/irand num))]
  (dotimes [y 600]
    (let [yy (m/norm y 0 600 0.0 1.0)
          lst (map #(s/make-wave (nth wvs %) (f (nth octaves %)) (a (nth octaves %)) (+ (* yy (nth phasemult %)) (nth phases %))) (range num))]
      (with-canvas canvas
        (draw-fun2 y (s/make-sum-wave lst))
                                        ;                 (set-background (java.awt.Color/black))
                                        ;                 (draw-fun (make-sum-wave lst))
        )
      :done)))


;; save signal to file
;; open in Audacity as RAW 16 bit signed, mono, big-endian, 44100Hz
(let [num 10
      amp (* 1.5 (/ 1.0 num))
      lst (into [] (map #(s/make-wave (rand-nth s/waves) (* 150 %) amp (m/drand 1)) (range 1 (inc num))))
      f (s/make-sum-wave lst)]
  (s/save-signal (s/make-signal-from-wave f 44100 10) "results/ex18/wave.raw"))
