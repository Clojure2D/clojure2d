;; visualize wave generators

(ns examples.ex18-waves
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.extra.signal :as s]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def canvas (create-canvas 600 600))

(def display (show-window canvas "waves" 600 600 25))

(defmethod key-pressed ["waves" \space] [_]
  (save-canvas canvas (next-filename "results/ex18/" ".jpg")))

;; frequencies and amplitudes
(def f (mapv #(bit-shift-left 1 ^long %) (range 16)))
(def a (mapv #(/ 1.0 ^long %) f))

(defn draw-fun
  ""
  [canvas f]
  (dotimes [x 600]
    (rect canvas x (+ 300.0 (* 300.0 ^double (f (/ x 600.0)))) 1 1)))

;; run several times
(let [lst (into [] (map #(s/make-wave (rand-nth s/oscillators) (f %) (a %) (r/drand 1)) (range 1 5)))]
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
      wvs (repeatedly num #(rand-nth s/oscillators))
      phases (repeatedly num #(r/drand 1.0))
      phasemult (repeatedly num #(r/drand -3.0 3.0))
      octaves (repeatedly num #(r/irand num))]
  (dotimes [y 600]
    (let [yy (/ y 600.0)
          lst (map #(s/make-wave (nth wvs %) (f (nth octaves %)) (a (nth octaves %)) (+ (* yy ^double (nth phasemult %)) ^double (nth phases %))) (range num))]
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
      lst (into [] (map #(s/make-wave (rand-nth s/oscillators) (* 150 ^long %) amp (r/drand 1)) (range 1 (inc num))))
      f (s/make-sum-wave lst)]
  (s/save-signal (s/make-signal-from-wave f 44100 10) "results/ex18/wave.raw"))
