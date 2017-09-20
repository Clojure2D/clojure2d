(ns examples.NOC.ch07.gameoflifesimple-7-2
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^int scl 8)
(def ^:const ^int w 640)
(def ^:const ^int h 360)

(def ^:const ^int columns (/ w scl))
(def ^:const ^int rows (/ h scl))

(defn inside?
  "Is inside the board?"
  [^long x ^long y]
  (and (pos? x) (pos? y)
       (< x (dec columns))
       (< y (dec rows))))

(defn neighbours
  "Count neighbours"
  [board ^long x ^long y]
  (reduce clojure.core/+ (for [^long offx [-1 0 1]
                               ^long offy [-1 0 1]
                               :when (not (and (zero? offx) (zero? offy)))
                               :let [xx (+ offx x)
                                     yy (+ offy y)]]
                           (get-in board [yy xx]))))

(defn next-board
  "Calculate next board"
  [board]
  (vec (map-indexed #(vec (map-indexed (fn [^long x val]
                                         (let [y (long %1)
                                               ^long ns (if-not (inside? x y) 0 (neighbours board x y))
                                               ^long curr (get-in board [y x])]
                                           (cond
                                             (and (pos? curr) (< ns 2)) 0
                                             (and (pos? curr) (> ns 3)) 0
                                             (and (zero? curr) (== ns 3)) 1
                                             :else curr))) %2)) board)))

(defn init-board
  "Create random board"
  []
  (let [rnd #(r/irand 2)]
    (vec (repeatedly rows #(vec (repeatedly columns rnd))))))

(defn draw-board
  "Draw board, local state is current board, window state tracks click event (reinit board)"
  [canvas window _ state]
  (let [reset? (get-state window)
        board (if (or (not state) reset?) (init-board) state)] ;; init board on first frame or when mouse is clicked 

    (when reset? (set-state! window false)) ;; reset window state after click
    
    (set-background canvas :white)
    (set-color canvas :black)
    (dotimes [x columns]
      (dotimes [y rows]
        (rect canvas (* x scl) (* y scl) scl scl true)
        (when (pos? ^long (get-in board [y x]))
          (rect canvas (* x scl) (* y scl) scl scl))))

    (next-board board)))

(def window (show-window (make-canvas w h) "Game of Life Simple 7_2" 24 draw-board))

(defmethod mouse-event ["Game of Life Simple 7_2" :mouse-clicked] [_ _] true) ;; reset board on click
