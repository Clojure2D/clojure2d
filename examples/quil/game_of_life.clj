;; http://quil.info/sketches/show/example_game-of-life

(ns examples.quil.game-of-life
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^long grid-size 20)

(def random-state (vec (repeatedly (m/sq grid-size) #(r/irand 2))))

(defn get-neighbors [^long idx vec]
  [
   (get vec (dec (- idx grid-size)))
   (get vec (- idx grid-size))
   (get vec (inc (- idx grid-size)))

   (get vec (dec idx))
   (get vec (inc idx))

   (get vec (dec (+ grid-size idx)))
   (get vec (+ grid-size idx))
   (get vec (inc (+ grid-size idx)))
   ])

(defn new-status [idx itm vec]
  (let [alive-n ^long (get (frequencies (get-neighbors idx vec)) 1 0)]
    (if (= 1 itm)
      (if (or (> alive-n 3) (< alive-n 2)) 0 1)
      (if (= 3 alive-n) 1 0)
      )))

(defn draw
  ""
  [canvas _ _ state]
  (let [state (or state random-state)]
    (set-background canvas 240 240 240)
    (let [cell-size (quot ^int (width canvas) grid-size)]
      (doseq [[^long i ^int v] (map-indexed vector state)]
        (let [multiplier (int (/ i grid-size))
              x (* cell-size (- i (* multiplier grid-size)))
              y (* cell-size multiplier)]
          (set-color canvas (if (== 1 v) :black :white))
          (rect canvas x y cell-size cell-size))))
    (vec
     (map-indexed
      (fn [idx itm] (new-status idx itm state))
      state))))

(def window (show-window (make-canvas 500 500 :low) "Game of life" 10 draw))
