(ns examples.NOC.introduction.randomdistribution-I-2
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn bar
  "Draw bar"
  [canvas x y w h c1 c2]
  (-> canvas
      (set-color c1)
      (rect x y w h)
      (set-color c2)
      (rect x y w h true)))

(defn draw
  ""
  [canvas window framecount state]
  (let [random-counts (or state (repeatedly 20 #(int 0)))
        l (int (count random-counts))
        index (r/irand l)
        w (int (/ ^int (width canvas) l))]

    (-> canvas
        (set-background :white)
        (set-stroke 2.0))
    
    (dorun (map-indexed #(-> canvas
                             (bar (* ^int %1 w) (- ^int (height canvas) ^int %2) (dec w) %2 :gray :black)) random-counts))
    
    (map-indexed #(if (== ^int %1 index) (inc ^int %2) %2) random-counts)))

(def window (show-window (make-canvas 640 360) "Random Distribution I_2" draw))
