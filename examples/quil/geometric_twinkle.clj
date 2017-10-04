;; original: http://quil.info/sketches/show/example_geometric-twinkle
;; author: Jeremy Kross

(ns examples.quil.geometric-twinkle
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^int w 500)
(def ^:const ^int h 500)

(defn setup [] 
  (let [r 10
        y-vals (take-nth r (range (+ h r)))
        x-vals (flatten
                (map #(repeat (count y-vals) %)
                     (take-nth r (range (+ w r)))))
        origins (partition 2 (interleave x-vals (cycle y-vals)))
        circles (map (fn [[x y]] {:x x :y y :r r}) origins)]
    {:radius r
     :circles circles}))

(defn update-state [state ^long t]
  (update state :circles #(map (fn [c]
                                 (let [cx (/ w 2)
                                       cy (/ w 2)
                                       ^double x (:x c)
                                       ^double y (:y c)
                                       ^double r (:radius state)]
                                   (assoc c :r (* r
                                                  (m/sin
                                                   (+ (/ t 30.0)
                                                      (+ (m/sq (- cx x))
                                                         (m/sq (- cy y))))))))) %)))

(defn draw-state
  [canvas _ fps state]
  (set-background canvas :black)
  (set-color canvas :white)
  (doseq [c (:circles state)]
    (ellipse canvas (:x c) (:y c) (:r c) (:r c)))
  (update-state state fps))

(def window (show-window {:canvas (make-canvas w h)
                          :window-name "Geometric twinkle"
                          :fps 30
                          :draw-fn draw-state
                          :draw-state (setup)}))
