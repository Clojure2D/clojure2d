(ns examples.NOC.ch08.kochsimple-8-5
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v])
  (:import [clojure2d.math.vector Vec2]))

(def canvas (make-canvas 383 200 :mid))
(def window (show-window canvas "Koch Simple 8_5"))

(def initial-state [[(v/vec2 0 150)
                     (v/vec2 (width canvas) 150)]])

(defn render
  "Render segments"
  [canvas segments]
  (run! #(let [[^Vec2 a ^Vec2 b] %]
           (line canvas (.x a) (.y a) (.x b) (.y b))) segments))


(defn iterate-segment
  "Return new segments for given pair"
  [[a e]]
  (let [diff (v/div (v/sub e a) 3.0)
        rot (v/rotate diff (m/radians -60))
        b (v/add a diff)
        c (v/add a (v/add rot diff))
        d (v/sub e diff)]
    [[a b] [b c] [c d] [d e]]))

(defn next-level
  "Calculate next depth"
  [segments]
  (reduce #(concat %1 (iterate-segment %2)) [] segments))

(with-canvas-> canvas
  (set-background :white)
  (set-color :black)
  (render (first (drop 5 (iterate next-level initial-state)))))

