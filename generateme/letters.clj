(ns generateme.letters
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [clojure2d.color :as c]
            [fastmath.stats :as stat])
  (:import [fastmath.vector Vec2]))

(def ^:const ^int w 800)
(def ^:const ^int hw (/ w 2))

(def ^Vec2 midpoint (v/vec2 hw hw))

(def ^:const ^double step (/ 1.0 20.0))

(def cb (c/to-color :red))

(defn draw-letter
  "Draw letter in the middle of canvas"
  [canvas letter]
  (-> canvas
      (set-background :white)
      (set-color cb)
      (set-font-attributes (* 0.7 w) :bold)
      (text (str letter) hw (- (+ (font-height canvas) hw) (font-ascent canvas)) :center)))

(defn cast-ray
  "Cast ray from given angle towards midpoint"
  [canvas angle]
  (let [start (v/add midpoint (v/from-polar (v/vec2 hw angle)))
        hit (loop [x 0.0]
              (let [pos (v/interpolate start midpoint x)
                    c (get-pixel canvas (long (pos 0)) (long (pos 1)))]
                (if (= c cb) pos (recur (+ x step)))))]
    (v/dist start hit)))

(defn cast-rays
  ""
  [canvas astep]
  (let [points (map #(cast-ray canvas %) (range 0 m/TWO_PI astep))
        [mn mx] (stat/extent points)
        cnt (count points)
        p (map #(v/vec2 (m/norm %1 0 cnt 100 700)
                        (+ hw (* %2 100.0))) (range cnt) (map #(m/norm % mn mx) points))]
    (set-color canvas :black 200)
    (path-bezier canvas p)))

(def c (canvas w w))

(with-canvas-> c
  (draw-letter \V)
  (cast-rays 0.2))

(show-window {:canvas c})

