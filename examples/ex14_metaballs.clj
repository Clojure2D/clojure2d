;; https://gist.github.com/yogthos/3411106

(ns examples.ex14-metaballs
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.color :as c]
            [clojure2d.utils :as u])
  (:import [java.awt Color]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:const SIZE 400)

(deftype Ball [x y vx vy radius color])

(defn make-ball
  ""
  []
  (Ball. (m/irand SIZE)
         (m/irand SIZE)
         (double (m/irand 1 7))
         (double (m/irand 1 7))
         (m/irand 40 55)
         (take 3 (repeatedly #(m/irand 256)))))

(defn direction
  ""
  [p v]
  (if (or (> p SIZE) (neg? p))
    (- v)
    v))

(defn move
  ""
  [^Ball ball]
  (let [vx (direction (.x ball) (.vx ball))
        vy (direction (.y ball) (.vy ball))]
    (Ball. (+ (.x ball) vx) (+ (.y ball) vy) vx vy (.radius ball) (.color ball))))

(defn color
  ""
  [r g b]
  (Color. ^int (c/clamp255 r)
          ^int( c/clamp255 g)
          ^int (c/clamp255 b)))

(defn influence 
  ""
  [^Ball ball px py]
  (let [dx (- (.x ball) px)
        dy (- (.y ball) py)]
    (/ (.radius ball) (+ m/EPSILON (m/hypot dx dy)))))

(defn compute-color
  ""
  [x y [red-cur green-cur blue-cur] ^Ball ball]
  (let [infl (influence ball x y)
        [r g b] (.color ball)]
    [(+ red-cur (* infl r))
     (+ green-cur (* infl g))
     (+ blue-cur (* infl b))]))

(defn draw
  ""
  [canvas balls]
  (loop [y 0]
   (loop [x 0]
    
    (let [[r g b] (reduce (partial compute-color x y) [0 0 0] balls)]
      (set-color canvas ^Color (color r g b))
      (rect canvas x y 2 2))
    
    (when (< x (- SIZE 2)) (recur (+ 2 x))))
   (when (< y (- SIZE 2)) (recur (+ 2 y)))))

(defn draw-balls
  ""
  [n canvas framecount result]
  (let [balls (if result
                (map move result)
                (take n (repeatedly make-ball)))]
    (with-canvas canvas
      (draw balls))
    balls))

(defn example-14
  ""
  [n]
  (let [canvas (create-canvas SIZE SIZE)]
    (show-window canvas "metaballs" SIZE SIZE 25 (partial draw-balls n)))
  :done)

(example-14 (m/irand 2 4))
