;; https://gist.github.com/yogthos/3411106

(ns examples.ex14-metaballs
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.color :as c]
            [clojure2d.math.vector :as v])
  (:import [java.awt Color]
           [clojure2d.math.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:const ^long SIZE 600)

(deftype Ball [^double x ^double y ^double vx ^double vy ^double radius ^Vec3 color])

(defn make-ball
  ""
  ^Ball []
  (Ball. (r/irand SIZE)
         (r/irand SIZE)
         (r/irand 1 7)
         (r/irand 1 7)
         (r/irand 40 55)
         (Vec3. (r/drand 255) (r/drand 255) (r/drand 255))))

(defn direction
  ""
  ^double [^double p ^double v]
  (if (or (> p SIZE) (neg? p))
    (- v)
    v))

(defn move
  ""
  [^Ball ball]
  (let [vx (direction (.x ball) (.vx ball))
        vy (direction (.y ball) (.vy ball))]
    (Ball. (+ (.x ball) vx) (+ (.y ball) vy) vx vy (.radius ball) (.color ball))))

(defn influence 
  ""
  ^double [^Ball ball ^double px ^double py]
  (let [dx (- (.x ball) px)
        dy (- (.y ball) py)]
    (/ (.radius ball) (+ m/EPSILON (m/hypot dx dy)))))

(defn compute-color
  ""
  ^Vec3 [x y ^Vec3 cur ^Ball ball]
  (let [infl (influence ball x y)
        ^Vec3 rgb (.color ball)]
    (Vec3. (+ (.x cur) (* infl (.x rgb)))
           (+ (.y cur) (* infl (.y rgb)))
           (+ (.z cur) (* infl (.z rgb))))))

(defn draw
  ""
  [canvas balls]
  (loop [y (int 0)]
    (loop [x (int 0)]
    
      (let [^Vec3 c (reduce (partial compute-color x y) (Vec3. 0.0 0.0 0.0) balls)]
        (set-color canvas c)
        (rect canvas x y 2 2))
    
      (when (< x (- SIZE 2)) (recur (+ 2 x))))
    (when (< y (- SIZE 2)) (recur (+ 2 y)))))

(defn draw-balls
  ""
  [n canvas framecount result]
  (let [balls (if result
                (map move result)
                (take n (repeatedly make-ball)))]
    (draw canvas balls)
    balls))

(defn example-14
  ""
  [n]
  (let [canvas (create-canvas SIZE SIZE)]
    (show-window canvas "metaballs" SIZE SIZE 25 (partial draw-balls n)))
  :done)

(example-14 (r/irand 2 6))
