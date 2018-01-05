(ns examples.NOC.ch06.simplescalarprojection
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v])
  (:import clojure2d.math.vector.Vec2))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^Vec2 a (Vec2. 20 300))
(def ^Vec2 b (Vec2. 500 250))

(defn scalar-projection
  ""
  [p a b]
  (let [ap (v/sub p a)
        ab (v/normalize (v/sub b a))]
    (-> ab
        (v/mult (v/dot ap ab))
        (v/add a))))

(defn draw
  ""
  [canvas window _ _]
  (let [^Vec2 mouse (mouse-pos window)
        ^Vec2 norm (scalar-projection mouse a b)]

    (-> canvas
        (set-background :white)
        (set-color :black)
        (set-stroke 2.0)
        (line (.x a) (.y a) (.x b) (.y b))
        (line (.x a) (.y a) (.x mouse) (.y mouse))
        (ellipse (.x a) (.y a) 8 8)
        (ellipse (.x b) (.y b) 8 8)
        (ellipse (.x mouse) (.y mouse) 8 8)
        (set-color 50 50 50)
        (set-stroke 1.0)
        (line (.x mouse) (.y mouse) (.x norm) (.y norm))
        (set-color :red)
        (ellipse (.x norm) (.y norm) 16 16))))

(def window (show-window (make-canvas 600 360) "Simple scalar projection" draw))
