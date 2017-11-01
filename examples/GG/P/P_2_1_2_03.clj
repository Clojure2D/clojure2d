(ns examples.GG.P.P-2-1-2-03
  (:require [clojure2d.core :refer :all]
            [clojure2d.math.vector :as v]
            [clojure2d.math :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^int w 600)
(def ^:const ^int h 600)
(def ^:const ^int midw (/ w 2))
(def ^:const ^int midh (/ h 2))
(def ^:const ^double rdiag (/ 1.0 (m/hypot w h)))

(defn draw 
  "Draw rects"
  [canvas window _ _]
  (let [mv (mouse-pos window)]
    (-> canvas
        (set-background :white)
        (set-stroke 3.0)
        (set-color 0 0 0 180)
        (translate midw midh))
    (doseq [grid-x (range 0 (width canvas) 25)
            grid-y (range 0 (height canvas) 25)]
      (let [^double dist (v/dist mv (v/vec2 grid-x grid-y))
            scl (inc (* dist rdiag))
            diameter (* scl 40.0 (/ dist 500.0))
            ngx (* scl (- ^long grid-x midw))
            ngy (* scl (- ^long grid-y midh))]
        (-> canvas
            (push-matrix)
            (translate ngx ngy) ;; simulating 3d with ngx ngy
            (rect 0 0 diameter diameter true)
            (pop-matrix))))))

(def window (show-window (make-canvas w h)  "P_2_1_2_03" draw))
