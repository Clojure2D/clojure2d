;; http://www.algorithmic-worlds.net/blog/blog.php?Post=20110227

(ns examples.ex34-ducks-fractals
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.complex :as c]
            [clojure2d.math.vector :as v]
            [clojure2d.pixels :as p]
            [clojure2d.math.random :as r]
            [clojure.pprint :refer :all])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 800)
(def ^:const ^int h 800)

(comment defn iter
         "Fractal formula"
         [^Vec2 z c]
         (c/log (c/add (Vec2. (.x z) (m/abs (.y z))) c))
         ;; (v/add (v/div (v/abs z) (v/magsq z)) c)
         )

(def canvas (make-canvas w h))
(def window (show-window canvas "Ducks"))

(def coloring-fns {:f1 (fn [nz z] (m/exp (* -6.0 ^double (v/mag nz))))
                   :f2 (fn [nz z] (m/exp (* -6.0 ^double (v/magsq nz))))
                   :f3 (fn [nz z] (m/exp (/ -6.0 ^double (v/mag (v/sub nz z)))))
                   :f4 (fn [nz z] (m/exp (/ -6.0 ^double (v/magsq (v/sub nz z)))))})

(def iter-fns {:ducks [(Vec2. -0.5 0.0)
                       (fn [^Vec2 z c] (c/log (c/add (Vec2. (.x z) (m/abs (.y z))) c)))]
               :ducksv1 [(Vec2. m/HALF_PI 0.0)
                         (fn [^Vec2 z c] (c/log (c/sin (c/add (Vec2. (.x z) (m/abs (.y z))) c))))]
               :kali [(Vec2. 0.0 0.0)
                      (fn [z c] (v/add (v/div (v/abs z) (v/magsq z)) c))]})

(def rand-c {:ducks #(Vec2. (r/drand 0.2 0.95) (r/drand -0.25 -1.5))
             :ducksv1 #(Vec2. (r/drand 0.2 0.95) (r/drand -0.25 -1.5))
             :kali #(Vec2. (r/drand -0.1 -1.9) (r/drand -0.1 -1.9))})

(defn draw-ducks
  "Iterate and draw"
  [canvas {:keys [c-const coloring-fn iter-fn ^double mlt]}]
  (let [f (coloring-fns coloring-fn)
        [shift iter] (iter-fns iter-fn)]
    (dotimes [x w]
      (dotimes [y h]
        (let [xx (m/norm x 0.0 w -2 2)
              yy (m/norm y 0.0 h -2 2)
              m (loop [i (int 0)
                       z (v/add (Vec2. xx yy) shift)
                       sum (double 0)
                       wsum (double 0)]
                  (let [w (m/exp (- (/ i 7.0)))
                        nz (iter z c-const)]
                    (if (< i 30)
                      (recur (inc i)
                             nz 
                             (+ sum (* w ^double (f nz z)))
                             (+ wsum w))
                      (/ sum wsum))))
              col (* 255 (m/abs (m/qsin (* mlt m/TWO_PI (m/sqrt m)))))]
          (set-color canvas col col col)
          (rect canvas y x 1 1)))))
  canvas)

(let [cfname (rand-nth (keys coloring-fns))
      itername (rand-nth (keys iter-fns))
      itername :ducksv1
      conf {:c-const ((rand-c itername))
            :coloring-fn cfname
            :mlt (r/drand 1 3)
            :iter-fn itername}]
  (pprint conf)
  (with-canvas canvas
    (draw-ducks conf)
    (p/set-canvas-pixels! (p/filter-channels p/normalize-filter nil (p/get-canvas-pixels canvas)))))
