(ns mandel
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v]
            [clojure2d.math.complex :as c])
  (:import [clojure2d.math.vector Vec2 Vec4]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const ^int w 500)

(def unary-op {:sqrt1z c/sqrt1z
               :sq c/sq
               :abs v/abs
               :absre (fn [^Vec2 v] (Vec2. (m/abs (.x v)) (.y v)))
               :absim (fn [^Vec2 v] (Vec2. (.x v) (m/abs (.y v))))
               :sin c/sin
               :cos c/cos
               :sinh c/sinh
               :cosh c/cosh
               :tanh c/tanh
               :log c/log
               :exp c/exp
               :neg c/neg
               :conjugate c/conjugate
               :sec c/sec
               :csc c/csc
               :reciprocal c/reciprocal})

(def binary-op {:mult c/mult
                :div c/div
                :pow c/pow
                :add c/add
                :sub c/sub})

(def z-fn (fn [z c] z))
(def c-fn (fn [z c] c))

(def initial-node [[z-fn :z] [c-fn :c] [#(c/add (c/sq %1) %2) {:add [{:sq :z} :c]}]])

(defn get-random-op
  ""
  [m]
  (let [op (rand-nth (keys m))]
    [(m op) op]))

(defn make-step
  ""
  [nodes]
  (if (r/brand 0.5)
    (let [[f op] (get-random-op unary-op)
          [f1 op1] (if (r/brand 0.75) (last nodes) (rand-nth nodes))]
      (conj nodes [#(f (f1 %1 %2)) {op op1}]))
    (let [[f op] (get-random-op binary-op)
          [f1 op1] (if (r/brand 0.5) (last nodes) (rand-nth nodes))
          [f2 op2] (if (r/brand 0.5) (rand-nth nodes) (last nodes))]
      (conj nodes [#(f (f1 %1 %2) (f2 %1 %2)) {op [op1 op2]}]))))

(defn calc
  [z c]
  (c/add (c/mult (v/abs z) z) c))

(def canvas (make-canvas w w))
(def window (show-window canvas "Mandelbrot"))

(defn draw-mandelbrot
  "Draw Mandelbrot type"
  [canvas f]
  (dotimes [x w]
    (dotimes [y w]
      (when (window-active? window)
        (let [xx (m/norm x 0 w -3 3)
              yy (m/norm y 0 w -3 3)
              sz (Vec2. 0.0 0.0)
              c (Vec2. xx yy)
              ^int idx (loop [iter (int 0)
                              z sz]
                         (if (and (< iter 128)
                                  (< ^double (v/magsq z) 4.0))
                           (recur (inc iter) (f z c)) 
                           (dec iter)))
              col (* 255.0 (m/pow (/ idx 128.0) 0.4))]
          (set-color canvas col col col)
          (rect canvas x y 1 1))))))

(defn draw-julia
  "Draw Julia type"
  [canvas f c]
  (dotimes [x w]
    (dotimes [y w]
      (let [xx (m/norm x 0 w -3 3)
            yy (m/norm y 0 w -3 3)
            sz (Vec2. xx yy)
            ^int idx (loop [iter (int 0)
                            z sz]
                       (if (and (< iter 64)
                                (< (v/magsq z) 4.0))
                         (recur (inc iter) (f z c)) 
                         (dec iter)))
            col (* 255.0 (m/pow (/ idx 64.0) 0.4))]
        (set-color canvas col col col)
        (rect canvas x y 1 1)))))


(let [[f d] (last (last (take (r/irand 2 6) (iterate make-step initial-node))))]
  (println d)
  (with-canvas-> canvas
    (set-background :black)
    (draw-julia f (Vec2. (r/drand -2 2) (r/drand -2 2)))))
