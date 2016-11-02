(ns examples.ex05-particles
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.math.joise :as n]
            [clojure2d.utils :as u]
            [clojure2d.extra.variations :refer :all])
  (:import  [java.awt Color]
            [clojure2d.math.vector Vec2]))

(def ^:const width 1200)
(def ^:const height 1200)

(def ^:const point-step 15.0)
(def ^:const rscale 15.0)
(def ^:const angle-mult 16.0)
(def ^:const point-size 0.9)
(def ^:const alpha 20)

(defn make-particle
  ""
  []
  (Vec2. (m/drand width) (m/drand height)))

(defn move-particle
  ""
  [canvas ^Vec2 vrand fun noise ^Vec2 in]
  (let [xx (m/norm (.x in) 0 width -2 2)
        yy (m/norm (.y in) 0 height -2 2)
        ^Vec2 vr (v/add vrand (Vec2. xx yy))
        ^Vec2 v (v/div (fun vr) rscale)
        n (noise (.x v) (.y v))
        ang (* n m/TWO_PI angle-mult)
        nx (+ (.x in) (* point-step (m/qcos ang)))
        ny (+ (.y in) (* point-step (m/qsin ang)))
        col (int (m/cnorm (m/sqrt n) 0 1 100 250))]
    (if (and (<= 80 ny (- height 81)) (<= 80 nx (- width 81)))
      (do
        (with-canvas canvas
          (set-color (Color. col col col alpha))
          (set-stroke point-size)
                                        ;      (line (.x in) (.y in) nx ny)
          (point nx ny))
        (Vec2. nx ny))
      (make-particle))))

(defn example-05
  []
  (let [canvas (create-canvas width height)
        [frame running] (show-window canvas "particles" width height 25)
        noise (n/make-random-fractal)
        variation1 (rand-nth variation-list)
        variation2 (rand-nth variation-list)
        vrand (Vec2. (m/drand -1 1) (m/drand -1 1))
        compv (make-variation :erf 1.0 {})
        mv-fun (partial move-particle canvas vrand (comp (make-variation variation2 1.0 {}) (make-variation variation1 1.0 {})) noise)
        particles (repeatedly 25000 make-particle)]
    
    (defmethod key-pressed ["particles" \space] [_]
      (let [r (u/to-hex (m/irand) 8)]
    (save-canvas canvas (str "results/ex05/" r ".jpg"))))

    (println (str variation1 " " variation2))

    (loop [xs particles]
      (when @running
        (recur (doall (map mv-fun xs)))))
    
    ))


(example-05)

