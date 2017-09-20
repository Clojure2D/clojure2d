;; variation of the http://www.iquilezles.org/www/articles/popcorns/popcorns.htm

(ns examples.ex21-popcorn
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]
            [clojure2d.math.vector :as v]
            [clojure2d.extra.variations :refer :all]
            [clojure2d.math.joise :as j])
  (:import [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^long w 900)
(def ^:const ^long h 900)

(def ^:const ^double point-step 0.02) ; 0.01 - 2.0
(def ^:const ^double point-size 1.0) ; 0.6 - 1.2
(def ^:const ^int alpha 20)

(defn make-particle
  ""
  []
  (let [r (r/drand 0.5 m/TWO_PI)
        a (r/drand m/TWO_PI)]
    (Vec2. (* r (m/qcos a)) (* r (m/qsin a)))))

(def sinusoidal (make-variation :sinusoidal 1.0 {}))

(defn move-particle
  ""
  [^Vec2 vrand noisef fun canvas ^Vec2 in]
  (let [^Vec2 nf (noisef in)
        ^Vec2 v (v/add in (v/mult (sinusoidal (v/mult (->> in
                                                           (v/add vrand)
                                                           fun
                                                           (v/add nf)) m/TWO_PI)) point-step))
        nx (.x v)
        ny (.y v)
        ^double screenx (m/norm nx -8.0 8.0 0 w)
        ^double screeny (m/norm ny -8.0 8.0 0 h)]
    (if (and (<= 40 screeny (- h 41)) (<= 40 screenx (- w 41)))
      (do
        (point canvas screenx screeny)
        (Vec2. nx ny))
      (make-particle))))

(defn make-random-noise
  ""
  []
  (let [type (rand-nth (keys j/fractal-type))
        b [j/make-random-basis-module
           j/make-random-basis-module
           j/make-random-cell-module]
        l (r/drand 1 3)
        f (r/drand 1 3)
        params {:type type
                :lacunarity l
                :frequency f
                :octaves [((rand-nth b))
                          ((rand-nth b))]}]
    (j/make-noise (j/make-fractal params))))

(defn get-noise
  ""
  [f ^Vec2 in]
  (let [^Vec2 in (v/mult in 0.3)]
    (Vec2. (- ^double (f (.x in) (.y in)) 0.5)
           (- ^double (f (.y in) (.x in) 0.3) 0.5))))

(defn example-21
  []
  (binding [*skip-random-variations* true]
    (let [canvas (create-canvas w h)
          window (show-window canvas "popcorn")
          field-config (make-random-configuration)
          field (make-combination field-config)
          vrand (Vec2. (r/drand -1 1) (r/drand -1 1))
          noisef (if (r/brand 0.2) (partial get-noise (make-random-noise)) (fn [_] (Vec2. 0.0 0.0)))
          mv-fun (partial move-particle vrand noisef field)
          
          particles (repeatedly 15000 make-particle)
          looper (fn [canvas] (loop [xs particles]
                                (if (window-active? window)
                                  (recur (mapv (partial mv-fun canvas) xs))
                                  canvas)))]
      
      (defmethod key-pressed ["popcorn" \space] [_ _]
        (binding [*jpeg-image-quality* 0.9]
          (save-canvas canvas (next-filename "results/ex21/" ".jpg"))))

      (println field-config)
      
      (with-canvas canvas
        (set-background 240 240 240)
        (set-color 49 52 59 alpha)
        (set-stroke point-size)
        (looper)))))

(example-21)

