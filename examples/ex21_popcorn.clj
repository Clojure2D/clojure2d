;; variation of the http://www.iquilezles.org/www/articles/popcorns/popcorns.htm

(ns examples.ex21-popcorn
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.vector :as v]
            [clojure2d.extra.variations :refer :all])
  (:import  [java.awt Color]
            [clojure2d.math.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:const width 900)
(def ^:const height 900)

(def ^:const point-step 1) ; 0.01 - 2.0
(def ^:const point-size 1) ; 0.6 - 1.2
(def ^:const alpha 20)

(defn make-particle
  ""
  []
  (let [r (m/drand 0.5 m/TWO_PI)
        a (m/drand m/TWO_PI)]
    (Vec2. (* r (m/qcos a)) (* r (m/qsin a)))))

(def sinusoidal (make-variation :sinusoidal 1.0 {}))

(defn move-particle
  ""
  [canvas ^Vec2 vrand fun ^Vec2 in]
  (let [^Vec2 v (v/add in (v/mult (sinusoidal (v/mult (->> in
                                                           (v/add vrand)
                                                           fun
                                                           sinusoidal
                                                           (v/add vrand)) m/TWO_PI)) point-step))
        nx (.x v)
        ny (.y v)
        screenx (m/norm nx -8.0 8.0 0 width)
        screeny (m/norm ny -8.0 8.0 0 height)]
    (if (and (<= 40 screeny (- height 41)) (<= 40 screenx (- width 41)))
      (do
        (with-canvas canvas
          (set-color (Color. 49 52 59 alpha))
          (set-stroke point-size)
          (point screenx screeny))
        (Vec2. nx ny))
      (make-particle))))

(defn example-21
  []
  (let [canvas (create-canvas width height)
        [frame running] (show-window canvas "popcorn" width height 25)
        variation1 (rand-nth variation-list)
        variation2 (rand-nth variation-list)
        vrand (Vec2. (m/drand -1 1) (m/drand -1 1))
        mv-fun (partial move-particle canvas vrand (comp (make-variation variation2 1.0 {}) (make-variation variation1 1.0 {})))
        particles (repeatedly 15000 make-particle)]
    
    (defmethod key-pressed ["popcorn" \space] [_]
      (binding [*jpeg-image-quality* 0.9]
        (save-canvas canvas (str (next-filename "results/ex21/") ".jpg"))))

    (with-canvas canvas
      (set-background (Color. 240 240 240)))

    (println (str variation1 " " variation2))

    (loop [xs particles]
      (when @running
        (recur (doall (map mv-fun xs)))))
    
    ))

(example-21)

