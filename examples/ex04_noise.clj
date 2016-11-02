(ns examples.ex04-noise
  (:require [clojure2d.core :refer :all]
            [clojure2d.math :as m]
            [clojure2d.math.joise :as j]
            [clojure2d.utils :as u])
  (:import  [java.awt Color]))

(def canvas (create-canvas 200 200))

(defn loop-noise
  ""
  [canvas n]
  (u/doloop 
   [y 180]
   (u/doloop
    [x 180]
    (let [xx (/ x 50.0)
          yy (/ y 50.0)
          n (int (m/cnorm (n xx yy) 0 1 0 255))]
      (set-color canvas (Color. n n n))
      (rect canvas (+ x 10) (+ y 10) 1 1))))
  canvas)

(defn draw-noise
  ""
  [n]
  (with-canvas canvas
    (set-background (Color. 0 0 0))
    (loop-noise n))
  nil)

(show-window canvas "noise" 600 600 1)

(defmethod key-pressed ["noise" \space] [_]
  (let [r (u/to-hex (m/irand) 8)]
    (save-canvas canvas (str "results/ex04/" r ".jpg"))))

; results/ex04/B129FE12.jpg
(draw-noise m/noise)

; results/ex04/53230C85.jpg
(draw-noise j/perlin-noise)

;; basis types
(keys j/basis-type)
;; => (:value :gradient :gradval :simplex)
(keys j/interpolation-type)
;; => (:none :linear :cubic :quintic)

; results/ex04/5202BCC1.jpg
; results/ex04/13BCACBE.jpg
; results/ex04/AB1C33D1.jpg
(let [t (rand-nth (keys j/basis-type))
      i (rand-nth (keys j/interpolation-type))]
  (draw-noise (j/make-noise (j/auto-correct (j/make-basis {:type t
                                                           :interpolation i}) 25000)))
  [t i])

;; cells

;saving: results/ex04/D9164162.jpg
;saving: results/ex04/A72AC802.jpg
;saving: results/ex04/B14C2C21.jpg
(let [coeffs [(m/drand -5 5) (m/drand -5 5) (m/drand -5 5) (m/drand -5 5)]]
  (draw-noise (j/make-noise (j/auto-correct (j/make-cell {:coeffs coeffs}) 500)))
  coeffs)

;; combined / fractal / slow!


;saving: results/ex04/91DA588E.jpg
;saving: results/ex04/54F7EC04.jpg
;saving: results/ex04/08A1FD78.jpg
;saving: results/ex04/9B37E417.jpg
;saving: results/ex04/9973D635.jpg
;saving: results/ex04/2991271E.jpg
(let [type (rand-nth (keys j/fractal-type))
      b [j/make-random-basis-module
         j/make-random-cell-module]
      l (m/drand 1 3)
      f (m/drand 1 3)
      params {:type type
              :lacunarity l
              :frequency f
              :octaves [[1 ((rand-nth b))]
                        [1 ((rand-nth b))]
                        [1 ((rand-nth b))]]}]
  (draw-noise (j/make-noise (j/make-fractal params)))
  [type l f])
