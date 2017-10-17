(ns clojure2d.pbrt.geometry-test
  (:require [clojure2d.pbrt.geometry :refer :all]
            [expectations :refer :all]
            [clojure2d.math.vector :as v]))


(def b2 (make-bounds2 (v/vec2 0 0) (v/vec2 1 1)))
(def b3 (make-bounds3 (v/vec3 0 0 0) (v/vec3 1 1 1)))

;; equality

(expect (make-bounds2 (v/vec2 0 0) (v/vec2 1 1)) b2)
(expect (make-bounds3 (v/vec3 0 0 0) (v/vec3 1 1 1)) b3)

;; diagonal

(expect (v/vec2 1.0 1.0) (diagonal b2))
(expect (v/vec3 1.0 1.0 1.0) (diagonal b3))

;; lerp

(expect (v/vec2 0.5 0.5) (lerp b2 (v/vec2 0.5 0.5)))
(expect (v/vec3 0.5 0.5 0.5) (lerp b3 (v/vec3 0.5 0.5 0.5)))

;; distances, pbrt3 tests

(expect 0.0 (distance b3 (v/vec3 0.5 0.5 0.5)))
(expect 0.0 (distance b3 (v/vec3 0 0 1)))
(expect 0.0 (distance b3 (v/vec3 0.25 0.8 1.0)))
(expect 0.0 (distance b3 (v/vec3 0 0.25 0.8)))
(expect 0.0 (distance b3 (v/vec3 0.7 0 0.8)))

(expect 0.0 (distance b2 (v/vec2 0.5 0.5)))
(expect 0.0 (distance b2 (v/vec2 0 0)))
(expect 0.0 (distance b2 (v/vec2 0.25 0.8)))
(expect 0.0 (distance b2 (v/vec2 0 0.25)))
(expect 0.0 (distance b2 (v/vec2 0.7 0)))

(expect 5.0 (distance b3 (v/vec3 6 1 1)))
(expect 10.0 (distance b3 (v/vec3 0 -10 1)))

(expect 5.0 (distance b2 (v/vec2 6 1)))
(expect 10.0 (distance b2 (v/vec2 0 -10)))

(expect (+ 9.0 49.0 100.0) (distance-sq b3 (v/vec3 4 8 -10)))
(expect (+ 36.0 100.0 49) (distance-sq b3 (v/vec3 -6 -10 8)))

(expect 0.0 (distance (make-bounds3 (v/vec3 -1 -3 5)
                                    (v/vec3 2 -2 18))
                      (v/vec3 -0.99 -2 5)))

(expect (+ 4.0 36.0 16.0) (distance-sq (make-bounds3 (v/vec3 -1 -3 5)
                                                     (v/vec3 2 -2 18))
                                       (v/vec3 -3 -9 22)))
