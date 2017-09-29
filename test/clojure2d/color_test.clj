(ns clojure2d.color-test
  (:require [clojure2d.color :refer :all]
            [expectations :refer :all]
            [clojure2d.math.vector :as v]
            [clojure2d.math :as m]
            [clojure2d.math.random :as r]))


;; mods

(expect 255 (clamp255 254.6))
(expect 0 (clamp255 -1))
(expect 22 (clamp255 22.49))
(expect 23 (clamp255 22.5))

(expect 255 (mod255 0xffff))
(expect 0 (mod255 0))

(expect 0.0 (clamp1 -2))
(expect 1.0 (clamp1 2))
(expect 0.34 (clamp1 0.34))

(expect (approximately 0.34) (mod1 1.34))
(expect (approximately 0.34) (mod1 -1.34))

;; color protocol

(def cv3 (v/vec3 245 245 220))
(def cv4 (v/vec4 245 245 220 255))
(def cc (java.awt.Color. 245 245 220))


(expect (approximately 243.195) (to-luma :beige)) ;; => 243.19577500000003
(expect (approximately 243.195) (to-luma cv3))
(expect (approximately 243.195) (to-luma cv4))
(expect (approximately 243.195) (to-luma cc))

(expect cv4 (to-color :beige))
(expect cv4 (to-color cv3))
(expect cv4 (to-color cc))

(expect cc (to-awt-color :beige))
(expect cc (to-awt-color cv3))
(expect cc (to-awt-color cv4))

;; alpha
(expect (v/vec4 245 245 220 100) (set-alpha cv4 100))
(expect (v/vec4 245 245 220 100) (to-color (set-awt-alpha cc 100)))

;; make-color
(expect cc (make-awt-color cv3 255))
(expect cc (make-awt-color 245 245 220))
(expect cc (make-awt-color 245 245 220 255))
(expect cv4 (make-color cv3 255))
(expect cv4 (make-color 245 245 220))
(expect cv4 (make-color 245 245 220 255))

;; hue
(expect (double (m/round (/ (* 255 60) 360))) (get-hue (make-color :beige)))

;; double array
(let [r (r/irand 256)]
  (expect (/ r 255.0) (get-r255 r)))

;; test blends
(def c50 (get-r255 50))
(def c100 (get-r255 100))
(def c200 (get-r255 200))

(expect c100 (blend-none c100 1))
(expect c200 (blend-none c200 0))
(expect 0.0 (blend-none 0 1))
(expect 1.0 (blend-none 1 0))

(expect 1.0 (blend-add c100 c200))
(expect (get-r255 150) (blend-add c50 c100))
(expect 1.0 (blend-add 0 1))
(expect 1.0 (blend-add 1 0))

(expect (m/frac (+ c100 c200)) (blend-madd c100 c200))
(expect (get-r255 150) (blend-madd c50 c100))
(expect 0.0 (blend-madd 0 1))
(expect 0.0 (blend-madd 1 0))

(expect 0.0 (blend-subtract c100 c200))
(expect c50 (blend-subtract c100 c50))
(expect 0.0 (blend-subtract 0 1))
(expect 1.0 (blend-subtract 1 0))

(expect (m/frac (- c100 c200)) (blend-msubtract c100 c200))
(expect c50 (blend-msubtract c100 c50))
(expect 0.0 (blend-msubtract 0 1))
(expect 0.0 (blend-msubtract 1 0))

;; test color converters

;; test iq palette generator

;; test paletton generator

;; test nearest color distance
