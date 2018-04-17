(ns clojure2d.color-test
  (:require [clojure2d.color :refer :all] 
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [clojure.test :refer :all]))

;; mods

(deftest clamp-test
  (is (== 255 (lclamp255 254.6)))
  (is (== 0 (lclamp255 -1)))
  (is (== 22 (lclamp255 22.49)))
  (is (== 23 (lclamp255 22.5)))
  
  (is (== 254.6 (clamp255 254.6)))
  (is (== 0.0 (clamp255 -1)))
  (is (== 22.49 (clamp255 22.49)))
  (is (== 255.0 (clamp255 255.5)))

  (is (== 255 (mod255 0xffff)))
  (is (== 0 (mod255 0)))

  (is (== 0.0 (clamp1 -2)))
  (is (== 1.0 (clamp1 2)))
  (is (== 0.34 (clamp1 0.34)))

  (is (m/approx-eq 0.34 (mod1 1.34)))
  (is (m/approx-eq 0.34 (mod1 -1.34))))

;; color protocol

(def cv3 (v/vec3 245 245 220))
(def cv4 (v/vec4 245 245 220 255))
(def cc (java.awt.Color. 245 245 220))

(deftest type-conversion-test
  (is (m/approx-eq 243.195 (luma :beige))) ;; => 243.19577500000003
  (is (m/approx-eq 243.195 (luma cv3)))
  (is (m/approx-eq 243.195 (luma cv4)))
  (is (m/approx-eq 243.195 (luma cc)))

  (is (= cv4 (to-color :beige)))
  (is (= cv4 (to-color cv3)))
  (is (= cv4 (to-color cc)))

  (is (= cc (to-awt-color :beige)))
  (is (= cc (to-awt-color cv3)))
  (is (= cc (to-awt-color cv4))))

(deftest alpha-test
  (is (= (v/vec4 245 245 220 100) (set-alpha cv4 100)))
  (is (= (v/vec4 245 245 220 100) (to-color (set-awt-alpha cc 100)))))

(deftest make-color-test
  (is (= cc (awt-color cv3 255)))
  (is (= cc (awt-color 245 245 220)))
  (is (= cc (awt-color 245 245 220 255)))
  (is (= cv4 (color cv3 255)))
  (is (= cv4 (color 245 245 220)))
  (is (= cv4 (color 245 245 220 255))))

(deftest hue-test
  (is (= 60.0 (hue (color :beige)))))

;; test blends
(def c50 (* rev255 50))
(def c100 (* rev255 100))
(def c200 (* rev255 200))

(deftest blend-test
  (is (== c100 (blend-none c100 1)))
  (is (== c200 (blend-none c200 0)))
  (is (== 0.0 (blend-none 0 1)))
  (is (== 1.0 (blend-none 1 0)))

  (is (== 1.0 (blend-add c100 c200)))
  (is (== (* rev255 150) (blend-add c50 c100)))
  (is (== 1.0 (blend-add 0 1)))
  (is (== 1.0 (blend-add 1 0)))

  (is (== (m/frac (+ c100 c200)) (blend-madd c100 c200)))
  (is (== (* rev255 150) (blend-madd c50 c100)))
  (is (== 0.0 (blend-madd 0 1)))
  (is (== 0.0 (blend-madd 1 0)))

  (is (== 0.0 (blend-subtract c100 c200)))
  (is (== c50 (blend-subtract c100 c50)))
  (is (== 0.0 (blend-subtract 0 1)))
  (is (== 1.0 (blend-subtract 1 0)))

  (is (== (m/frac (- c100 c200)) (blend-msubtract c100 c200)))
  (is (== c50 (blend-msubtract c100 c50)))
  (is (== 0.0 (blend-msubtract 0 1)))
  (is (== 0.0 (blend-msubtract 1 0))))

;; test color converters

(defn colorspace-validity
  "Test if colorspace conversion works properly"
  [cs]
  (let [[to from] (colorspaces cs)
        c (concat (map to-color (vals html-colors-map))
                  (repeatedly 100000 #(v/vec4 (v/generate-vec3 (fn [] (r/irand 256))) 255)))]
    (empty? (filter false? (map = c (map (comp (fn [v] (v/applyf v #(m/round %))) from to) c))))))

(deftest colorspace-test
  (is (colorspace-validity :RGB))
  (is (colorspace-validity :sRGB))
  (is (colorspace-validity :Cubehelix))
  (is (colorspace-validity :YIQ))
  (is (colorspace-validity :YUV))
  (is (colorspace-validity :YCgCo))
  (is (colorspace-validity :YCbCr))
  (is (colorspace-validity :YDbDr))
  (is (colorspace-validity :YPbPr))
  (is (colorspace-validity :GLHS))
  (is (colorspace-validity :HCL))
  (is (colorspace-validity :HSL))
  (is (colorspace-validity :HSB))
  (is (colorspace-validity :HSV))
  (is (colorspace-validity :HSI))
  (is (colorspace-validity :HWB))
  (is (colorspace-validity :CMY))
  (is (colorspace-validity :LAB))
  (is (colorspace-validity :HLAB))
  (is (colorspace-validity :LCH))
  (is (colorspace-validity :LUV))
  (is (colorspace-validity :XYZ))
  (is (colorspace-validity :YXY))
  (is (colorspace-validity :OHTA)))

;; test iq palette generator

;; test paletton generator

;; test nearest color distance
