(ns clojure2d.color-test
  (:require [clojure2d.color :refer :all]
            [clojure2d.color.blend :as b]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [clojure.test :refer :all]))

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

(deftest blend-test
  (is (== 255.0 (b/normal 100 255)))
  (is (== 0.0 (b/normal 200 0)))
  (is (== 255.0 (b/normal 0 255)))
  (is (== 0.0 (b/normal 1 0)))

  (is (== 255.0 (b/add 100 200)))
  (is (== 150.0 (b/add 50 100)))
  (is (== 255.0 (b/add 0 255)))
  (is (== 255.0 (b/add 255 0)))

  (is (== (mod 300 256.0) (b/madd 100 200)))
  (is (== 150.0 (b/madd 50 100)))
  (is (== 255.0 (b/madd 0 255)))
  (is (== 255.0 (b/madd 255 0)))

  (is (== 0.0 (b/subtract 100 200)))
  (is (== 50 (b/subtract 100 50)))
  (is (== 0.0 (b/subtract 0 255)))
  (is (== 255.0 (b/subtract 255 0)))

  (is (== (mod -100 256.0) (b/msubtract 100 200)))
  (is (== 50 (b/msubtract 100 50)))
  (is (== 1.0 (b/msubtract 0 255)))
  (is (== 255.0 (b/msubtract 255 0))))

;; test color converters

(defn colorspace-validity
  "Test if colorspace conversion works properly"
  [cs]
  (let [[to from] (colorspaces cs)
        [to* from*] (colorspaces* cs)
        c (concat (named-colors-list)
                  (repeatedly 30000 #(v/vec4 (v/generate-vec3 (fn [] (r/irand 256))) 255))
                  (repeatedly 30000 #(v/generate-vec3 (fn [] (r/irand 256))))
                  (repeatedly 30000 r/irand))]
    (empty? (concat (filter false? (map = (map to-color c) (map (comp (fn [v] (v/fmap v #(m/round %))) from to) c)))
                    (filter false? (map = (map to-color c) (map (comp (fn [v] (v/fmap v #(m/round %))) from* to*) c)))))))

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
  (is (colorspace-validity :JAB))
  (is (colorspace-validity :HunterLAB))
  (is (colorspace-validity :LCH))
  (is (colorspace-validity :JCH))
  (is (colorspace-validity :LUV))
  (is (colorspace-validity :XYZ))
  (is (colorspace-validity :Yxy))
  (is (colorspace-validity :LMS))
  (is (colorspace-validity :IPT))
  (is (colorspace-validity :OHTA))
  (is (colorspace-validity :OSA)))

;; chroma.js tests

(deftest chroma-example
  (is "#ff6d93" (-> :pink
                    (darken)
                    (saturate 2.0)
                    (format-hex))))

(deftest chroma-api
  (is "#ff69b4" (format-hex :hotpink))
  (is "#ff3399" (format-hex "#ff3399"))
  (is "#ff3399" (format-hex "F39"))
  (is "#ff3399" (format-hex 0xff3399))
  (is "#ff3399" (format-hex (color 0xff 0x33 0x99)))
  (is "#ff3399" (format-hex (color 255 51 153)))
  (is "#ff3399" (format-hex [255 51 153]))
  (is "#ff3399" (format-hex (from-HSL [330.0 1.0 0.6])))
  (is "#80ff80" (format-hex (from-HSL [120 1 0.75])))
  (is "#85d4d5" (format-hex (from-LCH [80 25 200])))
  (is "#aad28c" (format-hex (from-LCH [80 40 130])))
  (is "#b400b4" (format-hex (mix :red :blue 0.5)))
  (is "#dd0080" (format-hex (mix :red :blue 0.25)))
  (is "#8000dd" (format-hex (mix :red :blue 0.75)))
  (is "#800080" (format-hex (lerp :red :blue)))
  (is "#00ff00" (format-hex (lerp :red :blue :HSL 0.5))) ;; different than chroma, shortest path
  (is "#ca0088" (format-hex (lerp :red :blue :LAB 0.5))))

(def average-colors ["#ddd", :yellow, :red, :teal])

(deftest chroma-average
  (is "#b79757" (format-hex (average average-colors)))
  (is "#d3a96a" (format-hex (average average-colors :LAB))))

(deftest chroma-valid
  (is (valid-color? :red))
  (is (not (valid-color? :bread)))
  (is (valid-color? "#F0000D"))
  (is (not (valid-color? "#FOOOOD"))))

(deftest chroma-blend
  (is "#47af22" (format-hex (b/blend-colors b/multiply 0x4cbbfc 0xeeee22)))
  (is "#4cbb22" (format-hex (b/blend-colors b/darken 0x4cbbfc 0xeeee22)))
  (is "#eeeefc" (format-hex (b/blend-colors b/lighten 0x4cbbfc 0xeeee22))))

(deftest chroma-contrast
  (is 1.72 (m/approx (contrast-ratio :pink :hotpink)))
  (is 6.12 (m/approx (contrast-ratio :pink :purple))))

(deftest chroma-distance
  (is 96.96 (m/approx (delta-e-cie "fff" "ff0")))
  (is 122.18 (m/approx (delta-e-cie "fff" "f0f")))
  (is 255.0 (m/approx (euclidean "fff" "ff0")))
  (is 255.0 (m/approx (euclidean "fff" "f0f")))
  (is 1.64 (m/approx (delta-e-cmc 0xededee 0xedeeed)))
  (is 3.15 (m/approx (delta-e-cmc 0xececee 0xeceeec)))
  (is 7.36 (m/approx (delta-e-cmc 0xe9e9ee 0xe9eee9)))
  (is 14.85 (m/approx (delta-e-cmc 0xe4e4ee 0xe4eee4)))
  (is 21.33 (m/approx (delta-e-cmc 0xe0e0ee 0xe0eee0))))

(deftest chroma-color
  (is "#ff000080" (format-hex (set-alpha :red 128)))
  (is 128.0 (alpha (color 255 0 0 128)))
  (is "#c93384" (format-hex (darken :hotpink)))
  (is "#930058" (format-hex (darken :hotpink 2.0)))
  (is "#74003f" (format-hex (darken :hotpink 2.6)))
  (is "#ff9ce6" (format-hex (brighten :hotpink)))
  (is "#ffd1ff" (format-hex (brighten :hotpink 2.0)))
  (is "#ffffff" (format-hex (brighten :hotpink 3.0)))
  (is "#4b83ae" (format-hex (saturate :slategray)))
  (is "#0087cd" (format-hex (saturate :slategray 2.0)))
  (is "#008bec" (format-hex (saturate :slategray 3.0)))
  (is "#e77dea" (format-hex (desaturate :hotpink)))
  (is "#cd8ca8" (format-hex (desaturate :hotpink 2.0)))
  (is "#b299a3" (format-hex (desaturate :hotpink 3.0))) ;; differs a little bit from original, due to rounding
  (is "#a10000" (format-hex (modulate :orangered :LAB 0 0.5)))
  (is "#63c56c" (format-hex (modulate :darkseagreen :LCH 1 2.0)))
  (is "#eb8787" (format-hex (set-channel :skyblue :HSL 0 0)))
  (is "#ce8ca9" (format-hex (set-channel :hotpink :LCH 1 30)))
  (is 57.57 (m/approx (get-channel :orangered :LAB 0)))
  (is 0.5 (m/approx (get-channel :orangered :HSL 2)))
  (is 69.0 (m/approx (get-channel :orangered 1)))
  (is 1.0 (/ (relative-luma :white) 255.0))
  (is 0.81 (m/approx (/ (relative-luma :aquamarine) 255.0)))
  (is 0.35 (m/approx (/ (relative-luma :hotpink) 255.0)))
  (is 0.07 (m/approx (/ (relative-luma :darkslateblue) 255.0)))
  (is 0.0 (/ (relative-luma :black) 255.0)))

;; test iq palette generator

;; test paletton generator

;; test nearest color distance
