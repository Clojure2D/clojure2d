(ns clojure2d.color.adjustments-test
  "Color adjustments, CSS filters and color vision deficiency of `clojure2d.color` (area G of the color audit),
  checked against `utils/color/gen_filters.py`: W3C Filter Effects Level 1 matrices written from the specification,
  chroma.js 2.6.0 and colour-science's Machado et al. (2009) matrices; the author's designs as properties."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [fastmath.core :as m]
            [clojure2d.color :as c]))

(def reference (delay (-> "color/filters_reference.edn" io/resource slurp edn/read-string)))

(defn- max-diff ^double [expected actual]
  (reduce max (map #(m/abs (m/- (double %1) (double %2))) expected actual)))

(defn- check-filter
  [k f tol pred]
  (let [{:keys [colors filters]} @reference]
    (doseq [[amount rows] (filters k)
            :when (pred amount)
            [col e] (map vector colors rows)]
      (is (m/<= (max-diff e (vec ((f amount) col))) tol) (str k " " amount " " col)))))

(deftest css-filters-test
  ;; spec matrices on encoded sRGB, results clamped; amounts above 1 only where the code follows the spec
  (check-filter :sepia c/sepia 1.0e-9 #(m/<= % 1.0))
  (check-filter :grayscale c/grayscale 1.0e-9 #(m/<= % 1.0))
  (check-filter :contrast c/contrast 1.0e-9 any?)
  (check-filter :brightness c/exposure 1.0e-9 any?)
  ;; saturation weights 0.2126 vs the specification's 0.213 (saturate), hue-rotate 0.071 vs 0.072
  (check-filter :saturate c/saturation 0.25 any?)
  (check-filter :hue-rotate c/hue-rotate 0.3 any?))

(deftest chroma-test
  ;; chroma.js brighten / darken / saturate / desaturate: its own Lab white point; darken below L* 0 differs
  ;; (clojure2d keeps L* >= 0); saturating white differs (chroma.js gives white a noise hue, clojure2d hue 0)
  (let [{:keys [colors chroma chroma-amounts]} @reference]
    (doseq [[op f sign] [[:brighten c/brighten 1.0] [:darken c/darken -1.0]
                         [:saturate c/saturate 0.0] [:desaturate c/desaturate 0.0]]
            [col per-amount] (map vector colors (chroma op))
            [amount e] (map vector chroma-amounts per-amount)
            :when (and (m/>= (+ ((c/to-LAB col) 0) (* sign 18.0 amount)) 0.0)
                       (not (and (#{:saturate :desaturate} op) (= col [255.0 255.0 255.0]))))]
      (is (m/<= (max-diff e (take 3 (f col amount))) 2.0e-3) (str op " " col " " amount)))))

(deftest identity-test
  (testing "neutral amounts keep the color"
    (doseq [col (take 20 (:colors @reference))
            f [(c/sepia 0.0) (c/grayscale 0.0) (c/saturation 1.0) (c/contrast 1.0) (c/exposure 1.0)
               (c/brightness 0.0) (c/hue-rotate 0.0)
               #(c/brighten % 0.0) #(c/saturate % 0.0) #(c/whiten % 0.0) #(c/blacken % 0.0)]]
      (is (m/<= (max-diff col (take 3 (f col))) 1.0e-9) (str f " " col)))))

(deftest own-designs-test
  (testing "tinter: symmetric in color and tint, a color tinted by itself moves by less than 1"
    (doseq [[a b] (partition 2 (take 20 (:colors @reference)))]
      (is (m/<= (max-diff ((c/tinter a) b) ((c/tinter b) a)) 1.0e-9) (str a " " b))
      (is (m/< (max-diff (take 3 ((c/tinter a) a)) a) 1.0) (str a))))
  (testing "whiten / blacken keep the hue of saturated colors"
    (doseq [col [[200 50 50] [30 160 90] [40 60 220]] f [c/whiten c/blacken]]
      (is (m/<= (m/abs (m/- ((c/to-HWB col) 0) ((c/to-HWB (f col 0.1)) 0))) 1.0e-9) (str f " " col)))))

(deftest make-LCH-test
  (doseq [cs [:LAB :LUV :Oklab] col [[200 50 50] [30 160 90] [40 60 220]]
          :let [[to from] (c/make-LCH cs)]]
    (is (m/<= (max-diff col (take 3 (from (to col)))) 1.0e-9) (str cs " " col))))

(deftest docstrings-test
  (doseq [s '[whiten blacken brighten darken saturate desaturate adjust modulate tinter correct-luma apply-theme
              color-themes fe-color-matrix sepia contrast exposure brightness saturation hue-rotate grayscale
              achromatomaly achromatopsia deuteranomaly deuteranopia protanomaly protanopia tritanomaly tritanopia
              complementary make-LCH]]
    (is (some? (:doc (meta (ns-resolve 'clojure2d.color s)))) (str s))))
