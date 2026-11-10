(ns clojure2d.color.adjustments-test
  "Color adjustments, CSS filters and color vision deficiency of `clojure2d.color` (area G of the color audit),
  checked against `utils/color/gen_filters.py`: W3C Filter Effects Level 1 matrices written from the specification,
  chroma.js 2.6.0 and colour-science's Machado et al. (2009) matrices; the author's designs as properties."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [fastmath.core :as m]
            [clojure2d.color :as c]
            [clojure2d.test-predicates :refer [throws-data?]]))

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
  ;; spec matrices on encoded sRGB, results clamped; sepia and grayscale amounts above 1 clamped to 1
  (check-filter :sepia c/sepia 1.0e-9 any?)
  (check-filter :grayscale c/grayscale 1.0e-9 any?)
  (check-filter :contrast c/contrast 1.0e-9 any?)
  (check-filter :brightness c/exposure 1.0e-9 any?)
  (check-filter :saturate c/saturation 1.0e-9 any?)
  (check-filter :hue-rotate c/hue-rotate 1.0e-9 any?)
  (testing "hue-rotate keeps grays"
    (doseq [g [0 64 128 200 255] a [17.0 90.0 180.0 -45.0]]
      (is (m/<= (max-diff [g g g] (take 3 ((c/hue-rotate a) [g g g]))) 1.0e-9) (str g " " a)))))

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

(deftest whiten-blacken-test
  (doseq [col [[255 0 0] [200 50 80] [40 160 90] [200 150 160]] amt [0.2 0.7 -0.3 -0.8]
          [f ch] [[c/whiten 1] [c/blacken 2]]
          :let [v ((c/to-HWB col) ch)
                expected (if (neg? amt) (+ v (* amt v)) (+ v (* amt (- 1.0 v))))
                actual ((c/to-HWB (f col amt)) ch)]
          :when (m/< (+ ((c/to-HWB col) (- 3 ch)) expected) 0.99)]
    (is (m/<= (m/abs (m/- expected actual)) 1.0e-9) (str f " " col " " amt)))
  (testing "ends and clamping"
    (let [g (c/whiten [200 50 80] 1.0)]
      (is (m/<= (max-diff [(g 0) (g 0)] [(g 1) (g 2)]) 1.0e-9) "amount 1: W = 1, a gray (B kept, HWB normalization)"))
    (is (m/<= (max-diff [255 255 255] (take 3 (c/whiten [255 0 0] 1.0))) 1.0e-9) "B = 0: white")
    (is (= (c/whiten [200 50 80] 1.0) (c/whiten [200 50 80] 5.0)))
    (is (= (c/blacken [200 50 80] -1.0) (c/blacken [200 50 80] -3.0)))
    (is (m/< 0.1 ((c/to-HWB (c/whiten [255 0 0] 0.2)) 1)) "pure red whitens")))

(deftest complementary-test
  (doseq [cs [:HSV :HSL :HWB :GLHS] col [[200 50 50] [30 160 90] [40 60 220]]]
    (is (m/<= (max-diff col (take 3 (c/complementary (c/complementary col cs) cs))) 1.0e-9) (str cs " " col)))
  (testing "PalettonHSV (default): hue turned, saturation and value kept"
    (let [col [200 50 80]
          a (c/to-PalettonHSV col)
          b (c/to-PalettonHSV (c/complementary col))]
      (is (m/< (m/abs (m/- 180.0 (m/abs (m/- (a 0) (b 0))))) 1.0e-6))
      (is (m/< (m/abs (m/- (a 1) (b 1))) 1.0e-6))
      (is (m/< (m/abs (m/- (a 2) (b 2))) 1.0e-6)))))

(deftest cvd-vars-test
  (let [{:keys [colors cvd-vars]} @reference]
    (doseq [[k f] {:protanopia c/protanopia :deuteranopia c/deuteranopia :protanomaly c/protanomaly
                   :deuteranomaly c/deuteranomaly :tritanopia c/tritanopia :tritanomaly c/tritanomaly
                   :achromatopsia c/achromatopsia :achromatomaly c/achromatomaly}
            [col e] (map vector colors (cvd-vars k))]
      (is (m/<= (max-diff e (vec (f col))) 1.0e-9) (str k " " col)))))

(deftest make-LCH-test
  (doseq [cs [:LAB :LUV :Oklab] col [[200 50 50] [30 160 90] [40 60 220]]
          :let [[to from] (c/make-LCH cs)]]
    (is (m/<= (max-diff col (take 3 (from (to col)))) 1.0e-9) (str cs " " col))))

;; G2: contracts

(deftest negative-amount-test
  (doseq [f [c/sepia c/grayscale c/saturation c/contrast c/exposure]]
    (is (throws-data? {:amount -0.5} #(f -0.5)) (str f)))
  (testing "brightness (offset) and hue-rotate accept negative values"
    (is (fn? (c/brightness -0.2)))
    (is (fn? (c/hue-rotate -20.0)))))

(deftest adjust-modulate-errors-test
  (doseq [f [c/adjust c/modulate]]
    (is (throws-data? {:colorspace :Foo} #(f :red :Foo 0 1.0)) (str f))
    (is (throws-data? {:channel 7} #(f :red :HSV 7 1.0)) (str f))
    (is (throws-data? {:channel 7} #(f :red 7 1.0)) (str f))
    (is (throws-data? {:channel -1} #(f :red -1 1.0)) (str f))))

(defn- lightness-linear? [g]
  (let [l0 ((c/to-LAB (g 0.0)) 0)
        l1 ((c/to-LAB (g 1.0)) 0)]
    (every? (fn [^double t] (m/< (m/abs (m/- (m/lerp l0 l1 t) ((c/to-LAB (g t)) 0))) 0.1))
            (range 0.0 1.0 0.01))))

(deftest correct-luma-test
  (testing "increasing and decreasing lightness"
    (doseq [stops [[:navy :yellow] [:yellow :navy] [:white :black] [:lightyellow :orangered :deeppink :darkred]]]
      (is (lightness-linear? (c/correct-luma (c/gradient stops))) (str stops))))
  (testing "palettes sorted by lightness, both orders, keep their size"
    (let [pal (sort-by c/luma (c/palette :guell))]
      (is (= (count pal) (count (c/correct-luma pal))))
      (is (= (count pal) (count (c/correct-luma (reverse pal)))))))
  (testing "non-monotonic lightness"
    (is (throws-data? {:lightness :non-monotonic} #(c/correct-luma (c/gradient [:black :white :black]))))))

(deftest docstrings-test
  (doseq [s '[whiten blacken brighten darken saturate desaturate adjust modulate tinter correct-luma apply-theme
              color-themes fe-color-matrix sepia contrast exposure brightness saturation hue-rotate grayscale
              achromatomaly achromatopsia deuteranomaly deuteranopia protanomaly protanopia tritanomaly tritanopia
              complementary make-LCH]]
    (is (some? (:doc (meta (ns-resolve 'clojure2d.color s)))) (str s))))
