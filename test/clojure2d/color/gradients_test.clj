(ns clojure2d.color.gradients-test
  "Gradients and palettes of `clojure2d.color` (area C of the color audit), checked against
  `utils/color/gen_gradients.py`: chroma.js 2.6.0 scales, coloraide 8.14.1 interpolation, d3-scale-chromatic 3.1.0,
  the thi.ng/color and iq cosine formula, and the paletton.com color model (its own code); the author's designs as
  properties."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [fastmath.core :as m]
            [clojure2d.color :as c]))

(def reference (delay (-> "color/gradients_reference.edn" io/resource slurp edn/read-string)))

(defn- max-diff ^double [expected actual]
  (reduce max (map #(m/abs (m/- (double %1) (double %2))) expected actual)))

(defn- clip [col] (mapv #(m/constrain (double %) 0.0 255.0) (take 3 col)))

(defn- check-gradient
  "Compares `g` at `ts` with oracle rows (R, G, B), both clipped."
  [g ts rows tol msg]
  (doseq [[t e] (map vector ts rows)]
    (is (m/<= (max-diff (clip e) (clip (g t))) tol) (str msg " " t))))

(deftest cosine-presets-test
  (let [{:keys [ts cosine]} @reference]
    (doseq [[k rows] cosine]
      (check-gradient (c/gradient k) ts rows 1.0e-9 k))))

(deftest d3-gradients-test
  (let [{:keys [d3-ts d3]} @reference]
    (testing "d3-scale-chromatic API (rounded to integers)"
      (doseq [[k rows] (:api d3)]
        (check-gradient (c/gradient k) d3-ts rows 0.5 k)))
    (testing "the same cubehelix parameters without rounding (d3-color)"
      (doseq [[k rows] (:unrounded d3)]
        (check-gradient (c/gradient k) d3-ts rows 1.0e-9 k)))))

(deftest chroma-scale-test
  (let [{:keys [stops ts chroma]} @reference]
    (testing "rgb"
      (doseq [[st rows] (map vector stops (:rgb chroma))]
        (check-gradient (c/gradient st) ts rows 1.0e-9 st)))
    (testing "lab (chroma.js's own D65 white)"
      (doseq [[st rows] (map vector stops (:lab chroma))]
        (check-gradient (c/gradient st {:colorspace :LAB}) ts rows 2.0e-3 st)))))

(deftest paletton-shades-test
  (let [{:keys [presets hues shades]} (:paletton @reference)]
    (is (= (set presets) (set c/paletton-presets-list)) "the same 24 presets")
    (doseq [p presets
            [h rows] (map vector hues (shades p))]
      (is (m/<= (max-diff (flatten rows) (flatten (map #(take 3 %) (c/paletton :monochromatic h {:preset p})))) 0.5)
          (str p " " h)))))

(defn- palette-hues
  "Paletton hues of the five-shade groups of a palette (rounded)."
  [pal]
  (set (map (fn [grp] (m/round (c/hue-paletton (first grp)))) (partition 5 pal))))

(deftest paletton-schemes-test
  (let [{:keys [angles schemes]} (:paletton @reference)]
    (doseq [[a [_ s1 s2]] (map vector angles (:analog schemes))
            :when (<= 5 a 175)]
      (is (= #{0 (m/round s1) (m/round s2)} (palette-hues (c/paletton :triad 0 {:angle a}))) (str "analog " a)))
    (doseq [[a [_ s1 s2]] (map vector angles (:triad schemes))
            :when (<= 5 a 175)]
      (is (= #{0 (m/round s1) (m/round s2)} (palette-hues (c/paletton :triad 0 {:angle a :adj false}))) (str "triad " a)))
    (doseq [[a [cm s1 s2]] (map vector angles (:tetrad schemes))
            :when (<= 5 a 175)]
      (is (= #{0 (m/round cm) (m/round s1) (m/round s2)} (palette-hues (c/paletton :tetrad 0 {:angle a}))) (str "tetrad " a)))))

(deftest docstrings-test
  (doseq [s '[gradient palette merge-gradients iq-gradient gradient-cubehelix find-gradient find-palette random-color
              random-palette random-gradient paletton hue-paletton paletton-presets-list]]
    (is (some? (:doc (meta (ns-resolve 'clojure2d.color s)))) (str s))))
