(ns clojure2d.color.mixing-test
  "Mixing, averaging, nearest color and reduction of `clojure2d.color` (area F of the color audit), checked against
  `utils/color/gen_mixing.py`: chroma.js 2.6.0 (mix, average), coloraide 8.14.1 (hue interpolation, alpha),
  ProfJski ArtColors' subtractive mix and a numpy argmin; the author's designs as properties."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [clojure2d.color :as c]))

(def reference (delay (-> "color/mixing_reference.edn" io/resource slurp edn/read-string)))

(defn- max-diff ^double [expected actual]
  (reduce max (map #(m/abs (m/- (double %1) (double %2))) expected actual)))

(defn- clip [col] (mapv #(m/constrain (double %) 0.0 255.0) (take 3 col)))

(defn- check-pairs
  "Compares `(f c1 c2 t)` with the oracle rows `[[rgb per t] per pair]`, both clipped to [0, 255] when `clip?`."
  [rows f tol clip?]
  (let [{:keys [colors pairs ts]} @reference]
    (doseq [[[i j] per-t] (map vector pairs rows)
            [t e] (map vector ts per-t)
            :let [a (f (colors i) (colors j) t)]]
      (is (m/<= (max-diff (if clip? (clip e) e) (if clip? (clip a) (take (count e) a))) tol)
          (str (colors i) " " (colors j) " " t)))))

(defn- check-sets
  [rows f tol clip?]
  (let [{:keys [colors sets]} @reference]
    (doseq [[[s w] e] (map vector (map vector sets (:weights @reference)) rows)
            :let [a (f (map colors s) w)]]
      (is (m/<= (max-diff (if clip? (clip e) e) (if clip? (clip a) (take (count e) a))) tol) (str s)))))

(deftest chroma-mix-test
  (let [{:keys [mix]} (:chroma @reference)]
    (testing "mix: square-root mixing (chroma.js default lrgb)"
      (check-pairs (:lrgb mix) c/mix 1.0e-9 false))
    (testing "lerp: chroma.js rgb"
      (check-pairs (:rgb mix) c/lerp 1.0e-9 false))
    (testing "lerp in LAB: chroma.js lab (its own D65 white), clipped"
      (check-pairs (:lab mix) #(c/lerp %1 %2 :LAB %3) 2.0e-3 true))))

(deftest chroma-average-test
  (let [{:keys [average weighted]} (:chroma @reference)]
    (testing "average: chroma.js rgb"
      (check-sets (:rgb average) (fn [cs _] (c/average cs)) 1.0e-9 false))
    (testing "average in LAB: chroma.js lab, clipped"
      (check-sets (:lab average) (fn [cs _] (c/average cs :LAB)) 2.0e-3 true))
    (testing "weighted-average: chroma.js rgb with weights"
      (check-sets weighted c/weighted-average 1.0e-9 false))
    (testing "the default is not chroma.js's default (lrgb)"
      (is (m/< 10.0 (max-diff (first (:lrgb average)) (c/average (map (:colors @reference) (first (:sets @reference))))))))))

(deftest straight-alpha-test
  (testing "lerp interpolates channels and alpha separately (coloraide srgb, premultiplied=False)"
    (let [{:keys [alpha-pairs alpha-mix]} (:coloraide @reference)]
      (doseq [[[a b] per-t] (map vector alpha-pairs (get-in alpha-mix [:RGB :straight]))
              [t e] (map vector (:ts @reference) per-t)]
        (is (m/<= (max-diff e (c/lerp a b t)) 1.0e-9) (str a " " b " " t))))))

(deftest nearest-color-test
  (let [{:keys [colors nearest]} @reference
        pal (mapv v/vec4 (:palette nearest))]
    (doseq [[col i] (map vector colors (:index nearest))]
      (is (= (pal i) (c/nearest-color pal (conj col 255.0))) (str col))
      (is (= (pal i) ((c/nearest-color pal) (conj col 255.0))) (str col)))))

(deftest negate-test
  (doseq [col (take 20 (:colors @reference))]
    (is (m/<= (max-diff (take 3 ((c/invert 1.0) col)) (take 3 (c/negate col))) 1.0e-9) (str col)))
  (is (= [-45.0 255.0 0.0 100.0] (vec (c/negate [300 0 255 100]))) "not clamped, alpha kept")
  (is (= [55.0 255.0 0.0 155.0] (vec (c/negate [200 0 255 100] true))) "alpha negated"))

(deftest docstrings-test
  (doseq [s '[nearest-color average weighted-average lerp lerp- lerp+ mix mixbox mixsub negate reduce-colors resample]]
    (is (some? (:doc (meta (ns-resolve 'clojure2d.color s)))) (str s))))
