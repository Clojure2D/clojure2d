(ns clojure2d.color.temperature-test
  "Blackbody colors and spectral colors of `clojure2d.color` (area D of the color audit), checked against
  `utils/color/gen_temperature.py`: Mitchell Charity's method with colour-science (Planck spectrum, CIE 1964 10
  degree, sRGB without adaptation, negative channel lifted by adding white, largest channel 1), the values of his
  table kept in the author's gist, and normalized monochromatic colors (negative channels clipped)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [fastmath.core :as m]
            [clojure2d.color :as c]
            [clojure2d.color.rgb :as rgb]
            [clojure2d.test-predicates :refer [throws-data?]])
  (:import [fastmath.vector Vec3]))

(def reference (delay (-> "color/temperature_reference.edn" io/resource slurp edn/read-string)))

(defn- max-channel-diff ^double [expected actual]
  (reduce max (map #(m/abs (m/- (double %1) (double %2))) expected (take 3 actual))))

(deftest temperature-oracle-test
  (let [{:keys [temperature-10k temperature-fractional temperature-nodes]} @reference]
    (testing "table nodes: exact"
      (doseq [[t & rgb] temperature-nodes]
        (is (m/<= (max-channel-diff rgb (c/temperature t)) 1.0e-9) (str t))))
    (testing "between nodes: linear interpolation of linear RGB within 0.1 (0-255)"
      (doseq [[t & rgb] (concat temperature-10k temperature-fractional)]
        (is (m/<= (max-channel-diff rgb (c/temperature t)) 0.1) (str t))))))

(deftest gist-test
  ;; Mitchell Charity's table (4 decimals) as kept in the author's gist, linear 0-1: 2.8e-4 with colour's sRGB
  ;; matrix (D65 0.3127, 0.3290), 4.5e-4 with clojure2d's (CIE-2-D65 0.312727, 0.329023)
  (doseq [[t & rgb] (:gist @reference)
          :let [actual (map #(rgb/srgb-to-linear (/ (double %) 255.0)) (take 3 (c/temperature t)))]
          [e a] (map vector rgb actual)
          :when e]
    (is (m/<= (m/abs (m/- e a)) 5.0e-4) (str t))))

(deftest temperature-properties-test
  (testing "red does not increase from 6600 K, blue does not decrease up to 6500 K"
    (let [ts (range 6600.0 40000.0 25.0)]
      (is (every? (fn [[a b]] (m/>= (+ a 1.0e-9) b)) (partition 2 1 (map #((c/temperature %) 0) ts)))))
    (let [ts (range 1000.0 6500.0 25.0)]
      (is (every? (fn [[a b]] (m/<= a (+ b 1.0e-9))) (partition 2 1 (map #((c/temperature %) 2) ts))))))
  (testing "outside 500-40000 K the end of the table"
    (is (= (c/temperature 500.0) (c/temperature 499.0) (c/temperature 1.0)))
    (is (= (c/temperature 40000.0) (c/temperature 50000.0) (c/temperature 1.0e9))))
  (testing "full brightness: the largest channel is 255"
    (doseq [t (range 500.0 40000.0 997.0)]
      (is (m/< (m/abs (m/- 255.0 (reduce max (take 3 (c/temperature t))))) 1.0e-9) (str t))))
  (testing "6500 K is not white (no chromatic adaptation)"
    (is (m/< ((c/temperature 6500.0) 1) 250.0))))

(deftest names-test
  (is (= 13 (count c/temperature-names)))
  (doseq [[n t] {:candle 1800.0 :sunrise 2500.0 :sunset 2500.0 :lightbulb 2900.0 :warm 2900.0 :morning 3500.0
                 :moonlight 4000.0 :white 4250.0 :sunlight 4800.0 :midday 5500.0 :cloudy-sky 6500.0 :cool 7250.0
                 :blue-sky 10000.0}]
    (is (= (c/temperature t) (c/temperature n)) (str n))))

(deftest invalid-temperature-test
  (doseq [t [:foo "warm" 0 0.0 -100 -100.0 ##NaN ##-Inf nil]]
    (is (throws-data? {:temperature t} #(c/temperature t)) (str t)))
  (testing "valid: positive numbers of any type and names"
    (is (= (c/temperature 3000) (c/temperature 3000.0) (c/temperature (float 3000.0))))
    (is (= (c/temperature 1800.0) (c/temperature :candle))))
  (is (throws-data? {:temperature :foo} #(c/adjust-temperature :red :foo))))

(deftest adjust-temperature-test
  (testing "lerp- toward the temperature color, default amount 0.35"
    (doseq [col [[30 60 200] [10 10 10] [250 250 250]] t [1800.0 :warm 10000.0]]
      (is (= (c/lerp- col (c/temperature t) 0.35) (c/adjust-temperature col t)) (str col " " t))
      (is (= (c/lerp- col (c/temperature t) 0.8) (c/adjust-temperature col t 0.8)) (str col " " t))))
  (testing "the CIELAB lightness of the color is kept"
    (doseq [col [[30 60 200] [10 10 10] [250 250 250]]]
      (is (m/< (m/abs (m/- ((c/to-LAB col) 0) ((c/to-LAB (c/adjust-temperature col 3000.0)) 0))) 1.0e-9)
          (str col)))))

(deftest wavelength-test
  (doseq [[o rows] (:wavelength @reference)
          [l & rgb] rows]
    (is (m/<= (max-channel-diff rgb (c/wavelength l o)) 1.0e-9) (str o " " l)))
  (testing "outside the observer range: black; default observer :CIE-2"
    (is (= [0.0 0.0 0.0 255.0] (vec (c/wavelength 350.0))))
    (is (= [0.0 0.0 0.0 255.0] (vec (c/wavelength 385.0 :CIE-2015-2))))
    (is (= (c/wavelength 550.0 :CIE-2) (c/wavelength 550.0))))
  (is (throws-data? {:observer :nope} #(c/wavelength 550.0 :nope))))

(deftest full-brightness-srgb-test
  (let [f #'c/full-brightness-srgb]
    (is (m/< (max-channel-diff [255.0 0.0 0.0] (f (Vec3. 2.0 -1.0 -0.5))) 1.0e-12) "negative channels clipped")
    (is (m/< (max-channel-diff [255.0 (* 255.0 (rgb/linear-to-srgb 0.5)) 0.0] (f (Vec3. 4.0 2.0 0.0))) 1.0e-12)
        "scaled to max 1")
    (is (= [0.0 0.0 0.0 255.0] (vec (f (Vec3. 0.0 0.0 0.0)))) "no light: black")
    (is (= [0.0 0.0 0.0 255.0] (vec (f (Vec3. -1.0 -2.0 0.0)))) "no positive channel: black")))

(deftest docstrings-test
  (doseq [s '[temperature temperature-names adjust-temperature wavelength]]
    (is (some? (:doc (meta (ns-resolve 'clojure2d.color s)))) (str s))))
