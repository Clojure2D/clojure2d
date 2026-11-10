(ns clojure2d.color.temperature-test
  "Blackbody colors of `clojure2d.color` (area D of the color audit), checked against `utils/color/gen_temperature.py`
  (Mitchell Charity's method with colour-science: Planck spectrum, CIE 1964 10 degree, sRGB without adaptation,
  negative channel lifted by adding white, largest channel 1)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [fastmath.core :as m]
            [clojure2d.color :as c]
            [clojure2d.test-predicates :refer [throws-data?]]))

(def reference (delay (-> "color/temperature_reference.edn" io/resource slurp edn/read-string)))

(defn- max-channel-diff ^double [expected actual]
  (reduce max (map #(m/abs (m/- (double %1) (double %2))) expected (take 3 actual))))

(deftest baseline-fit-test
  ;; the curve fit of temperature vs the exact model, 0-255: 1.83 at 1000 K, 3.0 at 1900-2000 K (blue 0 below
  ;; 1900 K), 1.2 at 2000-6500 K, 0.06 above 6600 K; below 1000 K outside the fit
  (let [{:keys [temperature-10k temperature-fractional]} @reference]
    (doseq [[t & rgb] (concat temperature-10k temperature-fractional)
            :when (m/>= t 1000.0)
            :let [tol (cond (m/< 1900.0 t 2000.0) 3.1 (m/< t 1900.0) 1.9 :else 1.3)]]
      (is (m/<= (max-channel-diff rgb (c/temperature t)) tol) (str t)))))

(deftest names-test
  (is (= 13 (count c/temperature-names)))
  (doseq [[n t] {:candle 1800.0 :sunrise 2500.0 :sunset 2500.0 :lightbulb 2900.0 :warm 2900.0 :morning 3500.0
                 :moonlight 4000.0 :white 4250.0 :sunlight 4800.0 :midday 5500.0 :cloudy-sky 6500.0 :cool 7250.0
                 :blue-sky 10000.0}]
    (is (= (c/temperature t) (c/temperature n)) (str n))))

(deftest adjust-temperature-test
  (testing "lerp+ toward the temperature color, default amount 0.35"
    (doseq [col [[30 60 200] [10 10 10] [250 250 250]] t [1800.0 :warm 10000.0]]
      (is (= (c/lerp+ col (c/temperature t) 0.35) (c/adjust-temperature col t)) (str col " " t))
      (is (= (c/lerp+ col (c/temperature t) 0.8) (c/adjust-temperature col t 0.8)) (str col " " t))))
  (testing "the CIELAB lightness of the result is that of the temperature color"
    (doseq [col [[30 60 200] [10 10 10] [250 250 250]]]
      (is (m/< (m/abs (m/- ((c/to-LAB (c/temperature 3000.0)) 0) ((c/to-LAB (c/adjust-temperature col 3000.0)) 0))) 1.0e-9)
          (str col)))))

(deftest invalid-temperature-test
  (doseq [t [:foo "warm" 0 0.0 -100 -100.0 ##NaN ##-Inf nil]]
    (is (throws-data? {:temperature t} #(c/temperature t)) (str t)))
  (testing "valid: positive numbers of any type and names"
    (is (= (c/temperature 3000) (c/temperature 3000.0) (c/temperature (float 3000.0))))
    (is (= (c/temperature 1800.0) (c/temperature :candle)))))

(deftest docstrings-test
  (doseq [s '[temperature temperature-names adjust-temperature wavelength]]
    (is (some? (:doc (meta (ns-resolve 'clojure2d.color s)))) (str s))))
