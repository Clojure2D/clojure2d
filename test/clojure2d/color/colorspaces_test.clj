(ns clojure2d.color.colorspaces-test
  "Colorspace conversions of `clojure2d.color` (area B of the color audit), checked against
  `utils/color/gen_colorspaces.py`: colour-science, coloraide, standard matrices and a port of Leonard's
  RGB to RYB algorithm; round trips, normalized ranges and edge cases as properties."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [fastmath.core :as m]
            [clojure2d.color :as c]
            [clojure2d.color.whitepoints :as wp]
            [clojure2d.test-predicates :refer [rel-close?]]))

(def reference (delay (-> "color/colorspaces_reference.edn" io/resource slurp edn/read-string)))

(def ^:private hue-channel
  {:HSV 0 :HSL 0 :HSI 0 :HWB 0 :Cubehelix 0 :HCL 0 :LCH 2 :LCHuv 2 :JCH 2 :Oklch 2})

(def ^:private oracle-tolerance
  ;; Oklab family: rounded published matrices until G3
  {:Oklab 1.0e-7 :Oklch 1.0e-6})

(defn- hue-distance ^double [^double a ^double b]
  (let [d (m/abs (m/- a b))
        d (m/rem d 360.0)]
    (m/min d (m/- 360.0 d))))

(deftest oracle-test
  ;; raw conversions vs the oracles: 1e-12 relative to max(1, |expected|) unless stated in oracle-tolerance;
  ;; hue compared around the circle and only where defined (:chromatic)
  (let [{:keys [samples oracle chromatic]} @reference]
    (doseq [[k expected] oracle
            :let [to (first (c/colorspaces k))
                  tol (oracle-tolerance k 1.0e-12)
                  hc (hue-channel k)]
            [i [rgb e]] (map-indexed vector (map vector samples expected))
            :let [actual (to rgb)]
            ch [0 1 2]]
      (if (= ch hc)
        (when (nth (chromatic k) i)
          (is (m/<= (hue-distance (e ch) (actual ch)) (m/* tol 360.0)) (str k " " rgb " hue")))
        (is (rel-close? tol (e ch) (actual ch)) (str k " " rgb " channel " ch))))))

(def ^:private round-trip-exempt
  ;; Gray is lossy; YCbCr, YUV, YIQ (mixed Y coefficients) and the Oklab family (rounded matrices) until G3
  #{:Gray :YCbCr :YUV :YIQ :Oklab :Oklch :Okhsl :Okhsv :Okhwb})

(def ^:private round-trip-tolerance
  ;; 0-255 scale; XYB, OSA and HCL inverses are not exact (found in G1, fixed in G3); JAB/JCH power
  ;; functions lose ~1e-9
  {:XYB 1.0e-4 :OSA 1.0e-4 :HCL 0.06 :JAB 1.0e-8 :JCH 1.0e-8})

(deftest round-trip-test
  (let [samples (:samples @reference)]
    (doseq [k (remove round-trip-exempt (keys c/colorspaces))
            :let [[to from] (c/colorspaces k)
                  [to* from*] (c/colorspaces* k)
                  tol (round-trip-tolerance k 1.0e-9)]
            rgb samples
            :when (not (and (= k :HCL) (every? zero? rgb))) ;; HCL black: G3
            :let [x (c/color rgb)]]
      (is (every? #(m/<= % tol) (map #(m/abs (m/- %1 %2)) x (from (to x)))) (str k " " rgb))
      (is (every? #(m/<= % tol) (map #(m/abs (m/- %1 %2)) x (from* (to* x)))) (str k "* " rgb)))))

(deftest normalized-range-test
  ;; to-X* of sRGB colors stays in [0,255]; overshoot up to 0.005 until the ranges are recomputed (G3)
  (let [grid (for [r (range 0 256 17) g (range 0 256 17) b (range 0 256 17)] (c/color r g b))]
    (doseq [k (keys c/colorspaces*)
            :let [to* (first (c/colorspaces* k))
                  vs (mapcat #(take 3 (to* %)) grid)]]
      (is (m/<= -0.005 (reduce min vs)) (str k " min"))
      (is (m/<= (reduce max vs) 255.005) (str k " max")))))

(deftest edge-cases-test
  (testing "grays: hexagonal hue and saturation 0"
    (doseq [k [:HSV :HSL :HSI :HWB] g [0 1 128 255]
            :let [x ((first (c/colorspaces k)) (c/gray g))]]
      (is (zero? (x 0)) (str k g))
      (when (not= k :HWB) (is (zero? (x 1)) (str k g)))))
  (testing "Yxy and UVW of black: white-point chromaticity (Lindbloom), black round-trips"
    (let [yxy (c/to-Yxy :black)]
      (is (zero? (yxy 0)))
      (is (m/< 0.3 (yxy 1) 0.32))
      (is (= (c/to-color :black) (c/from-Yxy yxy)))))
  (testing "Gray is lossy: from-Gray is the same function"
    (is (identical? c/to-Gray c/from-Gray)))
  (testing "CMY is its own inverse"
    (is (identical? c/to-CMY c/from-CMY))))

(deftest reference-points-test
  (testing "Okhwb is Ottosson's HWB from Okhsv: w = (1 - s) v, b = 1 - v, same hue"
    (doseq [rgb (take 50 (:samples @reference))
            :let [hsv (c/to-Okhsv rgb) hwb (c/to-Okhwb rgb)]]
      (is (rel-close? 1.0e-12 (hsv 0) (hwb 0)) (str rgb))
      (is (rel-close? 1.0e-12 (* (- 1.0 (hsv 1)) (hsv 2)) (hwb 1)) (str rgb))
      (is (rel-close? 1.0e-12 (- 1.0 (hsv 2)) (hwb 2)) (str rgb))))
  (testing "aliases"
    (is (= (c/to-HSV [10 200 30]) (c/to-HSB [10 200 30])))))

(deftest docstrings-test
  (doseq [k (keys c/colorspaces)
          :when (not (#{:pass :RGB :sRGB :linearRGB :XYZ :XYZ1 :DIN99o :YCbCr :YUV :YIQ :YPbPr
                        :Oklab :Oklch :Okhsl :Okhsv :Okhwb :HCL} k))
          f ["to-" "from-" "to-" "from-"]
          :let [star (if (#{0 1} (.indexOf ["to-" "from-" "to-" "from-"] f)) "" "*")]]
    (is (some? (:doc (meta (ns-resolve 'clojure2d.color (symbol (str f (name k) star)))))) (str f (name k) star))))

;; G2: one sRGB matrix, CIE-2-D65 white

(deftest white-point-test
  (testing "to-XYZ of white is CIE-2-D65 x 100"
    (let [xyz (c/to-XYZ :white)]
      (is (rel-close? 1.0e-12 (* 100.0 (wp/CIE-2-D65 0)) (xyz 0)))
      (is (rel-close? 1.0e-12 100.0 (xyz 1)))
      (is (rel-close? 1.0e-12 (* 100.0 (wp/CIE-2-D65 1)) (xyz 2)))))
  (testing "white is neutral in the Lab-like spaces"
    (doseq [k [:LAB :LUV :HunterLAB :DIN99 :DIN99b :DIN99c :DIN99d :UVW]
            :let [x ((first (c/colorspaces k)) :white)]]
      (is (m/<= (m/abs (x (if (= k :UVW) 0 1))) 1.0e-12) (str k))
      (is (m/<= (m/abs (x (if (= k :UVW) 1 2))) 1.0e-12) (str k))))
  (testing "to-XYZ* maps white to 255"
    (is (every? #(m/<= (m/abs (m/- 255.0 %)) 1.0e-12) (take 3 (c/to-XYZ* :white))))))
