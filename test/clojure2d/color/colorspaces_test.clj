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
  {:HSV 0 :HSL 0 :HSI 0 :HWB 0 :Cubehelix 0 :HCL 0 :LCH 2 :LCHuv 2 :JCH 2 :Oklch 2 :Okhsl 0 :Okhsv 0})

(def ^:private hue-period
  ;; hue in degrees, Okhsl / Okhsv in 0-1
  {:Okhsl 1.0 :Okhsv 1.0})

(def ^:private oracle-tolerance
  ;; Okhsl / Okhsv: the same algorithm and constants as coloraide; DIN99o: coloraide's D65 white differs
  ;; from CIE-2-D65 (L 4e-3, a, b 7e-3)
  {:Oklab 1.0e-9 :Oklch 1.0e-9 :Okhsl 1.0e-9 :Okhsv 1.0e-9 :DIN99o 1.0e-2})

(defn- hue-distance ^double [^double period ^double a ^double b]
  (let [d (m/abs (m/- a b))
        d (m/rem d period)]
    (m/min d (m/- period d))))

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
          (let [period (hue-period k 360.0)]
            (is (m/<= (hue-distance period (e ch) (actual ch)) (m/* tol period)) (str k " " rgb " hue"))))
        (is (rel-close? tol (e ch) (actual ch)) (str k " " rgb " channel " ch))))))

(def ^:private round-trip-exempt
  ;; Gray is lossy
  #{:Gray})

(def ^:private round-trip-tolerance
  ;; 0-255 scale; JAB/JCH power functions lose ~1e-9; OSA is ill-conditioned near black (cube roots close
  ;; to 0, j and g in the thousands): up to 1.5e-6 there, 7e-10 elsewhere
  {:JAB 1.0e-8 :JCH 1.0e-8 :OSA 2.0e-6})

(deftest round-trip-test
  (let [samples (:samples @reference)]
    (doseq [k (remove round-trip-exempt (keys c/colorspaces))
            :let [[to from] (c/colorspaces k)
                  [to* from*] (c/colorspaces* k)
                  tol (round-trip-tolerance k 1.0e-9)]
            rgb samples
            :let [x (c/color rgb)]]
      (is (every? #(m/<= % tol) (map #(m/abs (m/- %1 %2)) x (from (to x)))) (str k " " rgb))
      (is (every? #(m/<= % tol) (map #(m/abs (m/- %1 %2)) x (from* (to* x)))) (str k "* " rgb)))))

(def ^:private range-grid
  ;; 16^3 grid and every cube edge at step 1
  (let [edges (for [t (range 256) [a b] [[0 0] [0 255] [255 0] [255 255]] p [[t a b] [a t b] [a b t]]] p)]
    (mapv c/color (concat (for [r (range 0 256 17) g (range 0 256 17) b (range 0 256 17)] [r g b]) edges))))

(def ^:private range-not-reached
  ;; channels whose sampled extrema are not reached on range-grid: hue (maximum just below 360 / 1), OSA j and g
  ;; (extrema near black, inside the cube), Okhsl and Okhsv saturation (extrema inside the cube)
  {:HSV #{0} :HSB #{0} :HSL #{0} :HSI #{0} :HWB #{0} :PalettonHSV #{0} :LCH #{2} :LCHuv #{2} :JCH #{2} :Oklch #{2}
   :Okhsl #{0 1} :Okhsv #{0 1} :Okhwb #{0} :OSA #{1 2}})

(deftest normalized-range-test
  ;; ranges are sampled extrema (utils/color/gen_normalized_ranges.clj, 64^3 grid + cube edges): on another grid
  ;; to-X* stays within [0,255] (overshoot <= 1e-6) and reaches 0 and 255 within 1e-6 (stale ranges fail)
  (doseq [k (keys c/colorspaces*)
          :let [to* (first (c/colorspaces* k))
                vs (mapv to* range-grid)]
          ch [0 1 2]
          :let [xs (map #(% ch) vs)
                mn (reduce min xs)
                mx (reduce max xs)]]
    (is (m/<= -1.0e-6 mn) (str k " " ch " min"))
    (is (m/<= mx (+ 255.0 1.0e-6)) (str k " " ch " max"))
    (when-not (contains? (range-not-reached k) ch)
      (is (m/<= mn 1.0e-6) (str k " " ch " min reached"))
      (is (m/<= (- 255.0 1.0e-6) mx) (str k " " ch " max reached")))))

(deftest edge-cases-test
  (testing "grays: hexagonal hue and saturation 0"
    (doseq [k [:HSV :HSL :HSI :HWB] g [0 1 128 255]
            :let [x ((first (c/colorspaces k)) (c/gray g))]]
      (is (zero? (x 0)) (str k g))
      (when (not= k :HWB) (is (zero? (x 1)) (str k g)))))
  (testing "Yxy and UVW of black: white-point chromaticity (Lindbloom), black round-trips"
    (let [yxy (c/to-Yxy :black)
          uvw (c/to-UVW :black)]
      (is (zero? (yxy 0)))
      (is (m/< 0.3 (yxy 1) 0.32))
      (is (= (c/to-color :black) (c/from-Yxy yxy)))
      (is (zero? (uvw 0)))
      (is (zero? (uvw 1)))
      (is (== -17.0 (uvw 2)))
      (is (= (c/to-color :black) (c/from-UVW uvw)))))
  (testing "UVW with W = 0: white chromaticity, gray with Y = (17/25)^3"
    (let [rgb (c/from-UVW [0 0 0])
          xyz (c/to-XYZ rgb)]
      (is (not-any? m/nan? rgb))
      (is (rel-close? 1.0e-9 (rgb 0) (rgb 1)))
      (is (rel-close? 1.0e-9 (rgb 1) (rgb 2)))
      (is (rel-close? 1.0e-9 (m/cb (/ 17.0 25.0)) (xyz 1)))))
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
          :when (not= k :pass)
          prefix ["to-" "from-"]
          suffix ["" "*"]
          :let [n (str prefix (name k) suffix)]]
    (is (some? (:doc (meta (ns-resolve 'clojure2d.color (symbol n))))) n))
  (testing "XYZ helpers"
    (doseq [s '[XYZ-to-LAB LAB-to-XYZ XYZ-to-LUV LUV-to-XYZ XYZ-to-HunterLAB HunterLAB-to-XYZ XYZ-to-LCH LCH-to-XYZ
                XYZ-to-LCHuv LCHuv-to-XYZ XYZ-to-Yxy Yxy-to-XYZ XYZ-to-UVW UVW-to-XYZ XYZ-to-UCS UCS-to-XYZ
                to-luma-color-hue from-luma-color-hue]]
      (is (some? (:doc (meta (ns-resolve 'clojure2d.color s)))) (str s)))))

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

;; G3: model fixes

(deftest g3-fixes-test
  (testing "Okhsl, Okhsv: black, white and grays have saturation 0, hue 0"
    (doseq [g [0 1 128 254 255] f [c/to-Okhsl c/to-Okhsv]
            :let [x (f (c/gray g))]]
      (is (zero? (x 0)) (str g))
      (is (zero? (x 1)) (str g))))
  (testing "Okhsl: lightness 0 is black, 1 is white, for any hue and saturation"
    (is (= (c/to-color :black) (c/from-Okhsl [0.3 0.7 0.0])))
    (is (= (c/to-color :white) (c/from-Okhsl [0.3 0.7 1.0]))))
  (testing "Okhsl, Okhsv saturation stays within 1 (up to rounding)"
    (doseq [rgb (:samples @reference) f [c/to-Okhsl c/to-Okhsv]]
      (is (m/<= ((f rgb) 1) (+ 1.0 1.0e-6)) (str rgb))))
  (testing "HCL: black round-trips, exact inverse"
    (is (= (c/to-color :black) (c/from-HCL (c/to-HCL :black)))))
  (testing "YCbCr, YUV, YIQ share BT.601 luma, equal to YDbDr's Y"
    (doseq [rgb (take 20 (:samples @reference))
            k [:YCbCr :YUV :YIQ]]
      (is (rel-close? 1.0e-12 ((c/to-YDbDr rgb) 0) (((first (c/colorspaces k)) rgb) 0)) (str k rgb))))
  (testing "YPbPr: Pb, Pr within 127.5"
    (is (rel-close? 1.0e-12 127.5 ((c/to-YPbPr :blue) 1)))
    (is (rel-close? 1.0e-12 127.5 ((c/to-YPbPr :red) 2)))))
