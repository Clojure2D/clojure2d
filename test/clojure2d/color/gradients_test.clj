(ns clojure2d.color.gradients-test
  "Gradients and palettes of `clojure2d.color` (area C of the color audit), checked against
  `utils/color/gen_gradients.py`: chroma.js 2.6.0 scales, coloraide 8.14.1 interpolation, d3-scale-chromatic 3.1.0,
  the thi.ng/color and iq cosine formula, and the paletton.com color model (its own code); the author's designs as
  properties."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [fastmath.core :as m]
            [clojure2d.color :as c]
            [clojure2d.test-predicates :refer [throws-data?]]))

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

;; G2: contracts

(deftest one-color-test
  (let [g (c/gradient [[10 20 30 40]])]
    (doseq [t [0.0 0.3 1.0 -1.0 2.0]]
      (is (= [10.0 20.0 30.0 40.0] (vec (g t))) (str t))))
  (is (= (repeat 4 (c/to-color :red)) (c/palette [:red] 4)))
  (is (= (repeat 3 (c/to-color :red)) (c/resample [:red] 3)))
  (let [p (c/palette :cartography/blue.pal-1)]
    (is (= 1 (count p)) "a one-color palette from the files")
    (is (= (repeat 5 (first p)) (c/palette p 5)))
    (is (= (first p) ((c/gradient p) 0.7))))
  (is (throws-data? {:colors 0} #(c/gradient [])))
  (is (every? fn? (repeatedly 3000 c/random-gradient)) "random-gradient: no failures"))

(deftest unknown-names-test
  (is (throws-data? {:palette :nope} #(c/palette :nope)))
  (is (throws-data? {:palette :nope/nope} #(c/palette :nope/nope)))
  (is (throws-data? {:palette 999999999} #(c/palette 999999999)))
  (is (throws-data? {:gradient :nope} #(c/gradient :nope)))
  (is (throws-data? {:gradient :nope/nope} #(c/gradient :nope/nope))))

(deftest validation-test
  (testing "domain"
    (is (throws-data? {:domain [0 0.5 0.5]} #(c/gradient [:red :blue :green] {:domain [0 0.5 0.5]})))
    (is (throws-data? {:domain [0 0.8 0.2]} #(c/gradient [:red :blue :green] {:domain [0 0.8 0.2]})))
    (is (throws-data? {:domain [0 1]} #(c/gradient [:red :blue :green] {:domain [0 1]})))
    (is (= (c/to-color :blue) ((c/gradient [:red :blue :green] {:domain [0 0.2 1]}) 0.2))))
  (testing "two colors for :iq and easings"
    (is (throws-data? {:interpolation :iq :colors 3} #(c/gradient [:red :blue :green] {:interpolation :iq})))
    (is (throws-data? {:interpolation :cubic-in :colors 3} #(c/gradient [:red :blue :green] {:interpolation :cubic-in})))
    (is (fn? (c/gradient [:red :blue] {:interpolation :iq})))
    (is (fn? (c/gradient [:red :blue] {:interpolation :cubic-in}))))
  (testing "merge-gradients midpoint"
    (let [g1 (c/gradient [:red :blue]) g2 (c/gradient [:green :white])]
      (doseq [mp [0.0 1.0 -0.5 1.5]]
        (is (throws-data? {:midpoint mp} #(c/merge-gradients g1 g2 mp))))
      (is (= (c/to-color :white) ((c/merge-gradients g1 g2 0.7) 1.0)))))
  (testing "number of colors"
    (is (throws-data? {:number-of-colors 0} #(c/palette [:red :blue] 0)))
    (is (throws-data? {:number-of-colors 0} #(c/palette (c/gradient [:red :blue]) 0)))
    (is (throws-data? {:number-of-colors 0} #(c/resample [:red :blue] 0))))
  (testing "paletton"
    (is (throws-data? {:preset :nope} #(c/paletton :monochromatic 100 {:preset :nope})))
    (is (throws-data? {:preset :nope} #(c/paletton :triad 100 {:preset :nope})))
    (is (throws-data? {:preset :nope} #(c/paletton :tetrad 100 {:preset :nope})))
    (is (throws-data? {:type :pentad} #(c/paletton :pentad 100)))
    (is (= 5 (count (c/paletton :monochromatic 100 {:preset [[1 1] [0.5 1] [1 0.5] [0.2 0.2] [2 2]]})))))
  (testing "options and colorspace"
    (is (throws-data? {:options :x} #(c/gradient [:red :blue] :x)))
    (is (throws-data? {:colorspace :Foo} #(c/gradient [:red :blue] {:colorspace :Foo})))))

(deftest find-test
  (is (= (c/find-gradient #"cubehelix") (c/find-gradient "cubehelix")))
  (is (seq (c/find-gradient "cubehelix")))
  (is (= (c/find-palette "pals" #"kovesi") (c/find-palette "pals" "kovesi"))))

;; G3: values

(def ^:private coloraide-tolerance
  ;; compared clipped (out of gamut coloraide extends the sRGB curve, area B); LCH: coloraide lch-d65 white point
  {:HSL 1.0e-9 :HWB 1.0e-9 :LCH 0.2 :Oklch 1.0e-6})

(deftest hue-modes-test
  (let [{:keys [stops ts coloraide]} @reference]
    (testing "linear, all hue modes, per-segment powerless hues (coloraide)"
      (doseq [[cs modes] coloraide
              [mode rows] modes
              [st per-t] (map vector stops rows)]
        (check-gradient (c/gradient st {:colorspace cs :hue mode}) ts per-t (coloraide-tolerance cs)
                        (str cs " " mode " " st))))
    (testing "chroma.js hsl scale (shorter hue): chromatic stop lists, chroma.js rounds to integers"
      (doseq [[st rows] (map vector stops (get-in @reference [:chroma :hsl]))
              :when rows]
        (check-gradient (c/gradient st {:colorspace :HSL}) ts rows 0.5 st)))
    (testing "default :shorter; :raw is the hue as a number"
      (is (= (c/to-color :magenta) ((c/gradient [:red :blue] {:colorspace :HSL}) 0.5)))
      (is (= (c/to-color :lime) ((c/gradient [:red :blue] {:colorspace :HSL :hue :raw}) 0.5))))
    (testing "other interpolators: continuous hues, wrapped into the range"
      (let [g (c/gradient [:red :blue :lime :yellow] {:colorspace :HSL :interpolation :cubic})]
        (doseq [t (range 0.0 1.0 0.01)
                :let [h ((c/to-HSL (g t)) 0)]]
          (is (and (m/<= 0.0 h) (m/< h 360.0)) (str t))))
      (is (= (c/to-color :magenta) ((c/gradient [:red :blue] {:colorspace :HSL :interpolation :cubic}) 0.5))))
    (testing "unknown hue mode"
      (is (throws-data? {:hue :around} #(c/gradient [:red :blue] {:colorspace :HSL :hue :around}))))))

(deftest gradient-range-test
  (doseq [k [:cubic-in :quad-out :sin-in-out]]
    (let [g (c/gradient [:red :blue] {:interpolation k})]
      (is (= (g 1.0) (g 1.5)) (str k))
      (is (= (g 0.0) (g -0.5)) (str k)))))

(deftest outputs-test
  (is (every? #(instance? fastmath.vector.Vec4 %) (repeatedly 2000 c/random-color)))
  (is (= [(c/to-color :red) (c/to-color "#00ff00")] (c/palette [:red "#00ff00"])))
  (is (= c/paletton-presets-list (sort c/paletton-presets-list))))

(deftest docstrings-test
  (doseq [s '[gradient palette merge-gradients iq-gradient gradient-cubehelix find-gradient find-palette random-color
              random-palette random-gradient paletton hue-paletton paletton-presets-list]]
    (is (some? (:doc (meta (ns-resolve 'clojure2d.color s)))) (str s))))
