(ns clojure2d.color.munsell-test
  "Munsell conversions checked against independent oracles generated in `utils/munsell/`:

  - colour-science 0.4.7 (`colour_reference.edn`) - primary oracle, same algorithm,
  - R munsellinterpol 3.6.0 (`munsellinterpol_reference.edn`) - independent value scales,
  - Centore's Octave toolbox, Sept 2017 (`centore_reference.edn`) - original algorithm."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [fastmath.core :as m]
            [clojure2d.color.munsell :as mu]
            [clojure2d.color.whitepoints :as wp])
  (:import [fastmath.vector Vec4]))

(defn- reference [name] (-> (str "munsell/" name) io/resource slurp edn/read-string))

(def colour (delay (reference "colour_reference.edn")))
(def munsellinterpol (delay (reference "munsellinterpol_reference.edn")))
(def centore (delay (reference "centore_reference.edn")))

(defn- close?
  [^double tol a b]
  (let [a (double a) b (double b)]
    (or (and (m/nan? a) (m/nan? b))
        (m/<= (m/abs (m/- a b)) tol))))

(defn- vec-close?
  [tol as bs]
  (and (= (count as) (count bs)) (every? true? (map (partial close? tol) as bs))))

(defn- throws-with?
  "Calls `f`, expects `ex-info` whose data contains `k`."
  [k f]
  (try (f) false
       (catch clojure.lang.ExceptionInfo e (contains? (ex-data e) k))))

(defn- Yxy->xyY [^Vec4 c] [(.y c) (.z c) (.x c)])

;; 4.1, 4.2

(deftest munsell-spec-test
  (testing "colour parse cases (0P expected as 10PB)"
    (doseq [[notation & expected] (:parse @colour)]
      (is (vec-close? 0.0 expected (vec (mu/munsell-spec notation))) notation)))
  (testing "specification vectors"
    (is (vec-close? 0.0 [10.0 5.0 4.0 10.0] (vec (mu/munsell-spec [0 5 4 9]))))
    (is (vec-close? 0.0 [10.0 5.0 4.0 8.0] (vec (mu/munsell-spec [0 5 4 7]))))
    (is (vec-close? 0.0 [##NaN 5.0 ##NaN ##NaN] (vec (mu/munsell-spec [2.5 5 0 7]))))
    (is (vec-close? 0.0 [##NaN 3.0 ##NaN ##NaN] (vec (mu/munsell-spec [##NaN 3 ##NaN ##NaN]))))
    (is (vec-close? 0.0 [2.5 4.0 6.0 1.0] (vec (mu/munsell-spec (Vec4. 2.5 4.0 6.0 1.0)))))))

(deftest munsell-spec-validation-test
  (is (throws-with? :notation #(mu/munsell-spec "x5R 5/2")))
  (is (throws-with? :notation #(mu/munsell-spec "5Q 5/2")))
  (is (throws-with? :notation #(mu/munsell-spec "")))
  (is (throws-with? :type #(mu/munsell-spec 5)))
  (is (throws-with? :notation #(mu/munsell-spec "5R -0.1/2")))
  (is (throws-with? :value #(mu/munsell-spec "5R 10.1/2")))
  (is (throws-with? :value #(mu/munsell-spec "N10.1")))
  (is (throws-with? :value #(mu/munsell-spec [5 -0.1 2 7])))
  (is (throws-with? :chroma #(mu/munsell-spec "5R 5/-2")))
  (is (throws-with? :hue #(mu/munsell-spec [10.5 5 2 7])))
  (is (throws-with? :code #(mu/munsell-spec [5 5 2 11])))
  (is (throws-with? :code #(mu/munsell-spec [5 5 2 2.5])))
  (is (some? (mu/munsell-spec "5R 0/2")))
  (is (some? (mu/munsell-spec "5R 10/2")))
  (is (some? (mu/munsell-spec "5R 5/0")))
  (is (some? (mu/munsell-spec "5R 5/+2"))))

;; 4.4

(def ^:private scales [:priest :munsell :moon :saunderson :ladd :mccamy :astm :newhall :osa])

(deftest munsell-value-colour-test
  ;; closed forms evaluate the same expressions as colour: 1e-12; ASTM D1535 is a lookup table in colour (step 0.001): 1e-6
  (doseq [[scale rows] (:munsell-value @colour)
          :let [tol (if (= scale :astm) 1e-6 1e-12)]
          [Y V] rows]
    (is (close? tol V (mu/munsell-value Y scale)) [scale Y])))

(deftest luminance-colour-test
  ;; same polynomials as colour: 1e-12
  (doseq [[scale rows] (:luminance @colour)
          [V Y] rows]
    (is (close? 1e-12 Y (mu/luminance V scale)) [scale V])))

(deftest value-scales-munsellinterpol-test
  ;; munsellinterpol: :newhall is MGO; forward polynomials: 1e-9
  (doseq [[scale rows] (:luminance @munsellinterpol)
          [V Y] rows]
    (is (close? 1e-9 Y (mu/luminance V scale)) [:luminance scale V]))
  ;; munsellinterpol inverts ASTM/MGO/OSA approximately (Y residual up to 2e-8, ours below 1e-13): 1e-8
  (doseq [[scale rows] (:munsell-value @munsellinterpol)
          [Y V] rows]
    (is (close? 1e-8 V (mu/munsell-value Y scale)) [:munsell-value scale Y])))

(deftest value-scales-round-trip-test
  ;; the only check of the McCamy, Moon, Saunderson and Ladd inverses (no external oracle): 1e-10
  (doseq [scale scales
          V (map #(* 0.1 %) (range 101))]
    (is (close? 1e-10 V (mu/munsell-value (mu/luminance V scale) scale)) [scale V])))

(deftest value-scales-domain-test
  (doseq [scale scales
          :let [lo (mu/luminance 0.0 scale)
                hi (mu/luminance 10.0 scale)]]
    (is (some? (mu/munsell-value lo scale)) [scale :lo])
    (is (some? (mu/munsell-value hi scale)) [scale :hi])
    (is (throws-with? :value #(mu/munsell-value (- lo 1e-9) scale)) [scale :below])
    (is (throws-with? :value #(mu/munsell-value (+ hi 1e-9) scale)) [scale :above])
    (is (throws-with? :value #(mu/luminance -1e-9 scale)) [scale :V-below])
    (is (throws-with? :value #(mu/luminance (+ 10.0 1e-9) scale)) [scale :V-above]))
  (is (throws-with? :value #(mu/munsell-value 0.0 :saunderson)))
  (is (throws-with? :value #(mu/munsell-value 0.0 :ladd)))
  (is (close? 1e-3 102.568 (mu/luminance 10.0 :newhall)))
  (is (throws-with? :scale #(mu/luminance 5.0 :unknown)))
  (is (throws-with? :scale #(mu/munsell-value 50.0 :unknown))))

;; 4.5

(deftest maximum-chroma-test
  ;; same lookup and arithmetic as colour: 1e-12
  (doseq [[h v code expected] (:max-chroma @colour)]
    (is (close? 1e-12 expected (#'mu/maximum-chroma h v code)) [h v code]))
  (is (zero? (#'mu/maximum-chroma 5.0 0.5 7.0)))
  (is (zero? (#'mu/maximum-chroma 5.0 0.999 7.0)))
  (is (zero? (#'mu/maximum-chroma 5.0 9.995 7.0))))

;; 4.6, 4.7

(defn- check-xyY
  [tol rows]
  (doseq [[h v c code & expected] rows]
    (is (vec-close? tol expected (Yxy->xyY (mu/munsell-to-Yxy [h v c code]))) [h v c code])))

(deftest munsell-to-Yxy-colour-test
  ;; same algorithm and data as colour, chroma clamped before conversion: 1e-10 (x, y and Y in 0-100)
  (testing "renotation grid" (check-xyY 1e-10 (:xyY-grid @colour)))
  (testing "random, chroma clamped" (check-xyY 1e-10 (:xyY-random @colour)))
  (testing "edges: value 1, 9-10, chroma below 2, hues near the grid, values below 1 as greys"
    (check-xyY 1e-10 (:xyY-edges @colour)))
  (testing "notation strings, 0P as 10PB"
    (doseq [[notation & expected] (:xyY-strings @colour)]
      (is (vec-close? 1e-10 expected (Yxy->xyY (mu/munsell-to-Yxy notation))) notation))))

(deftest munsell-to-Yxy-centore-test
  ;; Centore 2017 snaps hues within 1e-9 (colour 1e-3) and uses a spectral Illuminant C below chroma 2: 3e-5 in xy; Y: 1e-9
  ;; Above value 9 the toolbox accepts chromas beyond colour's maximum (clamped here), only unclamped specs are compared.
  (let [rows (filter (fn [[h v c code]] (<= c (#'mu/maximum-chroma h v code))) (:xyY @centore))]
    (is (< 450 (count rows)))
    (doseq [[h v c code x y Y] rows
            :let [[rx ry rY] (Yxy->xyY (mu/munsell-to-Yxy [h v c code]))]]
      (is (and (close? 3e-5 x rx) (close? 3e-5 y ry) (close? 1e-9 Y rY)) [h v c code]))))

(deftest munsell-to-Yxy-zero-hue-test
  ;; munsellinterpol agrees exactly with the renotation data at standard hues, integer values and even chromas (17 significant digits): 1e-9
  (let [{:keys [zero-hue-names zero-hue-xyY]} @munsellinterpol]
    (doseq [[notation expected] (map vector zero-hue-names zero-hue-xyY)]
      (is (vec-close? 1e-9 expected (Yxy->xyY (mu/munsell-to-Yxy notation))) notation)))
  (is (= (mu/munsell-to-Yxy "0P 5/4") (mu/munsell-to-Yxy "10PB 5/4"))))

(deftest munsell-to-Yxy-options-test
  (testing "Bradford C -> D65 (colour, clojure2d white points): 1e-10"
    ;; rows: spec, Y under C, x y under D65, adapted XYZ (D65, 0-100)
    (doseq [[h v c code _ x y _ Y] (:adapted-d65 @colour)]
      (is (vec-close? 1e-10 [x y Y] (Yxy->xyY (mu/munsell-to-Yxy [h v c code] {:whitepoint wp/CIE-2-D65})))
          [h v c code])))
  (testing "CAT02 C -> D65 (colour): 1e-10"
    (doseq [[h v c code _ x y _ Y] (:adapted-d65-cat02 @colour)]
      (is (vec-close? 1e-10 [x y Y] (Yxy->xyY (mu/munsell-to-Yxy [h v c code] {:whitepoint wp/CIE-2-D65 :adaptation :cat02})))
          [h v c code])))
  (testing "scale replaces Y only"
    (let [astm (mu/munsell-to-Yxy "4.2YR 8.1/5.3")
          newhall (mu/munsell-to-Yxy "4.2YR 8.1/5.3" {:scale :newhall})]
      (is (= (.y ^Vec4 astm) (.y ^Vec4 newhall)))
      (is (= (.z ^Vec4 astm) (.z ^Vec4 newhall)))
      (is (close? 1e-12 (mu/luminance 8.1 :newhall) (.x ^Vec4 newhall)))))
  (testing "alpha"
    (is (== 255.0 (.w ^Vec4 (mu/munsell-to-Yxy "N5"))))
    (is (== 255.0 (.w ^Vec4 (mu/munsell-to-Yxy "5R 5/4" {:whitepoint wp/CIE-2-D65})))))
  (testing "black gets the white point chromaticity"
    (is (vec-close? 0.0 [0.0 (.z ^Vec4 wp/CIE-2-D65) (.w ^Vec4 wp/CIE-2-D65)]
                    (take 3 (mu/munsell-to-Yxy "N0" {:whitepoint wp/CIE-2-D65}))))))

;; 6.3

(deftest munsell-properties-test
  (testing "Y increases with value"
    (let [Ys (map #(.x ^Vec4 (mu/munsell-to-Yxy [5.0 (* 0.05 %) 4.0 7.0])) (range 201))]
      (is (apply < Ys))))
  (testing "xy tends to Illuminant C when chroma tends to 0"
    (doseq [spec [[5.0 5.0 1e-9 7.0] [3.3 2.2 1e-9 4.0] [8.8 8.8 1e-9 10.0]]
            :let [[x y] (Yxy->xyY (mu/munsell-to-Yxy spec))]]
      (is (and (close? 1e-9 (.z ^Vec4 wp/CIE-2-C) x) (close? 1e-9 (.w ^Vec4 wp/CIE-2-C) y)) spec)))
  (testing "continuity across standard hues (1e-6 at +-1e-9)"
    (doseq [hue [2.5 5.0 7.5]
            code [1.0 4.0 7.0 10.0]
            [v c] [[3.0 6.0] [5.5 4.0] [7.2 3.3]]
            :let [[x0 y0] (Yxy->xyY (mu/munsell-to-Yxy [(- hue 1e-9) v c code]))
                  [x1 y1] (Yxy->xyY (mu/munsell-to-Yxy [(+ hue 1e-9) v c code]))]]
      (is (and (close? 1e-6 x0 x1) (close? 1e-6 y0 y1)) [hue code v c]))))
