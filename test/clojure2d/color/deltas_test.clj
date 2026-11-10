(ns clojure2d.color.deltas-test
  "Color differences and contrast of `clojure2d.color` (area E of the color audit), checked against
  `utils/color/gen_deltas.py`: colour-science, coloraide, published formulas, d3-jnd and the CIEDE2000 test data
  of Sharma, Wu and Dalal (2005); identity, symmetry and sign as properties."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [fastmath.core :as m]
            [clojure2d.color :as c]
            [clojure2d.test-predicates :refer [rel-close? throws-data?]]))

(def reference (delay (-> "color/deltas_reference.edn" io/resource slurp edn/read-string)))

(def ^:private lab-input {:colorspace :pass})

(def ^:private oracle-fns
  {:E76 c/delta-E*
   :E94 c/delta-E*-94
   :E94-textiles #(c/delta-E*-94 %1 %2 {:textiles? true})
   :E2000 c/delta-E*-2000
   :CMC11 c/delta-E*-CMC
   :CMC21 #(c/delta-E*-CMC %1 %2 {:l 2.0})
   :HyAB c/delta-E-HyAB
   :HyCH c/delta-E-HyCH
   :HyCH-textiles #(c/delta-E-HyCH %1 %2 {:textiles? true})
   :DIN99 #(c/delta-E*-euclidean %1 %2 {:colorspace :DIN99})
   :C* c/delta-C*
   :H* c/delta-H*
   :OK c/delta-E*-euclidean
   :Ez c/delta-E-z
   :redmean c/delta-C-RGB})

(def ^:private oracle-tolerance
  ;; H* = sqrt(da^2 + db^2 - dC^2) cancels when the hues are (nearly) equal: absolute error ~ sqrt(eps) x chroma,
  ;; e.g. gray vs red 1.3e-6 (oracle) vs 0 (clojure2d)
  {:H* 1.0e-5})

(deftest oracle-test
  ;; 1e-12 relative to max(1, |expected|) unless stated in oracle-tolerance
  (let [{:keys [pairs oracle]} @reference]
    (doseq [[k f] oracle-fns
            :let [tol (oracle-tolerance k 1.0e-12)]
            [[a b] e] (map vector pairs (oracle k))]
      (is (rel-close? tol e (f a b)) (str k " " a " " b)))))

(deftest contrast-ratio-test
  ;; WCAG formula: sRGB threshold 0.04045, weights 0.2126 0.7152 0.0722
  (let [{:keys [pairs oracle]} @reference]
    (doseq [[[a b] e] (map vector pairs (:wcag oracle))]
      (is (rel-close? 1.0e-12 e (c/contrast-ratio a b)) (str a " " b))))
  (testing "relative-luma keeps the Y row of to-XYZ (area A)"
    (is (rel-close? 1.0e-12 (* 2.55 ((c/to-XYZ :red) 1)) (c/relative-luma :red)))))

(deftest sharma-test
  (let [{:keys [sharma sharma-colour]} @reference]
    (doseq [[[l1 l2 e] ec] (map vector sharma sharma-colour)
            :let [d (c/delta-E*-2000 l1 l2 lab-input)]]
      (is (m/<= (m/abs (m/- e d)) 5.0e-5) (str "published " l1 " " l2))
      (is (rel-close? 1.0e-12 ec d) (str "colour " l1 " " l2)))))

(deftest noticeable-difference-test
  ;; per channel just below and just above the d3-jnd interval p (A + B / s)
  (doseq [[[p s] interval] (:jnd @reference)
          ch [0 1 2]
          :let [opts {:p p :s s :colorspace :pass}
                base [50.0 0.0 0.0]
                shifted (fn [^double f] (update base ch + (* f (double (interval ch)))))]]
    (is (not (c/noticeably-different? base (shifted (- 1.0 1.0e-9)) opts)) (str [p s] " " ch " below"))
    (is (c/noticeably-different? base (shifted (+ 1.0 1.0e-9)) opts) (str [p s] " " ch " above"))
    (is (c/noticeably-different? (shifted (+ 1.0 1.0e-9)) base opts) (str [p s] " " ch " symmetric")))
  (testing "defaults: p 0.5, s 0.1, CIELAB"
    (is (c/noticeably-different? :pink :hotpink))
    (is (not (c/noticeably-different? "00aabb" "00baba")))
    (is (not (c/noticeably-different? :red :red)))))

(def ^:private deltas
  [c/delta-E* c/delta-C* c/delta-H* c/delta-E-HyAB c/delta-E-HyCH c/delta-E*-94 c/delta-E*-euclidean c/delta-C-RGB
   c/delta-D-HCL c/delta-E*-CMC c/delta-E-z c/delta-E*-2000])

(def ^:private symmetric-deltas
  [c/delta-E* c/delta-H* c/delta-E-HyAB c/delta-E-HyCH c/delta-E*-euclidean c/delta-C-RGB c/delta-D-HCL c/delta-E-z
   c/delta-E*-2000])

(deftest properties-test
  (let [pairs (take 60 (:pairs @reference))]
    (testing "identity: d(c, c) = 0"
      (doseq [f deltas [a _] pairs]
        (is (zero? (f a a)) (str f " " a))))
    (testing "non-negative (all but the signed delta-C*)"
      (doseq [f (remove #{c/delta-C*} deltas) [a b] pairs]
        (is (m/<= 0.0 (f a b)) (str f " " a " " b))))
    (testing "symmetric"
      (doseq [f symmetric-deltas [a b] pairs]
        (is (rel-close? 1.0e-12 (f a b) (f b a)) (str f " " a " " b))))
    (testing "delta-C* changes sign with the argument order"
      (doseq [[a b] pairs]
        (is (== (c/delta-C* a b) (- (c/delta-C* b a))) (str a " " b))))
    (testing "CIE94 and CMC: the first color is the reference"
      (is (m/> (m/abs (- (c/delta-E*-94 :red :orange) (c/delta-E*-94 :orange :red))) 1.0))
      (is (m/> (m/abs (- (c/delta-E*-CMC :red :orange) (c/delta-E*-CMC :orange :red))) 1.0e-3)))
    (testing "contrast-ratio: 1 for equal colors, 21 for black and white, symmetric"
      (is (== 1.0 (c/contrast-ratio :red :red)))
      (is (rel-close? 1.0e-12 21.0 (c/contrast-ratio :black :white)))
      (is (== (c/contrast-ratio :pink :purple) (c/contrast-ratio :purple :pink))))))

(deftest delta-D-HCL-regression-test
  ;; no oracle (primary source unavailable): values of the current implementation
  (doseq [[a b e] [[:red :orange 761.913827289016]
                   [:navy :teal 661.8698898659094]
                   [[10 200 30] [200 10 30] 2669.6901159561144]
                   [:white :black 195.54039394221707]
                   [[120 80 60] [125 82 58] 7.55718121022717]]]
    (is (rel-close? 1.0e-12 e (c/delta-D-HCL a b)) (str a " " b))))

(deftest docstrings-test
  (doseq [s '[delta-E* delta-C* delta-H* delta-E-HyAB delta-E-HyCH delta-E*-94 delta-E*-euclidean delta-C-RGB delta-D-HCL
              delta-E*-CMC delta-E-z delta-E*-2000 contrast-ratio noticeably-different?]]
    (is (some? (:doc (meta (ns-resolve 'clojure2d.color s)))) (str s))))

;; G2: options contract

(def ^:private option-fns
  [c/delta-E* c/delta-C* c/delta-H* c/delta-E-HyAB c/delta-E-HyCH c/delta-E*-94 c/delta-E*-euclidean c/delta-D-HCL
   c/delta-E*-CMC c/delta-E-z c/delta-E*-2000 c/noticeably-different?])

(deftest options-contract-test
  (testing "unknown colorspace"
    (doseq [f option-fns]
      (is (throws-data? {:colorspace :Foo} #(f :red :blue {:colorspace :Foo})) (str f))))
  (testing "options not a map (e.g. a positional colorspace)"
    (doseq [f option-fns]
      (is (throws-data? {:options :YUV} #(f :red :blue :YUV)) (str f))
      (is (throws-data? {:options 2.0} #(f :red :blue 2.0)) (str f))))
  (testing "nil and {} mean defaults"
    (doseq [f option-fns]
      (is (= (f :red :blue) (f :red :blue nil) (f :red :blue {})) (str f)))))

;; G4: contrast methods

(defn- same-or-close?
  "rel-close? for finite values, equality for infinities, both NaN for NaN."
  [^double tol ^double e ^double a]
  (cond (m/nan? e) (m/nan? a)
        (m/inf? e) (== e a)
        :else (rel-close? tol e a)))

(deftest contrast-methods-test
  (let [{:keys [pairs oracle]} @reference]
    (doseq [[method k] [[:lstar :lstar] [:weber :weber] [:michelson :michelson]]
            [[a b] e] (map vector pairs (oracle k))]
      (is (same-or-close? 1.0e-12 e (c/contrast-ratio a b {:method method})) (str method " " a " " b)))
    (testing "lstar vs coloraide: within 1e-2"
      ;; coloraide's sRGB matrix comes from its own D65 white: Y of blue 0.0721923 vs 0.0721750 (colour, clojure2d),
      ;; L* differs by up to ~5e-3
      (doseq [[[a b] e] (map vector pairs (:lstar-coloraide oracle))]
        (is (m/<= (m/abs (m/- e (c/contrast-ratio a b {:method :lstar}))) 1.0e-2) (str a " " b))))
    (testing "the 2-argument form is :wcag"
      (doseq [[a b] (take 30 pairs)]
        (is (== (c/contrast-ratio a b) (c/contrast-ratio a b {:method :wcag}) (c/contrast-ratio a b nil))))))
  (testing "black: Weber infinite on black, NaN for two blacks; Michelson NaN for two blacks"
    (is (m/pos-inf? (c/contrast-ratio :white :black {:method :weber})))
    (is (m/nan? (c/contrast-ratio :black :black {:method :weber})))
    (is (m/nan? (c/contrast-ratio :black :black {:method :michelson})))
    (is (== -1.0 (c/contrast-ratio :black :white {:method :weber})))
    (is (== 1.0 (c/contrast-ratio :black :white {:method :michelson}))))
  (testing "errors"
    (is (throws-data? {:method :apca} #(c/contrast-ratio :red :blue {:method :apca})))
    (is (throws-data? {:options :lstar} #(c/contrast-ratio :red :blue :lstar)))))

;; point values moved from clojure2d.color-test

(deftest chroma-contrast
  (is (= 1.72 (m/approx (c/contrast-ratio :pink :hotpink))))
  (is (= 6.12 (m/approx (c/contrast-ratio :pink :purple)))))

(deftest chroma-distance
  (is (= 96.95 (m/approx (c/delta-E* "fff" "ff0"))))
  (is (= 122.16 (m/approx (c/delta-E* "fff" "f0f"))))
  (is (= 255.0 (m/approx (c/delta-E*-euclidean "fff" "ff0" {:colorspace :RGB}))))
  (is (= 255.0 (m/approx (c/delta-E*-euclidean "fff" "f0f" {:colorspace :RGB}))))
  (is (= 1.64 (m/approx (c/delta-E*-CMC 0xededee 0xedeeed))))
  (is (= 3.16 (m/approx (c/delta-E*-CMC 0xececee 0xeceeec))))
  (is (= 7.36 (m/approx (c/delta-E*-CMC 0xe9e9ee 0xe9eee9))))
  (is (= 14.84 (m/approx (c/delta-E*-CMC 0xe4e4ee 0xe4eee4))))
  (is (= 21.32 (m/approx (c/delta-E*-CMC 0xe0e0ee 0xe0eee0)))))
