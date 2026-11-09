(ns clojure2d.color.whitepoints-test
  "White point helpers, adaptation and spectral data checked against oracles generated in `utils/whitepoints/`:

  - colour-science 0.4.7 (`colour_reference.edn`) - primary oracle."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [fastmath.core :as m]
            [clojure2d.color.whitepoints :as wp])
  (:import [fastmath.vector Vec2 Vec3 Vec4]))

(defn- reference [name] (-> (str "whitepoints/" name) io/resource slurp edn/read-string))

(def colour (delay (reference "colour_reference.edn")))

(defn- rel-close?
  "Absolute difference within `tol`, scaled by `max(1, |b|)`."
  [^double tol a b]
  (let [a (double a) b (double b)]
    (m/<= (m/abs (m/- a b)) (m/* tol (m/max 1.0 (m/abs b))))))

(defn- vec-close?
  [tol as bs]
  (and (= (count as) (count bs)) (every? true? (map (partial rel-close? tol) as bs))))

;; G1: coordinate helpers vs colour xy_to_XYZ / XYZ_to_xy.
;; Same formulas; colour multiplies by 1/y and sums X+Y+Z in another order, so results differ
;; by a few ulps: tolerance 1e-15 relative to max(1, |expected|) (X reaches 14 at y = 0.05).

(def ^:private ^:const tol 1.0e-15)

(deftest xy->XZ-test
  (doseq [[x y X _ Z] (:xy->XYZ @colour)]
    (is (vec-close? tol [X Z] (vec (wp/xy->XZ x y))) (str [x y]))
    (is (vec-close? tol [X Z] (vec (wp/xy->XZ (Vec2. x y)))) (str [x y]))))

(deftest xy->coordinates-test
  (doseq [[x y X _ Z] (:xy->XYZ @colour)]
    (is (vec-close? tol [X Z x y] (vec (wp/xy->coordinates x y))) (str [x y]))
    (is (vec-close? tol [X Z x y] (vec (wp/xy->coordinates (Vec2. x y)))) (str [x y]))))

(deftest XZ->xy-test
  (doseq [[X Z x y] (:XYZ->xy @colour)]
    (is (vec-close? tol [x y] (vec (wp/XZ->xy X Z))) (str [X Z]))
    (is (vec-close? tol [x y] (vec (wp/XZ->xy (Vec2. X Z)))) (str [X Z]))))

(deftest XZ->coordinates-test
  (doseq [[X Z x y] (:XYZ->xy @colour)]
    (is (vec-close? tol [X Z x y] (vec (wp/XZ->coordinates X Z))) (str [X Z]))
    (is (vec-close? tol [X Z x y] (vec (wp/XZ->coordinates (Vec2. X Z)))) (str [X Z]))))

(deftest coordinates-test
  (testing "four entries are stored as given"
    (is (= (Vec4. 1.0 2.0 3.0 4.0) (wp/coordinates 1.0 2.0 3.0 4.0))))
  (testing "xy arity vs colour"
    (doseq [[x y X _ Z] (:xy->XYZ @colour)]
      (is (vec-close? tol [X Z x y] (vec (wp/coordinates x y))) (str [x y])))))

(deftest tristimulus-test
  (testing "default is CIE-2-D65"
    (is (= (wp/tristimulus wp/CIE-2-D65) (wp/tristimulus))))
  (testing "Vec4 arity reads X and Z only"
    (is (= (Vec3. 0.5 1.0 0.25) (wp/tristimulus (Vec4. 0.5 0.25 ##NaN ##NaN))))
    (is (= (Vec3. (.x ^Vec4 wp/CIE-2-C) 1.0 (.y ^Vec4 wp/CIE-2-C)) (wp/tristimulus wp/CIE-2-C))))
  (testing "xy arity vs colour"
    (doseq [[x y & XYZ] (:xy->XYZ @colour)]
      (is (vec-close? tol XYZ (vec (wp/tristimulus x y))) (str [x y])))))

(deftest coordinate-round-trip-test
  ;; xy -> XZ -> xy, two roundings each way
  (doseq [[x y] (:xy->XYZ @colour)]
    (is (vec-close? 1.0e-15 [x y] (vec (wp/XZ->xy (wp/xy->XZ x y)))) (str [x y]))))

(deftest namespace-docstring-test
  (let [doc (:doc (meta (find-ns 'clojure2d.color.whitepoints)))]
    (is (seq doc))
    (is (re-find #"\[X Z x y\]" doc))
    (is (re-find #"<observer>-<illuminant>" doc))
    (is (re-find #"\*" doc))))
