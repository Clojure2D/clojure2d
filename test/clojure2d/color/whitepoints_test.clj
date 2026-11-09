(ns clojure2d.color.whitepoints-test
  "White point helpers, adaptation and spectral data checked against oracles generated in `utils/whitepoints/`:

  - colour-science 0.4.7 (`colour_reference.edn`) - primary oracle,
  - R spacesXYZ 1.6.0 (`spacesxyz_reference.edn`) - second oracle for adaptation matrices."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [fastmath.core :as m]
            [fastmath.matrix :as mat]
            [clojure2d.color.whitepoints :as wp])
  (:import [fastmath.vector Vec2 Vec3 Vec4]))

(defn- reference [name] (-> (str "whitepoints/" name) io/resource slurp edn/read-string))

(def colour (delay (reference "colour_reference.edn")))
(def spacesxyz (delay (reference "spacesxyz_reference.edn")))

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

;; G2: chromatic adaptation

(defn- throws-method?
  [k f]
  (try (f) false
       (catch clojure.lang.ExceptionInfo e (= {:method k} (ex-data e)))))

(def ^:private methods-13 [:xyz-scaling :bradford :von-kries :sharp :fairchild :cat97 :cat97s :cat2000
                           :cat02 :cat02brill2008 :cat16 :bianco2010 :bianco2010-pc])

(deftest chromatic-adaptation-methods-test
  (is (= (set methods-13) (set (keys wp/chromatic-adaptation-methods))))
  (testing "cone response matrices equal colour CHROMATIC_ADAPTATION_TRANSFORMS (:cat97s is Fairchild)"
    (doseq [[k expected] (:cat-matrices @colour)]
      (is (= expected (mat/mat->seq (first (wp/chromatic-adaptation-methods k)))) (str k))))
  (testing "inverse: M * M^-1 = I (1e-12)"
    (doseq [k methods-13
            :let [[M M-1] (wp/chromatic-adaptation-methods k)]]
      (is (vec-close? 1.0e-12 (mat/mat->seq (mat/eye 3)) (mat/mat->seq (mat/mulm M M-1))) (str k)))))

(deftest chromatic-adaptation-transform-test
  (doseq [k methods-13]
    (is (identical? (wp/chromatic-adaptation-methods k) (wp/chromatic-adaptation-transform k))))
  (is (throws-method? :nope #(wp/chromatic-adaptation-transform :nope)))
  (is (throws-method? nil #(wp/chromatic-adaptation-transform nil))))

(defn- wp-var [k] @(ns-resolve 'clojure2d.color.whitepoints (symbol (name k))))

(deftest chromatic-adaptation-matrix-test
  ;; 13 methods x (C -> D65, A -> D50, D65 -> E), same formula as colour
  ;; matrix_chromatic_adaptation_VonKries: 1e-12 (observed differences are a few ulps)
  (testing "colour"
    (is (= 39 (count (:adaptation @colour))))
    (doseq [[[k s d] expected] (:adaptation @colour)]
      (is (vec-close? 1.0e-12 expected (mat/mat->seq (wp/chromatic-adaptation-matrix k (wp-var s) (wp-var d))))
          (str [k s d]))))
  (testing "spacesXYZ (:xyz-scaling, :bradford, :von-kries, :cat02, :bianco2010)"
    (is (= 15 (count (:adaptation @spacesxyz))))
    (doseq [[[k s d] expected] (:adaptation @spacesxyz)]
      (is (vec-close? 1.0e-12 expected (mat/mat->seq (wp/chromatic-adaptation-matrix k (wp-var s) (wp-var d))))
          (str [k s d]))))
  (testing "equal white points give the identity"
    (doseq [k methods-13]
      (is (vec-close? 1.0e-15 (mat/mat->seq (mat/eye 3))
                      (mat/mat->seq (wp/chromatic-adaptation-matrix k wp/CIE-2-D65 wp/CIE-2-D65)))
          (str k))))
  (testing "maps the source white to the destination white"
    (doseq [k methods-13]
      (is (vec-close? 1.0e-12 (vec (wp/tristimulus wp/CIE-2-D50))
                      (vec (mat/mulv (wp/chromatic-adaptation-matrix k wp/CIE-2-A wp/CIE-2-D50)
                                     (wp/tristimulus wp/CIE-2-A))))
          (str k))))
  (is (throws-method? :nope #(wp/chromatic-adaptation-matrix :nope wp/CIE-2-C wp/CIE-2-D65))))

;; G3: spectral data and spectrum -> XYZ

(def ^:private observers [:CIE-2 :CIE-10 :CIE-2015-2 :CIE-2015-10])

(defn- throws-data?
  [m f]
  (try (f) false
       (catch clojure.lang.ExceptionInfo e (= m (ex-data e)))))

(defn- xy [[^double X ^double Y ^double Z]] (let [s (m/+ X Y Z)] [(m// X s) (m// Y s)]))

(defn- numerically=
  "Equal structure, numbers compared with `==` (data files store integral values as longs)."
  [a b]
  (cond (map? a) (and (map? b) (= (set (keys a)) (set (keys b))) (every? #(numerically= (a %) (b %)) (keys a)))
        (sequential? a) (and (sequential? b) (= (count a) (count b)) (every? true? (map numerically= a b)))
        (number? a) (and (number? b) (== a b))
        :else (= a b)))

(deftest spectral-data-test
  ;; data files equal colour MSDS_CMFS / SDS_ILLUMINANTS exactly
  (is (numerically= (:cmfs @colour) wp/color-matching-functions-data))
  (is (numerically= (:illuminants @colour) wp/illuminants-spectrum-data)))

(deftest cmf-test
  ;; colour wavelength_to_XYZ (Sprague on the 1 nm data, as here), integer and fractional
  ;; wavelengths: 1e-12
  (doseq [[k rows] (:wavelength @colour)
          :let [[x y z] (wp/cmf k)]
          [l & XYZ] rows]
    (is (vec-close? 1.0e-12 XYZ [(x l) (y l) (z l)]) (str k " " l)))
  (testing "0 outside the observer range"
    (doseq [k observers
            :let [[x y z] (wp/cmf k)
                  [lo hi] (:range (wp/color-matching-functions-data k))]
            l [(- lo 0.5) (+ hi 0.5) 100.0 1000.0]]
      (is (= [0.0 0.0 0.0] [(x l) (y l) (z l)]) (str k " " l))))
  (testing "built once"
    (is (identical? (wp/cmf :CIE-10) (wp/cmf :CIE-10))))
  (testing "1931 and 1964 vars"
    (is (= [wp/cmf-cie2-x wp/cmf-cie2-y wp/cmf-cie2-z] (wp/cmf :CIE-2)))
    (is (= [wp/cmf-cie10-x wp/cmf-cie10-y wp/cmf-cie10-z] (wp/cmf :CIE-10))))
  (is (throws-data? {:observer :nope} #(wp/cmf :nope))))

(deftest spectrum-test
  (is (numerically= {:range [400 700] :lambda [700 400 500] :value [3 1 2]} (wp/spectrum [700 400 500] [3 1 2])))
  (is (= {:range [380 780] :lambda [400 700] :value [1 2]} (wp/spectrum [380 780] [400 700] [1 2]))))

(def ^:private flat-400-700 (wp/spectrum (range 400 701 10) (repeat 31 1.0)))

(deftest spectrum-to-XYZ1-white-test
  ;; perfect reflector under every illuminant and observer vs colour sd_to_XYZ Integration
  ;; (colour interpolates illuminants linearly, as the default here; constant extrapolation to
  ;; the CMF range): XYZ 1e-12
  (is (= 236 (count (:spectrum-white @colour))))
  (doseq [[[o i] XYZ] (:spectrum-white @colour)
          :let [f (wp/->spectrum-to-XYZ1 o i)]]
    (is (vec-close? 1.0e-12 XYZ (vec (f flat-400-700))) (str [o i]))))

(deftest spectrum-to-XYZ1-samples-test
  ;; ColorChecker N Ohta (380-780 nm, 5 nm) vs colour sd_to_XYZ Integration: defaults (:auto sample =
  ;; Sprague, :linear illuminant, = colour defaults), both :linear, both :sprague; :constant and :zero
  ;; extrapolation: 1e-12
  (is (= 36 (count (:spectrum-samples @colour))))
  (doseq [[[o i e interp ill-interp] rows] (:spectrum-samples @colour)
          :let [f (wp/->spectrum-to-XYZ1 o i {:interpolation interp :illuminant-interpolation ill-interp
                                              :extrapolation e})]
          [{:keys [lambda value]} XYZ] (map vector (:colorchecker @colour) rows)]
    (is (vec-close? 1.0e-12 XYZ (vec (f (wp/spectrum lambda value)))) (str [o i e interp ill-interp]))))

(deftest spectrum-to-XYZ1-nonuniform-test
  ;; non-uniform wavelengths: :auto picks a not-a-knot cubic spline, colour CubicSplineInterpolator
  ;; (scipy interp1d cubic), D65, CIE-2: 1e-12
  (let [{:keys [spectra XYZ]} (:spectrum-nonuniform @colour)
        f (wp/->spectrum-to-XYZ1)]
    (is (= 24 (count spectra)))
    (doseq [[{:keys [lambda value]} expected] (map vector spectra XYZ)]
      (is (vec-close? 1.0e-12 expected (vec (f (wp/spectrum lambda value))))))))

(deftest spectrum-to-XYZ1-contract-test
  (testing "defaults: :CIE-2, :D65, :auto, :linear illuminant, :constant (B1: used to throw)"
    (let [res ((wp/->spectrum-to-XYZ1) flat-400-700)]
      (is (instance? Vec3 res))
      (is (rel-close? 1.0e-12 1.0 (.y ^Vec3 res)))
      (is (= res ((wp/->spectrum-to-XYZ1 :CIE-2 :D65 {:interpolation :auto :illuminant-interpolation :linear
                                                      :extrapolation :constant :step 1.0})
                  flat-400-700)))))
  (testing ":auto falls back to :linear below 4 samples, uses a cubic spline for 4-5 samples"
    (let [s3 (wp/spectrum [400 550 700] [0.2 0.8 0.4])
          s5 (wp/spectrum [400 450 500 550 700] [0.2 0.5 0.8 0.6 0.4])]
      (is (= ((wp/->spectrum-to-XYZ1) s3) ((wp/->spectrum-to-XYZ1 :CIE-2 :D65 {:interpolation :linear}) s3)))
      (is (= ((wp/->spectrum-to-XYZ1) s5) ((wp/->spectrum-to-XYZ1 :CIE-2 :D65 {:interpolation :b-spline}) s5)))))
  (testing "B2: integration over the CMF range"
    (doseq [o [:CIE-2015-2 :CIE-2015-10]]
      (is (vec-close? 1.0e-6 [(/ 1.0 3.0) (/ 1.0 3.0)] (xy (vec ((wp/->spectrum-to-XYZ1 o :E) flat-400-700))))
          (str o)))
    ;; ISO-SP (350-560 nm) extended to 830 nm, was cut at 560 nm by :trim
    (is (vec-close? 1.0e-12 (xy (get-in @colour [:spectrum-white [:CIE-2 :ISO-SP]]))
                    (xy (vec ((wp/->spectrum-to-XYZ1 :CIE-2 :ISO-SP) flat-400-700))))))
  (testing ":zero extrapolation of a 400-700 nm flat sample gives Y < 1"
    (is (< 0.99 (.y ^Vec3 ((wp/->spectrum-to-XYZ1 :CIE-2 :D65 {:extrapolation :zero}) flat-400-700)) 1.0)))
  (testing "spectrum map as illuminant equals the keyword"
    (is (= ((wp/->spectrum-to-XYZ1 :CIE-10 :A) flat-400-700)
           ((wp/->spectrum-to-XYZ1 :CIE-10 (wp/illuminants-spectrum-data :A)) flat-400-700))))
  (testing "step: 5 nm sampling keeps Y = 1, white point within 1e-4 of 1 nm"
    (let [res ((wp/->spectrum-to-XYZ1 :CIE-2 :D65 {:step 5.0}) flat-400-700)]
      (is (rel-close? 1.0e-12 1.0 (.y ^Vec3 res)))
      (is (vec-close? 1.0e-4 (xy (vec ((wp/->spectrum-to-XYZ1) flat-400-700))) (xy (vec res))))))
  (testing "errors"
    (doseq [e [:trim nil :skip :error 0.0]]
      (is (throws-data? {:extrapolation e} #(wp/->spectrum-to-XYZ1 :CIE-2 :D65 {:extrapolation e})) (str e)))
    (is (throws-data? {:observer :nope} #(wp/->spectrum-to-XYZ1 :nope :D65)))
    (is (throws-data? {:illuminant :nope} #(wp/->spectrum-to-XYZ1 :CIE-2 :nope)))))

;; G4: white point data

(def ^:private d-series-keys
  #{:CIE-2-D50 :CIE-2-D55 :CIE-2-D60 :CIE-2-D75
    :CIE-10-D50 :CIE-10-D55 :CIE-10-D60 :CIE-10-D65 :CIE-10-D75})

(defn- wp-xy [k] (let [^Vec4 w (wp/whitepoints k)] [(.z w) (.w w)]))

(deftest whitepoints-keys-test
  (is (= 359 (count wp/whitepoints)))
  (is (= 236 (count (filter #(re-find #"\*$" (name %)) (keys wp/whitepoints)))))
  (testing "every key has a var with the same value and a docstring"
    (doseq [k (keys wp/whitepoints)
            :let [v (ns-resolve 'clojure2d.color.whitepoints (symbol (name k)))]]
      (is (= (wp/whitepoints k) @v) (str k))
      (is (seq (:doc (meta v))) (str k)))))

(deftest whitepoints-consistency-test
  ;; [X Z x y] describe the same white: tabulated X, Z = xy->XZ (exact), computed xy = XZ->xy (1e-15)
  (doseq [[k ^Vec4 w] wp/whitepoints]
    (if (re-find #"\*$" (name k))
      (is (vec-close? 1.0e-15 [(.z w) (.w w)] (vec (wp/XZ->xy (.x w) (.y w)))) (str k))
      (when-not (d-series-keys k)
        (is (= [(.x w) (.y w)] (vec (wp/xy->XZ (.z w) (.w w)))) (str k))))))

(deftest whitepoints-tabulated-test
  ;; colour CCS_ILLUMINANTS, exact; D-series replaced by integrations; CIE-2-D65 kept
  (let [ccs (:ccs @colour)]
    (is (= 123 (count ccs)))
    (doseq [[k xy] ccs
            :when (not (or (d-series-keys k) (= :CIE-2-D65 k)))]
      (is (= xy (wp-xy k)) (str k))))
  (is (= [0.312727 0.329023] (wp-xy :CIE-2-D65))))

(deftest whitepoints-d-series-test
  ;; colour sd_to_XYZ of the CIE D-series spectra (300-830 nm, linear, 1 nm): 1e-12
  (doseq [[k XYZ] (:d-series @colour)
          :when (not= :CIE-2-D65 k)
          :let [^Vec4 w (wp/whitepoints k)]]
    (is (vec-close? 1.0e-12 (xy XYZ) (wp-xy k)) (str k))
    (is (vec-close? 1.0e-12 [(first XYZ) (last XYZ)] [(.x w) (.y w)]) (str k)))
  (testing "CIE-2-D65 is the CIE D65 integration rounded to 6 decimals"
    (let [[x y] (xy (get-in @colour [:d-series :CIE-2-D65]))]
      (is (= [0.312727 0.329023] [(m/approx x 6) (m/approx y 6)])))))

(deftest whitepoints-computed-test
  ;; colour sd_to_XYZ Integration of a perfect reflector (= ->spectrum-to-XYZ1 defaults): 1e-12
  (doseq [[[o i] XYZ] (:spectrum-white @colour)
          :let [k (keyword (str (name o) "-" (name i) "*"))
                ^Vec4 w (wp/whitepoints k)
                [X Y Z] XYZ]]
    (is (vec-close? 1.0e-12 [(/ X Y) (/ Z Y)] [(.x w) (.y w)]) (str k))
    (is (vec-close? 1.0e-12 (xy XYZ) (wp-xy k)) (str k))
    (is (vec-close? 1.0e-12 (wp-xy k) (xy (vec ((wp/->spectrum-to-XYZ1 o i) flat-400-700)))) (str k))))

;; G5: white point lookup, correlated color temperature, light source spectra

(deftest whitepoint-test
  (is (= wp/CIE-2-D65 (wp/whitepoint :CIE-2 :D65)))
  (is (= (wp/whitepoints :CIE-2015-10-FL3-1*) (wp/whitepoint :CIE-2015-10 :FL3-1*)))
  (is (throws-data? {:observer :CIE-2015-2 :illuminant :D65} #(wp/whitepoint :CIE-2015-2 :D65)))
  (is (throws-data? {:observer :CIE-2 :illuminant :nope} #(wp/whitepoint :CIE-2 :nope))))

(def ^:private cct-data (delay (:cct @colour)))

(deftest daylight-locus-test
  ;; colour daylight_locus_function: 1e-12
  (doseq [[x y] (:daylight-locus @cct-data)]
    (is (rel-close? 1.0e-12 y (wp/daylight-locus x)) (str x))))

(deftest mired-test
  (doseq [[t mired] (:mired @cct-data)]
    (is (rel-close? 1.0e-15 mired (wp/CCT->mired t)) (str t))
    (is (rel-close? 1.0e-15 t (wp/mired->CCT mired)) (str t))))

(deftest CCT->xy-test
  ;; colour CCT_to_xy_CIE_D / CCT_to_xy_Kang2002, including branch boundaries: 1e-12
  (doseq [[method rows] (get @cct-data :cct->xy)
          [t x y] rows]
    (is (vec-close? 1.0e-12 [x y] (vec (wp/CCT->xy method t))) (str method " " t)))
  (is (= (wp/CCT->xy 6504.0) (wp/CCT->xy :cie-d 6504.0)))
  (testing "domains and methods"
    (is (throws-data? {:method :cie-d :cct 3999.0} #(wp/CCT->xy 3999.0)))
    (is (throws-data? {:method :cie-d :cct 25001.0} #(wp/CCT->xy :cie-d 25001.0)))
    (is (throws-data? {:method :kang2002 :cct 1666.0} #(wp/CCT->xy :kang2002 1666.0)))
    (is (throws-data? {:method :nope} #(wp/CCT->xy :nope 6500.0)))
    (is (thrown? clojure.lang.ExceptionInfo (wp/CCT->xy ##NaN)))))

(deftest xy->CCT-test
  (testing ":mccamy, :hernandez (both branches) vs colour, closed forms: 1e-12"
    (doseq [method [:mccamy :hernandez]
            [x y t] (get-in @cct-data [:xy->cct method])]
      (is (rel-close? 1.0e-12 t (wp/xy->CCT method x y)) (str method " " [x y]))
      (is (rel-close? 1.0e-12 t (wp/xy->CCT method (Vec2. x y))) (str method " " [x y]))))
  (testing ":cie-d vs colour xy_to_CCT_CIE_D (Nelder-Mead, xatol 1e-4 K): 1e-6 relative"
    (doseq [[x y t] (get-in @cct-data [:xy->cct :cie-d])]
      (is (rel-close? 1.0e-6 t (wp/xy->CCT x y)) (str [x y]))
      (is (rel-close? 1.0e-6 t (wp/xy->CCT (Vec2. x y))) (str [x y]))
      (is (rel-close? 1.0e-6 t (wp/xy->CCT :cie-d (Vec2. x y))) (str [x y]))))
  (testing ":cie-d round trip on the locus: 1e-8 relative"
    (doseq [t [4000.0 5003.0 6504.0 7000.0 12000.0 25000.0]]
      (is (rel-close? 1.0e-8 t (wp/xy->CCT (wp/CCT->xy t))) (str t))))
  (testing "errors"
    (is (throws-data? {:method :nope} #(wp/xy->CCT :nope 0.3 0.3)))
    (is (= :cie-d (:method (try (wp/xy->CCT 0.5 0.41) (catch clojure.lang.ExceptionInfo e (ex-data e))))))
    (is (< (:cct (ex-data (try (wp/xy->CCT 0.5 0.41) (catch clojure.lang.ExceptionInfo e e)))) 4000.0))))

(deftest CCT->uv-test
  ;; colour CCT_to_uv_Krystek1985: 1e-12
  (doseq [[t u v] (:cct->uv @cct-data)]
    (is (vec-close? 1.0e-12 [u v] (vec (wp/CCT->uv t))) (str t)))
  (is (throws-data? {:method :krystek :cct 999.0} #(wp/CCT->uv 999.0)))
  (is (throws-data? {:method :nope} #(wp/CCT->uv :nope 6500.0))))

(deftest uv->CCT-test
  ;; colour uv_to_CCT_Krystek1985 (Nelder-Mead): 1e-6 relative; round trip 1e-8
  (doseq [[u v t] (:uv->cct @cct-data)]
    (is (rel-close? 1.0e-6 t (wp/uv->CCT u v)) (str [u v]))
    (is (rel-close? 1.0e-6 t (wp/uv->CCT :krystek (Vec2. u v))) (str [u v])))
  (doseq [t [1000.0 2856.0 6504.0 15000.0]]
    (is (rel-close? 1.0e-8 t (wp/uv->CCT (wp/CCT->uv t))) (str t)))
  (is (throws-data? {:method :nope} #(wp/uv->CCT :nope 0.2 0.3))))

(def ^:private spectra-data (delay (:spectra @colour)))

(deftest planck-law-test
  ;; colour planck_law (sd_blackbody scaling, W/(sr m2 nm)): 1e-12 relative
  (doseq [[l t p] (:planck @spectra-data)]
    (is (m/<= (m/abs (m/- p (wp/planck-law l t))) (m/* 1.0e-12 (m/abs p))) (str [l t])))
  (let [s (wp/blackbody-spectrum 5000.0)]
    (is (numerically= [360 780] (:range s)))
    (is (= 421 (count (:value s))))
    (is (== (wp/planck-law 555.0 5000.0) (nth (:value s) 195))))
  (testing "blackbody white point vs the Krystek Planckian locus: 1.5e-4 in uv (Krystek's fit deviates
            from the integrated locus by up to 1.1e-4, measured 2000-10000 K)"
    (doseq [t [2000.0 3000.0 5000.0 6504.0 10000.0]
            :let [[X Y Z] (vec ((wp/->spectrum-to-XYZ1 :CIE-2 (wp/blackbody-spectrum t (range 360 831))) flat-400-700))
                  d (m/+ X (m/* 15.0 Y) (m/* 3.0 Z))]]
      (is (vec-close? 1.5e-4 (vec (wp/CCT->uv t)) [(m// (m/* 4.0 X) d) (m// (m/* 6.0 Y) d)]) (str t)))))

(deftest illuminant-A-spectrum-test
  ;; colour sd_CIE_standard_illuminant_A (300-830 nm, 5 nm): 1e-12 relative; tabulated :A: 5e-6 (table rounding)
  (let [rows (:illuminant-A @spectra-data)
        s (wp/illuminant-A-spectrum (map first rows))]
    (is (vec-close? 1.0e-12 (map second rows) (:value s)))
    (is (vec-close? 1.0e-12 [100.0] [(first (:value (wp/illuminant-A-spectrum [560])))]))
    (is (vec-close? 5.0e-6 (:value (wp/illuminants-spectrum-data :A))
                    (:value (wp/illuminant-A-spectrum (:lambda (wp/illuminants-spectrum-data :A))))))))

(deftest illuminant-D-spectrum-test
  ;; colour sd_CIE_illuminant_D_series (M1, M2 rounded): 1e-12 relative to max(1, |value|)
  (doseq [[[x y] values] (:illuminant-D @spectra-data)
          :let [s (wp/illuminant-D-spectrum (Vec2. x y))]]
    (is (numerically= [300 830] (:range s)))
    (is (vec-close? 1.0e-12 values (:value s)) (str [x y])))
  (testing "CCT argument equals the xy of CCT->xy"
    (is (= (wp/illuminant-D-spectrum 6504.0) (wp/illuminant-D-spectrum (wp/CCT->xy 6504.0)))))
  (testing "D65 from its nominal CCT 6500 * 1.4388 / 1.4380 matches the tabulated D65 (CIE table rounding): 1e-3"
    (let [tab (wp/illuminants-spectrum-data :D65)
          s (wp/illuminant-D-spectrum (m// (m/* 6500.0 1.4388) 1.4380))
          by-l (zipmap (:lambda s) (:value s))]
      (is (vec-close? 1.0e-3 (:value tab) (map by-l (:lambda tab))))))
  (is (throws-data? {:method :cie-d :cct 3000.0} #(wp/illuminant-D-spectrum 3000.0))))
