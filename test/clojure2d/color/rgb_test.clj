(ns clojure2d.color.rgb-test
  "RGB spaces and transfer functions checked against oracles generated in `utils/rgb/`:

  - colour-science 0.4.7 (`colour_reference.edn`), with the standard constants where colour deviates
    (BT.2020 double precision constants, ACEScct decoding capped at 65504)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [fastmath.core :as m]
            [fastmath.matrix :as mat]
            [clojure2d.color.rgb :as rgb]
            [fastmath.vector :as v]
            [clojure2d.color.whitepoints :as wp]))

(defn- reference [name] (-> (str "rgb/" name) io/resource slurp edn/read-string))

(def colour (delay (reference "colour_reference.edn")))

(defn- rel-close?
  "Difference within `tol` scaled by `max(1, |b|)`; equal infinities are close."
  [^double tol a b]
  (let [a (double a) b (double b)]
    (or (== a b)
        (m/<= (m/abs (m/- a b)) (m/* tol (m/max 1.0 (m/abs b)))))))

;; G1: transfer functions

(def ^:private transfer-fns
  {:sRGB [rgb/linear-to-srgb rgb/srgb-to-linear]
   :gamma-2.2 [(rgb/->gamma (/ 2.2)) (rgb/->gamma 2.2)]
   :gamma-1.8 [(rgb/->gamma (/ 1.8)) (rgb/->gamma 1.8)]
   :L* [rgb/to-L* rgb/from-L*]
   :ACEScc [rgb/to-ACEScc rgb/from-ACEScc]
   :ACEScct [rgb/to-ACEScct rgb/from-ACEScct]
   :Blackmagic-WG [rgb/to-Blackmagic-WG rgb/from-Blackmagic-WG]
   :BT2020 [rgb/to-BT2020 rgb/from-BT2020]})

(deftest transfer-functions-test
  ;; encoding and decoding on -0.5 ... 1000 vs colour: 1e-12 relative to max(1, |expected|)
  (is (= (set (keys transfer-fns)) (set (keys (:transfer @colour)))))
  (doseq [[k rows] (:transfer @colour)
          :let [[encode decode] (transfer-fns k)]
          [x enc dec] rows]
    (is (rel-close? 1.0e-12 enc (encode x)) (str k " encode " x))
    (is (rel-close? 1.0e-12 dec (decode x)) (str k " decode " x))))

(deftest transfer-round-trip-test
  ;; decode(encode(x)) and encode(decode(y)) for linear 1e-6 ... 1000: 1e-12 relative
  (doseq [[k [encode decode]] transfer-fns
          x [1.0e-6 1.0e-4 0.001 0.0031308 0.01 0.018 0.1 0.18 0.5 0.9 1.0 2.0 100.0 1000.0]]
    (is (rel-close? 1.0e-12 x (decode (encode x))) (str k " " x)))
  (testing "negative values round-trip, except ACEScc (constant below 0)"
    (doseq [[k [encode decode]] (dissoc transfer-fns :ACEScc)
            x [-0.5 -0.01 -1.0e-4]]
      (is (rel-close? 1.0e-12 x (decode (encode x))) (str k " " x)))
    (is (== (rgb/to-ACEScc 0.0) (rgb/to-ACEScc -1.0) (/ (- 9.72 16.0) 17.52)))))

(deftest transfer-boundaries-test
  (testing "sRGB decoding threshold 0.040449936 vs IEC 0.04045: both segments agree within 3e-9 there"
    (is (m/< (m/abs (m/- (m/pow (m// (m/+ 0.04045 0.055) 1.055) 2.4) (rgb/srgb-to-linear 0.04045))) 3.0e-9)))
  (testing "ACEScc and ACEScct decoding capped at 65504 (largest half float)"
    (doseq [f [rgb/from-ACEScc rgb/from-ACEScct]
            v [1.4679963120447153 1.5 10.0]]
      (is (== 65504.0 (f v)))))
  (testing "Blackmagic decoding overflows to infinity"
    (is (== ##Inf (rgb/from-Blackmagic-WG 100.0)))))

(deftest namespace-docstring-test
  (let [doc (:doc (meta (find-ns 'clojure2d.color.rgb)))]
    (is (re-find #":rgb-to-xyz" doc))
    (is (re-find #"to-ACEScct" doc))
    (is (re-find #"\[\[rgbs\]\]" doc))))

;; G2: space table

(def ^:private aliases {:ITU-R-BT470-525 :NTSC :ITU-R-BT470-625 :PAL-SECAM})

(defn- wp-var [k] @(ns-resolve 'clojure2d.color.whitepoints (symbol (name k))))

(defn- throws-data?
  "Calls `f`, expects `ex-info` with exactly `m` as data."
  [m f]
  (try (f) false
       (catch clojure.lang.ExceptionInfo e (= m (ex-data e)))))

(deftest rgbs-keys-test
  (is (= (set (concat (keys (:spaces @colour)) (keys aliases))) (set (keys rgb/rgbs))))
  (doseq [[a k] aliases]
    (is (identical? (rgb/rgbs k) (rgb/rgbs a)) (str a))))

(deftest rgbs-definitions-test
  ;; primaries equal colour RGB_COLOURSPACES; white point = the clojure2d var used by the oracle;
  ;; matrices vs colour normalised_primary_matrix with that white point: 1e-15, inverses 1e-14
  (doseq [[k {:keys [red green blue whitepoint rgb-to-xyz xyz-to-rgb]}] (:spaces @colour)
          :let [space (rgb/rgbs k)]]
    (is (= [red green blue] (mapv #(mapv double %) [(:red space) (:green space) (:blue space)])) (str k))
    (is (identical? (wp-var whitepoint) (:whitepoint space)) (str k))
    (is (every? true? (map (partial rel-close? 1.0e-15) rgb-to-xyz (mat/mat->seq (:rgb-to-xyz space)))) (str k))
    (is (every? true? (map (partial rel-close? 1.0e-14) xyz-to-rgb (mat/mat->seq (:xyz-to-rgb space)))) (str k))))

(deftest rgbs-transfer-test
  ;; space :to / :from vs colour cctf (standard constants where colour deviates): 1e-12 relative
  (let [grid (:grid @colour)]
    (doseq [[k {:keys [encoded decoded]}] (:spaces @colour)
            :let [{:keys [to from]} (rgb/rgbs k)]
            [x enc dec] (map vector grid encoded decoded)]
      (is (rel-close? 1.0e-12 enc (to x)) (str k " encode " x))
      (is (rel-close? 1.0e-12 dec (from x)) (str k " decode " x)))))

(deftest rgb-data->map-test
  (let [space (rgb/rgb-data->map [0.64 0.33] [0.30 0.60] [0.15 0.06] wp/CIE-2-D65
                                 rgb/linear-to-srgb rgb/srgb-to-linear)]
    (is (= #{:red :green :blue :whitepoint :rgb-to-xyz :xyz-to-rgb :to :from} (set (keys space))))
    (is (= [(:rgb-to-xyz space) (:xyz-to-rgb space)]
           (rgb/rgb-data->matrix [0.64 0.33] [0.30 0.60] [0.15 0.06] wp/CIE-2-D65)))
    (testing "RGB [1 1 1] maps to the white point, inverse matrix is the inverse"
      (is (every? true? (map (partial rel-close? 1.0e-15) (vec (wp/tristimulus wp/CIE-2-D65))
                             (vec (mat/mulv (:rgb-to-xyz space) (v/vec3 1.0 1.0 1.0))))))
      (is (every? true? (map (partial rel-close? 1.0e-15) (mat/mat->seq (mat/eye 3))
                             (mat/mat->seq (mat/mulm (:rgb-to-xyz space) (:xyz-to-rgb space)))))))))

(deftest rgb-space-test
  (is (identical? (rgb/rgbs :sRGB) (rgb/rgb-space :sRGB)))
  (let [space {:to identity}] (is (identical? space (rgb/rgb-space space))))
  (is (throws-data? {:space :nope} #(rgb/rgb-space :nope)))
  (is (throws-data? {:space nil} #(rgb/rgb-space nil)))
  (is (= (sort (keys rgb/rgbs)) rgb/rgb-spaces-list)))
