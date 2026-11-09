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
            [clojure2d.color.whitepoints :as wp]
            [clojure2d.test-predicates :refer [rel-close? throws-data?]]))

(defn- reference [name] (-> (str "rgb/" name) io/resource slurp edn/read-string))

(def colour (delay (reference "colour_reference.edn")))

;; G1: transfer functions

(def ^:private transfer-fns
  {:sRGB [rgb/linear-to-srgb rgb/srgb-to-linear]
   :gamma-2.2 [(rgb/->gamma (/ 2.2)) (rgb/->gamma 2.2)]
   :gamma-1.8 [(rgb/->gamma (/ 1.8)) (rgb/->gamma 1.8)]
   :L* [rgb/to-L* rgb/from-L*]
   :ACEScc [rgb/to-ACEScc rgb/from-ACEScc]
   :ACEScct [rgb/to-ACEScct rgb/from-ACEScct]
   :Blackmagic-WG [rgb/to-Blackmagic-WG rgb/from-Blackmagic-WG]
   :BT2020 [rgb/to-BT2020 rgb/from-BT2020]
   :BT709 [rgb/to-BT709 rgb/from-BT709]
   :ROMM [rgb/to-ROMM rgb/from-ROMM]
   :SMPTE240M [rgb/to-SMPTE240M rgb/from-SMPTE240M]
   :BT1886 [rgb/to-BT1886 rgb/from-BT1886]
   :BT1886-100-0.1 [#(rgb/to-BT1886 % 100.0 0.1) #(rgb/from-BT1886 % 100.0 0.1)]
   :PQ [rgb/to-PQ rgb/from-PQ]
   :HLG [rgb/to-HLG rgb/from-HLG]
   :gamma-2.6 [(rgb/->gamma (/ 2.6)) (rgb/->gamma 2.6)]})

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
  (testing "negative values round-trip, except ACEScc (constant below 0), BT.1886 and PQ (decoding clamps to 0)"
    (doseq [[k [encode decode]] (dissoc transfer-fns :ACEScc :BT1886 :BT1886-100-0.1 :PQ)
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

(deftest BT1886-defaults-test
  ;; default white 1, black 0 is the plain power law 2.4
  (doseq [x [0.0 0.01 0.18 0.5 1.0 2.0]]
    (is (rel-close? 1.0e-12 ((rgb/->gamma 2.4) x) (rgb/from-BT1886 x)) (str x))
    (is (rel-close? 1.0e-12 ((rgb/->gamma (/ 2.4)) x) (rgb/to-BT1886 x)) (str x))))

(deftest HDR-transfer-test
  (testing "PQ: 100 cd/m2 (0.01) and 10000 cd/m2 (1.0)"
    (is (rel-close? 1.0e-12 1.0 (rgb/to-PQ 1.0)))
    (is (m/< (m/abs (m/- 0.508078421517399 (rgb/to-PQ 0.01))) 1.0e-12)))
  (testing "HLG: 1/12 -> 0.5, 1 -> 1 within 1e-8 (the 8 digit constants a, b give 0.9999999951, as colour)"
    (is (rel-close? 1.0e-15 0.5 (rgb/to-HLG (/ 12.0))))
    (is (rel-close? 1.0e-8 1.0 (rgb/to-HLG 1.0)))))

(deftest namespace-docstring-test
  (let [doc (:doc (meta (find-ns 'clojure2d.color.rgb)))]
    (is (re-find #":rgb-to-xyz" doc))
    (is (re-find #"to-ACEScct" doc))
    (is (re-find #"\[\[rgbs\]\]" doc))))

;; G2: space table

(def ^:private aliases {:ITU-R-BT470-525 :NTSC :ITU-R-BT470-625 :PAL-SECAM :ROMM-RGB :ProPhoto-RGB})

(defn- wp-var [k] @(ns-resolve 'clojure2d.color.whitepoints (symbol (name k))))

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
  (let [space (into {} (rgb/rgbs :Adobe-RGB-1998))] (is (identical? space (rgb/rgb-space space))))
  (testing "space map without required keys"
    (is (throws-data? {:missing-keys #{:from :rgb-to-xyz :whitepoint :xyz-to-rgb}} #(rgb/rgb-space {:to identity})))
    (is (throws-data? {:missing-keys #{:whitepoint}} #(rgb/rgb-space (assoc (rgb/rgbs :sRGB) :whitepoint nil)))))
  (is (throws-data? {:space :nope} #(rgb/rgb-space :nope)))
  (is (throws-data? {:space nil} #(rgb/rgb-space nil)))
  (is (= (sort (keys rgb/rgbs)) rgb/rgb-spaces-list)))
