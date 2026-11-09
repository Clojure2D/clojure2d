(ns clojure2d.color.representation-test
  "Color representation layer of `clojure2d.color` (area A of the color audit), checked against
  `utils/color/gen_representation.py`: CSS Color 4 named colors and hex parsing (matplotlib), hexagonal
  hue (Python colorsys) and the polar hue formula; other behavior as properties."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [clojure2d.color :as c]
            [clojure2d.test-predicates :refer [rel-close?]])
  (:import [fastmath.vector Vec4]
           [java.awt Color]))

(def reference (delay (-> "color/representation_reference.edn" io/resource slurp edn/read-string)))

(defn- vec-close? [tol expected actual]
  (and (= (count expected) (count actual))
       (every? true? (map (partial rel-close? tol) expected actual))))

;; G1: oracle tests

(deftest css-named-colors-test
  ;; the 148 CSS Color 4 names (matplotlib CSS4_COLORS), exact, opaque
  (let [css (:css-names @reference)]
    (is (= 148 (count css)))
    (is (every? (set (c/named-colors-list)) (keys css)))
    (doseq [[k rgb] css]
      (is (= (conj rgb 255.0) (vec (c/to-color k))) (str k)))))

(deftest hex-strings-test
  ;; 3, 4 (CSS #rgba), 6 and 8 hex digits, with and without #, mixed case (matplotlib to_rgba), exact
  (doseq [[s & rgba] (:hex @reference)]
    (is (= (vec rgba) (vec (c/to-color s))) s)))

(deftest hue-test
  ;; hexagonal hue vs colorsys, polar hue vs atan2 formula: 1e-12 relative to max(1, |expected|)
  (let [{:keys [colors hue hue-polar]} @reference]
    (doseq [[col h hp] (map vector colors hue hue-polar)]
      (is (rel-close? 1.0e-12 h (c/hue col)) (str col))
      (is (rel-close? 1.0e-12 hp (c/hue-polar col)) (str col)))))

;; G1: properties

(deftest to-color-types-test
  (let [expected (v/vec4 10.0 20.0 30.0 40.0)]
    (testing "Vec4 is returned as is"
      (is (identical? expected (c/to-color expected))))
    (testing "every input type"
      (doseq [[label x] [["Vec3" (v/vec3 10 20 30)] ["Color" (Color. 10 20 30 40)]
                         ["vector" [10 20 30 40]] ["list" '(10 20 30 40)] ["lazy seq" (map identity [10 20 30 40])]
                         ["queue (Seqable)" (into clojure.lang.PersistentQueue/EMPTY [10 20 30 40])]
                         ["double[]" (double-array [10 20 30 40])] ["int[]" (int-array [10 20 30 40])]
                         ["long[]" (long-array [10 20 30 40])] ["long" 0x280a141e] ["int" (int 0x280a141e)]
                         ["string" "#0a141e28"]]]
        (is (= (if (= label "Vec3") (assoc expected 3 255.0) expected) (c/to-color x)) label)))
    (testing "Vec2 is gray with alpha; keyword is a named color"
      (is (= (v/vec4 10.0 10.0 10.0 40.0) (c/to-color (v/vec2 10 40))))
      (is (= (v/vec4 255.0 0.0 0.0 255.0) (c/to-color :red))))
    (testing "sequence length: 0 black, 1 gray, 2 gray + alpha, 3 RGB, 5 first four"
      (doseq [f [vec #(apply list %) double-array]]
        (is (= (v/vec4 0.0 0.0 0.0 255.0) (c/to-color (f []))))
        (is (= (v/vec4 7.0 7.0 7.0 255.0) (c/to-color (f [7]))))
        (is (= (v/vec4 7.0 7.0 7.0 9.0) (c/to-color (f [7 9]))))
        (is (= (v/vec4 1.0 2.0 3.0 255.0) (c/to-color (f [1 2 3]))))
        (is (= (v/vec4 1.0 2.0 3.0 4.0) (c/to-color (f [1 2 3 4 5]))))))
    (testing "packed integers: top byte 0 is opaque, hex strings with 1-2 digits are grays"
      (is (= (v/vec4 255.0 170.0 1.0 255.0) (c/to-color 0xffaa01)))
      (is (= (v/vec4 255.0 170.0 1.0 128.0) (c/to-color 0x80ffaa01)))
      (is (= (v/vec4 171.0 171.0 171.0 255.0) (c/to-color "ab")))
      (is (= (v/vec4 255.0 255.0 255.0 255.0) (c/to-color "f"))))
    (testing "to-color does not clamp, also for 1-2 element sequences"
      (is (= (v/vec4 300.0 -5.0 10.0 255.0) (c/to-color [300 -5 10])))
      (is (= (v/vec4 300.0 300.0 300.0 -5.0) (c/to-color [300 -5]) (c/to-color (double-array [300 -5])))))))

(deftest accessors-setters-test
  (let [x [10 20 30 40]]
    (is (= [10.0 20.0 30.0 40.0] [(c/red x) (c/green x) (c/blue x) (c/alpha x)]))
    (is (= [10.0 20.0 30.0] [(c/ch0 x) (c/ch1 x) (c/ch2 x)]))
    (is (= [10.0 20.0 30.0 40.0] (mapv (partial c/get-channel x) [0 1 2 3])))
    (testing "setters replace one channel and do not clamp"
      (is (= (v/vec4 300.0 20.0 30.0 40.0) (c/set-red x 300.0) (c/set-ch0 x 300.0) (c/set-channel x 0 300.0)))
      (is (= (v/vec4 10.0 -1.0 30.0 40.0) (c/set-green x -1.0) (c/set-ch1 x -1.0) (c/set-channel x 1 -1.0)))
      (is (= (v/vec4 10.0 20.0 99.0 40.0) (c/set-blue x 99.0) (c/set-ch2 x 99.0) (c/set-channel x 2 99.0)))
      (is (= (v/vec4 10.0 20.0 30.0 999.0) (c/set-alpha x 999.0) (c/set-channel x 3 999.0))))))

(deftest colorspace-channels-test
  ;; get-channel / set-channel in a colorspace = the channel of the converted color, 1e-9; setting a channel to
  ;; its own value gives from(to(x)) (the colorspace round trip itself is area B: Oklab, YCbCr are not exact)
  (let [x (c/color 200 50 30)]
    (doseq [cs [:HSB :LAB :Oklab :YCbCr :XYZ]
            :let [[to from] (c/colorspaces cs)
                  conv (to x)]
            ch [0 1 2]]
      (is (== (conv ch) (c/get-channel x cs ch)) (str cs ch))
      (is (vec-close? 1.0e-9 (from (assoc conv ch (+ 1.0 (conv ch)))) (c/set-channel x cs ch (+ 1.0 (conv ch))))
          (str cs ch))
      (is (vec-close? 1.0e-9 (from conv) (c/set-channel x cs ch (conv ch))) (str cs ch)))))

(deftest clamping-rounding-test
  (is (= (v/vec4 0.0 250.2 255.0 200.4) (c/clamp (v/vec4 -1.1 250.2 260.3 200.4))))
  (is (= (v/vec4 2.0 3.0 0.0 255.0) (c/lclamp [1.5 2.5 -0.5 254.5])) "half up")
  (testing "creation clamps every arity (G2), to-color does not"
    (is (= (v/vec4 255.0 0.0 10.0 255.0) (c/color 300 -5 10) (c/color [300 -5 10]) (c/color [300 -5 10 900])))
    (is (= (v/vec4 255.0 0.0 10.0 0.0) (c/color 300 -5 10 -1) (c/color [300 -5 10] -1)))
    (is (= (v/vec4 255.0 255.0 255.0 0.0) (c/gray 300 -5)))
    (is (= (v/vec4 0.0 0.0 0.0 255.0) (c/gray -3)))))

(deftest scale-test
  (is (= (v/vec4 20.0 40.0 60.0 40.0) (c/scale [10 20 30 40] 2.0)))
  (is (= (v/vec4 20.0 40.0 60.0 80.0) (c/scale [10 20 30 40] 2.0 true)))
  (is (= (v/vec4 1.0 1.0 1.0 255.0) (c/scale-down :white)))
  (is (= (v/vec4 1.0 1.0 1.0 1.0) (c/scale-down :white true)))
  (is (= (v/vec4 255.0 255.0 255.0 255.0) (c/scale-up [1 1 1])))
  (is (vec-close? 1.0e-15 [10 20 30 40] (c/scale-up (c/scale-down [10 20 30 40] true) true))))

(deftest format-hex-pack-test
  (testing "format-hex: #rrggbb, #rrggbbaa below alpha 255, rounded and clamped, read back by to-color"
    (is (= "#ff0000" (c/format-hex :red)))
    (is (= "#ff000080" (c/format-hex (c/color 255 0 0 128))))
    (is (= "#ff0000" (c/format-hex [255.6 -3 0.4])))
    (doseq [col [[10 20 30] [10 20 30 0] [255 254 1 128] [0 0 0 254]]]
      (is (= (c/lclamp col) (c/to-color (c/format-hex col))) (str col))))
  (testing "pack: ARGB int, read back by to-color for alpha > 0; transparent reads back as opaque"
    (is (= (unchecked-int 0xffff0000) (c/pack :red)))
    (is (= (c/pack :red) (c/quil :red)))
    (doseq [col [[10 20 30] [10 20 30 1] [255 254 1 128] [0 0 0 254]]]
      (is (= (c/lclamp col) (c/to-color (c/pack col))) (str col)))
    (is (== 255.0 (c/alpha (c/to-color (c/pack [10 20 30 0])))))))

(deftest black-test
  (is (c/black? :black))
  (is (c/black? (v/vec4 -1.0 0.0 0.0 0.0)) "negative counts as 0, alpha ignored")
  (is (not (c/black? [0.2 0 0])))
  (is (c/not-black? [0.2 0 0]))
  (is (not (c/not-black? [0 0 0 10]))))

(deftest awt-test
  (is (= (Color. 1 2 3) (c/awt-color 1.4 2.0 2.6)))
  (is (= (Color. 255 0 3 4) (c/awt-color 300 -5 3 4)))
  (is (= (Color. 255 0 0 128) (c/awt-color :red 128) (c/set-awt-alpha :red 128)))
  (is (= (Color. 255 0 0 255) (c/set-awt-alpha :red 300)))
  (is (= (Color. 100 100 100) (c/awt-gray 100)))
  (is (= (Color. 100 100 100 50) (c/awt-gray 100 50)))
  (is (= (Color. 10 20 30 40) (c/to-awt-color [10.2 19.6 30 40])))
  (is (= (v/vec4 10.0 20.0 30.0 40.0) (c/to-color (c/to-awt-color [10 20 30 40])))))

(deftest color-converter-test
  ;; Processing colorMode: channels rescaled from the given maxima to 0-255, then from-X*
  (let [hsb (c/color-converter :HSB 360.0 100.0 100.0)
        [_ from*] (c/colorspaces* :HSB)]
    (is (vec-close? 1.0e-12 (from* (v/vec4 (* 255.0 (/ 120.0 360.0)) 127.5 255.0 255.0)) (hsb [120 50 100])))
    (is (identical? from* (c/color-converter :HSB)))
    (is (vec-close? 1.0e-12 (from* (v/vec4 127.5 127.5 127.5 127.5)) ((c/color-converter :HSB 2.0) [1 1 1 1])))))

(deftest predicates-test
  (is (c/possible-color? :red))
  (is (c/possible-color? "#fff"))
  (is (c/possible-color? [1 2 3]))
  (is (c/possible-palette? [:red :green]))
  (is (not (c/possible-palette? :red)))
  (is (not (c/possible-palette? identity)))
  (is (instance? Vec4 (c/valid-color? :red)))
  (is (false? (c/valid-color? "zz"))))

(deftest docstrings-test
  (doseq [s ['named-colors-list 'possible-palette? 'to-awt-color 'red 'green 'blue 'ch0 'ch1 'ch2 'hue 'hue-polar
             'set-alpha 'set-red 'set-green 'set-blue 'set-ch0 'set-ch1 'set-ch2 'set-awt-alpha 'awt-color 'awt-gray
             'clamp 'lclamp 'scale 'scale-down 'scale-up 'format-hex 'pack 'quil 'black? 'not-black?]]
    (is (m/> (count (:doc (meta (ns-resolve 'clojure2d.color s)))) 80) (str s))))

;; G2: value fixes

(deftest packed-alpha-test
  ;; one rule for every accessor: top byte 0 means opaque
  (doseq [n [0x000000 0xff0000 0x00ff0000 0x01ff0000 0x80123456 0xff123456 -1 Integer/MIN_VALUE]
          x [n (unchecked-int n)]]
    (is (== (c/alpha x) (c/alpha (c/to-color x))) (str x))
    (is (= [(c/red x) (c/green x) (c/blue x)] (vec (take 3 (c/to-color x)))) (str x)))
  (is (== 255.0 (c/alpha 0xff0000)))
  (is (= (unchecked-int 0xffff0000) (c/pack 0xff0000))))

(deftest hyphenated-names-test
  ;; every non-namespaced hyphenated name equals its CSS name without hyphens (sandy-brown fixed in G2)
  (let [css (:css-names @reference)
        names (filter #(and (nil? (namespace %)) (re-find #"-" (name %))) (c/named-colors-list))]
    (is (< 80 (count names)))
    (doseq [k names
            :let [plain (keyword (clojure.string/replace (name k) "-" ""))]]
      (is (= (conj (css plain) 255.0) (vec (c/to-color k))) (str k)))))

(deftest luminance-test
  ;; relative-luma vs colour eotf_sRGB weighted with the to-XYZ Y row, and vs Y of to-XYZ: 1e-12 relative
  (let [{:keys [colors relative-luminance]} @reference]
    (doseq [[col e] (map vector colors relative-luminance)]
      (is (rel-close? 1.0e-12 e (c/relative-luma col)) (str col))
      (is (rel-close? 1.0e-12 (* 2.55 ((c/to-XYZ col) 1)) (c/relative-luma col)) (str col))))
  (testing "luma: the same weights on the encoded channels"
    (is (rel-close? 1.0e-15 (+ (* 0.21267285140562248 255.0)) (c/luma :red)))
    (is (rel-close? 1.0e-15 255.0 (c/luma :white)))
    (is (== 100.0 (c/luma (c/gray 100))) "gray: weights sum to 1")))
