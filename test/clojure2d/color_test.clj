(ns clojure2d.color-test
  (:require [clojure.test :refer [deftest is are testing]]
            [clojure.edn]
            [clojure.java.io]
            [clojure2d.color :as c]
            [clojure2d.color.blend :as b]
            [clojure2d.color.munsell :as mu]
            [clojure2d.color.whitepoints :as wp]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v])
  (:import [fastmath.vector Vec4]))

;; color protocol

(def cv3 (v/vec3 245.0 245.0 220.0))
(def cv4 (v/vec4  245.0 245.0 220.0 255.0))
(def cc (java.awt.Color. 245 245 220))

;; => "#f5f5dc"

(deftest type-conversion-test
  (is (m/approx-eq 243.195 (c/luma :beige))) ;; => 243.19577500000003
  (is (m/approx-eq 243.195 (c/luma cv3)))
  (is (m/approx-eq 243.195 (c/luma cv4)))
  (is (m/approx-eq 243.195 (c/luma cc)))

  (is (= cv4 (c/to-color :beige)))
  (is (= cv4 (c/to-color cv3)))
  (is (= cv4 (c/to-color cc)))

  (is (= cc (c/to-awt-color :beige)))
  (is (= cc (c/to-awt-color cv3)))
  (is (= cc (c/to-awt-color cv4)))

  (is (= cv4 (c/to-color "#f5f5dc")))
  (is (= cv4 (c/to-color "f5f5dc")))
  (is (= cv4 (c/to-color "F5F5DC")))
  (is (= cv4 (c/to-color "#f5f5dcff")))
  
  (is (= cv4 (c/to-color 0xf5f5dc)))
  (is (= cv4 (c/to-color 0xfff5f5dc))))

(deftest alpha-test
  (is (= (c/color 245.0 245.0 220.0 100.0) (c/set-alpha cv4 100)))
  (is (= (c/color 245.0 245.0 220.0 100.0) (c/to-color (c/set-awt-alpha cc 100)))))

(deftest make-color-test
  (is (= cc (c/awt-color cv3 255.0)))
  (is (= cc (c/awt-color 245.0 245.0 220.0)))
  (is (= cc (c/awt-color 245.0 245.0 220.0 255.0)))
  (is (= cv4 (c/color cv3 255)))
  (is (= cv4 (c/color 245.0 245.0 220.0)))
  (is (= cv4 (c/color 245.0 245.0 220.0 255.0))))

(deftest hue-test
  (is (= 60.0 (c/hue (c/color :beige)))))

;;

(deftest numerical-operations
  (is (= (v/vec4 0.0 250.2 255.0 200.4) (c/clamp (v/vec4 -1.1 250.2 260.3 200.4))))
  (is (= (v/vec4 0.0 250.0 255.0 200.0) (c/lclamp (v/vec4 -1.1 250.2 260.3 200.4))))
  (is (= (v/vec4 255.0 255.0 255.0 255.0) (c/scale [1 1 1] 255.0)))
  (is (= (v/vec4 0.5 0.5 0.5 255.0) (c/scale [127.5 127.5 127.5] (/ 255.0)))))

;; test blends

(deftest blend-test
  (is (== 255.0 (b/normal 100.0 255.0)))
  (is (== 0.0 (b/normal 200.0 0.0)))
  (is (== 255.0 (b/normal 0.0 255.0)))
  (is (== 0.0 (b/normal 1.0 0.0)))

  (is (== 255.0 (b/add 100.0 200.0)))
  (is (== 150.0 (b/add 50.0 100.0)))
  (is (== 255.0 (b/add 0.0 255.0)))
  (is (== 255.0 (b/add 255.0 0.0)))

  (is (== (mod 300 256.0) (b/madd 100.0 200.0)))
  (is (== 150.0 (b/madd 50.0 100.0)))
  (is (== 255.0 (b/madd 0.0 255.0)))
  (is (== 255.0 (b/madd 255.0 0.0)))

  (is (== 0.0 (b/subtract 100.0 200.0)))
  (is (== 50 (b/subtract 100.0 50.0)))
  (is (== 0.0 (b/subtract 0.0 255.0)))
  (is (== 255.0 (b/subtract 255.0 0.0)))

  (is (== (mod -100 256.0) (b/msubtract 100.0 200.0)))
  (is (== 50 (b/msubtract 100.0 50.0)))
  (is (== 1.0 (b/msubtract 0.0 255.0)))
  (is (== 255.0 (b/msubtract 255.0 0.0))))

;; test color converters

(def random-color-sequence
  (->> (concat (c/named-colors-list)
               (repeatedly 30000 #(v/generate-vec4 (fn [] (r/irand 256))))
               (repeatedly 30000 #(v/generate-vec3 (fn [] (r/irand 256))))
               (repeatedly 40000 r/irand)
               (repeatedly 40000 #(v/fmap (c/to-color (c/random-color)) (fn [^double v] (m/floor v)))))
       (mapv c/to-color)))

(defn compare-colors
  [[to from] colors]
  (filter identity
          (map (fn [c]
                 (not= c (v/fmap (from (to c)) #(m/round %)))) colors)))

(defn compare-colors-
  [[to from] colors]
  (filter #(not= (first %) (second %))
          (map (fn [c]
                 [c (v/fmap (from (to c)) #(m/round %))]) colors)))

(defn colorspace-validity
  "Test if colorspace conversion works properly"
  [cs cs-tab]
  (empty? (compare-colors (cs-tab cs) random-color-sequence)))

(defmacro build-colorspace-reversibility-test
  [cs-tab]
  (let [cs-sym (symbol (str "reversibility-test-" (name cs-tab)))]
    `(deftest ~cs-sym
       ~@(for [cs (remove #{:Gray} (keys c/colorspaces))]
           `(is (colorspace-validity ~cs ~cs-tab))))))

(build-colorspace-reversibility-test c/colorspaces)
(build-colorspace-reversibility-test c/colorspaces*)

;; color conversion

(deftest conversion-examples
  (let [reference-color (c/from-XYZ [20.654008, 12.197225, 5.136952])]
    ;; https://colour.readthedocs.io/en/v0.4.1/index.html#colour-models-colour-models
    (are [f res] (= res (v/approx (f reference-color)))
      c/to-Yxy (v/vec4 12.2 0.54 0.32 255.0)
      c/to-LAB (v/vec4 41.53 52.64 26.92 255.0)
      c/to-LUV (v/vec4 41.53 96.83 17.74 255.0)
      c/to-HunterLAB (v/vec4 34.92 47.05 14.36 255.0)
      c/to-IPT (v/vec4 0.38 0.38 0.19 255.0)
      c/to-IgPgTg (v/vec4 0.42 0.19 0.11 255.0)
      c/to-Oklab (v/vec4 0.52 0.15 0.06 255.0)
      c/to-OSA (v/vec4 -3.0 3.0 -9.67 255.0)
      c/to-UCS (v/vec4 0.14 0.12 0.11 255.0)
      c/to-UVW (v/vec4 94.55 11.55 40.55 255.0))
    ;; colorio, colour has different XYZ scaling
    (are [f res] (= res (v/approx (f reference-color) 4))
      c/to-JAB (v/vec4 0.0747 0.0707 0.0472 255.0))
    ;; array([ 0.99603944,  0.93246304,  0.45620519])
    ;; (* 0.99603944 360.0) ;; => 358.5741984    
    (is (= (v/vec4 358.57 0.93 0.46 255.0)
           (-> [0.45620519, 0.03081071, 0.04091952]
               (c/scale 255.0)
               (c/to-HSV)
               (v/approx))))
    (is (= (v/vec4 103 0 51 255) (c/lclamp (c/from-YCbCr* [36 136 175]))))
    (is (= (v/vec4 0.5625 -0.0625 0.125 255.0)
           (-> [0.75 0.5 0.5]
               (c/scale 255.0)
               (c/to-YCgCo)
               (c/scale (/ 255.0)))))))

;; chroma.js tests

(deftest chroma-example
  (is (= "#ff6d93" (-> :pink
                       (c/darken)
                       (c/saturate 2.0)
                       (c/format-hex)))))

(deftest chroma-api
  (is (= "#ff69b4" (c/format-hex :hotpink)))
  (is (= "#ff3399" (c/format-hex "#ff3399")))
  (is (= "#ff3399" (c/format-hex "F39")))
  (is (= "#ff3399" (c/format-hex 0xff3399)))
  (is (= "#ff3399" (c/format-hex (c/color 0xff 0x33 0x99))))
  (is (= "#ff3399" (c/format-hex (c/color 255.0 51.0 153.0))))
  (is (= "#ff3399" (c/format-hex [255 51 153])))
  (is (= "#ff3399" (c/format-hex (c/from-HSL [330.0 1.0 0.6]))))
  (is (= "#80ff80" (c/format-hex (c/from-HSL [120 1 0.75]))))
  (is (= "#85d4d5" (c/format-hex (c/from-LCH [80 25 200]))))
  (is (= "#aad28c" (c/format-hex (c/from-LCH [80 40 130]))))
  (is (= "#b400b4" (c/format-hex (c/mix :red :blue 0.5))))
  (is (= "#dd0080" (c/format-hex (c/mix :red :blue 0.25))))
  (is (= "#8000dd" (c/format-hex (c/mix :red :blue 0.75))))
  (is (= "#800080" (c/format-hex (c/lerp :red :blue))))
  (is (= "#00ff00" (c/format-hex (c/lerp :red :blue :HSL 0.5)))) ;; different than chroma, shortest path
  (is (= "#ca0088" (c/format-hex (c/lerp :red :blue :LAB 0.5)))))

(def average-colors ["#ddd", :yellow, :red, :teal])

(deftest chroma-average
  (is (= "#b79757" (c/format-hex (c/average average-colors))))
  (is (= "#d3a96a" (c/format-hex (c/average average-colors :LAB)))))

(deftest chroma-valid
  (is (c/valid-color? :red))
  (is (not (c/valid-color? :bread)))
  (is (c/valid-color? "#F0000D"))
  (is (not (c/valid-color? "#FOOOOD"))))

(deftest chroma-blend
  (is (= "#47af22" (c/format-hex (b/blend-colors b/multiply 0x4cbbfc 0xeeee22))))
  (is (= "#4cbb22" (c/format-hex (b/blend-colors b/darken 0x4cbbfc 0xeeee22))))
  (is (= "#eeeefc" (c/format-hex (b/blend-colors b/lighten 0x4cbbfc 0xeeee22)))))

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

(deftest chroma-color
  (is (= "#ff000080" (c/format-hex (c/set-alpha :red 128))))
  (is (= 128.0 (c/alpha (c/color 255.0 0.0 0.0 128.0))))
  (is (= "#c93384" (c/format-hex (c/darken :hotpink))))
  (is (= "#930058" (c/format-hex (c/darken :hotpink 2.0))))
  (is (= "#74003f" (c/format-hex (c/darken :hotpink 2.6))))
  (is (= "#ff9ce6" (c/format-hex (c/brighten :hotpink))))
  (is (= "#ffd1ff" (c/format-hex (c/brighten :hotpink 2.0))))
  (is (= "#ffffff" (c/format-hex (c/brighten :hotpink 3.0))))
  (is (= "#4b83ae" (c/format-hex (c/saturate :slategray))))
  (is (= "#0087cd" (c/format-hex (c/saturate :slategray 2.0))))
  (is (= "#008bec" (c/format-hex (c/saturate :slategray 3.0))))
  (is (= "#e77dae" (c/format-hex (c/desaturate :hotpink))))
  (is (= "#cd8ca8" (c/format-hex (c/desaturate :hotpink 2.0))))
  (is (= "#b199a3" (c/format-hex (c/desaturate :hotpink 3.0)))) ;; differs a little bit from original, due to rounding
  (is (= "#a10000" (c/format-hex (c/modulate :orangered :LAB 0 0.5))))
  (is (= "#63c56c" (c/format-hex (c/modulate :darkseagreen :LCH 1 2.0))))
  (is (= "#eb8787" (c/format-hex (c/set-channel :skyblue :HSL 0 0.0))))
  (is (= "#ce8ca9" (c/format-hex (c/set-channel :hotpink :LCH 1 30.0))))
  (is (= 57.58 (m/approx (c/get-channel :orangered :LAB 0))))
  (is (= 0.5 (m/approx (c/get-channel :orangered :HSL 2))))
  (is (= 69.0 (m/approx (c/get-channel :orangered 1))))
  (is (= 1.0 (/ (c/relative-luma :white) 255.0)))
  (is (= 0.81 (m/approx (/ (c/relative-luma :aquamarine) 255.0))))
  (is (= 0.35 (m/approx (/ (c/relative-luma :hotpink) 255.0))))
  (is (= 0.07 (m/approx (/ (c/relative-luma :darkslateblue) 255.0))))
  (is (= 0.0 (/ (c/relative-luma :black) 255.0))))

;; test iq palette generator

;; test paletton generator

;; test nearest color distance

;; Munsell (oracles generated in utils/munsell/, see clojure2d.color.munsell-test)

(defn- munsell-reference [name]
  (-> (str "munsell/" name) clojure.java.io/resource slurp clojure.edn/read-string))

(defn- max-abs-diff [as bs]
  (reduce max (map (fn [a b] (m/abs (- (double a) (double b)))) as bs)))

(deftest munsell-to-Yxy-test
  (is (= (mu/munsell-to-Yxy "4.2YR 8.1/5.3" {:whitepoint wp/CIE-2-D65})
         (c/munsell-to-Yxy "4.2YR 8.1/5.3")))
  (is (= (mu/munsell-to-Yxy [3.3 4.4 5.5 6] {:whitepoint wp/CIE-2-D65 :scale :newhall})
         (c/munsell-to-Yxy [3.3 4.4 5.5 6] {:scale :newhall})))
  (is (= (mu/munsell-to-Yxy "5R 5/4") (c/munsell-to-Yxy "5R 5/4" {:whitepoint wp/CIE-2-C}))))

(deftest munsell-to-sRGB-test
  (testing "colour Bradford C -> D65 XYZ through from-XYZ (sRGB matrix belongs to from-XYZ): 1e-9 on 0-255"
    (doseq [[h v ch code _ _ _ X Y Z] (:adapted-d65 (munsell-reference "colour_reference.edn"))]
      (is (< (max-abs-diff (c/from-XYZ (v/vec4 X Y Z 255.0)) (c/munsell-to-sRGB [h v ch code]))
             1e-9)
          [h v ch code])))
  (testing "munsellinterpol MunsellTosRGB, in-gamut grid points: 0.25 on 0-255 (its adaptation starts from NBS Illuminant C 0.3101/0.3163 and it averages neutrals; the D65/sRGB matrix convention accounts for at most 0.08)"
    (let [{:keys [srgb-names srgb]} (munsell-reference "munsellinterpol_reference.edn")]
      (doseq [[n rgb] (map vector srgb-names srgb)]
        (is (< (max-abs-diff rgb (take 3 (c/munsell-to-sRGB n))) 0.25) n))))
  (testing "not clamped, alpha 255, :whitepoint ignored"
    (let [rgb (c/munsell-to-sRGB "5PB 3/12")]
      (is (neg? (.x ^Vec4 rgb)))
      (is (== 255.0 (.w ^Vec4 rgb)))
      (is (= rgb (c/munsell-to-sRGB "5PB 3/12" {:whitepoint wp/CIE-2-C}))))))

(deftest XYZ-to-XYZ-adaptation-test
  ;; colour: Bradford C -> D65 with clojure2d white points; XYZ under C rebuilt from colour's xyY: 1e-10
  (let [d (munsell-reference "colour_reference.edn")
        xyY-C (into {} (map (fn [[h v ch code & xyY]] [[h v ch code] xyY])
                            (concat (:xyY-grid d) (:xyY-random d) (:xyY-edges d))))
        adapt (c/->XYZ-to-XYZ wp/CIE-2-C wp/CIE-2-D65)]
    (doseq [[h v ch code _ _ _ X Y Z] (:adapted-d65 d)
            :let [[x y Yc] (xyY-C [h v ch code])]
            :when (pos? Yc)]
      (is (< (max-abs-diff [X Y Z] (take 3 (adapt (v/vec4 (/ (* x Yc) y) Yc (/ (* (- 1.0 x y) Yc) y) 255.0))))
             1e-10)
          [h v ch code]))))

(deftest rainbow-gradient-range-test
  (let [g (c/gradient :rainbow)]
    (is (every? (fn [t] (every? #(<= 0.0 % 255.0) (take 3 (g (/ t 1000.0))))) (range 1001)))))

;; chromatic adaptation wrappers, oracle: test/resources/whitepoints (utils/whitepoints/gen_reference.py)

(def ^:private whitepoints-colour
  (delay (-> "whitepoints/colour_reference.edn" clojure.java.io/resource slurp clojure.edn/read-string)))

(defn- throws-method?
  [k f]
  (try (f) false
       (catch clojure.lang.ExceptionInfo e (= {:method k} (ex-data e)))))

(defn- throws-observer?
  [k f]
  (try (f) false
       (catch clojure.lang.ExceptionInfo e (= {:observer k} (ex-data e)))))

(deftest XYZ-to-XYZ-cat16-test
  ;; colour matrix_chromatic_adaptation_VonKries CAT16 C -> D65 applied to XYZ (0-100): 1e-12 relative to 100
  (let [M (partition 3 (get-in @whitepoints-colour [:adaptation [:cat16 :CIE-2-C :CIE-2-D65]]))
        adapt (c/->XYZ-to-XYZ :cat16 wp/CIE-2-C wp/CIE-2-D65)]
    (doseq [[X Y Z] (map (partial take 3) (get-in @whitepoints-colour [:lms :cat16]))
            :let [expected (map (fn [row] (reduce + (map * row [X Y Z]))) M)
                  ^Vec4 res (adapt (v/vec4 X Y Z 128.0))]]
      (is (< (max-abs-diff expected (take 3 res)) 1e-10) (str [X Y Z]))
      (is (== 128.0 (.w res)))))
  (is (throws-method? :nope #(c/->XYZ-to-XYZ :nope wp/CIE-2-C wp/CIE-2-D65))))

(deftest XYZ-LMS-test
  ;; LMS = Ma . XYZ vs colour, XYZ in 0-110: 1e-12; round trip 1e-12
  (doseq [[k rows] (:lms @whitepoints-colour)
          :let [->lms (c/->XYZ-to-LMS k)
                ->xyz (c/->LMS-to-XYZ k)]
          [X Y Z L M S] rows
          :let [^Vec4 lms (->lms (v/vec4 X Y Z 7.0))]]
    (is (< (max-abs-diff [L M S] (take 3 lms)) 1e-12) (str k [X Y Z]))
    (is (== 7.0 (.w lms)))
    (is (< (max-abs-diff [X Y Z] (take 3 (->xyz lms))) 1e-12) (str k [X Y Z])))
  (is (throws-method? :nope #(c/->XYZ-to-LMS :nope)))
  (is (throws-method? :nope #(c/->LMS-to-XYZ :nope))))

(deftest RGB-to-RGB-test
  ;; colour RGB_to_RGB with colourspaces built from clojure2d primaries, white points and transfer
  ;; functions (0-255): sRGB -> Adobe RGB 1998 (same white point), sRGB -> Adobe Wide Gamut RGB
  ;; (D65 -> D50, Bradford).
  ;; Linear target values (decoded with the target transfer function): 1e-12 on 0-255.
  ;; Encoded values: 1e-9, except channels below 1e-3 where the gamma slope (infinite at 0)
  ;; amplifies 1e-17 linear rounding noise to ~1e-5 (pure red/blue into Adobe RGB).
  (doseq [[[s t k] rows] (:rgb-to-rgb @whitepoints-colour)
          :let [f (c/->RGB-to-RGB s t k)
                linear (c/->RGB-to-linear-RGB t)]
          [r g b & expected] rows
          :let [actual (f (v/vec4 r g b 255.0))]]
    (is (< (max-abs-diff (take 3 (linear (apply v/vec4 (concat expected [255.0])))) (take 3 (linear actual))) 1e-12)
        (str [s t r g b]))
    (is (every? (fn [[e a]] (or (< (abs (double e)) 1e-3) (< (abs (- (double e) (double a))) 1e-9)))
                (map vector expected actual))
        (str [s t r g b])))
  (testing "default method is :bradford"
    (let [c (v/vec4 10.0 120.0 230.0 255.0)]
      (is (= ((c/->RGB-to-RGB :sRGB :Adobe-Wide-Gamut-RGB :bradford) c)
             ((c/->RGB-to-RGB :sRGB :Adobe-Wide-Gamut-RGB) c)))))
  (is (throws-method? :nope #(c/->RGB-to-RGB :sRGB :Adobe-Wide-Gamut-RGB :nope))))

;; spectral wrappers, oracle: test/resources/whitepoints (utils/whitepoints/gen_reference.py)

(deftest spectrum-aliases-test
  (is (identical? wp/spectrum c/spectrum))
  (is (identical? wp/->spectrum-to-XYZ1 c/->spectrum-to-XYZ1)))

(deftest spectrum-to-XYZ-test
  ;; 0-100 scale of ->spectrum-to-XYZ1: 1e-12 relative
  (let [s (c/spectrum (range 380 781 5) (map #(+ 0.2 (* 0.5 (m/sin (/ % 40.0)))) (range 380 781 5)))]
    (doseq [o [:CIE-2 :CIE-2015-10]
            i [:D65 :A :FL2]
            :let [^Vec4 res ((c/->spectrum-to-XYZ o i) s)
                  expected ((c/->spectrum-to-XYZ1 o i) s)]]
      (is (< (max-abs-diff (map #(* 100.0 %) expected) (take 3 res)) 1e-12) (str [o i]))
      (is (== 255.0 (.w res)))))
  (is (< (abs (- 100.0 (.y ^Vec4 ((c/->spectrum-to-XYZ) (c/spectrum (range 400 701 10) (repeat 31 1.0)))))) 1e-10)))

(deftest wavelength-to-XYZ-test
  ;; colour wavelength_to_XYZ for all four observers (B3: non :CIE-2 observers used CIE-10): 1e-10 on 0-100
  (doseq [[k rows] (:wavelength @whitepoints-colour)
          [l & XYZ] rows]
    (is (< (max-abs-diff (map #(* 100.0 %) XYZ) (take 3 (c/wavelength-to-XYZ l k))) 1e-10) (str k " " l)))
  (is (= (c/wavelength-to-XYZ 550.0 :CIE-2) (c/wavelength-to-XYZ 550.0)))
  (is (not= (c/wavelength-to-XYZ 550.0 :CIE-10) (c/wavelength-to-XYZ 550.0 :CIE-2015-10)))
  (is (= [0.0 0.0 0.0] (take 3 (c/wavelength-to-XYZ 385.0 :CIE-2015-2))))
  (is (= [0.0 0.0 0.0] (take 3 (c/wavelength-to-XYZ 900.0))))
  (is (throws-observer? :nope #(c/wavelength-to-XYZ 550.0 :nope))))

(deftest wavelength-test
  ;; ripple of wavelength-to-XYZ: sRGB of the colour XYZ, clamped
  (doseq [k [:CIE-2 :CIE-10 :CIE-2015-2]
          [l X Y Z] (take 20 (get-in @whitepoints-colour [:wavelength k]))]
    (is (< (max-abs-diff (take 3 (c/clamp (c/from-XYZ (v/vec4 (* 100.0 X) (* 100.0 Y) (* 100.0 Z) 255.0))))
                         (take 3 (c/wavelength l k)))
           1e-9)
        (str k " " l))))
