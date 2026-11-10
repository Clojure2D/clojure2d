(ns clojure2d.color.mixing-test
  "Mixing, averaging, nearest color and reduction of `clojure2d.color` (area F of the color audit), checked against
  `utils/color/gen_mixing.py`: chroma.js 2.6.0 (mix, average), coloraide 8.14.1 (hue interpolation, alpha),
  ProfJski ArtColors' subtractive mix and a numpy argmin; the author's designs as properties."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.random :as r]
            [clojure2d.color :as c]
            [clojure2d.test-predicates :refer [throws-data?]]))

(def reference (delay (-> "color/mixing_reference.edn" io/resource slurp edn/read-string)))

(defn- max-diff ^double [expected actual]
  (reduce max (map #(m/abs (m/- (double %1) (double %2))) expected actual)))

(defn- clip [col] (mapv #(m/constrain (double %) 0.0 255.0) (take 3 col)))

(defn- check-pairs
  "Compares `(f c1 c2 t)` with the oracle rows `[[rgb per t] per pair]`, both clipped to [0, 255] when `clip?`."
  [rows f tol clip?]
  (let [{:keys [colors pairs ts]} @reference]
    (doseq [[[i j] per-t] (map vector pairs rows)
            [t e] (map vector ts per-t)
            :let [a (f (colors i) (colors j) t)]]
      (is (m/<= (max-diff (if clip? (clip e) e) (if clip? (clip a) (take (count e) a))) tol)
          (str (colors i) " " (colors j) " " t)))))

(defn- check-sets
  [rows f tol clip?]
  (let [{:keys [colors sets]} @reference]
    (doseq [[[s w] e] (map vector (map vector sets (:weights @reference)) rows)
            :let [a (f (map colors s) w)]]
      (is (m/<= (max-diff (if clip? (clip e) e) (if clip? (clip a) (take (count e) a))) tol) (str s)))))

(deftest chroma-mix-test
  (let [{:keys [mix]} (:chroma @reference)]
    (testing "mix: square-root mixing (chroma.js default lrgb)"
      (check-pairs (:lrgb mix) c/mix 1.0e-9 false))
    (testing "lerp: chroma.js rgb"
      (check-pairs (:rgb mix) c/lerp 1.0e-9 false))
    (testing "lerp in LAB: chroma.js lab (its own D65 white), clipped"
      (check-pairs (:lab mix) #(c/lerp %1 %2 :LAB %3) 2.0e-3 true))))

(deftest chroma-average-test
  (let [{:keys [average weighted]} (:chroma @reference)]
    (testing "average: chroma.js rgb"
      (check-sets (:rgb average) (fn [cs _] (c/average cs)) 1.0e-9 false))
    (testing "average in LAB: chroma.js lab, clipped"
      (check-sets (:lab average) (fn [cs _] (c/average cs :LAB)) 2.0e-3 true))
    (testing "weighted-average: chroma.js rgb with weights"
      (check-sets weighted c/weighted-average 1.0e-9 false))
    (testing "the default is not chroma.js's default (lrgb)"
      (is (m/< 10.0 (max-diff (first (:lrgb average)) (c/average (map (:colors @reference) (first (:sets @reference))))))))))

(deftest straight-alpha-test
  (testing "lerp interpolates channels and alpha separately (coloraide srgb, premultiplied=False)"
    (let [{:keys [alpha-pairs alpha-mix]} (:coloraide @reference)]
      (doseq [[[a b] per-t] (map vector alpha-pairs (get-in alpha-mix [:RGB :straight]))
              [t e] (map vector (:ts @reference) per-t)]
        (is (m/<= (max-diff e (c/lerp a b t)) 1.0e-9) (str a " " b " " t))))))

(deftest nearest-color-test
  (let [{:keys [colors nearest]} @reference
        pal (mapv v/vec4 (:palette nearest))]
    (doseq [[col i] (map vector colors (:index nearest))]
      (is (= (pal i) (c/nearest-color pal (conj col 255.0))) (str col))
      (is (= (pal i) ((c/nearest-color pal) (conj col 255.0))) (str col)))))

(deftest negate-test
  (doseq [col (take 20 (:colors @reference))]
    (is (m/<= (max-diff (take 3 ((c/invert 1.0) col)) (take 3 (c/negate col))) 1.0e-9) (str col)))
  (is (= [-45.0 255.0 0.0 100.0] (vec (c/negate [300 0 255 100]))) "not clamped, alpha kept")
  (is (= [55.0 255.0 0.0 155.0] (vec (c/negate [200 0 255 100] true))) "alpha negated"))

;; G2: contracts and bugs

(deftest lerp+-test
  (doseq [[a b] [[:red :blue] [:yellow :navy] [[30 60 200] [250 240 10]]]]
    (is (= (c/lerp+ a b) (c/lerp+ a b 0.5)) (str a " " b))
    (is (m/< (m/abs (m/- ((c/to-LAB b) 0) ((c/to-LAB (c/lerp+ a b)) 0))) 1.0e-9) (str a " " b))
    (is (m/< (m/abs (m/- ((c/to-LAB a) 0) ((c/to-LAB (c/lerp- a b)) 0))) 1.0e-9) (str a " " b))))

(deftest nearest-color-contract-test
  (testing "entries of any color type, returned as given"
    (is (= :red (c/nearest-color [:red :blue] :pink)))
    (is (= [0 0 255] (c/nearest-color [[255 0 0] [0 0 255]] :navy)))
    (is (= "#00ff00" (c/nearest-color [:red "#00ff00"] [10 200 10] c/delta-E*))))
  (testing "1-arity: the same result, first entry on ties"
    (let [pal (map #(vector % (- 255 %) 100) (range 0 256 5))
          f (c/nearest-color pal)]
      (doseq [r (range 0 256 26) g (range 0 256 26) b (range 0 256 26)
              :let [col [r g b]]]
        (is (= (c/nearest-color pal col) (f col)) (str col)))
      (is (= [10 10 10] (c/nearest-color [[10 10 10] [10 10 10 255]] [10 10 10])))))
  (testing "empty palette"
    (is (throws-data? {:palette-size 0} #(c/nearest-color [] :red)))
    (is (throws-data? {:palette-size 0} #(c/nearest-color [])))))

(deftest transparency-test
  (is (= 0.0 ((c/mixbox [0 0 255 0] [255 255 0 0]) 3)) "mixbox: transparent stays transparent")
  (is (= [78.0 150.0 100.0 255.0] (vec (c/mixbox :blue :yellow))) "mixbox: pigment green")
  (is (= 102.0 ((c/mixbox [0 0 255 51] [255 255 0 153]) 3)))
  (let [cols (concat (repeat 5 [255 0 0 100]) (repeat 5 [250 0 0 100]) (repeat 5 [0 0 255 0]) (repeat 3 [0 0 250 0]))
        r (c/reduce-colors cols 2 {:fuzzy? false})]
    (is (= #{[0.0 0.0 255.0 0.0] [252.5 0.0 0.0 100.0]} (set (map vec r))) "reduce-colors: modes, alpha kept, ties averaged")))

(deftest colorspace-errors-test
  (doseq [f [#(c/average [:red] :Foo) #(c/weighted-average [:red] [1] :Foo) #(c/lerp :red :blue :Foo 0.5)
             #(c/lerp- :red :blue :Foo 0.5) #(c/lerp+ :red :blue :Foo 0.5) #(c/mix :red :blue :Foo 0.5)
             #(c/mixsub :red :blue :Foo 0.5) #(c/reduce-colors [[1 2 3 255] [4 5 6 255]] 1 {:colorspace :Foo})]]
    (is (throws-data? {:colorspace :Foo} f))))

(deftest average-errors-test
  (is (throws-data? {:colors 0} #(c/average [])))
  (is (throws-data? {:colors 0} #(c/average [] :LAB)))
  (is (throws-data? {:colors 0} #(c/weighted-average [] [])))
  (is (throws-data? {:colors 2 :weights 1} #(c/weighted-average [:red :blue] [1])))
  (is (throws-data? {:weights-sum 0.0} #(c/weighted-average [:red :blue] [1 -1])))
  (is (throws-data? {:weights-sum ##Inf} #(c/weighted-average [:red :blue] [1 ##Inf])))
  (is (= [510.0 0.0 -255.0 255.0] (vec (c/weighted-average [:red :blue] [2 -1]))) "negative weights allowed"))

(deftest resample-test
  (is (= 3 (count (c/resample [:red :blue] 3))))
  (is (throws-data? {:number-of-colors 0} #(c/resample [:red :blue] 0)))
  (is (throws-data? {:number-of-colors -2} #(c/resample [:red :blue] -2 {}))))

;; G3: values

(def ^:private coloraide-spaces [:HSL :HSV :HWB :LCH :Oklch])

(def ^:private coloraide-tolerance
  ;; compared clipped: out of gamut, coloraide extends the sRGB curve symmetrically, clojure2d linearly (area B);
  ;; LCH: coloraide lch-d65 builds XYZ from its own sRGB matrix (D65 0.3127, 0.3290), clojure2d from CIE-2-D65
  {:HSL 1.0e-9 :HSV 1.0e-9 :HWB 1.0e-9 :LCH 0.1 :Oklch 1.0e-6})

(deftest hue-lerp-test
  (testing "shorter arc, powerless hues (coloraide mix, straight alpha)"
    (doseq [cs coloraide-spaces]
      (check-pairs (get-in @reference [:coloraide :mix cs]) #(c/lerp %1 %2 cs %3) (coloraide-tolerance cs) true)))
  (testing "chroma.js hsl mix (shorter arc, gray hue undefined): hsl2rgb rounds to integers; skipped: pairs with white or
            black (chroma.js keeps the saturation of the other color, coloraide and clojure2d interpolate it)"
    (let [{:keys [colors pairs ts]} @reference
          bw #{[255.0 255.0 255.0] [0.0 0.0 0.0]}]
      (doseq [[[i j] per-t] (map vector pairs (get-in @reference [:chroma :mix :hsl]))
              :when (not (or (bw (colors i)) (bw (colors j))))
              [t e] (map vector ts per-t)]
        (is (m/<= (max-diff (clip e) (clip (c/lerp (colors i) (colors j) :HSL t))) 0.5)
            (str (colors i) " " (colors j) " " t)))))
  (testing "hue wrapped into the range of the colorspace (every hue space)"
    (doseq [[cs {:keys [hue lo period] :as spec}] @#'c/hue-spaces
            :let [[to] (c/colorspaces cs)]
            [c1 c2] [[[255 0 30] [200 0 255]] [[20 200 30] [250 240 10]] [[0 0 255] [255 0 0]]]
            t [0.1 0.5 0.9 1.4 -0.3]
            :let [h ((#'c/hue-lerp spec (to c1) (to c2) t) hue)]]
      (is (and (m/<= lo h) (m/< h (+ lo period))) (str cs " " c1 " " c2 " " t " " h))))
  (testing "lerp- and lerp+ use the same path"
    (is (m/< (max-diff (take 3 (c/lerp- :red :blue :HSL 0.5))
                       (take 3 (c/set-channel (c/lerp :red :blue :HSL 0.5) :LAB 0 ((c/to-LAB :red) 0)))) 1.0e-9))))

(deftest hue-average-test
  (testing "circular mean, cancelling hues achromatic (coloraide average)"
    (doseq [cs coloraide-spaces]
      (check-sets (get-in @reference [:coloraide :average cs]) (fn [cols _] (c/average cols cs)) (coloraide-tolerance cs) true)
      (check-sets (get-in @reference [:coloraide :weighted-average cs]) #(c/weighted-average %1 %2 cs) (coloraide-tolerance cs) true)))
  (testing "chroma.js hsl average: hsl2rgb rounds to integers; skipped: sets starting with an achromatic color (chroma.js
            counts its undefined hue as 0) and the cancelling red + cyan (chroma.js keeps a noise hue)"
    (let [{:keys [colors sets]} @reference]
      (doseq [[s e] (map vector sets (get-in @reference [:chroma :average :hsl]))
              :when (and (not= s [0 4]) (not (zero? ((c/to-HSL (colors (first s))) 1))))]
        (is (m/<= (max-diff (clip e) (clip (c/average (map colors s) :HSL))) 0.5) (str s)))))
  (testing "red + cyan in HSL: gray"
    (is (m/< (max-diff [127.5 127.5 127.5] (c/average [:red :cyan] :HSL)) 1.0e-9))))

(deftest mix-colorspace-test
  (doseq [cs [:LAB :Oklab :HSL :LCH]]
    (is (= (c/lerp :red :blue cs 0.3) (c/mix :red :blue cs 0.3)) (str cs)))
  (is (= (c/mix :red :blue 0.3) (c/mix :red :blue :RGB 0.3))))

(deftest mixsub-test
  (check-pairs (:mixsub @reference) c/mixsub 1.0e-9 false)
  (is (= 255.0 ((c/mixsub [255 0 0 10] [0 0 255 0]) 3)) "alpha 255"))

(deftest reduce-colors-test
  (let [vir (c/palette :viridis)]
    (testing "k-means++ default; a seeded RNG repeats; nil RNG"
      (is (= (c/reduce-colors vir 4 {:fuzzy? false :rng (r/rng :mersenne 5)}) (c/reduce-colors vir 4 {:rng (r/rng :mersenne 5)})))
      (is (= (c/reduce-colors vir 4 {:rng (r/rng :mersenne 1)}) (c/reduce-colors vir 4 {:rng (r/rng :mersenne 1)})))
      (is (= 4 (count (c/reduce-colors vir 4 {:rng nil})))))
    (testing "any color type"
      (is (= 2 (count (c/reduce-colors (concat (repeat 10 :red) (repeat 10 "#0000ff") [:yellow :green]) 2)))))
    (testing "no more distinct colors than clusters: returned as they are"
      (is (= [[10.0 20.0 30.0 255.0]] (mapv vec (c/reduce-colors (repeat 50 [10 20 30]) 3))))
      (is (= #{[255.0 0.0 0.0 255.0] [0.0 0.0 255.0 255.0]} (set (map vec (c/reduce-colors [:red :blue :red] 2 {:fuzzy? true})))))
      (is (= 1 (count (c/reduce-colors (repeat 50 [10 20 30]) 3 {:colorspace :LAB})))))
    (testing "fuzzy k-means still available, sorted by luma"
      (let [r (c/reduce-colors vir 3 {:fuzzy? true :rng (r/rng :mersenne 3)})]
        (is (= 3 (count r)))
        (is (apply <= (map c/luma r)))))))

(deftest docstrings-test
  (doseq [s '[nearest-color average weighted-average lerp lerp- lerp+ mix mixbox mixsub negate reduce-colors resample]]
    (is (some? (:doc (meta (ns-resolve 'clojure2d.color s)))) (str s))))
