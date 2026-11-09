(ns clojure2d.color.cssgram-test
  "CSSgram filters checked against `utils/blend/gen_cssgram.py`: the CSSgram source transcription
  (overlays, blend modes, opacity, filter lists) and the W3C Compositing Level 1 blend stage."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure2d.color :as c]
            [clojure2d.color.blend :as b]
            [clojure2d.color.cssgram :as g]
            [clojure2d.test-predicates :refer [rel-close? throws-data?]]))

(def reference (delay (-> "blend/cssgram_reference.edn" io/resource slurp edn/read-string)))

(defn- vec-close? [tol expected actual]
  (every? true? (map (partial rel-close? tol) expected actual)))

(deftest definitions-test
  ;; table = CSSgram source: same modes in stacking order, same filter list; solid overlay colors exact
  ;; (alpha = CSS alpha * opacity, 1e-12); gradient overlays marked as approximations
  (let [css (:definitions @reference)]
    (is (= (set (keys css)) (set (keys g/filter-definitions))))
    (doseq [[k {:keys [overlays filters]}] css
            :let [entry (g/filter-definitions k)]]
      (is (= (map :mode overlays) (map first (:overlays entry))) (str k))
      (is (= filters (:filters entry)) (str k))
      (is (= (boolean (some :gradient overlays)) (contains? entry :approximation)) (str k))
      (doseq [[{:keys [color opacity]} [_ table-color]] (map vector overlays (:overlays entry))
              :when color
              :let [[r gr b a] color]]
        (is (vec-close? 1.0e-12 [r gr b (* 255.0 a opacity)] (c/to-color table-color)) (str k))))))

(deftest earlybird-approximation-test
  (is (re-find #"0.15" (:approximation (g/filter-definitions :earlybird)))))

(deftest blend-stage-test
  ;; overlays of the exact filters vs W3C blending and compositing (numpy): 1e-9 relative
  (let [{:keys [colors blended]} @reference]
    (doseq [[k expected] blended
            :let [f (apply g/custom-filter (map (partial into [:blend]) (:overlays (g/filter-definitions k))))]
            [x e] (map vector colors expected)]
      (is (vec-close? 1.0e-9 e (f (c/color x))) (str k " " x)))))

(deftest filters-test
  (is (= 26 (count g/filters)))
  (is (= (sort (keys g/filter-definitions)) g/filters-list))
  (testing "public filter vars are the functions in filters"
    (doseq [k g/filters-list]
      (is (identical? (g/filters k) @(ns-resolve 'clojure2d.color.cssgram (symbol (name k))))
          (str k))))
  (testing "each filter = custom-filter of its definition"
    (doseq [[k {:keys [overlays filters]}] g/filter-definitions
            :let [f (apply g/custom-filter (concat (map (partial into [:blend]) overlays) filters))]
            x [(c/color 200 50 30) (c/color 10 120 220) (c/color 128 128 128)]]
      (is (= ((g/filters k) x) (f x)) (str k)))))

(deftest custom-filter-test
  (let [x (c/color 200 50 30)]
    (testing "K4: function steps compose with the steps before"
      (is (= ((c/contrast 1.5) x) ((g/custom-filter [:contrast 1.5] identity) x)))
      (is (= ((c/sepia 0.3) ((c/contrast 1.5) x)) ((g/custom-filter [:contrast 1.5] (c/sepia 0.3)) x))))
    (testing "K5: unknown operator or blend mode throws"
      (is (throws-data? {:operator :nope} #(g/custom-filter [:nope 1.0])))
      (is (throws-data? {:blend :nope} #(g/custom-filter [:blend :nope (c/color 0 0 0)])))
      (is (throws-data? {:blend nil} #(g/custom-filter [:blend nil (c/color 0 0 0)]))))
    (testing "blend steps: per-channel keyword or function, whole-color modes, channel copy with colorspace"
      (let [y (c/color 20 120 220 128)]
        (is (= (b/blend-colors b/screen x y) ((g/custom-filter [:blend :screen y]) x)))
        (is (= (b/w3c-hue x y) ((g/custom-filter [:blend :w3c-hue y]) x)))
        (is (= (b/ch0 :HSB x y) ((g/custom-filter [:blend :ch0 :HSB y]) x)))))
    (is (= x ((g/custom-filter) x)))))
