(ns clojure2d.core.shape-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure2d.core :as c2d]
            [clojure2d.core.shape :as sh])
  (:import [java.awt.geom Path2D PathIterator]))

(defn- segments
  "`[[type [coords ..]] ..]` of the path `p`."
  [^Path2D p]
  (let [it (.getPathIterator p nil)
        buf (double-array 6)]
    (loop [acc []]
      (if (.isDone it)
        acc
        (let [t (.currentSegment it buf)
              n ({PathIterator/SEG_MOVETO 2 PathIterator/SEG_LINETO 2 PathIterator/SEG_CUBICTO 6 PathIterator/SEG_CLOSE 0} t)]
          (.next it)
          (recur (conj acc [t (vec (take n buf))])))))))

(defn- end-points [p] (mapv (fn [[_ cs]] (vec (take-last 2 cs))) (segments p)))

(deftest path-bezier-test
  (testing "passes through every point"
    (is (= [[0.0 0.0] [10.0 5.0] [20.0 0.0] [30.0 5.0]]
           (end-points (sh/path-bezier [[0 0] [10 5] [20 0] [30 5]])))))
  (testing "small point counts: 0 and 1 empty, 2 a line, 3 curves through all"
    (is (empty? (segments (sh/path-bezier []))))
    (is (empty? (segments (sh/path-bezier [[1 1]]))))
    (is (= [[PathIterator/SEG_MOVETO [0.0 0.0]] [PathIterator/SEG_LINETO [10.0 10.0]]]
           (segments (sh/path-bezier [[0 0] [10 10]]))))
    (is (= [[0.0 0.0] [10.0 5.0] [20.0 0.0]] (end-points (sh/path-bezier [[0 0] [10 5] [20 0]])))))
  (testing "shape doesn't depend on the position (control points move with the points)"
    (let [pts [[0 0] [10 5] [20 0] [30 5]]
          moved (mapv (fn [[x y]] [(+ x 1000) (+ y 500)]) pts)
          shift (fn [cs] (mapv (fn [i v] (- v (if (even? i) 1000.0 500.0))) (range) cs))]
      (is (every? true? (map (fn [[_ a] [_ b]] (every? #(< (Math/abs (double %)) 1e-9) (map - a (shift b))))
                             (segments (sh/path-bezier pts)) (segments (sh/path-bezier moved)))))))
  (testing "coincident points give finite control points"
    (is (every? #(Double/isFinite %) (mapcat second (segments (sh/path-bezier [[0 0] [0 0] [10 10] [20 0]]))))))
  (testing "closed path needs 4 points"
    (is (empty? (segments (sh/path-bezier [[0 0] [10 0] [5 5]] true))))
    (is (= [10.0 0.0] (first (end-points (sh/path-bezier [[0 0] [10 0] [10 10] [0 10]] true)))))))

(defn- bounds [^java.awt.Shape s] (sh/rectangle2d->box (.getBounds2D s)))

(defn- close? [a b] (every? #(< (Math/abs (double %)) 1e-9) (map - a b)))

(deftest transform-shape-test
  (let [r (sh/rect 1 2 3 4)]
    (testing "operations compose like canvas transformations (later first)"
      (is (close? [12.0 24.0 6.0 8.0] (bounds (sh/transform-shape r [[:translate 10 20] [:scale 2]]))))
      (is (close? [22.0 44.0 6.0 8.0] (bounds (sh/transform-shape r [[:scale 2] [:translate 10 20]])))))
    (testing "same as an AffineTransform"
      (let [t (doto (java.awt.geom.AffineTransform.) (.translate 10.0 20.0) (.scale 2.0 3.0) (.rotate 0.5) (.shear 0.1 0.2))]
        (is (= (segments (sh/transform-shape r t))
               (segments (sh/transform-shape r [[:translate 10 20] [:scale 2 3] [:rotate 0.5] [:shear 0.1 0.2]]))))))
    (testing "no operation - identity; zero scale - degenerate"
      (is (= (bounds r) (bounds (sh/transform-shape r []))))
      (is (close? [0.0 0.0 0.0 0.0] (bounds (sh/transform-shape r [[:scale 0]])))))
    (testing "unknown operation"
      (is (thrown? IllegalArgumentException (sh/transform-shape r [[:skew 1 1]]))))))

(deftest rounded-rect-test
  (testing "one radius - the same path as RoundRectangle2D"
    (doseq [[x y w h r] [[20.3 15.7 100.0 60.0 10.0] [0.0 0.0 80.0 80.0 40.0] [1.0 1.0 60.0 30.0 4.5]]]
      (is (= (bounds (java.awt.geom.RoundRectangle2D$Double. x y w h (* 2.0 r) (* 2.0 r)))
             (bounds (sh/rounded-rect x y w h r))))
      (let [img (fn [s] (let [c (c2d/canvas 200 200)]
                          (c2d/with-canvas-> c (c2d/set-background :white) (c2d/set-color :black) (c2d/shape s))
                          (let [i ^java.awt.image.BufferedImage (c2d/get-image c)]
                            (vec (.getRGB i 0 0 200 200 nil 0 200)))))]
        (is (= (img (java.awt.geom.RoundRectangle2D$Double. x y w h (* 2.0 r) (* 2.0 r)))
               (img (sh/rounded-rect x y w h r)))))))
  (testing "radii per corner; a number equals four equal radii"
    (is (= (segments (sh/rounded-rect 0 0 10 10 2)) (segments (sh/rounded-rect 0 0 10 10 [2 2 2 2]))))
    (is (not= (segments (sh/rounded-rect 0 0 10 10 [2 0 0 0])) (segments (sh/rounded-rect 0 0 10 10 [0 2 0 0])))))
  (testing "zero radius - square corners; radii clamped to half the shorter side"
    (is (= [0.0 0.0 10.0 4.0] (bounds (sh/rounded-rect 0 0 10 4 0))))
    (is (= (segments (sh/rounded-rect 0 0 10 4 2)) (segments (sh/rounded-rect 0 0 10 4 100))))
    (is (= [[0.0 0.0] [10.0 0.0] [10.0 4.0] [0.0 4.0]]
           (->> (segments (sh/rounded-rect 0 0 10 4 0)) (map second) (remove empty?) (map #(vec (take-last 2 %))) distinct vec)))))

(deftest svg-path-test
  (testing "parses, non-zero by default"
    (let [s (sh/svg-path "M0 0 L10 0 L5 8 Z")]
      (is (= [0.0 0.0 10.0 8.0] (bounds s)))
      (is (= PathIterator/WIND_NON_ZERO (.getWindingRule (.getPathIterator s nil))))))
  (testing "even-odd"
    (is (= PathIterator/WIND_EVEN_ODD (.getWindingRule (.getPathIterator (sh/svg-path "M0 0 L10 0 L5 8 Z" :even-odd) nil)))))
  (testing "empty data - empty shape; bad data and bad winding throw"
    (is (empty? (segments (sh/svg-path ""))))
    (is (thrown? Exception (sh/svg-path "M0 0 Q")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown winding rule" (sh/svg-path "M0 0 L1 1" :odd)))
    (is (thrown? clojure.lang.ExceptionInfo (sh/svg-path "M0 0 L1 1" nil)))))

(deftest text-outline-test
  (let [font (java.awt.Font. "Dialog" java.awt.Font/PLAIN 100)]
    (testing "an outline at the baseline, wider for longer text"
      (let [[_ y w h] (bounds (sh/text-outline font "Hg"))
            [_ _ w1] (bounds (sh/text-outline font "Hgg"))]
        (is (neg? y))
        (is (pos? h))
        (is (< w w1))))
    (testing "fallback font only where the font can't show a character"
      (let [runs @#'sh/font-runs
            fallback (java.awt.Font. "Monospaced" java.awt.Font/PLAIN 100)
            emoji "a🐄b"]
        (is (= [[0 2 font]] (runs "ab" font fallback)))
        (is (= [[0 2 font]] (runs "ab" font nil)))
        (if (.canDisplay font (.codePointAt emoji 1))
          (is (= [[0 4 font]] (runs emoji font fallback)))
          (is (= [[0 1 font] [1 3 fallback] [3 4 font]] (runs emoji font fallback))))
        (is (= (vec (c2d/shape->path-def (sh/text-outline font "abc")))
               (vec (c2d/shape->path-def (sh/text-outline font "abc" fallback)))))))
    (testing "empty text - empty outline"
      (is (empty? (c2d/shape->path-def (sh/text-outline font "")))))))

(deftest rounded-rect-boundary-test
  (testing "negative radii - square corners; negative size - square corners"
    (is (= (segments (sh/rounded-rect 0 0 10 10 0)) (segments (sh/rounded-rect 0 0 10 10 -3))))
    (is (= [0.0 0.0 10.0 10.0] (bounds (sh/rounded-rect 0 0 10 10 [-1 -2 -3 -4]))))
    (is (= (segments (sh/rounded-rect 0 0 -10 10 0)) (segments (sh/rounded-rect 0 0 -10 10 2)))))
  (testing "point arity"
    (is (= (segments (sh/rounded-rect 1 2 10 10 3)) (segments (sh/rounded-rect [1 2] 10 10 3)))))
  (testing "a radius that isn't a number or four numbers"
    (doseq [r [nil [1 2 3] [1 2 3 4 5] [1 nil 2 3] :big]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Corner radius" (sh/rounded-rect 0 0 10 10 r)) (pr-str r)))))

(deftest winding-rules-test
  (is (= {:non-zero Path2D/WIND_NON_ZERO :even-odd Path2D/WIND_EVEN_ODD} sh/winding-rules)))
