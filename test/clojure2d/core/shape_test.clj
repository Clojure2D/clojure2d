(ns clojure2d.core.shape-test
  (:require [clojure.test :refer [deftest is testing]]
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
