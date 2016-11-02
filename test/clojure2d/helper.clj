(ns clojure2d.helper
  (:require [clojure.test :refer :all]
            [clojure2d.math :refer :all]))

(defn number-compare
  "safely compare double numbers"
  [v1 v2]
  (is (= (round (* 1000 v1)) (round (* 1000 v2)))))
