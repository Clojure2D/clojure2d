(ns clojure2d.pixels-test
  (:require [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [fastmath.vector :as v]
            [clojure2d.core :refer :all]
            [clojure.test :refer :all])
  (:import [clojure2d.pixels Pixels]))

(def ^Pixels pixels (p/pixels 100 99))

(defn pixels-fixture
  [f]
  (p/set-color pixels 10 10 (c/color 100 200 10 22))
  (p/set-color pixels 222 (c/color 1 2 3 4))
  (p/set-value pixels 2 20 20 123)
  (p/set-value pixels 1 5555 30)
  (f))

(use-fixtures :once pixels-fixture)

;; protocol
(deftest get-colot-test
  (is (= (c/color 100 200 10 22) (p/get-color pixels 10 10)))
  (is (= (c/color 1 2 3 4) (p/get-color pixels 222)))
  (is (= (c/color 0 0 0 0) (p/get-color pixels 10 11))))

(deftest get-pixel-test
  (is (= 200 (p/get-value pixels 1 10 10)))
  (is (= 123 (p/get-value pixels 2 20 20)))
  (is (= 0 (p/get-value pixels 0 4444)))
  (is (= 30 (p/get-value pixels 1 5555))))

(deftest size-test
  (is (= (.size pixels) (* (.w pixels) (.h pixels))))  
  (is (= (* 4 (.size pixels)) (alength (.p pixels)))))

;; set/get channel

;; from/to planar

;; get/set pixels

;; filtering (use atom)

;; bin pixels
