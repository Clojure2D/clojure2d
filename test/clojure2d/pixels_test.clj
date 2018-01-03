(ns clojure2d.pixels-test
  (:require [clojure2d.pixels :as p]
            [clojure2d.color :as c]
            [clojure2d.math.vector :as v]
            [clojure2d.core :refer :all]
            [clojure.test :refer :all])
  (:import [clojure2d.pixels Pixels]))

(def ^Pixels pixels-planar (p/make-pixels 100 99 true))
(def ^Pixels pixels-interleaved (p/make-pixels 100 99 false))

(defn pixels-fixture
  [f]
  (p/set-color pixels-planar 10 10 (c/make-color 100 200 10 22))
  (p/set-color pixels-planar 222 (c/make-color 1 2 3 4))
  (p/set-value pixels-planar 2 20 20 123)
  (p/set-value pixels-planar 1 5555 30)
  (p/set-color pixels-interleaved 10 10 (c/make-color 100 200 10 22))
  (p/set-color pixels-interleaved 222 (c/make-color 1 2 3 4))
  (p/set-value pixels-interleaved 2 20 20 123)
  (p/set-value pixels-interleaved 1 5555 30)
  (f))

(use-fixtures :once pixels-fixture)

;; protocol

(deftest get-colot-test
  (is (= (c/make-color 100 200 10 22) (p/get-color pixels-planar 10 10)))
  (is (= (c/make-color 1 2 3 4) (p/get-color pixels-planar 222)))
  (is (= (c/make-color 0 0 0 0) (p/get-color pixels-planar 10 11)))
  (is (= (c/make-color 100 200 10 22) (p/get-color pixels-interleaved 10 10)))
  (is (= (c/make-color 1 2 3 4) (p/get-color pixels-interleaved 222)))
  (is (= (c/make-color 0 0 0 0) (p/get-color pixels-interleaved 10 11))))

(deftest get-pixel-test
  (is (= 200 (p/get-value pixels-planar 1 10 10)))
  (is (= 123 (p/get-value pixels-planar 2 20 20)))
  (is (= 0 (p/get-value pixels-planar 0 4444)))
  (is (= 30 (p/get-value pixels-planar 1 5555)))
  (is (= 200 (p/get-value pixels-interleaved 1 10 10)))
  (is (= 123 (p/get-value pixels-interleaved 2 20 20)))
  (is (= 0 (p/get-value pixels-interleaved 0 4444)))
  (is (= 30 (p/get-value pixels-interleaved 1 5555))))

(deftest idx->pos-test
  (is (= (v/vec2 23 1) (p/idx->pos pixels-planar 123)))
  (is (= (v/vec2 23 1) (p/idx->pos pixels-planar 123))))

(deftest position-test
  (is (= (+ 123 (* 100 99)) ((.pos pixels-planar) 1 123)))
  (is (= (+ 1 (* 4 123)) ((.pos pixels-interleaved) 1 123))))

(deftest planar-test
  (is (.planar? pixels-planar))
  (is (not (.planar? pixels-interleaved))))

(deftest size-test
  (is (= (.size pixels-planar) (* (.w pixels-planar) (.h pixels-planar))))
  (is (= (.size pixels-interleaved) (* (width pixels-interleaved) (height pixels-interleaved))))
  (is (= (* 4 (.size pixels-planar)) (alength (.p pixels-planar)))))

;; set/get channel

;; from/to planar

;; get/set pixels

;; filtering (use atom)

;; bin pixels
