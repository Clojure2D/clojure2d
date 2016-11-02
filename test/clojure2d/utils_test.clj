(ns clojure2d.utils-test
  (:import [java.awt Color])
  (:require [clojure.test :refer :all]
            [clojure2d.utils :refer :all]))

(deftest test-repeat-val
  (testing "various types"
    (is (= "0000" (repeat-val 0 4)))
    (is (= "0000" (repeat-val "0" 4)))
    (is (= "0000" (repeat-val \0 4))))
  (testing "nil values"
    (is (= "" (repeat-val nil 4))))
  (testing "longer string value"
    (is (= "121212" (repeat-val 12 3)))
    (is (= "44334433" (repeat-val "4433" 2)))))

(deftest test-pad-left
  (testing "pad with zeros"
    (is (= "00abc" (pad-left "abc" 5 \0)))
    (is (= "abc" (pad-left "abc" 2 "0"))))
  (testing "default padding with space"
    (is (= "abc" (pad-left "abc" 2 )))
    (is (= "  abc" (pad-left "abc" 5)))))

(deftest test-hex
  (testing "simple conversion"
    (is (= "1" (to-hex 1)))
    (is (= "A" (to-hex 10))))
  (testing "with padding"
    (is (= "000A" (to-hex 10 4))))
  (testing "color conversion"
    (is (= "FFFA141D" (to-hex (.getRGB (Color. 250 20 29)))))
    (is (= "20202" (to-hex (.getRGB (Color. 2 2 2 0)))))))
