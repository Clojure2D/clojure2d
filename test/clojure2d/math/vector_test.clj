(ns clojure2d.math.vector-test
  (:require [clojure2d.math.vector :refer :all]
            [expectations :refer :all]
            [clojure2d.math :as m]))


;; test protocol

;; clojure vector
(expect [1 2] (to-vec [1 2]))
(expect [4 5] (applyf [3 4] inc))
(expect (approximately 2.0) (magsq [1 1]))
(expect (approximately (m/sqrt 2.0)) (mag [1 1]))
(expect (approximately 5.2) (dot [1 1 1 1 1.1] [1 1 1.1 1 1]))
(expect [0 1 2] (add [-1 -1 -1] [1 2 3]))
(expect [-1 -1 -1] (sub [0 1 2] [1 2 3]))
(expect [8.0 6.0 4.0 2.0 0.0] (mult [4 3 2 1 0] 2.0))
(expect [0 1.0 0] (emult [0 0.5 0] [4 2 44]))
(expect [0.25] (div [1] 4))
