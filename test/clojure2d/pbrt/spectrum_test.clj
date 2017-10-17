(ns clojure2d.pbrt.spectrum-test
  (:require [clojure2d.pbrt.spectrum :refer :all]
            [expectations :refer :all]
            [clojure2d.math :as m]))

;; blackbody

(defn err
  "Relative error"
  [^double v ^double r]
  (/ (m/abs (- v r)) r))

(expect (approximately 0.0) (err (first (blackbody (double-array [483]) 6000)) 3.1849e13))
(expect (approximately 0.0) (err (first (blackbody (double-array [600]) 6000)) 2.86772e13))
(expect (approximately 0.0) (err (first (blackbody (double-array [500]) 3700)) 1.59845e12))
(expect (approximately 0.0) (err (first (blackbody (double-array [600]) 4500)) 7.46497e12))

(defn wiens-displacement-law
  ""
  [^double t]
  (let [lambda-max (* 1.0e9 (/ 2.8977721e-3 t))
        ^doubles res (blackbody (double-array [(* 0.999 lambda-max) lambda-max (* 1.001 lambda-max)]) t)]
    (and (< (aget res 0)
            (aget res 1))
         (> (aget res 1)
            (aget res 2)))))

(expect true (every? true? (map #(wiens-displacement-law %) (range 2700 6000 100))))
