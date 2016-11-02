(ns clojure2d.extra.filters
  (:require [clojure2d.math :as m]))

(defn lowpass-filter
  ""
  [alpha [sample prev]]
  (let [s1 (* sample alpha)
        s2 (- prev (* prev alpha))
        nprev (+ s1 s2)]
    [nprev nprev]))

(defn highpass-filter
  ""
  [lpfilter [sample prev]]
  (let [[res rprev] (lpfilter [sample prev])]
    [(- sample res) rprev]))

(defn- calc-filter-alpha
  ""
  [rate cutoff]
  (let [tinterval (/ 1.0 rate)
        tau (/ 1.0 (* cutoff m/TWO_PI))]
    (/ tinterval (+ tau tinterval))))

(defn make-lowpass-filter
  ""
  [conf]
  (partial lowpass-filter (calc-filter-alpha (conf :rate) (conf :cutoff))))

(defn make-highpass-filter
  ""
  [conf]
  (partial highpass-filter (make-lowpass-filter conf)))
