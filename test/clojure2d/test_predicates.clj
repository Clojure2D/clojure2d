(ns clojure2d.test-predicates
  "Predicates shared by clojure2d tests: numeric closeness and `ex-info` data checks."
  (:require [fastmath.core :as m]))

(defn rel-close?
  "Is `a` within `tol` of `b`, scaled by `max(1, |b|)`? Equal values (also equal infinities) are close."
  [^double tol a b]
  (let [a (double a) b (double b)]
    (or (== a b)
        (m/<= (m/abs (m/- a b)) (m/* tol (m/max 1.0 (m/abs b)))))))

(defn throws-data?
  "Does calling `f` throw `ex-info` with exactly `m` as data?"
  [m f]
  (try (f) false
       (catch clojure.lang.ExceptionInfo e (= m (ex-data e)))))
