(ns clojure2d.extra.signal-test
  (:require [clojure.test :refer :all])
  (:import [clojure2d.java.signal Converter]))

(def input (int-array (range 0 256)))
(def result (into [] input))

(defn- subsets
  [s]
  (if (empty? s)
    #{#{}}
    (let [ts (subsets (rest s))]
      (->> ts
           (map #(conj % (first s)))
           (clojure.set/union ts)))))

(defn- process-input
  [channels bits little-endian? signed? planar?]
  (let [target (int-array (range 0 256))
        channels (int-array channels)
        sig (clojure2d.java.signal.Converter/toSignal input channels bits little-endian? signed? planar? 1)]
    (into [] (clojure2d.java.signal.Converter/fromSignal sig target channels bits little-endian? signed? planar? 2))))

(deftest java-signal-converter
  (is (empty? (filter (comp not second) (for [channels (remove empty? (subsets #{0 1 2 3}))
                                              bits (range 3)
                                              little-endian? [true false]
                                              signed? [true false]
                                              planar? [true false]]
                                          [{:channels channels
                                            :bits bits
                                            :little-endian? little-endian?
                                            :signed? signed?
                                            :planar? planar?} (= result (process-input channels bits little-endian? signed? planar?))])))))
