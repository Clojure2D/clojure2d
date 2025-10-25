(ns clojure2d.color.munsell
  (:require [fastmath.vector :as v]
            [fastmath.core :as m]
            [fastmath.polynomials :as poly])
  (:import [fastmath.vector Vec4]))

(def ^:private FP "[0-9]*\\.?[0-9]+")
(def ^:private GRAY (re-pattern (str "N(" FP ")") ))
(def ^:private MUNSELL
  (re-pattern (str "(" FP ")\\s*(BG|GY|YR|RP|PB|B|G|Y|R|P)\\s*(" FP ")\\s*/\\s*([-+]?" FP ")")))

(defn- parse-gray
  ^Vec4 [munsell]
  (when-let [v (second (re-find GRAY munsell))]
    (Vec4. ##NaN (read-string v) ##NaN ##NaN)))

(def ^:private letters->code {"BG" 2 "GY" 4 "YR" 6"RP" 8 "PB" 10 "B" 1 "G" 3 "Y" 5 "R" 7 "P" 9})

(defn- parse-color
  ^Vec4 [munsell]
  (when-let [[_ h l v c] (re-find MUNSELL munsell)]
    (let [chroma (read-string c)
          value (read-string v)]
      (if (m/zero? chroma)
        (Vec4. ##NaN value ##NaN ##NaN)
        (let [hue (read-string h)
              code (letters->code l)]
          (if (m/zero? hue)
            (Vec4. 10.0 value chroma (m/mod (m/inc code) 10.0))
            (Vec4. hue value chroma code)))))))

(defn- parse-munsell
  ^Vec4 [munsell]
  (or (parse-gray munsell)
      (parse-color munsell)))

(defn scale-value
  ^double [^double v]
  (poly/mevalpoly v 0.0 1.1914 -0.22533 0.23352 -0.020484 0.00081939))

(parse-munsell "0YR 2.3/2.2")
