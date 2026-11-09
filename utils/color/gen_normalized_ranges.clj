;; Sampled extrema of the raw colorspace conversions (area B, item 4.6 of the color audit).
;;
;; Run from the repository root:
;;
;;     lein run -m clojure.main utils/color/gen_normalized_ranges.clj
;;
;; Samples: a 64^3 grid of sRGB colors (step 255/63) plus 256 points on each of the 12 edges of the RGB cube.
;; For every colorspace prints the minimum and maximum of each channel of the raw `to-` conversion and of
;; the normalized `to-*` conversion. The normalization literals in `clojure2d.color` are set from the raw
;; extrema (OSA j and g: extrema of their triple cube root).

(require '[clojure2d.color :as c] '[fastmath.core :as m])

(def grid
  (let [steps (map #(* 255.0 (/ % 63.0)) (range 64))
        edge (map #(* 255.0 (/ % 255.0)) (range 256))
        edges (for [t edge [a b] [[0.0 0.0] [0.0 255.0] [255.0 0.0] [255.0 255.0]]
                    p [[t a b] [a t b] [a b t]]]
                p)]
    (vec (concat (for [r steps g steps b steps] [r g b]) edges))))

(defn extrema [f]
  (reduce (fn [[mn mx] col]
            (let [v (f (c/color col))]
              [(mapv min mn (take 3 v)) (mapv max mx (take 3 v))]))
          [[##Inf ##Inf ##Inf] [##-Inf ##-Inf ##-Inf]] grid))

(defn triple-cbrt ^double [^double x] (m/cbrt (m/cbrt (m/cbrt x))))

(doseq [k c/colorspaces-list :when (not= k :pass)
        :let [[to] (c/colorspaces k)
              [to*] (c/colorspaces* k)
              raw (extrema (if (= k :OSA)
                             (fn [col] (let [v (to col)] [(v 0) (triple-cbrt (v 1)) (triple-cbrt (v 2))]))
                             to))
              norm (extrema to*)]]
  (println (pr-str [k {:raw raw :normalized norm}])))
