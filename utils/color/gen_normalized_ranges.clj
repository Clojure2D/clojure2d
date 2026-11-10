;; Sampled extrema of the raw colorspace conversions (area B, item 4.6 of the color audit).
;;
;; Run from the repository root:
;;
;;     lein run -m clojure.main utils/color/gen_normalized_ranges.clj
;;
;; Samples: a 64^3 grid of sRGB colors (step 255/63) plus 256 points on each of the 12 edges of the RGB cube;
;; the 6 best samples of each extremum are refined by a pattern search in the continuous RGB cube (steps 16
;; down to 1e-10), so extrema between grid points (interior or on faces) are found as well.
;; For every colorspace prints the minimum and maximum of each channel of the raw `to-` conversion and of
;; the normalized `to-*` conversion. The normalization literals in `clojure2d.color` are set from the raw
;; extrema, except hue channels, which use the whole period (0-360 degrees, -180-180 for HCL, 0-1 for Okhsl,
;; Okhsv, Okhwb) and OSA j and g, unbounded near the surface Y0^(1/3) = 2/3 (dark colors), mapped by an
;; arctangent (no literals).

(require '[clojure2d.color :as c] '[fastmath.core :as m])

(def grid
  (let [steps (map #(* 255.0 (/ % 63.0)) (range 64))
        edge (map #(* 255.0 (/ % 255.0)) (range 256))
        edges (for [t edge [a b] [[0.0 0.0] [0.0 255.0] [255.0 0.0] [255.0 255.0]]
                    p [[t a b] [a t b] [a b t]]]
                p)]
    (vec (concat (for [r steps g steps b steps] [r g b]) edges))))

(defn- clamp-rgb [p] (mapv #(m/constrain (double %) 0.0 255.0) p))

(defn- refine
  "Pattern search maximizing `f` from `p0` in the RGB cube; returns the best value."
  ^double [f p0]
  (loop [p p0 v (double (f p0)) s 16.0]
    (if (< s 1.0e-10)
      v
      (let [[bv bp] (apply max-key first (for [i (range 3) d [s (- s)]
                                               :let [q (clamp-rgb (update p i + d))]]
                                           [(double (f q)) q]))]
        (if (> (double bv) v) (recur bp bv s) (recur p v (/ s 2.0)))))))

(defn- channel-extremum
  "Refined extremum of channel `ch` of `f` (`sign` 1 maximum, -1 minimum)."
  ^double [f ch ^double sign]
  (let [g (fn [p] (* sign (double ((f (c/color p)) ch))))
        best (take 6 (sort-by (comp - g) grid))]
    (* sign (double (reduce max (map #(refine g %) best))))))

(defn extrema [f]
  [(mapv #(channel-extremum f % -1.0) [0 1 2])
   (mapv #(channel-extremum f % 1.0) [0 1 2])])

(doseq [k c/colorspaces-list :when (not= k :pass)
        :let [[to] (c/colorspaces k)
              [to*] (c/colorspaces* k)
              raw (extrema to)
              norm (extrema to*)]]
  (println (pr-str [k {:raw raw :normalized norm}])))
