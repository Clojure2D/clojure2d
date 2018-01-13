(ns meta-doc.gen-images
  "Generate images from examples attached to metadata."
  (:require [clojure2d.core :refer :all]
            [clojure2d.color :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.math :as m]
            [clojure2d.math.joise :as j]
            [clojure2d.math.complex :as c]
            [clojure2d.extra.glitch :as g]
            [clojure2d.extra.signal :as s]
            [clojure2d.extra.overlays :as o]
            [clojure2d.extra.variations :as var]
            [clojure2d.math.vector :as vec]
            [clojure2d.extra.raymarching :as r]
            [clojure2d.extra.segmentation :as segm]))

(def ^:dynamic *generate-images* false)

(defn draw-example
  "Draw example on canvas."
  ([f] (draw-example f {}))
  ([f {:keys [w h hints background]
       :or {w 160 h 160 hints :high background 0x30426a}}]
   (with-canvas [c (make-canvas w h hints)]
     (set-background c background)
     (f c)
     c)))

(defn save-example
  "Save generated image to file."
  [name img]
  (binding [*jpeg-image-quality* 0.85]
    (save img (str "docs/images/" name))))

(defn get-all-clojure2d-ns
  "Return all namespaces from clojure2d."
  [] 
  (->> (all-ns)
       (map ns-name)
       (filter #(re-matches #".*clojure2d.*" (str %)))
       (map the-ns)))

(defn get-examples-from-vars
  "Return all examples from metatags"
  [namespace]
  (->> (ns-publics namespace)
       (vals)
       (map meta)
       (map :examples)
       (filter (complement nil?))))

(defn generate-image
  "Generate image from example."
  [{:keys [filename value-fn params]}]
  (save-example filename (draw-example value-fn params)))

(defn generate-images
  "Process all examples from all loaded namespaces."
  []
  (when *generate-images* 
    (doseq [example (->> (get-all-clojure2d-ns)
                         (mapcat get-examples-from-vars)
                         (apply concat)
                         (filter #(= (:type %) :gen-image)))] 
      (generate-image example))))

(defn generate-math-graph
  "Generate math graphs for function"
  [f canvas]
  (set-color canvas 60 100 120)
  (set-stroke canvas 0.8)
  (line canvas 0 80 160 80)
  (line canvas 80 0 80 160)
  (set-color canvas :white 130)
  (set-stroke canvas 1.5)
  (dotimes [x 320]
    (let [xx (m/norm x 0 320 -3.2 3.2) 
          y (m/norm (f xx) 3.2 -3.2 0 160)]
      (point canvas (* 0.5 x) y))))

(defn generate-math-graphs
  "Generate graphs for math functions."
  [fs]
  (when *generate-images*
    (doseq [n fs]
      (let [fs (symbol n)
            f (eval `(fn [x#] (~fs x#)))
            c (draw-example (partial generate-math-graph f))]
        (save-example (str n ".png") c)))))

(def math-names ["m/sin" "m/cos" "m/tan" "m/cot" "m/sec" "m/csc" "m/asin" "m/acos" "m/atan" "m/acot" "m/asec" "m/acsc"
                 "m/sinh" "m/cosh" "m/tanh" "m/coth" "m/sech" "m/csch" "m/asinh" "m/acosh" "m/atanh" "m/acoth" "m/asech" "m/acsch"
                 "m/qsin" "m/qcos" "m/exp" "m/log" "m/log10" "m/ln" "m/sqrt" "m/cbrt" "m/qexp"
                 "m/erf" "m/erfc" "m/inv-erf" "m/inv-erfc" "m/sinc" "m/log2" "m/qlog"
                 "m/sq" "m/pow2" "m/pow3" "m/safe-sqrt" "m/floor" "m/ceil" "m/round" "m/rint" "m/abs" "m/iabs" "m/trunc"
                 "m/frac" "m/sfrac" "m/low-2-exp" "m/high-2-exp" "m/round-up-pow2" "m/next-float-up" "m/next-float-down"
                 "m/signum" "m/sgn"])

(generate-math-graphs math-names)

(generate-images)

