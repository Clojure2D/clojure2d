(ns meta-doc.gen-images
  "Generate images from examples attached to metadata."
  (:require [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [clojure2d.math.joise :as j]
            [clojure2d.math.complex :as c]
            [clojure2d.extra.glitch :as g]
            [clojure2d.extra.signal :as s]
            [clojure2d.extra.overlays :as o]
            [clojure2d.extra.variations :as v]
            [clojure2d.extra.raymarching :as r]
            [clojure2d.extra.segmentation :as segm]))

(def ^:dynamic *generate-images* false)

(defn draw-example
  "Draw example on canvas."
  [f]
  (with-canvas [c (make-canvas 160 160)]
    (set-background c 0x30426a)
    (f c)))

(defn save-example
  "Save generated image to file."
  [name img]
  (binding [*jpeg-image-quality* 0.7]
    (save img (str "docs/images/" name ".jpg"))))

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
  [{:keys [digest value-fn]}]
  (println digest)
  (save-example digest (draw-example value-fn)))

(defn generate-images
  "Process all examples from all loaded namespaces."
  []
  (when *generate-images* 
    (doseq [example (->> (get-all-clojure2d-ns)
                         (mapcat get-examples-from-vars)
                         (apply concat))]
      (generate-image example))))

(generate-images)
