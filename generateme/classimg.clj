(ns classimg
  (:require [clojure2d.pixels :as p]
            [clojure2d.core :refer :all]
            [clojure2d.color :as c]
            [fastmath.classification :as cl]
            [fastmath.clustering :as clust]
            [fastmath.distance :as dist]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [fastmath.rbf :as rbf]
            [fastmath.random :as r]
            [clojure2d.extra.utils :as utl]))

(def img (p/load-pixels "generateme/blue.png"))
(def imgt (p/load-pixels "/mnt/hgfs/clojure-dropbox/mlimg/s.jpg"))
(def imgt img)

(utl/show-image img)

(def clusters (clust/k-means img 40))

(def labels (vec (:representatives clusters)))

(defn surrounding-idxs
  [^long x ^long y] 
  [[(dec x) (dec y)]
   [x (dec y)]
   [(inc x) (dec y)]
   [(dec x) y]
   ;; [(inc x) y]
   [(dec x) (inc y)]
   ;; [x (inc y)]
   ;; [(inc x) (inc y)]
   ;; [x y]
   ])

(defn color-vector
  [img ^long x ^long y]
  (map #(/ ^double % 255.0) (mapcat (fn [[x y]] (take 3 (p/get-color img x y))) (surrounding-idxs x y))))

(def samples (take 600 (map (fn [[^double x ^double y]]
                              (let [x (int (* x (width img)))
                                    y (int (* y (height img)))
                                    p (p/get-color img x y)
                                    l (labels (clusters p))]
                                [(color-vector img x y) l])) (r/sequence-generator :sobol 2))))


(def data (map first samples))
(def data-labels (map second samples))

(def cl (cl/lda {:max-nodes 10 :number-of-trees 10} data data-labels))

rbf/rbfs-list

(def c (canvas (width imgt) (height imgt)))
(def p (p/clone-pixels imgt))
(show-window {:canvas c
              :draw-fn (fn [c _ _ _]
                         (p/set-canvas-pixels! c p))})

dist/distance-names

(p/set-color p 600 600 :white)
;; => #object[clojure2d.pixels.Pixels 0x4ee4cd85 "pixels (600, 679)"]
;; => #object[clojure2d.pixels.Pixels 0x4ee4cd85 "pixels (600, 679)"]

(dotimes [i 1]
  (dotimes [y (height imgt)]
    (dotimes [x (width imgt)]
      (let [col (cl (color-vector p x y))]
        (p/set-color p x y col)))))

(save p "/mnt/hgfs/clojure-dropbox/mlimg/sada2.jpg")

(color-vector img 1000 1000)

(clusters (first img))

(def mapx (m/make-norm 0 (width img) 0 1))
(def mapy (m/make-norm 0 (height img) 0 1))
(def mapxt (m/make-norm 0 (width imgt) 0 1))
(def mapyt (m/make-norm 0 (height imgt) 0 1))

(defn convert-color
  [img mx my x y]
  (let [col (p/get-color img x y)]
    [(mx x) (my y) (/ (c/red col) 255.0) (/ (c/green col) 255.0) (/ (c/blue col) 255.0)]))

(def sampl (for [x (range 0 (width img) 10)
                 y (range 0 (height img) 10)]
             [(mapx x) (mapy y) (p/get-color img x y)]))

(def labels (map #(drop 2 %) sampl))

(def cl (cl/lda (map #(take 2 %) sampl) labels))


(def c (canvas (width imgt) (height imgt)))

(with-canvas [c c]
  (set-background c 50 150 50)
  (dotimes [x (width imgt)]
    (dotimes [y (height imgt)]
      (let [col (cl [(mapxt x) (mapyt y)])]
        (set-color c col)
        (rect c x y 1 1)))))

(show-window {:canvas c})

(defn- labels-converters
  "Convert y into label->int and int->label functions."
  [y]
  (let [ydata (mapv vector (sort (distinct y)) (range))]
    [(mapv first ydata) (into {} ydata)]))

(labels-converters [1 2 3])

(sort (distinct labels))

(instance?  1)
