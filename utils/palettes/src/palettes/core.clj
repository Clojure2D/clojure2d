(ns palettes.core
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.xml :as xml]))

(require '[clojisr.v1.r :as r]
         '[tech.ml.dataset :as ds])

(r/discard-all-sessions)

(r/require-r '[paletteer :as p])

(def number-set (set "0123456789"))

(defn fix-name [s]
  (let [s (s/replace s #"\s" "-")]
    (if (number-set (first s))
      (str "_" s)
      s)))

(def all-palettes (atom {}))
(def all-gradients (atom {}))

;; gradients _c_

(doseq [[package palettes] (group-by :package (ds/->flyweight (r/r->clj p/palettes_c_names)))
        :let [fpackage (fix-name package)
              pals (into {} (map (fn [{:keys [palette]}]
                                   (let [k (keyword fpackage (fix-name palette))
                                         n (str package "::" palette)
                                         v (r/r->clj (p/paletteer_c n :n 128))]
                                     [k v]))) palettes)]]
  (swap! all-gradients merge pals)
  (spit (str "resources/gradients/c2d_" fpackage ".edn") (with-out-str (pr pals))))

;; palettes _d_

(doseq [[package palettes] (group-by :package (ds/->flyweight (r/r->clj p/palettes_d_names)))
        :let [fpackage (fix-name package)
              pals (into {} (map (fn [{:keys [palette]}]
                                   (let [k (keyword fpackage (fix-name palette))
                                         n (str package "::" palette)
                                         v (r/r->clj (p/paletteer_d n))]
                                     [k v]))) palettes)]]
  (swap! all-palettes merge pals)
  (spit (str "resources/palettes/c2d_" fpackage ".edn") (with-out-str (pr pals))))

;; palettes _dynamic_

(doseq [[package palettes] (group-by :package (ds/->flyweight (r/r->clj p/palettes_dynamic_names)))
        :let [fpackage (fix-name package)
              pals (into {} (mapcat (fn [{:keys [palette length]}]
                                      (let [n (str package "::" palette)]
                                        (for [id (range 1 (inc length))
                                              :let [k (keyword fpackage (str (fix-name palette) "-" id))
                                                    v (r/r->clj (p/paletteer_dynamic n id))]] 
                                          [k v])))) palettes)]]
  (swap! all-palettes merge pals)
  (spit (str "resources/palettes/c2d_" fpackage ".edn") (with-out-str (pr pals))))

;; http://soliton.vm.bytemark.co.uk/pub/cpt-city/

;; gradient line
(def c3g-pat #"rgba?\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*(?:,\s*(\d+(?:\.\d+)?)\s*)?\)\s+(.*)%")
(def file-pat #"\.\/(.*)\/(.+)\.c3g$")

(defn format-rgba
  ([r g b] (format-rgba r g b 255))
  ([r g b a]
   (let [s (str (format "%02x" (int r))
                (format "%02x" (int g))
                (format "%02x" (int b)))
         a (int a)]
     (if (< a 255) (str s (format "%02x" a)) s))))

(defn parse-rgb
  [data]
  (let [[r g b a p] (map read-string (map #(or % "1.0") data))]
    {:color (format-rgba r g b (int (* 255 a)))
     :percent (* 0.01 p)}))

(defn parse-c3g
  [filename]
  (let [colors (reduce 
                (fn [buff line]
                  (if-let [data (re-find c3g-pat line)]
                    (conj buff (parse-rgb (rest data)))
                    buff))
                []
                (line-seq (io/reader filename)))]
    {:colors (map :color colors)
     :percents (map :percent colors)}))

(defn gradient-or-palette
  [{:keys [colors percents]}]
  (if (= (count percents)
         (count (distinct percents)))
    {:type :gradients
     :c colors
     :p percents}
    {:type :palettes
     :c (distinct colors)}))

(defn process-file
  [[fullpath path filename]]
  (let [package (s/replace path "/" "_")
        colors (parse-c3g (str "resources/cpt-city/" fullpath))]
    (assoc (gradient-or-palette colors)
           :package package
           :name filename)))

(def cpt-city
  (->> "resources/cpt-city/cpt-city-names.txt"
       (io/reader)
       (line-seq)
       (map (comp process-file (partial re-find file-pat)))
       (group-by :type)
       (map (fn [[k v]]
              [k (group-by :package v)]))
       (into {})))

(defn save-cities
  []
  (doseq [[type groups] cpt-city]
    (doseq [[package group] groups
            :let [fpackage (fix-name package)
                  m  (into {} (map (fn [{:keys [c p name]}]
                                     (let [k (keyword fpackage (fix-name name))]
                                       (if (= type :palettes)
                                         [k c]
                                         [k {:c c :p p}]))) group))
                  n (str "resources/" (name type) "/c2d_" fpackage ".edn")]]
      (println n)
      (if (= type :palettes)
        (swap! all-palettes merge m)
        (swap! all-gradients merge m))
      (spit n (with-out-str (pr m))))))

(save-cities)

;;

(defn- read-gz-edn
  [n]
  (-> (io/resource n)
      (io/input-stream)
      (java.util.zip.GZIPInputStream.)
      (slurp)
      (read-string)))

;; mathematica

(let [pals (into {} (map (fn [[n g]]
                           (let [c (map (partial apply format-rgba) g)
                                 n (keyword "mathematica" (name n))]
                             [n c]))) (read-gz-edn "mathematica.edn.gz"))]
  (swap! all-gradients merge pals)
  (spit "resources/gradients/c2d_mathematica.edn" (with-out-str (pr pals))))

;; colourlovers

(let [f (fn [xml-in] (map (fn [x] (map #((:content %) 0) (:content (first (filter #(= (:tag %) :colors) (:content ((:content xml-in) x))))))) (range 100))) ;; parser
      all (->> (range 1 6) ;; five files
               (map #(-> (str "cl" % ".xml.gz")
                         (io/resource)
                         (io/input-stream)
                         (java.util.zip.GZIPInputStream.)
                         (xml/parse)
                         (f)))
               (apply concat)) 
      pals (into {} (map-indexed vector all))]
  (println (count pals))
  (swap! all-palettes merge pals)
  (spit "resources/palettes/c2d_colourlovers.edn" (with-out-str (pr pals))))

;; the old one

(let [pals (into {} (map identity (read-gz-edn "palette_presets.edn.gz")))]
  (swap! all-palettes merge pals)
  (spit "resources/palettes/c2d_.edn" (with-out-str (pr pals))))

;;

(defn prepare-dictionary
  [m]
  (into {} (map (fn [[k v]]
                  [k (->> (keep keyword v)
                          (map name))]) (group-by #(when (keyword? %) (namespace %)) (keys @m)))))

(defn prepare-dictionary->set
  [m]
  (set (keys @m)))

(spit "resources/palettes/all-palettes.edn"
      (keys @all-palettes))
(spit "resources/gradients/all-gradients.edn"
      (keys @all-gradients))
