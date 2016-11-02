(ns clojure2d.math.joise
  (:require [clojure2d.math :refer :all])
  (:import [com.sudoplay.joise.module Module SourcedModule ScalarParameter
            ModuleBasisFunction ModuleBasisFunction$BasisType ModuleBasisFunction$InterpolationType
            ModuleCellular ModuleCellGen
            ModuleAutoCorrect
            ModuleFractal ModuleFractal$FractalType
            ModuleTriangle ModuleSawtooth
            ModuleScaleDomain]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;;;

(defn get-noise-fn
  "Get noise from Joise Module class"
  ([^Module m x]
   (.get m x 0.333))
  ([^Module m x y]
   (.get m x y))
  ([^Module m x y z]
   (.get m x y z))
  ([^Module m x y z w]
   (.get m x y z w))
  ([^Module m x y z w u v]
   (.get m x y z w u v)))

(defn make-noise 
  "Returns noise function based on module"
  [module]
  (partial get-noise-fn module))

(defn auto-correct
  "Autocorrect module to desired range, (0,1) default"
  ([^Module m samples mn mx]
   (let [^ModuleAutoCorrect ac (ModuleAutoCorrect. mn mx)]
     (doto ac
       (.setSource m)
       (.setSamples samples)
       (.calculate))
     ac))
  ([^Module m]
   (auto-correct m 10000 0.0 1.0))
  ([^Module m samples]
   (auto-correct m samples 0.0 1.0)))

(def basis-type {:value ModuleBasisFunction$BasisType/VALUE
                 :gradient ModuleBasisFunction$BasisType/GRADIENT
                 :gradval ModuleBasisFunction$BasisType/GRADVAL
                 :simplex ModuleBasisFunction$BasisType/SIMPLEX})

(def interpolation-type {:none ModuleBasisFunction$InterpolationType/NONE
                         :linear ModuleBasisFunction$InterpolationType/LINEAR
                         :cubic ModuleBasisFunction$InterpolationType/CUBIC
                         :quintic ModuleBasisFunction$InterpolationType/QUINTIC})

(def default-basis-params {:type :gradval
                           :interpolation :cubic})

;; ![nn](../results/ex00/0000.jpg)
(defn make-basis
  "Create noise basis module"
  [& m]
  (let [params (merge default-basis-params {:seed (lrand)} (apply merge m))
        typ (:type params)
        ^ModuleBasisFunction fun (ModuleBasisFunction. (typ basis-type)
                                                       ((:interpolation params) interpolation-type)
                                                       (:seed params))]
    (if (= typ :simplex)
      (let [^ModuleScaleDomain scale (ModuleScaleDomain.)
            simplex-scale 0.4]
        (doto scale
          (.setScaleX simplex-scale)
          (.setScaleY simplex-scale)
          (.setScaleZ simplex-scale)
          (.setScaleW simplex-scale)
          (.setScaleU simplex-scale)
          (.setScaleV simplex-scale)
          (.setSource fun))
        scale)
      fun)))

(defn make-random-basis-conf
  ""
  []
  (let [t (rand-nth (keys basis-type))
        i (rand-nth (keys interpolation-type))]
    [t i]))

(defn make-random-basis-module
  ""
  []
  (let [[t i] (make-random-basis-conf)]
    (auto-correct (make-basis {:type t
                               :interpolation i}) 100)))

(defn make-random-basis
  ""
  []
  (make-noise (make-random-basis-module)))

(defn make-cell
  "Create cellular module with 4 coefficients"
  [& m]
  (let [params (merge {:coeffs [1 0 0 0] :seed (lrand)} (apply merge m))
        [f1 f2 f3 f4] (:coeffs params)
        ^ModuleCellGen cg (ModuleCellGen.)
        ^ModuleCellular cell (ModuleCellular.)]
    (.setSeed cg (:seed params))
    (.setCoefficients cell f1 f2 f3 f4)
    (.setCellularSource cell cg)
    cell))

(defn make-random-cell-conf
  ""
  []
  [(drand -5 5) (drand -5 5) (drand -5 5) (drand -5 5)])


(defn make-random-cell-module
  ""
  []
  (auto-correct (make-cell {:coeffs (make-random-cell-conf)}) 100))

(defn make-random-cell
  ""
  []
  (make-noise (make-random-cell-module)))

(def fractal-type {:fbm ModuleFractal$FractalType/FBM
                   :ridgemulti ModuleFractal$FractalType/RIDGEMULTI
                   :billow ModuleFractal$FractalType/BILLOW
                   :multi ModuleFractal$FractalType/MULTI
                   :hybridmulti ModuleFractal$FractalType/HYBRIDMULTI
                   :decarpentierswiss ModuleFractal$FractalType/DECARPENTIERSWISS})

(defn process-octaves
  "Prepare list of octaves"
  [xs]
  (loop [lxs xs
         result []]
    (if-not (seq lxs)
      (mapcat identity result)
      (let [[no module] (first lxs)]
        (recur (rest lxs)
               (conj result (repeat no module)))))))

(defn make-fractal
  "Create fractal with octaves"
  [& m]
  (let [params (merge {:type :fbm
                       :lacunarity 2.0
                       :frequency 2.0
                       :octaves [[2 (make-basis)]
                                 [2 (make-basis)]]} (apply merge m))
        octaves (process-octaves (:octaves params))
        ^ModuleFractal fract (ModuleFractal.)]
    (doto fract
      (.setNumOctaves (count octaves))
      (.setFrequency (:frequency params))
      (.setLacunarity (:lacunarity params))
      (.setType ((:type params) fractal-type)))
    (loop [xs octaves
           idx 0]
      (when (seq xs)
        (.overrideSource fract idx (first xs))
        (recur (rest xs) (inc idx))))
    (auto-correct fract 100)))

(defn make-random-fractal-conf
  ""
  []
  (let [type (rand-nth (keys fractal-type))
        b [(make-random-basis-module)
           (make-random-cell-module)
           (make-random-basis-module)
           (make-random-cell-module)]
        l (drand 1 3)
        f (drand 1 3)]
    {:type type
     :lacunarity l
     :frequency f
     :octaves [[1 (rand-nth b)]
               [1 (rand-nth b)]
               [1 (rand-nth b)]]}))

(defn make-random-fractal-module
  ""
  []
  (make-fractal (make-random-fractal-conf)))

(defn make-random-fractal
  ""
  []
  (make-noise (make-random-fractal-module)))

(def perlin-noise (make-noise (make-fractal)))

