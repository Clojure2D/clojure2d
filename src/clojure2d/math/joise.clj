;; # Namespace scope
;;
;; Joise noise library bindings
;; https://github.com/SudoPlayGames/Joise
;;
;; There are three group of noises:
;;
;; * Basis (one octave) noise. 4 types with 4 interpolations.
;; * Cell noise. Based on 4 coefficients.
;; * Fractal noise. Combination of above in given number of octaves. 6 types.
;;
;; General concept in Joise is Module. Module returns value from [0.0,1.0] range from 1d,2d,3d,4d and 6d spaces.
;; To make noise function based on module call `make-noise`.
;;
;; See `examples/ex04_noise.clj` for examples.

(ns clojure2d.math.joise
  (:require [clojure2d.math.random :refer :all])
  (:import [com.sudoplay.joise.module Module SourcedModule ScalarParameter
            ModuleBasisFunction ModuleBasisFunction$BasisType ModuleBasisFunction$InterpolationType
            ModuleCellular ModuleCellGen
            ModuleAutoCorrect
            ModuleFractal ModuleFractal$FractalType
            ModuleTriangle ModuleSawtooth
            ModuleScaleDomain]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; ## Noise creators

(defn- get-noise-fn
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

;; ## Autocorrect
;;
;; Noise autocorrection function calculates scaling factor. The main reason is that usually noise doesn't return values from [0,1] range. To make it more accurate use `auto-correct` which returns scaled version of passed module.

(defn- auto-correct
  "Autocorrect module to desired range, (0,1) default. Function calculate scaling factor based on number of samples."
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

;; ## Basis
;;
;; To make basis module call `make-basis`. Configuration is a map:
;;
;; * :type - basis type
;; * :interpolation
;; * :seed - type long
;;
;; Default is {:type :gradval :interpolation :cubic} and random seed. List of types and interpolations below.
;;
;; You can create:
;;
;; * Random configuration with `make-random-basis-conf`
;; * Random basis module with `make-random-basis-module` (autocorrected)
;; * Random basis noise function with `make-random-basis-noise`

;; List of basis types
(def basis-type {:value ModuleBasisFunction$BasisType/VALUE
                 :gradient ModuleBasisFunction$BasisType/GRADIENT
                 :gradval ModuleBasisFunction$BasisType/GRADVAL
                 :simplex ModuleBasisFunction$BasisType/SIMPLEX})

;; List of interpolations
(def interpolation-type {:none ModuleBasisFunction$InterpolationType/NONE
                         :linear ModuleBasisFunction$InterpolationType/LINEAR
                         :cubic ModuleBasisFunction$InterpolationType/CUBIC
                         :quintic ModuleBasisFunction$InterpolationType/QUINTIC})

(defn make-basis
  "Create noise basis module"
  [& m]
  (let [params (merge {:type :gradval
                       :interpolation :cubic
                       :seed (lrand)} (apply merge m))
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
  "Create random configuration for basis noise"
  []
  {:type (rand-nth (keys basis-type))
   :interpolation (rand-nth (keys interpolation-type))
   :seed (lrand)})

(defn make-random-basis-module
  "Create module from random config."
  []
  (auto-correct (make-basis (make-random-basis-conf)) 100))

(defn make-random-basis-noise
  "Create basis noise function."
  []
  (make-noise (make-random-basis-module)))

;; ## Cells
;;
;; To make cell module call `make-cell`. Configuration is a map:
;;
;; * :coeffs - vector with four doubles [a b c d] as cell coefficients
;; * :seed - type long
;;
;; Default is {:coeffs [1 0 0 0]} and random seed.
;;
;; You can create:
;;
;; * Random configuration with `make-random-cell-conf`
;; * Random basis module with `make-random-cell-module` (autocorrected)
;; * Random basis noise function with `make-random-cell-noise`

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
  "Create random configuration for cell module."
  []
  {:coeffs [(drand -5 5) (drand -5 5) (drand -5 5) (drand -5 5)]
   :seed (lrand)})

(defn make-random-cell-module
  "Create random cell module. Autocorrected."
  []
  (auto-correct (make-cell (make-random-cell-conf)) 100))

(defn make-random-cell-noise
  "Create cell noise function"
  []
  (make-noise (make-random-cell-module)))

;; ## Fractal noise
;;
;; To make fractal module call `make-fractal` (autocorrected). Configuration is a map:
;;
;; * :type - fractal type, see list below
;; * :lacunarity - scaling factor for next octave
;; * :frequency - base saling factor
;; * :octaves - list of modules used for octaves (basis or cell)
;;
;; Default is {:type :fbm :lacunarity 2.0 :frequencty 2.0} and 4 basis modules
;;
;; You can create:
;;
;; * Random configuration with `make-random-fractal-conf`
;; * Random basis module with `make-random-fractal-module`
;; * Random basis noise function with `make-random-fractal-noise`

(def fractal-type {:fbm ModuleFractal$FractalType/FBM
                   :ridgemulti ModuleFractal$FractalType/RIDGEMULTI
                   :billow ModuleFractal$FractalType/BILLOW
                   :multi ModuleFractal$FractalType/MULTI
                   :hybridmulti ModuleFractal$FractalType/HYBRIDMULTI
                   :decarpentierswiss ModuleFractal$FractalType/DECARPENTIERSWISS})

(defn make-fractal
  "Create fractal with octaves"
  [& m]
  (let [params (merge {:type :fbm
                       :lacunarity 2.0
                       :frequency 2.0
                       :octaves [(make-basis)
                                 (make-basis)
                                 (make-basis)
                                 (make-basis)]} (apply merge m))
        octaves (:octaves params)
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
  "Create random fractal configuration"
  []
  (let [type (rand-nth (keys fractal-type))
        b [(make-random-basis-module)
           (make-random-cell-module)]
        l (drand 1 3)
        f (drand 1 3)]
    {:type type
     :lacunarity l
     :frequency f
     :octaves [(rand-nth b)
               (rand-nth b)
               (rand-nth b)]}))

(defn make-random-fractal-module
  "Create random fractal module"
  []
  (make-fractal (make-random-fractal-conf)))

(defn make-random-fractal-noise
  "Create random fractal noise"
  []
  (make-noise (make-random-fractal-module)))

;; ## Perlin noise
;;
;; Joise version of Perlin noise

(def perlin-noise (make-noise (make-fractal)))
